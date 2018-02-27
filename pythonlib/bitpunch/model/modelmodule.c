/* -*- c-file-style: "cc-mode" -*- */
/*
 * Copyright (c) 2017, Jonathan Gramain <jonathan.gramain@gmail.com>. All
 * rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * * The names of the bitpunch project contributors may not be used to
 *   endorse or promote products derived from this software without
 *   specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

#include <Python.h>
#include <structmember.h>

#include "api/bitpunch-structs.h"
#include "api/bitpunch_api.h"
#include "core/browse_internal.h"
#include "core/expr_internal.h"
#include "core/print.h"

#define MAX_KEY_LENGTH 4096

static PyObject *BitpunchExc_NoItemError;
static PyObject *BitpunchExc_DataError;
static PyObject *BitpunchExc_OutOfBoundsError;

struct DataContainerObject;
struct DataTreeObject;
struct TrackerObject;

static int
expr_value_from_PyObject(PyObject *py_expr,
                         expr_value_t *exprp);
static PyObject *
expr_value_to_PyObject(struct DataTreeObject *dtree,
                       expr_value_t value_eval,
                       int tracker);
static PyObject *
expr_dpath_to_PyObject(struct DataTreeObject *dtree,
                       expr_dpath_t dpath_eval,
                       int tracker);
static PyObject *
eval_expr_as_python_object(struct DataContainerObject *cont,
                           const char *expr, int tracker);

static PyObject *
tracker_item_to_deep_PyObject(struct DataTreeObject *dtree,
                              struct tracker *tk);
static PyObject *
tracker_item_to_shallow_PyObject(struct DataTreeObject *dtree,
                                 struct tracker *tk);
static PyObject *
box_to_shallow_PyObject(struct DataTreeObject *dtree,
                        struct box *box,
                        int tracker);

static int
tk_goto_item_by_key(struct tracker *tk, PyObject *index,
                    expr_value_t *keyp,
                    int *all_twinsp);

static int
dpath_is_complex_type(const struct dpath_node *dpath)
{
    return ast_node_is_origin_container(dpath_node_get_as_type(dpath))
        && AST_NODE_TYPE_REXPR_INTERPRETER !=
        ast_node_get_target_filter(dpath->filter)->ndat->type;
}

static int __attribute__((unused))
expr_dpath_is_complex_type(expr_dpath_t dpath)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return dpath_is_complex_type(dpath.item.tk->dpath);
    case EXPR_DPATH_TYPE_CONTAINER:
        return dpath_is_complex_type(&dpath.container.box->dpath);
    default:
        assert(0);
    }
    return -1;
}

static PyObject *
tracker_info_to_python(struct tracker *tk)
{
    char *path_str;
    PyObject *info = NULL;
    PyObject *path;

    info = PyDict_New();
    if (NULL == info) {
        return NULL;
    }
    path_str = tracker_get_abs_dpath_alloc(tk);
    if (NULL == path_str) {
        PyErr_SetNone(PyExc_MemoryError);
        goto err;
    }
    path = PyString_FromString(path_str);
    free(path_str);
    PyDict_SetItem(info, PyString_FromString("path"), path);

    if (-1 != tk->item_offset) {
        PyObject *offset;

        offset = PyInt_FromLong(tk->item_offset);
        if (NULL == offset) {
            goto err;
        }
        PyDict_SetItem(info, PyString_FromString("offset"), offset);
    }
    if (-1 != tk->item_size) {
        PyObject *size;

        size = PyInt_FromLong(tk->item_size);
        if (NULL == size) {
            goto err;
        }
        PyDict_SetItem(info, PyString_FromString("size"), size);
    }

    return info;

  err:
    Py_XDECREF(info);
    return NULL;
}

static PyObject *
box_info_to_python(struct box *box)
{
    char *path_str;
    PyObject *info = NULL;
    PyObject *path;

    info = PyDict_New();
    if (NULL == info) {
        return NULL;
    }
    path_str = box_get_abs_dpath_alloc(box);
    if (NULL == path_str) {
        PyErr_SetNone(PyExc_MemoryError);
        goto err;
    }
    path = PyString_FromString(path_str);
    free(path_str);
    PyDict_SetItem(info, PyString_FromString("path"), path);

    return info;

  err:
    Py_XDECREF(info);
    return NULL;
}

static PyObject *
tracker_error_context_info_to_python(
    struct tracker_error_context_info *ctx_info)
{
    PyObject *context_obj = NULL;
    PyObject *info_obj;

    context_obj = PyDict_New();
    if (NULL == context_obj) {
        return NULL;
    }
    if (NULL != ctx_info->tk) {
        info_obj = tracker_info_to_python(ctx_info->tk);
        if (NULL == info_obj) {
            goto err;
        }
        PyDict_SetItem(context_obj,
                       PyString_FromString("item"), info_obj);
    }
    if (NULL != ctx_info->box) {
        info_obj = box_info_to_python(ctx_info->box);
        if (NULL == info_obj) {
            goto err;
        }
        PyDict_SetItem(context_obj,
                       PyString_FromString("box"), info_obj);
    }
    //TODO: add node info if ctx_info->node != NULL
    if (NULL != ctx_info->message) {
        PyDict_SetItem(context_obj,
                       PyString_FromString("contextmsg"),
                       PyString_FromString(ctx_info->message));
    }

    return context_obj;

  err:
    Py_XDECREF(context_obj);
    return NULL;
}

static PyObject *
tracker_error_get_info_as_PyObject(struct tracker_error *err)
{
    PyObject *errobj = NULL;
    int ctx_i;
    PyObject *contexts_obj;

    errobj = PyDict_New();
    if (NULL == errobj) {
        return NULL;
    }

    if (NULL != err->tk || NULL != err->box) {
        PyObject *info_obj;

        if (NULL != err->tk) {
            info_obj = tracker_info_to_python(err->tk);
        } else {
            info_obj = box_info_to_python(err->box);
        }
        if (NULL == info_obj) {
            goto err;
        }
        PyDict_SetItem(errobj,
                       PyString_FromString(NULL != err->tk ?
                                           "item" : "box"), info_obj);
    }
    PyDict_SetItem(errobj,
                   PyString_FromString("message"),
                   PyString_FromString(err->error_buf));
    if (err->n_contexts == 0) {
        return errobj;
    }
    contexts_obj = PyList_New(err->n_contexts);
    if (NULL == contexts_obj) {
        goto err;
    }
    PyDict_SetItem(errobj, PyString_FromString("contexts"), contexts_obj);
    for (ctx_i = 0; ctx_i < err->n_contexts; ++ctx_i) {
        PyObject *context_obj;

        context_obj = tracker_error_context_info_to_python(
            &err->contexts[ctx_i]);
        if (NULL == context_obj) {
            goto err;
        }
        PyList_SET_ITEM(contexts_obj, ctx_i, context_obj);
    }
    return errobj;

  err:
    Py_XDECREF(errobj);
    return NULL;
}

static void
set_tracker_error(struct tracker_error *err,
                  bitpunch_status_t ret)
{
    PyObject *errobj;

    if (NULL != err) {
        errobj = tracker_error_get_info_as_PyObject(err);
    } else {
        Py_INCREF(Py_None);
        errobj = Py_None;
    }
    switch (ret) {
    case BITPUNCH_INVALID_PARAM:
        PyErr_SetObject(PyExc_ValueError, errobj);
        break ;
    case BITPUNCH_NO_ITEM:
        PyErr_SetObject(BitpunchExc_NoItemError, Py_None);
        break ;
    case BITPUNCH_NOT_CONTAINER:
        PyErr_SetString(PyExc_TypeError,
                        "tracked item is not a container type");
        break ;
    case BITPUNCH_DATA_ERROR:
        PyErr_SetObject(BitpunchExc_DataError, errobj);
        break ;
    case BITPUNCH_OUT_OF_BOUNDS_ERROR:
        PyErr_SetObject(BitpunchExc_OutOfBoundsError, errobj);
        break ;
    case BITPUNCH_NOT_IMPLEMENTED:
        PyErr_SetObject(PyExc_NotImplementedError, errobj);
        break ;
    case BITPUNCH_ERROR:
        PyErr_SetObject(PyExc_RuntimeError, errobj);
        break ;
    default:
        PyErr_Format(PyExc_RuntimeError,
                     "Tracker unknown error code %d", (int)ret);
        break ;
    }
    Py_DECREF(errobj);
    tracker_error_destroy(err);
}


static PyObject *
DataBlock_box_to_python_object(struct DataTreeObject *dtree,
                               struct box *box);

static PyObject *
DataArray_box_to_python_object(struct DataTreeObject *dtree,
                               struct box *box);

static struct TrackerObject *
Tracker_new_from_DataContainer(PyTypeObject *subtype,
                               struct DataContainerObject *dcont);

static struct TrackerObject *
Tracker_new_from_tk(PyTypeObject *subtype,
                    struct DataTreeObject *dtree,
                    struct tracker *tk);

/*
 * Format
 */

PyDoc_STRVAR(FormatSpec__doc__,
             "Compiled representation of the format of a binary file");

typedef struct FormatSpecObject {
    PyObject_HEAD
    struct bitpunch_schema_hdl *schema;
} FormatSpecObject;

static PyMethodDef FormatSpec_methods[] = {

    { NULL, NULL, 0, NULL }
};

static PyTypeObject FormatSpecType = {
    PyObject_HEAD_INIT(NULL)
    0,                         /*ob_size*/
    "bitpunch.FormatSpec",      /*tp_name*/
    sizeof(FormatSpecObject),  /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    0,                         /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0,                         /*tp_hash */
    0,                         /*tp_call*/
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,       /*tp_flags*/
    FormatSpec__doc__,         /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,		               /* tp_richcompare */
    0,		               /* tp_weaklistoffset */
    0,		               /* tp_iter */
    0,		               /* tp_iternext */
    FormatSpec_methods,        /* tp_methods */
    0,                         /* tp_members */
    0,                         /* tp_getset */
    0,                         /* tp_base */
    0,                         /* tp_dict */
    0,                         /* tp_descr_get */
    0,                         /* tp_descr_set */
    0,                         /* tp_dictoffset */
    0,                         /* tp_init */
    0,                         /* tp_alloc */
    0,                         /* tp_new */
};

static PyObject *
FormatSpec_new(PyTypeObject *subtype,
               PyObject *args, PyObject *kwds)
{
    int ret;
    PyObject *arg;
    FormatSpecObject *self;

    if (!PyArg_ParseTuple(args, "O", &arg)) {
        return NULL;
    }
    self = (FormatSpecObject *)subtype->tp_alloc(subtype, 0);
    if (NULL == self) {
        return NULL;
    }
    if (PyString_Check(arg)) {
        const char *contents;

        /* compile the provided text contents */
        contents = PyString_AsString(arg);
        ret = bitpunch_load_schema_from_string(contents, &self->schema);
    } else if (PyFile_Check(arg)) {
        FILE *file;

        /* compile the contents from the file object */
        file = PyFile_AsFile(arg);
        //PyFile_IncUseCount((PyFileObject *)arg);
        ret = bitpunch_load_schema_from_fd(fileno(file), &self->schema);
    } else {
        Py_DECREF((PyObject *)self);
        PyErr_SetString(PyExc_TypeError,
                        "The argument must be a string or a file object");
        return NULL;
    }
    if (-1 == ret) {
        Py_DECREF((PyObject *)self);
        PyErr_SetString(PyExc_OSError,
                        "Error loading binary definition grammar");
        return NULL;
    }
    return (PyObject *)self;
}


static void
FormatSpec_clear(FormatSpecObject *self)
{
    if (NULL != self->schema) {
        bitpunch_free_schema(self->schema);
        self->schema = NULL;
    }
}

static void
FormatSpec_dealloc(FormatSpecObject *self)
{
    FormatSpec_clear(self);
    Py_TYPE(self)->tp_free(self);
}

static int
FormatSpecType_setup(void)
{
    FormatSpecType.ob_type = &PyType_Type;
    FormatSpecType.tp_new = FormatSpec_new;
    FormatSpecType.tp_dealloc = (destructor)FormatSpec_dealloc;
    if (PyType_Ready(&FormatSpecType) < 0) {
        return -1;
    }
    return 0;
}

/*
 * IndexKey
 */

PyDoc_STRVAR(IndexKey__doc__,
             "A key object that can be used to access an item from an "
             "indexed array");

typedef struct IndexKeyObject {
    PyObject_HEAD
    PyObject *key;
} IndexKeyObject;


static PyObject *
IndexKey_create_str(IndexKeyObject *self, reprfunc key_repr_func)
{
    PyObject *repr = NULL;

    if (PyTuple_Check(self->key)) {
        PyObject *key;
        int twin_idx;
        PyObject *repr_key;

        if (!PyArg_ParseTuple(self->key, "Oi", &key, &twin_idx)) {
            return NULL;
        }
        repr_key = key_repr_func(key);
        if (NULL != repr_key) {
            repr = PyString_FromFormat("%s{%d}",
                                       PyString_AS_STRING(repr_key),
                                       twin_idx);
            Py_DECREF(repr_key);
        }
    } else {
        repr = key_repr_func(self->key);
    }
    return repr;
}

static PyObject *
key_repr_double_quoted(PyObject *key)
{
    char *buf;
    Py_ssize_t len;
    FILE *memstr;
    char *quoted_str;
    size_t quoted_str_len;
    PyObject *res;

    if (!PyString_Check(key)) {
        return PyObject_Repr(key);
    }
    if (-1 == PyString_AsStringAndSize(key, &buf, &len)) {
        return NULL;
    }
    memstr = open_memstream(&quoted_str, &quoted_str_len);
    if (NULL == memstr) {
        PyErr_SetString(PyExc_OSError, "error opening memory stream");
        return NULL;
    }
    fputc('"', memstr);
    print_bytes(buf, len, memstr, MAX_KEY_LENGTH * 4);
    fputc('"', memstr);
    fclose(memstr);
    res = PyString_FromStringAndSize(quoted_str, quoted_str_len);
    free(quoted_str);
    return res;
}

static PyObject *
IndexKey_str_single_quoted(IndexKeyObject *self)
{
    return IndexKey_create_str(self, PyObject_Repr);
}

static PyObject *
IndexKey_str_double_quoted(IndexKeyObject *self)
{
    return IndexKey_create_str(self, (reprfunc)key_repr_double_quoted);
}

static PyMethodDef IndexKey_methods[] = {

    { "str_single_quoted",
      (PyCFunction)IndexKey_str_single_quoted, METH_NOARGS,
      "Return the index key as a single-quoted string."
    },

    { "str_double_quoted",
      (PyCFunction)IndexKey_str_double_quoted, METH_NOARGS,
      "Return the index key as a double-quoted string."
    },

    { NULL, NULL, 0, NULL }
};

static PyTypeObject IndexKeyType = {
    PyObject_HEAD_INIT(NULL)
    0,                           /*ob_size*/
    "bitpunch.IndexKey",          /*tp_name*/
    sizeof(IndexKeyObject),      /*tp_basicsize*/
    0,                           /*tp_itemsize*/
    0,                           /*tp_dealloc*/
    0,                           /*tp_print*/
    0,                           /*tp_getattr*/
    0,                           /*tp_setattr*/
    0,                           /*tp_compare*/
    0,                           /*tp_repr*/
    0,                           /*tp_as_number*/
    0,                           /*tp_as_sequence*/
    0,                           /*tp_as_mapping*/
    0,                           /*tp_hash */
    0,                           /*tp_call*/
    0,                           /*tp_str*/
    0,                           /*tp_getattro*/
    0,                           /*tp_setattro*/
    0,                           /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE |
    Py_TPFLAGS_HAVE_RICHCOMPARE, /*tp_flags*/
    IndexKey__doc__,           /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,		               /* tp_richcompare */
    0,		               /* tp_weaklistoffset */
    0,		               /* tp_iter */
    0,		               /* tp_iternext */
    IndexKey_methods,          /* tp_methods */
    0,                         /* tp_members */
    0,                         /* tp_getset */
    0,                         /* tp_base */
    0,                         /* tp_dict */
    0,                         /* tp_descr_get */
    0,                         /* tp_descr_set */
    0,                         /* tp_dictoffset */
    0,                         /* tp_init */
    0,                         /* tp_alloc */
    0,                         /* tp_new */
};

static PyObject *
IndexKey_new(PyTypeObject *subtype,
             PyObject *args, PyObject *kwds)
{
    PyObject *key;
    IndexKeyObject *self;

    if (!PyArg_ParseTuple(args, "O", &key)) {
        return NULL;
    }
    self = (IndexKeyObject *)subtype->tp_alloc(subtype, 0);
    if (NULL == self) {
        Py_DECREF(key);
        return NULL;
    }
    Py_INCREF(key);
    self->key = key;
    return (PyObject *)self;
}

static int
IndexKey_clear(IndexKeyObject *self)
{
    Py_CLEAR(self->key);
    return 0;
}

static void
IndexKey_dealloc(IndexKeyObject *self)
{
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
IndexKey_str(IndexKeyObject *self)
{
    return IndexKey_create_str(self, PyObject_Str);
}

static PyObject *
IndexKey_repr(IndexKeyObject *self)
{
    PyObject *key_repr;
    PyObject *repr = NULL;

    key_repr = IndexKey_create_str(self, PyObject_Repr);
    if (NULL != key_repr) {
        repr = PyString_FromFormat("IndexKey(%s)",
                                   PyString_AS_STRING(key_repr));
        Py_DECREF(key_repr);
    }
    return repr;
}

static PyObject *
IndexKey_richcompare(IndexKeyObject *self, PyObject *b, int op)
{
    PyObject *key_obj2;
    PyObject *k1, *k2;
    int twin1, twin2;
    PyObject *key_eq;
    int cmp_res = 0;

#define TWIN_ANY -1
    if (PyTuple_Check(self->key)) {
        if (!PyArg_ParseTuple(self->key, "Oi", &k1, &twin1)) {
            return NULL;
        }
    } else {
        k1 = self->key;
        twin1 = 0;
    }
    if (PyObject_TypeCheck(b, &IndexKeyType)) {
        key_obj2 = ((IndexKeyObject *)b)->key;
    } else {
        key_obj2 = b;
    }
    if (PyTuple_Check(key_obj2)) {
        if (!PyArg_ParseTuple(key_obj2, "Oi", &k2, &twin2)) {
            return NULL;
        }
        if (twin2 < 0) {
            PyErr_SetString(PyExc_ValueError,
                            "Twin index cannot be negative");
            return NULL;
        }
    } else {
        k2 = key_obj2;
        if (PyObject_TypeCheck(b, &IndexKeyType)) {
            twin2 = 0;
        } else {
            twin2 = TWIN_ANY;
        }
    }
    key_eq = PyObject_RichCompare(k1, k2, Py_EQ);
    if (NULL == key_eq) {
        return NULL;
    }
    if (key_eq == Py_False) {
        if (op == Py_EQ) {
            return key_eq;
        }
        Py_DECREF(key_eq);
        if (op == Py_NE) {
            Py_INCREF(Py_True);
            return Py_True;
        }
        return PyObject_RichCompare(k1, k2, op);
    } else {
        Py_DECREF(key_eq);
    }
    if (TWIN_ANY == twin2) {
        switch (op) {
        case Py_EQ:
        case Py_LE:
        case Py_GE:
            cmp_res = TRUE;
            break ;
        case Py_NE:
        case Py_LT:
        case Py_GT:
            cmp_res = FALSE;
            break ;
        default:
            assert(0);
        }
    } else {
        switch (op) {
        case Py_LT:
            cmp_res = twin1 < twin2;
            break ;
        case Py_LE:
            cmp_res = twin1 <= twin2;
            break ;
        case Py_EQ:
            cmp_res = twin1 == twin2;
            break ;
        case Py_NE:
            cmp_res = twin1 != twin2;
            break ;
        case Py_GT:
            cmp_res = twin1 > twin2;
            break ;
        case Py_GE:
            cmp_res = twin1 >= twin2;
            break ;
        default:
            assert(0);
        }
    }
    return PyBool_FromLong(cmp_res);
#undef TWIN_ANY
}

static int
IndexKeyType_setup(void)
{
    IndexKeyType.ob_type = &PyType_Type;
    IndexKeyType.tp_new = IndexKey_new;
    IndexKeyType.tp_clear = (inquiry)IndexKey_clear;
    IndexKeyType.tp_dealloc = (destructor)IndexKey_dealloc;
    IndexKeyType.tp_repr = (reprfunc)IndexKey_repr;
    IndexKeyType.tp_str = (reprfunc)IndexKey_str;
    IndexKeyType.tp_richcompare = (richcmpfunc)IndexKey_richcompare;
    if (PyType_Ready(&IndexKeyType) < 0) {
        return -1;
    }
    return 0;
}

/*
 * DataContainer
 */

PyDoc_STRVAR(DataContainer__doc__,
             "Represents a data container representing a section "
             "of the binary contents");

typedef struct DataContainerObject {
    PyObject_HEAD
    struct DataTreeObject *dtree;
    struct box *box;
} DataContainerObject;


static PyObject *
DataContainer_box_to_python_object(struct DataTreeObject *dtree,
                                   struct box *box);

static int
DataContainer_bf_getbuffer(DataContainerObject *exporter,
                           Py_buffer *view, int flags);


static PyBufferProcs DataContainerAsBuffer = {
    .bf_getbuffer = (getbufferproc)DataContainer_bf_getbuffer,
};

static PyObject *
DataContainer_iter_keys(DataContainerObject *self);

static PyObject *
DataContainer_iter_items(DataContainerObject *self);

static PyObject *
DataContainer_eval_expr(DataContainerObject *cont,
                        PyObject *args, PyObject *kwds);

static PyObject *
DataContainer_get_size(DataContainerObject *self, PyObject *args);

static PyObject *
DataContainer_get_offset(DataContainerObject *self, PyObject *args);

static PyObject *
DataContainer_get_location(DataContainerObject *self, PyObject *args);

static PyObject *
DataContainer___unicode__(DataContainerObject *self, PyObject *args);

static PyMethodDef DataContainer_methods[] = {
    { "iter_keys", (PyCFunction)DataContainer_iter_keys, METH_NOARGS,
      "get an iterator over the array keys"
    },
    { "iter_items", (PyCFunction)DataContainer_iter_items, METH_NOARGS,
      "get an iterator over the array items as (key, value) tuples"
    },
    { "eval_expr", (PyCFunction)DataContainer_eval_expr,
      METH_VARARGS | METH_KEYWORDS,
      "evaluate a bitpunch expression in the object's scope"
      "\n"
      "keyword arguments:\n"
      "tracker -- if True, return a Tracker object pointing\n"
      "to the original object location in place of native Python types "
      "(default is False)"
    },
    { "get_size", (PyCFunction)DataContainer_get_size, METH_NOARGS,
      "get the spanned size of the container in the file or "
      "filtered byte contents"
    },
    { "get_offset", (PyCFunction)DataContainer_get_offset, METH_NOARGS,
      "get the absolute byte offset of the container from the beginning "
      "of the file or filtered byte contents"
    },
    { "get_location",
      (PyCFunction)DataContainer_get_location, METH_NOARGS,
      "get a tuple of the absolute byte offset of the container from "
      "the beginning of the file or filtered byte contents, and the "
      "byte size of the container"
    },
    { "__unicode__",
      (PyCFunction)DataContainer___unicode__, METH_NOARGS,
      "convert to unicode string"
    },
    { NULL, NULL, 0, NULL }
};

static PyTypeObject DataContainerType = {
    PyObject_HEAD_INIT(NULL)
    0,                           /*ob_size*/
    "bitpunch.DataContainer",     /*tp_name*/
    sizeof(DataContainerObject), /*tp_basicsize*/
    0,                           /*tp_itemsize*/
    0,                           /*tp_dealloc*/
    0,                           /*tp_print*/
    0,                           /*tp_getattr*/
    0,                           /*tp_setattr*/
    0,                           /*tp_compare*/
    0,                           /*tp_repr*/
    0,                           /*tp_as_number*/
    0,                           /*tp_as_sequence*/
    0,                           /*tp_as_mapping*/
    0,                           /*tp_hash */
    0,                           /*tp_call*/
    0,                           /*tp_str*/
    0,                           /*tp_getattro*/
    0,                           /*tp_setattro*/
    &DataContainerAsBuffer,      /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE |
    Py_TPFLAGS_HAVE_NEWBUFFER,   /*tp_flags*/
    DataContainer__doc__,      /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,		               /* tp_richcompare */
    0,		               /* tp_weaklistoffset */
    0,		               /* tp_iter */
    0,		               /* tp_iternext */
    DataContainer_methods,     /* tp_methods */
    0,                         /* tp_members */
    0,                         /* tp_getset */
    0,                         /* tp_base */
    0,                         /* tp_dict */
    0,                         /* tp_descr_get */
    0,                         /* tp_descr_set */
    0,                         /* tp_dictoffset */
    0,                         /* tp_init */
    0,                         /* tp_alloc */
    0,                         /* tp_new */
};

static PyObject *
DataContainer_new(PyTypeObject *subtype,
                  PyObject *args, PyObject *kwds)
{
    DataContainerObject *self;

    self = (DataContainerObject *)subtype->tp_alloc(subtype, 0);
    if (NULL == self) {
        return NULL;
    }
    self->box = NULL;
    return (PyObject *)self;
}

static int
DataContainer_clear(DataContainerObject *self)
{
    if (NULL != self->box) {
        box_delete(self->box);
        self->box = NULL;
    }
    Py_CLEAR(self->dtree);
    return 0;
}

static void
DataContainer_dealloc(DataContainerObject *self)
{
    DataContainer_clear(self);
    Py_TYPE(self)->tp_free(self);
}

static void
DataContainer_construct(struct DataContainerObject *dcont,
                        struct DataTreeObject *dtree, struct box *box)
{
    dcont->box = box;
    box_acquire(box);

    dcont->dtree = dtree;
    Py_INCREF(dtree);
}

static PyObject *
DataContainer_make_python_object(DataContainerObject *obj)
{
    return DataContainer_box_to_python_object(obj->dtree, obj->box);
}

static PyObject *
DataContainer_str(DataContainerObject *self);


static PyObject *
DataContainer_iter(struct DataContainerObject *self);

static int
DataContainerType_setup(void)
{
    DataContainerType.ob_type = &PyType_Type;
    DataContainerType.tp_new = DataContainer_new;
    DataContainerType.tp_clear = (inquiry)DataContainer_clear;
    DataContainerType.tp_dealloc = (destructor)DataContainer_dealloc;
    DataContainerType.tp_str = (reprfunc)DataContainer_str;
    DataContainerType.tp_iter = (getiterfunc)DataContainer_iter;

    if (PyType_Ready(&DataContainerType) < 0) {
        return -1;
    }
    return 0;
}

/*
 * DataBlock
 */

PyDoc_STRVAR(DataBlock__doc__,
             "Represents a structured block");

typedef struct DataBlockObject {
    DataContainerObject container;
} DataBlockObject;


static PyObject *
DataBlock_box_to_python_object(struct DataTreeObject *dtree,
                               struct box *box)
{
    PyObject *dict;
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    expr_value_t value_eval;
    PyObject *py_key;
    PyObject *py_value;
    struct tracker_error *tk_err = NULL;

    dict = PyDict_New();
    if (NULL == dict) {
        return NULL;
    }
    tk = track_box_contents(box, &tk_err);
    if (NULL == tk) {
        assert(NULL != tk_err);
        bt_ret = tk_err->bt_ret;
        goto tk_error;
    }
    bt_ret = tracker_goto_next_item(tk, &tk_err);
    while (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_get_item_key(tk, &value_eval, &tk_err);
        if (BITPUNCH_OK != bt_ret) {
            goto tk_error;
        }
        py_key = expr_value_to_PyObject(dtree, value_eval, FALSE);
        if (NULL == py_key) {
            goto error;
        }
        py_value = tracker_item_to_deep_PyObject(dtree, tk);
        if (NULL == py_value) {
            Py_DECREF(py_key);
            goto error;
        }
        PyDict_SetItem(dict, py_key, py_value);
        Py_DECREF(py_key);
        Py_DECREF(py_value);
        bt_ret = tracker_goto_next_item(tk, &tk_err);
    }
    if (BITPUNCH_NO_ITEM != bt_ret) {
        goto tk_error;
    }
    tracker_delete(tk);
    return dict;

  tk_error:
    set_tracker_error(tk_err, bt_ret);
  error:
    tracker_delete(tk);
    Py_DECREF(dict);
    return NULL;
}

static PyObject *
DataContainer_get_dict(DataContainerObject *self, const char *attr_str)
{
    PyObject *dict;
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    PyObject *py_key;
    expr_value_t key_value;
    tnamed_expr_iterator named_expr_iter;
    const struct named_expr *named_expr;
    struct tracker_error *tk_err = NULL;

    /* generate a dict containing all attribute keys on-the-fly */
    dict = PyDict_New();
    if (NULL == dict) {
        return NULL;
    }
    tk = track_box_contents(self->box, &tk_err);
    if (NULL == tk) {
        assert(NULL != tk_err);
        set_tracker_error(tk_err, tk_err->bt_ret);
        Py_DECREF(dict);
        return NULL;
    }
    bt_ret = tracker_goto_next_item(tk, NULL);
    while (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_get_item_key(tk, &key_value, NULL);
        if (BITPUNCH_OK != bt_ret
            || key_value.type != EXPR_VALUE_TYPE_STRING) {
            break ;
        }
        py_key = PyString_FromStringAndSize(key_value.string.str,
                                            key_value.string.len);
        if (NULL != py_key) {
            PyDict_SetItem(dict, py_key, Py_None);
            Py_DECREF(py_key);
        }
        bt_ret = tracker_goto_next_item(tk, NULL);
    }
    tracker_delete(tk);
    named_expr_iter = box_iter_named_exprs(self->box);
    bt_ret = box_iter_named_exprs_next(self->box, &named_expr_iter,
                                       &named_expr, NULL);
    while (BITPUNCH_OK == bt_ret) {
        py_key = PyString_FromString(named_expr->nstmt.name);
        if (NULL != py_key) {
            PyDict_SetItem(dict, py_key, Py_None);
            Py_DECREF(py_key);
        }
        bt_ret = box_iter_named_exprs_next(self->box, &named_expr_iter,
                                           &named_expr, NULL);
    }
    return dict;
}


static PyObject *
DataContainer_get_named_item(DataContainerObject *self,
                             const char *attr_str)
{
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    PyObject *res = NULL;
    struct tracker_error *tk_err = NULL;

    tk = track_box_contents(self->box, &tk_err);
    if (NULL == tk) {
        assert(NULL != tk_err);
        set_tracker_error(tk_err, tk_err->bt_ret);
        return NULL;
    }
    bt_ret = tracker_goto_named_item(tk, attr_str, &tk_err);
    if (BITPUNCH_NO_ITEM == bt_ret) {
        return PyErr_Format(PyExc_AttributeError,
                            "no such key: %s", attr_str);
    }
    if (BITPUNCH_OK == bt_ret) {
        res = tracker_item_to_shallow_PyObject(self->dtree, tk);
    } else {
        set_tracker_error(tk_err, bt_ret);
    }
    tracker_delete(tk);
    return res;
}

static PyObject *
DataContainer_eval_attr(DataContainerObject *self, const char *attr_str)
{
    bitpunch_status_t bt_ret;
    expr_value_t value_eval;
    expr_dpath_t dpath_eval;
    struct tracker_error *tk_err = NULL;
    const struct ast_node_hdl *filter;

    bt_ret = box_evaluate_attribute_dpath(self->box, attr_str,
                                          &dpath_eval, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        if (BITPUNCH_NO_ITEM == bt_ret) {
            PyErr_Format(PyExc_AttributeError,
                         "no such named expression or field: %s",
                         attr_str);
        } else {
            set_tracker_error(tk_err, bt_ret);
        }
        return NULL;
    }
    if (EXPR_DPATH_TYPE_NONE == dpath_eval.type) {
        bt_ret = box_evaluate_attribute_value(self->box, attr_str,
                                              &value_eval, &tk_err);
        if (BITPUNCH_OK != bt_ret) {
            set_tracker_error(tk_err, bt_ret);
            return NULL;
        }
        return expr_value_to_PyObject(self->dtree, value_eval, FALSE);
    }
    // FIXME rework choice between dpath or value type
    filter = expr_dpath_get_target_filter(dpath_eval);
    if (EXPR_VALUE_TYPE_INTEGER != filter->ndat->u.rexpr.value_type) {
        return expr_dpath_to_PyObject(self->dtree, dpath_eval, FALSE);
    }
    bt_ret = dpath_read_value(dpath_eval, &value_eval, &tk_err);
    expr_dpath_destroy(dpath_eval);
    if (BITPUNCH_OK != bt_ret) {
        set_tracker_error(tk_err, bt_ret);
        return NULL;
    }
    return expr_value_to_PyObject(self->dtree, value_eval, FALSE);
}

static PyObject *
DataBlock_getattr_base(DataBlockObject *self, PyObject *attr_name,
                       int lookup_methods)
{
    PyObject *attr;
    const char *attr_str;

    if (!PyString_Check(attr_name)) {
        return PyErr_Format(PyExc_TypeError,
                            "attribute must be a string type");
    }
    attr_str = PyString_AS_STRING(attr_name);
    if (0 == strcmp(attr_str, "__dict__")) {
        return DataContainer_get_dict(&self->container, attr_str);
    }
    if (lookup_methods) {
        attr = PyObject_GenericGetAttr((PyObject *)self, attr_name);
        if (NULL != attr) {
            return attr;
        }
        PyErr_Clear();
    }
    return DataContainer_eval_attr(&self->container, attr_str);
}

static PyObject *
DataBlock_getattro(DataBlockObject *self, PyObject *attr_name)
{
    return DataBlock_getattr_base(self, attr_name, TRUE);
}

static PyObject *
DataBlock_mp_subscript(PyObject *_d_blk, PyObject *key)
{
    DataBlockObject *d_blk = (DataBlockObject *)_d_blk;

    return DataBlock_getattr_base(d_blk, key, FALSE);
}

static PyMappingMethods DataBlock_as_mapping = {
    .mp_length = NULL,
    .mp_subscript = DataBlock_mp_subscript,
    .mp_ass_subscript = NULL
};


static PyTypeObject DataBlockType = {
    PyObject_HEAD_INIT(NULL)
    0,                         /*ob_size*/
    "bitpunch.DataBlock",       /*tp_name*/
    sizeof(DataBlockObject),   /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    0,                         /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    &DataBlock_as_mapping,     /*tp_as_mapping*/
    0,                         /*tp_hash */
    0,                         /*tp_call*/
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,       /*tp_flags*/
    DataBlock__doc__,          /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,		               /* tp_richcompare */
    0,		               /* tp_weaklistoffset */
    0,		               /* tp_iter */
    0,		               /* tp_iternext */
    0,                         /* tp_methods */
    0,                         /* tp_members */
    0,                         /* tp_getset */
    &DataContainerType,        /* tp_base */
    0,                         /* tp_dict */
    0,                         /* tp_descr_get */
    0,                         /* tp_descr_set */
    0,                         /* tp_dictoffset */
    0,                         /* tp_init */
    0,                         /* tp_alloc */
    0,                         /* tp_new */
};

static PyObject *
DataBlock_new(PyTypeObject *subtype,
              PyObject *args, PyObject *kwds)
{
    DataBlockObject *self;

    self = (DataBlockObject *)
        DataContainer_new(subtype, args, kwds);
    if (NULL == self) {
        return NULL;
    }
    return (PyObject *)self;
}

static void
DataBlock_dealloc(DataBlockObject *self)
{
    DataContainer_clear(&self->container);
    Py_TYPE(self)->tp_free(self);
}


static int
DataBlockType_setup(void)
{
    DataBlockType.ob_type = &PyType_Type;
    DataBlockType.tp_new = DataBlock_new;
    DataBlockType.tp_clear = (inquiry)DataContainer_clear;
    DataBlockType.tp_dealloc = (destructor)DataBlock_dealloc;
    DataBlockType.tp_getattro = (getattrofunc)DataBlock_getattro;

    if (PyType_Ready(&DataBlockType) < 0) {
        return -1;
    }
    return 0;
}

/*
 * DataArray
 */

PyDoc_STRVAR(DataArray__doc__,
             "Represents a structured data container representing a "
             "section of the binary contents");

typedef struct DataArrayObject {
    struct DataContainerObject container;
} DataArrayObject;


static Py_ssize_t
DataArray_mp_length(PyObject *_d_arr)
{
    DataArrayObject *d_arr = (DataArrayObject *)_d_arr;
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    int64_t item_count;
    struct tracker_error *tk_err = NULL;

    tk = track_box_contents(d_arr->container.box, &tk_err);
    if (NULL == tk) {
        assert(NULL != tk_err);
        set_tracker_error(tk_err, tk_err->bt_ret);
        return -1;
    }
    bt_ret = tracker_get_n_items(tk, &item_count, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        set_tracker_error(tk_err, bt_ret);
        tracker_delete(tk);
        return -1;
    }
    tracker_delete(tk);
    return (Py_ssize_t)item_count;
}

static struct tracker *
DataArray_mp_subscript_get_tk_at_index(DataArrayObject *d_arr,
                                       PyObject *key,
                                       expr_value_t *keyp,
                                       int *all_twinsp)
{
    struct tracker *tk;
    PyObject *key_obj;
    struct tracker_error *tk_err = NULL;

    if (PyObject_TypeCheck(key, &IndexKeyType)) {
        key_obj = ((IndexKeyObject *)key)->key;
    } else {
        key_obj = key;
    }
    tk = track_box_contents(d_arr->container.box, &tk_err);
    if (NULL == tk) {
        assert(NULL != tk_err);
        set_tracker_error(tk_err, tk_err->bt_ret);
        return NULL;
    }
    if (-1 == tk_goto_item_by_key(tk, key_obj, keyp, all_twinsp)) {
        tracker_delete(tk);
        if (PyErr_Occurred() == BitpunchExc_NoItemError) {
            PyObject *repr_key;

            PyErr_Clear();
            repr_key = PyObject_Repr(key);
            if (NULL != repr_key) {
                PyErr_Format(PyExc_IndexError,
                             "no such index: %s",
                             PyString_AS_STRING(repr_key));
                Py_DECREF(repr_key);
            }
        }
        return NULL;
    }
    return tk;
}

static PyObject *
DataArray_mp_subscript_key(DataArrayObject *d_arr, PyObject *key)
{
    PyObject *res = NULL;
    struct tracker *tk;
    expr_value_t tk_key;
    int all_twins;

    tk = DataArray_mp_subscript_get_tk_at_index(d_arr, key,
                                                &tk_key, &all_twins);
    if (NULL == tk) {
        return NULL;
    }
    if (all_twins) {
        PyObject *item;

        res = PyList_New(0);
        if (NULL == res) {
            tracker_delete(tk);
            return NULL;
        }
        while (TRUE) {
            bitpunch_status_t bt_ret;
            struct tracker_error *tk_err = NULL;

            item = tracker_item_to_shallow_PyObject(d_arr->container.dtree,
                                                    tk);
            if (NULL == item) {
                Py_DECREF(res);
                tracker_delete(tk);
                return NULL;
            }
            if (Py_None == item) {
                // end of matching elements list
                break ;
            }
            if (-1 == PyList_Append(res, item)) {
                Py_DECREF(res);
                tracker_delete(tk);
                return NULL;
            }
            bt_ret = tracker_goto_next_item_with_key(tk, tk_key, &tk_err);
            if (BITPUNCH_OK != bt_ret) {
                if (BITPUNCH_NO_ITEM == bt_ret) {
                    break ;
                }
                Py_DECREF(res);
                set_tracker_error(tk_err, bt_ret);
                tracker_delete(tk);
                return NULL;
            }
        }
    } else {
        res = tracker_item_to_shallow_PyObject(d_arr->container.dtree, tk);
    }
    tracker_delete(tk);
    return res;
}

static PyObject *
DataArray_mp_subscript_slice(DataArrayObject *d_arr, PyObject *key)
{
    PyObject *res = NULL;
    int64_t item_count;
    bitpunch_status_t bt_ret;
    struct tracker *tk_start = NULL;
    struct tracker *tk_end = NULL;
    Py_ssize_t slice_start;
    Py_ssize_t slice_stop;
    Py_ssize_t slice_step;
    Py_ssize_t slice_length;
    struct box *slice_box;
    struct browse_state bst;
    struct tracker_error *tk_err = NULL;

    bt_ret = box_get_n_items(d_arr->container.box, &item_count, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        set_tracker_error(tk_err, bt_ret);
        return NULL;
    }
    if (-1 == PySlice_GetIndicesEx((PySliceObject *)key, item_count,
                                   &slice_start, &slice_stop,
                                   &slice_step, &slice_length)) {
        return NULL;
    }
    assert(slice_start >= 0 && slice_start <= item_count);
    assert(slice_stop >= 0 && slice_stop <= item_count);
    if (slice_stop < slice_start) {
        // python is permissive and returns an empty list in such
        // case, keep the same behavior instead of returning an error.
        slice_stop = slice_start;
    }
    tk_start = track_box_contents(d_arr->container.box, &tk_err);
    if (NULL == tk_start) {
        assert(NULL != tk_err);
        set_tracker_error(tk_err, tk_err->bt_ret);
        goto end;
    }
    bt_ret = tracker_goto_nth_position(tk_start, slice_start, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        set_tracker_error(tk_err, bt_ret);
        goto end;
    }
    tk_end = track_box_contents(d_arr->container.box, &tk_err);
    if (NULL == tk_end) {
        assert(NULL != tk_err);
        set_tracker_error(tk_err, tk_err->bt_ret);
        goto end;
    }
    bt_ret = tracker_goto_nth_position(tk_end, slice_stop, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        set_tracker_error(tk_err, bt_ret);
        goto end;
    }
    browse_state_init(&bst);
    slice_box = box_new_slice_box(tk_start, tk_end, &bst);
    if (NULL == slice_box) {
        if (NULL != bst.last_error) {
            bt_ret = bst.last_error->bt_ret;
        } else {
            bt_ret = BITPUNCH_DATA_ERROR;
        }
        set_tracker_error(bst.last_error, bt_ret);
        goto end;
    }
    res = box_to_shallow_PyObject(d_arr->container.dtree, slice_box, FALSE);
    box_delete(slice_box);
  end:
    tracker_delete(tk_start);
    tracker_delete(tk_end);
    return res;
}

static PyObject *
DataArray_mp_subscript(PyObject *_d_arr, PyObject *key)
{
    DataArrayObject *d_arr = (DataArrayObject *)_d_arr;

    if (PySlice_Check(key)) {
        return DataArray_mp_subscript_slice(d_arr, key);
    } else {
        return DataArray_mp_subscript_key(d_arr, key);
    }
}

static PyMappingMethods DataArray_as_mapping = {
    .mp_length = DataArray_mp_length,
    .mp_subscript = DataArray_mp_subscript,
    .mp_ass_subscript = NULL
};

static PyObject *
DataArray_getattro(DataBlockObject *self, PyObject *attr_name)
{
    PyObject *attr;
    const char *attr_str;

    attr_str = PyString_AS_STRING(attr_name);
    if (0 == strcmp(attr_str, "__dict__")) {
        return DataContainer_get_dict(&self->container, attr_str);
    }
    attr = PyObject_GenericGetAttr((PyObject *)self, attr_name);
    if (NULL != attr) {
        return attr;
    }
    if (!PyErr_ExceptionMatches(PyExc_AttributeError)) {
        return NULL;
    }
    if (!box_contains_indexed_items(self->container.box)) {
        return NULL;
    }
    PyErr_Clear();
    return DataContainer_get_named_item(&self->container, attr_str);
}

static PyObject *
DataArray_box_to_python_object(struct DataTreeObject *dtree,
                               struct box *box)
{
    PyObject *list = NULL;
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    int64_t item_count;
    PyObject *py_value;
    int64_t index;
    struct tracker_error *tk_err = NULL;

    tk = track_box_contents(box, &tk_err);
    if (NULL == tk) {
        assert(NULL != tk_err);
        bt_ret = tk_err->bt_ret;
        goto tk_error;
    }
    bt_ret = tracker_get_n_items(tk, &item_count, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        goto tk_error;
    }
    list = PyList_New(item_count);
    if (NULL == list) {
        return NULL;
    }
    index = 0;
    bt_ret = tracker_goto_next_item(tk, &tk_err);
    while (BITPUNCH_OK == bt_ret) {
        py_value = tracker_item_to_deep_PyObject(dtree, tk);
        if (NULL == py_value) {
            goto error;
        }
        PyList_SET_ITEM(list, index, py_value);
        ++index;
        bt_ret = tracker_goto_next_item(tk, &tk_err);
    }
    if (BITPUNCH_NO_ITEM != bt_ret) {
        goto tk_error;
    }
    tracker_delete(tk);
    return list;

  tk_error:
    set_tracker_error(tk_err, bt_ret);
  error:
    tracker_delete(tk);
    Py_XDECREF(list);
    return NULL;
}

static PyObject *
DataArray_bytes_box_to_python_object(struct DataTreeObject *dtree,
                                     struct box *box);


static PyMethodDef DataArray_methods[] = {

    { NULL, NULL, 0, NULL }
};

static PyTypeObject DataArrayType = {
    PyObject_HEAD_INIT(NULL)
    0,                           /*ob_size*/
    "bitpunch.DataArray",         /*tp_name*/
    sizeof(DataArrayObject),     /*tp_basicsize*/
    0,                           /*tp_itemsize*/
    0,                           /*tp_dealloc*/
    0,                           /*tp_print*/
    0,                           /*tp_getattr*/
    0,                           /*tp_setattr*/
    0,                           /*tp_compare*/
    0,                           /*tp_repr*/
    0,                           /*tp_as_number*/
    0,                           /*tp_as_sequence*/
    &DataArray_as_mapping,       /*tp_as_mapping*/
    0,                           /*tp_hash */
    0,                           /*tp_call*/
    0,                           /*tp_str*/
    0,                           /*tp_getattro*/
    0,                           /*tp_setattro*/
    0,                           /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,         /*tp_flags*/
    DataArray__doc__,          /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,		               /* tp_richcompare */
    0,		               /* tp_weaklistoffset */
    0,		               /* tp_iter */
    0,		               /* tp_iternext */
    DataArray_methods,         /* tp_methods */
    0,                         /* tp_members */
    0,                         /* tp_getset */
    &DataContainerType,        /* tp_base */
    0,                         /* tp_dict */
    0,                         /* tp_descr_get */
    0,                         /* tp_descr_set */
    0,                         /* tp_dictoffset */
    0,                         /* tp_init */
    0,                         /* tp_alloc */
    0,                         /* tp_new */
};

static PyObject *
DataArray_new(PyTypeObject *subtype,
              PyObject *args, PyObject *kwds)
{
    DataArrayObject *self;

    self = (DataArrayObject *)
        DataContainer_new(subtype, args, kwds);
    if (NULL == self) {
        return NULL;
    }
    return (PyObject *)self;
}

static void
DataArray_dealloc(DataArrayObject *self)
{
    DataContainer_clear(&self->container);
    Py_TYPE(self)->tp_free(self);
}

static int
DataArrayType_setup(void)
{
    DataArrayType.ob_type = &PyType_Type;
    DataArrayType.tp_new = DataArray_new;
    DataArrayType.tp_clear = (inquiry)DataContainer_clear;
    DataArrayType.tp_dealloc = (destructor)DataArray_dealloc;
    DataArrayType.tp_getattro = (getattrofunc)DataArray_getattro;

    if (PyType_Ready(&DataArrayType) < 0) {
        return -1;
    }
    return 0;
}


/*
 * DataTree
 */

PyDoc_STRVAR(DataTree__doc__,
             "Represents the data tree of flat binary contents");

typedef struct DataTreeObject {
    DataBlockObject block;
    FormatSpecObject *fmt;
    struct bitpunch_binary_file_hdl *binary_file;
} DataTreeObject;

static PyTypeObject DataTreeType = {
    PyObject_HEAD_INIT(NULL)
    0,                         /*ob_size*/
    "bitpunch.DataTree",        /*tp_name*/
    sizeof(DataTreeObject),    /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    0,                         /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0,                         /*tp_hash */
    0,                         /*tp_call*/
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    0,                         /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,       /*tp_flags*/
    DataTree__doc__,           /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,		               /* tp_richcompare */
    0,		               /* tp_weaklistoffset */
    0,		               /* tp_iter */
    0,		               /* tp_iternext */
    0,                         /* tp_methods */
    0,                         /* tp_members */
    0,                         /* tp_getset */
    &DataBlockType,            /* tp_base */
    0,                         /* tp_dict */
    0,                         /* tp_descr_get */
    0,                         /* tp_descr_set */
    0,                         /* tp_dictoffset */
    0,                         /* tp_init */
    0,                         /* tp_alloc */
    0,                         /* tp_new */
};

static PyObject *
DataTree_new(PyTypeObject *subtype,
             PyObject *args, PyObject *kwds)
{
    int ret;
    PyObject *bin;
    FormatSpecObject *fmt;
    struct bitpunch_binary_file_hdl *binary_file;
    DataTreeObject *self;
    struct box *container_box;

    if (!PyArg_ParseTuple(args, "OO", &bin, (PyObject **)&fmt)) {
        return NULL;
    }
    if (PyObject_IsInstance((PyObject *)fmt,
                            (PyObject *)&FormatSpecType)) {
        Py_INCREF(fmt);
    } else {
        PyObject *subarg;

        subarg = PyTuple_Pack(1, (PyObject *)fmt);
        if (NULL == subarg) {
            return NULL;
        }
        fmt = (FormatSpecObject *)PyObject_CallObject(
            (PyObject *)&FormatSpecType, subarg);
        Py_DECREF(subarg);
        if (NULL == fmt) {
            return NULL;
        }
    }
    if (PyString_Check(bin)) {
        char *contents;
        Py_ssize_t length;

        /* load the provided text contents */
        ret = PyString_AsStringAndSize(bin, &contents, &length);
        assert(-1 != ret);
        ret = bitpunch_load_binary_file_from_buffer(contents, length,
                                                   &binary_file);
    } else if (PyByteArray_Check(bin)) {
        char *contents;
        Py_ssize_t length;

        /* load the provided text contents */
        contents = PyByteArray_AS_STRING(bin);
        length = PyByteArray_GET_SIZE(bin);
        ret = bitpunch_load_binary_file_from_buffer(contents, length,
                                                   &binary_file);
    } else if (PyFile_Check(bin)) {
        FILE *file;

        /* load the contents from the file object */
        file = PyFile_AsFile(bin);
        //PyFile_IncUseCount((PyFileObject *)bin);
        ret = bitpunch_load_binary_file_from_fd(fileno(file), &binary_file);
    } else {
        PyErr_SetString(PyExc_TypeError,
                        "The first argument must be a string or a file object");
        return NULL;
    }
    if (-1 == ret) {
        PyErr_SetString(PyExc_OSError, "Error loading binary contents");
        return NULL;
    }

    self = (DataTreeObject *)DataContainer_new(subtype, NULL, NULL);
    if (NULL == self) {
        (void) bitpunch_close_binary_file(binary_file);
        return NULL;
    }
    container_box = box_new_from_file(fmt->schema, binary_file);
    if (NULL == container_box) {
        PyErr_SetString(PyExc_OSError, "Error creating top-level box");
        Py_DECREF((PyObject *)self);
        (void) bitpunch_close_binary_file(binary_file);
        return NULL;
    }
    self->block.container.dtree = self;
    Py_INCREF(self->block.container.dtree);

    self->block.container.box = container_box;
    box_acquire(container_box);

    self->binary_file = binary_file;

    self->fmt = fmt;

    return (PyObject *)self;
}

static int
DataTree_clear(DataTreeObject *self)
{
    PyObject *tmp;

    DataContainer_clear(&self->block.container);

    if (NULL != self->binary_file) {
        bitpunch_free_binary_file(self->binary_file);
        self->binary_file = NULL;
    }
    tmp = (PyObject *)self->fmt;
    self->fmt = NULL;
    Py_XDECREF(tmp);
    return 0;
}

static void
DataTree_dealloc(DataTreeObject *self)
{
    DataTree_clear(self);
    Py_TYPE(self)->tp_free(self);
}

static int
DataTreeType_setup(void)
{
    DataTreeType.ob_type = &PyType_Type;
    DataTreeType.tp_new = DataTree_new;
    DataTreeType.tp_clear = (inquiry)DataTree_clear;
    DataTreeType.tp_dealloc = (destructor)DataTree_dealloc;
    if (PyType_Ready(&DataTreeType) < 0) {
        return -1;
    }
    return 0;
}

static PyObject *
DataContainer_box_to_python_object(struct DataTreeObject *dtree,
                                   struct box *box)
{
    switch (dpath_node_get_as_type(&box->dpath)->ndat->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        return DataBlock_box_to_python_object(dtree, box);
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
        return DataArray_box_to_python_object(dtree, box);
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_DATA_FILTER:
        return DataArray_bytes_box_to_python_object(dtree, box);
    default:
        PyErr_Format(PyExc_ValueError,
                     "Cannot convert container type '%s' to python object",
                     ast_node_type_str(box->dpath.item->ndat->type));
        return NULL;
    }
    /*NOT REACHED*/
}

static PyObject *
DataArray_bytes_box_to_python_object(
    struct DataTreeObject *dtree, struct box *box)
{
    PyObject *res = NULL;
    bitpunch_status_t bt_ret;
    expr_value_t value_eval;
    struct tracker_error *tk_err = NULL;

    assert(ast_node_is_item(box->dpath.item));

    bt_ret = box_read_value(box, &value_eval, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        set_tracker_error(tk_err, bt_ret);
        return NULL;
    }
    res = expr_value_to_PyObject(dtree, value_eval, FALSE);
    return res;
}


/*
 * Tracker
 */

#define X_ITER_TYPES                                                    \
    X(ITER_FIELD_NAMES, "iterate over field names")                     \
    X(ITER_FIELD_VALUES, "iterate over field values")                   \
    X(ITER_FIELD_KEYVALUE_TUPLES, "iterate over field's (key, value) tuples") \
    X(ITER_NAMED_EXPR_NAMES, "iterate over all named expressions' names")                 \
    X(ITER_MEMBER_NAMES, "iterate over all member's names (fields and " \
      "named expressions)")                                             \


#define X(NAME, DESC) TRACKER_##NAME,

typedef enum TrackerIterType {
    X_ITER_TYPES
} TrackerIterType;

#undef X

PyDoc_STRVAR(Tracker__doc__,
             "A Tracker object is able to browse items over a "
             "DataTree object. It can iterate a sequence of objects "
             "and enter any complex object to further iterate its "
             "sub-elements.");

typedef struct TrackerObject {
    PyObject_HEAD
    DataTreeObject *dtree;
    struct tracker *tk;
    TrackerIterType iter_mode;
    TrackerIterType current_iter_mode;
    tnamed_expr_iterator named_expr_iter;
} TrackerObject;


static int
Tracker_set_default_iter_mode(TrackerObject *self)
{
    switch (self->tk->box->dpath.item->ndat->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        self->iter_mode = TRACKER_ITER_FIELD_NAMES;
        break ;
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
        if (box_contains_indexed_items(self->tk->box)) {
            self->iter_mode = TRACKER_ITER_FIELD_NAMES;
        } else {
            self->iter_mode = TRACKER_ITER_FIELD_VALUES;
        }
        break ;
    default:
        PyErr_Format(PyExc_TypeError,
                     "container of type '%s' cannot be iterated",
                     ast_node_type_str(self->tk->box->dpath.item->ndat->type));
        return -1;
    }
    self->current_iter_mode = self->iter_mode;
    return 0;
}


/*
 * Tracker move functions
 */

static PyObject *
Tracker_rewind(TrackerObject *self)
{
    tracker_rewind(self->tk);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
Tracker_goto_end(TrackerObject *self)
{
    bitpunch_status_t ret;
    struct tracker_error *tk_err = NULL;

    ret = tracker_goto_end(self->tk, &tk_err);
    if (BITPUNCH_OK != ret) {
        set_tracker_error(tk_err, ret);
        return NULL;
    }
    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

static PyObject *
Tracker_goto_next_item(TrackerObject *self)
{
    bitpunch_status_t ret;
    struct tracker_error *tk_err = NULL;

    ret = tracker_goto_next_item(self->tk, &tk_err);
    if (BITPUNCH_OK != ret) {
        set_tracker_error(tk_err, ret);
        return NULL;
    }
    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

static PyObject *
Tracker_goto_nth_item(TrackerObject *self, PyObject *args)
{
    bitpunch_status_t ret;
    int64_t n;
    struct tracker_error *tk_err = NULL;

    if (!PyArg_ParseTuple(args, "l", &n)) {
        return NULL;
    }
    ret = tracker_goto_nth_item(self->tk, n, &tk_err);
    if (BITPUNCH_OK != ret) {
        set_tracker_error(tk_err, ret);
        return NULL;
    }
    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

static PyObject *
Tracker_goto_named_item(TrackerObject *self, PyObject *args)
{
    bitpunch_status_t ret;
    const char *name;
    struct tracker_error *tk_err = NULL;

    if (!PyArg_ParseTuple(args, "s", &name)) {
        return NULL;
    }
    ret = tracker_goto_named_item(self->tk, name, &tk_err);
    if (BITPUNCH_OK != ret) {
        set_tracker_error(tk_err, ret);
        return NULL;
    }
    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

static int
tk_goto_item_by_key(struct tracker *tk, PyObject *index,
                    expr_value_t *keyp,
                    int *all_twinsp)
{
    bitpunch_status_t ret;
    PyObject *index_value;
    int64_t twin_index;
    int all_twins;
    enum expr_value_type key_expected_type;
    expr_value_t key;
    int indexed_access;
    struct tracker_error *tk_err = NULL;

    all_twins = FALSE;
    indexed_access = FALSE;
    if (PyTuple_Check(index)) {
        PyObject *py_twin_index;

        indexed_access = TRUE;
        switch (PyTuple_GET_SIZE(index)) {
        case 1:
            index_value = PyTuple_GET_ITEM(index, 0);
            twin_index = 0;
            break ;
        case 2:
            index_value = PyTuple_GET_ITEM(index, 0);
            py_twin_index = PyTuple_GET_ITEM(index, 1);
            if (PyString_Check(py_twin_index)
                && 0 == strcmp(PyString_AsString(py_twin_index), "*")) {
                twin_index = 0;
                all_twins = TRUE;
            } else if (PyInt_Check(py_twin_index)) {
                twin_index = PyInt_AS_LONG(py_twin_index);
            } else {
                PyErr_SetString(PyExc_TypeError,
                                "twin index (second element of index as "
                                "tuple) must be an integer or '*'");
                return -1;
            }
            break ;
        default:
            PyErr_SetString(PyExc_ValueError,
                            "Index as tuple must have one or two elements");
            return -1;
        }
    } else {
        index_value = index;
        twin_index = 0;
    }
    if (-1 == expr_value_from_PyObject(index_value, &key)) {
        return -1;
    }
    if (box_contains_indexed_items(tk->box)) {
        key_expected_type = box_get_index_type(tk->box);
        if (key.type != EXPR_VALUE_TYPE_INTEGER) {
            indexed_access = TRUE;
        }
    } else {
        key_expected_type = EXPR_VALUE_TYPE_INTEGER;
    }
    if (indexed_access) {
        if (!box_contains_indexed_items(tk->box)) {
            PyErr_Format(PyExc_TypeError,
                         "requested indexed access by index tuple on "
                         "non-indexed array");
            return -1;
        }
        if (key.type != key_expected_type) {
            PyErr_Format(PyExc_TypeError,
                         "key of type '%s' does not match index type '%s'",
                         expr_value_type_str(key.type),
                         expr_value_type_str(key_expected_type));
            return -1;
        }
        ret = tracker_goto_nth_item_with_key(tk, key, twin_index, &tk_err);
    } else {
        if (key.type != EXPR_VALUE_TYPE_INTEGER) {
            PyErr_SetString(PyExc_TypeError,
                            "key of non-indexed array must be an integer");
            return -1;
        }
        ret = tracker_goto_nth_item(tk, key.integer, &tk_err);
    }
    if (BITPUNCH_OK != ret) {
        set_tracker_error(tk_err, ret);
        return -1;
    }
    if (NULL != keyp) {
        *keyp = key;
    }
    if (NULL != all_twinsp) {
        *all_twinsp = all_twins;
    }
    return 0;
}

static PyObject *
Tracker_goto_item_by_key(TrackerObject *self, PyObject *index)
{
    if (-1 == tk_goto_item_by_key(self->tk, index, NULL, NULL)) {
        return NULL;
    }
    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}


static PyObject *
Tracker_enter_item(TrackerObject *self)
{
    bitpunch_status_t ret;
    struct tracker_error *tk_err = NULL;

    ret = tracker_enter_item(self->tk, &tk_err);
    if (BITPUNCH_OK != ret) {
        set_tracker_error(tk_err, ret);
        return NULL;
    }
    if (-1 == Tracker_set_default_iter_mode(self)) {
        return NULL;
    }
    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

static PyObject *
Tracker_leave_container(TrackerObject *self)
{
    bitpunch_status_t ret;
    struct tracker_error *tk_err = NULL;

    ret = tracker_return(self->tk, &tk_err);
    if (BITPUNCH_OK != ret) {
        set_tracker_error(tk_err, ret);
        return NULL;
    }
    if (-1 == Tracker_set_default_iter_mode(self)) {
        return NULL;
    }
    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

/*
 * Tracker container functions
 */

static PyObject *
Tracker_get_n_items(TrackerObject *self)
{
    bitpunch_status_t ret;
    int64_t n_items;
    struct tracker_error *tk_err = NULL;

    ret = tracker_get_n_items(self->tk, &n_items, &tk_err);
    if (BITPUNCH_OK != ret) {
        set_tracker_error(tk_err, ret);
        return NULL;
    }
    return Py_BuildValue("l", n_items);
}

/*
 * Tracker item functions
 */

static PyObject *
Tracker_get_item_key(TrackerObject *self)
{
    bitpunch_status_t bt_ret;
    expr_value_t key;
    int twin_index;
    PyObject *py_key_value;
    PyObject *py_key_arg;
    PyObject *res;
    struct tracker_error *tk_err = NULL;

    bt_ret = tracker_get_item_key_multi(self->tk, &key, &twin_index, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        set_tracker_error(tk_err, bt_ret);
        return NULL;
    }
    py_key_value = expr_value_to_PyObject(self->dtree, key, FALSE);
    if (NULL == py_key_value) {
        return NULL;
    }
    if (box_contains_indexed_items(self->tk->box)) {
        if (0 == twin_index) {
            py_key_arg = Py_BuildValue("(O)", py_key_value);
        } else {
            py_key_arg = Py_BuildValue("((Oi))", py_key_value, twin_index);
        }
        Py_DECREF(py_key_value);
        if (NULL == py_key_arg) {
            return NULL;
        }
        res = IndexKey_new(&IndexKeyType, py_key_arg, NULL);
        Py_DECREF(py_key_arg);
    } else {
        res = py_key_value;
    }
    return res;
}

static PyObject *
Tracker_get_size(TrackerObject *self)
{
    int64_t item_size;
    bitpunch_status_t bt_ret;
    struct tracker_error *tk_err = NULL;

    bt_ret = tracker_get_item_size(self->tk, &item_size, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        set_tracker_error(tk_err, bt_ret);
        return NULL;
    }
    return PyInt_FromLong(item_size);
}

static PyObject *
Tracker_get_offset(TrackerObject *self)
{
    int64_t item_offset;
    bitpunch_status_t bt_ret;
    struct tracker_error *tk_err = NULL;

    bt_ret = tracker_get_item_offset(self->tk, &item_offset, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        set_tracker_error(tk_err, bt_ret);
        return NULL;
    }
    return PyInt_FromLong(item_offset);
}

static PyObject *
Tracker_get_location(TrackerObject *self)
{
    bitpunch_status_t ret;
    int64_t item_offset;
    int64_t item_size;
    struct tracker_error *tk_err = NULL;

    ret = tracker_get_item_location(self->tk, &item_offset, &item_size,
                                    &tk_err);
    if (BITPUNCH_OK != ret) {
        set_tracker_error(tk_err, ret);
        return NULL;
    }
    return Py_BuildValue("ii", item_offset, item_size);
}

static PyObject *
Tracker_read_item_raw(TrackerObject *self)
{
    bitpunch_status_t ret;
    const char *item_contents;
    int64_t item_size;
    Py_buffer view;
    PyObject *memview;
    struct tracker_error *tk_err = NULL;

    ret = tracker_read_item_raw(self->tk, &item_contents, &item_size,
                                &tk_err);
    if (BITPUNCH_OK != ret) {
        set_tracker_error(tk_err, ret);
        return NULL;
    }
    if (-1 == PyBuffer_FillInfo(&view, NULL,
                                (void *)item_contents, item_size,
                                TRUE /* read-only */, PyBUF_FULL_RO)) {
        return NULL;
    }
    memview = PyMemoryView_FromBuffer(&view); /* view gets stolen */
    return memview;
}

static PyObject *
Tracker_read_item_value(TrackerObject *self)
{
    return tracker_item_to_shallow_PyObject(self->dtree, self->tk);
}

static PyMethodDef Tracker_methods[] = {

    /* move functions */

    { "rewind",
      (PyCFunction)Tracker_rewind, METH_NOARGS,
      "Rewind the tracker to the beginning of the tracked container."
    },

    { "goto_end",
      (PyCFunction)Tracker_goto_end, METH_NOARGS,
      "Move the tracker to the end of the container (i.e. pointing\n"
      "after the last element)."
    },

    { "goto_next_item",
      (PyCFunction)Tracker_goto_next_item, METH_NOARGS,
      "Advance the tracker to the next item in the container.\n"
      "\n"
      "Raise NoItemError if there are no more items."
    },

    { "goto_nth_item",
      (PyCFunction)Tracker_goto_nth_item, METH_VARARGS,
      "Move the tracker to the n-th item in the container's order "
      "(starting from 0).\n"
      "\n"
      "Raise NoItemError if the number of items is less or equals n."
    },

    { "goto_named_item",
      (PyCFunction)Tracker_goto_named_item, METH_VARARGS,
      "Move the tracker to the item with the given name.\n"
      "\n"
      "Raise NoItemError if no item exists with such name."
    },

    { "goto_item_by_key",
      (PyCFunction)Tracker_goto_item_by_key, METH_O,
      "Move the tracker to an item in the container\n"
      "indexed by the given key. If the argument is a tuple with two\n"
      "elements (index, n), move to the n-th item (starting from 0)\n"
      "matching the index.\n"
      "\n"
      "Raise NoItemError if no item has such indexed key, or if given\n"
      "a tuple and if the number of items matching the index is less\n"
      "or equals n."
    },

    { "enter_item",
      (PyCFunction)Tracker_enter_item, METH_NOARGS,
      "Enter the target child item, in order to further iterate over it. "
      "The tracker will be at beginning position, not pointing to any "
      "element."
    },

    { "leave_container",
      (PyCFunction)Tracker_leave_container, METH_NOARGS,
      "Leave the tracked container, returning to the parent container. "
      "The tracker will then point to the previously tracked container as "
      "an item."
    },

    /* container functions */

    { "get_n_items",
      (PyCFunction)Tracker_get_n_items, METH_NOARGS,
      "get the number of items in the currently tracked container"
    },

    /* item functions */

    { "get_item_key",
      (PyCFunction)Tracker_get_item_key, METH_NOARGS,
      "Return the item's key:\n"
      " - as integer if tracking an non-indexed array\n"
      " - as the index data type if tracking an indexed array\n"
      " - as string if tracking a block"
    },

    { "get_size",
      (PyCFunction)Tracker_get_size, METH_NOARGS,
      "get the target item's byte size in the file or filtered "
      "byte contents"
    },

    { "get_offset",
      (PyCFunction)Tracker_get_offset, METH_NOARGS,
      "get the target item's absolute byte offset in the file or "
      "filtered byte contents"
    },

    { "get_location",
      (PyCFunction)Tracker_get_location, METH_NOARGS,
      "get the target item's location in the file or filtered "
      "byte contents, as a (offset, size) tuple"
    },

    { "read_item_raw",
      (PyCFunction)Tracker_read_item_raw, METH_NOARGS,
      "get the target item's byte data, as a memoryview object"
    },

    { "read_item_value",
      (PyCFunction)Tracker_read_item_value, METH_NOARGS,
      "get the target item's value, either as a DataContainer object for "
      "structured types (blocks and arrays) or as native Python types"
    },

    { NULL, NULL, 0, NULL }
};

#define X(NAME, DESC) "    Tracker."#NAME" -- "DESC"\n"

static PyMemberDef Tracker_members[] = {
    { "iter_mode", T_INT, offsetof (TrackerObject, iter_mode), 0,
      "Iteration mode:\n"
      X_ITER_TYPES
    },

    { NULL, 0, 0, 0, NULL }
};

#undef X

static int
Tracker_bf_getbuffer(TrackerObject *exporter,
                     Py_buffer *view, int flags);


static PyBufferProcs TrackerAsBuffer = {
    .bf_getbuffer = (getbufferproc)Tracker_bf_getbuffer,
};

static PyTypeObject TrackerType = {
    PyObject_HEAD_INIT(NULL)
    0,                         /*ob_size*/
    "bitpunch.Tracker",         /*tp_name*/
    sizeof(TrackerObject),     /*tp_basicsize*/
    0,                         /*tp_itemsize*/
    0,                         /*tp_dealloc*/
    0,                         /*tp_print*/
    0,                         /*tp_getattr*/
    0,                         /*tp_setattr*/
    0,                         /*tp_compare*/
    0,                         /*tp_repr*/
    0,                         /*tp_as_number*/
    0,                         /*tp_as_sequence*/
    0,                         /*tp_as_mapping*/
    0,                         /*tp_hash */
    0,                         /*tp_call*/
    0,                         /*tp_str*/
    0,                         /*tp_getattro*/
    0,                         /*tp_setattro*/
    &TrackerAsBuffer,          /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE |
    Py_TPFLAGS_HAVE_NEWBUFFER, /*tp_flags*/
    Tracker__doc__,            /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,		               /* tp_richcompare */
    0,		               /* tp_weaklistoffset */
    0,		               /* tp_iter */
    0,		               /* tp_iternext */
    Tracker_methods,           /* tp_methods */
    Tracker_members,           /* tp_members */
    0,                         /* tp_getset */
    0,                         /* tp_base */
    0,                         /* tp_dict */
    0,                         /* tp_descr_get */
    0,                         /* tp_descr_set */
    0,                         /* tp_dictoffset */
    0,                         /* tp_init */
    0,                         /* tp_alloc */
    0,                         /* tp_new */
};


static TrackerObject *
Tracker_new_from_DataContainer(PyTypeObject *subtype,
                               DataContainerObject *dcont)
{
    TrackerObject *self;
    struct tracker_error *tk_err = NULL;

    self = (TrackerObject *)subtype->tp_alloc(subtype, 0);
    if (NULL == self) {
        return NULL;
    }
    self->dtree = dcont->dtree;
    Py_INCREF(self->dtree);
    self->tk = track_box_contents(dcont->box, &tk_err);
    if (NULL == self->tk) {
        assert(NULL != tk_err);
        set_tracker_error(tk_err, tk_err->bt_ret);
        subtype->tp_free(self);
        return NULL;
    }
    if (-1 == Tracker_set_default_iter_mode(self)) {
        subtype->tp_free(self);
        return NULL;
    }
    return self;
}

static TrackerObject *
Tracker_new_from_tk(PyTypeObject *subtype,
                    DataTreeObject *dtree,
                    struct tracker *tk)
{
    TrackerObject *self;

    self = (TrackerObject *)subtype->tp_alloc(subtype, 0);
    if (NULL == self) {
        return NULL;
    }
    self->dtree = dtree;
    Py_INCREF(dtree);
    self->tk = tracker_dup(tk);
    if (NULL == self->tk) {
        Py_DECREF((PyObject *)self);
        PyErr_SetString(PyExc_OSError, "Error creating tracker");
        return NULL;
    }
    Tracker_set_default_iter_mode(self);
    return self;
}

static TrackerObject *
Tracker_new_from_Tracker(PyTypeObject *subtype,
                         TrackerObject *tkobj)
{
    TrackerObject *self;

    self = (TrackerObject *)subtype->tp_alloc(subtype, 0);
    if (NULL == self) {
        return NULL;
    }
    self->dtree = tkobj->dtree;
    Py_INCREF(self->dtree);
    self->tk = tracker_dup(tkobj->tk);
    if (NULL == self->tk) {
        Py_DECREF((PyObject *)self);
        PyErr_SetString(PyExc_OSError, "Error duplicating tracker");
        return NULL;
    }
    self->iter_mode = tkobj->iter_mode;
    return self;
}

static PyObject *
Tracker_new(PyTypeObject *subtype,
            PyObject *args, PyObject *kwds)
{
    TrackerObject *self;
    static char *kwlist[] = { "from", "iter_mode", NULL };
    PyObject *arg;
    int iter_mode = -1;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "O|i", kwlist,
                                     &arg, &iter_mode)) {
        return NULL;
    }
    if (PyObject_TypeCheck(arg, &DataContainerType)) {
        self = Tracker_new_from_DataContainer(subtype,
                                              (DataContainerObject *)arg);
        if (NULL == self) {
            return NULL;
        }
    } else if (PyObject_TypeCheck(arg, &TrackerType)) {
        self = Tracker_new_from_Tracker(subtype, (TrackerObject *)arg);
        if (NULL == self) {
            return NULL;
        }
    } else {
        PyErr_SetString(PyExc_TypeError,
                        "The argument must be a bitpunch.DataContainer "
                        "or bitpunch.Tracker object");
        return NULL;
    }
#define X(NAME, DESC) case TRACKER_##NAME:

    if (-1 != iter_mode) {
        switch (iter_mode) {
            X_ITER_TYPES
                self->iter_mode = iter_mode;
            break ;
        default:
            PyErr_Format(PyExc_ValueError,
                         "'%d' is not a valid iter_mode parameter",
                         iter_mode);
            Py_DECREF((PyObject *)self);
            return NULL;
        }
    }

#undef X

    return (PyObject *)self;
}

static int
Tracker_clear(TrackerObject *self)
{
    tracker_delete(self->tk);
    self->tk = NULL;

    Py_CLEAR(self->dtree);
    return 0;
}

static void
Tracker_dealloc(TrackerObject *self)
{
    Tracker_clear(self);
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
Tracker_iter(TrackerObject *self)
{
    Py_INCREF((PyObject *)self);
    self->current_iter_mode = self->iter_mode;
    if (TRACKER_ITER_NAMED_EXPR_NAMES == self->iter_mode) {
        self->named_expr_iter = box_iter_named_exprs(self->tk->box);
    }
    return (PyObject *)self;
}

static PyObject *
Tracker_iternext(TrackerObject *self)
{
    if (TRACKER_ITER_NAMED_EXPR_NAMES != self->current_iter_mode) {
        if (NULL == Tracker_goto_next_item(self)) {
            if (PyErr_ExceptionMatches(BitpunchExc_NoItemError)) {
                PyErr_Clear();
                if (TRACKER_ITER_MEMBER_NAMES == self->current_iter_mode) {
                    self->current_iter_mode = TRACKER_ITER_NAMED_EXPR_NAMES;
                    self->named_expr_iter = box_iter_named_exprs(self->tk->box);
                } else {
                    /* StopIteration is implicitly set by the API */
                    return NULL;
                }
            } else {
                return NULL;
            }
        }
    }
    switch (self->current_iter_mode) {
    case TRACKER_ITER_FIELD_NAMES:
    case TRACKER_ITER_MEMBER_NAMES:
        return Tracker_get_item_key(self);
    case TRACKER_ITER_FIELD_VALUES:
        return Tracker_read_item_value(self);
    case TRACKER_ITER_FIELD_KEYVALUE_TUPLES: {
        PyObject *key;
        PyObject *value;
        PyObject *res;

        key = Tracker_get_item_key(self);
        if (NULL == key) {
            return NULL;
        }
        value = Tracker_read_item_value(self);
        if (NULL == value) {
            Py_DECREF(key);
            return NULL;
        }
        res = PyTuple_Pack(2, key, value);
        Py_DECREF(key);
        Py_DECREF(value);
        return res;
    }
    case TRACKER_ITER_NAMED_EXPR_NAMES: {
        bitpunch_status_t bt_ret;
        const struct named_expr *named_expr;
        struct tracker_error *tk_err = NULL;

        bt_ret = box_iter_named_exprs_next(self->tk->box,
                                           &self->named_expr_iter,
                                           &named_expr, &tk_err);
        if (BITPUNCH_NO_ITEM == bt_ret) {
            PyErr_Clear();
            /* StopIteration is implicitly set by the API */
            return NULL;
        }
        if (BITPUNCH_OK != bt_ret) {
            set_tracker_error(tk_err, bt_ret);
            return NULL;
        }
        return PyString_FromString(named_expr->nstmt.name);
    }
    default:
        PyErr_Format(PyExc_ValueError,
                     "Invalid value for Tracker.iter_mode: %d",
                     self->iter_mode);
        return NULL;
    }
}

static int
Tracker_bf_getbuffer(TrackerObject *exporter,
                     Py_buffer *view, int flags)
{
    struct tracker *tk;
    int64_t item_offset;
    int64_t item_size;
    bitpunch_status_t bt_ret;
    const char *buf;
    int64_t len;
    struct tracker_error *tk_err = NULL;

    tk = exporter->tk;
    bt_ret = tracker_get_item_location(tk, &item_offset, &item_size,
                                       &tk_err);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = box_apply_filter(tk->box, &tk_err);
    }
    if (BITPUNCH_OK != bt_ret) {
        PyObject *errobj;

        if (NULL != tk_err) {
            errobj = tracker_error_get_info_as_PyObject(tk_err);
        } else {
            Py_INCREF(Py_None);
            errobj = Py_None;
        }
        PyErr_SetObject(PyExc_BufferError, errobj);
        Py_DECREF(errobj);
        tracker_error_destroy(tk_err);
        view->obj = NULL;
        return -1;
    }
    buf = tk->box->file_hdl->bf_data + item_offset;
    len = item_size;
    return PyBuffer_FillInfo(view, (PyObject *)exporter,
                             (void *)buf, (Py_ssize_t)len,
                             TRUE /* read-only */, flags);
}

static int
TrackerType_setup(void)
{
    TrackerType.ob_type = &PyType_Type;
    TrackerType.tp_new = Tracker_new;
    TrackerType.tp_clear = (inquiry)Tracker_clear;
    TrackerType.tp_dealloc = (destructor)Tracker_dealloc;
    TrackerType.tp_iter = (getiterfunc)Tracker_iter;
    TrackerType.tp_iternext = (iternextfunc)Tracker_iternext;
    if (PyType_Ready(&TrackerType) < 0) {
        return -1;
    }

#define X(NAME, DESC) do {                                      \
        PyObject *val = PyInt_FromLong(TRACKER_##NAME);         \
        if (NULL == val) {                                      \
            return -1;                                          \
        }                                                       \
        PyDict_SetItemString(TrackerType.tp_dict, #NAME, val);  \
        Py_DECREF(val);                                         \
    } while (0);

    X_ITER_TYPES

#undef X

        return 0;
}


static PyObject *
DataContainer_iter_keys(DataContainerObject *self)
{
    TrackerObject *tracker;

    tracker = Tracker_new_from_DataContainer(&TrackerType, self);
    if (NULL != tracker) {
        tracker->iter_mode = TRACKER_ITER_FIELD_NAMES;
    }
    return (PyObject *)tracker;
}

static PyObject *
DataContainer_iter_items(DataContainerObject *self)
{
    TrackerObject *tracker;

    tracker = Tracker_new_from_DataContainer(&TrackerType, self);
    if (NULL != tracker) {
        tracker->iter_mode = TRACKER_ITER_FIELD_KEYVALUE_TUPLES;
    }
    return (PyObject *)tracker;
}

static PyObject *
DataContainer_eval_expr(DataContainerObject *cont,
                        PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = { "expr", "tracker", NULL };
    const char *expr;
    int tracker = FALSE;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "s|i", kwlist,
                                     &expr, &tracker)) {
        return NULL;
    }
    return eval_expr_as_python_object(cont, expr, tracker);
}

static PyObject *
DataContainer_get_size(DataContainerObject *self, PyObject *args)
{
    int64_t start_offset;
    int64_t item_size;
    bitpunch_status_t bt_ret;
    struct tracker_error *tk_err = NULL;
    int64_t end_offset;

    bt_ret = box_compute_end_offset(self->box, BOX_END_OFFSET_USED,
                                    &end_offset, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        set_tracker_error(tk_err, bt_ret);
        return NULL;
    }
    start_offset = box_get_start_offset(self->box);
    assert(end_offset >= start_offset);
    item_size = end_offset - start_offset;
    return PyInt_FromLong(item_size);
}

static PyObject *
DataContainer_get_offset(DataContainerObject *self, PyObject *args)
{
    int64_t item_offset;

    item_offset = box_get_start_offset(self->box);
    return PyInt_FromLong(item_offset);
}

static PyObject *
DataContainer_get_location(DataContainerObject *self, PyObject *args)
{
    int64_t item_offset;
    int64_t item_size;
    bitpunch_status_t bt_ret;
    struct tracker_error *tk_err = NULL;
    int64_t end_offset;

    bt_ret = box_compute_end_offset(self->box, BOX_END_OFFSET_USED,
                                    &end_offset, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        set_tracker_error(tk_err, bt_ret);
        return NULL;
    }
    assert(end_offset >= box_get_start_offset(self->box));
    item_offset = box_get_start_offset(self->box);
    item_size = end_offset - item_offset;

    return Py_BuildValue("ii", item_offset, item_size);
}

static PyObject *
DataContainer_str(DataContainerObject *self)
{
    bitpunch_status_t bt_ret;
    expr_value_t value;
    PyObject *res = NULL;
    struct tracker_error *tk_err = NULL;

    bt_ret = box_read_value(self->box, &value, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        set_tracker_error(tk_err, bt_ret);
        return NULL;
    }
    assert(EXPR_VALUE_TYPE_BYTES == value.type);
    res = PyString_FromStringAndSize(value.bytes.buf,
                                     (Py_ssize_t)value.bytes.len);
    expr_value_destroy(value);
    return res;
}

static PyObject *
DataContainer___unicode__(DataContainerObject *self, PyObject *args)
{
    struct box *box;
    bitpunch_status_t bt_ret;
    expr_value_t value;
    struct tracker_error *tk_err = NULL;

    box = self->box;
    bt_ret = box_read_value(box, &value, &tk_err);
    if (BITPUNCH_OK != bt_ret) {
        set_tracker_error(tk_err, bt_ret);
        return NULL;
    }
    assert(EXPR_VALUE_TYPE_BYTES == value.type);
    // FIXME: use proper encoding configured in string() filter when
    // this is implemented
    return PyUnicode_Decode(value.bytes.buf, (Py_ssize_t)value.bytes.len,
                            "latin_1", NULL);
}

static PyObject *
DataContainer_iter(DataContainerObject *self)
{
    TrackerObject *iter;

    iter = Tracker_new_from_DataContainer(&TrackerType, self);
    return (PyObject *)iter;
}

static int
DataContainer_bf_getbuffer(DataContainerObject *exporter,
                           Py_buffer *view, int flags)
{
    struct box *box;
    int64_t end_offset;
    bitpunch_status_t bt_ret;
    const char *buf;
    int64_t len;
    struct tracker_error *tk_err = NULL;

    box = exporter->box;
    bt_ret = box_compute_end_offset(box, BOX_END_OFFSET_USED, &end_offset,
                                    &tk_err);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = box_apply_filter(box, &tk_err);
    }
    if (BITPUNCH_OK != bt_ret) {
        PyObject *errmsg;

        if (NULL != tk_err) {
            errmsg = PyString_FromString(tk_err->error_buf);
        } else {
            Py_INCREF(Py_None);
            errmsg = Py_None;
        }
        PyErr_SetObject(PyExc_BufferError, errmsg);
        Py_DECREF(errmsg);
        view->obj = NULL;
        tracker_error_destroy(tk_err);
        return -1;
    }
    buf = box->file_hdl->bf_data + box_get_start_offset(box);
    len = end_offset - box_get_start_offset(box);
    return PyBuffer_FillInfo(view, (PyObject *)exporter,
                             (void *)buf, (Py_ssize_t)len,
                             TRUE /* read-only */, flags);
}


static int
expr_value_from_PyObject(PyObject *py_expr,
                         expr_value_t *exprp)
{
    if (PyInt_Check(py_expr)) {
        exprp->type = EXPR_VALUE_TYPE_INTEGER;
        exprp->integer = PyInt_AS_LONG(py_expr);
    } else if (PyBool_Check(py_expr)) {
        exprp->type = EXPR_VALUE_TYPE_BOOLEAN;
        exprp->boolean = (py_expr == Py_True);
    } else if (PyString_Check(py_expr)) {
        char *str;
        Py_ssize_t length;

        exprp->type = EXPR_VALUE_TYPE_STRING;
        PyString_AsStringAndSize(py_expr, &str, &length);
        exprp->string.str = str;
        exprp->string.len = length;
    } else if (PyByteArray_Check(py_expr)) {
        exprp->type = EXPR_VALUE_TYPE_BYTES;
        exprp->bytes.buf = PyByteArray_AS_STRING(py_expr);
        exprp->bytes.len = PyByteArray_GET_SIZE(py_expr);
    } else {
        PyErr_Format(PyExc_TypeError,
                     "unsupported expression type '%s'",
                     Py_TYPE(py_expr)->tp_name);
        return -1;
    }
    return 0;
}

static PyObject *
expr_value_to_shallow_PyObject(DataTreeObject *dtree,
                               expr_value_t value_eval)
{
    struct browse_state bst;
    struct box *box;
    bitpunch_status_t bt_ret;
    PyObject *res = NULL;

    browse_state_init(&bst);
    box = box_new_from_expr_value(value_eval, &bst);
    if (NULL == box) {
        if (NULL != bst.last_error) {
            bt_ret = bst.last_error->bt_ret;
        } else {
            bt_ret = BITPUNCH_DATA_ERROR;
        }
        set_tracker_error(bst.last_error, bt_ret);
        return NULL;
    }
    res = box_to_shallow_PyObject(dtree, box, TRUE);
    box_delete(box);
    return res;
}

/**
 * @brief convert an expression into a python object
 *
 * @note this function call always destroys @ref value_eval
 */
static PyObject *
expr_value_to_PyObject(DataTreeObject *dtree,
                       expr_value_t value_eval,
                       int tracker)
{
    PyObject *res = NULL;

    switch (value_eval.type) {
    case EXPR_VALUE_TYPE_INTEGER:
        res = Py_BuildValue("l", value_eval.integer);
        goto end;
    case EXPR_VALUE_TYPE_BOOLEAN:
        res = PyBool_FromLong(value_eval.boolean);
        goto end;
    case EXPR_VALUE_TYPE_STRING:
        if (tracker) {
            res = expr_value_to_shallow_PyObject(dtree, value_eval);
        } else {
            res = PyString_FromStringAndSize(
                value_eval.string.str,
                (Py_ssize_t)value_eval.string.len);
        }
        goto end;
    case EXPR_VALUE_TYPE_BYTES:
        if (tracker) {
            res = expr_value_to_shallow_PyObject(dtree, value_eval);
        } else {
            res = PyString_FromStringAndSize(
                value_eval.bytes.buf,
                (Py_ssize_t)value_eval.bytes.len);
        }
        goto end;
    default:
        PyErr_Format(PyExc_ValueError,
                     "Unsupported expression type '%d'", (int)value_eval.type);
        goto end;
    }
  end:
    expr_value_destroy(value_eval);
    return res;
}

/**
 * @brief convert a dpath expression into a python object
 *
 * @note this function call always destroys @ref dpath_eval
 */
static PyObject *
expr_dpath_to_PyObject(DataTreeObject *dtree,
                       expr_dpath_t dpath_eval,
                       int tracker)
{
    PyObject *res = NULL;

    switch (dpath_eval.type) {
    case EXPR_DPATH_TYPE_ITEM:
        if (tracker) {
            res = (PyObject *)
                Tracker_new_from_tk(&TrackerType,
                                    dtree, dpath_eval.item.tk);
        } else {
            res = tracker_item_to_shallow_PyObject(dtree,
                                                   dpath_eval.item.tk);
        }
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        res = box_to_shallow_PyObject(dtree, dpath_eval.container.box,
                                      tracker);
        break ;
    default:
        assert(0);
    }
    expr_dpath_destroy(dpath_eval);
    return res;
}

static PyObject *
eval_expr_as_python_object(DataContainerObject *cont,
                           const char *expr, int tracker)
{
    DataTreeObject *dtree;
    struct bitpunch_schema_hdl *schema;
    struct bitpunch_binary_file_hdl *binary_file;
    struct box *scope;
    int ret;
    expr_value_t expr_value;
    expr_dpath_t expr_dpath;
    PyObject *res = NULL;
    struct tracker_error *tk_err = NULL;

    if (NULL != cont) {
        dtree = cont->dtree;
        schema = dtree->fmt->schema;
        binary_file = dtree->binary_file;
        scope = cont->box;
    } else {
        dtree = NULL;
        schema = NULL;
        binary_file = NULL;
        scope = NULL;
    }

    ret = bitpunch_eval_expr(schema, binary_file, expr, scope,
                             &expr_value, &expr_dpath,
                             &tk_err);
    if (-1 == ret) {
        if (NULL != tk_err) {
            set_tracker_error(tk_err, tk_err->bt_ret);
        } else {
            PyErr_Format(PyExc_ValueError,
                         "Error evaluating expression '%s'", expr);
        }
        return NULL;
    }
    // FIXME rework choice between dpath or value type
    if (EXPR_DPATH_TYPE_NONE != expr_dpath.type
        && (tracker || EXPR_VALUE_TYPE_INTEGER != expr_value.type)) {
        res = expr_dpath_to_PyObject(dtree, expr_dpath, tracker);
        expr_value_destroy(expr_value);
    } else {
        assert(EXPR_VALUE_TYPE_UNSET != expr_value.type);
        expr_dpath_destroy(expr_dpath);
        res = expr_value_to_PyObject(dtree, expr_value, tracker);
    }
    return res;
}

static PyObject *
tracker_item_to_deep_PyObject(DataTreeObject *dtree,
                              struct tracker *tk)
{
    PyObject *res = NULL;
    bitpunch_status_t bt_ret;
    expr_value_t value_eval;
    int complex_type;
    struct box *filtered_box;
    struct tracker_error *tk_err = NULL;

    if (NULL == tk->dpath) {
        Py_INCREF(Py_None);
        return Py_None;
    }
    complex_type = dpath_is_complex_type(tk->dpath);
    if (complex_type) {
        bt_ret = tracker_get_filtered_item_box(tk, &filtered_box, &tk_err);
        if (BITPUNCH_OK == bt_ret) {
            res = DataContainer_box_to_python_object(dtree, filtered_box);
            box_delete(filtered_box);
        } else {
            set_tracker_error(tk_err, bt_ret);
        }
    } else {
        bt_ret = tracker_read_item_value(tk, &value_eval, &tk_err);
        if (BITPUNCH_OK != bt_ret) {
            set_tracker_error(tk_err, bt_ret);
            return NULL;
        }
        res = expr_value_to_PyObject(dtree, value_eval, FALSE);
    }
    return res;
}

static PyObject *
tracker_item_to_shallow_PyObject(DataTreeObject *dtree,
                                 struct tracker *tk)
{
    PyObject *res = NULL;
    bitpunch_status_t bt_ret;
    int complex_type;
    struct box *filtered_box;
    struct tracker_error *tk_err = NULL;

    if (NULL == tk->dpath) {
        Py_INCREF(Py_None);
        return Py_None;
    }
    complex_type = dpath_is_complex_type(tk->dpath);
    if (complex_type) {
        bt_ret = tracker_get_filtered_item_box(tk, &filtered_box, &tk_err);
        if (BITPUNCH_OK != bt_ret) {
            set_tracker_error(tk_err, bt_ret);
            return NULL;
        }
        res = box_to_shallow_PyObject(dtree, filtered_box, FALSE);
        box_delete(filtered_box);
    } else {
        expr_value_t value_eval;

        bt_ret = tracker_read_item_value(tk, &value_eval, &tk_err);
        if (BITPUNCH_OK != bt_ret) {
            set_tracker_error(tk_err, bt_ret);
            return NULL;
        }
        res = expr_value_to_PyObject(dtree, value_eval, TRUE);
    }
    return res;
}

static PyObject *
box_to_shallow_PyObject(DataTreeObject *dtree, struct box *box,
                        int tracker)
{
    struct ast_node_hdl *as_type;
    DataContainerObject *dcont;

    as_type = dpath_node_get_as_type(&box->dpath);
    switch (as_type->ndat->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        dcont = (DataContainerObject *)DataBlock_new(&DataBlockType,
                                                     NULL, NULL);
        if (NULL == dcont) {
            return NULL;
        }
        DataContainer_construct(dcont, dtree, box);
        return (PyObject *)dcont;

    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_DATA_FILTER:
        dcont = (DataContainerObject *)DataArray_new(&DataArrayType,
                                                     NULL, NULL);
        if (NULL == dcont) {
            return NULL;
        }
        DataContainer_construct(dcont, dtree, box);
        return (PyObject *)dcont;

    default:
        PyErr_Format(PyExc_TypeError,
                     "cannot convert box type '%s' to shallow python "
                     "object", ast_node_type_str(as_type->ndat->type));
        return NULL;
    }
    /*NOT REACHED*/
}

/*
 * bitpunch
 */

static int
setup_exceptions(PyObject *bitpunch_m)
{
    BitpunchExc_NoItemError = PyErr_NewException("bitpunch.NoItemError",
                                                NULL, NULL);
    PyModule_AddObject(bitpunch_m, "NoItemError", BitpunchExc_NoItemError);

    BitpunchExc_DataError = PyErr_NewException("bitpunch.DataError",
                                              NULL, NULL);
    PyModule_AddObject(bitpunch_m, "DataError", BitpunchExc_DataError);

    BitpunchExc_OutOfBoundsError =
        PyErr_NewException("bitpunch.OutOfBoundsError", NULL, NULL);
    PyModule_AddObject(bitpunch_m, "OutOfBoundsError",
                       BitpunchExc_OutOfBoundsError);
    return 0;
}


/* bitpunch methods */

static PyObject *
mod_bitpunch_make_python_object(PyObject *self, PyObject *obj)
{
    if (PyObject_TypeCheck(obj, &DataContainerType)) {
        return DataContainer_make_python_object((DataContainerObject *)obj);
    } else {
        Py_INCREF(obj);
        return obj;
    }
}

static PyObject *
mod_bitpunch_eval_expr(PyObject *self, PyObject *args)
{
    const char *expr;

    if (!PyArg_ParseTuple(args, "s|Oi", &expr)) {
        return NULL;
    }
    return eval_expr_as_python_object(NULL, expr, FALSE);
}

static PyObject *
mod_bitpunch_get_builtin_names(PyObject *self,
                              PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = { "prefix", "object", NULL };
    const char *prefix = "";
    DataContainerObject *object = NULL;
    const struct ast_node_hdl *object_node = NULL;
    int prefix_len;
    const char *prev_builtin_name;
    const char *next_builtin_name;
    PyObject *list;
    PyObject *item;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "|sO", kwlist,
                                     &prefix, &object)) {
        return NULL;
    }
    if (NULL != object && (PyObject *)object != Py_None) {
        if (!PyObject_TypeCheck((PyObject *)object, &DataContainerType)) {
            return PyErr_Format(PyExc_TypeError,
                                "'object' argument must be a "
                                "'bitpunch.DataContainer' object");
        }
        object_node = object->box->dpath.item;
    }
    list = PyList_New(0);
    if (NULL == list) {
        return NULL;
    }
    prefix_len = strlen(prefix);
    next_builtin_name = expr_find_first_builtin(prefix, object_node);
    while (NULL != next_builtin_name &&
           0 == strncmp(next_builtin_name, prefix, prefix_len)) {
        item = PyString_FromString(next_builtin_name);
        if (NULL == item) {
            Py_DECREF(list);
            return NULL;
        }
        if (-1 == PyList_Append(list, item)) {
            Py_DECREF(item);
            Py_DECREF(list);
            return NULL;
        }
        prev_builtin_name = next_builtin_name;
        next_builtin_name = expr_find_next_builtin(prev_builtin_name,
                                                   object_node);
    }
    return list;
}

static PyObject *
mod_bitpunch_enable_debug_mode(PyObject *self)
{
    tracker_debug_mode = TRUE;
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
mod_bitpunch_disable_debug_mode(PyObject *self)
{
    tracker_debug_mode = FALSE;
    Py_INCREF(Py_None);
    return Py_None;
}

static PyMethodDef bitpunch_methods[] = {
    { "make_python_object", mod_bitpunch_make_python_object, METH_O,
      "Make a deep python object from a bitpunch data object."
    },

    { "eval_expr", (PyCFunction)mod_bitpunch_eval_expr,
      METH_VARARGS,
      "evaluate a bitpunch expression outside any object scope"
    },

    { "get_builtin_names", (PyCFunction)mod_bitpunch_get_builtin_names,
      METH_VARARGS | METH_KEYWORDS,
      "Return a list of all built-in function names in expressions.\n"
      "\n"
      "Keyword arguments:\n"
      "prefix -- if provided, a list of the built-in function names\n"
      "          starting with prefix is returned."
    },

#ifdef DEBUG
    { "enable_debug_mode", (PyCFunction)mod_bitpunch_enable_debug_mode,
      METH_NOARGS,
      "Enable dump of debugging info"
    },

    { "disable_debug_mode", (PyCFunction)mod_bitpunch_disable_debug_mode,
      METH_NOARGS,
      "Disable dump of debugging info (default)"
    },
#endif

    { NULL, NULL, 0, NULL }
};

PyMODINIT_FUNC
initmodel_ext(void)
{
    PyObject *bitpunch_m;

    if (-1 == bitpunch_init()) {
        PyErr_SetString(PyExc_RuntimeError,
                        "Failed to initialize libbitpunch: "
                        "bitpunch_init() returned -1");
        return ;
    }
    bitpunch_m = Py_InitModule("model_ext", bitpunch_methods);
    if (setup_exceptions(bitpunch_m) < 0) {
        return ;
    }

    /* FormatSpec */
    if (FormatSpecType_setup() < 0) {
        return ;
    }
    Py_INCREF(&FormatSpecType);
    PyModule_AddObject(bitpunch_m,
                       "FormatSpec", (PyObject *)&FormatSpecType);

    /* IndexKey */
    if (IndexKeyType_setup() < 0) {
        return ;
    }
    Py_INCREF(&IndexKeyType);
    PyModule_AddObject(bitpunch_m,
                       "IndexKey", (PyObject *)&IndexKeyType);

    /* DataContainer */
    if (DataContainerType_setup() < 0) {
        return ;
    }
    Py_INCREF(&DataContainerType);
    PyModule_AddObject(bitpunch_m,
                       "DataContainer", (PyObject *)&DataContainerType);

    /* DataBlock */
    if (DataBlockType_setup() < 0) {
        return ;
    }
    Py_INCREF(&DataBlockType);
    PyModule_AddObject(bitpunch_m,
                       "DataBlock", (PyObject *)&DataBlockType);

    /* DataArray */
    if (DataArrayType_setup() < 0) {
        return ;
    }
    Py_INCREF(&DataArrayType);
    PyModule_AddObject(bitpunch_m,
                       "DataArray", (PyObject *)&DataArrayType);

    /* DataTree */
    if (DataTreeType_setup() < 0) {
        return ;
    }
    Py_INCREF(&DataTreeType);
    PyModule_AddObject(bitpunch_m, "DataTree", (PyObject *)&DataTreeType);

    /* Tracker */
    if (TrackerType_setup() < 0) {
        return ;
    }
    Py_INCREF(&TrackerType);
    PyModule_AddObject(bitpunch_m, "Tracker", (PyObject *)&TrackerType);
}
