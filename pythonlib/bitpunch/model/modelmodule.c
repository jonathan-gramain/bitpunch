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
#include "filters/array_slice.h"
#include "core/expr_internal.h"
#include "core/filter.h"
#include "core/print.h"
#include "utils/dynarray.h"

#define MAX_KEY_LENGTH 4096

static PyObject *BitpunchExc_NoItemError;
static PyObject *BitpunchExc_DataError;
static PyObject *BitpunchExc_OutOfBoundsError;
static PyObject *BitpunchExc_NoDataError;

struct DataItemObject;
struct BoardObject;
struct ScopeIterObject;
struct TrackerObject;

static int
expr_value_from_PyObject(PyObject *py_expr, int create_data_source,
                         expr_value_t *exprp);
static PyObject *
expr_value_and_dpath_to_PyObject(struct BoardObject *dtree,
                                 expr_value_t value, expr_dpath_t dpath);
static PyObject *
expr_value_to_native_PyObject(struct BoardObject *dtree,
                              expr_value_t value_eval);
static PyObject *
expr_value_to_native_PyObject_nodestroy(struct BoardObject *dtree,
                                        expr_value_t value_eval);
static PyObject *
eval_expr_as_python_object(struct DataItemObject *cont, const char *expr);

static PyObject *
tracker_item_to_shallow_PyObject(struct BoardObject *dtree,
                                 struct tracker *tk);

static struct ScopeIterObject *
make_scope_iter(struct scope_def *scope_def);

static struct ScopeIterObject *
make_filter_iter(struct ast_node_hdl *filter);

static int
tk_goto_item_by_key(struct tracker *tk, PyObject *index,
                    expr_value_t *keyp,
                    int *all_twinsp);

static int
dpath_is_complex_type(const struct dpath_node *dpath)
{
    const struct ast_node_hdl *filter;

    filter = ast_node_get_target_filter(dpath->filter);
    return (ast_node_filter_maps_list(filter) ||
            ast_node_filter_maps_object(filter));
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
node_info_to_python(const struct ast_node_hdl *node)
{
    PyObject *info_obj;
    FILE *loc_info_stream;
    char *loc_info_str;
    size_t loc_info_str_len;

    loc_info_stream = open_memstream(&loc_info_str, &loc_info_str_len);
    if (NULL == loc_info_stream) {
        return NULL;
    }
    fdump_ast_location(node, loc_info_stream);
    fclose(loc_info_stream);
    info_obj = PyDict_New();
    PyDict_SetItem(info_obj,
                   PyString_FromString("type"),
                   PyString_FromString(ast_node_type_str(node->ndat->type)));
    PyDict_SetItem(info_obj,
                   PyString_FromString("location"),
                   PyString_FromString(loc_info_str));
    free(loc_info_str);
    return info_obj;
}

static PyObject *
bitpunch_error_context_info_to_python(
    struct bitpunch_error_context_info *ctx_info)
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
    if (NULL != ctx_info->node) {
        info_obj = node_info_to_python(ctx_info->node);
        if (NULL == info_obj) {
            goto err;
        }
        PyDict_SetItem(context_obj,
                       PyString_FromString("node"), info_obj);
    }
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
bitpunch_error_get_info_as_PyObject(struct bitpunch_error *err)
{
    PyObject *errobj = NULL;
    int ctx_i;
    PyObject *info_obj;
    PyObject *contexts_obj;
    FILE *memstr;
    char *desc_str;
    size_t desc_str_len;

    errobj = PyDict_New();
    if (NULL == errobj) {
        return NULL;
    }

    if (NULL != err->tk || NULL != err->box) {
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
    if (NULL != err->node) {
        info_obj = node_info_to_python(err->node);
        if (NULL == info_obj) {
            goto err;
        }
        PyDict_SetItem(errobj, PyString_FromString("node"), info_obj);
    }
    PyDict_SetItem(errobj,
                   PyString_FromString("reason"),
                   PyString_FromString(err->reason));
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

        context_obj = bitpunch_error_context_info_to_python(
            &err->contexts[ctx_i]);
        if (NULL == context_obj) {
            goto err;
        }
        PyList_SET_ITEM(contexts_obj, ctx_i, context_obj);
    }
    memstr = open_memstream(&desc_str, &desc_str_len);
    if (NULL == memstr) {
        PyErr_SetString(PyExc_OSError, "error opening memory stream");
        goto err;
    }
    bitpunch_error_dump_full(err, memstr);
    fclose(memstr);
    PyDict_SetItem(errobj,
                   PyString_FromString("description"),
                   PyString_FromStringAndSize(desc_str, desc_str_len));
    free(desc_str);
    return errobj;

  err:
    Py_XDECREF(errobj);
    return NULL;
}

struct python_saved_error {
    PyObject *err_type;
    PyObject *err_value;
    PyObject *err_traceback;
};

static void
set_bitpunch_error(struct bitpunch_error *err,
                  bitpunch_status_t ret)
{
    struct python_saved_error *saved_err;
    PyObject *errobj;

    if (NULL != err) {
        saved_err = (struct python_saved_error *)
            bitpunch_error_fetch_user_arg(err);
        if (NULL != saved_err) {
            PyErr_Restore(saved_err->err_type, saved_err->err_value,
                          saved_err->err_traceback);
            free(saved_err);
            return ;
        }
        errobj = bitpunch_error_get_info_as_PyObject(err);
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
    case BITPUNCH_NO_DATA:
        PyErr_SetObject(BitpunchExc_NoDataError, errobj);
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
    bitpunch_error_destroy(err);
}


static PyObject *
tracker_item_to_deep_PyObject(struct BoardObject *dtree, struct tracker *tk);

static PyObject *
box_to_deep_PyObject(struct BoardObject *dtree, struct box *box);

static PyObject *
box_to_deep_PyDict(struct BoardObject *dtree, struct box *box);

static PyObject *
box_to_deep_PyList(struct BoardObject *dtree, struct box *box);


struct python_data_source {
    struct bitpunch_data_source ds; /* inherits */
    PyObject *data_obj;
};

static int
python_data_source_close(struct bitpunch_data_source *ds)
{
    struct python_data_source *py_ds = (struct python_data_source *)ds;
    Py_DECREF(py_ds->data_obj);
    return 0;
}

static struct bitpunch_data_source *
python_data_source_new_from_memory(
    PyObject *data_obj,
    const char *data, Py_ssize_t data_len)
{
    struct python_data_source *py_ds;

    assert(NULL != dsp);

    py_ds = new_safe(struct python_data_source);
    py_ds->ds.use_count = 1;
    py_ds->ds.backend.close = python_data_source_close;
    py_ds->ds.ds_data = (char *)data;
    py_ds->ds.ds_data_length = data_len;
    py_ds->data_obj = data_obj;
    Py_INCREF(data_obj);
    return (struct bitpunch_data_source *)py_ds;
}



/*
 * Format
 */

PyDoc_STRVAR(SpecNode__doc__,
             "Compiled representation of the format of a binary file");

typedef struct SpecNodeObject {
    PyObject_HEAD
    struct ast_node_hdl *schema;
} SpecNodeObject;

static PyMethodDef SpecNode_methods[] = {

    { NULL, NULL, 0, NULL }
};

static SpecNodeObject *
create_SpecNode(struct ast_node_hdl *node);

static PyObject *
SpecNode_get_attributes_dict(SpecNodeObject *self)
{
    PyObject *dict;

    dict = PyDict_New();
    if (NULL == dict) {
        return NULL;
    }
    // TODO
    return dict;
}

static PyObject *
SpecNode_eval_attr(SpecNodeObject *self, const char *attr_name)
{
    struct ast_node_hdl *parsed_target;
    struct ast_node_hdl *parsed_filter;
    struct scope_def *scope_def = NULL;
    struct named_expr *named_expr;

    parsed_target = ast_node_get_named_expr_target(self->schema);
    if (ast_node_is_scope_def(parsed_target)) {
        scope_def = &parsed_target->ndat->u.scope_def;
    } else {
        parsed_filter = ast_node_get_target_filter(parsed_target);
        if (ast_node_is_filter(parsed_filter)) {
            scope_def = filter_get_scope_def(parsed_filter);
        }
    }
    if (NULL != scope_def) {
        STATEMENT_FOREACH(named_expr, named_expr,
                          scope_def->block_stmt_list.named_expr_list, list) {
            if (0 == strcmp(named_expr->nstmt.name, attr_name)) {
                return (PyObject *)create_SpecNode(named_expr->expr);
            }
        }
    }
    PyErr_Format(PyExc_AttributeError,
                 "no such named expression: %s", attr_name);
    return NULL;
}

static PyObject *
SpecNode_get_item(SpecNodeObject *self, PyObject *attr_name,
                    int getattr)
{
    PyObject *attr;
    const char *attr_str;

    if (PyString_Check(attr_name)) {
        attr_str = PyString_AS_STRING(attr_name);
        if (getattr) {
            if (0 == strcmp(attr_str, "__dict__")) {
                return SpecNode_get_attributes_dict(self);
            }
            attr = PyObject_GenericGetAttr((PyObject *)self, attr_name);
            if (NULL != attr) {
                return attr;
            }
            if (!PyErr_ExceptionMatches(
                    getattr ? PyExc_AttributeError : PyExc_IndexError)) {
                return NULL;
            }
            PyErr_Clear();
        }
        attr = SpecNode_eval_attr(self, attr_str);
        if (NULL != attr) {
            return attr;
        }
    }
    return NULL;
}

static PyObject *
SpecNode_getattro(SpecNodeObject *self, PyObject *attr_name)
{
    return SpecNode_get_item(self, attr_name, TRUE);
}

static PyObject *
SpecNode_mp_subscript(SpecNodeObject *self, PyObject *key)
{
    return SpecNode_get_item(self, key, FALSE);
}

static PyMappingMethods SpecNode_as_mapping = {
    .mp_subscript = (binaryfunc)SpecNode_mp_subscript,
    .mp_ass_subscript = NULL
};

static PyTypeObject SpecNodeType = {
    PyObject_HEAD_INIT(NULL)
    0,                         /* ob_size */
    "bitpunch.SpecNode",       /* tp_name */
    sizeof(SpecNodeObject),    /* tp_basicsize */
    0,                         /* tp_itemsize */
    0,                         /* tp_dealloc */
    0,                         /* tp_print */
    0,                         /* tp_getattr */
    0,                         /* tp_setattr */
    0,                         /* tp_compare */
    0,                         /* tp_repr */
    0,                         /* tp_as_number */
    0,                         /* tp_as_sequence */
    &SpecNode_as_mapping,      /* tp_as_mapping */
    0,                         /* tp_hash */
    0,                         /* tp_call */
    0,                         /* tp_str */
    0,                         /* tp_getattro */
    0,                         /* tp_setattro */
    0,                         /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,       /* tp_flags */
    SpecNode__doc__,           /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,                         /* tp_richcompare */
    0,                         /* tp_weaklistoffset */
    0,                         /* tp_iter */
    0,                         /* tp_iternext */
    SpecNode_methods,          /* tp_methods */
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
SpecNode_new(PyTypeObject *subtype,
               PyObject *args, PyObject *kwds)
{
    SpecNodeObject *self;

    self = (SpecNodeObject *)subtype->tp_alloc(subtype, 0);
    return (PyObject *)self;
}

static SpecNodeObject *
create_SpecNode(struct ast_node_hdl *node)
{
    SpecNodeObject *ast_node;

    ast_node = (SpecNodeObject *)
        SpecNode_new(&SpecNodeType, NULL, NULL);
    if (NULL == ast_node) {
        return NULL;
    }
    ast_node->schema = node;
    return ast_node;
}


static void
SpecNode_clear(SpecNodeObject *self)
{
}

static void
SpecNode_dealloc(SpecNodeObject *self)
{
    SpecNode_clear(self);
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
SpecNode_iter(SpecNodeObject *self)
{
    struct ast_node_hdl *parsed_target;
    struct ast_node_hdl *parsed_filter;

    parsed_target = ast_node_get_named_expr_target(self->schema);
    if (ast_node_is_scope_def(parsed_target)) {
        return (PyObject *)make_scope_iter(
            &parsed_target->ndat->u.scope_def);
    }
    parsed_filter = ast_node_get_target_filter(parsed_target);
    if (ast_node_is_filter(parsed_filter)) {
        return (PyObject *)make_filter_iter(parsed_filter);
    }
    PyErr_SetString(PyExc_TypeError,
                    "spec node is not iterable");
    return NULL;
}

static int
SpecNodeType_setup(void)
{
    SpecNodeType.ob_type = &PyType_Type;
    SpecNodeType.tp_new = SpecNode_new;
    SpecNodeType.tp_dealloc = (destructor)SpecNode_dealloc;
    SpecNodeType.tp_getattro = (getattrofunc)SpecNode_getattro;
    SpecNodeType.tp_iter = (getiterfunc)SpecNode_iter;
    if (PyType_Ready(&SpecNodeType) < 0) {
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
    0,                           /* ob_size */
    "bitpunch.IndexKey",         /* tp_name */
    sizeof(IndexKeyObject),      /* tp_basicsize */
    0,                           /* tp_itemsize */
    0,                           /* tp_dealloc */
    0,                           /* tp_print */
    0,                           /* tp_getattr */
    0,                           /* tp_setattr */
    0,                           /* tp_compare */
    0,                           /* tp_repr */
    0,                           /* tp_as_number */
    0,                           /* tp_as_sequence */
    0,                           /* tp_as_mapping */
    0,                           /* tp_hash */
    0,                           /* tp_call */
    0,                           /* tp_str */
    0,                           /* tp_getattro */
    0,                           /* tp_setattro */
    0,                           /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE |
    Py_TPFLAGS_HAVE_RICHCOMPARE, /* tp_flags */
    IndexKey__doc__,             /* tp_doc */
    0,                           /* tp_traverse */
    0,                           /* tp_clear */
    0,                           /* tp_richcompare */
    0,                           /* tp_weaklistoffset */
    0,                           /* tp_iter */
    0,                           /* tp_iternext */
    IndexKey_methods,            /* tp_methods */
    0,                           /* tp_members */
    0,                           /* tp_getset */
    0,                           /* tp_base */
    0,                           /* tp_dict */
    0,                           /* tp_descr_get */
    0,                           /* tp_descr_set */
    0,                           /* tp_dictoffset */
    0,                           /* tp_init */
    0,                           /* tp_alloc */
    0,                           /* tp_new */
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
 * Board
 */

typedef struct BoardObject {
    PyObject_HEAD
    struct bitpunch_board *board;
    //ARRAY_HEAD(datasource_array, struct ast_node_hdl) data_sources;
} BoardObject;

PyDoc_STRVAR(Board__doc__,
             "Represents the workspace where to load schemas and data sources "
             "and where expressions can be evaluated");

static PyObject *
Board_new(PyTypeObject *subtype,
          PyObject *args, PyObject *kwds)
{
    BoardObject *self;

    self = (BoardObject *)subtype->tp_alloc(subtype, 0);
    if (NULL == self) {
        return NULL;
    }
    self->board = bitpunch_board_new();
    return (PyObject *)self;
}

static struct ast_node_hdl *
load_spec_internal(PyObject *spec_arg, const char *path)
{
    int ret;
    struct ast_node_hdl *spec_node = NULL;

    if (NULL == spec_arg && NULL == path) {
        PyErr_SetString(
            PyExc_TypeError,
            "usage error: no spec specified");
        return NULL;
    }
    if (NULL != spec_arg && NULL != path) {
        PyErr_SetString(
            PyExc_TypeError,
            "usage error: 'spec' and 'path' arguments cannot both be set");
        return NULL;
    }

    if (NULL != path) {
        ret = bitpunch_schema_create_from_path(&spec_node, path);
    } else if (PyString_Check(spec_arg)) {
        const char *contents;

        /* compile the provided text contents */
        contents = PyString_AsString(spec_arg);
        ret = bitpunch_schema_create_from_string(&spec_node, contents);
    } else if (PyFile_Check(spec_arg)) {
        FILE *file;

        /* compile the contents from the file object */
        file = PyFile_AsFile(spec_arg);
        //PyFile_IncUseCount((PyFileObject *)spec_arg);
        ret = bitpunch_schema_create_from_file_descriptor(
            &spec_node, fileno(file));
    } else {
        PyErr_SetString(PyExc_TypeError,
                        "The spec argument must be a string or a file object");
        return NULL;
    }
    if (-1 == ret) {
        PyErr_SetString(PyExc_OSError,
                        "Error loading bitpunch specification from file");
        return NULL;
    }
    return spec_node;
}

static PyObject *
Board_add_spec(BoardObject *self, PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = { "name", "spec", "path", NULL };
    const char *name;
    PyObject *spec_arg = NULL;
    const char *path = NULL;
    struct ast_node_hdl *spec_node;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "s|Os", kwlist,
                                     &name, &spec_arg, &path)) {
        return NULL;
    }
    spec_node = load_spec_internal(spec_arg, path);
    if (NULL == spec_node) {
        return NULL;
    }
    bitpunch_board_add_let_expression(self->board, name, spec_node);

    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

static PyObject *
Board_use_spec(BoardObject *self, PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = { "spec", "path", NULL };
    PyObject *spec_arg = NULL;
    const char *path = NULL;
    struct ast_node_hdl *spec_node;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "|Os", kwlist,
                                     &spec_arg, &path)) {
        return NULL;
    }
    spec_node = load_spec_internal(spec_arg, path);
    if (NULL == spec_node) {
        return NULL;
    }
    bitpunch_board_use_spec(self->board, spec_node);

    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

static PyObject *
Board_forget_spec(BoardObject *board)
{
    bitpunch_board_forget_spec(board->board);
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *
Board_add_data_source(BoardObject *self, PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = { "name", "data", "path", NULL };
    const char *name;
    PyObject *data = NULL;
    const char *path = NULL;
    int ret;
    struct bitpunch_data_source *data_source;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "s|Os", kwlist,
                                     &name, &data, &path)) {
        return NULL;
    }
    if (NULL == data && NULL == path) {
        PyErr_SetString(
            PyExc_TypeError,
            "usage error: no data source specified");
        return NULL;
    }
    if (NULL != data && NULL != path) {
        PyErr_SetString(
            PyExc_TypeError,
            "usage error: 'data' and 'path' attributes cannot both be set");
        return NULL;
    }
    if (NULL != path) {
        ret = bitpunch_data_source_create_from_file_path(&data_source, path);
    } else if (PyString_Check(data)) {
        char *contents;
        Py_ssize_t length;

        /* load the provided text contents */
        ret = PyString_AsStringAndSize(data, &contents, &length);
        assert(-1 != ret);
        data_source = python_data_source_new_from_memory(
            data, contents, length);
        ret = 0;
    } else if (PyByteArray_Check(data)) {
        /* load the provided text contents */
        data_source = python_data_source_new_from_memory(
            data,
            PyByteArray_AS_STRING(data),
            PyByteArray_GET_SIZE(data));
        ret = 0;
    } else if (PyFile_Check(data)) {
        FILE *file;

        /* load the contents from the file object */
        file = PyFile_AsFile(data);
        //PyFile_IncUseCount((PyFileObject *)data);
        ret = bitpunch_data_source_create_from_file_descriptor(
            &data_source, fileno(file));
    } else {
        PyErr_SetString(
            PyExc_TypeError,
            "The first argument must be a string or a file object");
        return NULL;
    }
    if (-1 == ret) {
        PyErr_SetString(PyExc_OSError, "Error loading binary contents");
        return NULL;
    }
    bitpunch_board_add_let_expression(
        self->board, name, bitpunch_data_source_to_filter(data_source));

    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

struct python_filter_instance {
    struct filter_instance filter; /* inherits */
    PyObject *py_filter;
};

static bitpunch_status_t
python_filter_read(
    struct ast_node_hdl *filter,
    struct box *scope,
    const char *buffer, size_t buffer_size,
    expr_value_t *valuep,
    struct browse_state *bst)
{
    struct python_filter_instance *f_instance;
    PyObject *method_name;
    PyObject *eval_result;
    Py_buffer input_buffer;
    PyObject *input_memview;
    int ret;

    if (-1 == PyBuffer_FillInfo(&input_buffer, NULL,
                                (void *)buffer, buffer_size,
                                TRUE /* read-only */, PyBUF_FULL_RO)) {
        return BITPUNCH_ERROR;
    }
    f_instance = (struct python_filter_instance *)
        filter->ndat->u.rexpr_filter.f_instance;
    method_name = PyString_FromString("read");
    /* input_buffer gets stolen */
    input_memview = PyMemoryView_FromBuffer(&input_buffer);

    eval_result = PyObject_CallMethodObjArgs(
        (PyObject *)f_instance->py_filter, method_name, input_memview, NULL);
    Py_DECREF(method_name);
    Py_DECREF(input_memview);
    if (NULL == eval_result) {
        struct python_saved_error *saved_err;
        bitpunch_status_t bt_ret;

        saved_err = new_safe(struct python_saved_error);
        PyErr_Fetch(&saved_err->err_type, &saved_err->err_value,
                    &saved_err->err_traceback);
        bt_ret = node_error(BITPUNCH_DATA_ERROR, NULL, bst, NULL);
        bitpunch_error_attach_user_arg(bst->last_error, saved_err);
        return bt_ret;
    }
    ret = expr_value_from_PyObject(eval_result, TRUE, valuep);
    Py_DECREF(eval_result);
    if (0 != ret) {
        return node_error(
            BITPUNCH_INVALID_PARAM, NULL, bst,
            "unsupported data type returned by external function");
    }
    return BITPUNCH_OK;
}

static struct filter_instance *
python_filter_instance_build(
    struct ast_node_hdl *filter)
{
    const struct filter_class *filter_cls;
    PyTypeObject *filter_type;
    PyObject *filter_obj;
    struct python_filter_instance *f_instance;
    PyObject *method_name;
    PyObject *res_obj;

    filter_cls = filter->ndat->u.rexpr_filter.filter_cls;
    filter_type = (PyTypeObject *)filter_cls->user_arg;
    // call the type object
    filter_obj = PyObject_CallObject((PyObject *)filter_type, NULL);
    if (NULL == filter_obj) {
        // TODO handle error with PyErr_Fetch()
        return NULL;
    }
    method_name = PyString_FromString("__init__");
    res_obj = PyObject_CallMethodObjArgs(
        (PyObject *)filter_obj, method_name, NULL);
    Py_DECREF(method_name);
    if (NULL == res_obj) {
        Py_DECREF(filter_obj);
        return NULL;
    }
    Py_DECREF(res_obj);
    f_instance = new_safe(struct python_filter_instance);
    f_instance->filter.b_item.read_value_from_buffer = python_filter_read;
    f_instance->py_filter = filter_obj;
    return (struct filter_instance *)f_instance;
}

static int
declare_python_filter_class(
    BoardObject *self, const char *filter_name, PyTypeObject *filter_type)
{
    struct ast_node_hdl *filter_hdl;
    int ret;

    ret = bitpunch_external_create_filter(
        &filter_hdl,
        filter_name,
        python_filter_instance_build,
        NULL,
        (void *)filter_type);
    if (-1 == ret) {
        PyErr_SetString(PyExc_ValueError,
                        "error creating external filter class");
        return -1;
    }
    bitpunch_board_add_external_def(self->board, filter_name, filter_hdl);
    return 0;
}

static PyObject *
Board_register_filter(BoardObject *self, PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = { "name", "filter", NULL };
    const char *name;
    PyObject *filter = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "sO", kwlist,
                                     &name, &filter)) {
        return NULL;
    }
    if (!PyType_Check(filter)) {
        PyErr_SetString(PyExc_TypeError,
                        "The filter argument must be a class type");
        return NULL;
    }
    if (-1 == declare_python_filter_class(self, name, (PyTypeObject *)filter)) {
        return NULL;
    }
    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

static bitpunch_status_t
extern_func_as_python_function(
    void *user_arg,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    PyObject *arg_obj;
    PyObject *eval_result;
    int ret;

    arg_obj = (PyObject *)user_arg;
    eval_result = PyObject_CallFunctionObjArgs(arg_obj, NULL);
    if (NULL == eval_result) {
        struct python_saved_error *saved_err;
        bitpunch_status_t bt_ret;

        saved_err = new_safe(struct python_saved_error);
        PyErr_Fetch(&saved_err->err_type, &saved_err->err_value,
                    &saved_err->err_traceback);
        bt_ret = node_error(BITPUNCH_DATA_ERROR, NULL, bst, NULL);
        bitpunch_error_attach_user_arg(bst->last_error, saved_err);
        return bt_ret;
    }
    if (NULL != valuep) {
        ret = expr_value_from_PyObject(eval_result, TRUE, valuep);
        Py_DECREF(eval_result);
        if (0 != ret) {
            // TODO log data type
            return node_error(
                BITPUNCH_INVALID_PARAM, NULL, bst,
                "unsupported data type returned by external function");
        }
    }
    if (NULL != dpathp) {
        *dpathp = expr_dpath_none();
    }
    return BITPUNCH_OK;
}

static int
declare_python_function(
    BoardObject *self, const char *function_name, PyObject *func_obj)
{
    struct ast_node_hdl *func_hdl;
    int ret;

    ret = bitpunch_external_create_function(
        &func_hdl, extern_func_as_python_function, func_obj);
    if (-1 == ret) {
        PyErr_SetString(PyExc_OSError, "Error creating external function");
        return -1;
    }
    bitpunch_board_add_external_def(self->board, function_name, func_hdl);
    return 0;
}

static PyObject *
Board_register_function(BoardObject *self, PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = { "name", "function", NULL };
    const char *name;
    PyObject *function = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "sO", kwlist,
                                     &name, &function)) {
        return NULL;
    }
    if (!PyCallable_Check(function)) {
        PyErr_SetString(PyExc_TypeError, "Argument must be a callable");
        return NULL;
    }
    if (-1 == declare_python_function(self, name, (PyObject *)function)) {
        return NULL;
    }
    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

static PyObject *
Board_add_expr(BoardObject *board, PyObject *args)
{
    const char *name;
    const char *expr;
    bitpunch_status_t bt_ret;
    struct bitpunch_error *bp_err = NULL;

    if (!PyArg_ParseTuple(args, "ss", &name, &expr)) {
        return NULL;
    }
    bt_ret = bitpunch_board_add_expr(board->board, name, expr);
    if (BITPUNCH_OK != bt_ret) {
        if (NULL != bp_err) {
            set_bitpunch_error(bp_err, bp_err->bt_ret);
        } else {
            PyErr_Format(PyExc_ValueError,
                         "Error compiling expression '%s'", expr);
        }
        return NULL;
    }
    Py_INCREF(board);
    return (PyObject *)board;
}

static PyObject *
Board_remove(BoardObject *board, PyObject *args)
{
    const char *name;
    int n_removed;

    if (!PyArg_ParseTuple(args, "s", &name)) {
        return NULL;
    }
    n_removed = bitpunch_board_remove_by_name(board->board, name);
    return PyInt_FromLong(n_removed);
}

static PyObject *
Board_get_spec(BoardObject *self)
{
    return (PyObject *)create_SpecNode(self->board->ast_root);
}

static PyObject *
Board_eval_expr(BoardObject *board, PyObject *args)
{
    const char *expr;
    bitpunch_status_t bt_ret;
    struct ast_node_hdl *parsed_expr;
    expr_value_t expr_value;
    expr_dpath_t expr_dpath;
    struct bitpunch_error *bp_err = NULL;

    if (!PyArg_ParseTuple(args, "s", &expr)) {
        return NULL;
    }
    bt_ret = bitpunch_eval_expr(
        board->board, expr, NULL,
        BITPUNCH_EVAL_DPATH_XOR_VALUE,
        &parsed_expr, &expr_value, &expr_dpath, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        if (BITPUNCH_NO_DATA == bt_ret) {
            return (PyObject *)create_SpecNode(parsed_expr);
        }
        set_bitpunch_error(bp_err, bt_ret);
        return NULL;
    }
    return expr_value_and_dpath_to_PyObject(board, expr_value, expr_dpath);
}

static PyMethodDef Board_methods[] = {
    { "add_spec", (PyCFunction)Board_add_spec,
      METH_VARARGS | METH_KEYWORDS,
      "add specification code to the board from a string, buffer, "
      "file object or path"
    },
    { "use_spec", (PyCFunction)Board_use_spec,
      METH_VARARGS | METH_KEYWORDS,
      "import all names from a specification into the board, from a string, "
      "buffer, file object or path"
    },
    { "forget_spec", (PyCFunction)Board_forget_spec,
      METH_NOARGS,
      "clear all names from the currently imported specification"
    },
    { "add_data_source", (PyCFunction)Board_add_data_source,
      METH_VARARGS | METH_KEYWORDS,
      "add a data source to the board from a string, buffer, "
      "file object or path"
    },
    { "register_filter", (PyCFunction)Board_register_filter,
      METH_VARARGS | METH_KEYWORDS,
      // TODO add details of the class contract
      "add a filter implementation to the board from a python class"
    },
    { "register_function", (PyCFunction)Board_register_function,
      METH_VARARGS | METH_KEYWORDS,
      // TODO add details of the function contract
      "add a function implementation to the board from a python callable"
    },
    { "add_expr", (PyCFunction)Board_add_expr,
      METH_VARARGS,
      "add an expression to the board from a string"
    },
    { "remove", (PyCFunction)Board_remove,
      METH_VARARGS,
      "remove a name from the board\n"
      "\n"
      "returns the number of references removed"
    },
    { "get_spec", (PyCFunction)Board_get_spec,
      METH_NOARGS,
      "Get the specification root node"
    },
    { "eval_expr", (PyCFunction)Board_eval_expr,
      METH_VARARGS | METH_KEYWORDS,
      "evaluate a bitpunch expression in the board's scope"
    },
    { NULL, NULL, 0, NULL }
};

static int
Board_clear(BoardObject *self)
{
    bitpunch_board_free(self->board);
    return 0;
}

static void
Board_dealloc(BoardObject *self)
{
    Board_clear(self);
    Py_TYPE(self)->tp_free(self);
}

PyDoc_STRVAR(ScopeIter__doc__, "Scope iterator object");

typedef struct ScopeIterObject {
    PyObject_HEAD
    struct named_expr *cur;
} ScopeIterObject;

static PyObject *
ScopeIter_new(PyTypeObject *subtype,
              PyObject *args, PyObject *kwds)
{
    ScopeIterObject *self;

    self = (ScopeIterObject *)subtype->tp_alloc(subtype, 0);
    return (PyObject *)self;
}

static int
ScopeIter_clear(ScopeIterObject *self)
{
    return 0;
}

static void
ScopeIter_dealloc(ScopeIterObject *self)
{
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
ScopeIter_iter(ScopeIterObject *self)
{
    Py_INCREF(self);
    return (PyObject *)self;
}

static PyObject *
ScopeIter_iternext(ScopeIterObject *iter)
{
    PyObject *name;

    if (NULL == iter->cur) {
        return NULL;
    }
    name = PyString_FromString(iter->cur->nstmt.name);
    iter->cur = STATEMENT_NEXT(named_expr, iter->cur, list);
    return name;
}

static PyMethodDef ScopeIter_methods[] = {
    { NULL, NULL, 0, NULL }
};

static PyTypeObject ScopeIterType = {
    PyObject_HEAD_INIT(NULL)
    0,                           /* ob_size */
    "bitpunch.ScopeIter",        /* tp_name */
    sizeof(ScopeIterObject),     /* tp_basicsize */
    0,                           /* tp_itemsize */
    0,                           /* tp_dealloc */
    0,                           /* tp_print */
    0,                           /* tp_getattr */
    0,                           /* tp_setattr */
    0,                           /* tp_compare */
    0,                           /* tp_repr */
    0,                           /* tp_as_number */
    0,                           /* tp_as_sequence */
    0,                           /* tp_as_mapping */
    0,                           /* tp_hash */
    0,                           /* tp_call */
    0,                           /* tp_str */
    0,                           /* tp_getattro */
    0,                           /* tp_setattro */
    0,                           /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE |
    Py_TPFLAGS_HAVE_ITER,        /* tp_flags */
    ScopeIter__doc__,            /* tp_doc */
    0,                           /* tp_traverse */
    0,                           /* tp_clear */
    0,                           /* tp_richcompare */
    0,                           /* tp_weaklistoffset */
    0,                           /* tp_iter */
    0,                           /* tp_iternext */
    ScopeIter_methods,           /* tp_methods */
    0,                           /* tp_members */
    0,                           /* tp_getset */
    0,                           /* tp_base */
    0,                           /* tp_dict */
    0,                           /* tp_descr_get */
    0,                           /* tp_descr_set */
    0,                           /* tp_dictoffset */
    0,                           /* tp_init */
    0,                           /* tp_alloc */
    0,                           /* tp_new */
};

static int
ScopeIterType_setup(void)
{
    ScopeIterType.ob_type = &PyType_Type;
    ScopeIterType.tp_new = ScopeIter_new;
    ScopeIterType.tp_clear = (inquiry)ScopeIter_clear;
    ScopeIterType.tp_dealloc = (destructor)ScopeIter_dealloc;
    ScopeIterType.tp_iter = (getiterfunc)ScopeIter_iter;
    ScopeIterType.tp_iternext = (iternextfunc)ScopeIter_iternext;
    if (PyType_Ready(&ScopeIterType) < 0) {
        return -1;
    }
    return 0;
}

static ScopeIterObject *
make_scope_iter(struct scope_def *scope_def)
{
    ScopeIterObject *self;

    self = (ScopeIterObject *)ScopeIter_new(&ScopeIterType, NULL, NULL);
    if (NULL == self) {
        return NULL;
    }
    self->cur = STATEMENT_FIRST(
        named_expr, scope_def->block_stmt_list.named_expr_list);
    return self;
}

static ScopeIterObject *
make_filter_iter(struct ast_node_hdl *filter)
{
    return make_scope_iter(filter_get_scope_def(filter));
}



static PyObject *
Board_iter(BoardObject *self)
{
    return (PyObject *)
        make_scope_iter(&self->board->ast_root->ndat->u.scope_def);
}

static PyTypeObject BoardType = {
    PyObject_HEAD_INIT(NULL)
    0,                         /* ob_size */
    "bitpunch.Board",          /* tp_name */
    sizeof(BoardObject),       /* tp_basicsize */
    0,                         /* tp_itemsize */
    0,                         /* tp_dealloc */
    0,                         /* tp_print */
    0,                         /* tp_getattr */
    0,                         /* tp_setattr */
    0,                         /* tp_compare */
    0,                         /* tp_repr */
    0,                         /* tp_as_number */
    0,                         /* tp_as_sequence */
    0,                         /* tp_as_mapping */
    0,                         /* tp_hash */
    0,                         /* tp_call */
    0,                         /* tp_str */
    0,                         /* tp_getattro */
    0,                         /* tp_setattro */
    0,                         /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,       /* tp_flags */
    Board__doc__,              /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,                         /* tp_richcompare */
    0,                         /* tp_weaklistoffset */
    0,                         /* tp_iter */
    0,                         /* tp_iternext */
    Board_methods,             /* tp_methods */
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

static int
BoardType_setup(void)
{
    BoardType.ob_type = &PyType_Type;
    BoardType.tp_new = Board_new;
    BoardType.tp_clear = (inquiry)Board_clear;
    BoardType.tp_dealloc = (destructor)Board_dealloc;
    BoardType.tp_iter = (getiterfunc)Board_iter;
    if (PyType_Ready(&BoardType) < 0) {
        return -1;
    }
    return 0;
}



/*
 * DataItem
 */

PyDoc_STRVAR(DataItem__doc__,
             "Represents a data item as a section in the binary contents");

typedef struct DataItemObject {
    PyObject_HEAD
    struct BoardObject *dtree;
    expr_value_t value;
    expr_dpath_t dpath;

    struct box *filtered_box;
} DataItemObject;

static int
DataItem_bf_getbuffer(DataItemObject *exporter,
                      Py_buffer *view, int flags);


static PyBufferProcs DataItem_as_buffer = {
    .bf_getbuffer = (getbufferproc)DataItem_bf_getbuffer,
};

static PyObject *
DataItem_nb_int(DataItemObject *self);

#define X_NB_FUNCS_UNARY                        \
    X(nb_negative)                              \
    X(nb_positive)                              \
    X(nb_absolute)                              \
    X(nb_invert)                                \
    X(nb_long)                                  \
    X(nb_float)                                 \
    X(nb_oct)                                   \
    X(nb_hex)                                   \
    X(nb_index)                                 \


#define X_NB_FUNCS_BINARY                       \
    X(nb_add)                                   \
    X(nb_subtract)                              \
    X(nb_multiply)                              \
    X(nb_divide)                                \
    X(nb_remainder)                             \
    X(nb_divmod)                                \
    X(nb_lshift)                                \
    X(nb_rshift)                                \
    X(nb_and)                                   \
    X(nb_xor)                                   \
    X(nb_or)                                    \
    X(nb_inplace_add)                           \
    X(nb_inplace_subtract)                      \
    X(nb_inplace_multiply)                      \
    X(nb_inplace_divide)                        \
    X(nb_inplace_remainder)                     \
    X(nb_inplace_lshift)                        \
    X(nb_inplace_rshift)                        \
    X(nb_inplace_and)                           \
    X(nb_inplace_xor)                           \
    X(nb_inplace_or)                            \
    X(nb_floor_divide)                          \
    X(nb_true_divide)                           \
    X(nb_inplace_floor_divide)                  \
    X(nb_inplace_true_divide)                   \


#define X_NB_FUNCS_TERNARY                      \
    X(nb_power)                                 \
    X(nb_inplace_power)                         \


enum PyNumberMethods_op {
#define X(FUNC) OP_##FUNC,
    X_NB_FUNCS_UNARY
    X_NB_FUNCS_BINARY
    X_NB_FUNCS_TERNARY
#undef X
};


static PyObject *
DataItem_nb_unary_op(enum PyNumberMethods_op op, PyObject *opd);
static PyObject *
DataItem_nb_binary_op(enum PyNumberMethods_op op,
                      PyObject *opd1, PyObject *opd2);
static PyObject *
DataItem_nb_ternary_op(enum PyNumberMethods_op op,
                       PyObject *opd1, PyObject *opd2, PyObject *opd3);

#define X(FUNC) static PyObject *DataItem_##FUNC(PyObject *opd)         \
    {                                                                   \
        return DataItem_nb_unary_op(OP_##FUNC, opd);                    \
    }
    X_NB_FUNCS_UNARY
#undef X
#define X(FUNC) static PyObject *DataItem_##FUNC(PyObject *opd1,        \
                                                 PyObject *opd2)        \
    {                                                                   \
        return DataItem_nb_binary_op(OP_##FUNC, opd1, opd2);            \
    }
    X_NB_FUNCS_BINARY
#undef X
#define X(FUNC) static PyObject *DataItem_##FUNC(PyObject *opd1,        \
                                                 PyObject *opd2,        \
                                                 PyObject *opd3)        \
    {                                                                   \
        return DataItem_nb_ternary_op(OP_##FUNC, opd1, opd2, opd3);     \
    }
    X_NB_FUNCS_TERNARY
#undef X

static int DataItem_nb_nonzero(PyObject *self);

static int DataItem_nb_coerce(PyObject **self, PyObject **opd);

static PyNumberMethods DataItem_as_number = {
#define X(FUNC) .FUNC = DataItem_##FUNC,
    X_NB_FUNCS_UNARY
#undef X
#define X(FUNC) .FUNC = DataItem_##FUNC,
    X_NB_FUNCS_BINARY
#undef X
#define X(FUNC) .FUNC = DataItem_##FUNC,
    X_NB_FUNCS_TERNARY
#undef X
    .nb_int = (unaryfunc)DataItem_nb_int,
    .nb_nonzero = DataItem_nb_nonzero,
    .nb_coerce = DataItem_nb_coerce,
};

static size_t PyNumberMethods_func_offsets[] = {
#define X(FUNC) [OP_##FUNC] = offsetof(PyNumberMethods, FUNC),
    X_NB_FUNCS_UNARY
    X_NB_FUNCS_BINARY
    X_NB_FUNCS_TERNARY
#undef X
};

static void *
PyNumberMethods_lookup_func(PyNumberMethods *methods,
                            enum PyNumberMethods_op op)
{
    assert(op >= 0 && op < N_ELEM(PyNumberMethods_func_offsets));
    return *(void **)((char *)methods + PyNumberMethods_func_offsets[op]);
}

static int
DataItem_convert_dpath_to_box(DataItemObject *self)
{
    bitpunch_status_t bt_ret;
    expr_dpath_t box_dpath;
    struct bitpunch_error *bp_err = NULL;

    if (EXPR_DPATH_TYPE_CONTAINER == self->dpath.type) {
        return 0;
    }
    bt_ret = expr_dpath_to_dpath(self->dpath,
                                 EXPR_DPATH_TYPE_CONTAINER, &box_dpath,
                                 &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        return -1;
    }
    expr_dpath_destroy(self->dpath);
    self->dpath = box_dpath;
    return 0;
}

static int
DataItem_apply_dpath_filters(DataItemObject *self)
{
    bitpunch_status_t bt_ret;
    expr_dpath_t filtered_dpath;
    struct bitpunch_error *bp_err = NULL;

    if (EXPR_DPATH_TYPE_ITEM == self->dpath.type) {
        bt_ret = tracker_get_filtered_dpath(self->dpath.tk,
                                            &filtered_dpath, &bp_err);
        if (BITPUNCH_OK != bt_ret) {
            set_bitpunch_error(bp_err, bt_ret);
            return -1;
        }
        expr_dpath_destroy(self->dpath);
        self->dpath = filtered_dpath;
    }
    return 0;
}

static PyObject *
DataItem_iter(DataItemObject *self);

static PyObject *
DataItem_iter_keys(DataItemObject *self);

static PyObject *
DataItem_iter_items(DataItemObject *self);

static PyObject *
DataItem_eval_expr(DataItemObject *cont, PyObject *args, PyObject *kwds);

static PyObject *
DataItem_get_size(DataItemObject *self, PyObject *args);

static PyObject *
DataItem_get_offset(DataItemObject *self, PyObject *args);

static PyObject *
DataItem_get_location(DataItemObject *self, PyObject *args);

static PyObject *
DataItem_get_filter_type(DataItemObject *self);

static PyObject *
DataItem___unicode__(DataItemObject *self, PyObject *args);

static PyMethodDef DataItem_methods[] = {
    { "iter_keys", (PyCFunction)DataItem_iter_keys, METH_NOARGS,
      "get an iterator over the array keys"
    },
    { "iter_items", (PyCFunction)DataItem_iter_items, METH_NOARGS,
      "get an iterator over the array items as (key, value) tuples"
    },
    { "eval_expr", (PyCFunction)DataItem_eval_expr,
      METH_VARARGS | METH_KEYWORDS,
      "evaluate a bitpunch expression in the item's scope\n"
      "\n"
      "keyword arguments:\n"
      "tracker -- if True, return a Tracker object pointing\n"
      "to the original object location in place of native Python types "
      "(default is False)"
    },
    { "get_size", (PyCFunction)DataItem_get_size, METH_NOARGS,
      "get the spanned size of the item in the file or "
      "filtered byte contents"
    },
    { "get_offset", (PyCFunction)DataItem_get_offset, METH_NOARGS,
      "get the absolute byte offset of the item from the beginning "
      "of the file or filtered byte contents"
    },
    { "get_location",
      (PyCFunction)DataItem_get_location, METH_NOARGS,
      "get a tuple of the absolute byte offset of the item from "
      "the beginning of the file or filtered byte contents, and the "
      "byte size of the item"
    },
    { "get_filter_type",
      (PyCFunction)DataItem_get_filter_type, METH_NOARGS,
      "get the DataItem's type of filter ('composite', 'array', "
      "'integer' etc.)"
    },
    { "__unicode__",
      (PyCFunction)DataItem___unicode__, METH_NOARGS,
      "convert to unicode string"
    },
    { NULL, NULL, 0, NULL }
};

static Py_ssize_t
DataItem_mp_length(DataItemObject *self)
{
    bitpunch_status_t bt_ret;
    int64_t item_count;
    struct bitpunch_error *bp_err = NULL;

    if (-1 == DataItem_convert_dpath_to_box(self)) {
        return -1;
    }
    bt_ret = box_get_n_items(self->dpath.box, &item_count, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        return -1;
    }
    return (Py_ssize_t)item_count;
}

static PyObject *
DataItem_get_item(DataItemObject *self, PyObject *attr_name,
                  int getattr);

static PyObject *
DataItem_get_keyed_item(DataItemObject *self, PyObject *key,
                        int getattr);

static PyObject *
DataItem_get_slice(DataItemObject *self, PyObject *key);

static PyObject *
DataItem_getattro(DataItemObject *self, PyObject *attr_name)
{
    return DataItem_get_item(self, attr_name, TRUE);
}

static PyObject *
DataItem_mp_subscript(DataItemObject *self, PyObject *key)
{
    if (PySlice_Check(key)) {
        return DataItem_get_slice(self, key);
    } else {
        return DataItem_get_item(self, key, FALSE);
    }
}

static PyMappingMethods DataItem_as_mapping = {
    .mp_length = (lenfunc)DataItem_mp_length,
    .mp_subscript = (binaryfunc)DataItem_mp_subscript,
    .mp_ass_subscript = NULL
};

static PyTypeObject DataItemType = {
    PyObject_HEAD_INIT(NULL)
    0,                           /* ob_size */
    "bitpunch.DataItem",         /* tp_name */
    sizeof(DataItemObject),      /* tp_basicsize */
    0,                           /* tp_itemsize */
    0,                           /* tp_dealloc */
    0,                           /* tp_print */
    0,                           /* tp_getattr */
    0,                           /* tp_setattr */
    0,                           /* tp_compare */
    0,                           /* tp_repr */
    &DataItem_as_number,         /* tp_as_number */
    0,                           /* tp_as_sequence */
    &DataItem_as_mapping,        /* tp_as_mapping */
    0,                           /* tp_hash */
    0,                           /* tp_call */
    0,                           /* tp_str */
    0,                           /* tp_getattro */
    0,                           /* tp_setattro */
    &DataItem_as_buffer,         /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE |
    Py_TPFLAGS_HAVE_NEWBUFFER |
    Py_TPFLAGS_HAVE_RICHCOMPARE |
    Py_TPFLAGS_HAVE_ITER |
    Py_TPFLAGS_CHECKTYPES,       /* tp_flags */
    DataItem__doc__,             /* tp_doc */
    0,                           /* tp_traverse */
    0,                           /* tp_clear */
    0,                           /* tp_richcompare */
    0,                           /* tp_weaklistoffset */
    0,                           /* tp_iter */
    0,                           /* tp_iternext */
    DataItem_methods,            /* tp_methods */
    0,                           /* tp_members */
    0,                           /* tp_getset */
    0,                           /* tp_base */
    0,                           /* tp_dict */
    0,                           /* tp_descr_get */
    0,                           /* tp_descr_set */
    0,                           /* tp_dictoffset */
    0,                           /* tp_init */
    0,                           /* tp_alloc */
    0,                           /* tp_new */
};

static PyObject *
DataItem_new(PyTypeObject *subtype,
             PyObject *args, PyObject *kwds)
{
    DataItemObject *self;

    self = (DataItemObject *)subtype->tp_alloc(subtype, 0);
    if (NULL == self) {
        return NULL;
    }
    self->dpath = expr_dpath_none();
    self->value = expr_value_unset();
    return (PyObject *)self;
}

static PyObject *
DataItem_new_from_tracker(struct BoardObject *dtree, struct tracker *tk)
{
    DataItemObject *self;

    self = (DataItemObject *)DataItem_new(&DataItemType, NULL, NULL);
    if (NULL == self) {
        return NULL;
    }
    self->dtree = dtree;
    Py_INCREF(self->dtree);
    self->dpath = expr_dpath_as_item(tracker_dup(tk));
    return (PyObject *)self;
}

static PyObject *
DataItem_new_from_box(struct BoardObject *dtree, struct box *box)
{
    DataItemObject *self;

    self = (DataItemObject *)DataItem_new(&DataItemType, NULL, NULL);
    if (NULL == self) {
        return NULL;
    }
    self->dtree = dtree;
    Py_INCREF(self->dtree);
    self->dpath = expr_dpath_as_container(box);
    box_acquire(box);
    return (PyObject *)self;
}

static int
DataItem_clear(DataItemObject *self)
{
    expr_dpath_destroy(self->dpath);
    self->dpath = expr_dpath_none();
    expr_value_destroy(self->value);
    self->value = expr_value_unset();
    Py_CLEAR(self->dtree);
    box_delete(self->filtered_box);
    return 0;
}

static void
DataItem_dealloc(DataItemObject *self)
{
    DataItem_clear(self);
    Py_TYPE(self)->tp_free(self);
}

static void
DataItem_construct(DataItemObject *self, struct BoardObject *dtree)
{
    self->dtree = dtree;
    Py_INCREF(dtree);
}

static PyObject *
DataItem_make_python_object(DataItemObject *obj)
{
    switch (obj->dpath.type) {
    case EXPR_DPATH_TYPE_CONTAINER:
        return box_to_deep_PyObject(obj->dtree, obj->dpath.box);
    case EXPR_DPATH_TYPE_ITEM:
        return tracker_item_to_deep_PyObject(obj->dtree, obj->dpath.tk);
    default:
        Py_INCREF(obj);
        return (PyObject *)obj;
    }
}

static PyObject *
DataItem_get_value(DataItemObject *self);

static PyObject *
DataItem_str(DataItemObject *self);

static PyObject *
DataItem_richcompare(DataItemObject *self, PyObject *b, int op)
{
    PyObject *conv_self = NULL;
    PyObject *conv_b = NULL;
    PyObject *comp = NULL;

    conv_self = DataItem_get_value(self);
    if (NULL == conv_self) {
        return NULL;
    }
    if (PyObject_IsInstance((PyObject *)b,
                            (PyObject *)&DataItemType)) {
        conv_b = (PyObject *)DataItem_get_value((DataItemObject *)b);
    } else {
        conv_b = b;
        Py_INCREF(conv_b);
    }
    if (NULL != conv_self && NULL != conv_b) {
        comp = PyObject_RichCompare(conv_self, conv_b, op);
    }
    Py_XDECREF(conv_self);
    Py_XDECREF(conv_b);
    return comp;
}


static int
DataItemType_setup(void)
{
    DataItemType.ob_type = &PyType_Type;
    DataItemType.tp_new = DataItem_new;
    DataItemType.tp_clear = (inquiry)DataItem_clear;
    DataItemType.tp_dealloc = (destructor)DataItem_dealloc;
    DataItemType.tp_str = (reprfunc)DataItem_str;
    DataItemType.tp_getattro = (getattrofunc)DataItem_getattro;
    DataItemType.tp_richcompare = (richcmpfunc)DataItem_richcompare;
    DataItemType.tp_iter = (getiterfunc)DataItem_iter;

    if (PyType_Ready(&DataItemType) < 0) {
        return -1;
    }
    return 0;
}

static PyObject *
DataItem_eval_expr(DataItemObject *item, PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = { "expr", NULL };
    const char *expr;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "s", kwlist, &expr)) {
        return NULL;
    }
    return eval_expr_as_python_object(item, expr);
}

static PyObject *
DataItem_get_size(DataItemObject *self, PyObject *args)
{
    // TODO merge with Tracker_get_size()
    int64_t item_size;
    bitpunch_status_t bt_ret;
    struct bitpunch_error *bp_err = NULL;

    if (-1 == DataItem_apply_dpath_filters(self)) {
        return NULL;
    }
    switch (self->dpath.type) {
    case EXPR_DPATH_TYPE_CONTAINER:
        bt_ret = box_compute_size(self->dpath.box, BOX_SIZE_USED,
                                  &item_size, &bp_err);
        if (BITPUNCH_OK != bt_ret) {
            set_bitpunch_error(bp_err, bt_ret);
            return NULL;
        }
        break ;
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = tracker_get_item_size(self->dpath.tk,
                                       &item_size, &bp_err);
        if (BITPUNCH_OK != bt_ret) {
            set_bitpunch_error(bp_err, bt_ret);
            return NULL;
        }
        break ;
    default:
        assert(0);
    }
    return PyInt_FromLong(item_size);
}

static PyObject *
DataItem_get_offset(DataItemObject *self, PyObject *args)
{
    // TODO merge with Tracker_get_offset()
    int64_t item_offset;
    bitpunch_status_t bt_ret;
    struct bitpunch_error *bp_err = NULL;

    if (-1 == DataItem_apply_dpath_filters(self)) {
        return NULL;
    }
    switch (self->dpath.type) {
    case EXPR_DPATH_TYPE_CONTAINER:
        bt_ret = box_compute_offset(self->dpath.box, BOX_START_OFFSET_USED,
                                    &item_offset, &bp_err);
        if (BITPUNCH_OK != bt_ret) {
            set_bitpunch_error(bp_err, bt_ret);
            return NULL;
        }
        break ;
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = tracker_get_item_offset(self->dpath.tk,
                                         &item_offset, &bp_err);
        if (BITPUNCH_OK != bt_ret) {
            set_bitpunch_error(bp_err, bt_ret);
            return NULL;
        }
        break ;
    default:
        assert(0);
    }
    return PyInt_FromLong(item_offset);
}

static PyObject *
DataItem_get_location(DataItemObject *self, PyObject *args)
{
    // TODO merge with Tracker_get_location()
    bitpunch_status_t bt_ret;
    int64_t item_offset;
    int64_t item_size;
    struct bitpunch_error *bp_err = NULL;

    if (-1 == DataItem_apply_dpath_filters(self)) {
        return NULL;
    }
    switch (self->dpath.type) {
    case EXPR_DPATH_TYPE_CONTAINER:
        bt_ret = box_compute_size(self->dpath.box, BOX_SIZE_USED,
                                  &item_size, &bp_err);
        if (BITPUNCH_OK != bt_ret) {
            set_bitpunch_error(bp_err, bt_ret);
            return NULL;
        }
        item_offset = box_get_offset(self->dpath.box, BOX_START_OFFSET_USED);
        assert(item_offset >= 0);
        break ;
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = tracker_get_item_location(self->dpath.tk,
                                           &item_offset, &item_size, &bp_err);
        if (BITPUNCH_OK != bt_ret) {
            set_bitpunch_error(bp_err, bt_ret);
            return NULL;
        }
        break ;
    default:
        assert(0);
    }
    return Py_BuildValue("ii", item_offset, item_size);
}

static int
DataItem_read_value(DataItemObject *self)
{
    bitpunch_status_t bt_ret;
    struct bitpunch_error *bp_err = NULL;

    bt_ret = dpath_read_value(self->dpath, &self->value, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        return -1;
    }
    return 0;
}

static PyObject *
DataItem_get_value(DataItemObject *self)
{
    if (EXPR_VALUE_TYPE_UNSET == self->value.type
        && -1 == DataItem_read_value(self)) {
        return NULL;
    }
    return expr_value_to_native_PyObject_nodestroy(self->dtree, self->value);
}

static PyObject *
DataItem_str(DataItemObject *self)
{
    PyObject *conv = NULL;
    PyObject *str = NULL;

    conv = DataItem_get_value(self);
    if (NULL == conv) {
        return NULL;
    }
    if (EXPR_VALUE_TYPE_BYTES == self->value.type ||
        EXPR_VALUE_TYPE_STRING == self->value.type) {
        return conv;
    }
    str = PyObject_Str(conv);
    Py_DECREF(conv);
    return str;
}

static PyObject *
DataItem___unicode__(DataItemObject *self, PyObject *args)
{
    // TODO
    // NOTE unicode support should be in expr_value_to_PyObject
    return PyUnicode_Decode("no unicode support!", 19, "latin_1", NULL);
#if 0
    struct box *box;
    bitpunch_status_t bt_ret;
    expr_value_t value;
    struct bitpunch_error *bp_err = NULL;

    box = self->box;
    bt_ret = box_read_value(box, &value, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        return NULL;
    }
    assert(EXPR_VALUE_TYPE_BYTES == value.type);
    // FIXME: use proper encoding configured in string() filter when
    // this is implemented
    return PyUnicode_Decode(value.bytes.buf, (Py_ssize_t)value.bytes.len,
                            "latin_1", NULL);
#endif
}

static const char *
DataItem_get_filter_type_str(DataItemObject *self)
{
    const struct ast_node_hdl *filter;
    const struct ast_node_hdl *item;

    filter = expr_dpath_get_target_filter(self->dpath);
    if (AST_NODE_TYPE_REXPR_FILTER == filter->ndat->type) {
        return filter->ndat->u.rexpr_filter.filter_cls->name;
    }
    item = expr_dpath_get_as_type(self->dpath);
    switch (item->ndat->type) {
    case AST_NODE_TYPE_REXPR_FILTER:
        return item->ndat->u.rexpr_filter.filter_def->filter_type;
    case AST_NODE_TYPE_ARRAY:
        return "array";
    default:
        return "<unknown filter type>";
    }
}

static PyObject *
DataItem_get_filter_type(DataItemObject *self)
{
    return PyString_FromString(DataItem_get_filter_type_str(self));
}

static PyObject *
DataItem_nb_int(DataItemObject *self)
{
    PyObject *conv = NULL;
    PyObject *integer = NULL;

    conv = DataItem_get_value(self);
    if (NULL == conv) {
        return NULL;
    }
    if (EXPR_VALUE_TYPE_INTEGER == self->value.type) {
        return conv;
    }
    if (NULL != conv->ob_type->tp_as_number &&
        NULL != conv->ob_type->tp_as_number->nb_int) {
        integer = conv->ob_type->tp_as_number->nb_int(conv);
    } else {
        PyErr_SetString(PyExc_TypeError,
                        "cannot convert item to integer");
    }
    Py_DECREF(conv);
    return integer;
}

static int DataItem_nb_nonzero(PyObject *self)
{
    return TRUE;
}

static int DataItem_nb_coerce(PyObject **self, PyObject **opd)
{
    // TODO
    return 0;
}

static PyObject *
DataItem_nb_unary_op(enum PyNumberMethods_op op, PyObject *opd)
{
    PyObject *conv_opd = NULL;
    PyObject *res = NULL;
    unaryfunc func = NULL;

    if (PyObject_IsInstance((PyObject *)opd,
                            (PyObject *)&DataItemType)) {
        conv_opd = DataItem_get_value((DataItemObject *)opd);
    } else {
        conv_opd = opd;
        Py_INCREF(conv_opd);
    }
    if (NULL != conv_opd) {
        if (NULL != conv_opd->ob_type->tp_as_number) {
            func = PyNumberMethods_lookup_func(
                conv_opd->ob_type->tp_as_number, op);
        }
        if (NULL != func) {
            res = func(conv_opd);
        } else {
            PyErr_SetString(PyExc_TypeError, "no method to apply operation");
        }
    }
    Py_XDECREF(conv_opd);
    return res;
}

static PyObject *
DataItem_nb_binary_op(enum PyNumberMethods_op op,
                      PyObject *opd1, PyObject *opd2)
{
    PyObject *conv_opd1 = NULL;
    PyObject *conv_opd2 = NULL;
    PyObject *res = NULL;
    binaryfunc func = NULL;

    if (PyObject_IsInstance((PyObject *)opd1,
                            (PyObject *)&DataItemType)) {
        conv_opd1 = DataItem_get_value((DataItemObject *)opd1);
    } else {
        conv_opd1 = opd1;
        Py_INCREF(conv_opd1);
    }
    if (PyObject_IsInstance((PyObject *)opd2,
                            (PyObject *)&DataItemType)) {
        conv_opd2 = DataItem_get_value((DataItemObject *)opd2);
    } else {
        conv_opd2 = opd2;
        Py_INCREF(conv_opd2);
    }
    if (NULL != conv_opd1 && NULL != conv_opd2) {
        if (NULL != conv_opd1->ob_type->tp_as_number) {
            func = PyNumberMethods_lookup_func(
                conv_opd1->ob_type->tp_as_number, op);
        }
        if (NULL != func) {
            res = func(conv_opd1, conv_opd2);
        } else {
            PyErr_SetString(PyExc_TypeError, "no method to apply operation");
        }
    }
    Py_XDECREF(conv_opd1);
    Py_XDECREF(conv_opd2);
    return res;
}

static PyObject *
DataItem_nb_ternary_op(enum PyNumberMethods_op op,
                       PyObject *opd1, PyObject *opd2, PyObject *opd3)
{
    PyObject *conv_opd1 = NULL;
    PyObject *conv_opd2 = NULL;
    PyObject *conv_opd3 = NULL;
    PyObject *res = NULL;
    ternaryfunc func = NULL;

    if (PyObject_IsInstance((PyObject *)opd1,
                            (PyObject *)&DataItemType)) {
        conv_opd1 = DataItem_get_value((DataItemObject *)opd1);
    } else {
        conv_opd1 = opd1;
        Py_INCREF(conv_opd1);
    }
    if (PyObject_IsInstance((PyObject *)opd2,
                            (PyObject *)&DataItemType)) {
        conv_opd2 = (PyObject *)DataItem_get_value((DataItemObject *)opd2);
    } else {
        conv_opd2 = opd2;
        Py_INCREF(conv_opd2);
    }
    if (PyObject_IsInstance((PyObject *)opd3,
                            (PyObject *)&DataItemType)) {
        conv_opd3 = (PyObject *)DataItem_get_value((DataItemObject *)opd3);
    } else {
        conv_opd3 = opd3;
        Py_INCREF(conv_opd3);
    }
    if (NULL != conv_opd1 && NULL != conv_opd2 && NULL != conv_opd3) {
        if (NULL != conv_opd1->ob_type->tp_as_number) {
            func = PyNumberMethods_lookup_func(
                conv_opd1->ob_type->tp_as_number, op);
        }
        if (NULL != func) {
            res = func(conv_opd1, conv_opd2, conv_opd3);
        } else {
            PyErr_SetString(PyExc_TypeError, "no method to apply operation");
        }
    }
    Py_XDECREF(conv_opd1);
    Py_XDECREF(conv_opd2);
    Py_XDECREF(conv_opd3);
    return res;
}

static int
DataItem_bf_getbuffer(DataItemObject *exporter,
                      Py_buffer *view, int flags)
{
    // TODO merge with Tracker_bf_getbuffer()
    struct bitpunch_data_source *filtered_data_source;
    int64_t data_offset;
    int64_t data_size;
    bitpunch_status_t bt_ret;
    const char *buf;
    struct bitpunch_error *bp_err = NULL;

    if (NULL != exporter->filtered_box) {
        box_delete(exporter->filtered_box);
        exporter->filtered_box = NULL;
    }
    bt_ret = expr_dpath_get_filtered_data(
        exporter->dpath,
        &filtered_data_source, &data_offset, &data_size,
        &exporter->filtered_box, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        PyObject *errobj;

        if (NULL != bp_err) {
            errobj = bitpunch_error_get_info_as_PyObject(bp_err);
        } else {
            Py_INCREF(Py_None);
            errobj = Py_None;
        }
        PyErr_SetObject(PyExc_BufferError, errobj);
        Py_DECREF(errobj);
        bitpunch_error_destroy(bp_err);
        view->obj = NULL;
        return -1;
    }
    buf = filtered_data_source->ds_data + data_offset;
    return PyBuffer_FillInfo(view, (PyObject *)exporter,
                             (void *)buf, (Py_ssize_t)data_size,
                             TRUE /* read-only */, flags);
}


static PyObject *
box_to_deep_PyDict(struct BoardObject *dtree, struct box *box)
{
    PyObject *dict;
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    expr_value_t value_eval;
    PyObject *py_key;
    PyObject *py_value;
    struct bitpunch_error *bp_err = NULL;

    dict = PyDict_New();
    if (NULL == dict) {
        return NULL;
    }
    bt_ret = track_box_contents(box, &tk, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        goto bp_error;
    }
    bt_ret = tracker_goto_next_item(tk, &bp_err);
    while (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_get_item_key(tk, &value_eval, &bp_err);
        if (BITPUNCH_OK != bt_ret) {
            goto bp_error;
        }
        py_key = expr_value_to_native_PyObject(dtree, value_eval);
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
        bt_ret = tracker_goto_next_item(tk, &bp_err);
    }
    if (BITPUNCH_NO_ITEM != bt_ret) {
        goto bp_error;
    }
    tracker_delete(tk);
    return dict;

  bp_error:
    set_bitpunch_error(bp_err, bt_ret);
  error:
    tracker_delete(tk);
    Py_DECREF(dict);
    return NULL;
}

static PyObject *
box_get_attributes_dict(struct box *box)
{
    PyObject *dict;
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    PyObject *py_key;
    expr_value_t key_value;
    tfilter_iterator attr_iter;
    const struct named_expr *named_expr;
    struct bitpunch_error *bp_err = NULL;

    /* generate a dict containing all attribute keys on-the-fly */
    dict = PyDict_New();
    if (NULL == dict) {
        return NULL;
    }
    bt_ret = track_box_contents(box, &tk, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        Py_DECREF(dict);
        return NULL;
    }
    bt_ret = tracker_goto_next_item(tk, NULL);
    while (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_get_item_key(tk, &key_value, NULL);
        if (BITPUNCH_OK != bt_ret) {
            break ;
        }
        if (key_value.type != EXPR_VALUE_TYPE_STRING) {
            expr_value_destroy(key_value);
            break ;
        }
        py_key = PyString_FromStringAndSize(key_value.string.str,
                                            key_value.string.len);
        expr_value_destroy(key_value);
        if (NULL != py_key) {
            PyDict_SetItem(dict, py_key, Py_None);
            Py_DECREF(py_key);
        }
        bt_ret = tracker_goto_next_item(tk, NULL);
    }
    tracker_delete(tk);
    attr_iter = filter_iter_statements_in_context(box->filter, box,
                                       STATEMENT_TYPE_NAMED_EXPR |
                                       STATEMENT_TYPE_ATTRIBUTE, NULL);
    bt_ret = filter_iter_statements_next(
        &attr_iter, NULL, (const struct statement **)&named_expr, NULL);
    while (BITPUNCH_OK == bt_ret) {
        py_key = PyString_FromString(named_expr->nstmt.name);
        if (NULL != py_key) {
            PyDict_SetItem(dict, py_key, Py_None);
            Py_DECREF(py_key);
        }
        bt_ret = filter_iter_statements_next(
            &attr_iter, NULL, (const struct statement **)&named_expr, NULL);
    }
    return dict;
}


static PyObject *
DataItem_eval_attr(DataItemObject *self, const char *attr_str,
                   int getattr)
{
    bitpunch_status_t bt_ret;
    expr_value_t attr_value;
    expr_dpath_t attr_dpath;
    struct bitpunch_error *bp_err = NULL;

    if (-1 == DataItem_convert_dpath_to_box(self)) {
        return NULL;
    }
    bt_ret = filter_evaluate_identifier(
        self->dpath.box->filter, self->dpath.box,
        STATEMENT_TYPE_FIELD |
        STATEMENT_TYPE_NAMED_EXPR |
        STATEMENT_TYPE_ATTRIBUTE, attr_str,
        EXPR_EVALFLAG_DPATH_XOR_VALUE,
        &attr_value, &attr_dpath, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        if (BITPUNCH_NO_ITEM == bt_ret) {
            PyErr_Format(getattr ?
                         PyExc_AttributeError : PyExc_IndexError,
                         "no such named expression or field: %s",
                         attr_str);
        } else {
            set_bitpunch_error(bp_err, bt_ret);
        }
        return NULL;
    }
    return expr_value_and_dpath_to_PyObject(self->dtree,
                                            attr_value, attr_dpath);
}

static PyObject *
DataItem_get_item(DataItemObject *self, PyObject *attr_name,
                  int getattr)
{
    PyObject *attr;
    const char *attr_str;

    if (PyString_Check(attr_name)) {
        attr_str = PyString_AS_STRING(attr_name);
        if (getattr) {
            if (0 == strcmp(attr_str, "__dict__")) {
                if (-1 == DataItem_convert_dpath_to_box(self)) {
                    return NULL;
                }
                return box_get_attributes_dict(self->dpath.box);
            }
            attr = PyObject_GenericGetAttr((PyObject *)self, attr_name);
            if (NULL != attr) {
                return attr;
            }
            if (!PyErr_ExceptionMatches(
                    getattr ? PyExc_AttributeError : PyExc_IndexError)) {
                return NULL;
            }
            PyErr_Clear();
        }
        attr = DataItem_eval_attr(self, attr_str, getattr);
        if (NULL != attr) {
            return attr;
        }
        if (!PyErr_ExceptionMatches(
                getattr ? PyExc_AttributeError : PyExc_IndexError)) {
            return NULL;
        }
        PyErr_Clear();
    }
    return DataItem_get_keyed_item(self, attr_name, getattr);
}

static struct tracker *
DataItem_get_tracker_at_key(
    DataItemObject *self, PyObject *key, int getattr,
    expr_value_t *keyp, int *all_twinsp)
{
    struct tracker *tk;
    PyObject *key_obj;
    bitpunch_status_t bt_ret;
    struct bitpunch_error *bp_err = NULL;

    if (PyObject_TypeCheck(key, &IndexKeyType)) {
        key_obj = ((IndexKeyObject *)key)->key;
    } else {
        key_obj = key;
    }
    if (-1 == DataItem_convert_dpath_to_box(self)) {
        return NULL;
    }
    bt_ret = track_box_contents(self->dpath.box, &tk, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        return NULL;
    }
    if (-1 == tk_goto_item_by_key(tk, key_obj, keyp, all_twinsp)) {
        tracker_delete(tk);
        if (PyErr_Occurred() == BitpunchExc_NoItemError) {
            PyObject *repr_key;

            PyErr_Clear();
            repr_key = PyObject_Repr(key);
            if (NULL != repr_key) {
                PyErr_Format(getattr ?
                             PyExc_AttributeError : PyExc_IndexError,
                             "no item with key: %s",
                             PyString_AS_STRING(repr_key));
                Py_DECREF(repr_key);
            }
        }
        return NULL;
    }
    return tk;
}

static PyObject *
DataItem_get_keyed_item(DataItemObject *self, PyObject *key,
                        int getattr)
{
    PyObject *res = NULL;
    struct tracker *tk;
    expr_value_t tk_key;
    int all_twins;

    tk = DataItem_get_tracker_at_key(self, key, getattr, &tk_key, &all_twins);
    if (NULL == tk) {
        return NULL;
    }
    if (all_twins) {
        PyObject *item;

        res = PyList_New(0);
        if (NULL == res) {
            tracker_delete(tk);
            expr_value_destroy(tk_key);
            return NULL;
        }
        while (TRUE) {
            bitpunch_status_t bt_ret;
            struct bitpunch_error *bp_err = NULL;

            item = tracker_item_to_shallow_PyObject(self->dtree, tk);
            if (NULL == item) {
                Py_DECREF(res);
                tracker_delete(tk);
                expr_value_destroy(tk_key);
                return NULL;
            }
            if (Py_None == item) {
                // end of matching elements list
                break ;
            }
            if (-1 == PyList_Append(res, item)) {
                Py_DECREF(res);
                tracker_delete(tk);
                expr_value_destroy(tk_key);
                return NULL;
            }
            bt_ret = tracker_goto_next_item_with_key(tk, tk_key, &bp_err);
            if (BITPUNCH_OK != bt_ret) {
                if (BITPUNCH_NO_ITEM == bt_ret) {
                    break ;
                }
                Py_DECREF(res);
                set_bitpunch_error(bp_err, bt_ret);
                tracker_delete(tk);
                expr_value_destroy(tk_key);
                return NULL;
            }
        }
    } else {
        res = tracker_item_to_shallow_PyObject(self->dtree, tk);
    }
    tracker_delete(tk);
    expr_value_destroy(tk_key);
    return res;
}

static PyObject *
DataItem_get_slice(DataItemObject *self, PyObject *key)
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
    struct bitpunch_error *bp_err = NULL;

    if (-1 == DataItem_convert_dpath_to_box(self)) {
        return NULL;
    }
    bt_ret = box_get_n_items(self->dpath.box, &item_count, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
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
    bt_ret = track_box_contents(self->dpath.box, &tk_start, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        goto end;
    }
    bt_ret = tracker_goto_nth_position(tk_start, slice_start, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        goto end;
    }
    bt_ret = track_box_contents(self->dpath.box, &tk_end, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        goto end;
    }
    bt_ret = tracker_goto_nth_position(tk_end, slice_stop, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        goto end;
    }
    browse_state_init_tracker(&bst, tk_start);
    slice_box = box_new_slice_box(tk_start, tk_end, &bst);
    if (NULL == slice_box) {
        if (NULL != bst.last_error) {
            bt_ret = bst.last_error->bt_ret;
        } else {
            bt_ret = BITPUNCH_DATA_ERROR;
        }
        set_bitpunch_error(bst.last_error, bt_ret);
        goto end;
    }
    res = DataItem_new_from_box(self->dtree, slice_box);
    box_delete(slice_box);
  end:
    tracker_delete(tk_start);
    tracker_delete(tk_end);
    return res;
}

static PyObject *
box_to_deep_PyList(struct BoardObject *dtree, struct box *box)
{
    PyObject *list = NULL;
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    int64_t item_count;
    PyObject *py_value;
    int64_t index;
    struct bitpunch_error *bp_err = NULL;

    bt_ret = track_box_contents(box, &tk, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        goto bp_error;
    }
    bt_ret = tracker_get_n_items(tk, &item_count, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        goto bp_error;
    }
    list = PyList_New(item_count);
    if (NULL == list) {
        goto error;
    }
    index = 0;
    bt_ret = tracker_goto_next_item(tk, &bp_err);
    while (BITPUNCH_OK == bt_ret) {
        py_value = tracker_item_to_deep_PyObject(dtree, tk);
        if (NULL == py_value) {
            goto error;
        }
        PyList_SET_ITEM(list, index, py_value);
        ++index;
        bt_ret = tracker_goto_next_item(tk, &bp_err);
    }
    if (BITPUNCH_NO_ITEM != bt_ret) {
        goto bp_error;
    }
    tracker_delete(tk);
    return list;

  bp_error:
    set_bitpunch_error(bp_err, bt_ret);
  error:
    tracker_delete(tk);
    Py_XDECREF(list);
    return NULL;
}

static PyObject *
box_to_native_PyObject(struct BoardObject *dtree, struct box *box)
{
    bitpunch_status_t bt_ret;
    expr_value_t value_eval;
    struct bitpunch_error *bp_err = NULL;

    assert(ast_node_is_item(box->filter));

    bt_ret = box_read_value(box, &value_eval, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        return NULL;
    }
    return expr_value_to_native_PyObject(dtree, value_eval);
}

static PyObject *
tracker_item_to_deep_PyObject(BoardObject *dtree, struct tracker *tk)
{
    PyObject *res = NULL;
    bitpunch_status_t bt_ret;
    expr_value_t value_eval;
    int complex_type;
    struct box *filtered_box;
    struct bitpunch_error *bp_err = NULL;

    if (tracker_is_dangling(tk)) {
        Py_INCREF(Py_None);
        return Py_None;
    }
    complex_type = dpath_is_complex_type(&tk->dpath);
    if (complex_type) {
        bt_ret = tracker_get_filtered_item_box(tk, &filtered_box, &bp_err);
        if (BITPUNCH_OK == bt_ret) {
            res = box_to_deep_PyObject(dtree, filtered_box);
            box_delete(filtered_box);
        } else {
            set_bitpunch_error(bp_err, bt_ret);
        }
    } else {
        bt_ret = tracker_read_item_value(tk, &value_eval, &bp_err);
        if (BITPUNCH_OK != bt_ret) {
            set_bitpunch_error(bp_err, bt_ret);
            return NULL;
        }
        // TODO: may be interesting to return DataItem items instead
        // of native values
        res = expr_value_to_native_PyObject(dtree, value_eval);
    }
    return res;
}

static PyObject *
box_to_deep_PyObject(struct BoardObject *dtree, struct box *box)
{
    struct ast_node_hdl *node;

    node = ast_node_get_as_type(box->filter);
    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY_SLICE:
        return box_to_deep_PyList(dtree, box);
    case AST_NODE_TYPE_BYTE_SLICE:
        return box_to_native_PyObject(dtree, box);
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_REXPR_FILTER: {
        if (ast_node_filter_maps_list(node)) {
            return box_to_deep_PyList(dtree, box);
        }
        if (ast_node_filter_maps_object(node)) {
            return box_to_deep_PyDict(dtree, box);
        }
        return box_to_native_PyObject(dtree, box);
    }
    default:
        PyErr_Format(PyExc_ValueError,
                     "Cannot convert container type '%s' to python object",
                     ast_node_type_str(box->filter->ndat->type));
        return NULL;
    }
    /*NOT REACHED*/
}


/*
 * Tracker
 */

#define X_ITER_TYPES                                                    \
    X(ITER_FIELD_NAMES, "iterate over field names")                     \
    X(ITER_FIELD_VALUES, "iterate over field values")                   \
    X(ITER_FIELD_KEYVALUE_TUPLES, "iterate over field's (key, value) tuples") \
    X(ITER_ATTRIBUTE_NAMES, "iterate over all attributes (named exprs and properties)") \
    X(ITER_MEMBER_NAMES, "iterate over all member's names (fields and " \
      "named expressions)")                                             \


#define X(NAME, DESC) TRACKER_##NAME,

typedef enum TrackerIterType {
    X_ITER_TYPES
} TrackerIterType;

#undef X

PyDoc_STRVAR(Tracker__doc__,
             "A Tracker object is able to browse items over a "
             "Board object. It can iterate a sequence of objects "
             "and enter any complex object to further iterate its "
             "sub-elements.");

typedef struct TrackerObject {
    PyObject_HEAD
    BoardObject *dtree;
    struct tracker *tk;
    TrackerIterType iter_mode;
    TrackerIterType current_iter_mode;
    tfilter_iterator attr_iter;
} TrackerObject;


static int
Tracker_set_default_iter_mode(TrackerObject *self)
{
    struct ast_node_hdl *node;

    node = self->tk->box->filter;
    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
        if (box_contains_indexed_items(self->tk->box)) {
            self->iter_mode = TRACKER_ITER_FIELD_NAMES;
        } else {
            self->iter_mode = TRACKER_ITER_FIELD_VALUES;
        }
        break ;
    case AST_NODE_TYPE_REXPR_FILTER:
        if (ast_node_filter_maps_list(node)) {
            self->iter_mode = TRACKER_ITER_FIELD_VALUES;
        } else {
            self->iter_mode = TRACKER_ITER_FIELD_NAMES;
        }
        break ;
    default:
        PyErr_Format(PyExc_TypeError,
                     "container of type '%s' cannot be iterated",
                     ast_node_type_str(self->tk->box->filter->ndat->type));
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
    bitpunch_status_t bt_ret;
    struct bitpunch_error *bp_err = NULL;

    bt_ret = tracker_goto_end(self->tk, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        return NULL;
    }
    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

static PyObject *
Tracker_goto_next_item(TrackerObject *self)
{
    bitpunch_status_t bt_ret;
    struct bitpunch_error *bp_err = NULL;

    bt_ret = tracker_goto_next_item(self->tk, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        return NULL;
    }
    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

static PyObject *
Tracker_goto_nth_item(TrackerObject *self, PyObject *args)
{
    bitpunch_status_t bt_ret;
    int64_t n;
    struct bitpunch_error *bp_err = NULL;

    if (!PyArg_ParseTuple(args, "l", &n)) {
        return NULL;
    }
    bt_ret = tracker_goto_nth_item(self->tk, n, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        return NULL;
    }
    Py_INCREF((PyObject *)self);
    return (PyObject *)self;
}

static PyObject *
Tracker_goto_named_item(TrackerObject *self, PyObject *args)
{
    bitpunch_status_t bt_ret;
    const char *name;
    struct bitpunch_error *bp_err = NULL;

    if (!PyArg_ParseTuple(args, "s", &name)) {
        return NULL;
    }
    bt_ret = tracker_goto_named_item(self->tk, name, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
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
    bitpunch_status_t bt_ret;
    PyObject *index_value;
    int64_t twin_index;
    int all_twins;
    enum expr_value_type key_expected_type;
    expr_value_t key;
    int indexed_access;
    struct bitpunch_error *bp_err = NULL;

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
    if (-1 == expr_value_from_PyObject(index_value, FALSE, &key)) {
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
            expr_value_destroy(key);
            return -1;
        }
        if (key.type != key_expected_type) {
            PyErr_Format(PyExc_TypeError,
                         "key of type '%s' does not match index type '%s'",
                         expr_value_type_str(key.type),
                         expr_value_type_str(key_expected_type));
            expr_value_destroy(key);
            return -1;
        }
        bt_ret = tracker_goto_nth_item_with_key(tk, key, twin_index, &bp_err);
    } else {
        switch (key.type) {
        case EXPR_VALUE_TYPE_INTEGER:
            bt_ret = tracker_goto_nth_item(tk, key.integer, &bp_err);
            break ;
        default:
            bt_ret = tracker_goto_first_item_with_key(tk, key, &bp_err);
            break ;
        }
    }
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        expr_value_destroy(key);
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
    bitpunch_status_t bt_ret;
    struct bitpunch_error *bp_err = NULL;

    bt_ret = tracker_enter_item(self->tk, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
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
    bitpunch_status_t bt_ret;
    struct bitpunch_error *bp_err = NULL;

    bt_ret = tracker_return(self->tk, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
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
    bitpunch_status_t bt_ret;
    int64_t n_items;
    struct bitpunch_error *bp_err = NULL;

    bt_ret = tracker_get_n_items(self->tk, &n_items, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
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
    struct bitpunch_error *bp_err = NULL;

    bt_ret = tracker_get_item_key_multi(self->tk, &key, &twin_index, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        return NULL;
    }
    py_key_value = expr_value_to_native_PyObject(self->dtree, key);
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
    struct bitpunch_error *bp_err = NULL;

    bt_ret = tracker_get_item_size(self->tk, &item_size, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        return NULL;
    }
    return PyInt_FromLong(item_size);
}

static PyObject *
Tracker_get_offset(TrackerObject *self)
{
    int64_t item_offset;
    bitpunch_status_t bt_ret;
    struct bitpunch_error *bp_err = NULL;

    bt_ret = tracker_get_item_offset(self->tk, &item_offset, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        return NULL;
    }
    return PyInt_FromLong(item_offset);
}

static PyObject *
Tracker_get_location(TrackerObject *self)
{
    bitpunch_status_t bt_ret;
    int64_t item_offset;
    int64_t item_size;
    struct bitpunch_error *bp_err = NULL;

    bt_ret = tracker_get_item_location(self->tk, &item_offset, &item_size,
                                       &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
        return NULL;
    }
    return Py_BuildValue("ii", item_offset, item_size);
}

static PyObject *
Tracker_read_item_raw(TrackerObject *self)
{
    bitpunch_status_t bt_ret;
    const char *item_contents;
    int64_t item_size;
    Py_buffer view;
    PyObject *memview;
    struct bitpunch_error *bp_err = NULL;

    bt_ret = tracker_read_item_raw(self->tk, &item_contents, &item_size,
                                   &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
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
      "get the target item's value, either as a DataItem object for "
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


static PyBufferProcs Tracker_as_buffer = {
    .bf_getbuffer = (getbufferproc)Tracker_bf_getbuffer,
};

static PyTypeObject TrackerType = {
    PyObject_HEAD_INIT(NULL)
    0,                         /* ob_size */
    "bitpunch.Tracker",        /* tp_name */
    sizeof(TrackerObject),     /* tp_basicsize */
    0,                         /* tp_itemsize */
    0,                         /* tp_dealloc */
    0,                         /* tp_print */
    0,                         /* tp_getattr */
    0,                         /* tp_setattr */
    0,                         /* tp_compare */
    0,                         /* tp_repr */
    0,                         /* tp_as_number */
    0,                         /* tp_as_sequence */
    0,                         /* tp_as_mapping */
    0,                         /* tp_hash */
    0,                         /* tp_call */
    0,                         /* tp_str */
    0,                         /* tp_getattro */
    0,                         /* tp_setattro */
    &Tracker_as_buffer,        /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE |
    Py_TPFLAGS_HAVE_NEWBUFFER |
    Py_TPFLAGS_HAVE_ITER,      /* tp_flags */
    Tracker__doc__,            /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,                         /* tp_richcompare */
    0,                         /* tp_weaklistoffset */
    0,                         /* tp_iter */
    0,                         /* tp_iternext */
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
Tracker_new_from_DataItem(PyTypeObject *subtype, DataItemObject *item)
{
    TrackerObject *self;
    bitpunch_status_t bt_ret;
    struct bitpunch_error *bp_err = NULL;

    self = (TrackerObject *)subtype->tp_alloc(subtype, 0);
    if (NULL == self) {
        return NULL;
    }
    self->dtree = item->dtree;
    Py_INCREF(self->dtree);
    bt_ret = track_dpath_contents(item->dpath, &self->tk, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        set_bitpunch_error(bp_err, bt_ret);
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
    if (PyObject_TypeCheck(arg, &DataItemType)) {
        self = Tracker_new_from_DataItem(subtype, (DataItemObject *)arg);
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
                        "The argument must be a bitpunch.DataItem "
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
    if (TRACKER_ITER_ATTRIBUTE_NAMES == self->iter_mode) {
        self->attr_iter = filter_iter_statements_in_context(
            self->tk->box->filter, self->tk->box,
            STATEMENT_TYPE_NAMED_EXPR | STATEMENT_TYPE_ATTRIBUTE, NULL);
    }
    return (PyObject *)self;
}

static PyObject *
Tracker_iternext(TrackerObject *self)
{
    if (TRACKER_ITER_ATTRIBUTE_NAMES != self->current_iter_mode) {
        PyObject *iter;

        iter = Tracker_goto_next_item(self);
        if (NULL == iter) {
            if (PyErr_ExceptionMatches(BitpunchExc_NoItemError)) {
                PyErr_Clear();
                if (TRACKER_ITER_MEMBER_NAMES == self->current_iter_mode) {
                    self->current_iter_mode = TRACKER_ITER_ATTRIBUTE_NAMES;
                    self->attr_iter = filter_iter_statements_in_context(
                        self->tk->box->filter, self->tk->box,
                        STATEMENT_TYPE_NAMED_EXPR | STATEMENT_TYPE_ATTRIBUTE,
                        NULL);
                } else {
                    /* StopIteration is implicitly set by the API */
                    return NULL;
                }
            } else {
                return NULL;
            }
        } else {
            Py_DECREF(iter);
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
    case TRACKER_ITER_ATTRIBUTE_NAMES: {
        bitpunch_status_t bt_ret;
        const struct named_expr *named_expr;
        struct bitpunch_error *bp_err = NULL;

        bt_ret = filter_iter_statements_next(
            &self->attr_iter, NULL,
            (const struct statement **)&named_expr, &bp_err);
        if (BITPUNCH_NO_ITEM == bt_ret) {
            PyErr_Clear();
            /* StopIteration is implicitly set by the API */
            return NULL;
        }
        if (BITPUNCH_OK != bt_ret) {
            set_bitpunch_error(bp_err, bt_ret);
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
    struct bitpunch_error *bp_err = NULL;

    tk = exporter->tk;
    // TODO consider using tracker_read_item_raw()
    bt_ret = tracker_get_item_location(tk, &item_offset, &item_size,
                                       &bp_err);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = box_apply_filter(tk->box, &bp_err);
    }
    if (BITPUNCH_OK != bt_ret) {
        PyObject *errobj;

        if (NULL != bp_err) {
            errobj = bitpunch_error_get_info_as_PyObject(bp_err);
        } else {
            Py_INCREF(Py_None);
            errobj = Py_None;
        }
        PyErr_SetObject(PyExc_BufferError, errobj);
        Py_DECREF(errobj);
        bitpunch_error_destroy(bp_err);
        view->obj = NULL;
        return -1;
    }
    buf = tk->box->ds_in->ds_data + item_offset;
    return PyBuffer_FillInfo(view, (PyObject *)exporter,
                             (void *)buf, (Py_ssize_t)item_size,
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
DataItem_iter_keys(DataItemObject *self)
{
    TrackerObject *tracker;

    tracker = Tracker_new_from_DataItem(&TrackerType, self);
    if (NULL != tracker) {
        tracker->iter_mode = TRACKER_ITER_FIELD_NAMES;
    }
    return (PyObject *)tracker;
}

static PyObject *
DataItem_iter_items(DataItemObject *self)
{
    TrackerObject *tracker;

    tracker = Tracker_new_from_DataItem(&TrackerType, self);
    if (NULL != tracker) {
        tracker->iter_mode = TRACKER_ITER_FIELD_KEYVALUE_TUPLES;
    }
    return (PyObject *)tracker;
}

static PyObject *
DataItem_iter(DataItemObject *self)
{
    TrackerObject *iter;

    iter = Tracker_new_from_DataItem(&TrackerType, self);
    return (PyObject *)iter;
}


static int
expr_value_from_PyObject(PyObject *py_expr, int create_data_source,
                         expr_value_t *exprp)
{
    if (PyInt_Check(py_expr)) {
        *exprp = expr_value_as_integer(PyInt_AS_LONG(py_expr));
    } else if (PyBool_Check(py_expr)) {
        *exprp = expr_value_as_boolean(py_expr == Py_True);
    } else if (PyString_Check(py_expr)) {
        char *str;
        Py_ssize_t length;

        PyString_AsStringAndSize(py_expr, &str, &length);
        if (create_data_source) {
            *exprp = expr_value_as_data(
                python_data_source_new_from_memory(py_expr, str, length));
        } else {
            *exprp = expr_value_as_string_len(str, length);
        }
    } else if (PyByteArray_Check(py_expr)) {
        if (create_data_source) {
            *exprp = expr_value_as_data(
                python_data_source_new_from_memory(
                    py_expr,
                    PyByteArray_AS_STRING(py_expr),
                    PyByteArray_GET_SIZE(py_expr)));
        } else {
            *exprp = expr_value_as_bytes(PyByteArray_AS_STRING(py_expr),
                                         PyByteArray_GET_SIZE(py_expr));
        }
    } else {
        PyErr_Format(PyExc_TypeError,
                     "unsupported expression type '%s'",
                     Py_TYPE(py_expr)->tp_name);
        return -1;
    }
    return 0;
}

/**
 * @brief convert an expression into a native-typed python object
 */
static PyObject *
expr_value_to_native_PyObject_nodestroy(BoardObject *dtree,
                                        expr_value_t value_eval)
{
    // TODO cleanup data and data_range support
    switch (value_eval.type) {
    case EXPR_VALUE_TYPE_INTEGER:
        return Py_BuildValue("l", value_eval.integer);
    case EXPR_VALUE_TYPE_BOOLEAN:
        return PyBool_FromLong(value_eval.boolean);
    case EXPR_VALUE_TYPE_STRING:
        return PyString_FromStringAndSize(value_eval.string.str,
                                          (Py_ssize_t)value_eval.string.len);
    case EXPR_VALUE_TYPE_BYTES:
        return PyString_FromStringAndSize(value_eval.bytes.buf,
                                          (Py_ssize_t)value_eval.bytes.len);
    case EXPR_VALUE_TYPE_DATA:
        return PyString_FromStringAndSize(
            value_eval.data.ds->ds_data,
            (Py_ssize_t)value_eval.data.ds->ds_data_length);
    case EXPR_VALUE_TYPE_DATA_RANGE:
        return PyString_FromStringAndSize(
            value_eval.data.ds->ds_data + value_eval.data_range.start_offset,
            (Py_ssize_t)(value_eval.data_range.end_offset -
                         value_eval.data_range.start_offset));
    default:
        return PyErr_Format(PyExc_ValueError,
                            "unsupported expression type '%d'",
                            (int)value_eval.type);
    }
}

/**
 * @brief convert an expression into a native-typed python object
 *
 * @note this function call destroys @ref value_eval
 */
static PyObject *
expr_value_to_native_PyObject(BoardObject *dtree,
                              expr_value_t value_eval)
{
    PyObject *res;

    res = expr_value_to_native_PyObject_nodestroy(dtree, value_eval);
    expr_value_destroy(value_eval);
    return res;
}

/**
 * @brief convert an expression into a DataItem if expression refers
 * to a data source item, or a native-typed python object otherwise
 *
 * @note this function call destroys or steals @ref value and @ref
 * dpath
 */
static PyObject *
expr_value_and_dpath_to_PyObject(BoardObject *dtree,
                                 expr_value_t value, expr_dpath_t dpath)
{
    DataItemObject *item;

    if (EXPR_DPATH_TYPE_NONE == dpath.type) {
        return expr_value_to_native_PyObject(dtree, value);
    }
    item = (DataItemObject *)DataItem_new(&DataItemType, NULL, NULL);
    if (NULL == item) {
        return NULL;
    }
    DataItem_construct(item, dtree);
    item->value = value;
    item->dpath = dpath;
    return (PyObject *)item;
}

static PyObject *
eval_expr_as_python_object(DataItemObject *item, const char *expr)
{
    BoardObject *dtree;
    struct bitpunch_board *board;
    struct box *scope;
    bitpunch_status_t bt_ret;
    expr_value_t expr_value;
    expr_dpath_t expr_dpath;
    struct bitpunch_error *bp_err = NULL;

    if (NULL != item) {
        if (-1 == DataItem_convert_dpath_to_box(item)) {
            return NULL;
        }
        dtree = item->dtree;
        board = dtree->board;
        scope = item->dpath.box;
    } else {
        dtree = NULL;
        board = NULL;
        scope = NULL;
    }

    bt_ret = bitpunch_eval_expr(
        board, expr, scope,
        BITPUNCH_EVAL_DPATH_XOR_VALUE,
        NULL, &expr_value, &expr_dpath, &bp_err);
    if (BITPUNCH_OK != bt_ret) {
        if (NULL != bp_err) {
            set_bitpunch_error(bp_err, bt_ret);
        } else {
            PyErr_Format(PyExc_ValueError,
                         "Error evaluating expression '%s'", expr);
        }
        return NULL;
    }
    return expr_value_and_dpath_to_PyObject(dtree, expr_value, expr_dpath);
}

static PyObject *
tracker_item_to_shallow_PyObject(BoardObject *dtree,
                                 struct tracker *tk)
{
    if (tracker_is_dangling(tk)) {
        Py_INCREF(Py_None);
        return Py_None;
    }
    return DataItem_new_from_tracker(dtree, tk);
}

/*
 * bitpunch
 */

static int
setup_exceptions(PyObject *bitpunch_m)
{
    BitpunchExc_NoItemError = PyErr_NewException("bitpunch.model.NoItemError",
                                                NULL, NULL);
    PyModule_AddObject(bitpunch_m, "NoItemError", BitpunchExc_NoItemError);

    BitpunchExc_DataError = PyErr_NewException("bitpunch.model.DataError",
                                              NULL, NULL);
    PyModule_AddObject(bitpunch_m, "DataError", BitpunchExc_DataError);

    BitpunchExc_OutOfBoundsError =
        PyErr_NewException("bitpunch.model.OutOfBoundsError", NULL, NULL);
    PyModule_AddObject(bitpunch_m, "OutOfBoundsError",
                       BitpunchExc_OutOfBoundsError);

    BitpunchExc_NoDataError = PyErr_NewException("bitpunch.model.NoDataError",
                                                 NULL, NULL);
    PyModule_AddObject(bitpunch_m, "NoDataError", BitpunchExc_NoDataError);
    return 0;
}


/* bitpunch methods */

static PyObject *
mod_bitpunch_make_python_object(PyObject *self, PyObject *obj)
{
    if (PyObject_TypeCheck(obj, &DataItemType)) {
        return DataItem_make_python_object((DataItemObject *)obj);
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
    return eval_expr_as_python_object(NULL, expr);
}

static PyObject *
mod_bitpunch_get_builtin_names(PyObject *self,
                               PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = { "prefix", NULL };
    const char *prefix = "";
    int prefix_len;
    const char *prev_builtin_name;
    const char *next_builtin_name;
    PyObject *list;
    PyObject *item;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "|s", kwlist, &prefix)) {
        return NULL;
    }
    list = PyList_New(0);
    if (NULL == list) {
        return NULL;
    }
    prefix_len = strlen(prefix);
    next_builtin_name = expr_find_first_builtin(prefix, NULL);
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
        next_builtin_name = expr_find_next_builtin(prev_builtin_name, NULL);
    }
    return list;
}

static PyObject *
mod_bitpunch_notify_file_change(PyObject *self, PyObject *args)
{
    const char *path;

    if (!PyArg_ParseTuple(args, "s", &path)) {
        return NULL;
    }
    bitpunch_data_source_notify_file_change(path);
    Py_INCREF(Py_None);
    return Py_None;
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

    { "notify_file_change", (PyCFunction)mod_bitpunch_notify_file_change,
      METH_VARARGS,
      "notify bitpunch when an underlying file used as data source has "
      "changed (so to refresh the cache)"
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

    /* SpecNode */
    if (SpecNodeType_setup() < 0) {
        return ;
    }
    Py_INCREF(&SpecNodeType);
    PyModule_AddObject(bitpunch_m,
                       "SpecNode", (PyObject *)&SpecNodeType);

    /* IndexKey */
    if (IndexKeyType_setup() < 0) {
        return ;
    }
    Py_INCREF(&IndexKeyType);
    PyModule_AddObject(bitpunch_m,
                       "IndexKey", (PyObject *)&IndexKeyType);

    /* DataItem */
    if (DataItemType_setup() < 0) {
        return ;
    }
    Py_INCREF(&DataItemType);
    PyModule_AddObject(bitpunch_m, "DataItem", (PyObject *)&DataItemType);

    /* ScopeIter */
    if (ScopeIterType_setup() < 0) {
        return ;
    }
    Py_INCREF(&ScopeIterType);
    PyModule_AddObject(bitpunch_m,
                       "ScopeIter", (PyObject *)&ScopeIterType);

    /* Board */
    if (BoardType_setup() < 0) {
        return ;
    }
    Py_INCREF(&BoardType);
    PyModule_AddObject(bitpunch_m, "Board", (PyObject *)&BoardType);

    /* Tracker */
    if (TrackerType_setup() < 0) {
        return ;
    }
    Py_INCREF(&TrackerType);
    PyModule_AddObject(bitpunch_m, "Tracker", (PyObject *)&TrackerType);
}
