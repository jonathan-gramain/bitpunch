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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stddef.h>
#include <assert.h>

#include "core/interpreter.h"
#include "core/browse_internal.h"
#include "core/expr_internal.h"

#define MAX_INTERPRETER_COUNT           256

struct interpreter interpreter_table[MAX_INTERPRETER_COUNT];
int                interpreter_count = 0;

#define MAX_INTERPRETER_ATTR_DEF_COUNT 1024
#define INTERPRETER_MAX_ATTR_REF       256

struct interpreter_attr_def interpreter_attr_def_table[MAX_INTERPRETER_ATTR_DEF_COUNT];
int                          interpreter_attr_def_count = 0;

static struct interpreter *
interpreter_new(void)
{
    if (interpreter_count == MAX_INTERPRETER_COUNT) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "Too many interpreters (max %d)\n",
                         MAX_INTERPRETER_COUNT);
        return NULL;
    }
    return &interpreter_table[interpreter_count++];
}

static struct interpreter_attr_def *
interpreter_attr_def_new(void)
{
    if (interpreter_attr_def_count == MAX_INTERPRETER_ATTR_DEF_COUNT) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "Global interpreter attr def limit reached (max %d)\n",
                         MAX_INTERPRETER_ATTR_DEF_COUNT);
        return NULL;
    }
    return &interpreter_attr_def_table[interpreter_attr_def_count++];
}

int
interpreter_declare(const char *name,
                    enum expr_value_type value_type_mask,
                    interpreter_rcall_build_func_t rcall_build_func,
                    int n_attrs,
                    ... /* attrs: (name, index, type, flags) tuples */)
{
    struct interpreter *interpreter;
    int max_attr_ref;
    va_list ap;
    int i;
    struct interpreter_attr_def *attr_def;

    assert(NULL != rcall_build_func);

    interpreter = interpreter_lookup(name);
    if (NULL != interpreter) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "duplicate interpreter '%s'\n", name);
        return -1;
    }
    interpreter = interpreter_new();
    if (NULL == interpreter) {
        return -1;
    }
    interpreter->name = name;
    interpreter->value_type_mask = value_type_mask;
    interpreter->rcall_build_func = rcall_build_func;
    interpreter->n_attrs = n_attrs;
    max_attr_ref = -1;
    STAILQ_INIT(&interpreter->attr_list);
    va_start(ap, n_attrs);
    for (i = 0; i < n_attrs; ++i) {
        attr_def = interpreter_attr_def_new();
        if (NULL == attr_def) {
            return -1;
        }
        attr_def->name = va_arg(ap, const char *);
        attr_def->ref_idx = va_arg(ap, int);
        assert(attr_def->ref_idx >= 0 &&
               attr_def->ref_idx <= INTERPRETER_MAX_ATTR_REF);
        attr_def->value_type_mask = va_arg(ap, enum ast_node_type);
        attr_def->flags = va_arg(ap, enum interpreter_attr_flags);
        STAILQ_INSERT_TAIL(&interpreter->attr_list, attr_def, list);
        max_attr_ref = MAX(max_attr_ref, attr_def->ref_idx);
    }
    va_end(ap);
    interpreter->max_attr_ref = max_attr_ref;
    return 0;
}

struct interpreter *
interpreter_lookup(const char *name)
{
    int i;

    for (i = 0; i < interpreter_count; ++i) {
        if (0 == strcmp(interpreter_table[i].name, name)) {
            return &interpreter_table[i];
        }
    }
    return NULL;
}

void interpreter_declare_item(void);
void interpreter_declare_binary_integer(void);
void interpreter_declare_string(void);
void interpreter_declare_varint(void);
void interpreter_declare_base64(void);
void interpreter_declare_snappy(void);
void interpreter_declare_formatted_integer(void);

void
interpreter_declare_std(void)
{
    interpreter_declare_item();
    interpreter_declare_binary_integer();
    interpreter_declare_string();
    interpreter_declare_varint();
    interpreter_declare_base64();
    interpreter_declare_snappy();
    interpreter_declare_formatted_integer();
}

int
interpreter_rcall_build(struct ast_node_hdl *node,
                        const struct interpreter *interpreter,
                        struct statement_list *attr_list)
{
    struct ast_node_data *ndat;

    ndat = new_safe(struct ast_node_data);
    ndat->type = AST_NODE_TYPE_REXPR_INTERPRETER;
    ndat->flags = (0 == (ndat->u.rexpr.value_type_mask &
                         (EXPR_VALUE_TYPE_BYTES |
                          EXPR_VALUE_TYPE_STRING)) ?
                   ASTFLAG_IS_VALUE_TYPE : 0u);
    ndat->u.rexpr.value_type_mask = interpreter->value_type_mask;
    ndat->u.rexpr_interpreter.interpreter = interpreter;
    ndat->u.rexpr_interpreter.get_size_func = NULL;
    node->ndat = ndat;
    if (-1 == interpreter_build_attrs(
            node, interpreter, attr_list,
            &node->ndat->u.rexpr_interpreter.attr_set)) {
        free(ndat);
        node->ndat = NULL;
        return -1;
    }
    return 0;
}

static int
get_attr_index(const struct interpreter *interpreter,
               const char *attr_name)
{
    struct interpreter_attr_def *attr_def;

    STAILQ_FOREACH(attr_def, &interpreter->attr_list, list) {
        if (0 == strcmp(attr_def->name, attr_name)) {
            return attr_def->ref_idx;
        }
    }
    return -1; /* not found */
}

int
interpreter_build_attrs(struct ast_node_hdl *node,
                        const struct interpreter *interpreter,
                        struct statement_list *attr_list,
                        struct attribute_set *attr_setp)
{
    struct named_expr *attr;
    int attr_ref;
    struct interpreter_attr_def *attr_def;
    int sem_error = FALSE;

    memset(attr_setp, 0, sizeof (*attr_setp));
    attr_setp->n_attrs = interpreter->max_attr_ref + 1;
    attr_setp->attrs = new_n_safe(struct named_expr *, attr_setp->n_attrs);
    STATEMENT_FOREACH(named_expr, attr, attr_list, list) {
        attr_ref = get_attr_index(interpreter, attr->nstmt.name);
        if (-1 == attr_ref) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &attr->nstmt.stmt.loc,
                           "no such attribute \"%s\" for interpreter \"%s\"",
                           attr->nstmt.name, interpreter->name);
            sem_error = TRUE;
            continue ;
        }
        assert(attr_ref >= 0 && attr_ref <= interpreter->max_attr_ref);
        if (NULL != attr_setp->attrs[attr_ref]) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &attr->nstmt.stmt.loc,
                           "duplicate attribute \"%s\"", attr->nstmt.name);
            sem_error = TRUE;
            continue ;
        }
        attr_setp->attrs[attr_ref] = attr;
    }
    STAILQ_FOREACH(attr_def, &interpreter->attr_list, list) {
        if (NULL == attr_setp->attrs[attr_def->ref_idx]) {
            if (0 != (INTERPRETER_ATTR_FLAG_MANDATORY & attr_def->flags)) {
                semantic_error(SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                               "missing mandatory attribute \"%s\"",
                               attr_def->name);
                sem_error = TRUE;
                continue ;
            }
        }
    }
    if (sem_error) {
        return -1;
    }
    return 0;
}

bitpunch_status_t
interpreter_rcall_evaluate_attrs(struct ast_node_hdl *expr,
                                 struct box *scope,
                                 int **attr_is_specifiedp,
                                 expr_value_t **attr_valuep,
                                 struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    const struct interpreter *interpreter;
    int p_idx;
    struct named_expr *attr;
    expr_value_t *attr_value;
    int *attr_is_specified;

    interpreter = expr->ndat->u.rexpr_interpreter.interpreter;

    attr_value = new_n_safe(expr_value_t, interpreter->n_attrs);
    attr_is_specified = new_n_safe(int, interpreter->n_attrs);

    for (p_idx = 0; p_idx < interpreter->n_attrs; ++p_idx) {
        attr = expr->ndat->u.rexpr_interpreter.attr_set.attrs[p_idx];
        if (NULL == attr) {
            attr_value[p_idx].type = EXPR_VALUE_TYPE_UNSET;
            continue ;
        }
        attr_is_specified[p_idx] = TRUE;
        bt_ret = expr_evaluate_value_internal(attr->expr, scope,
                                              &attr_value[p_idx], bst);
        if (BITPUNCH_OK != bt_ret) {
            for (--p_idx; p_idx >= 0; --p_idx) {
                expr_value_destroy(attr_value[p_idx]);
            }
            free(attr_value);
            free(attr_is_specified);
            return bt_ret;
        }
    }
    *attr_valuep = attr_value;
    *attr_is_specifiedp = attr_is_specified;
    return BITPUNCH_OK;
}

void
interpreter_rcall_destroy_attr_values(struct ast_node_hdl *expr,
                                      int *attr_is_specified,
                                      expr_value_t *attr_value)
{
    const struct interpreter *interpreter;
    int p_idx;

    interpreter = expr->ndat->u.rexpr_interpreter.interpreter;
    if (NULL != attr_value) {
        for (p_idx = 0; p_idx < interpreter->n_attrs; ++p_idx) {
            expr_value_destroy(attr_value[p_idx]);
        }
        free(attr_value);
        free(attr_is_specified);
    }
}

bitpunch_status_t
interpreter_rcall_read_value(struct ast_node_hdl *expr,
                             struct box *scope,
                             const char *item_data,
                             int64_t item_size,
                             expr_value_t *valuep,
                             struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int *attr_is_specified = NULL;
    expr_value_t *attr_value = NULL;
    int64_t span_size;
    int64_t used_size;
    expr_value_t value;

    value.type = EXPR_VALUE_TYPE_UNSET;
    bt_ret = interpreter_rcall_evaluate_attrs(expr, scope,
                                              &attr_is_specified,
                                              &attr_value, bst);
    if (BITPUNCH_OK == bt_ret) {
        if (NULL == expr->ndat->u.rexpr_interpreter.get_size_func) {
            span_size = item_size;
        } else if (-1 == expr->ndat->u.rexpr_interpreter.get_size_func(
                       expr,
                       &span_size, &used_size, item_data, item_size,
                       attr_is_specified, attr_value)) {
            bt_ret = BITPUNCH_DATA_ERROR;
        }
    }
    if (BITPUNCH_OK == bt_ret) {
        memset(&value, 0, sizeof(value));
        if (-1 == expr->ndat->u.rexpr_interpreter.read_func(
                expr,
                &value, item_data, span_size,
                attr_is_specified, attr_value)) {
            bt_ret = BITPUNCH_DATA_ERROR;
        }
    }
    if (BITPUNCH_OK == bt_ret && NULL != valuep) {
        *valuep = value;
    } else {
        expr_value_destroy(value);
    }
    interpreter_rcall_destroy_attr_values(expr, attr_is_specified,
                                          attr_value);
    return bt_ret;
}
