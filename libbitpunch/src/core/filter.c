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

#include "core/filter.h"
#include "core/browse_internal.h"
#include "core/expr_internal.h"

#define MAX_FILTER_COUNT           256

struct filter_class filter_class_table[MAX_FILTER_COUNT];
int                 filter_class_count = 0;

#define MAX_FILTER_ATTR_DEF_COUNT 1024
#define FILTER_MAX_ATTR_REF       256

struct filter_attr_def filter_attr_def_table[MAX_FILTER_ATTR_DEF_COUNT];
int                    filter_attr_def_count = 0;

static struct filter_class *
filter_class_new(void)
{
    if (filter_class_count == MAX_FILTER_COUNT) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "Too many filters (max %d)\n",
                         MAX_FILTER_COUNT);
        return NULL;
    }
    return &filter_class_table[filter_class_count++];
}

static struct filter_attr_def *
filter_attr_def_new(void)
{
    if (filter_attr_def_count == MAX_FILTER_ATTR_DEF_COUNT) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "Global filter attribute definition limit reached "
                       "(max %d)\n", MAX_FILTER_ATTR_DEF_COUNT);
        return NULL;
    }
    return &filter_attr_def_table[filter_attr_def_count++];
}

int
filter_class_declare(const char *name,
                    enum expr_value_type value_type_mask,
                    filter_instance_build_func_t filter_instance_build_func,
                    int n_attrs,
                    ... /* attrs: (name, index, type, flags) tuples */)
{
    struct filter_class *filter_cls;
    int max_attr_ref;
    va_list ap;
    int i;
    struct filter_attr_def *attr_def;

    assert(NULL != filter_instance_build_func);

    filter_cls = filter_class_lookup(name);
    if (NULL != filter_cls) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "duplicate filter '%s'\n", name);
        return -1;
    }
    filter_cls = filter_class_new();
    if (NULL == filter_cls) {
        return -1;
    }
    filter_cls->name = name;
    filter_cls->value_type_mask = value_type_mask;
    filter_cls->filter_instance_build_func = filter_instance_build_func;
    filter_cls->n_attrs = n_attrs;
    max_attr_ref = -1;
    STAILQ_INIT(&filter_cls->attr_list);
    va_start(ap, n_attrs);
    for (i = 0; i < n_attrs; ++i) {
        attr_def = filter_attr_def_new();
        if (NULL == attr_def) {
            return -1;
        }
        attr_def->name = va_arg(ap, const char *);
        attr_def->ref_idx = va_arg(ap, int);
        assert(attr_def->ref_idx >= 0 &&
               attr_def->ref_idx <= FILTER_MAX_ATTR_REF);
        attr_def->value_type_mask = va_arg(ap, enum ast_node_type);
        attr_def->flags = va_arg(ap, enum filter_attr_flags);
        STAILQ_INSERT_TAIL(&filter_cls->attr_list, attr_def, list);
        max_attr_ref = MAX(max_attr_ref, attr_def->ref_idx);
    }
    va_end(ap);
    filter_cls->max_attr_ref = max_attr_ref;
    return 0;
}

struct filter_class *
filter_class_lookup(const char *name)
{
    int i;

    for (i = 0; i < filter_class_count; ++i) {
        if (0 == strcmp(filter_class_table[i].name, name)) {
            return &filter_class_table[i];
        }
    }
    return NULL;
}

void filter_class_declare_item(void);
void filter_class_declare_binary_integer(void);
void filter_class_declare_bytes(void);
void filter_class_declare_string(void);
void filter_class_declare_varint(void);
void filter_class_declare_base64(void);
void filter_class_declare_snappy(void);
void filter_class_declare_formatted_integer(void);

void
filter_class_declare_std(void)
{
    filter_class_declare_item();
    filter_class_declare_binary_integer();
    filter_class_declare_bytes();
    filter_class_declare_string();
    filter_class_declare_varint();
    filter_class_declare_base64();
    filter_class_declare_snappy();
    filter_class_declare_formatted_integer();
}

int
filter_instance_build(struct ast_node_hdl *node,
                      const struct filter_class *filter_cls,
                      struct filter_def *filter_def)
{
    struct ast_node_data *ndat;

    ndat = new_safe(struct ast_node_data);
    ndat->type = AST_NODE_TYPE_REXPR_FILTER;
    ndat->u.rexpr.value_type_mask = filter_cls->value_type_mask;
    ndat->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_UNSET;
    if (0 != (filter_cls->value_type_mask & (EXPR_VALUE_TYPE_INTEGER |
                                             EXPR_VALUE_TYPE_BOOLEAN))) {
        ndat->u.rexpr.dpath_type_mask |= EXPR_DPATH_TYPE_NONE;
    }
    if (0 != (filter_cls->value_type_mask & (EXPR_VALUE_TYPE_BYTES |
                                             EXPR_VALUE_TYPE_STRING))) {
        ndat->u.rexpr.dpath_type_mask |= EXPR_DPATH_TYPE_CONTAINER;
    }
    ndat->u.rexpr_filter.filter_cls = filter_cls;
    ndat->u.rexpr_filter.filter_def = filter_def;
    node->ndat = ndat;
    if (-1 == filter_build_attrs(
            node, filter_cls,
            filter_def->block_stmt_list.attribute_list)) {
        free(ndat);
        node->ndat = NULL;
        return -1;
    }
    return 0;
}

const struct filter_attr_def *
filter_class_get_attr(const struct filter_class *filter_cls,
                         const char *attr_name)
{
    struct filter_attr_def *attr_def;

    STAILQ_FOREACH(attr_def, &filter_cls->attr_list, list) {
        if (0 == strcmp(attr_def->name, attr_name)) {
            return attr_def;
        }
    }
    return NULL; /* not found */
}

int
filter_build_attrs(struct ast_node_hdl *node,
                        const struct filter_class *filter_cls,
                        struct statement_list *attribute_list)
{
    struct named_expr *attr;
    int attr_ref;
    const struct filter_attr_def *attr_def;
    int *attr_is_defined;
    int sem_error = FALSE;

    attr_is_defined = new_n_safe(int, filter_cls->max_attr_ref + 1);
    STATEMENT_FOREACH(named_expr, attr, attribute_list, list) {
        attr_def = filter_class_get_attr(filter_cls, attr->nstmt.name);
        if (NULL == attr_def) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &attr->nstmt.stmt.loc,
                           "no such attribute \"%s\" for filter \"%s\"",
                           attr->nstmt.name, filter_cls->name);
            sem_error = TRUE;
            continue ;
        }
        attr_ref = attr_def->ref_idx;
        assert(attr_ref >= 0 && attr_ref <= filter_cls->max_attr_ref);
        attr_is_defined[attr_ref] = TRUE;
    }
    STAILQ_FOREACH(attr_def, &filter_cls->attr_list, list) {
        if (!attr_is_defined[attr_def->ref_idx]
            && 0 != (FILTER_ATTR_FLAG_MANDATORY & attr_def->flags)) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                           "missing mandatory attribute \"%s\"",
                           attr_def->name);
            sem_error = TRUE;
        }
    }
    free(attr_is_defined);
    if (sem_error) {
        return -1;
    }
    return 0;
}



bitpunch_status_t
filter_instance_evaluate_attrs(struct ast_node_hdl *expr,
                                 struct box *scope,
                                 int **attr_is_specifiedp,
                                 expr_value_t **attr_valuep,
                                 struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    const struct filter_class *filter_cls;
    const struct filter_attr_def *attr_def;
    int attr_idx;
    struct named_expr *attr;
    expr_value_t *attr_value;
    int *attr_is_specified;
    int cond_eval;

    filter_cls = expr->ndat->u.rexpr_filter.filter_cls;

    attr_value = new_n_safe(expr_value_t, filter_cls->n_attrs);
    attr_is_specified = new_n_safe(int, filter_cls->n_attrs);

    STATEMENT_FOREACH(
        named_expr, attr,
        expr->ndat->u.rexpr_filter.filter_def->block_stmt_list.attribute_list,
        list) {
        // TODO optimize this lookup (e.g. store the index in the
        // attribute list item)
        attr_def = filter_class_get_attr(filter_cls, attr->nstmt.name);
        assert(NULL != attr_def);
        attr_idx = attr_def->ref_idx;
        if (attr_is_specified[attr_idx]) {
            continue ;
        }
        bt_ret = evaluate_conditional_internal(attr->nstmt.stmt.cond, scope,
                                               &cond_eval, bst);
        if (BITPUNCH_OK == bt_ret) {
            if (cond_eval) {
                attr_is_specified[attr_idx] = TRUE;
                bt_ret = expr_evaluate_value_internal(
                    attr->expr, scope, &attr_value[attr_idx], bst);
            }
        }
        if (BITPUNCH_OK != bt_ret) {
            for (attr_idx = 0; attr_idx < 0; --attr_idx) {
                expr_value_destroy(attr_value[attr_idx]);
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
filter_instance_destroy_attr_values(struct ast_node_hdl *expr,
                                    int *attr_is_specified,
                                    expr_value_t *attr_value)
{
    const struct filter_class *filter_cls;
    int attr_idx;

    filter_cls = expr->ndat->u.rexpr_filter.filter_cls;
    if (NULL != attr_value) {
        for (attr_idx = 0; attr_idx < filter_cls->n_attrs; ++attr_idx) {
            expr_value_destroy(attr_value[attr_idx]);
        }
        free(attr_value);
        free(attr_is_specified);
    }
}

bitpunch_status_t
filter_instance_read_value(struct ast_node_hdl *expr,
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
    bt_ret = filter_instance_evaluate_attrs(expr, scope,
                                            &attr_is_specified,
                                            &attr_value, bst);
    if (BITPUNCH_OK == bt_ret) {
        if (NULL == expr->ndat->u.rexpr_filter.get_size_func) {
            span_size = item_size;
        } else {
            bt_ret = expr->ndat->u.rexpr_filter.get_size_func(
                expr, scope,
                &span_size, &used_size, item_data, item_size,
                attr_is_specified, attr_value, bst);
        }
    }
    if (BITPUNCH_OK == bt_ret) {
        memset(&value, 0, sizeof(value));
        bt_ret = expr->ndat->u.rexpr_filter.read_func(
            expr, scope,
            &value, item_data, span_size,
            attr_is_specified, attr_value, bst);
    }
    if (BITPUNCH_OK == bt_ret && NULL != valuep) {
        *valuep = value;
    } else {
        expr_value_destroy(value);
    }
    filter_instance_destroy_attr_values(expr, attr_is_specified,
                                          attr_value);
    return bt_ret;
}
