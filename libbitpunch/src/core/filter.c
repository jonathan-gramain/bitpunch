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
#include "filters/composite.h"

#define MAX_FILTER_COUNT           256

struct filter_class filter_class_table[MAX_FILTER_COUNT];
int                 filter_class_count = 0;

#define MAX_FILTER_ATTR_DEF_COUNT 1024

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
filter_class_declare(
    const char *name,
    enum expr_value_type value_type_mask,
    filter_instance_build_func_t filter_instance_build_func,
    filter_instance_compile_func_t filter_instance_compile_func,
    int n_attrs,
    ... /* attrs: (name, type, flags) tuples */)
{
    struct filter_class *filter_cls;
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
    filter_cls->filter_instance_compile_func = filter_instance_compile_func;
    filter_cls->n_attrs = n_attrs;
    STAILQ_INIT(&filter_cls->attr_list);
    va_start(ap, n_attrs);
    for (i = 0; i < n_attrs; ++i) {
        attr_def = filter_attr_def_new();
        if (NULL == attr_def) {
            return -1;
        }
        attr_def->name = va_arg(ap, const char *);
        attr_def->value_type_mask = va_arg(ap, enum ast_node_type);
        attr_def->flags = va_arg(ap, enum filter_attr_flags);
        STAILQ_INSERT_TAIL(&filter_cls->attr_list, attr_def, list);
    }
    va_end(ap);
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

void filter_class_declare_byte(void);
void filter_class_declare_struct(void);
void filter_class_declare_union(void);
void filter_class_declare_array(void);
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
    filter_class_declare_byte();
    filter_class_declare_struct();
    filter_class_declare_union();
    filter_class_declare_array();
    filter_class_declare_binary_integer();
    filter_class_declare_bytes();
    filter_class_declare_string();
    filter_class_declare_varint();
    filter_class_declare_base64();
    filter_class_declare_snappy();
    filter_class_declare_formatted_integer();
}

struct filter_def *
filter_def_create_empty(const char *filter_type)
{
    struct filter_def *filter_def;

    filter_def = new_safe(struct filter_def);
    filter_def->filter_type = filter_type;
    init_block_stmt_list(&filter_def->scope_def.block_stmt_list);
    return filter_def;
}

int
filter_instance_build(struct ast_node_hdl *node,
                      const struct filter_class *filter_cls,
                      struct filter_def *filter_def)
{
    struct ast_node_data *ndat;
    struct filter_instance *f_instance;

    ndat = new_safe(struct ast_node_data);
    // template flag may be removed when compiling OP_FILTER
    ndat->flags |= ASTFLAG_DATA_TEMPLATE;
    ndat->u.item.min_span_size = SPAN_SIZE_UNDEF;
    if (0 == strcmp(filter_def->filter_type, "struct") ||
        0 == strcmp(filter_def->filter_type, "union")) {
        ndat->type = AST_NODE_TYPE_COMPOSITE;
    } else if (0 == strcmp(filter_def->filter_type, "array")) {
        ndat->type = AST_NODE_TYPE_ARRAY;
    } else if (0 == strcmp(filter_def->filter_type, "byte")) {
        ndat->type = AST_NODE_TYPE_BYTE;
    } else {
        ndat->type = AST_NODE_TYPE_REXPR_FILTER;
    }
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

    f_instance = filter_cls->filter_instance_build_func(node);
    if (NULL == f_instance) {
        return -1;
    }
    ndat->u.rexpr_filter.f_instance = f_instance;
    // FIXME merge filter item with node item
    //ndat->u.item = f_instance->item;
    return 0;
}

int
filter_instance_build_shared(struct ast_node_hdl *node,
                             const char *filter_name)
{
    const struct filter_class *filter_cls;

    filter_cls = filter_class_lookup(filter_name);
    assert(NULL != filter_cls);
    if (-1 == filter_instance_build(node, filter_cls,
                                    filter_def_create_empty(filter_name))) {
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

bitpunch_status_t
filter_instance_read_value(struct ast_node_hdl *filter,
                           struct box *scope,
                           int64_t item_offset,
                           int64_t item_size,
                           expr_value_t *valuep,
                           struct browse_state *bst)
{
    struct filter_instance *f_instance;
    bitpunch_status_t bt_ret;
    int64_t span_size;
    const char *item_data;
    expr_value_t value;

    assert(-1 != item_offset);

    value.type = EXPR_VALUE_TYPE_UNSET;
    f_instance = filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_item.compute_item_size) {
        span_size = item_size;
        bt_ret = BITPUNCH_OK;
    } else {
        bt_ret = f_instance->b_item.compute_item_size(
            filter, scope, item_offset, item_offset + item_size,
            &span_size, bst);
    }
    if (BITPUNCH_OK == bt_ret) {
        memset(&value, 0, sizeof(value));
        item_data = scope->ds_in->ds_data + item_offset;
        if (NULL != f_instance->read_func) {
            bt_ret = f_instance->read_func(filter, scope,
                                           &value, item_data, span_size, bst);
        } else {
            value.type = EXPR_VALUE_TYPE_BYTES;
            value.bytes.buf = item_data;
            value.bytes.len = span_size;
            value.bytes.from_box = scope;
            box_acquire(scope);
        }
    }
    if (BITPUNCH_OK == bt_ret && NULL != valuep) {
        *valuep = value;
    } else {
        expr_value_destroy(value);
    }
    return bt_ret;
}
