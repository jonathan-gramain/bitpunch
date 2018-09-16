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

#define _DEFAULT_SOURCE
#define _GNU_SOURCE
#include <string.h>
#include <assert.h>

#include "core/filter.h"

static bitpunch_status_t
string_box_init(struct box *box)
{
    box->u.array_generic.n_items = -1;
    return BITPUNCH_OK;
}

static bitpunch_status_t
string_read_no_boundary(
    struct ast_node_hdl *filter,
    struct box *scope,
    expr_value_t *read_value,
    const char *data, size_t span_size,
    struct browse_state *bst)
{
    read_value->type = EXPR_VALUE_TYPE_STRING;
    read_value->string.str = (char *)data;
    read_value->string.len = span_size;
    return BITPUNCH_OK;
}


struct string_single_char_constant_boundary {
    struct filter_instance p; /* inherits */
    char boundary;
};

static struct filter_instance *
string_build_no_boundary(void)
{
    struct filter_instance *f_instance;

    f_instance = new_safe(struct filter_instance);
    f_instance->b_box.init = string_box_init;
    f_instance->read_func = string_read_no_boundary;
    return f_instance;
}


static bitpunch_status_t
compute_item_size__string__single_char_constant_boundary(
    struct ast_node_hdl *filter,
    struct box *scope,
    int64_t item_offset,
    int64_t max_span_offset,
    int64_t *item_sizep,
    struct browse_state *bst)
{
    struct string_single_char_constant_boundary *f_instance;
    const char *data;
    const char *end;

    f_instance = (struct string_single_char_constant_boundary *)
        filter->ndat->u.rexpr_filter.f_instance;
    data = scope->ds_in->ds_data + item_offset;
    end = memchr(data, f_instance->boundary, max_span_offset - item_offset);
    if (NULL != end) {
        *item_sizep = end - data + 1;
    } else {
        *item_sizep = max_span_offset - item_offset;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
string_read_single_char_constant_boundary(
    struct ast_node_hdl *filter,
    struct box *scope,
    expr_value_t *read_value,
    const char *data, size_t span_size,
    struct browse_state *bst)
{
    struct string_single_char_constant_boundary *f_instance;

    f_instance = (struct string_single_char_constant_boundary *)
        filter->ndat->u.rexpr_filter.f_instance;
    read_value->type = EXPR_VALUE_TYPE_STRING;
    read_value->string.str = (char *)data;
    if (span_size >= 1
        && (data[span_size - 1] == f_instance->boundary)) {
        read_value->string.len = span_size - 1;
    } else {
        read_value->string.len = span_size;
    }
    return BITPUNCH_OK;
}

static struct filter_instance *
string_build_single_char_constant_boundary(char boundary)
{
    struct string_single_char_constant_boundary *f_instance;
                    
    f_instance = new_safe(struct string_single_char_constant_boundary);
    f_instance->boundary = boundary;
    f_instance->p.b_item.compute_item_size =
        compute_item_size__string__single_char_constant_boundary;
    f_instance->p.b_box.init = string_box_init;
    f_instance->p.read_func = string_read_single_char_constant_boundary;
    return (struct filter_instance *)f_instance;
}


static bitpunch_status_t
compute_item_size__string__generic(struct ast_node_hdl *filter,
                                   struct box *scope,
                                   int64_t item_offset,
                                   int64_t max_span_offset,
                                   int64_t *item_sizep,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    expr_value_t attr_value;
    const char *data;
    const char *end;

    bt_ret = filter_evaluate_attribute_internal(
        filter, scope, "@boundary", NULL, &attr_value, NULL, bst);
    if (BITPUNCH_OK == bt_ret) {
        data = scope->ds_in->ds_data + item_offset;
        end = memmem(data, max_span_offset - item_offset,
                     attr_value.string.str, attr_value.string.len);
        if (NULL != end) {
            *item_sizep = end - data + attr_value.string.len;
            expr_value_destroy(attr_value);
            return BITPUNCH_OK;
        }
        expr_value_destroy(attr_value);
    } else if (BITPUNCH_NO_ITEM != bt_ret) {
        return bt_ret;
    }
    *item_sizep = max_span_offset - item_offset;
    return BITPUNCH_OK;
}

static bitpunch_status_t
string_read_generic(
    struct ast_node_hdl *filter,
    struct box *scope,
    expr_value_t *read_value,
    const char *data, size_t span_size,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    expr_value_t attr_value;
    struct expr_value_string boundary;

    bt_ret = filter_evaluate_attribute_internal(
        filter, scope, "@boundary", NULL, &attr_value, NULL, bst);
    if (BITPUNCH_OK == bt_ret) {
        boundary = attr_value.string;
        read_value->type = EXPR_VALUE_TYPE_STRING;
        read_value->string.str = (char *)data;
        if (span_size >= boundary.len
            && 0 == memcmp(data + span_size - boundary.len,
                           boundary.str, boundary.len)) {
            read_value->string.len = span_size - boundary.len;
            expr_value_destroy(attr_value);
            return BITPUNCH_OK;
        }
        expr_value_destroy(attr_value);
    } else if (BITPUNCH_NO_ITEM != bt_ret) {
        return bt_ret;
    }
    read_value->string.len = span_size;
    return BITPUNCH_OK;
}

static struct filter_instance *
string_build_generic(void)
{
    struct filter_instance *f_instance;
                    
    f_instance = new_safe(struct filter_instance);
    f_instance->b_item.compute_item_size = compute_item_size__string__generic;
    f_instance->b_box.init = string_box_init;
    f_instance->read_func = string_read_generic;
    return f_instance;
}


static struct filter_instance *
string_filter_instance_build(struct ast_node_hdl *filter)
{
    const struct block_stmt_list *stmt_lists;
    struct named_expr *attr;
    struct expr_value_string boundary;

    stmt_lists = &filter->ndat->u.rexpr_filter.filter_def->block_stmt_list;
    STATEMENT_FOREACH(named_expr, attr, stmt_lists->attribute_list, list) {
        if (0 == strcmp(attr->nstmt.name, "@boundary")) {
            filter->ndat->u.item.flags &= ~ITEMFLAG_FILLS_SLACK;
            if (AST_NODE_TYPE_REXPR_NATIVE == attr->expr->ndat->type
                && NULL == attr->nstmt.stmt.cond) {
                boundary = attr->expr->ndat->u.rexpr_native.value.string;
                switch (boundary.len) {
                case 0:
                    return string_build_no_boundary();
                case 1:
                    return string_build_single_char_constant_boundary(
                        boundary.str[0]);
                default:
                    /* two or more characters boundary: use generic
                     * implementation */
                    return string_build_generic();
                }
            } else {
                /* dynamic boundary: use generic implementation */
                return string_build_generic();
            }
            break ;
        }
    }
    return string_build_no_boundary();
}

void
filter_class_declare_string(void)
{
    int ret;

    //TODO add regex boundary support
    ret = filter_class_declare("string",
                               EXPR_VALUE_TYPE_STRING,
                               string_filter_instance_build, NULL,
                               1,
                               "@boundary", EXPR_VALUE_TYPE_STRING, 0);
    assert(0 == ret);
}
