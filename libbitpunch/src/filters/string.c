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

struct state_single_char_constant_boundary {
    struct filter_instance f_instance; /* inherits */
    char boundary;
};

static bitpunch_status_t
string_get_size_byte_array_single_char_boundary(
    struct ast_node_hdl *filter,
    struct box *scope,
    int64_t *span_sizep,
    int64_t *used_sizep,
    const char *data, int64_t max_span_size,
    struct browse_state *bst)
{
    struct state_single_char_constant_boundary *state;
    const char *end;

    state = (struct state_single_char_constant_boundary *)
        filter->ndat->u.rexpr_filter.f_instance;
    end = memchr(data, state->boundary, max_span_size);
    if (NULL != end) {
        *span_sizep = end - data + 1;
        *used_sizep = end - data;
    } else {
        *span_sizep = max_span_size;
        *used_sizep = max_span_size;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
string_get_size_byte_array_multi_char_boundary(
    struct ast_node_hdl *filter,
    struct box *scope,
    int64_t *span_sizep,
    int64_t *used_sizep,
    const char *data, int64_t max_span_size,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    expr_value_t attr_value;
    const char *end;

    bt_ret = filter_evaluate_attribute_internal(
        filter, scope, "@boundary", NULL, &attr_value, NULL, bst);
    if (BITPUNCH_OK == bt_ret) {
        end = memmem(data, max_span_size,
                     attr_value.string.str, attr_value.string.len);
        if (NULL != end) {
            *span_sizep = end - data + attr_value.string.len;
            *used_sizep = end - data;
            expr_value_destroy(attr_value);
            return BITPUNCH_OK;
        }
        expr_value_destroy(attr_value);
    } else if (BITPUNCH_NO_ITEM != bt_ret) {
        return bt_ret;
    }
    *span_sizep = max_span_size;
    *used_sizep = max_span_size;
    return BITPUNCH_OK;
}


static bitpunch_status_t
string_read_byte_array_no_boundary(
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

static bitpunch_status_t
string_read_byte_array_single_char_boundary(
    struct ast_node_hdl *filter,
    struct box *scope,
    expr_value_t *read_value,
    const char *data, size_t span_size,
    struct browse_state *bst)
{
    struct state_single_char_constant_boundary *state;

    state = (struct state_single_char_constant_boundary *)
        filter->ndat->u.rexpr_filter.f_instance;
    read_value->type = EXPR_VALUE_TYPE_STRING;
    read_value->string.str = (char *)data;
    if (span_size >= 1
        && (data[span_size - 1] == state->boundary)) {
        read_value->string.len = span_size - 1;
    } else {
        read_value->string.len = span_size;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
string_read_byte_array_multi_char_boundary(
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
string_filter_instance_build(
    struct ast_node_hdl *filter,
    struct compile_ctx *ctx)
{
    const struct block_stmt_list *stmt_lists;
    struct named_expr *attr;
    struct expr_value_string boundary;

    stmt_lists = &filter->ndat->u.rexpr_filter.filter_def->block_stmt_list;
    // default read function, may be overriden next
    filter->ndat->u.rexpr_filter.read_func = string_read_byte_array_no_boundary;

    STATEMENT_FOREACH(named_expr, attr, stmt_lists->attribute_list, list) {
        if (0 == strcmp(attr->nstmt.name, "@boundary")) {
            if (AST_NODE_TYPE_REXPR_NATIVE == attr->expr->ndat->type
                && NULL == attr->nstmt.stmt.cond) {
                boundary = attr->expr->ndat->u.rexpr_native.value.string;
                switch (boundary.len) {
                case 0:
                    /* keep default byte array size */
                    break ;
                case 1: {
                    struct state_single_char_constant_boundary *state;

                    state = new_safe(
                        struct state_single_char_constant_boundary);
                    state->boundary = boundary.str[0];
                    filter->ndat->u.rexpr_filter.get_size_func =
                        string_get_size_byte_array_single_char_boundary;
                    filter->ndat->u.rexpr_filter.read_func =
                        string_read_byte_array_single_char_boundary;
                    return (struct filter_instance *)state;
                }
                default: /* two or more characters boundary */
                    filter->ndat->u.rexpr_filter.get_size_func =
                        string_get_size_byte_array_multi_char_boundary;
                    filter->ndat->u.rexpr_filter.read_func =
                        string_read_byte_array_multi_char_boundary;
                }
            } else {
                /* dynamic boundary: use generic multi-char
                 * implementation */
                filter->ndat->u.rexpr_filter.get_size_func =
                    string_get_size_byte_array_multi_char_boundary;
                filter->ndat->u.rexpr_filter.read_func =
                    string_read_byte_array_multi_char_boundary;
            }
            break ;
        }
    }
    return new_safe(struct filter_instance);
}

void
filter_class_declare_string(void)
{
    int ret;

    //TODO add regex boundary support
    ret = filter_class_declare("string",
                              EXPR_VALUE_TYPE_STRING,
                              string_filter_instance_build,
                              1,
                              "@boundary", EXPR_VALUE_TYPE_STRING, 0);
    assert(0 == ret);
}
