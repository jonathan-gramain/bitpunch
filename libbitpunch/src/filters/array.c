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

#include <assert.h>

#include "filters/array.h"

static struct filter_instance *
array_filter_instance_build(struct ast_node_hdl *filter)
{
    struct named_expr *attr;
    struct ast_node_hdl *item_type;
    struct ast_node_hdl *item_count;
    struct filter_instance_array *array;

    item_type = NULL;
    item_count = NULL;
    // FIXME this does not support conditional attributes
    STATEMENT_FOREACH(
        named_expr, attr,
        filter->ndat->u.rexpr_filter.filter_def->block_stmt_list.attribute_list,
        list) {
        if (0 == strcmp(attr->nstmt.name, "@item")) {
            item_type = attr->expr;
        } else if (0 == strcmp(attr->nstmt.name, "@length")) {
            item_count = attr->expr;
        }
    }
    array = new_safe(struct filter_instance_array);
    dpath_node_reset(&array->item_type);
    array->item_type.item = item_type;
    array->item_count = item_count;
    return (struct filter_instance *)array;
}

static int
compile_type_array(struct ast_node_hdl *filter,
                   struct filter_instance_array *array,
                   struct compile_ctx *ctx)
{
    if (-1 == compile_dpath(&array->item_type, ctx,
                            COMPILE_TAG_NODE_TYPE, 0u)) {
        return -1;
    }
    assert(NULL != array->item_type.filter);
    // if an array of unfiltered bytes, it's a byte array
    if (AST_NODE_TYPE_BYTE == array->item_type.item->ndat->type
        && AST_NODE_TYPE_REXPR_ITEM == array->item_type.filter->ndat->type) {
        filter->ndat->type = AST_NODE_TYPE_BYTE_ARRAY;
    }
    return 0;
}

static int
compile_span_size_array(struct ast_node_hdl *item,
                        struct filter_instance_array *array,
                        struct compile_ctx *ctx)
{
    struct dpath_node *item_dpath;
    struct ast_node_hdl *item_type;
    struct ast_node_hdl *item_count_expr;
    int64_t item_count;
    int64_t min_span_size;
    int dynamic_span;

    item_count_expr = ast_node_get_named_expr_target(array->item_count);
    if (NULL != item_count_expr) {
        if (-1 == compile_expr(item_count_expr, ctx, TRUE)) {
            return -1;
        }
        // XXX check all polymorphic named expr targets
        if (0 == (EXPR_VALUE_TYPE_INTEGER
                  & item_count_expr->ndat->u.rexpr.value_type_mask)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &item_count_expr->loc,
                "invalid array size type: expect an integer, got '%s'",
                expr_value_type_str(
                    item_count_expr->ndat->u.rexpr.value_type_mask));
            return -1;
        }
    }
    item_dpath = &array->item_type;
    item_type = ast_node_get_target_item(item_dpath->item);
    assert(ast_node_is_item(item_type));
    if (NULL != item_count_expr
        && item_count_expr->ndat->type == AST_NODE_TYPE_REXPR_NATIVE
        && item_count_expr->ndat->u.rexpr_native.value.integer > 0) {
        // compile item type as a dependency because the array
        // contains a fixed number of at least one item
        if (-1 == compile_dpath(item_dpath, ctx,
                                COMPILE_TAG_NODE_TYPE |
                                COMPILE_TAG_NODE_SPAN_SIZE, 0u)) {
            return -1;
        }
        assert(SPAN_SIZE_UNDEF != item_type->ndat->u.item.min_span_size);
        assert(EXPR_VALUE_TYPE_INTEGER
               == item_count_expr->ndat->u.rexpr.value_type_mask);
        assert(SPAN_SIZE_UNDEF != item_type->ndat->u.item.min_span_size);
        item_count = item_count_expr->ndat->u.rexpr_native.value.integer;
        min_span_size = item_count * item_type->ndat->u.item.min_span_size;
        dynamic_span =
            (0 != (item_dpath->item->ndat->u.item.flags & ASTFLAG_CONTAINS_LAST_ATTR)
             || 0 != (item_dpath->item->ndat->u.item.flags
                      & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC));
    } else {
        // schedule compilation of item type and size without
        // depending on it, so to allow recursive nesting of items and
        // possibly empty arrays
        if (-1 == compile_dpath(item_dpath, ctx,
                                0u, (COMPILE_TAG_NODE_TYPE |
                                     COMPILE_TAG_NODE_SPAN_SIZE))) {
            return -1;
        }
        min_span_size = 0;
        dynamic_span = TRUE;
        item->ndat->u.item.min_span_size = 0;
        item->ndat->u.item.flags |= (ITEMFLAG_IS_SPAN_SIZE_DYNAMIC |
                                      ITEMFLAG_IS_USED_SIZE_DYNAMIC);
    }
    item->ndat->u.item.min_span_size = min_span_size;
    if (dynamic_span) {
        item->ndat->u.item.flags |= (ITEMFLAG_IS_SPAN_SIZE_DYNAMIC |
                                      ITEMFLAG_IS_USED_SIZE_DYNAMIC);
    }
    if (0 == (item_type->flags & ASTFLAG_CONTAINS_LAST_ATTR)) {
        if (NULL == item_count_expr) {
            // because the array items can take more than one byte of
            // space, they may not allow to fill the whole slack
            // space, so we don't set ITEMFLAG_FILLS_SLACK (meaning
            // used size may be smaller than max span size)
            item->ndat->u.item.flags |= (ITEMFLAG_USES_SLACK |
                                          ITEMFLAG_SPREADS_SLACK);
        }
    }
    return 0;
}

static int
compile_span_size_byte_array(struct ast_node_hdl *item,
                             struct filter_instance_array *array,
                             struct compile_ctx *ctx)
{
    struct ast_node_hdl *size_expr;
    int64_t min_span_size;

    size_expr = array->item_count;
    if (NULL != size_expr) {
        if (-1 == compile_expr(size_expr, ctx, TRUE)) {
            return -1;
        }
        assert(ast_node_is_rexpr(size_expr));
        if (0 == (EXPR_VALUE_TYPE_INTEGER
                  & size_expr->ndat->u.rexpr.value_type_mask)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &size_expr->loc,
                "invalid byte array size type: expect an integer, "
                "got '%s'",
                expr_value_type_str(size_expr->ndat->u.rexpr.value_type_mask));
            return -1;
        }
    }
    if (NULL != size_expr
        && AST_NODE_TYPE_REXPR_NATIVE == size_expr->ndat->type) {
        min_span_size = size_expr->ndat->u.rexpr_native.value.integer;
    } else {
        min_span_size = 0;
        item->ndat->u.item.flags |= (ITEMFLAG_IS_SPAN_SIZE_DYNAMIC |
                                     ITEMFLAG_IS_USED_SIZE_DYNAMIC);
    }
    if (NULL == size_expr) {
        // for now we don't know if there's a filter defining span
        // size on top of the byte array which limits the amount of
        // slack used: assume for now it will use the entire space;
        // compilation of a size-defining filter may later adjust the
        // slack flags
        item->ndat->u.item.flags |= (ITEMFLAG_USES_SLACK |
                                     ITEMFLAG_SPREADS_SLACK |
                                     ITEMFLAG_FILLS_SLACK);
    }
    item->ndat->u.item.min_span_size = min_span_size;
    return 0;
}

static int
array_filter_instance_compile(struct ast_node_hdl *filter,
                              struct filter_instance *f_instance,
                              dep_resolver_tagset_t tags,
                              struct compile_ctx *ctx)
{
    struct filter_instance_array *array;

    array = (struct filter_instance_array *)f_instance;
    if (0 != (tags & COMPILE_TAG_NODE_TYPE)
        && -1 == compile_type_array(filter, array, ctx)) {
        return -1;
    }
    if (0 != (tags & COMPILE_TAG_NODE_SPAN_SIZE)) {
        switch (filter->ndat->type) {
        case AST_NODE_TYPE_ARRAY:
            if (-1 == compile_span_size_array(filter, array, ctx)) {
                return -1;
            }
            break ;
        case AST_NODE_TYPE_BYTE_ARRAY:
            if (-1 == compile_span_size_byte_array(filter, array, ctx)) {
                return -1;
            }
            break ;
        default:
            assert(0);
        }
    }
    return 0;
}

void
filter_class_declare_array(void)
{
    int ret;

    ret = filter_class_declare("array",
                               EXPR_VALUE_TYPE_UNSET,
                               array_filter_instance_build,
                               array_filter_instance_compile,
                               2,
                               "@item", EXPR_VALUE_TYPE_UNSET, 0,
                               "@length", EXPR_VALUE_TYPE_INTEGER, 0);
    assert(0 == ret);
}
