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

#include "filters/composite.h"

static struct filter_instance *
composite_filter_instance_build(struct ast_node_hdl *filter,
                                enum composite_type type)
{
    struct filter_instance_composite *composite;

    composite = new_safe(struct filter_instance_composite);
    composite->type = type;
    return (struct filter_instance *)composite;
}

static struct filter_instance *
struct_filter_instance_build(struct ast_node_hdl *filter)
{
    return composite_filter_instance_build(filter, COMPOSITE_TYPE_STRUCT);
}

static struct filter_instance *
union_filter_instance_build(struct ast_node_hdl *filter)
{
    return composite_filter_instance_build(filter, COMPOSITE_TYPE_UNION);
}

static int
compile_span_size_composite(struct ast_node_hdl *item,
                            struct filter_instance_composite *composite,
                            struct compile_ctx *ctx)
{
    struct named_expr *attr;
    struct ast_node_hdl *min_span_expr;
    struct ast_node_hdl *max_span_expr;
    const struct statement_list *field_list;
    struct field *field;
    struct ast_node_hdl_array field_items;
    struct ast_node_hdl *field_item;
    int64_t min_span_size;
    int dynamic_span;
    int dynamic_used;
    int contains_last_attr;
    int child_uses_slack;
    int child_spreads_slack;
    int child_conditionally_spreads_slack;
    int child_fills_slack;
    int child_conditionally_fills_slack;
    struct field *first_trailer_field;
    bitpunch_status_t bt_ret;

    /* - Compute the minimum span size from the sum (struct) or
       max (union) of fields' minimum span sizes. If size is
       static, minimum size is actual size.

       - If an unconditional span size is given by an expression,
       resolve it

       - Set the dynamic flag if actual size may be greater than
       the minimum
    */

    min_span_size = 0;
    dynamic_span = FALSE;
    dynamic_used = FALSE;
    contains_last_attr = FALSE;
    child_uses_slack = FALSE;
    child_spreads_slack = FALSE;
    child_conditionally_spreads_slack = FALSE;
    child_fills_slack = FALSE;
    child_conditionally_fills_slack = FALSE;

    min_span_expr = NULL;
    max_span_expr = NULL;
    STATEMENT_FOREACH(
        named_expr, attr,
        item->ndat->u.rexpr_filter.filter_def->block_stmt_list.attribute_list,
        list) {
        if (-1 == compile_expr(attr->expr, ctx, TRUE)) {
            return -1;
        }
        if (0 == strcmp(attr->nstmt.name, "@minspan")) {
            if (NULL == attr->nstmt.stmt.cond) {
                min_span_expr = attr->expr;
            }
            dynamic_span = TRUE;
        } else if (0 == strcmp(attr->nstmt.name, "@maxspan")) {
            if (NULL == attr->nstmt.stmt.cond) {
                max_span_expr = attr->expr;
            }
            dynamic_span = TRUE;
        } else if (0 == strcmp(attr->nstmt.name, "@span")) {
            if (NULL == attr->nstmt.stmt.cond) {
                min_span_expr = attr->expr;
                max_span_expr = attr->expr;
            } else {
                dynamic_span = TRUE;
            }
        } else if (0 == strcmp(attr->nstmt.name, "@last")) {
            contains_last_attr = TRUE;
        }
    }
    field_list = item->ndat->u.rexpr_filter.filter_def->block_stmt_list.field_list;
    first_trailer_field = NULL;
    STATEMENT_FOREACH(field, field, field_list, list) {
        compile_field(field, ctx,
                      COMPILE_TAG_NODE_TYPE |
                      COMPILE_TAG_NODE_SPAN_SIZE, 0u);
    }
    if (!compile_continue(ctx)) {
        return -1;
    }
    STATEMENT_FOREACH(field, field, field_list, list) {
        bt_ret = ast_node_filter_get_items(field->filter, &field_items);
        if (BITPUNCH_OK != bt_ret) {
            return -1;
        }
        assert(ARRAY_SIZE(&field_items) >= 1);
        field_item = ARRAY_ITEM(&field_items, 0);
        if (ARRAY_SIZE(&field_items) > 1) {
            dynamic_used = TRUE;
        } else {
            if (0 != (field_item->ndat->u.item.flags
                      & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
                dynamic_used = TRUE;
            } else if (NULL == field->nstmt.stmt.cond) {
                /* only update min span size if field is not conditional */
                assert(SPAN_SIZE_UNDEF != field_item->ndat->u.item.min_span_size);
                if (COMPOSITE_TYPE_UNION == composite->type) {
                    min_span_size = MAX(min_span_size,
                                        field_item->ndat->u.item.min_span_size);
                } else /* struct */ {
                    min_span_size += field_item->ndat->u.item.min_span_size;
                }
            } else {
                // if at least one conditional field is present, the
                // used size is dynamic
                dynamic_used = TRUE;
            }
            if (0 != (field_item->ndat->u.item.flags & ITEMFLAG_USES_SLACK)) {
                child_uses_slack = TRUE;
            }
            if (0 != (field_item->ndat->u.item.flags & ITEMFLAG_SPREADS_SLACK)) {
                if (NULL != field->nstmt.stmt.cond) {
                    child_conditionally_spreads_slack = TRUE;
                } else {
                    child_spreads_slack = TRUE;
                }
            }
            if (0 != (field_item->ndat->u.item.flags & ITEMFLAG_FILLS_SLACK)) {
                if (NULL != field->nstmt.stmt.cond) {
                    child_conditionally_fills_slack = TRUE;
                } else {
                    child_fills_slack = TRUE;
                }
            }
            if (0 != (field_item->ndat->u.item.flags
                      & ITEMFLAG_CONDITIONALLY_SPREADS_SLACK)) {
                child_conditionally_spreads_slack = TRUE;
            }
            if (0 != (field_item->ndat->u.item.flags
                      & ITEMFLAG_CONDITIONALLY_FILLS_SLACK)) {
                child_conditionally_fills_slack = TRUE;
            }
            if (0 != (field_item->ndat->u.item.flags
                      & (ITEMFLAG_SPREADS_SLACK |
                         ITEMFLAG_CONDITIONALLY_SPREADS_SLACK))
                || NULL != field->nstmt.stmt.cond) {
                first_trailer_field = NULL;
            } else if (COMPOSITE_TYPE_STRUCT == composite->type
                       && (child_spreads_slack ||
                           child_conditionally_spreads_slack)
                       && NULL == first_trailer_field
                       && 0 == (field_item->ndat->u.item.flags
                                & (ITEMFLAG_SPREADS_SLACK |
                                   ITEMFLAG_CONDITIONALLY_SPREADS_SLACK))) {
                first_trailer_field = field;
            }
        }
        ast_node_hdl_array_destroy(&field_items);
    }
    if (child_spreads_slack) {
        STATEMENT_FOREACH(field, field, field_list, list) {
            bt_ret = ast_node_filter_get_items(field->filter, &field_items);
            if (BITPUNCH_OK != bt_ret) {
                return -1;
            }
            field_item = ARRAY_ITEM(&field_items, 0);
            ast_node_hdl_array_destroy(&field_items);
            if (0 == (field_item->ndat->u.item.flags
                      & (ITEMFLAG_SPREADS_SLACK |
                         ITEMFLAG_CONDITIONALLY_SPREADS_SLACK))) {
                field->nstmt.stmt.stmt_flags |= FIELD_FLAG_HEADER;
            } else {
                break ;
            }
        }
    }
    for (field = first_trailer_field;
         NULL != field;
         field = (struct field *)
             TAILQ_NEXT((struct statement *)field, list)) {
        field->nstmt.stmt.stmt_flags |= FIELD_FLAG_TRAILER;
    }
    if (NULL != min_span_expr) {
        assert(EXPR_VALUE_TYPE_INTEGER
               == min_span_expr->ndat->u.rexpr.value_type_mask);
        if (AST_NODE_TYPE_REXPR_NATIVE == min_span_expr->ndat->type) {
            int64_t user_min_span_size;

            user_min_span_size = min_span_expr->ndat->u.rexpr_native.value.integer;
            if (user_min_span_size < min_span_size) {
                semantic_error(SEMANTIC_LOGLEVEL_ERROR,
                               &min_span_expr->loc,
                               "declared min span size too small to "
                               "hold all contained fields (requires %"
                               PRIi64" bytes but only spans %"PRIi64")",
                               min_span_size, user_min_span_size);
                return -1;
            }
            min_span_size = user_min_span_size;
        } else {
            dynamic_span = TRUE;
        }
    }
    if (NULL != max_span_expr) {
        assert(EXPR_VALUE_TYPE_INTEGER
               == max_span_expr->ndat->u.rexpr.value_type_mask);
        if (AST_NODE_TYPE_REXPR_NATIVE == max_span_expr->ndat->type) {
            int64_t user_max_span_size;

            user_max_span_size = max_span_expr->ndat->u.rexpr_native.value.integer;
            if (user_max_span_size < min_span_size) {
                semantic_error(SEMANTIC_LOGLEVEL_ERROR,
                               &max_span_expr->loc,
                               "declared max span size smaller than "
                               "min span size (%"PRIi64" bytes < %"
                               PRIi64")",
                               user_max_span_size, min_span_size);
                return -1;
            }
            // override
            dynamic_span = (user_max_span_size != min_span_size);
        } else {
            dynamic_span = TRUE;
        }
    }
    // when no span expression is declared, span space matches
    // used space
    if (NULL == min_span_expr && NULL == max_span_expr && !dynamic_span) {
        dynamic_span = dynamic_used;
    }
    // if max span expression exists and is inconditional, slack
    // space claimed by children is allocated greedily by their
    // parent up to the max allowable, otherwise it has to be
    // claimed as well
    if (NULL == max_span_expr) {
        // if any field uses the remaining slack space, the block also
        // does, otherwise the same idea applies with fields that may
        // use the slack space (those that have filters defining their
        // span size)
        if (child_uses_slack) {
            item->ndat->u.item.flags |= ITEMFLAG_USES_SLACK;
        }
        if (child_spreads_slack) {
            item->ndat->u.item.flags |= ITEMFLAG_SPREADS_SLACK;
        } else if (child_conditionally_spreads_slack) {
            item->ndat->u.item.flags |= ITEMFLAG_CONDITIONALLY_SPREADS_SLACK;
        }
        if (child_fills_slack) {
            item->ndat->u.item.flags |= ITEMFLAG_FILLS_SLACK;
        } else if (child_conditionally_fills_slack) {
            item->ndat->u.item.flags |= ITEMFLAG_CONDITIONALLY_FILLS_SLACK;
        }
    }
    item->ndat->u.item.min_span_size = min_span_size;
    if (dynamic_span) {
        item->ndat->u.item.flags |= ITEMFLAG_IS_SPAN_SIZE_DYNAMIC;
    }
    if (dynamic_used) {
        item->ndat->u.item.flags |= ITEMFLAG_IS_USED_SIZE_DYNAMIC;
    }
    if (contains_last_attr) {
        item->flags |= ASTFLAG_CONTAINS_LAST_ATTR;
    }
    return 0;
}

static int
composite_filter_instance_compile(struct ast_node_hdl *filter,
                                  struct filter_instance *f_instance,
                                  dep_resolver_tagset_t tags,
                                  struct compile_ctx *ctx)
{
    struct filter_instance_composite *composite;

    composite = (struct filter_instance_composite *)f_instance;
    if (0 != (tags & COMPILE_TAG_NODE_SPAN_SIZE)
        && -1 == compile_span_size_composite(filter, composite, ctx)) {
        return -1;
    }
    return 0;
}

void
filter_class_declare_struct(void)
{
    int ret;

    ret = filter_class_declare("struct",
                               EXPR_VALUE_TYPE_UNSET,
                               struct_filter_instance_build,
                               composite_filter_instance_compile,
                               5,
                               "@span", EXPR_VALUE_TYPE_INTEGER, 0,
                               "@minspan", EXPR_VALUE_TYPE_INTEGER, 0,
                               "@maxspan", EXPR_VALUE_TYPE_INTEGER, 0,
                               "@key", (EXPR_VALUE_TYPE_INTEGER |
                                        EXPR_VALUE_TYPE_STRING), 0,
                               "@last", EXPR_VALUE_TYPE_BOOLEAN, 0);
    assert(0 == ret);
}

void
filter_class_declare_union(void)
{
    int ret;

    ret = filter_class_declare("union",
                               EXPR_VALUE_TYPE_UNSET,
                               union_filter_instance_build,
                               composite_filter_instance_compile,
                               5,
                               "@span", EXPR_VALUE_TYPE_INTEGER, 0,
                               "@minspan", EXPR_VALUE_TYPE_INTEGER, 0,
                               "@maxspan", EXPR_VALUE_TYPE_INTEGER, 0,
                               "@key", (EXPR_VALUE_TYPE_INTEGER |
                                        EXPR_VALUE_TYPE_STRING), 0,
                               "@last", EXPR_VALUE_TYPE_BOOLEAN, 0);
    assert(0 == ret);
}
