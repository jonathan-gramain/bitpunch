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

#include "core/debug.h"
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
compile_type_composite(struct ast_node_hdl *item,
                       struct filter_instance_composite *composite,
                       struct compile_ctx *ctx)
{
    struct scope_def *scope_def;
    struct named_expr *attr;
    int contains_last_attr;

    contains_last_attr = FALSE;
    scope_def = filter_get_scope_def(item);
    STATEMENT_FOREACH(
        named_expr, attr, scope_def->block_stmt_list.attribute_list, list) {
        if (0 == strcmp(attr->nstmt.name, "@last")) {
            contains_last_attr = TRUE;
        }
    }
    if (contains_last_attr) {
        item->flags |= ASTFLAG_CONTAINS_LAST_ATTR;
    }
    item->ndat->u.item.flags |= ITEMFLAG_FILTER_MAPS_OBJECT;
    return 0;
}

static int
compile_span_size_composite(struct ast_node_hdl *item,
                            struct filter_instance_composite *composite,
                            struct compile_ctx *ctx)
{
    struct scope_def *scope_def;
    struct ast_node_hdl *base_filter;
    struct named_expr *attr;
    struct ast_node_hdl *min_span_expr;
    struct ast_node_hdl *max_span_expr;
    int new_min_span_expr;
    struct filter_iterator fit;
    const struct statement_list *field_list;
    struct field *field;
    struct ast_node_hdl_array field_items;
    struct ast_node_hdl *field_item;
    int64_t hard_min_span_size;
    int64_t user_min_span_size;
    int64_t min_span_size;
    int var_span;
    int var_used;
    int child_uses_slack;
    int child_spreads_slack;
    int child_conditionally_spreads_slack;
    int child_fills_slack;
    int child_conditionally_fills_slack;
    struct field *last_slack_field;
    struct field *first_trailer_field;
    enum field_flag field_flags;
    bitpunch_status_t bt_ret;

    /* - Compute the minimum span size from the sum (struct) or
       max (union) of fields' minimum span sizes. If size is
       static, minimum size is actual size.

       - If an unconditional span size is given by an expression,
       resolve it

       - Set the dynamic flag if actual size may be greater than
       the minimum
    */

    var_span = FALSE;
    var_used = FALSE;
    child_uses_slack = FALSE;
    child_spreads_slack = FALSE;
    child_conditionally_spreads_slack = FALSE;
    child_fills_slack = FALSE;
    child_conditionally_fills_slack = FALSE;

    scope_def = filter_get_scope_def(item);
    base_filter = item->ndat->u.rexpr_filter.filter_def->base_filter;
    field_list = scope_def->block_stmt_list.field_list;
    if (NULL != base_filter) {
        compile_node(base_filter, ctx,
                     COMPILE_TAG_NODE_TYPE |
                     COMPILE_TAG_NODE_SPAN_SIZE, 0u,
                     RESOLVE_EXPECT_TYPE);
    }
    compile_fields(field_list,
                   COMPILE_TAG_NODE_TYPE |
                   COMPILE_TAG_NODE_SPAN_SIZE, 0u, ctx);
    if (!compile_continue(ctx)) {
        return -1;
    }
    min_span_expr = NULL;
    max_span_expr = NULL;
    user_min_span_size = 0;
    fit = filter_iter_declared_statements(item, STATEMENT_TYPE_ATTRIBUTE, NULL);
    bt_ret = filter_iter_statements_next_internal(
        &fit, NULL, (const struct statement **)&attr, NULL);
    while (BITPUNCH_OK == bt_ret) {
        new_min_span_expr = FALSE;
        if (0 == strcmp(attr->nstmt.name, "@minspan")) {
            if (NULL == attr->nstmt.stmt.cond) {
                min_span_expr = attr->expr;
                new_min_span_expr = TRUE;
            }
            var_span = TRUE;
        } else if (0 == strcmp(attr->nstmt.name, "@maxspan")) {
            if (NULL == attr->nstmt.stmt.cond) {
                max_span_expr = attr->expr;
            }
            var_span = TRUE;
        } else if (0 == strcmp(attr->nstmt.name, "@span")) {
            if (NULL == attr->nstmt.stmt.cond) {
                min_span_expr = attr->expr;
                max_span_expr = attr->expr;
                new_min_span_expr = TRUE;
            } else {
                var_span = TRUE;
            }
        }
        if (new_min_span_expr) {
            assert(EXPR_VALUE_TYPE_INTEGER
                   == min_span_expr->ndat->u.rexpr.value_type_mask);
            if (AST_NODE_TYPE_REXPR_NATIVE == min_span_expr->ndat->type) {
                user_min_span_size = MAX(
                    user_min_span_size,
                    min_span_expr->ndat->u.rexpr_native.value.integer);
            } else {
                var_span = TRUE;
            }
        }
        bt_ret = filter_iter_statements_next_internal(
            &fit, NULL, (const struct statement **)&attr, NULL);
    }
    last_slack_field = NULL;
    first_trailer_field = NULL;
    hard_min_span_size = 0;
    fit = filter_iter_declared_statements(item, STATEMENT_TYPE_FIELD, NULL);
    bt_ret = filter_iter_statements_next_internal(
        &fit, NULL, (const struct statement **)&field, NULL);
    while (BITPUNCH_OK == bt_ret) {
        bt_ret = ast_node_filter_get_items(field->filter, &field_items);
        if (BITPUNCH_OK != bt_ret) {
            return -1;
        }
        assert(ARRAY_SIZE(&field_items) >= 1);
        field_item = ARRAY_ITEM(&field_items, 0);
        if (ARRAY_SIZE(&field_items) > 1) {
            var_used = TRUE;
        } else {
            if (0 != (field_item->ndat->u.item.flags
                      & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
                var_used = TRUE;
            } else if (NULL == field->nstmt.stmt.cond) {
                /* only update min span size if field is not conditional */
                assert(SPAN_SIZE_UNDEF != field_item->ndat->u.item.min_span_size);
                if (COMPOSITE_TYPE_UNION == composite->type) {
                    hard_min_span_size =
                        MAX(hard_min_span_size,
                            field_item->ndat->u.item.min_span_size);
                } else /* struct */ {
                    hard_min_span_size +=
                        field_item->ndat->u.item.min_span_size;
                }
            } else {
                // if at least one conditional field is present, the
                // used size is dynamic
                var_used = TRUE;
            }
            if (0 != (field_item->ndat->u.item.flags & ITEMFLAG_USES_SLACK)) {
                child_uses_slack = TRUE;
            }
            if (0 != (field_item->ndat->u.item.flags & ITEMFLAG_SPREADS_SLACK)) {
                if (NULL != field->nstmt.stmt.cond) {
                    child_conditionally_spreads_slack = TRUE;
                } else {
                    child_spreads_slack = TRUE;
                    last_slack_field = field;
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
        bt_ret = filter_iter_statements_next_internal(
            &fit, NULL, (const struct statement **)&field, NULL);
    }
    if (child_spreads_slack ||
        child_conditionally_spreads_slack) {
        field_flags = FIELD_FLAG_HEADER;
        fit = filter_iter_declared_statements(item, STATEMENT_TYPE_FIELD, NULL);
        bt_ret = filter_iter_statements_next_internal(
            &fit, NULL, (const struct statement **)&field, NULL);
        while (BITPUNCH_OK == bt_ret) {
            bt_ret = ast_node_filter_get_items(field->filter, &field_items);
            if (BITPUNCH_OK != bt_ret) {
                return -1;
            }
            field_item = ARRAY_ITEM(&field_items, 0);
            ast_node_hdl_array_destroy(&field_items);
            if (0 != (field_item->ndat->u.item.flags
                      & (ITEMFLAG_SPREADS_SLACK |
                         ITEMFLAG_CONDITIONALLY_SPREADS_SLACK))) {
                field_flags &= ~FIELD_FLAG_HEADER;
            }
            if (field == last_slack_field) {
                field_item->ndat->u.item.flags |= ITEMFLAG_FILLS_SLACK;
            }
            if (field == first_trailer_field) {
                field_flags |= FIELD_FLAG_TRAILER;
            }
            field->nstmt.stmt.stmt_flags |= field_flags;
            bt_ret = filter_iter_statements_next_internal(
                &fit, NULL, (const struct statement **)&field, NULL);
        }
    }
    min_span_size = MAX(hard_min_span_size, user_min_span_size);
    if (NULL != max_span_expr) {
        assert(EXPR_VALUE_TYPE_INTEGER
               == max_span_expr->ndat->u.rexpr.value_type_mask);
        if (AST_NODE_TYPE_REXPR_NATIVE == max_span_expr->ndat->type) {
            int64_t user_max_span_size;

            user_max_span_size =
                max_span_expr->ndat->u.rexpr_native.value.integer;
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
            var_span = (user_max_span_size != min_span_size);
        } else {
            var_span = TRUE;
        }
    }
    // when no span expression is declared, span space matches
    // used space
    if (NULL == min_span_expr && NULL == max_span_expr && !var_span) {
        var_span = var_used;
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
        // if @minspan is set, children (especially arrays) may query
        // for space that is not available, hence not filling the
        // slack space entirely.
        if (NULL == min_span_expr) {
            if (child_fills_slack) {
                item->ndat->u.item.flags |= ITEMFLAG_FILLS_SLACK;
            } else if (child_conditionally_fills_slack) {
                item->ndat->u.item.flags |= ITEMFLAG_CONDITIONALLY_FILLS_SLACK;
            }
        }
    }
    item->ndat->u.item.min_span_size = min_span_size;
    if (var_span) {
        item->ndat->u.item.flags |= ITEMFLAG_IS_SPAN_SIZE_VARIABLE;
    }
    if (var_used) {
        item->ndat->u.item.flags |= ITEMFLAG_IS_USED_SIZE_VARIABLE;
    }
    return 0;
}

static bitpunch_status_t
box_compute_used_size__struct(struct box *box,
                              struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct tracker *tk;

    DBG_BOX_DUMP(box);
    bt_ret = track_box_contents_internal(box, &tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    tk->flags |= (TRACKER_NEED_ITEM_OFFSET |
                  ((box->flags & BOX_RALIGN) ? TRACKER_REVERSED : 0u));
    bt_ret = tracker_goto_first_field_internal(tk, TRUE, bst);
    while (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_goto_next_field_internal(tk, TRUE, bst);
    }
    if (BITPUNCH_NO_ITEM == bt_ret) {
        assert(-1 != tk->item_offset);
        if (0 != (box->flags & BOX_RALIGN)) {
            bt_ret = box_set_start_offset(tk->box, tk->item_offset,
                                          BOX_START_OFFSET_USED, bst);
        } else {
            bt_ret = box_set_end_offset(tk->box, tk->item_offset,
                                        BOX_END_OFFSET_USED, bst);
        }
    }
    tracker_delete(tk);
    return bt_ret;
}

static bitpunch_status_t
box_compute_used_size__union_var_size(struct box *box,
                                      struct browse_state *bst)
{
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    int64_t subitem_size;
    int64_t max_subitem_size;

    DBG_BOX_DUMP(box);
    bt_ret = track_box_contents_internal(box, &tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    max_subitem_size = 0;
    bt_ret = tracker_goto_first_field_internal(tk, TRUE, bst);
    while (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_get_item_size_internal(tk, &subitem_size, bst);
        if (BITPUNCH_OK != bt_ret) {
            break ;
        }
        max_subitem_size = MAX(max_subitem_size, subitem_size);
        bt_ret = tracker_goto_next_field_internal(tk, TRUE, bst);
    }
    if (BITPUNCH_NO_ITEM == bt_ret) {
        bt_ret = box_set_used_size(box, max_subitem_size, bst);
    }
    tracker_delete(tk);
    return bt_ret;
}

static bitpunch_status_t
box_get_slack_child_allocation__struct(struct box *box,
                                       int get_left_offset,
                                       int64_t *max_slack_offsetp,
                                       struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct tracker *tk;
    const struct field *field;

    // FIXME this may need rework to get correctly the left-side max
    // slack offset
    DBG_BOX_DUMP(box);
    bt_ret = track_box_contents_internal(box, &tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    if (!get_left_offset) {
        // only reverse tracker when getting the right slack offset
        // (tracking fields backwards until a non-raligned field is
        // encountered)
        tk->flags |= TRACKER_REVERSED;
    }
    bt_ret = tracker_goto_first_field_internal(tk, TRUE, bst);
    while (BITPUNCH_OK == bt_ret) {
        field = tk->cur.u.field;
        if (get_left_offset) {
            if (0 == (field->nstmt.stmt.stmt_flags & FIELD_FLAG_HEADER)) {
                break ;
            }
        } else {
            if (0 == (field->nstmt.stmt.stmt_flags & FIELD_FLAG_TRAILER)) {
                break ;
            }
        }
        bt_ret = tracker_goto_next_field_internal(tk, TRUE, bst);
    }
    if (BITPUNCH_OK == bt_ret) {
        *max_slack_offsetp = tk->item_offset;
    }
    tracker_delete(tk);
    return bt_ret;
}

static void
compile_node_backends__box__composite(struct ast_node_hdl *item)
{
    struct box_backend *b_box = NULL;
    struct filter_instance_composite *composite;

    b_box = &item->ndat->u.rexpr_filter.f_instance->b_box;
    memset(b_box, 0, sizeof (*b_box));
    composite = (struct filter_instance_composite *)
        item->ndat->u.rexpr_filter.f_instance;

    if (NULL != filter_get_first_declared_attribute(item, "@span")) {
        b_box->compute_span_size = box_compute_span_size__span_expr;
    } else {
        b_box->compute_span_size = box_compute_span_size__as_used;
    }
    if (NULL != filter_get_first_declared_attribute(item, "@span") ||
        NULL != filter_get_first_declared_attribute(item, "@minspan")) {
        b_box->compute_min_span_size = box_compute_min_span_size__span_expr;
    } else {
        b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    }

    if (NULL != filter_get_first_declared_attribute(item, "@span") ||
        NULL != filter_get_first_declared_attribute(item, "@maxspan")) {
        b_box->compute_max_span_size = box_compute_max_span_size__span_expr;
    } else if (0 == (item->ndat->u.item.flags
                     & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
        b_box->compute_max_span_size = box_compute_max_span_size__as_span;
    } else {
        b_box->compute_max_span_size = box_compute_max_span_size__as_slack;
    }
    if (COMPOSITE_TYPE_STRUCT == composite->type) {
        b_box->get_slack_child_allocation =
            box_get_slack_child_allocation__struct;
    }
    b_box->get_n_items = box_get_n_items__scope;

    if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
        b_box->compute_used_size = box_compute_used_size__const_size;
    } else if (0 != (item->ndat->u.item.flags & ITEMFLAG_FILLS_SLACK)) {
        b_box->compute_used_size = box_compute_used_size__as_max_span;
    } else if (COMPOSITE_TYPE_STRUCT == composite->type) {
        b_box->compute_used_size = box_compute_used_size__struct;
    } else /* union */ {
        b_box->compute_used_size = box_compute_used_size__union_var_size;
    }
}



static bitpunch_status_t
tracker_init_item_offset__composite(struct tracker *tk,
                                    struct browse_state *bst)
{
    return tracker_set_item_offset_at_box(tk, tk->box, bst);
}

static bitpunch_status_t
tracker_advance_item_offset__composite(struct tracker *tk,
                                       struct browse_state *bst)
{
    struct filter_instance_composite *composite;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    composite = (struct filter_instance_composite *)
        tk->box->filter->ndat->u.rexpr_filter.f_instance;

    if (COMPOSITE_TYPE_STRUCT == composite->type) {
        int reversed;
        int64_t item_size;

        reversed = (0 != (tk->flags & TRACKER_REVERSED));
        bt_ret = tracker_get_item_size_internal(tk, &item_size, bst);
        if (BITPUNCH_OK != bt_ret) {
            DBG_TRACKER_CHECK_STATE(tk);
            return bt_ret;
        }
        DBG_TRACKER_CHECK_STATE(tk);
        if (reversed) {
            tk->item_offset -= item_size;
        } else {
            tk->item_offset += item_size;
        }
    } // else union: no offset change
    return BITPUNCH_OK;
}

static void
compile_node_backends__tracker__composite(struct ast_node_hdl *item)
{
    struct tracker_backend *b_tk;

    b_tk = &item->ndat->u.rexpr_filter.f_instance->b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

    b_tk->get_item_key = tracker_get_item_key__scope;
    b_tk->init_item_offset = tracker_init_item_offset__composite;
    b_tk->advance_item_offset = tracker_advance_item_offset__composite;
    b_tk->goto_first_item = tracker_goto_first_item__scope;
    b_tk->goto_next_item = tracker_goto_next_item__scope;
    b_tk->goto_nth_item = tracker_goto_nth_item__scope;
    b_tk->goto_named_item = tracker_goto_named_item__scope;
    b_tk->goto_next_key_match = tracker_goto_next_key_match__scope;
    b_tk->goto_next_item_with_key =
        tracker_goto_next_item_with_key__scope;
    b_tk->goto_nth_item_with_key =
        tracker_goto_nth_item_with_key__scope;
    b_tk->goto_end_path = tracker_goto_end_path__scope;
    b_tk->goto_nil = tracker_goto_nil__scope;
}

static int
compile_node_backends_composite(struct ast_node_hdl *filter,
                                struct compile_ctx *ctx)
{
    compile_node_backends__item__generic(filter);
    compile_node_backends__box__composite(filter);
    compile_node_backends__tracker__composite(filter);
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
    if (0 != (tags & COMPILE_TAG_NODE_TYPE)
        && -1 == compile_type_composite(filter, composite, ctx)) {
        return -1;
    }
    if (0 != (tags & COMPILE_TAG_NODE_SPAN_SIZE)
        && -1 == compile_span_size_composite(filter, composite, ctx)) {
        return -1;
    }
    if (0 != (tags & COMPILE_TAG_BROWSE_BACKENDS)
        && -1 == compile_node_backends_composite(filter, ctx)) {
        return -1;
    }
    return 0;
}

void
builtin_filter_declare_struct(void)
{
    int ret;

    ret = builtin_filter_declare("struct",
                               EXPR_VALUE_TYPE_UNSET,
                               struct_filter_instance_build,
                               composite_filter_instance_compile,
                               FILTER_CLASS_MAPS_OBJECT,
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
builtin_filter_declare_union(void)
{
    int ret;

    ret = builtin_filter_declare("union",
                               EXPR_VALUE_TYPE_UNSET,
                               union_filter_instance_build,
                               composite_filter_instance_compile,
                               FILTER_CLASS_MAPS_OBJECT,
                               5,
                               "@span", EXPR_VALUE_TYPE_INTEGER, 0,
                               "@minspan", EXPR_VALUE_TYPE_INTEGER, 0,
                               "@maxspan", EXPR_VALUE_TYPE_INTEGER, 0,
                               "@key", (EXPR_VALUE_TYPE_INTEGER |
                                        EXPR_VALUE_TYPE_STRING), 0,
                               "@last", EXPR_VALUE_TYPE_BOOLEAN, 0);
    assert(0 == ret);
}
