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

static bitpunch_status_t
tracker_goto_first_item_int__composite(struct tracker *tk, int flat,
                                       struct browse_state *bst);
static bitpunch_status_t
tracker_goto_next_item_int__composite(struct tracker *tk, int flat,
                                      struct browse_state *bst);

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
    struct filter_def *filter_def;
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

    filter_def = item->ndat->u.rexpr_filter.filter_def;
    field_list = filter_def->block_stmt_list.field_list;
    if (-1 == compile_fields(field_list,
                             COMPILE_TAG_NODE_TYPE |
                             COMPILE_TAG_NODE_SPAN_SIZE, 0u, ctx)) {
        return -1;
    }
    min_span_expr = NULL;
    max_span_expr = NULL;
    STATEMENT_FOREACH(
        named_expr, attr,
        item->ndat->u.rexpr_filter.filter_def->block_stmt_list.attribute_list,
        list) {
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
    first_trailer_field = NULL;
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

static bitpunch_status_t
box_compute_span_size__struct(struct box *box,
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
    bt_ret = tracker_goto_first_item_int__composite(tk, TRUE, bst);
    while (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_goto_next_item_int__composite(tk, TRUE, bst);
    }
    if (BITPUNCH_NO_ITEM == bt_ret) {
        assert(-1 != tk->item_offset);
        if (0 != (box->flags & BOX_RALIGN)) {
            bt_ret = box_set_start_offset(tk->box, tk->item_offset,
                                          BOX_START_OFFSET_SPAN, bst);
        } else {
            bt_ret = box_set_end_offset(tk->box, tk->item_offset,
                                        BOX_END_OFFSET_SPAN, bst);
        }
    }
    tracker_delete(tk);
    return bt_ret;
}

static bitpunch_status_t
box_compute_span_size__union_dynamic_size(struct box *box,
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
    bt_ret = tracker_goto_first_item_int__composite(tk, TRUE, bst);
    while (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_get_item_size_internal(tk, &subitem_size, bst);
        if (BITPUNCH_OK != bt_ret) {
            break ;
        }
        max_subitem_size = MAX(max_subitem_size, subitem_size);
        bt_ret = tracker_goto_next_item_int__composite(tk, TRUE, bst);
    }
    if (BITPUNCH_NO_ITEM == bt_ret) {
        bt_ret = box_set_span_size(box, max_subitem_size, bst);
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
    bt_ret = tracker_goto_first_item_int__composite(tk, TRUE, bst);
    while (BITPUNCH_OK == bt_ret) {
        field = tk->cur.u.block.field;
        if (get_left_offset) {
            if (0 == (field->nstmt.stmt.stmt_flags & FIELD_FLAG_HEADER)) {
                break ;
            }
        } else {
            if (0 == (field->nstmt.stmt.stmt_flags & FIELD_FLAG_TRAILER)) {
                break ;
            }
        }
        bt_ret = tracker_goto_next_item_int__composite(tk, TRUE, bst);
    }
    if (BITPUNCH_OK == bt_ret) {
        *max_slack_offsetp = tk->item_offset;
    }
    tracker_delete(tk);
    return bt_ret;
}

static bitpunch_status_t
box_get_n_items__composite(struct box *box, int64_t *item_countp,
                       struct browse_state *bst)
{
    DBG_BOX_DUMP(box);
    return filter_get_n_statements_internal(
        box->filter, box, STATEMENT_TYPE_FIELD, NULL,
        item_countp, bst);
}


static bitpunch_status_t
tracker_get_item_key__composite(struct tracker *tk,
                            expr_value_t *keyp,
                            int *nth_twinp,
                            struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    assert(NULL != tk->cur.u.block.field);
    if (NULL != keyp) {
        *keyp = expr_value_as_string(tk->cur.u.block.field->nstmt.name);
    }
    if (NULL != nth_twinp) {
        /* field names are unique */
        *nth_twinp = 0;
    }
    return BITPUNCH_OK;
}


static void
tracker_set_field_internal__composite(struct tracker *tk,
                                      const struct field *field,
                                      struct browse_state *bst)
{
    DPRINT("TK set field "ANSI_COLOR_GREEN"%s"ANSI_COLOR_RESET" on:\n",
           field->nstmt.name);
    DBG_TRACKER_DUMP(tk);
    tracker_set_dangling(tk);
    tk->cur = track_path_from_composite_field(field);
    tk->dpath.filter = field->filter;
    DBG_TRACKER_CHECK_STATE(tk);
}

static bitpunch_status_t
tracker_goto_item_int__composite(struct tracker *tk,
                             const struct field *field, int flat,
                             struct browse_state *bst)
{
    struct tracker *xtk;
    bitpunch_status_t bt_ret;
    struct statement_iterator stit;
    const struct statement *stmt;

    DBG_TRACKER_DUMP(tk);
    if (flat
        || NULL != field->nstmt.name
        || 0 != (field->nstmt.stmt.stmt_flags & FIELD_FLAG_HIDDEN)) {
        tracker_set_field_internal__composite(tk, field, bst);
        return BITPUNCH_OK;
    }
    // recurse into anonymous struct's fields
    xtk = tracker_dup(tk);
    do {
        tracker_set_field_internal__composite(xtk, field, bst);
        bt_ret = tracker_enter_item_internal(xtk, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(xtk);
            return bt_ret;
        }
        if (0 != (tk->flags & TRACKER_REVERSED)) {
            stit = filter_riter_statements(
                xtk->box->filter, xtk->box, STATEMENT_TYPE_FIELD, NULL);
        } else {
            stit = filter_iter_statements(
                xtk->box->filter, xtk->box, STATEMENT_TYPE_FIELD, NULL);
        }
        bt_ret = filter_iter_statements_next_internal(&stit, NULL, &stmt, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(xtk);
            return bt_ret;
        }
        field = (const struct field *)stmt;
    } while (NULL == field->nstmt.name
             && 0 == (field->nstmt.stmt.stmt_flags & FIELD_FLAG_HIDDEN));
    tracker_set(tk, xtk);
    tracker_delete(xtk);
    tracker_set_field_internal__composite(tk, field, bst);
    return BITPUNCH_OK;
}

static struct box *
box_get_container_parent_box(struct box *box)
{
    while (TRUE) {
        switch (box->filter->ndat->type) {
        case AST_NODE_TYPE_COMPOSITE:
        case AST_NODE_TYPE_ARRAY:
            return box;
        default:
            box = box->parent_box;
            break ;
        }
    }
    return box;
}

static int
tracker_in_anonymous_composite(struct tracker *tk)
{
    struct box *composite_box;
    struct box *parent_box;

    composite_box = box_get_container_parent_box(tk->box);
    parent_box = composite_box->parent_box;
    return (NULL != parent_box
            && AST_NODE_TYPE_COMPOSITE == parent_box->filter->ndat->type
            && NULL != composite_box->track_path.u.block.field
            && NULL == composite_box->track_path.u.block.field->nstmt.name
            && 0 == (composite_box->track_path.u.block.field
                     ->nstmt.stmt.stmt_flags & FIELD_FLAG_HIDDEN));
}

static bitpunch_status_t
tracker_goto_first_item_int__composite(struct tracker *tk, int flat,
                                       struct browse_state *bst)
{
    struct statement_iterator stit;
    const struct statement *stmt;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    if (!flat && NULL != tk->cur.u.block.field) {
        // return to base, non-anonymous level
        while (tracker_in_anonymous_composite(tk)) {
            bt_ret = tracker_return_internal(tk, bst);
            assert(BITPUNCH_OK == bt_ret);
        }
    }
    bt_ret = tracker_set_item_offset_at_box(tk, tk->box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (0 != (tk->flags & TRACKER_REVERSED)) {
        stit = filter_riter_statements(
            tk->box->filter, tk->box, STATEMENT_TYPE_FIELD, NULL);
    } else {
        stit = filter_iter_statements(
            tk->box->filter, tk->box, STATEMENT_TYPE_FIELD, NULL);
    }
    bt_ret = filter_iter_statements_next_internal(&stit, NULL, &stmt, bst);
    if (BITPUNCH_OK != bt_ret) {
        if (BITPUNCH_NO_ITEM == bt_ret) {
            bt_ret = tracker_set_end(tk, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
            return BITPUNCH_NO_ITEM;
        } else {
            return bt_ret;
        }
    }
    return tracker_goto_item_int__composite(tk, (const struct field *)stmt,
                                        flat, bst);
}


static bitpunch_status_t
tracker_goto_next_item_int__composite(struct tracker *tk, int flat,
                                      struct browse_state *bst)
{
    struct statement_iterator stit;
    const struct statement *stmt;
    bitpunch_status_t bt_ret;
    int reversed;

    DBG_TRACKER_DUMP(tk);
    reversed = (0 != (tk->flags & TRACKER_REVERSED));
    DBG_TRACKER_CHECK_STATE(tk);
    while (TRUE) {
        /* union: no offset change */
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
            struct filter_instance_composite *composite;

            composite = (struct filter_instance_composite *)
                tk->box->filter->ndat->u.rexpr_filter.f_instance;
            if (COMPOSITE_TYPE_STRUCT == composite->type) {
                int64_t item_size;

                bt_ret = tracker_get_item_size_internal(tk, &item_size, bst);
                if (BITPUNCH_OK != bt_ret) {
                    DBG_TRACKER_CHECK_STATE(tk);
                    return bt_ret;
                }
                DBG_TRACKER_CHECK_STATE(tk);
                if (reversed) {
                    assert(tk->item_offset >= item_size);
                    tk->item_offset -= item_size;
                } else {
                    tk->item_offset += item_size;
                }
            }
        }
        tracker_reset_item_cache(tk);
        if (reversed) {
            stit = filter_riter_statements_from(
                tk->box->filter, tk->box,
                (const struct statement *)tk->cur.u.block.field, NULL);
        } else {
            stit = filter_iter_statements_from(
                tk->box->filter, tk->box,
                (const struct statement *)tk->cur.u.block.field, NULL);
        }
        bt_ret = filter_iter_statements_next_internal(&stit, NULL, &stmt, bst);
        if (BITPUNCH_NO_ITEM != bt_ret) {
            break ;
        }
        if (!flat && tracker_in_anonymous_composite(tk)) {
            // return from anonymous struct's field list
            bt_ret = tracker_return_internal(tk, bst);
            assert(BITPUNCH_OK == bt_ret);
        } else {
            bt_ret = tracker_set_end(tk, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
            return BITPUNCH_NO_ITEM;
        }
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return tracker_goto_item_int__composite(tk, (const struct field *)stmt,
                                        flat, bst);
}

static bitpunch_status_t
tracker_goto_first_item__composite(struct tracker *tk,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_goto_first_item_int__composite(tk, FALSE, bst);
    // skip hidden fields
    while (BITPUNCH_OK == bt_ret
           && 0 != (tk->cur.u.block.field->nstmt.stmt.stmt_flags
                    & FIELD_FLAG_HIDDEN)) {
        bt_ret = tracker_goto_next_item_int__composite(tk, FALSE, bst);
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_next_item__composite(struct tracker *tk,
                                  struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    // skip hidden fields
    do {
        bt_ret = tracker_goto_next_item_int__composite(tk, FALSE, bst);
    } while (BITPUNCH_OK == bt_ret
             && 0 != (tk->cur.u.block.field->nstmt.stmt.stmt_flags
                      & FIELD_FLAG_HIDDEN));
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item__composite(struct tracker *tk, int64_t index,
                                 struct browse_state *bst)
{
    struct tracker *xtk;
    bitpunch_status_t bt_ret;
    int64_t cur_idx;

    DBG_TRACKER_DUMP(tk);
    xtk = tracker_dup(tk);
    bt_ret = tracker_goto_first_item__composite(xtk, bst);
    cur_idx = 0;
    while (TRUE) {
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(xtk);
            return bt_ret;
        }
        if (cur_idx == index) {
            break ;
        }
        ++cur_idx;
        bt_ret = tracker_goto_next_item__composite(xtk, bst);
    }
    tracker_set(tk, xtk);
    tracker_delete(xtk);
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_next_item_with_key__composite(struct tracker *tk,
                                           expr_value_t item_key,
                                           struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return BITPUNCH_NOT_IMPLEMENTED;
}

static bitpunch_status_t
tracker_goto_nth_item_with_key__composite(
    struct tracker *tk, expr_value_t item_key, int nth_twin,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    if (0 != nth_twin) {
        // all composite attributes are unique
        return BITPUNCH_NO_ITEM;
    }
    if (EXPR_VALUE_TYPE_STRING != item_key.type) {
        // all composite attributes are string-typed
        return BITPUNCH_NO_ITEM;
    }
    bt_ret = tracker_goto_first_item_int__composite(tk, FALSE, bst);
    while (BITPUNCH_OK == bt_ret) {
        if (NULL != tk->cur.u.block.field->nstmt.name
            && strlen(tk->cur.u.block.field->nstmt.name) == item_key.string.len
            && 0 == memcmp(tk->cur.u.block.field->nstmt.name,
                           item_key.string.str, item_key.string.len)) {
            return BITPUNCH_OK;
        }
        bt_ret = tracker_goto_next_item_int__composite(tk, FALSE, bst);
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_named_item__composite(struct tracker *tk, const char *name,
                                   struct browse_state *bst)
{
    expr_value_t key;

    key = expr_value_as_string(name);
    return tracker_goto_nth_item_with_key__composite(tk, key, 0, bst);
}

static bitpunch_status_t
tracker_goto_next_key_match__composite(struct tracker *tk,
                                   expr_value_t index,
                                   struct track_path search_boundary,
                                   struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst, NULL);
}

bitpunch_status_t
tracker_goto_field_internal(struct tracker *tk,
                            const struct field *to_field, int flat,
                            struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int tracker_set_reversed;
    int reverse_direction;

    DBG_TRACKER_DUMP(tk);
    assert(flat || NULL != to_field->nstmt.name);

    if (flat && 0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        tracker_set_field_internal__composite(tk, to_field, bst);
        return BITPUNCH_OK;
    }
    if (0 != (tk->box->flags & BOX_RALIGN)) {
        tracker_set_reversed =
            0 == (to_field->nstmt.stmt.stmt_flags & FIELD_FLAG_HEADER);
    } else {
        tracker_set_reversed =
            0 != (to_field->nstmt.stmt.stmt_flags & FIELD_FLAG_TRAILER);
    }
    // union may be nesting anonymous structures, so setting offset to
    // box offset as an easy optimization for union types is incorrect
    // (keep a more complex optim for later)
    reverse_direction =
        (tracker_set_reversed && 0 == (tk->flags & TRACKER_REVERSED))
        || (!tracker_set_reversed && 0 != (tk->flags & TRACKER_REVERSED));
    if (reverse_direction) {
        tk->flags ^= TRACKER_REVERSED;
    }
    bt_ret = tracker_goto_first_item_int__composite(tk, flat, bst);
    while (BITPUNCH_OK == bt_ret && tk->cur.u.block.field != to_field) {
        bt_ret = tracker_goto_next_item_int__composite(tk, flat, bst);
    }
    if (reverse_direction && BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_reverse_direction_internal(tk, bst);
    }
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

    b_box->compute_slack_size = box_compute_slack_size__as_container_slack;
    if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_box->compute_span_size = box_compute_span_size__static_size;
    } else if (0 != (item->ndat->u.item.flags & ITEMFLAG_FILLS_SLACK)) {
        b_box->compute_span_size = box_compute_span_size__as_max_span;
    } else if (COMPOSITE_TYPE_STRUCT == composite->type) {
        b_box->compute_span_size = box_compute_span_size__struct;
    } else /* union */ {
        b_box->compute_span_size = box_compute_span_size__union_dynamic_size;
    }
    if (NULL != block_get_first_attribute(item, "@span") ||
        NULL != block_get_first_attribute(item, "@minspan") ||
        NULL != block_get_first_attribute(item, "@maxspan")) {
        b_box->compute_min_span_size = box_compute_min_span_size__span_expr;
        b_box->compute_max_span_size = box_compute_max_span_size__span_expr;
    } else {
        b_box->compute_min_span_size =
            box_compute_min_span_size__as_hard_min;
        if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            b_box->compute_max_span_size =
                box_compute_max_span_size__as_span;
        } else {
            b_box->compute_max_span_size =
                box_compute_max_span_size__as_slack;
        }
    }
    if (COMPOSITE_TYPE_STRUCT == composite->type) {
        b_box->get_slack_child_allocation =
            box_get_slack_child_allocation__struct;
    }
    b_box->get_n_items = box_get_n_items__composite;
    b_box->compute_used_size = box_compute_used_size__as_span;
}

static void
compile_node_backends__tracker__composite(struct ast_node_hdl *item)
{
    struct item_backend *b_item;
    struct tracker_backend *b_tk;

    b_item = &item->ndat->u.rexpr_filter.f_instance->b_item;
    b_tk = &item->ndat->u.rexpr_filter.f_instance->b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

    b_tk->get_item_key = tracker_get_item_key__composite;
    if (NULL == b_item->compute_item_size) {
        b_tk->compute_item_size = tracker_compute_item_size__item_box;
    }
    b_tk->goto_first_item = tracker_goto_first_item__composite;
    b_tk->goto_next_item = tracker_goto_next_item__composite;
    b_tk->goto_nth_item = tracker_goto_nth_item__composite;
    b_tk->goto_named_item = tracker_goto_named_item__composite;
    b_tk->goto_next_key_match = tracker_goto_next_key_match__composite;
    b_tk->goto_next_item_with_key =
        tracker_goto_next_item_with_key__composite;
    b_tk->goto_nth_item_with_key =
        tracker_goto_nth_item_with_key__composite;
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
