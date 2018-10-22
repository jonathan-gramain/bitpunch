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

#include "core/expr_internal.h"
#include "core/debug.h"
#include "filters/array.h"
#include "filters/array_index_cache.h"
#include "filters/byte_array.h"
#include "filters/array_slice.h"
#include "filters/byte_slice.h"

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
        filter_get_scope_def(filter)->block_stmt_list.attribute_list,
        list) {
        if (0 == strcmp(attr->nstmt.name, "@item")) {
            item_type = attr->expr;
        } else if (0 == strcmp(attr->nstmt.name, "@length")) {
            item_count = attr->expr;
        }
    }
    array = new_safe(struct filter_instance_array);
    array->item_type = ast_node_get_named_expr_target(item_type);
    assert(ast_node_is_rexpr_filter(array->item_type));
    array->item_count = item_count;
    return (struct filter_instance *)array;
}

static int
compile_type_array(struct ast_node_hdl *filter,
                   struct filter_instance_array *array,
                   struct compile_ctx *ctx)
{
    if (-1 == compile_node(array->item_type, ctx,
                           COMPILE_TAG_NODE_TYPE, 0u,
                           RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_FILTER)) {
        return -1;
    }
    // if an array of unfiltered bytes, it's a byte array
    if (AST_NODE_TYPE_BYTE == array->item_type->ndat->type) {
        filter->ndat->type = AST_NODE_TYPE_BYTE_ARRAY;
    }
    return 0;
}

static int
compile_span_size_array(struct ast_node_hdl *item,
                        struct filter_instance_array *array,
                        struct compile_ctx *ctx)
{
    struct ast_node_hdl *item_type;
    struct ast_node_hdl *item_count_expr;
    int64_t item_count;
    int64_t min_span_size;
    int var_span;

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
    item_type = ast_node_get_target_item(array->item_type);
    assert(ast_node_is_item(item_type));
    if (NULL != item_count_expr
        && item_count_expr->ndat->type == AST_NODE_TYPE_REXPR_NATIVE
        && item_count_expr->ndat->u.rexpr_native.value.integer > 0) {
        // compile item type as a dependency because the array
        // contains a fixed number of at least one item
        if (-1 == compile_node(item_type, ctx,
                               COMPILE_TAG_NODE_TYPE |
                               COMPILE_TAG_NODE_SPAN_SIZE, 0u,
                               RESOLVE_EXPECT_TYPE |
                               RESOLVE_EXPECT_FILTER)) {
            return -1;
        }
        assert(SPAN_SIZE_UNDEF != item_type->ndat->u.item.min_span_size);
        assert(EXPR_VALUE_TYPE_INTEGER
               == item_count_expr->ndat->u.rexpr.value_type_mask);
        assert(SPAN_SIZE_UNDEF != item_type->ndat->u.item.min_span_size);
        item_count = item_count_expr->ndat->u.rexpr_native.value.integer;
        min_span_size = item_count * item_type->ndat->u.item.min_span_size;
        var_span =
            (0 != (item_type->ndat->u.item.flags & ASTFLAG_CONTAINS_LAST_ATTR)
             || 0 != (item_type->ndat->u.item.flags
                      & ITEMFLAG_IS_SPAN_SIZE_VARIABLE));
    } else {
        // schedule compilation of item type and size without
        // depending on it, so to allow recursive nesting of items and
        // possibly empty arrays
        if (-1 == compile_node(item_type, ctx,
                               0u, (COMPILE_TAG_NODE_TYPE |
                                    COMPILE_TAG_NODE_SPAN_SIZE),
                               RESOLVE_EXPECT_TYPE |
                               RESOLVE_EXPECT_FILTER)) {
            return -1;
        }
        min_span_size = 0;
        var_span = TRUE;
        item->ndat->u.item.min_span_size = 0;
        item->ndat->u.item.flags |= (ITEMFLAG_IS_SPAN_SIZE_VARIABLE |
                                      ITEMFLAG_IS_USED_SIZE_VARIABLE);
    }
    item->ndat->u.item.min_span_size = min_span_size;
    if (var_span) {
        item->ndat->u.item.flags |= (ITEMFLAG_IS_SPAN_SIZE_VARIABLE |
                                      ITEMFLAG_IS_USED_SIZE_VARIABLE);
    }
    if (0 == (item_type->flags & ASTFLAG_CONTAINS_LAST_ATTR)) {
        if (NULL == item_count_expr) {
            item->ndat->u.item.flags |= (ITEMFLAG_USES_SLACK |
                                         ITEMFLAG_SPREADS_SLACK);
            // array items taking more than one byte of space may not
            // allow to fill the whole slack space, so we don't set
            // ITEMFLAG_FILLS_SLACK (meaning used size may be smaller
            // than max span size)
            // FIXME rework trailer / RALIGN handling completely
            if (AST_NODE_TYPE_BYTE == item_type->ndat->type) {
                item->ndat->u.item.flags |= ITEMFLAG_FILLS_SLACK;
            }
        }
    }
    return 0;
}


static bitpunch_status_t
array_box_init(struct box *box, struct browse_state *bst)
{
    box->u.array_generic.n_items = -1;
    return array_box_init_index_cache(box, bst);
}

static void
array_box_destroy(struct box *box)
{
    array_box_destroy_index_cache(box);
}

static bitpunch_status_t
compute_item_size__array_const_item_size(struct ast_node_hdl *item_filter,
                                          struct box *scope,
                                          int64_t item_offset,
                                          int64_t max_span_offset,
                                          int64_t *item_sizep,
                                          struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    int64_t child_item_size;
    expr_value_t item_count;
    struct ast_node_hdl *item_type;

    array = (struct filter_instance_array *)
        item_filter->ndat->u.rexpr_filter.f_instance;
    bt_ret = expr_evaluate_value_internal(array->item_count, scope,
                                          &item_count, bst);
    if (BITPUNCH_OK != bt_ret) {
        // FIXME more appropriate context
        tracker_error_add_box_context(
            scope, bst, "when evaluating array item count expression");
        return bt_ret;
    }
    //TODO optimize by storing the static size in the filter instance
    bt_ret = expr_evaluate_filter_type_internal(
        array->item_type, scope, FILTER_KIND_ITEM, &item_type, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    child_item_size = ast_node_get_min_span_size(item_type);
    *item_sizep = item_count.integer * child_item_size;
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_span_size__array_const_item_size(struct box *box,
                                              struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    int64_t n_elems;
    struct ast_node_hdl *item_type;
    int64_t item_size;

    DBG_BOX_DUMP(box);
    /* array with dynamic item count but static item size:
     * evaluate item count and multiply by item unitary size to
     * get span size */
    bt_ret = box_get_n_items_internal(box, &n_elems, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    array = (struct filter_instance_array *)
        box->filter->ndat->u.rexpr_filter.f_instance;
    //TODO optimize by storing the static size in the filter instance
    bt_ret = expr_evaluate_filter_type_internal(
        array->item_type, box, FILTER_KIND_ITEM, &item_type, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    item_size = ast_node_get_min_span_size(item_type);
    return box_set_span_size(box, n_elems * item_size, bst);
}

static bitpunch_status_t
box_compute_n_items_by_iteration(struct box *box,
                                 struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct tracker *tk;
    int64_t n_items;

    DBG_BOX_DUMP(box);
    /* first compute available slack space to notify iteration where
     * to stop */
    bt_ret = box_compute_slack_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    /* slow path: iterate over all elements */
    bt_ret = track_box_contents_internal(box, &tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    bt_ret = tracker_goto_first_item_internal(tk, bst);
    n_items = 0;
    while (BITPUNCH_OK == bt_ret) {
        ++n_items;
        bt_ret = tracker_goto_next_item_internal(tk, bst);
    }
    if (BITPUNCH_NO_ITEM == bt_ret) {
        if (-1 == box->u.array_generic.n_items) {
            box->u.array_generic.n_items = n_items;
        }
        if (0 != (box->flags & BOX_RALIGN)) {
            bt_ret = box_set_start_offset(box, tk->item_offset,
                                          BOX_START_OFFSET_SPAN, bst);
        } else {
            bt_ret = box_set_end_offset(box, tk->item_offset,
                                        BOX_END_OFFSET_SPAN, bst);
        }
    }
    tracker_delete(tk);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_get_n_items__by_iteration(struct box *box, int64_t *item_countp,
                              struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        bt_ret = box_compute_n_items_by_iteration(box, bst);
    } else {
        bt_ret = BITPUNCH_OK;
    }
    if (BITPUNCH_OK == bt_ret && NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return bt_ret;
}

bitpunch_status_t
box_get_n_items__array_non_slack(struct box *box, int64_t *item_countp,
                                 struct browse_state *bst)
{
    struct filter_instance_array *array;
    expr_value_t item_count;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        bitpunch_status_t bt_ret;

        array = (struct filter_instance_array *)
            box->filter->ndat->u.rexpr_filter.f_instance;
        assert(0 != (array->item_count->ndat->u.rexpr.value_type_mask
                     & EXPR_VALUE_TYPE_INTEGER));
        bt_ret = expr_evaluate_value_internal(array->item_count, box,
                                              &item_count, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        if (EXPR_VALUE_TYPE_INTEGER != item_count.type) {
            return box_error(
                BITPUNCH_DATA_ERROR, box, array->item_count, bst,
                "evaluation of array items count returned a value-type "
                "'%s', expect an integer",
                expr_value_type_str(item_count.type));
        }
        if (item_count.integer < 0) {
            return box_error(
                BITPUNCH_DATA_ERROR, box, array->item_count, bst,
                "evaluation of array items count gives "
                "negative value (%"PRIi64")", item_count.integer);
        }
        box->u.array_generic.n_items = item_count.integer;
    }
    if (NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_get_n_items__array_non_slack_with_last(struct box *box,
                                           int64_t *item_countp,
                                           struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t n_items;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        // start by evaluating array size
        bt_ret = box_get_n_items__array_non_slack(box, &n_items, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = box_compute_n_items_by_iteration(box, bst);
    } else {
        bt_ret = BITPUNCH_OK;
    }
    if (BITPUNCH_OK == bt_ret && NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return bt_ret;
}


static bitpunch_status_t
box_get_n_items__array_slack_const_item_size(struct box *box,
                                              int64_t *item_countp,
                                              struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    const struct ast_node_hdl *node;
    struct ast_node_hdl *item_type;
    int64_t item_size;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        bt_ret = box_compute_slack_size(box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        assert(-1 != box->start_offset_slack);
        assert(-1 != box->end_offset_slack);
        node = box->filter;
        array = (struct filter_instance_array *)
            node->ndat->u.rexpr_filter.f_instance;
        //TODO optimize by storing the static size in the filter instance
        bt_ret = expr_evaluate_filter_type_internal(
            array->item_type, box, FILTER_KIND_ITEM, &item_type, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        item_size = ast_node_get_min_span_size(item_type);
        if (0 == item_size) {
            return box_error(
                BITPUNCH_DATA_ERROR, box, item_type, bst,
                "slack array only contains items spanning 0 bytes: "
                "the item count cannot be computed");
        }
        /* deduce number of items from the available slack space and
         * the unit elem static size */
        box->u.array_generic.n_items =
            (box->end_offset_slack - box->start_offset_slack) / item_size;
        /* now is a good time to set the used size as well */
        bt_ret = box_set_used_size(
            box, box->u.array_generic.n_items * item_size, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    if (NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return BITPUNCH_OK;
}


bitpunch_status_t
tracker_get_item_key__array_generic(struct tracker *tk,
                                    expr_value_t *keyp,
                                    int *nth_twinp,
                                    struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    assert(-1 != tk->cur.u.array.index);
    if (NULL != keyp) {
        memset(keyp, 0, sizeof(*keyp));
        keyp->type = EXPR_VALUE_TYPE_INTEGER;
        keyp->integer = tk->cur.u.array.index;
    }
    if (NULL != nth_twinp) {
        /* only relevant for indexed arrays */
        *nth_twinp = 0;
    }
    return BITPUNCH_OK;
}


static bitpunch_status_t
tracker_lookup_current_twin_index(struct tracker *tk,
                                  expr_value_t item_key,
                                  struct track_path in_slice_path,
                                  int *nth_twinp,
                                  struct browse_state *bst)

{
    bitpunch_status_t bt_ret;
    const struct ast_node_hdl *node;
    int cur_twin;
    struct tracker *xtk;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_index_cache_lookup_current_twin_index(tk, item_key,
                                                           in_slice_path,
                                                           nth_twinp, bst);
    if (BITPUNCH_NO_ITEM != bt_ret) {
        return bt_ret; /* BITPUNCH_OK included */
    }
    /* Browse the yet-uncached tail */
    node = tk->box->filter;
    xtk = tracker_dup(tk);
    tracker_goto_last_cached_item_internal(xtk, bst);
    cur_twin = *nth_twinp;
    do {
        bt_ret = tracker_goto_next_item_internal(xtk, bst);
        if (BITPUNCH_OK == bt_ret) {
            bt_ret = node->ndat->u.rexpr_filter.f_instance->b_tk.goto_next_key_match(
                xtk, item_key, TRACK_PATH_NONE, bst);
            if (BITPUNCH_OK == bt_ret
                && (-1 == in_slice_path.u.array.index
                    || (xtk->cur.u.array.index
                        >= in_slice_path.u.array.index))) {
                ++cur_twin;
            }
        }
    } while (BITPUNCH_OK == bt_ret && ! track_path_eq(xtk->cur, tk->cur));

    if (BITPUNCH_OK == bt_ret) {
        assert(track_path_eq(xtk->cur, tk->cur));
        *nth_twinp = cur_twin;
    }
    tracker_delete(xtk);
    return bt_ret;
}

bitpunch_status_t
tracker_get_item_key__indexed_array_internal(
    struct tracker *tk,
    int64_t from_index,
    expr_value_t *keyp,
    int *nth_twinp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *filtered_box;
    struct ast_node_hdl *key_expr;
    expr_value_t item_key;

    DBG_TRACKER_DUMP(tk);
    assert(-1 != tk->cur.u.array.index);
    bt_ret = tracker_get_filtered_item_box_internal(tk, &filtered_box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = box_apply_filter_internal(filtered_box, bst);
    if (BITPUNCH_OK == bt_ret) {
        key_expr = ast_node_get_key_expr(tk->box->filter);
        bt_ret = expr_evaluate_value_internal(key_expr, filtered_box,
                                              &item_key, bst);
    }
    box_delete_non_null(filtered_box);
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_tracker_context(
            tk, bst, "when evaluating item key expression");
        return bt_ret;
    }
    if (NULL != nth_twinp) {
        //TODO use hint flag if index is unique to avoid this
        //computation
        bt_ret = tracker_lookup_current_twin_index(
            tk, item_key,
            track_path_from_array_slice(from_index, -1),
            nth_twinp, bst);
        if (BITPUNCH_OK != bt_ret) {
            expr_value_destroy(item_key);
            return bt_ret;
        }
    }
    if (NULL != keyp) {
        *keyp = item_key;
    } else {
        expr_value_destroy(item_key);
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_get_item_key__indexed_array(struct tracker *tk,
                                    expr_value_t *keyp,
                                    int *nth_twinp,
                                    struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_get_item_key__indexed_array_internal(
        tk, 0 /* count twins from first item */,
        keyp, nth_twinp, bst);
}

bitpunch_status_t
tracker_goto_first_item__array_generic(struct tracker *tk,
                                       struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    int64_t n_items;

    DBG_TRACKER_DUMP(tk);
    array = (struct filter_instance_array *)
        tk->box->filter->ndat->u.rexpr_filter.f_instance;
    bt_ret = box_get_n_items_internal(tk->box, &n_items, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != n_items);
    if (0 == n_items) {
        return BITPUNCH_NO_ITEM;
    }
    tk->cur.u.array.index = 0;
    // FIXME reset item to NULL if filter is dynamic
    tk->dpath.filter = array->item_type;
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_first_item__array_slack(struct tracker *tk,
                                     struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    int start_from_box_end;

    DBG_TRACKER_DUMP(tk);
    array = (struct filter_instance_array *)
        tk->box->filter->ndat->u.rexpr_filter.f_instance;
    start_from_box_end =
        (0 != (tk->box->flags & BOX_RALIGN)) ? TRUE : FALSE
        != (0 != (tk->flags & TRACKER_REVERSED)) ? TRUE : FALSE;
    if (start_from_box_end) {
        bt_ret = box_compute_span_size(tk->box, bst);
    } else {
        bt_ret = box_compute_slack_size(tk->box, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    /* slack arrays require a maintained item offset to browse their
     * elements */
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    if (0 != (tk->flags & TRACKER_REVERSED)) {
        if (0 != (tk->box->flags & BOX_RALIGN)) {
            tk->item_offset = tk->box->end_offset_slack;
        } else {
            tk->item_offset = tk->box->end_offset_span;
        }
    } else {
        if (0 != (tk->box->flags & BOX_RALIGN)) {
            tk->item_offset = tk->box->start_offset_span;
        } else {
            tk->item_offset = tk->box->start_offset_slack;
        }
    }
    tk->cur.u.array.index = 0;
    // FIXME reset item to NULL if filter is dynamic
    tk->dpath.filter = array->item_type;
    DBG_TRACKER_CHECK_STATE(tk);

    /* check if there's size for at least one element */
    WITH_EXPECTED_ERROR(BITPUNCH_OUT_OF_BOUNDS_ERROR, {
        bt_ret = tracker_compute_item_location(tk, bst);
    });
    if (BITPUNCH_OK != bt_ret) {
        if (BITPUNCH_OUT_OF_BOUNDS_ERROR == bt_ret) {
            browse_state_clear_error(bst);
            tracker_set_dangling(tk);
            return BITPUNCH_NO_ITEM;
        } else {
            return bt_ret;
        }
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_next_item__array(struct tracker *tk,
                              struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *filtered_box;
    int64_t item_size;
    expr_value_t last_eval;
    int is_last;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_compute_item_filter_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    is_last = FALSE;
    if (0 != (tk->dpath.item->flags & ASTFLAG_CONTAINS_LAST_ATTR)) {
        bt_ret = tracker_get_filtered_item_box_internal(tk, &filtered_box,
                                                        bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = filter_evaluate_attribute_internal(
            filtered_box->filter, filtered_box, "@last",
            NULL, &last_eval, NULL, bst);
        box_delete_non_null(filtered_box);
        switch (bt_ret) {
        case BITPUNCH_OK:
            assert(EXPR_VALUE_TYPE_BOOLEAN == last_eval.type);
            is_last = last_eval.boolean;
            break ;
        case BITPUNCH_NO_ITEM:
            break ;
        default:
            return bt_ret;
        }
    }
    if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        bt_ret = tracker_compute_item_location(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        item_size = tk->item_size;
        tk->item_offset += (0 != (tk->flags & TRACKER_REVERSED) ?
                            -tk->item_size : tk->item_size);
        if (0 != (tk->dpath.item->ndat->u.item.flags
                  & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
            tk->item_size = -1;
        }
    }
    ++tk->cur.u.array.index;
    if ((-1 != tk->box->u.array_generic.n_items
         && tk->cur.u.array.index == tk->box->u.array_generic.n_items)
        || (-1 == tk->box->u.array_generic.n_items
            && (0 != (tk->flags & TRACKER_REVERSED) ?
                tk->item_offset == tk->box->start_offset_slack :
                tk->item_offset == tk->box->end_offset_slack))
        || is_last) {
        bt_ret = tracker_set_end(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        return BITPUNCH_NO_ITEM;
    }
    /* check new item */
    if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)
        && -1 == tk->box->u.array_generic.n_items) {
        WITH_EXPECTED_ERROR(BITPUNCH_OUT_OF_BOUNDS_ERROR, {
            bt_ret = tracker_compute_item_size(tk, bst);
            if (BITPUNCH_OK == bt_ret) {
                bt_ret = tracker_check_item(tk, bst);
            }
        });
    } else {
        bt_ret = tracker_check_item(tk, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        if (BITPUNCH_OUT_OF_BOUNDS_ERROR == bt_ret
            && -1 == tk->box->u.array_generic.n_items) {
            // slack array can't host the additional item, stop here
            bt_ret = tracker_set_end(tk, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
            return BITPUNCH_NO_ITEM;
        }
        /* rollback */
        tracker_reset_item_cache(tk);
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
            tk->item_offset += (0 != (tk->flags & TRACKER_REVERSED) ?
                                item_size : -item_size);
        }
        --tk->cur.u.array.index;
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item__array_const_item_size(struct tracker *tk,
                                              int64_t index,
                                              struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    int64_t n_items;
    int64_t item_size;
    struct tracker *xtk;

    DBG_TRACKER_DUMP(tk);
    bt_ret = box_get_n_items_internal(tk->box, &n_items, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (index >= n_items) {
        return BITPUNCH_NO_ITEM;
    }
    array = (struct filter_instance_array *)
        tk->box->filter->ndat->u.rexpr_filter.f_instance;
    if (0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        tk->cur.u.array.index = index;
        // FIXME reset item to NULL if filter is dynamic
        tk->dpath.filter = array->item_type;
        tk->flags &= ~TRACKER_AT_END;
        DBG_TRACKER_CHECK_STATE(tk);
        return BITPUNCH_OK;
    }
    xtk = tracker_dup(tk);
    // FIXME reset item to NULL if filter is dynamic
    xtk->dpath.filter = array->item_type;
    xtk->flags &= ~TRACKER_AT_END;
    tracker_reset_item_cache(xtk);
    bt_ret = tracker_compute_item_filter_internal(xtk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = box_apply_filter_internal(xtk->box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    item_size = ast_node_get_min_span_size(xtk->dpath.item);
    /* no item cleanup, transform item info instead */
    if (0 != (xtk->flags & TRACKER_REVERSED)) {
        assert(0 != (xtk->box->flags & BOX_RALIGN));
        assert(-1 != xtk->box->end_offset_span);
        xtk->item_offset = xtk->box->end_offset_span
            - (n_items - index - 1) * item_size;
    } else {
        assert(-1 != xtk->box->start_offset_span);
        xtk->item_offset = xtk->box->start_offset_span
            + index * item_size;
    }
    xtk->item_size = item_size;
    xtk->cur.u.array.index = index;
    bt_ret = tracker_check_item(xtk, bst);
    if (BITPUNCH_OK == bt_ret) {
        tracker_set(tk, xtk);
    }
    tracker_delete(xtk);
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item__array_var_item_size(
    struct tracker *tk, int64_t index,
    struct browse_state *bst)
{
    struct tracker *xtk;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    xtk = tracker_dup(tk);
    if (index < tk->box->u.array.last_cached_index) {
        int64_t mark;

        mark = box_array_get_index_mark(xtk->box, index);
        bt_ret = tracker_goto_mark_internal(xtk, mark, bst);
    } else {
        tracker_goto_last_cached_item_internal(xtk, bst);
        if (tracker_is_dangling(xtk)) {
            bt_ret = tracker_goto_next_item_internal(xtk, bst);
            if (BITPUNCH_OK != bt_ret) {
                tracker_delete(xtk);
                return bt_ret;
            }
        }
        bt_ret = BITPUNCH_OK;
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (box_index_cache_exists(xtk->box)) {
        while (TRUE) {
            if (xtk->cur.u.array.index
                == xtk->box->u.array.last_cached_index + 1) {
                expr_value_t index_key;

                bt_ret = tracker_get_item_key_internal(xtk, &index_key, bst);
                if (BITPUNCH_OK != bt_ret) {
                    break ;
                }
                bt_ret = tracker_index_cache_add_item(xtk, index_key, bst);
                expr_value_destroy(index_key);
                if (BITPUNCH_OK != bt_ret) {
                    break ;
                }
            }
            if (xtk->cur.u.array.index == index) {
                break ;
            }
            bt_ret = tracker_goto_next_item_internal(xtk, bst);
            if (BITPUNCH_OK != bt_ret) {
                break ;
            }
        }
    } else {
        while (TRUE) {
            expr_value_t dummy;

            if (xtk->cur.u.array.index
                == xtk->box->u.array.last_cached_index + 1) {
                bt_ret = tracker_index_cache_add_item(xtk, dummy, bst);
                if (BITPUNCH_OK != bt_ret) {
                    break ;
                }
            }
            if (xtk->cur.u.array.index == index) {
                break ;
            }
            bt_ret = tracker_goto_next_item_internal(xtk, bst);
            if (BITPUNCH_OK != bt_ret) {
                break ;
            }
        }
    }
    if (BITPUNCH_OK == bt_ret) {
        tracker_set(tk, xtk);
        tracker_delete(xtk);
        DBG_TRACKER_CHECK_STATE(tk);
    } else {
        tracker_delete(xtk);
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item__array_non_slack_var_item_size(
    struct tracker *tk, int64_t index,
    struct browse_state *bst)
{
    struct filter_instance_array *array;
    struct ast_node_hdl *item_type;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    bt_ret = box_get_n_items_internal(tk->box, NULL, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != tk->box->u.array_generic.n_items);
    if (index >= tk->box->u.array_generic.n_items) {
        return BITPUNCH_NO_ITEM;
    }
    array = (struct filter_instance_array *)
        tk->box->filter->ndat->u.rexpr_filter.f_instance;
    item_type = array->item_type;
    if (0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        // FIXME reset item to NULL if filter is dynamic
        tk->dpath.filter = item_type;
        tk->dpath.item = item_type;
        tk->flags &= ~TRACKER_AT_END;
        tk->cur.u.array.index = index;
        DBG_TRACKER_CHECK_STATE(tk);
        return BITPUNCH_OK;
    }

    return tracker_goto_nth_item__array_var_item_size(tk, index, bst);
}

static bitpunch_status_t
tracker_goto_next_key_match__array(struct tracker *tk,
                                   expr_value_t key,
                                   struct track_path search_boundary,
                                   struct browse_state *bst)
{
    struct filter_instance_array *array;
    const struct ast_node_hdl *item_type;
    struct ast_node_hdl *key_expr;
    bitpunch_status_t bt_ret;
    expr_value_t item_key;
    struct box *filtered_box;

    DBG_TRACKER_DUMP(tk);
    assert(AST_NODE_TYPE_ARRAY == tk->box->filter->ndat->type);
    array = (struct filter_instance_array *)
        tk->box->filter->ndat->u.rexpr_filter.f_instance;
    item_type = ast_node_get_as_type(array->item_type);
    if (!ast_node_is_filter(item_type)) {
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, item_type, bst,
                             "only arrays which items are structures can "
                             "be accessed through named index");
    }
    key_expr = ast_node_get_key_expr(tk->box->filter);
    if (NULL == key_expr) {
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, key_expr, bst,
                             "array is not indexed");
    }
    while (search_boundary.type == TRACK_PATH_NOTYPE
           || ! track_path_eq(tk->cur, search_boundary)) {
        bt_ret = tracker_get_filtered_item_box_internal(tk, &filtered_box,
                                                        bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = box_apply_filter_internal(filtered_box, bst);
        if (BITPUNCH_OK == bt_ret) {
            bt_ret = expr_evaluate_value_internal(key_expr, filtered_box,
                                                  &item_key, bst);
        }
        box_delete_non_null(filtered_box);
        if (BITPUNCH_OK != bt_ret) {
            tracker_error_add_tracker_context(
                tk, bst, "when evaluating item key expression");
            expr_value_destroy(item_key);
            return bt_ret;
        }
        if (tk->cur.u.array.index ==
            tk->box->u.array.last_cached_index + 1) {
            tracker_index_cache_add_item(tk, item_key, bst);
        }
        if (0 == expr_value_cmp(item_key, key)) {
            expr_value_destroy(item_key);
            return BITPUNCH_OK;
        }
        expr_value_destroy(item_key);
        bt_ret = tracker_goto_next_item_internal(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    return BITPUNCH_NO_ITEM;
}

bitpunch_status_t
tracker_goto_next_item_with_key__indexed_array_internal(
    struct tracker *tk,
    expr_value_t item_key,
    int64_t end_index,
    struct browse_state *bst)
{
    struct index_cache_iterator twin_iter;
    bitpunch_status_t bt_ret;
    struct track_path in_slice_path;
    struct track_path item_path;
    struct tracker *xtk;
    const struct ast_node_hdl *node;
 
    DBG_TRACKER_DUMP(tk);
    assert(box_index_cache_exists(tk->box));

    if (tk->cur.u.array.index < tk->box->u.array.last_cached_index) {
        in_slice_path = track_path_from_array_slice(
            tk->cur.u.array.index + 1, end_index);
        bt_ret = box_index_cache_lookup_key_twins(
            tk->box, item_key, in_slice_path, &twin_iter, bst);
        if (BITPUNCH_OK == bt_ret) {
            bt_ret = index_cache_iterator_next_twin(&twin_iter, &item_path,
                                                    bst);
        }
        if (BITPUNCH_OK == bt_ret) {
            tracker_set(tk, twin_iter.xtk);
        }
        if (BITPUNCH_NO_ITEM != bt_ret) {
            index_cache_iterator_done(&twin_iter);
            return bt_ret;
        }
        index_cache_iterator_done(&twin_iter);
    }
    /* Browse the yet-uncached tail */
    node = tk->box->filter;
    xtk = tracker_dup(tk);
    tracker_goto_last_cached_item_internal(xtk, bst);
    if (-1 != end_index && xtk->cur.u.array.index >= end_index) {
        bt_ret = BITPUNCH_NO_ITEM;
    } else {
        bt_ret = tracker_goto_next_item_internal(xtk, bst);
        if (BITPUNCH_OK == bt_ret) {
            struct track_path end_path;

            if (-1 != end_index) {
                end_path = track_path_from_array_index(end_index);
            } else {
                end_path = TRACK_PATH_NONE;
            }
            bt_ret = node->ndat->u.rexpr_filter.f_instance->b_tk.goto_next_key_match(
                xtk, item_key, end_path, bst);
        }
        if (BITPUNCH_OK == bt_ret) {
            tracker_set(tk, xtk);
        }
    }
    tracker_delete(xtk);
    return bt_ret;
}

bitpunch_status_t
tracker_goto_nth_item_with_key__indexed_array_internal(
    struct tracker *tk,
    expr_value_t item_key,
    int nth_twin,
    int64_t from_index,
    int64_t end_index,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int cur_twin;
    struct tracker *xtk;
    const struct ast_node_hdl *node;

    DBG_TRACKER_DUMP(tk);
    //TODO use from_index and end_index
    assert(box_index_cache_exists(tk->box));

    bt_ret = tracker_index_cache_goto_twin(
        tk, item_key, nth_twin,
        track_path_from_array_slice(from_index, end_index),
        &cur_twin, bst);
    if (BITPUNCH_NO_ITEM != bt_ret) {
        return bt_ret; /* BITPUNCH_OK included */
    }
    /* Browse the yet-uncached tail */
    node = tk->box->filter;
    xtk = tracker_dup(tk);
    tracker_goto_last_cached_item_internal(xtk, bst);
    if (-1 != end_index && xtk->cur.u.array.index >= end_index) {
        bt_ret = BITPUNCH_NO_ITEM;
    } else {
        struct track_path end_path;

        if (-1 != end_index) {
            end_path = track_path_from_array_index(end_index);
        } else {
            end_path = TRACK_PATH_NONE;
        }
        do {
            bt_ret = tracker_goto_next_item_internal(xtk, bst);
            if (BITPUNCH_OK == bt_ret) {
                bt_ret = node->ndat->u.rexpr_filter.f_instance->b_tk.goto_next_key_match(
                    xtk, item_key, end_path, bst);
            }
            if (xtk->cur.u.array.index >= from_index) {
                ++cur_twin;
            }
        } while (BITPUNCH_OK == bt_ret && cur_twin != nth_twin);
    }

    if (BITPUNCH_OK == bt_ret) {
        tracker_set(tk, xtk);
    }
    tracker_delete(xtk);
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_next_item_with_key__indexed_array(struct tracker *tk,
                                               expr_value_t item_key,
                                               struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_goto_next_item_with_key__indexed_array_internal(
        tk, item_key, -1 /* no end boundary */, bst);
}

static bitpunch_status_t
tracker_goto_nth_item_with_key__indexed_array(
    struct tracker *tk, expr_value_t item_key, int nth_twin,
    struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_goto_nth_item_with_key__indexed_array_internal(
        tk, item_key, nth_twin,
        0 /* twin count starts at first item */,
        -1 /* no end boundary */,
        bst);
}


static bitpunch_status_t
tracker_goto_named_item__array(struct tracker *tk, const char *name,
                               struct browse_state *bst)
{
    expr_value_t item_key;

    DBG_TRACKER_DUMP(tk);
    memset(&item_key, 0, sizeof(item_key));
    item_key.string.str = name;
    item_key.string.len = strlen(name);
    return tracker_goto_first_item_with_key_internal(tk, item_key, bst);
}

bitpunch_status_t
tracker_goto_end_path__array_generic(struct tracker *tk,
                                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t n_items;

    bt_ret = box_get_n_items_internal(tk->box, &n_items, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    tk->cur = track_path_from_array_index(n_items);
    return BITPUNCH_OK;
}

void
tracker_goto_nil__array_generic(struct tracker *tk)
{
    tk->cur = track_path_from_array_index(-1);
}


static void
compile_node_backends__item__array(struct ast_node_hdl *item)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    struct ast_node_hdl_array item_types;
    const struct ast_node_hdl *item_type;
    struct item_backend *b_item;

    compile_node_backends__item__generic(item);

    array = (struct filter_instance_array *)
        item->ndat->u.rexpr_filter.f_instance;
    // FIXME we should take into account whether there is one or more
    // types of items, and test
    bt_ret = ast_node_filter_get_items(array->item_type, &item_types);
    assert(BITPUNCH_OK == bt_ret);
    assert(ARRAY_SIZE(&item_types) >= 1);
    item_type = ARRAY_ITEM(&item_types, 0);
    ast_node_hdl_array_destroy(&item_types);

    b_item = &array->filter.b_item;
    if (NULL == b_item->compute_item_size) {
        if (NULL != array->item_count
            && 0 == (item_type->ndat->u.item.flags
                     & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
            b_item->compute_item_size =
                compute_item_size__array_const_item_size;
        }
    }
}

static void
compile_node_backends__box__array(struct ast_node_hdl *item)
{
    struct filter_instance_array *array;
    struct box_backend *b_box = NULL;
    bitpunch_status_t bt_ret;
    struct ast_node_hdl_array item_types;
    const struct ast_node_hdl *item_type;

    array = (struct filter_instance_array *)
        item->ndat->u.rexpr_filter.f_instance;
    // FIXME we should take into account whether there is one or more
    // types of items, and test
    bt_ret = ast_node_filter_get_items(array->item_type, &item_types);
    assert(BITPUNCH_OK == bt_ret);
    assert(ARRAY_SIZE(&item_types) >= 1);
    item_type = ARRAY_ITEM(&item_types, 0);
    ast_node_hdl_array_destroy(&item_types);
    b_box = &array->filter.b_box;
    memset(b_box, 0, sizeof (*b_box));

    b_box->init = array_box_init;
    b_box->destroy = array_box_destroy;
    b_box->compute_slack_size = box_compute_slack_size__as_container_slack;
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->compute_max_span_size = box_compute_max_span_size__as_slack;
    if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
        b_box->compute_span_size = box_compute_span_size__const_size;
    } else if (0 == (item_type->ndat->u.item.flags
                     & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
        b_box->compute_span_size = box_compute_span_size__array_const_item_size;
    } else {
        b_box->compute_span_size = box_compute_span_size__packed_var_size;
    }
    if (0 != (item_type->flags & ASTFLAG_CONTAINS_LAST_ATTR)) {
        if (NULL != array->item_count) {
            b_box->get_n_items = box_get_n_items__array_non_slack_with_last;
        } else {
            b_box->get_n_items = box_get_n_items__by_iteration;
        }
    } else if (NULL != array->item_count) {
        b_box->get_n_items = box_get_n_items__array_non_slack;
    } else if (0 == (item_type->ndat->u.item.flags
                     & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
        b_box->get_n_items = box_get_n_items__array_slack_const_item_size;
    } else {
        b_box->get_n_items = box_get_n_items__by_iteration;
    }
    b_box->compute_used_size = box_compute_used_size__as_span;
}

static void
compile_node_backends__tracker__array(struct ast_node_hdl *item)
{
    struct filter_instance_array *array;
    struct item_backend *b_item;
    struct tracker_backend *b_tk;
    bitpunch_status_t bt_ret;
    struct ast_node_hdl_array item_types;
    const struct ast_node_hdl *item_type;

    array = (struct filter_instance_array *)
        item->ndat->u.rexpr_filter.f_instance;
    // FIXME we should take into account whether there is one or more
    // types of items, and test
    bt_ret = ast_node_filter_get_items(array->item_type, &item_types);
    assert(BITPUNCH_OK == bt_ret);
    assert(ARRAY_SIZE(&item_types) >= 1);
    item_type = ARRAY_ITEM(&item_types, 0);
    ast_node_hdl_array_destroy(&item_types);
    b_item = &array->filter.b_item;
    b_tk = &array->filter.b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

    if (NULL != ast_node_get_key_expr(item)) {
        b_tk->get_item_key = tracker_get_item_key__indexed_array;
    } else {
        b_tk->get_item_key = tracker_get_item_key__array_generic;
    }
    if (NULL == b_item->compute_item_size) {
        b_tk->compute_item_size = tracker_compute_item_size__item_box;
    }
    if (NULL != array->item_count) {
        b_tk->goto_first_item = tracker_goto_first_item__array_generic;
    } else {
        b_tk->goto_first_item = tracker_goto_first_item__array_slack;
    }
    b_tk->goto_next_item = tracker_goto_next_item__array;
    if (0 == (item_type->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
        b_tk->goto_nth_item = tracker_goto_nth_item__array_const_item_size;
    } else if (NULL != array->item_count) {
        b_tk->goto_nth_item =
            tracker_goto_nth_item__array_non_slack_var_item_size;
    } else {
        b_tk->goto_nth_item = tracker_goto_nth_item__array_var_item_size;
    }
    b_tk->goto_named_item = tracker_goto_named_item__array;
    b_tk->goto_next_key_match = tracker_goto_next_key_match__array;
    if (ast_node_is_indexed(item)) {
        b_tk->goto_next_item_with_key =
            tracker_goto_next_item_with_key__indexed_array;
        b_tk->goto_nth_item_with_key =
            tracker_goto_nth_item_with_key__indexed_array;
    } else {
        b_tk->goto_next_item_with_key =
            tracker_goto_next_item_with_key__default;
        b_tk->goto_nth_item_with_key =
            tracker_goto_nth_item_with_key__default;
    }
    b_tk->goto_end_path = tracker_goto_end_path__array_generic;
    b_tk->goto_nil = tracker_goto_nil__array_generic;
}

static void
compile_node_backends__array(struct ast_node_hdl *item)
{
    compile_node_backends__item__array(item);
    compile_node_backends__box__array(item);
    compile_node_backends__tracker__array(item);
}

static int
compile_node_backends_array(struct ast_node_hdl *item,
                            struct filter_instance_array *array,
                            struct compile_ctx *ctx)
{
    if (-1 == compile_node(array->item_type, ctx,
                           COMPILE_TAG_NODE_TYPE |
                           COMPILE_TAG_NODE_SPAN_SIZE,
                           COMPILE_TAG_BROWSE_BACKENDS,
                           RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_FILTER)) {
        return -1;
    }
    switch (item->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
        compile_node_backends__array(item);
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
        compile_node_backends__byte_array(item);
        break ;
    default:
        assert(0);
    }
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
    if (0 != (tags & COMPILE_TAG_NODE_SPAN_SIZE)
        && -1 == compile_span_size_array(filter, array, ctx)) {
        return -1;
    }
    if (0 != (tags & COMPILE_TAG_BROWSE_BACKENDS)
        && -1 == compile_node_backends_array(filter, array, ctx)) {
        return -1;
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
