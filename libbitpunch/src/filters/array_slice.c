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
#include "filters/array_slice.h"
#include "filters/byte_slice.h"

struct ast_node_data shared_ast_node_data_array_slice = {
    .type = AST_NODE_TYPE_ARRAY_SLICE,
};
struct ast_node_hdl shared_ast_node_array_slice = {
    .ndat = &shared_ast_node_data_array_slice,
};

#define AST_NODE_ARRAY_SLICE &shared_ast_node_array_slice

struct box *
box_array_slice_get_ancestor_array(struct box *box)
{
    struct box *array_box;

    DBG_BOX_DUMP(box);
    array_box = box;
    while (AST_NODE_TYPE_ARRAY != array_box->filter->ndat->type &&
           AST_NODE_TYPE_BYTE_ARRAY != array_box->filter->ndat->type &&
           AST_NODE_TYPE_SOURCE != array_box->filter->ndat->type) {
        array_box = array_box->parent_box;
        /* An array slice box shall always have a real array ancestor. */
        assert(NULL != array_box);
    }
    return array_box;
}

struct box *
box_new_slice_box(struct tracker *slice_start,
                  struct tracker *slice_end,
                  struct browse_state *bst)
{
    struct box *slice_box;
    bitpunch_status_t bt_ret;
    struct ast_node_hdl *slice_filter;
    int64_t index_start;
    int64_t index_end;
    struct track_path slice_path;
    int64_t slice_start_offset_span;

    if (NULL != slice_end && slice_start->box != slice_end->box) {
        (void) tracker_error(BITPUNCH_INVALID_PARAM, slice_start, NULL, bst,
                             "can't set tracker slice: end of slice "
                             "tracker must track the same box");
        return NULL;
    }

    switch (slice_start->box->filter->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
        slice_filter = AST_NODE_ARRAY_SLICE;
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_REXPR_FILTER:
    case AST_NODE_TYPE_SOURCE:
        slice_filter = filter_get_global_instance__byte_slice();
        break ;
    default:
        (void) tracker_error(BITPUNCH_INVALID_PARAM, slice_start, NULL, bst,
                             "can't set tracker slice: does not track "
                             "an array");
        return NULL;
    }
    index_start = slice_start->cur.u.array.index;
    if (-1 == index_start) {
        index_start = 0;
    }
    index_end = (NULL != slice_end ? slice_end->cur.u.array.index : -1);
    if (index_end != -1 && index_end < index_start) {
        (void) tracker_error(BITPUNCH_DATA_ERROR, slice_start, NULL,
                             bst,
                             "slice end index %"PRIi64" before "
                             "start index %"PRIi64"",
                             index_end, index_start);
        return NULL;
    }

    // FIXME this may not work for RALIGN track path
    slice_path = track_path_from_array_slice(index_start, index_end);
    if (-1 != slice_start->cur.u.array.index) {
        if (-1 == slice_start->item_offset) {
            bt_ret = tracker_compute_item_offset(slice_start, bst);
            if (BITPUNCH_OK != bt_ret) {
                return NULL;
            }
            assert(slice_start->item_offset >= 0);
        }
        slice_start_offset_span = slice_start->item_offset;
    } else {
        slice_start_offset_span = slice_start->box->start_offset_span;
    }
    slice_box = new_safe(struct box);
    bt_ret = box_construct(slice_box,
                           slice_start->box, slice_filter,
                           slice_start_offset_span, 0u, bst);
    if (BITPUNCH_OK != bt_ret) {
        box_delete_non_null(slice_box);
        DBG_TRACKER_CHECK_STATE(slice_start);
        return NULL;
    }
    slice_box->track_path = slice_path;
    if (0 != (TRACKER_AT_END & slice_start->flags)) {
        slice_box->u.array_generic.n_items = 0;
    }
    return slice_box;
}

static bitpunch_status_t
box_compute_span_size__array_slice(struct box *box,
                                   struct browse_state *bst)
{
    struct filter_instance_array *array;
    struct box *array_box;
    const struct ast_node_hdl *array_node;
    struct ast_node_hdl *item_type;
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    array_box = box_array_slice_get_ancestor_array(box);
    array_node = array_box->filter;
    array = (struct filter_instance_array *)
        array_node->ndat->u.rexpr_filter.f_instance;
    bt_ret = expr_evaluate_filter_type_internal(
        array->item_type, box, FILTER_KIND_ITEM, &item_type, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (0 == (item_type->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
        int64_t n_elems;
        int64_t item_size;

        bt_ret = box_get_n_items__slice_generic(box, &n_elems, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        item_size = ast_node_get_min_span_size(item_type);
        bt_ret = box_set_span_size(box, n_elems * item_size, bst);
    } else {
        /* Default implementation of dynamic sized arrays works by
         * tracking contents, which has a custom implementation for
         * array slices. */
        bt_ret = array_node->ndat->u.rexpr_filter.f_instance->b_box.compute_span_size(box, bst);
    }
    return bt_ret;
}

bitpunch_status_t
box_get_n_items__slice_generic(struct box *box, int64_t *item_countp,
                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t array_n_items;
    int64_t index_start;
    int64_t index_end;
    int64_t item_count;

    DBG_BOX_DUMP(box);
    assert(box->track_path.type == TRACK_PATH_ARRAY_SLICE);
    if (-1 == box->u.array_generic.n_items) {
        bt_ret = box_get_n_items_internal(box->parent_box, &array_n_items, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        index_start = box->track_path.u.array.index;
        index_end = box->track_path.u.array_slice.index_end;
        if (-1 == index_end || index_end > array_n_items) {
            index_end = array_n_items;
        }
        if (-1 == index_start) {
            index_start = 0;
        } else if (index_start > index_end) {
            index_start = index_end;
        }
        item_count = index_end - index_start;
        box->u.array_generic.n_items = item_count;
    }
    if (NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
tracker_get_item_key__array_slice(struct tracker *tk,
                                  expr_value_t *keyp,
                                  int *nth_twinp,
                                  struct browse_state *bst)
{
    struct box *slice_box;
    struct box *array_box;
    bitpunch_status_t bt_ret;
    int64_t from_index;

    DBG_TRACKER_DUMP(tk);
    slice_box = tk->box;
    array_box = box_array_slice_get_ancestor_array(slice_box);
    tk->box = array_box;
    from_index = slice_box->track_path.u.array.index;
    if (-1 == from_index) {
        from_index = 0;
    }
    if (ast_node_is_indexed(array_box->filter)) {
        bt_ret = tracker_get_item_key__indexed_array_internal(
            tk, from_index, keyp, nth_twinp, bst);
    } else {
        if (NULL != keyp) {
            assert(tk->cur.u.array.index >= from_index);
            *keyp = expr_value_as_integer(tk->cur.u.array.index - from_index);
        }
        if (NULL != nth_twinp) {
            /* only relevant for indexed arrays */
            *nth_twinp = 0;
        }
        bt_ret = BITPUNCH_OK;
    }
    tk->box = slice_box;
    return bt_ret;
}

bitpunch_status_t
tracker_goto_first_item__array_slice(struct tracker *tk,
                                     struct browse_state *bst)
{
    struct box *slice_box;
    struct box *array_box;
    bitpunch_status_t bt_ret;
    int index;

    DBG_TRACKER_DUMP(tk);
    slice_box = tk->box;
    if (-1 != slice_box->track_path.u.array.index
        && (slice_box->track_path.u.array.index
            == slice_box->track_path.u.array_slice.index_end)) {
        tk->cur = slice_box->track_path;
        tk->item_offset = slice_box->start_offset_span;
        bt_ret = tracker_set_end(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        return BITPUNCH_NO_ITEM;
    }
    array_box = box_array_slice_get_ancestor_array(slice_box);
    tk->box = array_box;
    index = slice_box->track_path.u.array.index;
    if (-1 == index) {
        index = 0;
    }
    bt_ret = array_box->filter->ndat->u.rexpr_filter.f_instance->b_tk.goto_nth_item(tk, index, bst);
    tk->box = slice_box;
    return bt_ret;
}

bitpunch_status_t
tracker_goto_next_item__array_slice(struct tracker *tk,
                                    struct browse_state *bst)
{
    struct box *slice_box;
    struct box *array_box;
    bitpunch_status_t bt_ret;
    int64_t index_end;

    DBG_TRACKER_DUMP(tk);
    if (0 != (tk->flags & TRACKER_REVERSED) ||
        0 != (tk->box->flags & BOX_RALIGN)) {
        return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst,
                             "tracker_goto_next_item() not implemented "
                             "in reverse mode on array slices");
    }
    slice_box = tk->box;
    index_end = slice_box->track_path.u.array_slice.index_end;
    if (tk->cur.u.array.index + 1 == index_end) {
        bt_ret = tracker_compute_item_location(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        ++tk->cur.u.array.index;
        tk->item_offset += tk->item_size;
        bt_ret = tracker_set_end(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        return BITPUNCH_NO_ITEM;
    }
    array_box = box_array_slice_get_ancestor_array(slice_box);
    tk->box = array_box;
    bt_ret = array_box->filter->ndat->u.rexpr_filter.f_instance->b_tk.goto_next_item(tk, bst);
    tk->box = slice_box;
    return bt_ret;
}

bitpunch_status_t
tracker_goto_nth_item__array_slice(struct tracker *tk, int64_t index,
                                   struct browse_state *bst)
{
    int64_t index_start;
    int64_t index_end;

    DBG_TRACKER_DUMP(tk);
    index_start = tk->box->track_path.u.array.index;
    if (-1 == index_start) {
        index_start = 0;
    }
    index_end = tk->box->track_path.u.array_slice.index_end;
    if ((-1 != index_end && index >= index_end - index_start)) {
        return BITPUNCH_NO_ITEM;
    }
    return tracker_goto_ancestor_array_index_internal(
        tk, index_start + index, bst);
}

bitpunch_status_t
tracker_goto_named_item__array_slice(struct tracker *tk, const char *name,
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
tracker_goto_next_key_match__array_slice(struct tracker *tk,
                                         expr_value_t index,
                                         struct track_path search_boundary,
                                         struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    // should not be called, only used internally by indexed arrays
    return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst, NULL);
}

static bitpunch_status_t
tracker_goto_next_item_with_key__array_slice(struct tracker *tk,
                                             expr_value_t item_key,
                                             struct browse_state *bst)
{
    struct box *slice_box;
    struct box *array_box;
    bitpunch_status_t bt_ret;
    int64_t end_index;

    DBG_TRACKER_DUMP(tk);
    slice_box = tk->box;
    array_box = box_array_slice_get_ancestor_array(slice_box);
    end_index = slice_box->track_path.u.array_slice.index_end;

    tk->box = array_box;
    if (ast_node_is_indexed(array_box->filter)) {
        bt_ret = tracker_goto_next_item_with_key__indexed_array_internal(
            tk, item_key, end_index, bst);
    } else {
        bt_ret = array_box->filter->ndat->u.rexpr_filter.f_instance->b_tk.goto_next_item_with_key(
            tk, item_key, bst);
        if (BITPUNCH_OK == bt_ret && tk->cur.u.array.index == end_index) {
            bt_ret = tracker_set_end(tk, bst);
            if (BITPUNCH_OK == bt_ret) {
                bt_ret = BITPUNCH_NO_ITEM;
            }
        }
    }
    tk->box = slice_box;
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item_with_key__array_slice(
    struct tracker *tk, expr_value_t item_key, int nth_twin,
    struct browse_state *bst)
{
    struct box *slice_box;
    struct box *array_box;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    slice_box = tk->box;
    array_box = box_array_slice_get_ancestor_array(slice_box);

    tk->box = array_box;
    if (ast_node_is_indexed(array_box->filter)) {
        int64_t from_index;
        int64_t end_index;

        from_index = slice_box->track_path.u.array.index;
        if (-1 == from_index) {
            from_index = 0;
        }
        end_index = slice_box->track_path.u.array_slice.index_end;
        bt_ret = tracker_goto_nth_item_with_key__indexed_array_internal(
            tk, item_key, nth_twin, from_index, end_index, bst);
    } else {
        /* this call shall return an error since the function we're in
         * expects the underlying array to be indexed. */
        bt_ret = array_box->filter->ndat->u.rexpr_filter.f_instance->b_tk.goto_nth_item_with_key(
            tk, item_key, nth_twin, bst);
    }
    tk->box = slice_box;
    return bt_ret;
}

bitpunch_status_t
tracker_goto_end_path__array_slice(struct tracker *tk,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t n_items;
    int64_t index_start;

    bt_ret = box_get_n_items_internal(tk->box, &n_items, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    // start of slice relative to parent array
    index_start = tk->box->track_path.u.array.index;
    if (-1 == index_start) {
        index_start = 0;
    }
    // track path from beginning of parent array
    tk->cur = track_path_from_array_index(index_start + n_items);
    return BITPUNCH_OK;
}

static struct filter_instance *
array_slice_filter_instance_build(struct ast_node_hdl *item)
{
    struct filter_instance *slice;
    struct box_backend *b_box;
    struct tracker_backend *b_tk;

    slice = new_safe(struct filter_instance);
    b_box = &slice->b_box;
    b_tk = &slice->b_tk;
    memset(b_box, 0, sizeof (*b_box));
    memset(b_tk, 0, sizeof (*b_tk));

    b_box->compute_slack_size = box_compute_slack_size__from_parent;
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    //XXX should it be b_box->compute_max_span_size =
    //box_compute_max_span_size__as_slack?
    b_box->compute_max_span_size = box_compute_max_span_size__as_span;
    b_box->compute_span_size = box_compute_span_size__array_slice;
    b_box->get_n_items = box_get_n_items__slice_generic;
    b_box->compute_used_size = box_compute_used_size__as_span;

    b_tk->get_item_key = tracker_get_item_key__array_slice;
    b_tk->compute_item_size = tracker_compute_item_size__item_box;
    b_tk->goto_first_item = tracker_goto_first_item__array_slice;
    b_tk->goto_next_item = tracker_goto_next_item__array_slice;
    b_tk->goto_nth_item = tracker_goto_nth_item__array_slice;
    b_tk->goto_named_item = tracker_goto_named_item__array_slice;
    b_tk->goto_next_key_match = tracker_goto_next_key_match__array_slice;
    b_tk->goto_next_item_with_key =
        tracker_goto_next_item_with_key__array_slice;
    b_tk->goto_nth_item_with_key =
        tracker_goto_nth_item_with_key__array_slice;
    b_tk->goto_end_path = tracker_goto_end_path__array_slice;
    b_tk->goto_nil = tracker_goto_nil__array_generic;

    return slice;
}

static bitpunch_status_t
tracker_enter_slice_internal(struct tracker *tk, struct tracker *slice_end,
                             struct browse_state *bst)
{
    struct box *slice_box;

    DBG_TRACKER_DUMP(tk);
    slice_box = box_new_slice_box(tk, slice_end, bst);
    if (NULL == slice_box) {
        return browse_state_get_last_error_status(bst);
    }
    box_delete_non_null(tk->box);
    tk->box = slice_box;
    tracker_set_dangling(tk);
    tk->item_offset = -1;
    return box_apply_filter_internal(tk->box, bst);
}

bitpunch_status_t
tracker_enter_slice(struct tracker *tk, struct tracker *slice_end,
                    struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_enter_slice_internal(tk, slice_end, &bst),
        &bst, errp);
}



static void
compile_node_backends__array_slice(struct ast_node_hdl *filter)
{
    filter->ndat->u.rexpr_filter.f_instance =
        array_slice_filter_instance_build(filter);

    compile_node_backends__filter__filter(filter);
}

int
compile_global_nodes__array_slice(struct compile_ctx *ctx)
{
    compile_node_backends__array_slice(AST_NODE_ARRAY_SLICE);
    return 0;
}
