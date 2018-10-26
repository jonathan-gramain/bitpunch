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
#include "filters/byte.h"
#include "filters/byte_array.h"


bitpunch_status_t
byte_array_box_init(struct box *box, struct browse_state *bst)
{
    box->u.array_generic.n_items = -1;
    return BITPUNCH_OK;
}

void
byte_array_box_destroy(struct box *box)
{
}

static bitpunch_status_t
compute_item_size__byte_array_var_size(
    struct ast_node_hdl *item_filter,
    int64_t item_offset, int64_t max_span_offset,
    int64_t *item_sizep, struct browse_state *bst)
{
    struct filter_instance_array *array;
    expr_value_t byte_count;
    bitpunch_status_t bt_ret;

    array = (struct filter_instance_array *)
        item_filter->ndat->u.rexpr_filter.f_instance;
    bt_ret = expr_evaluate_value_internal(
        array->item_count, &byte_count, bst);
    if (BITPUNCH_OK != bt_ret) {
        // FIXME more appropriate context
        tracker_error_add_box_context(
            bst->scope, bst, "when evaluating byte array size expression");
        return bt_ret;
    }
    if (EXPR_VALUE_TYPE_INTEGER != byte_count.type) {
        return box_error(
            BITPUNCH_DATA_ERROR, bst->scope, array->item_count, bst,
            "evaluation of byte array size returned a value-type "
            "'%s', expect an integer",
            expr_value_type_str(byte_count.type));
    }
    if (byte_count.integer < 0) {
        return box_error(
            BITPUNCH_DATA_ERROR, bst->scope, array->item_count, bst,
            "evaluation of byte array size gives negative value (%"PRIi64")",
            byte_count.integer);
    }
    *item_sizep = byte_count.integer;
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_span_size__byte_array_var_size(
    struct box *box, struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t used_size;

    DBG_BOX_DUMP(box);
    bt_ret = box_get_n_items_internal(box, &used_size, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_span_size(box, used_size, bst);
}

static bitpunch_status_t
box_get_n_items__byte_array_slack(struct box *box, int64_t *item_countp,
                                  struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        bt_ret = box_compute_slack_size(box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        assert(-1 != box->end_offset_slack);
        /* deduce size from the available slack space */
        box->u.array_generic.n_items =
            box->end_offset_slack - box->start_offset_span;
        /* now is a good time to set the used size as well */
        bt_ret = box_set_used_size(box, box->u.array_generic.n_items, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    if (NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return BITPUNCH_OK;
}



static bitpunch_status_t
tracker_goto_first_item__byte_array_slack(struct tracker *tk,
                                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    bt_ret = box_compute_slack_size(tk->box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    /* check if there's size for at least one element */
    if (tk->box->start_offset_slack == tk->box->end_offset_slack) {
        return BITPUNCH_NO_ITEM;
    }
    /* slack arrays require a maintained item offset to browse their
     * elements */
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    bt_ret = tracker_set_item_offset_at_box(tk, tk->box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    tk->item_size = 1;
    tk->cur = track_path_from_array_index(0);
    tk->dpath.filter = filter_get_global_instance__byte();
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_next_item__byte_array_generic(struct tracker *tk,
                                           struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    ++tk->cur.u.array.index;
    if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        tk->item_offset += 1;
    }
    if ((-1 != tk->box->u.array_generic.n_items
         && tk->cur.u.array.index == tk->box->u.array_generic.n_items)
        || (-1 == tk->box->u.array_generic.n_items
            && tk->item_offset == tk->box->end_offset_slack)) {
        bt_ret = tracker_set_end(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        return BITPUNCH_NO_ITEM;
    }
    /* check new item */
    tk->dpath.filter = filter_get_global_instance__byte();
    bt_ret = tracker_check_item(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        /* rollback */
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
            tk->item_offset -= 1;
        }
        --tk->cur.u.array.index;
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item__byte_array_generic(
    struct tracker *tk, int64_t index,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t n_items;

    DBG_TRACKER_DUMP(tk);
    if (0 != (tk->flags & TRACKER_REVERSED) ||
        0 != (tk->box->flags & BOX_RALIGN)) {
        return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst,
                             "tracker_goto_nth_item() not implemented "
                             "in reverse mode on byte arrays");
    }
    bt_ret = box_get_n_items_internal(tk->box, &n_items, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != n_items);
    if (index >= n_items) {
        return BITPUNCH_NO_ITEM;
    }
    tk->flags &= ~TRACKER_AT_END;
    tk->dpath.filter = filter_get_global_instance__byte();
    if (0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        tk->cur = track_path_from_array_index(index);
        DBG_TRACKER_CHECK_STATE(tk);
        return BITPUNCH_OK;
    }
    assert(-1 != tk->box->start_offset_used);
    tk->item_offset = tk->box->start_offset_used + index;
    tk->item_size = 1;
    tk->cur = track_path_from_array_index(index);
    return tracker_check_item(tk, bst);
}


static void
compile_node_backends__box__byte_array(struct ast_node_hdl *item)
{
    struct filter_instance_array *array;
    struct box_backend *b_box = NULL;

    array = (struct filter_instance_array *)
        item->ndat->u.rexpr_filter.f_instance;
    b_box = &array->filter.b_box;
    memset(b_box, 0, sizeof (*b_box));

    b_box->init = byte_array_box_init;
    b_box->destroy = byte_array_box_destroy;
    b_box->compute_slack_size = box_compute_slack_size__as_container_slack;
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
        b_box->compute_span_size = box_compute_span_size__const_size;
        b_box->compute_max_span_size = box_compute_max_span_size__as_span;
    } else {
        if (0 != (item->ndat->u.item.flags & ITEMFLAG_FILLS_SLACK)) {
            b_box->compute_span_size = box_compute_span_size__as_max_span;
        } else {
            b_box->compute_span_size =
                box_compute_span_size__byte_array_var_size;
        }
        b_box->compute_max_span_size = box_compute_max_span_size__as_slack;
    }
    if (NULL != array->item_count) {
        b_box->get_n_items = box_get_n_items__array_non_slack;
    } else {
        b_box->get_n_items = box_get_n_items__byte_array_slack;
    }
    b_box->compute_used_size = box_compute_used_size__as_span;
}

static void
compile_node_backends__item__byte_array(struct ast_node_hdl *item)
{
    struct filter_instance_array *array;
    struct item_backend *b_item;

    compile_node_backends__item__generic(item);

    array = (struct filter_instance_array *)
        item->ndat->u.rexpr_filter.f_instance;
    b_item = &array->filter.b_item;
    if (NULL == b_item->compute_item_size) {
        if (NULL != array->item_count) {
            b_item->compute_item_size = compute_item_size__byte_array_var_size;
        }
    }
}

static void
compile_node_backends__tracker__byte_array(struct ast_node_hdl *item)
{
    struct filter_instance_array *array;
    struct tracker_backend *b_tk;

    array = (struct filter_instance_array *)
        item->ndat->u.rexpr_filter.f_instance;
    b_tk = &array->filter.b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

    b_tk->get_item_key = tracker_get_item_key__array_generic;
    if (NULL != array->item_count) {
        b_tk->goto_first_item = tracker_goto_first_item__array_generic;
    } else {
        b_tk->goto_first_item = tracker_goto_first_item__byte_array_slack;
    }
    b_tk->goto_next_item = tracker_goto_next_item__byte_array_generic;
    b_tk->goto_nth_item = tracker_goto_nth_item__byte_array_generic;
    b_tk->goto_next_item_with_key = tracker_goto_next_item_with_key__default;
    b_tk->goto_nth_item_with_key = tracker_goto_nth_item_with_key__default;
    b_tk->goto_end_path = tracker_goto_end_path__array_generic;
    b_tk->goto_nil = tracker_goto_nil__array_generic;
}


void
compile_node_backends__byte_array(struct ast_node_hdl *item)
{
  compile_node_backends__item__byte_array(item);
  compile_node_backends__box__byte_array(item);
  compile_node_backends__tracker__byte_array(item);
}
