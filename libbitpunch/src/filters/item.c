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
#include "core/browse_internal.h"
#include "filters/item.h"

bitpunch_status_t
box_compute_used_size__const_size(struct box *box,
                                  struct browse_state *bst)
{
    DBG_BOX_DUMP(box);
    return box_set_used_size(box, box->filter->ndat->u.item.min_span_size, bst);
}


bitpunch_status_t
box_compute_max_span_size__as_span(struct box *box,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    bt_ret = box_compute_span_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != box->start_offset_span && -1 != box->end_offset_span);
    return box_set_size(
        box, box->end_offset_span - box->start_offset_span,
        BOX_SIZE_MAX_SPAN, bst);
}

bitpunch_status_t
box_compute_max_span_size__as_slack(struct box *box,
                                    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    bt_ret = box_compute_slack_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (-1 == box->start_offset_slack || -1 == box->end_offset_slack) {
        return BITPUNCH_OK;
    }
    return box_set_size(
        box, box->end_offset_slack - box->start_offset_slack,
        BOX_SIZE_MAX_SPAN, bst);
}

bitpunch_status_t
box_compute_span_size__as_slack(struct box *box,
                                struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    bt_ret = box_compute_slack_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_size(box, box->end_offset_slack - box->start_offset_slack,
                        BOX_SIZE_SPAN, bst);
}

bitpunch_status_t
box_compute_used_size__as_max_span(struct box *box,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    // FIXME check if this function cannot be replaced by
    // box_compute_span_size__as_slack()
    DBG_BOX_DUMP(box);
    bt_ret = box_compute_max_span_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_size(box, box->end_offset_max_span
                        - box->start_offset_max_span,
                        BOX_SIZE_USED, bst);
}

bitpunch_status_t
box_compute_span_size__from_compute_item_size(
    struct box *box, struct browse_state *bst)
{
    struct item_backend *b_item;
    bitpunch_status_t bt_ret;
    int64_t span_size;

    DBG_BOX_DUMP(box);
    b_item = &box->filter->ndat->u.rexpr_filter.f_instance->b_item;
    bt_ret = box_compute_max_span_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = b_item->compute_item_size(
        box->filter, box,
        box->start_offset_max_span, box->end_offset_max_span,
        &span_size, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_size(box, span_size, BOX_SIZE_SPAN, bst);
}

bitpunch_status_t
box_compute_span_size__from_compute_item_size_from_buffer(
    struct box *box, struct browse_state *bst)
{
    struct item_backend *b_item;
    bitpunch_status_t bt_ret;
    const char *item_data;
    int64_t span_size;

    DBG_BOX_DUMP(box);
    b_item = &box->filter->ndat->u.rexpr_filter.f_instance->b_item;
    bt_ret = box_compute_max_span_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    item_data = box->ds_in->ds_data + box->start_offset_max_span;
    bt_ret = b_item->compute_item_size_from_buffer(
        box->filter, box,
        item_data, box->end_offset_max_span - box->start_offset_max_span,
        &span_size, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_size(box, span_size, BOX_SIZE_SPAN, bst);
}

bitpunch_status_t
box_compute_span_size__span_expr(struct box *box,
                                 struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    const struct named_expr *span_stmt;
    expr_value_t span_size;

    DBG_BOX_DUMP(box);
    bt_ret = filter_evaluate_attribute_internal(
        box->filter, box, "@span", 0u, &span_stmt, &span_size, NULL, bst);
    switch (bt_ret) {
    case BITPUNCH_OK:
        break ;
    case BITPUNCH_NO_ITEM:
        // no variable span enabled by conditional: span is the
        // used size
        return box_compute_span_size__as_used(box, bst);
    default:
        return bt_ret;
    }
    /* TODO: show data path info in errors */
    if (span_size.integer < 0) {
        return box_error(BITPUNCH_DATA_ERROR, box, span_stmt->expr, bst,
                         "evaluation of span size expression gives "
                         "negative value (%"PRIi64")", span_size.integer);
    }
    bt_ret = box_set_span_size(box, span_size.integer, bst);
    return bt_ret;
}

bitpunch_status_t
box_compute_span_size__as_used(struct box *box,
                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    bt_ret = box_compute_used_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_size(box, box->end_offset_used
                        - box->start_offset_used,
                        BOX_SIZE_SPAN, bst);
}

bitpunch_status_t
box_compute_min_span_size__as_hard_min(struct box *box,
                                       struct browse_state *bst)
{
    int64_t hard_min;

    DBG_BOX_DUMP(box);
    hard_min = ast_node_get_min_span_size(box->filter);
    return box_set_size(box, hard_min, BOX_SIZE_MIN_SPAN, bst);
}

bitpunch_status_t
box_compute_min_span_size__span_expr(struct box *box,
                                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    const struct named_expr *span_stmt;
    int span_expr_defines_max;
    expr_value_t span_size;

    DBG_BOX_DUMP(box);
    span_expr_defines_max = FALSE;
    bt_ret = filter_evaluate_attribute_internal(
        box->filter, box, "@minspan", 0u, &span_stmt, &span_size, NULL, bst);
    if (BITPUNCH_NO_ITEM == bt_ret) {
        span_expr_defines_max = TRUE;
        bt_ret = filter_evaluate_attribute_internal(
            box->filter, box, "@span", 0u, &span_stmt, &span_size, NULL, bst);
    }
    switch (bt_ret) {
    case BITPUNCH_OK:
        break ;
    case BITPUNCH_NO_ITEM:
        // no variable span enabled by conditional: min span is the
        // hard minimum
        return box_compute_min_span_size__as_hard_min(box, bst);
    default:
        return bt_ret;
    }
    /* TODO: show data path info in errors */
    if (span_size.integer < 0) {
        return box_error(BITPUNCH_DATA_ERROR, box, span_stmt->expr, bst,
                         "evaluation of span size expression gives "
                         "negative value (%"PRIi64")", span_size.integer);
    }
    bt_ret = box_set_min_span_size(box, span_size.integer, bst);
    if (span_expr_defines_max && BITPUNCH_OK == bt_ret) {
        bt_ret = box_set_max_span_size(box, span_size.integer, bst);
    }
    return bt_ret;
}

bitpunch_status_t
box_compute_max_span_size__span_expr(struct box *box,
                                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    const struct named_expr *span_stmt;
    int span_expr_defines_min;
    expr_value_t span_size;
    int64_t max_span_size;

    DBG_BOX_DUMP(box);
    span_expr_defines_min = FALSE;
    bt_ret = filter_evaluate_attribute_internal(
        box->filter, box, "@maxspan", 0u, &span_stmt, &span_size, NULL, bst);
    if (BITPUNCH_NO_ITEM == bt_ret) {
        span_expr_defines_min = TRUE;
        bt_ret = filter_evaluate_attribute_internal(
            box->filter, box, "@span", 0u, &span_stmt, &span_size, NULL, bst);
    }
    switch (bt_ret) {
    case BITPUNCH_OK:
        break ;
    case BITPUNCH_NO_ITEM:
        // no variable span enabled by conditional
        return box_compute_max_span_size__as_slack(box, bst);
    default:
        return bt_ret;
    }
    /* TODO: show data path info in errors */
    if (span_size.integer < 0) {
        return box_error(BITPUNCH_DATA_ERROR, box, span_stmt->expr, bst,
                         "evaluation of span size expression gives "
                         "negative value (%"PRIi64")", span_size.integer);
    }
    bt_ret = box_compute_slack_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    max_span_size = MIN(span_size.integer,
                        box->end_offset_slack - box->start_offset_slack);
    bt_ret = box_set_max_span_size(box, max_span_size, bst);
    if (span_expr_defines_min && BITPUNCH_OK == bt_ret) {
        bt_ret = box_set_min_span_size(box, span_size.integer, bst);
    }
    return bt_ret;
}

bitpunch_status_t
box_compute_used_size__as_span(struct box *box,
                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    bt_ret = box_compute_span_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_size(box, box->end_offset_span
                        - box->start_offset_span,
                        BOX_SIZE_USED, bst);
}

bitpunch_status_t
tracker_compute_item_size__item_box(struct tracker *tk,
                                    int64_t *item_sizep,
                                    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *item_box;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_create_item_box_internal(tk, &item_box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = box_compute_span_size(item_box, bst);
    if (BITPUNCH_OK == bt_ret) {
        assert(item_box->end_offset_span >= 0);
        *item_sizep =
            item_box->end_offset_span - item_box->start_offset_span;
    }
    box_delete(item_box);
    DBG_TRACKER_CHECK_STATE(tk);
    return bt_ret;
}


bitpunch_status_t
compute_item_size__const_size(struct ast_node_hdl *item_filter,
                               struct box *scope,
                               int64_t item_offset,
                               int64_t max_span_offset,
                               int64_t *item_sizep,
                               struct browse_state *bst)
{
    *item_sizep = item_filter->ndat->u.item.min_span_size;
    return BITPUNCH_OK;
}


void
compile_node_backends__item__generic(struct ast_node_hdl *item)
{
    struct item_backend *b_item;

    compile_node_backends__filter__filter(item);

    b_item = &item->ndat->u.rexpr_filter.f_instance->b_item;
    if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
        b_item->compute_item_size = compute_item_size__const_size;
    }
}
