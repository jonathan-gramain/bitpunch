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
// FIXME should not need this, only for as_bytes tracker backend
#include "filters/byte_array.h"

bitpunch_status_t
filter_read_value__bytes(struct ast_node_hdl *item_filter,
                         struct box *scope,
                         int64_t item_offset,
                         int64_t item_size,
                         expr_value_t *valuep,
                         struct browse_state *bst)
{
    if (NULL != valuep) {
        memset(valuep, 0, sizeof(*valuep));
        valuep->type = EXPR_VALUE_TYPE_BYTES;
        valuep->bytes.buf = scope->ds_out->ds_data + item_offset;
        valuep->bytes.len = item_size;
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
filter_read_value__filter(struct ast_node_hdl *filter,
                          struct box *scope,
                          int64_t item_offset,
                          int64_t item_size,
                          expr_value_t *valuep,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    // if box filter is a data filter, getting the value is reading
    // the bytes from the filter output.
    if (0 != (scope->flags & BOX_DATA_SOURCE)) {
        bt_ret = box_apply_filter_internal(scope, bst);
        if (BITPUNCH_OK == bt_ret) {
            bt_ret = box_compute_used_size(scope, bst);
        }
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        return filter_read_value__bytes(
            filter, scope, scope->start_offset_used,
            scope->end_offset_used - scope->start_offset_used, valuep, bst);
    }
    return filter_instance_read_value(filter, scope,
                                      item_offset, item_size,
                                      valuep, bst);
}

bitpunch_status_t
box_compute_span_size__static_size(struct box *box,
                                   struct browse_state *bst)
{
    DBG_BOX_DUMP(box);
    return box_set_span_size(box, box->filter->ndat->u.item.min_span_size, bst);
}


bitpunch_status_t
box_compute_slack_size__from_parent(struct box *box,
                                    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    assert(NULL != box->parent_box);

    // A filter box needs to know the actual size used by the upstream
    // filtered data to know how much input data is available. On the
    // other hand, an item box filter's size may have to be determined
    // to compute the size of the parent (container) filter, hence the
    // different call required.
    if (0 != (box->flags & BOX_FILTER)) {
        bt_ret = box_compute_used_size(box->parent_box, bst);
    } else {
        bt_ret = box_compute_slack_size(box->parent_box, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (0 != (box->flags & BOX_RALIGN)) {
        return box_set_start_offset(
            box, box_get_known_start_offset(box->parent_box),
            BOX_START_OFFSET_SLACK, bst);
    } else {
        return box_set_end_offset(
            box, box_get_known_end_offset(box->parent_box),
            BOX_END_OFFSET_SLACK, bst);
    }
}

bitpunch_status_t
box_compute_slack_size__as_container_slack(struct box *box,
                                        struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int get_left_offset;
    int64_t max_slack_offset;

    DBG_BOX_DUMP(box);
    assert(NULL != box->parent_box);
    /* slack child is limited by parent's maximum slack offset */
    get_left_offset = 0 != (box->flags & BOX_RALIGN);
    if (0 != (box->flags & BOX_FILTER)) {
        bt_ret = box_compute_used_size(box->parent_box, bst);
    } else {
        bt_ret = box_compute_slack_size(box->parent_box, bst);
    }
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = box_get_slack_child_allocation(
            box->parent_box, get_left_offset, &max_slack_offset, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != max_slack_offset);
    if (0 != (box->flags & BOX_RALIGN)) {
        return box_set_start_offset(box, max_slack_offset,
                                    BOX_START_OFFSET_SLACK, bst);
    } else {
        return box_set_end_offset(box, max_slack_offset,
                                  BOX_END_OFFSET_SLACK, bst);
    }
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
    assert(-1 != box->start_offset_slack && -1 != box->end_offset_slack);
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
box_compute_span_size__as_max_span(struct box *box,
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
                        BOX_SIZE_SPAN, bst);
}

bitpunch_status_t
box_compute_span_size__from_item_size(struct box *box,
                                      struct browse_state *bst)
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
        box->filter, box, "@minspan", &span_stmt, &span_size, NULL, bst);
    if (BITPUNCH_NO_ITEM == bt_ret) {
        span_expr_defines_max = TRUE;
        bt_ret = filter_evaluate_attribute_internal(
            box->filter, box, "@span", &span_stmt, &span_size, NULL, bst);
    }
    switch (bt_ret) {
    case BITPUNCH_OK:
        break ;
    case BITPUNCH_NO_ITEM:
        // no dynamic span enabled by conditional: min span is the
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
        box->filter, box, "@maxspan", &span_stmt, &span_size, NULL, bst);
    if (BITPUNCH_NO_ITEM == bt_ret) {
        span_expr_defines_min = TRUE;
        bt_ret = filter_evaluate_attribute_internal(
            box->filter, box, "@span", &span_stmt, &span_size, NULL, bst);
    }
    switch (bt_ret) {
    case BITPUNCH_OK:
        break ;
    case BITPUNCH_NO_ITEM:
        // no dynamic span enabled by conditional
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
box_compute_used_size__from_apply_filter(struct box *box,
                                         struct browse_state *bst)
{
    DBG_BOX_DUMP(box);
    return box_apply_filter_internal(box, bst);
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
compute_item_size__static_size(struct ast_node_hdl *item_filter,
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
browse_setup_backends__filter__filter(struct ast_node_hdl *filter)
{
    struct item_backend *b_item = NULL;

    b_item = &filter->ndat->u.rexpr_filter.f_instance->b_item;
    //memset(b_item, 0, sizeof (*b_item));

    b_item->read_value = filter_read_value__filter;
}

static void
browse_setup_backends__box__filter(struct ast_node_hdl *item)
{
    struct filter_instance *f_instance;
    struct item_backend *b_item;
    struct box_backend *b_box;

    f_instance = item->ndat->u.rexpr_filter.f_instance;
    b_item = &f_instance->b_item;
    b_box = &f_instance->b_box;
    // FIXME avoid memset because filter may have set functions
    // already, find a cleaner way to deal with this
    //memset(b_box, 0, sizeof (*b_box));

    b_box->compute_slack_size = box_compute_slack_size__as_container_slack;
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->compute_max_span_size = box_compute_max_span_size__as_slack;
    if (NULL != b_item->compute_item_size) {
        b_box->compute_span_size = box_compute_span_size__from_item_size;
    } else {
        b_box->compute_span_size = box_compute_span_size__as_slack;
    }
    b_box->get_n_items = box_get_n_items__as_used;
    // FIXME should check for non-dpath filter only instead of
    // specific value-type (i.e. output dpath == input dpath)
    if (EXPR_VALUE_TYPE_INTEGER ==
        item->ndat->u.rexpr_filter.filter_cls->value_type_mask) {
        b_box->compute_used_size = box_compute_used_size__as_span;
    } else {
        b_box->compute_used_size = box_compute_used_size__from_apply_filter;
    }
}

static void
browse_setup_backends__filter_generic(struct ast_node_hdl *filter)
{
    browse_setup_backends__filter__filter(filter);
    browse_setup_backends__box__filter(filter);
    browse_setup_backends__tracker__as_bytes(filter);
}

int
browse_setup_backends_rexpr_filter(struct ast_node_hdl *filter)
{
    const struct filter_class *filter_cls;

    filter_cls = filter->ndat->u.rexpr_filter.filter_cls;
    if (NULL != filter_cls->filter_instance_compile_func) {
        return filter_cls->filter_instance_compile_func(
            filter, filter->ndat->u.rexpr_filter.f_instance,
            COMPILE_TAG_BROWSE_BACKENDS, NULL);
    } else {
      browse_setup_backends__filter_generic(filter);
    }
    return 0;
}

void
browse_setup_backends__item__generic(struct ast_node_hdl *item)
{
    struct item_backend *b_item;

    browse_setup_backends__filter__filter(item);

    b_item = &item->ndat->u.rexpr_filter.f_instance->b_item;
    if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_item->compute_item_size = compute_item_size__static_size;
    }
}

static struct filter_instance *
item_filter_instance_build(struct ast_node_hdl *item)
{
    struct filter_instance *filter;
    struct item_backend *b_item;

    filter = new_safe(struct filter_instance);
    b_item = &filter->b_item;
    memset(b_item, 0, sizeof (*b_item));

    if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_item->compute_item_size = compute_item_size__static_size;
    }
    b_item->read_value = filter_read_value__bytes;
    return filter;
}

void
browse_setup_backends__item(struct ast_node_hdl *item)
{
    item->ndat->u.rexpr_filter.f_instance = item_filter_instance_build(item);
}

static void
browse_setup_backends__box__source(struct ast_node_hdl *item)
{
    struct filter_instance *f_instance;
    struct box_backend *b_box;

    f_instance = item->ndat->u.rexpr_filter.f_instance;
    b_box = &f_instance->b_box;
    // FIXME avoid memset because filter may have set functions
    // already, find a cleaner way to deal with this
    //memset(b_box, 0, sizeof (*b_box));

    // data sources do not provide span backends
    b_box->compute_slack_size = box_compute__error;
    b_box->compute_min_span_size = box_compute__error;
    b_box->compute_max_span_size = box_compute__error;
    b_box->compute_span_size = box_compute__error;

    b_box->get_n_items = box_get_n_items__as_used;
    b_box->compute_used_size = box_compute_used_size__from_apply_filter;
}

void
browse_setup_backends__source(struct ast_node_hdl *item)
{
    item->ndat->u.rexpr_filter.f_instance = new_safe(struct filter_instance);

    browse_setup_backends__item__generic(item);
    browse_setup_backends__box__source(item);
    browse_setup_backends__tracker__as_bytes(item);
}
