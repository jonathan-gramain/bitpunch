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
#include "filters/byte_slice.h"

struct ast_node_data shared_ast_node_data_byte_slice = {
    .type = AST_NODE_TYPE_BYTE_SLICE,
};
struct ast_node_hdl shared_ast_node_byte_slice = {
    .ndat = &shared_ast_node_data_byte_slice,
};

#define AST_NODE_BYTE_SLICE &shared_ast_node_byte_slice

static bitpunch_status_t
box_compute_span_size__byte_slice(struct box *box,
                                  struct browse_state *bst)
{
    int64_t n_bytes;
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    bt_ret = box_get_n_items__slice_generic(box, &n_bytes, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_span_size(box, n_bytes, bst);
}

static struct filter_instance *
byte_slice_filter_instance_build(struct ast_node_hdl *item)
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
    b_box->compute_span_size = box_compute_span_size__byte_slice;
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
        tracker_goto_next_item_with_key__not_impl;
    b_tk->goto_nth_item_with_key =
        tracker_goto_nth_item_with_key__not_impl;

    return slice;
}

static void
compile_node_backends__byte_slice(struct ast_node_hdl *item)
{
    item->ndat->u.rexpr_filter.f_instance =
        byte_slice_filter_instance_build(item);

    compile_node_backends__filter__filter(item);
}

int
compile_global_nodes__byte_slice(struct compile_ctx *ctx)
{
    compile_node_backends__byte_slice(AST_NODE_BYTE_SLICE);
    return 0;
}

struct ast_node_hdl *
filter_get_global_instance__byte_slice(void)
{
    return AST_NODE_BYTE_SLICE;
}
