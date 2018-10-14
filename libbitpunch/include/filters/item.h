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

#ifndef __FILTER_ITEM_H__
#define __FILTER_ITEM_H__

#include "core/filter.h"

bitpunch_status_t
filter_read_value__bytes(struct ast_node_hdl *item_filter,
                         struct box *scope,
                         int64_t item_offset,
                         int64_t item_size,
                         expr_value_t *valuep,
                         struct browse_state *bst);
bitpunch_status_t
filter_read_value__filter(struct ast_node_hdl *filter,
                          struct box *scope,
                          int64_t item_offset,
                          int64_t item_size,
                          expr_value_t *valuep,
                          struct browse_state *bst);

bitpunch_status_t
box_compute_span_size__static_size(struct box *box,
                                   struct browse_state *bst);
bitpunch_status_t
box_compute_slack_size__from_parent(struct box *box,
                                    struct browse_state *bst);
bitpunch_status_t
box_compute_slack_size__as_container_slack(struct box *box,
                                        struct browse_state *bst);
bitpunch_status_t
box_compute_max_span_size__as_span(struct box *box,
                                   struct browse_state *bst);
bitpunch_status_t
box_compute_max_span_size__as_slack(struct box *box,
                                    struct browse_state *bst);
bitpunch_status_t
box_compute_span_size__as_slack(struct box *box,
                                struct browse_state *bst);
bitpunch_status_t
box_compute_span_size__as_max_span(struct box *box,
                                   struct browse_state *bst);
bitpunch_status_t
box_compute_span_size__from_item_size(struct box *box,
                                      struct browse_state *bst);
bitpunch_status_t
box_compute_min_span_size__as_hard_min(struct box *box,
                                       struct browse_state *bst);
bitpunch_status_t
box_compute_min_span_size__span_expr(struct box *box,
                                     struct browse_state *bst);
bitpunch_status_t
box_compute_max_span_size__span_expr(struct box *box,
                                     struct browse_state *bst);
bitpunch_status_t
box_compute_used_size__from_apply_filter(struct box *box,
                                         struct browse_state *bst);
bitpunch_status_t
box_compute_used_size__as_span(struct box *box,
                               struct browse_state *bst);
bitpunch_status_t
tracker_compute_item_size__item_box(struct tracker *tk,
                                    int64_t *item_sizep,
                                    struct browse_state *bst);
bitpunch_status_t
compute_item_size__static_size(struct ast_node_hdl *item_filter,
                               struct box *scope,
                               int64_t item_offset,
                               int64_t max_span_offset,
                               int64_t *item_sizep,
                               struct browse_state *bst);

void
compile_node_backends__filter_generic(struct ast_node_hdl *filter);
void
compile_node_backends__item__generic(struct ast_node_hdl *item);
void
compile_node_backends__item(struct ast_node_hdl *item);

int
compile_global_nodes__item(struct compile_ctx *ctx);

struct ast_node_hdl *
filter_get_global_instance__source(void);

#endif
