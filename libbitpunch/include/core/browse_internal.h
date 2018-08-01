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

#ifndef __BROWSE_INTERNAL_H__
#define __BROWSE_INTERNAL_H__

#include "core/browse.h"

extern struct ast_node_hdl shared_ast_node_byte;
extern struct ast_node_hdl shared_ast_node_array_slice;
extern struct ast_node_hdl shared_ast_node_byte_slice;
extern struct ast_node_hdl shared_ast_node_as_bytes;
extern struct ast_node_hdl shared_ast_node_as_bytes_filter;
extern struct ast_node_hdl shared_ast_node_raw_byte_filter;
extern struct dpath_node shared_dpath_node_raw_byte;
extern struct dpath_node shared_dpath_node_array_slice;
extern struct dpath_node shared_dpath_node_byte_slice;

#define AST_NODE_BYTE &shared_ast_node_byte
#define AST_NODE_ARRAY_SLICE &shared_ast_node_array_slice
#define AST_NODE_BYTE_SLICE &shared_ast_node_byte_slice
#define AST_NODE_AS_BYTES &shared_ast_node_as_bytes
#define DPATH_NODE_RAW_BYTE &shared_dpath_node_raw_byte
#define DPATH_NODE_ARRAY_SLICE &shared_dpath_node_array_slice
#define DPATH_NODE_BYTE_SLICE &shared_dpath_node_byte_slice

void
browse_state_init(struct browse_state *bst);
void
browse_state_cleanup(struct browse_state *bst);

struct box *
box_new_slice_box(struct tracker *slice_start,
                  struct tracker *slice_end,
                  struct browse_state *bst);

struct box *
box_new_as_box(struct box *parent_box,
               struct dpath_node *as_dpath,
               struct browse_state *bst);
struct box *
box_new_filter_box(struct box *parent_box,
                   struct ast_node_hdl *filter,
                   struct ast_node_hdl *filter_defining_used_size,
                   struct browse_state *bst);
bitpunch_status_t
box_apply_filter_internal(struct box *box,
                          struct browse_state *bst);

bitpunch_status_t
box_get_n_items_internal(struct box *box, int64_t *item_countp,
                         struct browse_state *bst);
bitpunch_status_t
box_get_min_span_size(struct box *box, int64_t *min_span_sizep,
                      struct browse_state *bst);
bitpunch_status_t
box_get_used_size(struct box *box, int64_t *used_sizep,
                  struct browse_state *bst);
bitpunch_status_t
box_get_slack_size(struct box *box, int64_t *slack_sizep,
                   struct browse_state *bst);
bitpunch_status_t
box_read_value_internal(struct box *box,
                        expr_value_t *valuep,
                        struct browse_state *bst);

bitpunch_status_t
tracker_create_item_box_internal(struct tracker *tk,
                                 struct browse_state *bst);

bitpunch_status_t
tracker_create_item_box(struct tracker *tk,
                        struct tracker_error **errp);

bitpunch_status_t
tracker_get_filtered_dpath_internal(struct tracker *tk,
                                    expr_dpath_t *filtered_dpathp,
                                    struct browse_state *bst);
bitpunch_status_t
tracker_get_filtered_dpath(struct tracker *tk,
                           expr_dpath_t *filtered_dpathp,
                           struct tracker_error **errp);
bitpunch_status_t
tracker_get_filtered_item_box_internal(struct tracker *tk,
                                       struct box **filtered_boxp,
                                       struct browse_state *bst);
bitpunch_status_t
tracker_get_filtered_item_box(struct tracker *tk,
                              struct box **filtered_boxp,
                              struct tracker_error **errp);

bitpunch_status_t
track_item_contents_internal(struct tracker *tk,
                             struct tracker **tkp,
                             struct browse_state *bst);
struct tracker *
track_box_contents(struct box *box,
                   struct tracker_error **errp);

bitpunch_status_t
tracker_check_item(struct tracker *tk,
                   struct browse_state *bst);

bitpunch_status_t
tracker_goto_field_internal(struct tracker *tk,
                            const struct field *to_field, int flat,
                            struct browse_state *bst);
bitpunch_status_t
tracker_goto_field_internal2(struct tracker *tk,
                             const struct field *to_field, int flat,
                             int is_trailer_field,
                             struct browse_state *bst);
bitpunch_status_t
tracker_goto_index_internal(struct tracker *tk,
                            struct subscript_index index,
                            const char *index_desc,
                            struct box *scope,
                            int allow_end_boundary,
                            int is_end_of_slice,
                            struct browse_state *bst);

bitpunch_status_t
tracker_enter_item_internal(struct tracker *tk,
                            struct browse_state *bst);
bitpunch_status_t
tracker_return_internal(struct tracker *tk,
                        struct browse_state *bst);

bitpunch_status_t
tracker_get_item_offset_internal(struct tracker *tk, int64_t *item_offsetp,
                                 struct browse_state *bst);
bitpunch_status_t
tracker_get_item_size_internal(struct tracker *tk, int64_t *item_sizep,
                               struct browse_state *bst);
bitpunch_status_t
tracker_get_item_location_internal(struct tracker *tk,
                                   int64_t *item_offsetp,
                                   int64_t *item_sizep,
                                   struct browse_state *bst);
bitpunch_status_t
tracker_read_item_value_internal(struct tracker *tk,
                                 expr_value_t *valuep,
                                 struct browse_state *bst);
bitpunch_status_t
tracker_read_item_value_direct_internal(struct tracker *tk,
                                        expr_value_t *valuep,
                                        struct browse_state *bst);

bitpunch_status_t
tracker_read_item_raw_internal(struct tracker *tk,
                               const char **item_contentsp,
                               int64_t *item_sizep,
                               struct browse_state *bst);
bitpunch_status_t
tracker_reverse_direction_internal(struct tracker *tk,
                                   struct browse_state *bst);

bitpunch_status_t
transmit_error(bitpunch_status_t bt_ret, struct browse_state *bst,
               struct tracker_error **errp);

#endif /* __BROWSE_INTERNAL_H__ */
