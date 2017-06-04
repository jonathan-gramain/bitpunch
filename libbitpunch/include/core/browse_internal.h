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

void
browse_state_init(struct browse_state *bst);
void
browse_state_cleanup(struct browse_state *bst);

struct box *
box_new_slice_box(struct tracker *slice_start,
                  struct tracker *slice_end,
                  struct browse_state *bst);

struct box *
box_new_bytes_box_from_item(struct tracker *tk,
                            struct browse_state *bst);
struct box *
box_new_bytes_box_from_box(struct box *box,
                           struct browse_state *bst);
struct box *
box_new_as_box(struct box *parent_box,
               struct ast_node *as_type, int64_t box_offset,
               struct browse_state *bst);
struct box *
box_new_filter_box(struct box *unfiltered_box,
                   const char *filtered_data,
                   size_t filtered_size,
                   struct browse_state *bst);

bitpunch_status_t
box_get_n_items_internal(struct box *box, int64_t *n_itemsp,
                         struct browse_state *bst);
bitpunch_status_t
box_get_min_span_size(struct box *box, int64_t *min_span_sizep,
                      struct browse_state *bst);
bitpunch_status_t
box_get_used_size(struct box *box, int64_t *used_sizep,
                  struct browse_state *bst);
bitpunch_status_t
box_read_value_internal(struct box *box,
                        enum expr_value_type *typep,
                        union expr_value *valuep,
                        struct browse_state *bst);

bitpunch_status_t
tracker_create_item_box(struct tracker *tk,
                        struct browse_state *bst);

bitpunch_status_t
tracker_check_item(struct tracker *tk,
                   struct browse_state *bst);

bitpunch_status_t
tracker_goto_field_internal(struct tracker *tk,
                            const struct field *to_field, int flat,
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
tracker_get_item_offset_internal(struct tracker *tk, int64_t *item_offsetp,
                                 struct browse_state *bst);
bitpunch_status_t
tracker_get_item_size_internal(struct tracker *tk, int64_t *item_sizep,
                               struct browse_state *bst);
bitpunch_status_t
tracker_read_item_value_internal(struct tracker *tk,
                                 enum expr_value_type *typep,
                                 union expr_value *valuep,
                                 struct browse_state *bst);

bitpunch_status_t
tracker_reverse_direction_internal(struct tracker *tk,
                                   struct browse_state *bst);

bitpunch_status_t
item_read_value__interpreter(const struct ast_node *item_node,
                             struct box *scope,
                             int64_t item_offset,
                             int64_t item_size,
                             enum expr_value_type *typep,
                             union expr_value *valuep,
                             struct browse_state *bst);

#endif /* __BROWSE_INTERNAL_H__ */
