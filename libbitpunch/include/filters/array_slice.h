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

#ifndef __FILTER_ARRAY_SLICE_H__
#define __FILTER_ARRAY_SLICE_H__

#include "filters/item.h"

struct box *
box_array_slice_get_ancestor_array(struct box *box);

struct box *
box_new_slice_box(struct tracker *slice_start,
                  struct tracker *slice_end,
                  struct browse_state *bst);

bitpunch_status_t
box_get_n_items__slice_generic(struct box *box, int64_t *item_countp,
                               struct browse_state *bst);

bitpunch_status_t
tracker_get_item_key__array_slice(struct tracker *tk,
                                  expr_value_t *keyp,
                                  int *nth_twinp,
                                  struct browse_state *bst);

bitpunch_status_t
tracker_goto_first_item__array_slice(struct tracker *tk,
                                     struct browse_state *bst);
bitpunch_status_t
tracker_goto_next_item__array_slice(struct tracker *tk,
                                    struct browse_state *bst);
bitpunch_status_t
tracker_goto_nth_item__array_slice(struct tracker *tk, int64_t index,
                                   struct browse_state *bst);
bitpunch_status_t
tracker_goto_named_item__array_slice(struct tracker *tk, const char *name,
                                     struct browse_state *bst);
bitpunch_status_t
tracker_goto_next_key_match__array_slice(struct tracker *tk,
                                         expr_value_t index,
                                         struct track_path search_boundary,
                                         struct browse_state *bst);

int
compile_global_nodes__array_slice(struct compile_ctx *ctx);

#endif
