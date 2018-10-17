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

#ifndef __FILTER_ARRAY_H__
#define __FILTER_ARRAY_H__

#include "filters/container.h"

struct filter_instance_array {
    struct filter_instance filter; /* inherits */
    struct ast_node_hdl *item_type;
    struct ast_node_hdl *item_count;
};

bitpunch_status_t
box_get_n_items__array_non_slack(struct box *box, int64_t *item_countp,
                                 struct browse_state *bst);

bitpunch_status_t
tracker_goto_first_item__array_generic(struct tracker *tk,
                                       struct browse_state *bst);
bitpunch_status_t
tracker_get_item_key__array_generic(struct tracker *tk,
                                    expr_value_t *keyp,
                                    int *nth_twinp,
                                    struct browse_state *bst);
bitpunch_status_t
tracker_goto_next_item_with_key__indexed_array_internal(
    struct tracker *tk,
    expr_value_t item_key,
    int64_t end_index,
    struct browse_state *bst);
bitpunch_status_t
tracker_goto_nth_item_with_key__indexed_array_internal(
    struct tracker *tk,
    expr_value_t item_key,
    int nth_twin,
    int64_t from_index,
    int64_t end_index,
    struct browse_state *bst);
bitpunch_status_t
tracker_get_item_key__indexed_array_internal(
    struct tracker *tk,
    int64_t from_index,
    expr_value_t *keyp,
    int *nth_twinp,
    struct browse_state *bst);
bitpunch_status_t
tracker_goto_end_path__array_generic(struct tracker *tk,
                                     struct browse_state *bst);
void
tracker_goto_nil__array_generic(struct tracker *tk);

#endif
