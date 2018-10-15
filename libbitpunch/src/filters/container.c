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
#include "filters/container.h"

bitpunch_status_t
box_compute_span_size__packed_var_size(struct box *box,
                                       struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct tracker *tk;

    DBG_BOX_DUMP(box);
    bt_ret = track_box_contents_internal(box, &tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    tk->flags |= (TRACKER_NEED_ITEM_OFFSET |
                  ((box->flags & BOX_RALIGN) ? TRACKER_REVERSED : 0u));
    bt_ret = tracker_goto_first_item_internal(tk, bst);
    while (BITPUNCH_OK == bt_ret) {
      bt_ret = tracker_goto_next_item_internal(tk, bst);
    }
    if (BITPUNCH_NO_ITEM == bt_ret) {
        assert(-1 != tk->item_offset);
        if (0 != (box->flags & BOX_RALIGN)) {
            bt_ret = box_set_start_offset(tk->box, tk->item_offset,
                                          BOX_START_OFFSET_SPAN, bst);
        } else {
            bt_ret = box_set_end_offset(tk->box, tk->item_offset,
                                        BOX_END_OFFSET_SPAN, bst);
        }
    }
    tracker_delete(tk);
    return bt_ret;
}
