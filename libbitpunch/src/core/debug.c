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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <execinfo.h>

#include "core/parser.h"
#include PATH_TO_PARSER_TAB_H
#include "core/browse.h"
#include "core/print.h"
#include "core/debug.h"

const char *
dbg_tracker_get_dpath(const struct tracker *tk)
{
    static char dpath[1024];

    (void) tracker_get_abs_dpath(tk, dpath, sizeof (dpath));
    return dpath;
}

const char *
dbg_tracker_state_str(enum tracker_state state)
{
#define MAP(state) case TRACKER_STATE_##state: return #state
    switch (state) {
        MAP(DANGLING);
        MAP(ITEM);
        MAP(ITEM_OFFSET);
        MAP(ITEM_SIZE);
        MAP(ITEM_BOX);
        MAP(ITEM_BOX_SIZE);
        MAP(AT_END);
    default:
        return "unknown state";
    }
#undef MAP
}

static void
dbg_tracker_dump_flags(const struct tracker *tk)
{
    int first = TRUE;

#define MAP(value) do {                                 \
        if (tk->flags & TRACKER_##value) {              \
            printf("%s" #value, (first ? "" : "|"));    \
            first = FALSE;                              \
        }                                               \
    } while (0)

    MAP(AT_END);
    MAP(NEED_ITEM_OFFSET);
    MAP(REVERSED);
}

void
dbg_tracker_dump(const char *tk_str, const struct tracker *tk)
{
    if (NULL != tk) {
        printf("%s (%p): (%s flags=", tk_str, tk,
               dbg_tracker_state_str(tracker_get_state(tk)));
        dbg_tracker_dump_flags(tk);
        printf(") ");
        tracker_fdump(tk, stdout);
        if (NULL != tk->item_box) {
            printf("    ITEM ");
            box_fdump(tk->item_box, stdout);
        }
    } else {
        printf("%s: null", tk_str);
    }
}

void
dbg_box_dump(const char *box_str, const struct box *box)
{
    printf("%s (%p): ", box_str, box);
    if (NULL != box) {
        box_fdump(box, stdout);
    } else {
        printf("null");
    }
}

static void
dbg_tracker_check_track_path(const struct tracker *tk,
                             int expect_is_set)
{
    assert(NULL != tk->box->dpath.filter);
    switch (tk->box->dpath.filter->ndat->type) {
    case AST_NODE_TYPE_COMPOSITE:
        assert(TRACK_PATH_COMPOSITE == tk->cur.type);
        if (expect_is_set) {
            assert(NULL != tk->cur.u.block.field);
        } else {
            assert(NULL == tk->cur.u.block.field);
        }
        break ;
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
        if (expect_is_set) {
            assert(-1 != tk->cur.u.array.index);
        } else {
            assert(-1 == tk->cur.u.array.index);
        }
        break ;
    default:
        break ;
    }
}

void
dbg_tracker_check_state(const struct tracker *tk)
{
    int reversed_iter;

    assert(NULL != tk->box);
    reversed_iter = (0 != (tk->flags & TRACKER_REVERSED));
    switch (tracker_get_state(tk)) {
    case TRACKER_STATE_DANGLING:
        assert(tracker_is_dangling(tk));
        // tk->item_offset may not be -1, and track_path may be set,
        // if pointing at end of slice
        assert(NULL == tk->item_box);
        assert(-1 == tk->item_size);
        assert(0 == (tk->flags & TRACKER_AT_END));
        break ;

    case TRACKER_STATE_ITEM:
        assert(!tracker_is_dangling(tk));
        assert(-1 == tk->item_offset);
        assert(-1 == tk->item_size);
        assert(NULL == tk->item_box);
        assert(0 == (tk->flags & TRACKER_AT_END));
        dbg_tracker_check_track_path(tk, TRUE);
        break ;

    case TRACKER_STATE_ITEM_OFFSET:
        assert(!tracker_is_dangling(tk));
        assert(tk->item_offset >= 0);
        assert(-1 == tk->box->start_offset_max_span
               || tk->item_offset >= tk->box->start_offset_max_span);
        assert(-1 == tk->box->start_offset_used
               || tk->item_offset >= tk->box->start_offset_used);
        assert(-1 == tk->box->end_offset_max_span
               || tk->item_offset <= tk->box->end_offset_max_span);
        assert(-1 == tk->box->end_offset_used
               || tk->item_offset <= tk->box->end_offset_used);
        assert(-1 == tk->item_size);
        assert(NULL == tk->item_box);
        assert(0 == (tk->flags & TRACKER_AT_END));
        dbg_tracker_check_track_path(tk, TRUE);
        break ;

    case TRACKER_STATE_ITEM_SIZE: {
        int64_t item_end;

        assert(!tracker_is_dangling(tk));
        assert(tk->item_offset >= 0);
        assert(tk->item_size >= 0);
        assert(-1 == tk->box->start_offset_max_span
               || tk->item_offset >= tk->box->start_offset_max_span);
        assert(-1 == tk->box->start_offset_used
               || tk->item_offset >= tk->box->start_offset_used);
        assert(-1 == tk->box->end_offset_max_span
               || tk->item_offset <= tk->box->end_offset_max_span);
        assert(-1 == tk->box->end_offset_used
               || tk->item_offset <= tk->box->end_offset_used);
        if (reversed_iter) {
            item_end = tk->item_offset - tk->item_size;
        } else {
            item_end = tk->item_offset + tk->item_size;
        }
        assert(-1 == tk->box->start_offset_max_span
               || item_end >= tk->box->start_offset_max_span);
        assert(-1 == tk->box->start_offset_used
               || item_end >= tk->box->start_offset_used);
        assert(-1 == tk->box->end_offset_max_span
               || item_end <= tk->box->end_offset_max_span);
        assert(-1 == tk->box->end_offset_used
               || item_end <= tk->box->end_offset_used);
        assert(NULL == tk->item_box);
        dbg_tracker_check_track_path(tk,
                                     0 == (tk->flags & TRACKER_AT_END));
        break ;
    }
    case TRACKER_STATE_ITEM_BOX:
        assert(!tracker_is_dangling(tk));
        assert(tk->item_offset >= 0);
        assert(-1 == tk->item_size);
        assert(NULL != tk->item_box);
        if (reversed_iter) {
            assert(tk->item_box->end_offset_used == tk->item_offset);
        } else {
            assert(tk->item_box->start_offset_used == tk->item_offset);
        }
        assert(0 == (tk->flags & TRACKER_AT_END));
        dbg_tracker_check_track_path(tk, TRUE);
        break ;

    case TRACKER_STATE_ITEM_BOX_SIZE: {
        int64_t item_end;

        assert(!tracker_is_dangling(tk));
        assert(tk->item_offset >= 0);
        assert(tk->item_size >= 0);
        assert(NULL != tk->item_box);
        if (reversed_iter) {
            item_end = tk->item_offset - tk->item_size;
        } else {
            item_end = tk->item_offset + tk->item_size;
        }
        assert(-1 == tk->box->start_offset_max_span
               || item_end >= tk->box->start_offset_max_span);
        assert(-1 == tk->box->end_offset_max_span
               || item_end <= tk->box->end_offset_max_span);
        if (reversed_iter) {
            assert(tk->item_box->end_offset_used == tk->item_offset);
            assert(tk->item_box->start_offset_used == item_end);
        } else {
            assert(tk->item_box->start_offset_used == tk->item_offset);
            assert(tk->item_box->end_offset_used == item_end);
        }
        assert(0 == (tk->flags & TRACKER_AT_END));
        dbg_tracker_check_track_path(tk, TRUE);
        break ;
    }
    case TRACKER_STATE_AT_END:
        assert(NULL != tk->box);
        assert(tk->item_offset >= 0
               || 0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET));
        assert(-1 == tk->item_size);
        assert(0 != (tk->flags & TRACKER_AT_END));
        assert(tracker_is_dangling(tk));
        assert(NULL == tk->item_box);
        break ;

    default:
        assert(0);
    }
}

struct browse_state *
dbg_browse_state_dummy(void)
{
    struct browse_state *bst;

    bst = new_safe(struct browse_state);
    browse_state_init(bst);
    return bst;
}
