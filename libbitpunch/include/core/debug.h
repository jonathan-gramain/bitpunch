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

#ifndef __DEBUG_H__
#define __DEBUG_H__

#include "core/browse_internal.h"
#include "utils/port.h"

#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"

#if defined DEBUG
#    define DPRINT(...) do {                                            \
        if (tracker_debug_mode) {                                       \
            fprintf(stdout, "in "ANSI_COLOR_RED"%s()"ANSI_COLOR_RESET   \
                    " at %s:%d: ", __func__, __FILE__, __LINE__);       \
            fprintf(stdout, __VA_ARGS__);                               \
        }                                                               \
    } while (0)

#    define DBG_TRACKER_CHECK_STATE(tk) do {            \
        dbg_tracker_check_state(tk);                    \
    } while (0)

#    define DBG_TRACKER_DUMP(tk) do {           \
        if (tracker_debug_mode) {               \
            DPRINT("\n");                       \
            dbg_tracker_dump(#tk, tk);          \
        }                                       \
    } while (0)
#    define DBG_BOX_DUMP(box) do {              \
        if (tracker_debug_mode) {               \
            DPRINT("\n");                       \
            dbg_box_dump(#box, box);            \
        }                                       \
    } while (0)
#else
#    define DPRINT(...)
#    define DBG_TRACKER_CHECK_STATE(tk, bst)
#    define DBG_TRACKER_DUMP(tk)
#    define DBG_BOX_DUMP(box)
#endif

struct tracker;

const char *
dbg_tracker_get_dpath(const struct tracker *tk);

const char *
dbg_tracker_state_str(enum tracker_state state);

void
dbg_tracker_dump(const char *tk_str, const struct tracker *tk);

void
dbg_box_dump(const char *box_str, const struct box *box);

void
dbg_tracker_check_state(const struct tracker *tk);

struct browse_state *
dbg_browse_state_dummy(void);

#endif /*__DEBUG_H__*/
