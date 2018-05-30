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
#include <sys/types.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>
#include <endian.h>

#include "core/interpreter.h"

#define REF_SPAN    0
#define REF_MINSPAN 1
#define REF_MAXSPAN 2
#define REF_KEY     3
#define REF_LAST    4

static int
item_rcall_build(struct ast_node_hdl *rcall,
                 const struct ast_node_hdl *attr_values,
                 struct compile_ctx *ctx)
{
    return 0;
}

void
interpreter_declare_item(void)
{
    int ret;

    ret = interpreter_declare("item",
                              EXPR_VALUE_TYPE_UNSET,
                              item_rcall_build,
                              5,
                              "@span", REF_SPAN,
                              EXPR_VALUE_TYPE_INTEGER, 0,
                              "@minspan", REF_MINSPAN,
                              EXPR_VALUE_TYPE_INTEGER, 0,
                              "@maxspan", REF_MAXSPAN,
                              EXPR_VALUE_TYPE_INTEGER, 0,
                              "@key", REF_KEY,
                              (EXPR_VALUE_TYPE_INTEGER |
                               EXPR_VALUE_TYPE_STRING), 0,
                              "@last", REF_LAST,
                              EXPR_VALUE_TYPE_BOOLEAN, 0);
    assert(0 == ret);
}
