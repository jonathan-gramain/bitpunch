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

#define _BSD_SOURCE
#define _GNU_SOURCE
#include <sys/types.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>
#include <endian.h>

#include "core/interpreter.h"

static int
varint_get_size(size_t *sizep,
                const char *data, size_t max_span_size,
                const struct ast_node *param_values)
{
    size_t bytepos;

    // FIXME optimize
    for (bytepos = 0; bytepos < max_span_size; ++bytepos) {
        if (!(data[bytepos] & 0x80)) {
            *sizep = bytepos + 1;
            return 0;
        }
    }
    // invalid varint
    return -1;
}

static int
varint_read(union expr_value *read_value,
            const char *data, size_t span_size,
            const struct ast_node *param_values)
{
    // FIXME optimize
    const unsigned char *udata = (const unsigned char *)data;
    size_t bytepos;
    size_t cur_shift;
    uint64_t rawvalue;
    int64_t value;

    // FIXME optimize
    rawvalue = 0;
    cur_shift = 0;
    for (bytepos = 0; bytepos < span_size; ++bytepos) {
        rawvalue |= ((uint64_t)udata[bytepos] & 0x7f) << cur_shift;
        if (!(udata[bytepos] & 0x80)) {
            break ;
        }
        cur_shift += 7;
    }
    if (bytepos == span_size) {
        // invalid varint
        return -1;
    }
    // zigzag encoding
    //value = (rawvalue & 1) ? -((rawvalue + 1) / 2) : (rawvalue / 2);
    value = rawvalue;
    read_value->integer = value;
    return 0;
}

static int
varint_write(const union expr_value *write_value,
             char *data, size_t span_size,
             const struct ast_node *param_values)
{
    return -1;
}


static int
varint_rcall_build(struct ast_node *rcall,
                   const struct ast_node *call,
                   const struct ast_node *param_values)
{
    const struct ast_node *data_source;

    data_source = call->u.filter.target;
    if (AST_NODE_TYPE_BYTE != data_source->type &&
        AST_NODE_TYPE_BYTE_ARRAY != data_source->type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &call->loc,
            "varint interpreter expects a byte array");
        return -1;
    }
    rcall->u.rexpr_interpreter.get_size_func = varint_get_size;
    rcall->u.rexpr_interpreter.read_func = varint_read;
    rcall->u.rexpr_interpreter.write_func = varint_write;
    return 0;
}

void
interpreter_declare_varint(void)
{
    int ret;

    ret = interpreter_declare("varint",
                              EXPR_VALUE_TYPE_INTEGER,
                              varint_rcall_build,
                              0);
    assert(0 == ret);
}
