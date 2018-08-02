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

#include "core/filter.h"

static bitpunch_status_t
varint_get_size(struct ast_node_hdl *filter,
                struct box *scope,
                int64_t *span_sizep,
                int64_t *used_sizep,
                const char *data, int64_t max_span_size,
                struct browse_state *bst)
{
    size_t bytepos;

    // FIXME optimize
    for (bytepos = 0; bytepos < max_span_size; ++bytepos) {
        if (!(data[bytepos] & 0x80)) {
            *span_sizep = bytepos + 1;
            *used_sizep = bytepos + 1;
            return BITPUNCH_OK;
        }
    }
    // invalid varint
    return BITPUNCH_DATA_ERROR;
}

static bitpunch_status_t
varint_read(struct ast_node_hdl *filter,
            struct box *scope,
            expr_value_t *read_value,
            const char *data, size_t span_size,
            struct browse_state *bst)
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
        return BITPUNCH_DATA_ERROR;
    }
    // zigzag encoding
    //value = (rawvalue & 1) ? -((rawvalue + 1) / 2) : (rawvalue / 2);
    value = rawvalue;
    read_value->type = EXPR_VALUE_TYPE_INTEGER;
    read_value->integer = value;
    return BITPUNCH_OK;
}

static struct filter_instance *
varint_filter_instance_build(
    struct ast_node_hdl *filter,
    struct compile_ctx *ctx)
{
#if 0
    assert(NULL != data_source);

    if (AST_NODE_TYPE_BYTE != data_source->ndat->type &&
        AST_NODE_TYPE_BYTE_ARRAY != data_source->ndat->type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &filter->loc,
            "varint filter expects a byte array");
        return -1;
    }
#endif
    filter->ndat->u.rexpr_filter.get_size_func = varint_get_size;
    filter->ndat->u.rexpr_filter.read_func = varint_read;
    return new_safe(struct filter_instance);
}

void
filter_class_declare_varint(void)
{
    int ret;

    ret = filter_class_declare("varint",
                              EXPR_VALUE_TYPE_INTEGER,
                              varint_filter_instance_build,
                              0);
    assert(0 == ret);
}
