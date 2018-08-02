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
#include <assert.h>
#include <string.h>
#include <snappy-c.h>

#include "core/filter.h"

#define UNCOMPRESSED_BUFFER_MAX_SIZE (1024 * 1024 * 1024)

static bitpunch_status_t
snappy_read(struct ast_node_hdl *filter,
            struct box *scope,
            expr_value_t *read_value,
            const char *data, size_t span_size,
            struct browse_state *bst)
{
    snappy_status snappy_ret;
    size_t uncompressed_length;
    char *uncompressed;

    snappy_ret = snappy_uncompressed_length(data, span_size,
                                            &uncompressed_length);
    if (SNAPPY_OK != snappy_ret) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &filter->loc,
                       "error from snappy_uncompressed_length() -> %d",
                       snappy_ret);
        return BITPUNCH_DATA_ERROR;
    }
    if (uncompressed_length > UNCOMPRESSED_BUFFER_MAX_SIZE) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &filter->loc,
                       "snappy uncompressed buffer too large "
                       "(%zu bytes, max %d)",
                       uncompressed_length, UNCOMPRESSED_BUFFER_MAX_SIZE);
        return BITPUNCH_DATA_ERROR;
    }
    uncompressed = malloc_safe(uncompressed_length);
    snappy_ret = snappy_uncompress(data, span_size,
                                   uncompressed, &uncompressed_length);
    if (SNAPPY_OK != snappy_ret) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &filter->loc,
                       "error from snappy_uncompress() -> %d",
                       snappy_ret);
        return BITPUNCH_DATA_ERROR;
    }
    read_value->type = EXPR_VALUE_TYPE_BYTES;
    read_value->bytes.buf = uncompressed;
    read_value->bytes.len = uncompressed_length;
    return BITPUNCH_OK;
}

static struct filter_instance *
snappy_filter_instance_build(
    struct ast_node_hdl *filter,
    struct compile_ctx *ctx)
{
    filter->ndat->u.rexpr_filter.read_func = snappy_read;
    return new_safe(struct filter_instance);
}

void
filter_class_declare_snappy(void)
{
    int ret;

    ret = filter_class_declare("snappy",
                              EXPR_VALUE_TYPE_BYTES,
                              snappy_filter_instance_build,
                              0);
    assert(0 == ret);
}
