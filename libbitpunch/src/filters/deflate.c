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
#include <zlib.h>

#include "core/filter.h"

#define INFLATED_MAX_SIZE (1024 * 1024 * 1024)

static bitpunch_status_t
deflate_read(
    struct ast_node_hdl *filter,
    struct box *scope,
    const char *buffer, size_t buffer_size,
    expr_value_t *valuep,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int zret;
    z_stream zs;
    expr_value_t attr_value;
    int64_t inflated_size;
    struct bitpunch_data_source *inflated;

    bt_ret = filter_evaluate_attribute_internal(
        filter, scope, "@output_size", 0u, NULL, &attr_value, NULL, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(EXPR_VALUE_TYPE_INTEGER == attr_value.type);
    inflated_size = attr_value.integer;
    if (inflated_size > INFLATED_MAX_SIZE) {
        return node_error(BITPUNCH_DATA_ERROR, filter, bst,
                          "inflated size too large (%zu bytes, max %d)",
                          inflated_size, INFLATED_MAX_SIZE);
    }
    zs.next_in = (z_const Bytef *)buffer;
    zs.avail_in = (uLong)buffer_size;
    zs.zalloc = Z_NULL;
    zs.zfree = Z_NULL;
    zs.opaque = Z_NULL;

    // set windowBits to a negative value to request raw (headerless)
    // inflate, and 2^15 as a higher bound to be compatible with any
    // window size used during compression
    zret = inflateInit2(&zs, -15);
    if (Z_OK != zret) {
        return node_error(BITPUNCH_DATA_ERROR, filter, bst,
                          "error from inflateInit(): %s (%d)",
                          zs.msg, zret);
    }
    bitpunch_buffer_new(&inflated, inflated_size);
    zs.next_out = (Bytef *)inflated->ds_data;
    zs.avail_out = (uInt)inflated_size;
    zret = inflate(&zs, Z_FINISH);
    if (Z_STREAM_END != zret) {
        node_error(BITPUNCH_DATA_ERROR, filter, bst,
                   "error from inflate(): %s (%d)",
                   zs.msg, zret);
        inflateEnd(&zs);
        return BITPUNCH_DATA_ERROR;
    }
    if (zs.avail_out != 0) {
        node_error(BITPUNCH_DATA_ERROR, filter, bst,
                   "inflate() did not fill the output buffer: "
                   "%u bytes left (with %u input bytes unread)",
                   zs.avail_out, zs.avail_in);
        inflateEnd(&zs);
        return BITPUNCH_DATA_ERROR;
    }
    inflateEnd(&zs);
    *valuep = expr_value_as_data(inflated);
    return BITPUNCH_OK;
}

static struct filter_instance *
deflate_filter_instance_build(struct ast_node_hdl *filter)
{
    struct filter_instance *f_instance;

    f_instance = new_safe(struct filter_instance);
    f_instance->b_item.read_value_from_buffer = deflate_read;
    return f_instance;
}

void
builtin_filter_declare_deflate(void)
{
    int ret;

    ret = builtin_filter_declare("deflate",
                               EXPR_VALUE_TYPE_BYTES,
                               deflate_filter_instance_build, NULL,
                               0u,
                               1,
                               "@output_size", EXPR_VALUE_TYPE_INTEGER,
                               FILTER_ATTR_MANDATORY);
    assert(0 == ret);
}
