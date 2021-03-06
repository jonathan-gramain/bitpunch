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
snappy_read(
    struct ast_node_hdl *filter,
    struct box *scope,
    const char *buffer, size_t buffer_size,
    expr_value_t *valuep,
    struct browse_state *bst)
{
    snappy_status snappy_ret;
    size_t uncompressed_length;
    struct bitpunch_data_source *ds;

    snappy_ret = snappy_uncompressed_length(buffer, buffer_size,
                                            &uncompressed_length);
    if (SNAPPY_OK != snappy_ret) {
        return node_error(BITPUNCH_DATA_ERROR, filter, bst,
                          "error from snappy_uncompressed_length() -> %d",
                          snappy_ret);
    }
    if (uncompressed_length > UNCOMPRESSED_BUFFER_MAX_SIZE) {
        return node_error(BITPUNCH_DATA_ERROR, filter, bst,
                          "snappy uncompressed buffer too large "
                          "(%zu bytes, max %d)",
                          uncompressed_length, UNCOMPRESSED_BUFFER_MAX_SIZE);
    }
    bitpunch_buffer_new(&ds, uncompressed_length);
    snappy_ret = snappy_uncompress(buffer, buffer_size,
                                   (char *)ds->ds_data, &ds->ds_data_length);
    if (SNAPPY_OK != snappy_ret) {
        return node_error(BITPUNCH_DATA_ERROR, filter, bst,
                          "error from snappy_uncompress() -> %d",
                          snappy_ret);
    }
    *valuep = expr_value_as_data(ds);
    return BITPUNCH_OK;
}

static struct filter_instance *
snappy_filter_instance_build(struct ast_node_hdl *filter)
{
    struct filter_instance *f_instance;

    f_instance = new_safe(struct filter_instance);
    f_instance->b_item.read_value_from_buffer = snappy_read;
    return f_instance;
}

void
builtin_filter_declare_snappy(void)
{
    int ret;

    ret = builtin_filter_declare("snappy",
                               EXPR_VALUE_TYPE_BYTES,
                               snappy_filter_instance_build, NULL,
                               0u,
                               0);
    assert(0 == ret);
}
