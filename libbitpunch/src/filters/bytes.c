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
#include <string.h>
#include <assert.h>

#include "core/filter.h"

static bitpunch_status_t
bytes_read(
    struct ast_node_hdl *filter,
    struct box *scope,
    expr_value_t *read_value,
    const char *data, size_t span_size,
    int *attr_is_specified, expr_value_t *attr_value,
    struct browse_state *bst)
{
    read_value->type = EXPR_VALUE_TYPE_BYTES;
    read_value->bytes.buf = (char *)data;
    read_value->bytes.len = span_size;
    return BITPUNCH_OK;
}


static int
bytes_filter_instance_build(struct ast_node_hdl *filter,
                            const struct statement_list *attribute_list,
                            struct compile_ctx *ctx)
{
    filter->ndat->u.rexpr_filter.read_func = bytes_read;
    return 0;
}

void
filter_class_declare_bytes(void)
{
    int ret;

    ret = filter_class_declare("bytes",
                               EXPR_VALUE_TYPE_BYTES,
                               bytes_filter_instance_build,
                               0);
    assert(0 == ret);
}
