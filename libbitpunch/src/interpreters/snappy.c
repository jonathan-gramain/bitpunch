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
#include <assert.h>
#include <string.h>
#include <snappy-c.h>

#include "core/interpreter.h"

#define UNCOMPRESSED_BUFFER_MAX_SIZE (1024 * 1024 * 1024)

static int
snappy_read(union expr_value *read_value,
            const char *data, size_t span_size,
            const struct ast_node_hdl *param_values)
{
    size_t uncompressed_length;
    char *uncompressed;

    if (snappy_uncompressed_length(data, span_size, &uncompressed_length)
        !=  SNAPPY_OK) {
        //TODO log
        return -1;
    }
    if (uncompressed_length > UNCOMPRESSED_BUFFER_MAX_SIZE) {
        //TODO log
        return -1;
    }
    uncompressed = malloc_safe(uncompressed_length);
    if (snappy_uncompress(data, span_size,
                          uncompressed, &uncompressed_length)
        != SNAPPY_OK) {
        //TODO log
        return -1;
    }
    read_value->bytes.buf = uncompressed;
    read_value->bytes.len = uncompressed_length;
    return 0;
}

static int
snappy_write(const union expr_value *write_value,
             char *data, size_t span_size,
             const struct ast_node_hdl *param_values)
{
    return -1;
}


static int
snappy_rcall_build(struct ast_node_hdl *rcall,
                   const struct ast_node_hdl *data_source,
                   const struct ast_node_hdl *param_values,
                   struct compile_ctx *ctx)
{
    rcall->ndat->u.rexpr_interpreter.read_func = snappy_read;
    rcall->ndat->u.rexpr_interpreter.write_func = snappy_write;
    return 0;
}

void
interpreter_declare_snappy(void)
{
    int ret;

    ret = interpreter_declare("snappy",
                              EXPR_VALUE_TYPE_BYTES,
                              snappy_rcall_build,
                              0);
    assert(0 == ret);
}
