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

#define REF_BOUNDARY  0

static int
string_get_size_byte_array_single_char_boundary(
    size_t *sizep,
    const char *data, size_t max_span_size,
    const struct ast_node_hdl *param_values)
{
    const char *end;

    end = memchr(
        data, param_values[REF_BOUNDARY].ndat->u.rexpr_native.value.string.str[0],
        max_span_size);
    if (NULL != end) {
        *sizep = end - data + 1;
    } else {
        *sizep = max_span_size;
    }
    return 0;
}

static int
string_get_size_byte_array_multi_char_boundary(
    size_t *sizep,
    const char *data, size_t max_span_size,
    const struct ast_node_hdl *param_values)
{
    const char *end;

    end = memmem(data, max_span_size,
                 param_values[REF_BOUNDARY].ndat->u.rexpr_native.value.string.str,
                 param_values[REF_BOUNDARY].ndat->u.rexpr_native.value.string.len);
    if (NULL != end) {
        *sizep = end - data
            + param_values[REF_BOUNDARY].ndat->u.rexpr_native.value.string.len;
    } else {
        *sizep = max_span_size;
    }
    return 0;
}


static int
string_read_byte_array_no_boundary(
    union expr_value *read_value,
    const char *data, size_t span_size,
    const struct ast_node_hdl *param_values)
{
    read_value->string.str = (char *)data;
    read_value->string.len = span_size;
    return 0;
}

static int
string_read_byte_array_single_char_boundary(
    union expr_value *read_value,
    const char *data, size_t span_size,
    const struct ast_node_hdl *param_values)
{
    read_value->string.str = (char *)data;
    if (span_size >= 1
        && (data[span_size - 1]
            == param_values[REF_BOUNDARY].ndat->u.rexpr_native.value.string.str[0])) {
        read_value->string.len = span_size - 1;
    } else {
        read_value->string.len = span_size;
    }
    return 0;
}

static int
string_read_byte_array_multi_char_boundary(
    union expr_value *read_value,
    const char *data, size_t span_size,
    const struct ast_node_hdl *param_values)
{
    struct expr_value_string boundary;

    boundary = param_values[REF_BOUNDARY].ndat->u.rexpr_native.value.string;

    read_value->string.str = (char *)data;
    if (span_size >= boundary.len
        && 0 == memcmp(data + span_size - boundary.len,
                       boundary.str, boundary.len)) {
        read_value->string.len = span_size - boundary.len;
    } else {
        read_value->string.len = span_size;
    }
    return 0;
}


static int
string_write_byte_array(const union expr_value *write_value,
                        char *data, size_t span_size,
                        const struct ast_node_hdl *param_values)
{
    return -1;
}


static int
string_rcall_build(struct ast_node_hdl *rcall,
                   const struct ast_node_hdl *data_source,
                   const struct ast_node_hdl *param_values,
                   struct compile_ctx *ctx)
{
    struct expr_value_string boundary;

    assert(param_values[REF_BOUNDARY].ndat->u.rexpr.value_type
           == EXPR_VALUE_TYPE_STRING ||
           param_values[REF_BOUNDARY].ndat->u.rexpr.value_type
           == EXPR_VALUE_TYPE_UNSET);

    // default read function, may be overriden next
    rcall->ndat->u.rexpr_interpreter.read_func =
        string_read_byte_array_no_boundary;

    if (param_values[REF_BOUNDARY].ndat->u.rexpr.value_type
        != EXPR_VALUE_TYPE_UNSET) {
        if (param_values[REF_BOUNDARY].ndat->u.rexpr.value_type
            != EXPR_VALUE_TYPE_STRING) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &rcall->loc,
                "string boundary must be a string, not \"%s\"",
                expr_value_type_str(
                    param_values[REF_BOUNDARY].ndat->u.rexpr.value_type));
            return -1;
        }
        boundary = param_values[REF_BOUNDARY].ndat->u.rexpr_native.value.string;
        switch (boundary.len) {
        case 0:
            /* keep default byte array size */
            break ;
        case 1:
            rcall->ndat->u.rexpr_interpreter.get_size_func =
                string_get_size_byte_array_single_char_boundary;
            rcall->ndat->u.rexpr_interpreter.read_func =
                string_read_byte_array_single_char_boundary;
            break ;
        default: /* two or more characters boundary */
            rcall->ndat->u.rexpr_interpreter.get_size_func =
                string_get_size_byte_array_multi_char_boundary;
            rcall->ndat->u.rexpr_interpreter.read_func =
                string_read_byte_array_multi_char_boundary;
            break ;
        }
    }
    rcall->ndat->u.rexpr_interpreter.write_func = string_write_byte_array;
    return 0;
}

void
interpreter_declare_string(void)
{
    int ret;

    //TODO add regex boundary support
    ret = interpreter_declare("string",
                              EXPR_VALUE_TYPE_STRING,
                              string_rcall_build,
                              1,
                              "boundary", REF_BOUNDARY,
                              EXPR_VALUE_TYPE_STRING,
                              0);
    assert(0 == ret);
}
