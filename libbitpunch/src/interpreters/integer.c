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
#include <sys/types.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>
#include <endian.h>

#include "core/interpreter.h"

#define REF_SIGNED  0
#define REF_ENDIAN  1

enum endian {
    ENDIAN_BAD = -1,
    ENDIAN_BIG,
    ENDIAN_LITTLE,
    ENDIAN_NATIVE,
    ENDIAN_DEFAULT = ENDIAN_BIG
};

static enum endian str2endian(const char *str)
{
#define MAP(VALUE_STR, VALUE)                           \
    if (0 == strcasecmp(str, VALUE_STR)) return VALUE
    MAP("big", ENDIAN_BIG);
    MAP("little", ENDIAN_LITTLE);
    MAP("native", ENDIAN_NATIVE);
#undef MAP
    return ENDIAN_BAD;
}

#define be8toh(nb) (nb)
#define le8toh(nb) (nb)
#define htobe8(nb) (nb)
#define htole8(nb) (nb)

#define GEN_RW_FUNCS(NBITS, ENDIAN_STR, SIGN)                           \
    static int                                                          \
    binary_integer_read_##SIGN##int##NBITS##_##ENDIAN_STR(              \
        union expr_value *read_value,                                  \
        const char *data, size_t span_size,                             \
        const struct ast_node *param_values)                            \
    {                                                                   \
        read_value->integer = (int64_t)(SIGN##int##NBITS##_t)ENDIAN_STR##NBITS##toh(*(uint##NBITS##_t *)data); \
        return 0;                                                       \
    }                                                                   \
                                                                        \
    static int                                                          \
    binary_integer_write_##SIGN##int##NBITS##_##ENDIAN_STR(             \
        const union expr_value *write_value,                           \
        char *data, size_t span_size,                                   \
        const struct ast_node *param_values)                            \
    {                                                                   \
        *((uint##NBITS##_t *)data) = hto##ENDIAN_STR##NBITS((uint##NBITS##_t)write_value->integer); \
        return 0;                                                       \
    }

#define GEN_RW_FUNCS_2(NBITS, ENDIAN_STR)       \
    GEN_RW_FUNCS(NBITS, ENDIAN_STR, )           \
    GEN_RW_FUNCS(NBITS, ENDIAN_STR, u)

#define GEN_RW_FUNCS_1(NBITS)                   \
    GEN_RW_FUNCS_2(NBITS, be)                   \
    GEN_RW_FUNCS_2(NBITS, le)

#define GEN_ALL_RW_FUNCS                        \
    GEN_RW_FUNCS_1(8)                           \
    GEN_RW_FUNCS_1(16)                          \
    GEN_RW_FUNCS_1(32)                          \
    GEN_RW_FUNCS_1(64)

GEN_ALL_RW_FUNCS


static int
binary_integer_rcall_build(struct ast_node *rcall,
                           const struct ast_node *call,
                           const struct ast_node *param_values)
{
    const struct ast_node *data_source;
    struct ast_node *size_value;
    int64_t size;
    int _signed;
    enum endian endian;
    interpreter_read_func_t read_func;
    interpreter_write_func_t write_func;

    assert(param_values[REF_SIGNED].type == AST_NODE_TYPE_BOOLEAN);
    assert(param_values[REF_ENDIAN].type == AST_NODE_TYPE_IDENTIFIER ||
           param_values[REF_ENDIAN].type == AST_NODE_TYPE_NONE);

    assert(AST_NODE_TYPE_FILTER == call->type);
    data_source = call->u.filter.target;
    assert(NULL != data_source);
    if (ast_node_is_rexpr(data_source)) {
        if (!ast_node_is_rexpr_to_item(data_source)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &call->loc,
                "integer interpreter only supports expressions resolving "
                "to dpath items (not \"%s\")",
                ast_node_type_str(data_source->type));
            return -1;
        }
        data_source = data_source->u.rexpr.target_item;
        assert(NULL != data_source);
    }
    switch (data_source->type) {
    case AST_NODE_TYPE_BYTE_ARRAY:
        size_value = data_source->u.byte_array.size;
        assert(ast_node_is_rexpr(size_value));
        if (AST_NODE_TYPE_REXPR_NATIVE != size_value->type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &call->loc,
                "integer interpreter only supports fixed-sized "
                "byte arrays");
            return -1;
        }
        assert(EXPR_VALUE_TYPE_INTEGER == size_value->u.rexpr.value_type);
        size = size_value->u.rexpr_native.value.integer;
        break ;

    case AST_NODE_TYPE_BYTE:
        size = 1;
        break ;

    default:
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &call->loc,
            "integer interpreter can only interpret byte or byte array "
            "types, not %s",
            ast_node_type_str(data_source->type));
        return -1;
    }
    _signed = param_values[REF_SIGNED].u.boolean;
    if (param_values[REF_ENDIAN].type == AST_NODE_TYPE_NONE) {
        endian = ENDIAN_DEFAULT;
    } else {
        endian = str2endian(param_values[REF_ENDIAN].u.identifier);
        if (endian == ENDIAN_BAD) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &param_values[REF_ENDIAN].loc,
                "bad endian value \"%s\": "
                "must be \"big\", \"little\" or \"native\"",
                param_values[REF_ENDIAN].u.identifier);
            return -1;
        }
    }
    if (endian == ENDIAN_NATIVE) {
        if (is_little_endian()) {
            endian = ENDIAN_LITTLE;
        } else {
            endian = ENDIAN_BIG;
        }
    }
    assert(endian == ENDIAN_BIG || endian == ENDIAN_LITTLE);

#define SET_RW_FUNCS_3(NBITS, ENDIAN_STR, SIGN) do {                   \
    read_func = binary_integer_read_##SIGN##int##NBITS##_##ENDIAN_STR; \
    write_func = binary_integer_write_##SIGN##int##NBITS##_##ENDIAN_STR; \
} while (0)

#define SET_RW_FUNCS_2(NBITS, ENDIAN, ENDIAN_STR)       \
    case ENDIAN:                                        \
        if (_signed) {                                  \
            SET_RW_FUNCS_3(NBITS, ENDIAN_STR, );        \
        } else {                                        \
            SET_RW_FUNCS_3(NBITS, ENDIAN_STR, u);       \
        }                                               \
        break

#define SET_RW_FUNCS_1(NBITS)                           \
    case NBITS / NBBY:                                  \
        switch (endian) {                               \
            SET_RW_FUNCS_2(NBITS, ENDIAN_BIG, be);      \
            SET_RW_FUNCS_2(NBITS, ENDIAN_LITTLE, le);   \
        default: assert(0);                             \
        }                                               \
        break

    switch (size) {
        SET_RW_FUNCS_1(8);
        SET_RW_FUNCS_1(16);
        SET_RW_FUNCS_1(32);
        SET_RW_FUNCS_1(64);
    default:
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &size_value->loc,
                       "size %"PRIi64" not supported by integer interpreter",
                       size);
        return -1;
    }
    rcall->u.rexpr_interpreter.read_func = read_func;
    rcall->u.rexpr_interpreter.write_func = write_func;
    return 0;
}

void
interpreter_declare_binary_integer(void)
{
    int ret;

    ret = interpreter_declare("integer",
                              EXPR_VALUE_TYPE_INTEGER,
                              binary_integer_rcall_build,
                              2,
                              "signed", REF_SIGNED,
                              AST_NODE_TYPE_BOOLEAN,
                              INTERPRETER_PARAM_FLAG_MANDATORY,
                              "endian", REF_ENDIAN,
                              AST_NODE_TYPE_IDENTIFIER,
                              0);
    assert(0 == ret);
}
