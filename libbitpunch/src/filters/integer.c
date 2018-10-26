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
#include <sys/types.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>
#include <endian.h>

#include "core/filter.h"

enum endian {
    ENDIAN_BAD = -1,
    ENDIAN_BIG,
    ENDIAN_LITTLE,
    ENDIAN_NATIVE,
    ENDIAN_DEFAULT = ENDIAN_BIG
};

static enum endian str2endian(struct expr_value_string string)
{
#define MAP(VALUE_STR, VALUE)                                           \
    if (string.len == strlen(VALUE_STR)                                 \
        && 0 == memcmp(string.str, VALUE_STR, string.len)) return VALUE
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


#if 0 // optimized code that sets the ad-hoc function depending on the
      // source, keep for later

#define GEN_RW_FUNCS(NBITS, ENDIAN_STR, SIGN)                           \
    static int                                                          \
    binary_integer_read_##SIGN##int##NBITS##_##ENDIAN_STR(              \
        expr_value_t *read_value,                                  \
        const char *data, size_t span_size,                             \
        const struct ast_node_hdl *attr_values)                            \
    {                                                                   \
        read_value->integer = (int64_t)(SIGN##int##NBITS##_t)ENDIAN_STR##NBITS##toh(*(uint##NBITS##_t *)data); \
        return 0;                                                       \
    }                                                                   \
                                                                        \
    static int                                                          \
    binary_integer_write_##SIGN##int##NBITS##_##ENDIAN_STR(             \
        const expr_value_t *write_value,                           \
        char *data, size_t span_size,                                   \
        const struct ast_node_hdl *attr_values)                            \
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
binary_integer_filter_instance_build(struct ast_node_hdl *filter,
                           const struct ast_node_hdl *data_source,
                           const struct ast_node_hdl *attr_values,
                           struct compile_ctx *ctx)
{
    struct ast_node_hdl *size_value;
    struct ast_node_hdl *endian_value;
    int64_t size;
    int _signed;
    enum endian endian;
    filter_read_func_t read_func;
    filter_write_func_t write_func;

    assert(attr_values[REF_SIGNED].ndat->u.rexpr.value_type
           == EXPR_VALUE_TYPE_BOOLEAN);
    assert(attr_values[REF_ENDIAN].ndat->u.rexpr.value_type
           == EXPR_VALUE_TYPE_STRING ||
           attr_values[REF_ENDIAN].ndat->u.rexpr.value_type
           == EXPR_VALUE_TYPE_UNSET);
    assert(NULL != data_source);

    switch (data_source->ndat->type) {
    case AST_NODE_TYPE_BYTE_ARRAY:
        size_value = ast_node_get_named_expr_target(
            data_source->ndat->u.byte_array.size);
        if (NULL != size_value) {
            if (-1 == compile_expr(size_value, ctx, TRUE)) {
                return -1;
            }
            assert(ast_node_is_rexpr(size_value));
        }
        if (NULL == size_value
            || AST_NODE_TYPE_REXPR_NATIVE != size_value->ndat->type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &filter->loc,
                "integer filter only supports fixed-sized "
                "byte arrays");
            return -1;
        }
        assert(EXPR_VALUE_TYPE_INTEGER == size_value->ndat->u.rexpr.value_type);
        size = size_value->ndat->u.rexpr_native.value.integer;
        break ;

    case AST_NODE_TYPE_BYTE:
        size = 1;
        break ;

    default:
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &filter->loc,
            "integer filter can only interpret byte or byte array "
            "types, not %s",
            ast_node_type_str(data_source->ndat->type));
        return -1;
    }
    _signed = ast_node_get_named_expr_target(
        (struct ast_node_hdl *)&attr_values[REF_SIGNED])
        ->ndat->u.rexpr_native.value.boolean;
    endian_value = ast_node_get_named_expr_target(
        (struct ast_node_hdl *)&attr_values[REF_ENDIAN]);
    if (endian_value->ndat->u.rexpr.value_type == EXPR_VALUE_TYPE_UNSET) {
        endian = ENDIAN_DEFAULT;
    } else {
        endian = str2endian(endian_value->ndat->u.rexpr_native.value.string);
        if (endian == ENDIAN_BAD) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &attr_values[REF_ENDIAN].loc,
                "bad endian value \"%.*s\": "
                "must be \"big\", \"little\" or \"native\"",
                (int)attr_values[REF_ENDIAN].ndat->u.string.len,
                attr_values[REF_ENDIAN].ndat->u.string.str);
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
                       "size %"PRIi64" not supported by integer filter",
                       size);
        return -1;
    }
    filter->ndat->u.rexpr_filter.read_func = read_func;
    filter->ndat->u.rexpr_filter.write_func = write_func;
    return 0;
}

#else

static bitpunch_status_t
binary_integer_read_generic(
    struct ast_node_hdl *filter,
    expr_value_t *read_value,
    const char *data, size_t span_size,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    expr_value_t attr_value;
    int _signed;
    enum endian endian;

    bt_ret = filter_evaluate_attribute_internal(
        filter, "@signed", NULL, &attr_value, NULL, bst);
    if (BITPUNCH_OK != bt_ret) {
        // @signed is a mandatory attribute so BITPUNCH_NO_ITEM shall
        // be considered an error
        return bt_ret;
    }
    _signed = attr_value.boolean;
    bt_ret = filter_evaluate_attribute_internal(
        filter, "@endian", NULL, &attr_value, NULL, bst);
    if (BITPUNCH_OK == bt_ret) {
        endian = str2endian(attr_value.string);
        if (endian == ENDIAN_BAD) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &filter->loc,
                "bad endian value \"%.*s\": "
                "must be \"big\", \"little\" or \"native\"",
                (int)attr_value.string.len, attr_value.string.str);
            return BITPUNCH_INVALID_PARAM;
        }
    } else if (BITPUNCH_NO_ITEM == bt_ret) {
        endian = ENDIAN_DEFAULT;
    } else {
        return bt_ret;
    }
    if (endian == ENDIAN_NATIVE) {
        if (is_little_endian()) {
            endian = ENDIAN_LITTLE;
        } else {
            endian = ENDIAN_BIG;
        }
    }
    assert(endian == ENDIAN_BIG || endian == ENDIAN_LITTLE);

#define READ_BRANCH_3(NBITS, ENDIAN_STR, SIGN) do {                     \
        read_value->integer = (int64_t)(SIGN##int##NBITS##_t)ENDIAN_STR##NBITS##toh(*(uint##NBITS##_t *)data); \
    } while (0)

#define READ_BRANCH_2(NBITS, ENDIAN, ENDIAN_STR)        \
    case ENDIAN:                                        \
        if (_signed) {                                  \
            READ_BRANCH_3(NBITS, ENDIAN_STR, );         \
        } else {                                        \
            READ_BRANCH_3(NBITS, ENDIAN_STR, u);        \
        }                                               \
        break

#define READ_BRANCH_1(NBITS)                            \
    case NBITS / NBBY:                                  \
        switch (endian) {                               \
            READ_BRANCH_2(NBITS, ENDIAN_BIG, be);       \
            READ_BRANCH_2(NBITS, ENDIAN_LITTLE, le);    \
        default: assert(0);                             \
        }                                               \
        break

    read_value->type = EXPR_VALUE_TYPE_INTEGER;
    switch (span_size) {
        READ_BRANCH_1(8);
        READ_BRANCH_1(16);
        READ_BRANCH_1(32);
        READ_BRANCH_1(64);
    default:
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &filter->loc,
                       "size %"PRIi64" not supported by integer filter",
                       span_size);
        return BITPUNCH_NOT_IMPLEMENTED;
    }
    return BITPUNCH_OK;
}

static struct filter_instance *
binary_integer_filter_instance_build(struct ast_node_hdl *filter)
{
    struct filter_instance *f_instance;

    f_instance = new_safe(struct filter_instance);
    f_instance->read_func = binary_integer_read_generic;
    return f_instance;
}

#endif // optimized code

void
filter_class_declare_binary_integer(void)
{
    int ret;

    ret = filter_class_declare("integer",
                               EXPR_VALUE_TYPE_INTEGER,
                               binary_integer_filter_instance_build, NULL,
                               2,
                               "@signed", EXPR_VALUE_TYPE_BOOLEAN,
                               FILTER_ATTR_FLAG_MANDATORY,
                               "@endian", EXPR_VALUE_TYPE_STRING, 0);
    assert(0 == ret);
}
