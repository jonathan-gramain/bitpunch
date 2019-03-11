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
#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>

#include "core/filter.h"
#include "core/print.h"

#define PLUS_SIGN -2
#define MINUS_SIGN -3

/* lookup table indexed by ascii value of each character */
static const char lookup[256] = {
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, PLUS_SIGN, -1, MINUS_SIGN, -1, -1,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
    -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,

    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
};

static bitpunch_status_t
node_error_with_data_context(
    struct ast_node_hdl *filter,
    struct browse_state *bst,
    const char *data, size_t data_len,
    const char *fmt,
    ...)
{
    va_list ap;
    FILE *data_loc_stream;
    char *data_loc_str;
    size_t data_loc_str_len;

    if (NULL == bst) {
        return BITPUNCH_DATA_ERROR;
    }
    browse_state_clear_error(bst);

    va_start(ap, fmt);
    bst->last_error = bitpunch_error_new(
        BITPUNCH_DATA_ERROR, NULL, NULL, filter, fmt, ap);
    va_end(ap);

    data_loc_stream = open_memstream(&data_loc_str, &data_loc_str_len);
    if (NULL == data_loc_stream) {
        return BITPUNCH_ERROR;
    }
    fprintf(data_loc_stream, " when parsing: \"");
    print_bytes(data, data_len, data_loc_stream, 20);
    fprintf(data_loc_stream, "\"");
    fclose(data_loc_stream);
    bitpunch_error_add_node_context(filter, bst, "%s", data_loc_str);
    free(data_loc_str);
    return BITPUNCH_DATA_ERROR;
}

static bitpunch_status_t
formatted_integer_read(struct ast_node_hdl *filter,
                       struct box *scope,
                       expr_value_t *read_value,
                       const char *data, size_t span_size,
                       struct browse_state *bst)
{
    static const char *preamble = "invalid formatted integer";
    bitpunch_status_t bt_ret;
    expr_value_t attr_value;
    int base;
    int64_t parsed_value;
    int64_t prev_parsed_value;
    int lookup_value;
    const unsigned char *in;
    const unsigned char *end;
    int _signed;
    int negative;

    // we're not using strtoll() because that would involve using a
    // temporary null-terminated buffer, and strtoll() is too lax
    // regarding validity checks for our purpose: basically the buffer
    // has to represent a valid formatted number in its entirety.

    bt_ret = filter_evaluate_attribute_internal(
        filter, scope, "@base", NULL, &attr_value, NULL, bst);
    if (BITPUNCH_OK == bt_ret) {
        base = attr_value.integer;
    } else if (BITPUNCH_NO_ITEM == bt_ret) {
        base = 10;
    } else {
        return bt_ret;
    }
    bt_ret = filter_evaluate_attribute_internal(
        filter, scope, "@signed", NULL, &attr_value, NULL, bst);
    if (BITPUNCH_OK == bt_ret) {
        _signed = attr_value.boolean;
    } else if (BITPUNCH_NO_ITEM == bt_ret) {
        _signed = TRUE;
    } else {
        return bt_ret;
    }
    if (0 == span_size) {
        bt_ret = filter_evaluate_attribute_internal(
            filter, scope, "@empty_value", NULL, read_value, NULL, bst);
        if (BITPUNCH_NO_ITEM != bt_ret) {
            return bt_ret;
        }
        return node_error_with_data_context(
            filter, bst, data, span_size,
            "%s: empty buffer", preamble);
    }
    negative = FALSE;
    in = (const unsigned char *)data;
    end = in + span_size;
    for (; in < end; ++in) {
        lookup_value = lookup[*in];
        switch (lookup_value) {
        case PLUS_SIGN:
            if (!_signed) {
                return node_error_with_data_context(
                    filter, bst, data, span_size,
                    "%s: found a sign but expected unsigned integer",
                    preamble);
            }
            break ;
        case MINUS_SIGN:
            if (!_signed) {
                return node_error_with_data_context(
                    filter, bst, data, span_size,
                    "%s: found a sign but expected unsigned integer",
                    preamble);
            }
            negative = !negative;
            break ;
        case -1:
            return node_error_with_data_context(
                filter, bst, data, span_size,
                "%s: invalid digit", preamble);
        default:
            goto parse;
        }
    }
    return node_error_with_data_context(
        filter, bst, data, span_size,
        "%s: no digit found", preamble);

  parse:
    parsed_value = 0;
    for (; in < end; ++in) {
        lookup_value = lookup[*in];
        switch (lookup_value) {
        case PLUS_SIGN:
        case MINUS_SIGN:
        case -1:
            return node_error_with_data_context(
                filter, bst, data, span_size,
                "%s: invalid digit", preamble);
        default:
            break ;
        }
        if (lookup_value >= base) {
            return node_error_with_data_context(
                filter, bst, data, span_size,
                "%s: digit not in base %d", preamble, base);
        }
        prev_parsed_value = parsed_value;
        parsed_value *= base;
        parsed_value += lookup_value;
        if (parsed_value < prev_parsed_value) {
            return node_error_with_data_context(
                filter, bst, data, span_size,
                "unsupported formatted integer: "
                "overflows a 64-bit signed integer value");
        }
    }
    read_value->type = EXPR_VALUE_TYPE_INTEGER;
    read_value->integer = negative ? -parsed_value : parsed_value;
    return BITPUNCH_OK;
}

static struct filter_instance *
formatted_integer_filter_instance_build(struct ast_node_hdl *filter)
{
    struct filter_instance *f_instance;

    f_instance = new_safe(struct filter_instance);
    f_instance->read_func = formatted_integer_read;
    return f_instance;
}

void
filter_class_declare_formatted_integer(void)
{
    int ret;

    ret = filter_class_declare("formatted_integer",
                               EXPR_VALUE_TYPE_INTEGER,
                               formatted_integer_filter_instance_build, NULL,
                               0u,
                               3,
                               "@base", EXPR_VALUE_TYPE_INTEGER, 0,
                               "@empty_value", EXPR_VALUE_TYPE_INTEGER, 0,
                               "@signed", EXPR_VALUE_TYPE_BOOLEAN, 0);
    assert(0 == ret);
}


#ifndef DISABLE_UTESTS

#include <check.h>
#include <stdlib.h>

static int
formatted_integer_read_test(expr_value_t *resultp,
                            const char *buffer,
                            int base,
                            int empty_value,
                            int _signed)
{
    struct filter_class *filter_cls;
    struct ast_node_hdl *filter;
    bitpunch_status_t bt_ret;

    filter_cls = filter_class_lookup("formatted_integer");
    assert(NULL != filter_cls);
    filter = ast_node_hdl_new();
    filter_instance_build(filter, filter_cls,
                          filter_def_create_empty("formatted_integer"));
    if (base != -1) {
        filter_attach_native_attribute(filter, "@base",
                                       expr_value_as_integer(base));
    }
    if (empty_value != -1) {
        filter_attach_native_attribute(filter, "@empty_value",
                                       expr_value_as_integer(empty_value));
    }
    if (_signed != -1) {
        filter_attach_native_attribute(filter, "@signed",
                                       expr_value_as_boolean(_signed));
    }
    bt_ret = formatted_integer_read(filter, NULL, resultp,
                                    buffer, strlen(buffer), NULL);
    if (BITPUNCH_OK == bt_ret) {
        ck_assert_int_eq(resultp->type, EXPR_VALUE_TYPE_INTEGER);
        return 0;
    }
    ck_assert_int_eq(bt_ret, BITPUNCH_DATA_ERROR);
    return -1;
}

START_TEST(test_formatted_integer)
{
    int ret;
    expr_value_t result;

    ret = formatted_integer_read_test(&result, "42", -1, -1, -1);
    ck_assert_int_eq(ret, 0);
    ck_assert_int_eq(result.integer, 42);

    ret = formatted_integer_read_test(&result, "-42", -1, -1, -1);
    ck_assert_int_eq(ret, 0);
    ck_assert_int_eq(result.integer, -42);

    ret = formatted_integer_read_test(&result, "0", -1, -1, -1);
    ck_assert_int_eq(ret, 0);
    ck_assert_int_eq(result.integer, 0);

    ret = formatted_integer_read_test(&result, "899999999997", -1, -1, -1);
    ck_assert_int_eq(ret, 0);
    ck_assert_int_eq(result.integer, 899999999997ll);

    ret = formatted_integer_read_test(&result,
                                      "9223372036854775807", -1, -1, -1);
    ck_assert_int_eq(ret, 0);
    ck_assert_int_eq(result.integer, 9223372036854775807ll);

    ret = formatted_integer_read_test(&result,
                                      "9223372036854775808", -1, -1, -1);
    ck_assert_int_eq(ret, -1); // overflow

    ret = formatted_integer_read_test(&result, "", -1, -1, -1);
    ck_assert_int_eq(ret, -1); // empty buffer

    ret = formatted_integer_read_test(&result, "", -1, 42, -1);
    ck_assert_int_eq(ret, 0);
    ck_assert_int_eq(result.integer, 42);

    ret = formatted_integer_read_test(&result, "-", -1, -1, -1);
    ck_assert_int_eq(ret, -1); // invalid

    ret = formatted_integer_read_test(&result, "ABCD", -1, -1, -1);
    ck_assert_int_eq(ret, -1); // invalid

    ret = formatted_integer_read_test(&result, "123A", -1, -1, -1);
    ck_assert_int_eq(ret, -1); // invalid

    ret = formatted_integer_read_test(&result, "123ABC", 16, -1, -1);
    ck_assert_int_eq(ret, 0);
    ck_assert_int_eq(result.integer, 0x123ABC);

    ret = formatted_integer_read_test(&result, "123456", 8, -1, -1);
    ck_assert_int_eq(ret, 0);
    ck_assert_int_eq(result.integer, 0123456);

    ret = formatted_integer_read_test(&result, "12345678", 8, -1, -1);
    ck_assert_int_eq(ret, -1);

    ret = formatted_integer_read_test(&result, " 123456", 8, -1, -1);
    ck_assert_int_eq(ret, -1);

    ret = formatted_integer_read_test(&result, "123456 ", -1, -1, -1);
    ck_assert_int_eq(ret, -1);

    ret = formatted_integer_read_test(&result, "0123456", -1, -1, -1);
    ck_assert_int_eq(ret, 0);
    ck_assert_int_eq(result.integer, 123456);

    ret = formatted_integer_read_test(&result, "0123456", 10, -1, -1);
    ck_assert_int_eq(ret, 0);
    ck_assert_int_eq(result.integer, 123456);

    ret = formatted_integer_read_test(&result, "-123456", -1, -1, TRUE);
    ck_assert_int_eq(ret, 0);
    ck_assert_int_eq(result.integer, -123456);

    ret = formatted_integer_read_test(&result, "-123456", -1, -1, FALSE);
    ck_assert_int_eq(ret, -1);

    ret = formatted_integer_read_test(&result, "123456", -1, -1, FALSE);
    ck_assert_int_eq(ret, 0);
    ck_assert_int_eq(result.integer, 123456);
}
END_TEST

void check_formatted_integer_add_tcases(Suite *s)
{
    TCase *tc_formatted_integer;

    tc_formatted_integer = tcase_create("formatted_integer");
    tcase_add_test(tc_formatted_integer, test_formatted_integer);
    suite_add_tcase(s, tc_formatted_integer);
}

#endif // #ifndef DISABLE_UTESTS
