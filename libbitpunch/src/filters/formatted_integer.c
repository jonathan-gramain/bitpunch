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

#include "core/filter.h"

#define REF_BASE        0
#define REF_SIGNED      1
#define REF_EMPTY_VALUE 2

#define PLUS_SIGN -2
#define MINUS_SIGN -3

/* lookup table indexed by ascii value of each base64-encoded char */
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

static int
formatted_integer_read(struct ast_node_hdl *rcall,
                       expr_value_t *read_value,
                       const char *data, size_t span_size,
                       int *attr_is_specified, expr_value_t *attr_value)
{
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

    if (attr_is_specified[REF_BASE]) {
        base = attr_value[REF_BASE].integer;
    } else {
        base = 10;
    }
    if (attr_is_specified[REF_SIGNED]) {
        _signed = attr_value[REF_SIGNED].boolean;
    } else {
        _signed = TRUE;
    }
    if (0 == span_size) {
        if (attr_is_specified[REF_EMPTY_VALUE]) {
            read_value->type = EXPR_VALUE_TYPE_INTEGER;
            read_value->integer = attr_value[REF_EMPTY_VALUE].integer;
            return 0;
        }
        semantic_error(SEMANTIC_LOGLEVEL_ERROR,
                       NULL != rcall ? &rcall->loc : NULL,
                       "invalid formatted integer input buffer: "
                       "empty buffer");
        return -1;
    }
    negative = FALSE;
    in = (const unsigned char *)data;
    end = in + span_size;
    for (; in < end; ++in) {
        lookup_value = lookup[*in];
        switch (lookup_value) {
        case PLUS_SIGN:
            if (!_signed) {
                goto invalid;
            }
            break ;
        case MINUS_SIGN:
            if (!_signed) {
                goto invalid;
            }
            negative = !negative;
            break ;
        case -1:
            goto invalid;
        default:
            goto parse;
        }
    }
    // no digit
    goto invalid;

  parse:
    parsed_value = 0;
    for (; in < end; ++in) {
        lookup_value = lookup[*in];
        switch (lookup_value) {
        case PLUS_SIGN:
        case MINUS_SIGN:
        case -1:
            goto invalid;
        default:
            break ;
        }
        if (lookup_value >= base) {
            goto invalid;
        }
        prev_parsed_value = parsed_value;
        parsed_value *= base;
        parsed_value += lookup_value;
        if (parsed_value < prev_parsed_value) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR,
                           NULL != rcall ? &rcall->loc : NULL,
                           "invalid formatted integer input buffer: "
                           "overflows a 64-bit signed integer value");
            return -1;
        }
    }
    read_value->type = EXPR_VALUE_TYPE_INTEGER;
    read_value->integer = negative ? -parsed_value : parsed_value;
    return 0;

  invalid:
    semantic_error(SEMANTIC_LOGLEVEL_ERROR,
                   NULL != rcall ? &rcall->loc : NULL,
                   "invalid formatted integer input buffer: "
                   "not a valid formatted integer");
    return -1;
}

static int
formatted_integer_write(struct ast_node_hdl *rcall,
                        const expr_value_t *write_value,
                        char *data, size_t span_size,
                        int *attr_is_specified, expr_value_t *attr_value)
{
    return -1;
}


static int
formatted_integer_rcall_build(struct ast_node_hdl *rcall,
                              const struct statement_list *attribute_list,
                              struct compile_ctx *ctx)
{
    rcall->ndat->u.rexpr_interpreter.read_func = formatted_integer_read;
    rcall->ndat->u.rexpr_interpreter.write_func = formatted_integer_write;
    return 0;
}

void
interpreter_declare_formatted_integer(void)
{
    int ret;

    ret = interpreter_declare("formatted_integer",
                              EXPR_VALUE_TYPE_INTEGER,
                              formatted_integer_rcall_build,
                              3,
                              "@base", REF_BASE,
                              EXPR_VALUE_TYPE_INTEGER, 0,
                              "@empty_value", REF_EMPTY_VALUE,
                              EXPR_VALUE_TYPE_INTEGER, 0,
                              "@signed", REF_SIGNED,
                              EXPR_VALUE_TYPE_BOOLEAN, 0);
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
    int attr_is_specified[3];
    expr_value_t attr_value[3];
    int ret;

    attr_is_specified[REF_BASE] = base != -1;
    attr_is_specified[REF_EMPTY_VALUE] = empty_value != -1;
    attr_is_specified[REF_SIGNED] = _signed != -1;
    attr_value[REF_BASE].integer = base;
    attr_value[REF_EMPTY_VALUE].integer = empty_value;
    attr_value[REF_SIGNED].boolean = _signed;

    ret = formatted_integer_read(NULL, resultp,
                                 buffer, strlen(buffer),
                                 attr_is_specified, attr_value);
    if (0 == ret) {
        ck_assert_int_eq(resultp->type, EXPR_VALUE_TYPE_INTEGER);
    }
    return ret;
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
