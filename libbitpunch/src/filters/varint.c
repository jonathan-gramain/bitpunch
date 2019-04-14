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
#include <string.h>
#include <assert.h>
#include <stdint.h>
#include <endian.h>

#include "core/filter.h"
#include "filters/integer.h"

static bitpunch_status_t
compute_item_size__varint(
    struct ast_node_hdl *filter,
    struct box *scope,
    const char *buffer, size_t buffer_size,
    int64_t *item_sizep,
    struct browse_state *bst)
{
    size_t bytepos;

    for (bytepos = 0; bytepos < buffer_size; ++bytepos) {
        if (!(buffer[bytepos] & 0x80)) {
            *item_sizep = bytepos + 1;
            return BITPUNCH_OK;
        }
    }
    // invalid varint
    // FIXME add context
    return BITPUNCH_DATA_ERROR;
}

static bitpunch_status_t
varint_read__little_endian(
    const char *buffer, size_t buffer_size, int64_t *valuep)
{
    const unsigned char *ubuffer = (const unsigned char *)buffer;
    size_t bytepos;
    size_t cur_shift;
    uint64_t rawvalue;

    rawvalue = 0;
    cur_shift = 0;
    for (bytepos = 0; bytepos < buffer_size; ++bytepos) {
        rawvalue |= ((uint64_t)ubuffer[bytepos] & 0x7f) << cur_shift;
        if (!(ubuffer[bytepos] & 0x80)) {
            break ;
        }
        cur_shift += 7;
    }
    if (bytepos == buffer_size) {
        // invalid varint
        // FIXME add context
        return BITPUNCH_DATA_ERROR;
    }
    // zigzag encoding
    //value = (rawvalue & 1) ? -((rawvalue + 1) / 2) : (rawvalue / 2);
    *valuep = rawvalue;
    return BITPUNCH_OK;
}

static bitpunch_status_t
varint_read__big_endian(
    const char *buffer, size_t buffer_size, int64_t *valuep)
{
    const unsigned char *ubuffer = (const unsigned char *)buffer;
    size_t bytepos;
    uint64_t rawvalue;

    rawvalue = 0;
    for (bytepos = 0; bytepos < buffer_size; ++bytepos) {
        rawvalue <<= 7;
        rawvalue |= ((uint64_t)ubuffer[bytepos] & 0x7f);
        if (!(ubuffer[bytepos] & 0x80)) {
            break ;
        }
    }
    if (bytepos == buffer_size) {
        // invalid varint
        // FIXME add context
        return BITPUNCH_DATA_ERROR;
    }
    // zigzag encoding
    //value = (rawvalue & 1) ? -((rawvalue + 1) / 2) : (rawvalue / 2);
    *valuep = rawvalue;
    return BITPUNCH_OK;
}

static bitpunch_status_t
varint_read(
    struct ast_node_hdl *filter,
    struct box *scope,
    const char *buffer, size_t buffer_size,
    expr_value_t *valuep,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    enum endian endian;
    int64_t value;

    bt_ret = integer_read_endian_attribute(filter, scope, &endian, bst);
    if (BITPUNCH_NO_ITEM == bt_ret) {
        // default to google's varint convention
        endian = ENDIAN_LITTLE;
    } else if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (ENDIAN_BIG == endian) {
        bt_ret = varint_read__big_endian(buffer, buffer_size, &value);
    } else {
        bt_ret = varint_read__little_endian(buffer, buffer_size, &value);
    }
    if (BITPUNCH_OK == bt_ret) {
        valuep->type = EXPR_VALUE_TYPE_INTEGER;
        valuep->integer = value;
    }
    return bt_ret;
}

static struct filter_instance *
varint_filter_instance_build(struct ast_node_hdl *filter)
{
    struct filter_instance *f_instance;

    f_instance = new_safe(struct filter_instance);
    f_instance->b_item.compute_item_size_from_buffer =
        compute_item_size__varint;
    f_instance->b_item.read_value_from_buffer = varint_read;
    return f_instance;
}

void
filter_class_declare_varint(void)
{
    int ret;

    ret = filter_class_declare("varint",
                               EXPR_VALUE_TYPE_INTEGER,
                               varint_filter_instance_build, NULL,
                               0u,
                               1,
                               "@endian", EXPR_VALUE_TYPE_STRING, 0);
    assert(0 == ret);
}



#ifndef DISABLE_UTESTS

#include <check.h>


struct varint_testcase {
    enum endian endian;
    const char *buffer;
    size_t buffer_size;
    int64_t expected_value;
};

START_TEST(test_filter_varint)
{
#define TCASE_STR(STR) .buffer = STR, .buffer_size = sizeof (STR) - 1

    struct varint_testcase testcases[] = {
        {
            .endian = ENDIAN_LITTLE,
            TCASE_STR("\x00"),
            .expected_value = 0,
        }, {
            .endian = ENDIAN_BIG,
            TCASE_STR("\x00"),
            .expected_value = 0,
        }, {
            .endian = ENDIAN_LITTLE,
            TCASE_STR("\x42"),
            .expected_value = 0x42,
        }, {
            .endian = ENDIAN_BIG,
            TCASE_STR("\x42"),
            .expected_value = 0x42,
        }, {
            .endian = ENDIAN_LITTLE,
            TCASE_STR("\x42\x01"),
            .expected_value = 0x42,
        }, {
            .endian = ENDIAN_BIG,
            TCASE_STR("\x42\x01"),
            .expected_value = 0x42,
        }, {
            .endian = ENDIAN_LITTLE,
            TCASE_STR("\x80\x01"),
            .expected_value = 128,
        }, {
            .endian = ENDIAN_BIG,
            TCASE_STR("\x80\x01"),
            .expected_value = 1,
        }, {
            .endian = ENDIAN_LITTLE,
            TCASE_STR("\x80\x03"),
            .expected_value = 384,
        }, {
            .endian = ENDIAN_BIG,
            TCASE_STR("\x80\x03"),
            .expected_value = 3,
        }, {
            .endian = ENDIAN_LITTLE,
            TCASE_STR("\xc0\x03"),
            .expected_value = 448,
        }, {
            .endian = ENDIAN_BIG,
            TCASE_STR("\xc0\x03"),
            .expected_value = 8195,
        }, {
            .endian = ENDIAN_LITTLE,
            TCASE_STR("\xc0\xd2\x93\xb2\x82\x77"),
            .expected_value = 4089450916160ll,
        }, {
            .endian = ENDIAN_BIG,
            TCASE_STR("\xc0\xd2\x93\xb2\x82\x77"),
            .expected_value = 2221075628407ll,
        },
    };
    struct varint_testcase *tc;
    int i;
    int64_t value;
    bitpunch_status_t bt_ret;

    for (i = 0; i < N_ELEM(testcases); ++i) {
        tc = &testcases[i];
        if (ENDIAN_LITTLE == tc->endian) {
            bt_ret = varint_read__little_endian(
                tc->buffer, tc->buffer_size, &value);
        } else {
            bt_ret = varint_read__big_endian(
                tc->buffer, tc->buffer_size, &value);
        }
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);
        ck_assert_int_eq(value, tc->expected_value);
    }
}
END_TEST

void check_filter_varint_add_tcases(Suite *s)
{
    TCase *tc_filter_varint;

    tc_filter_varint = tcase_create("filter:varint");
    tcase_add_test(tc_filter_varint, test_filter_varint);
    suite_add_tcase(s, tc_filter_varint);
}

#endif // #ifndef DISABLE_UTESTS
