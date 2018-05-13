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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <error.h>
#include <errno.h>
#include <fcntl.h>
#include <assert.h>
#include <err.h>
#include <string.h>
#include <check.h>

#include "api/bitpunch_api.h"
#include "core/browse.h"
#include "core/print.h"
#include "check_tracker.h"

static const char *check_cond_def =
    "let u8 = [1] byte: integer { signed: false; };\n"
    "let u16_le = [2] byte: integer { signed: false; endian: 'little'; };\n"
    "let u32 = [4] byte: integer { signed: false; endian: 'big'; };\n"
    "let u32_le = [4] byte: integer { signed: false; endian: 'little'; };\n"
    "let IfStruct = struct {\n"
    "    hdr: struct {\n"
    "        magic: u32;\n"
    "        size: u32;\n"
    "    };\n"
    "    if (hdr.magic == 0) {\n"
    "        version: u32;\n"
    "        values: [] u32;\n"
    "    } else {\n"
    "        contents: [] byte;\n"
    "    }\n"
    "    $span: sizeof (hdr) + hdr.size;\n"
    "};\n"
    "file {\n"
    "    ifs: [2] IfStruct;\n"
    "}\n";

static struct bitpunch_schema_hdl *check_cond_def_hdl;


static const char check_cond_valid1_contents[] = {
    0x0,0x0,0x0,0x1, /* hdr.magic */
    0x0,0x0,0x0,0x5, /* hdr.size */
    0x1,0x2,0x3,0x4,0x5, /* contents */

    0x0,0x0,0x0,0x0, /* hdr.magic */
    0x0,0x0,0x0,0x10, /* hdr.size */
    0x0,0x0,0x0,0x2A, /* version */
    0x0,0x0,0x0,0x1, 0x0,0x0,0x0,0x2, 0x0,0x0,0x0,0x3 /* contents */
};

static const struct test_tracker_expect_box check_cond_valid1_expect[] = {
    { "ifs", 0, 37,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "ifs", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 2 },

    { "ifs[0]", 0, 13,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 0 },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 2 },
    { "ifs[0].hdr", 0, 8,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "hdr", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 2 },
    { "ifs[0].hdr.magic", 0, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "magic", .len = 5 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 1 } },
    { "ifs[0].hdr.size", 4, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "size", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 5 } },
    { "ifs[0].contents", 8, 5,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "contents", .len = 8 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "\x01\x02\x03\x04\x05", .len = 5 } } },

    { "ifs[1]", 13, 24,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 1 },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 3 },
    { "ifs[1].hdr", 13, 8,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "hdr", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 2 },
    { "ifs[1].hdr.magic", 13, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "magic", .len = 5 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 0 } },
    { "ifs[1].hdr.size", 17, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "size", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 16 } },
    { "ifs[1].version", 21, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "version", .len = 7 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 42 } },
    { "ifs[1].values", 25, 12,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "values", .len = 6 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 3 },
    { "ifs[1].values[0]", 25, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 0 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 1 } },
    { "ifs[1].values[1]", 29, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 1 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 2 } },
    { "ifs[1].values[2]", 33, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 2 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 3 } },
};

static const struct test_tracker_spec check_cond_valid1_spec = {
    .test_name = "cond.valid1",
    .contents_def = &check_cond_def_hdl,
    .contents = check_cond_valid1_contents,
    .contents_size = sizeof (check_cond_valid1_contents),
    .expect_boxes = check_cond_valid1_expect,
    .n_expect_boxes = N_ELEM(check_cond_valid1_expect),
};


static void cond_setup(void)
{
    int ret;

    ret = bitpunch_load_schema_from_string(check_cond_def,
                                            &check_cond_def_hdl);
    assert(0 == ret);
}

static void cond_teardown(void)
{
    bitpunch_free_schema(check_cond_def_hdl);
}


START_TEST(cond_valid1)
{
    check_tracker_launch_test(&check_cond_valid1_spec);
}
END_TEST

void check_cond_add_tcases(Suite *s)
{
    TCase *tc_cond;

    tc_cond = tcase_create("cond.valid1");
    tcase_add_unchecked_fixture(tc_cond, cond_setup, cond_teardown);
    tcase_add_test(tc_cond, cond_valid1);
    suite_add_tcase(s, tc_cond);
}
