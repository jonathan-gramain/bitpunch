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

static const char *check_struct_def =
    "let u8 = [1] byte <> integer { @signed: false; };\n"
    "let u16_le = [2] byte <> integer { @signed: false; @endian: 'little'; };\n"
    "let u32 = [4] byte <> integer { @signed: false; @endian: 'big'; };\n"
    "let u32_le = [4] byte <> integer { @signed: false; @endian: 'little'; };\n"
    "let MyStruct = struct {\n"
    "    field1_u32: u32;\n"
    "    field2_byte: byte;\n"
    "    field3_u32: u32;\n"
    "};\n"
    "let MyUnion = union {\n"
    "    field1_u32: u32_le;\n"
    "    field2_byte: byte;\n"
    "    field3_u16: u16_le;\n"
    "    field4_u8: u8;\n"
    "};\n"
    "let Root = struct {\n"
    "    ms1: MyStruct;\n"
    "    ms2: MyStruct;\n"
    "    mu3: MyUnion;\n"
    "    u4: u32;\n"
    "};\n";

static struct ast_node_hdl *check_struct_schema_hdl;


static const char check_struct_valid1_contents[] = {
    0x0,0x0,0x0,0x1,0x2,0x0,0x0,0x0,0x3,
    0x0,0x0,0x0,0x4,0x5,0x0,0x0,0x0,0x6,
    0x7,0x0,0x0,0x0,
    0x0,0x0,0x0,0x8
};

static const struct test_tracker_expect_box check_struct_valid1_expect[] = {
    { "ms1", 0, 9,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "ms1", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 3 },

    { "ms1.field1_u32", 0, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field1_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 1 } },

    { "ms1.field2_byte", 4, 1,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field2_byte", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "\x02", .len = 1 } } },

    { "ms1.field3_u32", 5, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field3_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 3 } },

    { "ms2", 9, 9,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "ms2", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 3 },

    { "ms2.field1_u32", 9, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field1_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 4 } },

    { "ms2.field2_byte", 13, 1,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field2_byte", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "\x05", .len = 1 } } },

    { "ms2.field3_u32", 14, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field3_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 6 } },

    { "mu3", 18, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "mu3", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 4 },

    { "mu3.field1_u32", 18, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field1_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 7 } },

    { "mu3.field2_byte", 18, 1,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field2_byte", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "\x07", .len = 1 } } },

    { "mu3.field3_u16", 18, 2,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field3_u16", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 7 } },

    { "mu3.field4_u8", 18, 1,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field4_u8", .len = 9 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 7 } },

    { "u4", 22, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "u4", .len = 2 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 8 } },
};

static const struct test_tracker_spec check_struct_valid1_spec = {
    .test_name = "struct.valid1",
    .schema_hdl = &check_struct_schema_hdl,
    .contents = check_struct_valid1_contents,
    .contents_size = sizeof (check_struct_valid1_contents),
    .expect_boxes = check_struct_valid1_expect,
    .n_expect_boxes = N_ELEM(check_struct_valid1_expect),
};


static const char check_struct_invalid_truncated1_contents[] = {
    0x0,0x0,0x0,0x1,0x2,0x0,0x0,0x0,0x3,
    0x0,0x0,0x0,0x4,0x5,0x0,0x0,0x0,0x6
};

static const struct test_tracker_expect_box
check_struct_invalid_truncated1_expect[] = {
    { "ms1", 0, 9,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "ms1", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 3 },

    { "ms1.field1_u32", 0, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field1_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 1 } },

    { "ms1.field2_byte", 4, 1,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field2_byte", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "\x02", .len = 1 } } },

    { "ms1.field3_u32", 5, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field3_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 3 } },

    { "ms2", 9, 9,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "ms2", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 3 },

    { "ms2.field1_u32", 9, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field1_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 4 } },

    { "ms2.field2_byte", 13, 1,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field2_byte", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "\x05", .len = 1 } } },

    { "ms2.field3_u32", 14, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field3_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 6 } },

    { "mu3", 18, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "mu3", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 4,
      .read_item_ret = BITPUNCH_OUT_OF_BOUNDS_ERROR },

    { "mu3.field1_u32", 18, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field1_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 7 },
      .read_item_ret = BITPUNCH_OUT_OF_BOUNDS_ERROR },

    { "mu3.field2_byte", 18, 1,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field2_byte", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "\x07", .len = 1 } },
      .read_item_ret = BITPUNCH_OUT_OF_BOUNDS_ERROR },

    { "mu3.field3_u16", 18, 2,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field3_u16", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 7 },
      .read_item_ret = BITPUNCH_OUT_OF_BOUNDS_ERROR },

    { "mu3.field4_u8", 18, 1,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field4_u8", .len = 9 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 7 },
      .read_item_ret = BITPUNCH_OUT_OF_BOUNDS_ERROR },

    { "u4", 22, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "u4", .len = 2 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 8 },
      .read_item_ret = BITPUNCH_OUT_OF_BOUNDS_ERROR },
};

static const struct test_tracker_spec
check_struct_invalid_truncated1_spec = {
    .test_name = "struct.invalid_truncated1",
    .schema_hdl = &check_struct_schema_hdl,
    .contents = check_struct_invalid_truncated1_contents,
    .contents_size = sizeof (check_struct_invalid_truncated1_contents),
    .expect_boxes = check_struct_invalid_truncated1_expect,
    .n_expect_boxes = N_ELEM(check_struct_invalid_truncated1_expect),
    .truncated = TRUE,
};



static const char *check_vstruct_def =
    "let u8 = [1] byte <> integer { @signed: false; };\n"
    "let u16_le = [2] byte <> integer { @signed: false; @endian: 'little'; };\n"
    "let u32 = [4] byte <> integer { @signed: false; @endian: 'big'; };\n"
    "let u32_le = [4] byte <> integer { @signed: false; @endian: 'little'; };\n"
    "let MyStruct = struct {\n"
    "    field1_u32: u32;\n"
    "    field2_bytes: [field1_u32] byte;\n"
    "    field3_u32: u32;\n"
    "};\n"
    "let MyUnion = union {\n"
    "    field1_u32: u32_le;\n"
    "    field2_bytes: [field1_u32] byte;\n"
    "    field3_u16: u16_le;\n"
    "    field4_u8: u8;\n"
    "};\n"
    "let Root = struct {\n"
    "    ms1: MyStruct;\n"
    "    ms2: MyStruct;\n"
    "    mu3: MyUnion;\n"
    "    u4: u32;\n"
    "};\n";

static struct ast_node_hdl *check_vstruct_schema_hdl;


static const char check_vstruct_valid1_contents[] = {
    0x0,0x0,0x0,0x1,0x2,0x0,0x0,0x0,0x3,
    0x0,0x0,0x0,0x4,0x5,0x6,0x7,0x8,0x0,0x0,0x0,0x9,
    0xA,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,
    0x0,0x0,0x0,0xB
};

static const struct test_tracker_expect_box check_vstruct_valid1_expect[] = {
    { "ms1", 0, 9,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "ms1", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 3 },

    { "ms1.field1_u32", 0, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field1_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 1 } },

    { "ms1.field2_bytes", 4, 1,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field2_bytes", .len = 12 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "\x02", .len = 1 } } },

    { "ms1.field3_u32", 5, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field3_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 3 } },

    { "ms2", 9, 12,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "ms2", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 3 },

    { "ms2.field1_u32", 9, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field1_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 4 } },

    { "ms2.field2_bytes", 13, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field2_bytes", .len = 12 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "\x05\x06\x07\x08", .len = 4 } } },

    { "ms2.field3_u32", 17, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field3_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 9 } },

    { "mu3", 21, 10,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "mu3", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 4 },

    { "mu3.field1_u32", 21, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field1_u32", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 10 } },

    { "mu3.field2_bytes", 21, 10,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field2_bytes", .len = 12 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = {
                .buf = "\x0A\x00\x00\x00\x00\x00\x00\x00\x00\x00",
                .len = 10 } } },

    { "mu3.field3_u16", 21, 2,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field3_u16", .len = 10 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 10 } },

    { "mu3.field4_u8", 21, 1,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "field4_u8", .len = 9 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 10 } },

    { "u4", 31, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "u4", .len = 2 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 11 } },
};

static const struct test_tracker_spec check_vstruct_valid1_spec = {
    .test_name = "vstruct.valid1",
    .schema_hdl = &check_vstruct_schema_hdl,
    .contents = check_vstruct_valid1_contents,
    .contents_size = sizeof (check_vstruct_valid1_contents),
    .expect_boxes = check_vstruct_valid1_expect,
    .n_expect_boxes = N_ELEM(check_vstruct_valid1_expect),
};


static void struct_setup(void)
{
    int ret;

    ret = bitpunch_schema_create_from_string(
        &check_struct_schema_hdl, check_struct_def);
    assert(0 == ret);

    ret = bitpunch_schema_create_from_string(
        &check_vstruct_schema_hdl, check_vstruct_def);
    assert(0 == ret);
}

static void struct_teardown(void)
{
    bitpunch_schema_free(check_struct_schema_hdl);
    bitpunch_schema_free(check_vstruct_schema_hdl);
}


START_TEST(struct_valid1)
{
    check_tracker_launch_test(&check_struct_valid1_spec);
}
END_TEST

START_TEST(struct_invalid_truncated1)
{
    check_tracker_launch_test(&check_struct_invalid_truncated1_spec);
}
END_TEST

START_TEST(vstruct_valid1)
{
    check_tracker_launch_test(&check_vstruct_valid1_spec);
}
END_TEST

void check_struct_add_tcases(Suite *s)
{
    TCase *tc_struct;

    tc_struct = tcase_create("struct.valid1");
    tcase_add_unchecked_fixture(tc_struct, struct_setup, struct_teardown);
    tcase_add_test(tc_struct, struct_valid1);
    suite_add_tcase(s, tc_struct);

    tc_struct = tcase_create("struct.invalid_truncated1");
    tcase_add_unchecked_fixture(tc_struct, struct_setup, struct_teardown);
    tcase_add_test(tc_struct, struct_invalid_truncated1);
    suite_add_tcase(s, tc_struct);

    tc_struct = tcase_create("vstruct.valid1");
    tcase_add_unchecked_fixture(tc_struct, struct_setup, struct_teardown);
    tcase_add_test(tc_struct, vstruct_valid1);
    suite_add_tcase(s, tc_struct);
}
