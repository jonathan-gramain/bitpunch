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
#include "core/print.h"

#include "check_tracker.h"

/* static array */

static const char *check_sarray_contents_def =
    "type u32 = byte[4]: integer(signed=false, endian=big);\n"
    "file {\n"
    "    u32[5] int_array;"
    "}\n";

static struct bitpunch_schema_hdl *check_sarray_def_hdl;

static const char check_sarray_valid1_contents[] = {
    0x0,0x0,0x0,0x1,0x0,0x0,0x0,0x2,0x0,0x0,0x0,0x3,0x0,0x0,0x0,0x4,
    0x0,0x0,0x0,0x5
};

static const struct test_tracker_expect_box check_sarray_valid1_expect[] = {
    { "int_array", 0, 20,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "int_array", .len = 9 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 5 },

    { "int_array[0]", 0, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 0 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 1 } },

    { "int_array[1]", 4, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 1 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 2 } },

    { "int_array[2]", 8, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 2 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 3 } },

    { "int_array[3]", 12, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 3 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 4 } },

    { "int_array[4]", 16, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 4 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 5 } },
};

static const struct test_tracker_spec check_sarray_valid1_spec = {
    .test_name = "sarray.valid1",
    .contents_def = &check_sarray_def_hdl,
    .contents = check_sarray_valid1_contents,
    .contents_size = sizeof (check_sarray_valid1_contents),
    .expect_boxes = check_sarray_valid1_expect,
    .n_expect_boxes = N_ELEM(check_sarray_valid1_expect),
};


static const char check_sarray_invalid_truncated1_contents[] = {
    0x0,0x0,0x0,0x1,0x0,0x0,0x0,0x2,0x0,0x0,0x0,0x3,0x0,0x0,0x0,0x4
};

static const struct test_tracker_expect_box
check_sarray_invalid_truncated1_expect[] = {
    /* error detected early because the array is of static size */
};

static const struct test_tracker_spec check_sarray_invalid_truncated1_spec = {
    .test_name = "sarray.invalid_truncated1",
    .contents_def = &check_sarray_def_hdl,
    .contents = check_sarray_invalid_truncated1_contents,
    .contents_size = sizeof (check_sarray_invalid_truncated1_contents),
    .expect_boxes = check_sarray_invalid_truncated1_expect,
    .n_expect_boxes = N_ELEM(check_sarray_invalid_truncated1_expect),
    .tracker_error = BITPUNCH_OUT_OF_BOUNDS_ERROR,
};


static const char check_sarray_invalid_truncated2_contents[] = {
    0x0,0x0,0x0,0x1,0x0,0x0,0x0,0x2,0x0,0x0,0x0,0x3,0x0,0x0,0x0,0x4,
    0x0,0x0,0x0
};

static const struct test_tracker_expect_box
check_sarray_invalid_truncated2_expect[] = {
    /* error detected early because the array is of static size */
};


static const struct test_tracker_spec check_sarray_invalid_truncated2_spec = {
    .test_name = "sarray.invalid_truncated2",
    .contents_def = &check_sarray_def_hdl,
    .contents = check_sarray_invalid_truncated2_contents,
    .contents_size = sizeof (check_sarray_invalid_truncated2_contents),
    .expect_boxes = check_sarray_invalid_truncated2_expect,
    .n_expect_boxes = N_ELEM(check_sarray_invalid_truncated2_expect),
    .tracker_error = BITPUNCH_OUT_OF_BOUNDS_ERROR,
};


/* dynamic array */


static const char *check_varray_contents_def =
    "type u32 = byte[4]: integer(signed=false, endian=big);\n"
    "file {\n"
    "    u32 int_array_size;\n"
    "    u32[(2 + int_array_size)] int_array;\n"
    "}\n";

static struct bitpunch_schema_hdl *check_varray_def_hdl;

static const char check_varray_valid1_contents[] = {
    0x0,0x0,0x0,0x3, /* 2 + size=3 -> 5 elements */
    0x0,0x0,0x0,0x1,0x0,0x0,0x0,0x2,0x0,0x0,0x0,0x3,0x0,0x0,0x0,0x4,
    0x0,0x0,0x0,0x5
};

static const struct test_tracker_expect_box check_varray_valid1_expect[] = {
    { "int_array_size", 0, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "int_array_size", .len = 14 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 3 } },

    { "int_array", 4, 20,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "int_array", .len = 9 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 5 },

    { "int_array[0]", 4, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 0 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 1 } },

    { "int_array[1]", 8, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 1 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 2 } },

    { "int_array[2]", 12, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 2 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 3 } },

    { "int_array[3]", 16, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 3 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 4 } },

    { "int_array[4]", 20, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 4 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 5 } },
};

static const struct test_tracker_spec check_varray_valid1_spec = {
    .test_name = "varray.valid1",
    .contents_def = &check_varray_def_hdl,
    .contents = check_varray_valid1_contents,
    .contents_size = sizeof (check_varray_valid1_contents),
    .expect_boxes = check_varray_valid1_expect,
    .n_expect_boxes = N_ELEM(check_varray_valid1_expect),
};


static const char check_varray_invalid_truncated1_contents[] = {
    0x0,0x0,0x0,0x5,
    0x0,0x0,0x0,0x1,0x0,0x0,0x0,0x2,0x0,0x0,0x0,0x3,0x0,0x0,0x0,0x4
};

static const struct test_tracker_expect_box check_varray_invalid_truncated1_expect[] = {
    { "int_array_size", 0, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "int_array_size", .len = 14 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 5 } },

    { "int_array", 4, 20,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "int_array", .len = 9 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 7,
      /* error detected at read because of lazy evaluation of size */
      .read_item_ret = BITPUNCH_OUT_OF_BOUNDS_ERROR },

    { "int_array[0]", 4, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 0 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 1 } },

    { "int_array[1]", 8, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 1 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 2 } },

    { "int_array[2]", 12, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 2 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 3 } },

    { "int_array[3]", 16, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 3 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 4 } },

    { "int_array[4]",
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 4 },
      .read_item_ret = BITPUNCH_OUT_OF_BOUNDS_ERROR },
};

static const struct test_tracker_spec check_varray_invalid_truncated1_spec = {
    .test_name = "varray.invalid_truncated1",
    .contents_def = &check_varray_def_hdl,
    .contents = check_varray_invalid_truncated1_contents,
    .contents_size = sizeof (check_varray_invalid_truncated1_contents),
    .expect_boxes = check_varray_invalid_truncated1_expect,
    .n_expect_boxes = N_ELEM(check_varray_invalid_truncated1_expect),
    .tracker_error = BITPUNCH_OUT_OF_BOUNDS_ERROR,
};


static void array_setup(void)
{
    int ret;

    ret = bitpunch_load_schema_from_string(check_sarray_contents_def,
                                            &check_sarray_def_hdl);
    assert(0 == ret);

    ret = bitpunch_load_schema_from_string(check_varray_contents_def,
                                            &check_varray_def_hdl);
    assert(0 == ret);
}

static void array_teardown(void)
{
    bitpunch_free_schema(check_sarray_def_hdl);
    bitpunch_free_schema(check_varray_def_hdl);
}

START_TEST(sarray_valid1)
{
    check_tracker_launch_test(&check_sarray_valid1_spec);
}
END_TEST

START_TEST(sarray_invalid_truncated1)
{
    check_tracker_launch_test(&check_sarray_invalid_truncated1_spec);
}
END_TEST

START_TEST(sarray_invalid_truncated2)
{
    check_tracker_launch_test(&check_sarray_invalid_truncated2_spec);
}
END_TEST

START_TEST(varray_valid1)
{
    check_tracker_launch_test(&check_varray_valid1_spec);
}
END_TEST

START_TEST(varray_invalid_truncated1)
{
    check_tracker_launch_test(&check_varray_invalid_truncated1_spec);
}
END_TEST


void check_array_add_tcases(Suite *s)
{
    TCase *tc_array;

    tc_array = tcase_create("sarray.valid1");
    tcase_add_unchecked_fixture(tc_array, array_setup, array_teardown);
    tcase_add_test(tc_array, sarray_valid1);
    suite_add_tcase(s, tc_array);

    tc_array = tcase_create("sarray.invalid_truncated1");
    tcase_add_unchecked_fixture(tc_array, array_setup, array_teardown);
    tcase_add_test(tc_array, sarray_invalid_truncated1);
    suite_add_tcase(s, tc_array);

    tc_array = tcase_create("sarray.invalid_truncated2");
    tcase_add_unchecked_fixture(tc_array, array_setup, array_teardown);
    tcase_add_test(tc_array, sarray_invalid_truncated2);
    suite_add_tcase(s, tc_array);

    tc_array = tcase_create("varray.valid1");
    tcase_add_unchecked_fixture(tc_array, array_setup, array_teardown);
    tcase_add_test(tc_array, varray_valid1);
    suite_add_tcase(s, tc_array);

    tc_array = tcase_create("varray.invalid_truncated1");
    tcase_add_unchecked_fixture(tc_array, array_setup, array_teardown);
    tcase_add_test(tc_array, varray_invalid_truncated1);
    suite_add_tcase(s, tc_array);
}
