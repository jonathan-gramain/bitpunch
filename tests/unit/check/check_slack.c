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

static const char *check_slack_def =
    "let u32 = byte[4]: integer { signed: false; endian: 'big'; };\n"
    "let MyHdr = struct {\n"
    "    magic: byte[5];\n"
    "};\n"
    "file {\n"
    "    hdr: MyHdr;\n"
    "    slack_array: u32[];\n"
    "}\n";

static struct bitpunch_schema_hdl *check_slack_def_hdl;


static const char check_slack_valid1_contents[] = {
    'm', 'a', 'g', 'i', 'c',
    0x0,0x0,0x0,0x1,0x0,0x0,0x0,0x2,0x0,0x0,0x0,0x3,
    0x0,0x0,0x0,0x4,0x0,0x0,0x0,0x5
};

static const struct test_tracker_expect_box check_slack_valid1_expect[] = {
    { "hdr", 0, 5,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "hdr", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 1 },

    { "hdr.magic", 0, 5,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "magic", .len = 5 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "magic", .len = 5 } } },

    { "slack_array", 5, 20,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "slack_array", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 5 },

    { "slack_array[0]", 5, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 0 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 1 } },

    { "slack_array[1]", 9, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 1 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 2 } },

    { "slack_array[2]", 13, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 2 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 3 } },

    { "slack_array[3]", 17, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 3 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 4 } },

    { "slack_array[4]", 21, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 4 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 5 } },
};

static const struct test_tracker_spec check_slack_valid1_spec = {
    .test_name = "slack.valid1",
    .contents_def = &check_slack_def_hdl,
    .contents = check_slack_valid1_contents,
    .contents_size = sizeof (check_slack_valid1_contents),
    .expect_boxes = check_slack_valid1_expect,
    .n_expect_boxes = N_ELEM(check_slack_valid1_expect),
};



static const char *check_slack_trailing_field_def =
    "let u32 = byte[4]: integer { signed: false; endian: 'big'; };\n"
    "let MyHdr = struct {\n"
    "    magic: byte[5];\n"
    "};\n"
    "file {\n"
    "    hdr: MyHdr;\n"
    "    slack_array: u32[];\n"
    "    : byte[];\n" // to ensure slack space is filled
    "    trailer: byte[7];\n"
    "}\n";

static struct bitpunch_schema_hdl *check_slack_trailing_field_def_hdl;


static const char check_slack_trailing_field_valid1_contents[] = {
    'm', 'a', 'g', 'i', 'c',
    0x0,0x0,0x0,0x1,0x0,0x0,0x0,0x2,0x0,0x0,0x0,0x3,
    0x0,0x0,0x0,0x4,0x0,0x0,0x0,0x5,
    't', 'r', 'a', 'i', 'l', 'e', 'r'
};

static const struct test_tracker_expect_box check_slack_trailing_field_valid1_expect[] = {
    { "hdr", 0, 5,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "hdr", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 1 },

    { "hdr.magic", 0, 5,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "magic", .len = 5 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "magic", .len = 5 } } },

    { "slack_array", 5, 20,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "slack_array", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 5 },

    { "slack_array[0]", 5, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 0 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 1 } },

    { "slack_array[1]", 9, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 1 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 2 } },

    { "slack_array[2]", 13, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 2 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 3 } },

    { "slack_array[3]", 17, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 3 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 4 } },

    { "slack_array[4]", 21, 4,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 4 },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 5 } },

    { "trailer", 25, 7,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "trailer", .len = 7 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "trailer", .len = 7 } } },
};

static const struct test_tracker_spec check_slack_trailing_field_valid1_spec = {
    .test_name = "slack.trailing_field_valid1",
    .contents_def = &check_slack_trailing_field_def_hdl,
    .contents = check_slack_trailing_field_valid1_contents,
    .contents_size = sizeof (check_slack_trailing_field_valid1_contents),
    .expect_boxes = check_slack_trailing_field_valid1_expect,
    .n_expect_boxes = N_ELEM(check_slack_trailing_field_valid1_expect),
};


static const char *check_slack_byte_array_def =
    "let u32 = byte[4]: integer { signed: false; endian: 'big'; };\n"
    "let MyHdr = struct {\n"
    "    magic: byte[5];\n"
    "};\n"
    "let MyFtr = struct {\n"
    "    bye: byte[3];\n"
    "};\n"
    "file {\n"
    "    hdr: MyHdr;\n"
    "    padding: byte[];\n"
    "    ftr: MyHdr;\n"
    "}\n";

static struct bitpunch_schema_hdl *check_slack_byte_array_def_hdl;


static const char check_slack_byte_array_valid1_contents[] = {
    'm', 'a', 'g', 'i', 'c',
    'p', 'a', 'd', 'd', 'i', 'n', 'g',
    'b', 'y', 'e'
};

static const struct test_tracker_expect_box check_slack_byte_array_valid1_expect[] = {
    { "hdr", 0, 5,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "hdr", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 1 },

    { "hdr.magic", 0, 5,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "magic", .len = 5 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "magic", .len = 5 } } },

    { "padding", 5, 7,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "padding", .len = 7 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "padding", .len = 7 } } },

    { "ftr", 12, 3,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "ftr", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 1 },

    { "ftr.bye", 12, 3,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "bye", .len = 3 } },
      .value_type = EXPR_VALUE_TYPE_BYTES,
      .value = { .bytes = { .buf = "bye", .len = 3 } } },
};

static const struct test_tracker_spec check_slack_byte_array_valid1_spec = {
    .test_name = "slack_byte_array.valid1",
    .contents_def = &check_slack_byte_array_def_hdl,
    .contents = check_slack_byte_array_valid1_contents,
    .contents_size = sizeof (check_slack_byte_array_valid1_contents),
    .expect_boxes = check_slack_byte_array_valid1_expect,
    .n_expect_boxes = N_ELEM(check_slack_byte_array_valid1_expect),
};



static const char *check_slack_trailing_field_recur_def =
    "let u32 = byte[4]: integer { signed: false; endian: 'big'; };\n"
    "let BOX = struct {\n"
    "    size:        u32;\n"
    "    name:        byte[8]: string;\n"
    "    sub_boxes:   BOX[];\n"
    "    trailer_str: byte[7]: string;\n"
    "\n"
    "    span size;\n"
    "};\n"
    "file {\n"
    "     hello_str:  byte[6]: string;\n"
    "     boxes:      BOX[];\n"
    "     end_str:    byte[10]: string;\n"
    "}\n";


static struct bitpunch_schema_hdl *check_slack_trailing_field_recur_def_hdl;
static const char check_slack_trailing_field_recur_valid1_contents[] = {
    'h', 'e', 'l', 'l', 'o', 0x00,
    /* -> */ 0x00, 0x00, 0x00, 0x26,
    /* -> */ 'b', 'o', 'x', '1', 0x00, 0x00, 0x00, 0x00,
    /* ->      -> */ 0x00, 0x00, 0x00, 0x13,
    /* ->      -> */ 'b', 'o', 'x', '1', '.', '1', 0x00, 0x00,
    /* ->      -> */ 't', 'r', 'a', 'i', 'l', 'e', 'r',
    /* -> */ 't', 'r', 'a', 'i', 'l', 'e', 'r',
    /* -> */ 0x00, 0x00, 0x00, 0x39,
    /* -> */ 'b', 'o', 'x', '2', 0x00, 0x00, 0x00, 0x00,
    /* ->      -> */ 0x00, 0x00, 0x00, 0x13,
    /* ->      -> */ 'b', 'o', 'x', '2', '.', '1', 0x00, 0x00,
    /* ->      -> */ 't', 'r', 'a', 'i', 'l', 'e', 'r',
    /* ->      -> */ 0x00, 0x00, 0x00, 0x13,
    /* ->      -> */ 'b', 'o', 'x', '2', '.', '2', 0x00, 0x00,
    /* ->      -> */ 't', 'r', 'a', 'i', 'l', 'e', 'r',
    /* -> */ 't', 'r', 'a', 'i', 'l', 'e', 'r',
    /* -> */ 0x00, 0x00, 0x00, 0x13,
    /* -> */ 'b', 'o', 'x', '3', 0x00, 0x00, 0x00, 0x00,
    /* -> */ 't', 'r', 'a', 'i', 'l', 'e', 'r',
    /* -> */ 'i', 't', '\'', 's', ' ', 'o', 'v', 'e', 'r', 0x00
};


static const struct test_tracker_expect_box check_slack_trailing_field_recur_valid1_expect[] = {
    { "hello_str", 0, 6,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "hello_str", .len = 9 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "hello\0", .len = 6 } } },

    { "boxes", 6, 114,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "boxes", .len = 5 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 3 },

    { "boxes[0]", 6, 38,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 0 },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 4 },

    { "boxes[0].size", 6, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "size", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 38 } },

    { "boxes[0].name", 10, 8,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "name", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "box1\0\0\0\0", .len = 8 } } },

    { "boxes[0].sub_boxes", 18, 19,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "sub_boxes", .len = 9 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 1 },

    { "boxes[0].sub_boxes[0]", 18, 19,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 0 },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 4 },

    { "boxes[0].sub_boxes[0].size", 18, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "size", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 19 } },

    { "boxes[0].sub_boxes[0].name", 22, 8,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "name", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "box1.1\0\0", .len = 8 } } },

    { "boxes[0].sub_boxes[0].sub_boxes", 30, 0,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "sub_boxes", .len = 9 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 0 },

    { "boxes[0].sub_boxes[0].trailer_str", 30, 7,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "trailer_str", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "trailer", .len = 7 } } },

    { "boxes[0].trailer_str", 37, 7,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "trailer_str", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "trailer", .len = 7 } } },

    { "boxes[1]", 44, 57,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 1 },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 4 },

    { "boxes[1].size", 44, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "size", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 57 } },

    { "boxes[1].name", 48, 8,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "name", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "box2\0\0\0\0", .len = 8 } } },

    { "boxes[1].sub_boxes", 56, 38,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "sub_boxes", .len = 9 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 2 },

    { "boxes[1].sub_boxes[0]", 56, 19,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 0 },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 4 },

    { "boxes[1].sub_boxes[0].size", 56, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "size", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 19 } },

    { "boxes[1].sub_boxes[0].name", 60, 8,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "name", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "box2.1\0\0", .len = 8 } } },

    { "boxes[1].sub_boxes[0].sub_boxes", 68, 0,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "sub_boxes", .len = 9 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 0 },

    { "boxes[1].sub_boxes[0].trailer_str", 68, 7,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "trailer_str", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "trailer", .len = 7 } } },

    { "boxes[1].sub_boxes[1]", 75, 19,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 1 },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 4 },

    { "boxes[1].sub_boxes[1].size", 75, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "size", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 19 } },

    { "boxes[1].sub_boxes[1].name", 79, 8,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "name", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "box2.2\0\0", .len = 8 } } },

    { "boxes[1].sub_boxes[1].sub_boxes", 87, 0,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "sub_boxes", .len = 9 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 0 },

    { "boxes[1].sub_boxes[1].trailer_str", 87, 7,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "trailer_str", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "trailer", .len = 7 } } },

    { "boxes[1].trailer_str", 94, 7,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "trailer_str", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "trailer", .len = 7 } } },

    { "boxes[2]", 101, 19,
      .key_type = EXPR_VALUE_TYPE_INTEGER,
      .key = { .integer = 2 },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 4 },

    { "boxes[2].size", 101, 4,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "size", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_INTEGER,
      .value = { .integer = 19 } },

    { "boxes[2].name", 105, 8,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "name", .len = 4 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "box3\0\0\0\0", .len = 8 } } },

    { "boxes[2].sub_boxes", 113, 0,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "sub_boxes", .len = 9 } },
      .value_type = EXPR_VALUE_TYPE_UNSET,
      .n_items = 0 },

    { "boxes[2].trailer_str", 113, 7,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "trailer_str", .len = 11 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "trailer", .len = 7 } } },

    { "end_str", 120, 10,
      .key_type = EXPR_VALUE_TYPE_STRING,
      .key = { .string = { .str = "end_str", .len = 7 } },
      .value_type = EXPR_VALUE_TYPE_STRING,
      .value = { .string = { .str = "it's over\0", .len = 10 } } },
};

static const struct test_tracker_spec check_slack_trailing_field_recur_valid1_spec = {
    .test_name = "slack.trailing_field_recur_valid1",
    .contents_def = &check_slack_trailing_field_recur_def_hdl,
    .contents = check_slack_trailing_field_recur_valid1_contents,
    .contents_size = sizeof (check_slack_trailing_field_recur_valid1_contents),
    .expect_boxes = check_slack_trailing_field_recur_valid1_expect,
    .n_expect_boxes = N_ELEM(check_slack_trailing_field_recur_valid1_expect),
};


static void slack_setup(void)
{
    int ret;

    ret = bitpunch_load_schema_from_string(check_slack_def,
                                            &check_slack_def_hdl);
    assert(0 == ret);

    ret = bitpunch_load_schema_from_string(check_slack_trailing_field_def,
                                            &check_slack_trailing_field_def_hdl);
    assert(0 == ret);

    ret = bitpunch_load_schema_from_string(check_slack_byte_array_def,
                                            &check_slack_byte_array_def_hdl);
    assert(0 == ret);

    ret = bitpunch_load_schema_from_string(check_slack_trailing_field_recur_def,
                                            &check_slack_trailing_field_recur_def_hdl);
    assert(0 == ret);
}

static void slack_teardown(void)
{
    bitpunch_free_schema(check_slack_def_hdl);
    bitpunch_free_schema(check_slack_trailing_field_def_hdl);
    bitpunch_free_schema(check_slack_byte_array_def_hdl);
    bitpunch_free_schema(check_slack_trailing_field_recur_def_hdl);
}


START_TEST(slack_valid1)
{
    check_tracker_launch_test(&check_slack_valid1_spec);
}
END_TEST

START_TEST(slack_trailing_field_valid1)
{
    check_tracker_launch_test(&check_slack_trailing_field_valid1_spec);
}
END_TEST

START_TEST(slack_trailing_field_recur_valid1)
{
    check_tracker_launch_test(&check_slack_trailing_field_recur_valid1_spec);
}
END_TEST

void check_slack_add_tcases(Suite *s)
{
    TCase *tc_slack;

    tc_slack = tcase_create("slack.valid1");
    tcase_add_unchecked_fixture(tc_slack, slack_setup, slack_teardown);
    tcase_add_test(tc_slack, slack_valid1);
    suite_add_tcase(s, tc_slack);

    tc_slack = tcase_create("slack.trailing_field_valid1");
    tcase_add_unchecked_fixture(tc_slack, slack_setup, slack_teardown);
    tcase_add_test(tc_slack, slack_trailing_field_valid1);
    suite_add_tcase(s, tc_slack);

    tc_slack = tcase_create("slack.trailing_field_recur_valid1");
    tcase_add_unchecked_fixture(tc_slack, slack_setup, slack_teardown);
    tcase_add_test(tc_slack, slack_trailing_field_recur_valid1);
    suite_add_tcase(s, tc_slack);
}
