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


#define _GNU_SOURCE

#include <check.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "api/bitpunch_api.h"
#include "core/browse.h"
#include "core/filter.h"
#include "core/debug.h"

static const char *radio_sources[] = {
    "static",
    "var_length",
    "var_item_length",
    "item_span",
    "item_cond_span",
    "var_item_span",
    "item_size_table",
    "named_exprs_shuffled",
    "with_last",
};

static const char *radio_codenames[] = {
    "alpha",
    "bravo",
    "charlie",
    "delta",
    "echo",
    "foxtrot",
    "golf",
    "hotel",
    "india",
    "juliett",
    "kilo",
    "lima",
    "mike",
    "november",
    "oscar",
    "papa",
    "quebec",
    "romeo",
    "sierra",
    "tango",
    "uniform",
    "victor",
    "whisky",
    "x-ray",
    "yankee",
    "zulu"
};

struct radio_source_info {
    struct bitpunch_board *board;
    struct tracker *tk;
    int64_t codename_offset[N_ELEM(radio_codenames)];
    int codename_is_named_expr;
};

static struct radio_source_info radio_source_info[N_ELEM(radio_sources)];

static void testcase_radio_setup(void)
{
    int i;
    int ret;
    bitpunch_status_t bt_ret;
    char filepath[128];
    struct radio_source_info *info;
    struct ast_node_hdl *bp_schema;
    struct bitpunch_data_source *ds;
    int c;
    const char *codename;
    char *location;

    for (i = 0; i < N_ELEM(radio_sources); ++i) {
        info = &radio_source_info[i];

        info->board = bitpunch_board_new();

        snprintf(filepath, sizeof (filepath), "tests/common/radio/radio_%s.bin",
                 radio_sources[i]);
        ret = bitpunch_data_source_create_from_file_path(&ds, filepath);
        assert(0 == ret);
        bitpunch_board_add_let_expression(
            info->board, "data", bitpunch_data_source_to_filter(ds));

        snprintf(filepath, sizeof (filepath),
                 "tests/common/radio/radio_%s.bp", radio_sources[i]);
        ret = bitpunch_schema_create_from_path(&bp_schema, filepath);
        assert(0 == ret);
        bitpunch_board_add_let_expression(info->board, "Schema", bp_schema);

        bt_ret = bitpunch_board_add_expr(
            info->board, "Model", "data <> Schema.Radio");
        assert(BITPUNCH_OK == bt_ret);

        for (c = 0; c < N_ELEM(radio_codenames); ++c) {
            codename = radio_codenames[c];
            location = memmem(ds->ds_data,
                              ds->ds_data_length,
                              codename, strlen(codename));
            assert(NULL != location);
            info->codename_offset[c] = location - ds->ds_data;
        }
        if (memmem(bp_schema->loc.parser_ctx->parser_data,
                   bp_schema->loc.parser_ctx->parser_data_length,
                   "let codename", strlen("let codename")) != 0) {
            info->codename_is_named_expr = TRUE;
        }
    }
}

static void testcase_radio_teardown(void)
{
    int i;
    struct radio_source_info *info;

    for (i = 0; i < N_ELEM(radio_sources); ++i) {
        info = &radio_source_info[i];
        bitpunch_board_free(info->board);
    }
}

static void check_codename_value(struct radio_source_info *info,
                                 expr_value_t value,
                                 int code_idx)
{
    const char *codename;
    int64_t expect_size;

    codename = radio_codenames[code_idx];
    expect_size = strlen(codename);

    ck_assert_int_eq(value.type, EXPR_VALUE_TYPE_STRING);
    ck_assert_int_eq(value.string.len, expect_size);
    ck_assert(0 == memcmp(value.string.str,
                          codename, value.string.len));
}

static void check_codename_item(struct radio_source_info *info,
                                struct tracker *tk,
                                int code_idx)
{
    bitpunch_status_t bt_ret;
    int64_t code_offset;
    int64_t expect_offset;
    expr_value_t value;

    expect_offset = info->codename_offset[code_idx];

    if (!tracker_is_dangling(tk)) {
        bt_ret = tracker_read_item_value(tk, &value, NULL);
    } else {
        bt_ret = box_read_value(tk->box, &value, NULL);
    }
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    if (!tracker_is_dangling(tk)) {
        bt_ret = tracker_get_item_offset(tk, &code_offset, NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    } else {
        code_offset = tk->box->start_offset_span;
    }
    ck_assert_int_eq(code_offset, expect_offset);

    check_codename_value(info, value, code_idx);
    expr_value_destroy(value);
}

static void check_codename_entry(struct radio_source_info *info,
                                 struct tracker *tk,
                                 int code_idx)
{
    const char *codename;
    int64_t expect_size;
    struct tracker *tk2;
    bitpunch_status_t bt_ret;
    expr_value_t value;

    codename = radio_codenames[code_idx];
    expect_size = strlen(codename);

    tk2 = tracker_dup(tk);
    ck_assert_ptr_ne(tk2, NULL);
    bt_ret = tracker_enter_item(tk2, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    bt_ret = filter_evaluate_identifier(
        tk2->box->filter, tk2->box,
        STATEMENT_TYPE_FIELD | STATEMENT_TYPE_NAMED_EXPR |
        STATEMENT_TYPE_ATTRIBUTE,
        "codename", 0u, &value, NULL, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    check_codename_value(info, value, code_idx);
    expr_value_destroy(value);
    if (!info->codename_is_named_expr) {
        bt_ret = tracker_goto_named_item(tk2, "codename", NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);
        check_codename_item(info, tk2, code_idx);
    }
    bt_ret = tracker_get_item_key(tk, &value, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    ck_assert_int_eq(value.type, EXPR_VALUE_TYPE_STRING);
    ck_assert_int_eq(value.string.len, expect_size);
    ck_assert(0 == memcmp(value.string.str,
                          codename, value.string.len));
    expr_value_destroy(value);
    tracker_delete(tk2);
}

void testcase_radio_launch_test_iterate(struct radio_source_info *info)
{
    expr_dpath_t dpath;
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    int c;
    int code_idx;

    bt_ret = bitpunch_eval_expr(info->board, "Model", NULL, 0u,
                                NULL, NULL, &dpath, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    bt_ret = track_dpath_contents(dpath, &tk, NULL);
    expr_dpath_destroy(dpath);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    ck_assert_ptr_ne(tk, NULL);

    bt_ret = tracker_goto_named_item(tk, "codes", NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    bt_ret = tracker_enter_item(tk, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    bt_ret = tracker_goto_first_item(tk, NULL);
    for (c = 0; c < N_ELEM(radio_codenames); ++c) {
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);

        bt_ret = tracker_goto_next_item(tk, NULL);
    }
    ck_assert_int_eq(bt_ret, BITPUNCH_NO_ITEM);

    bt_ret = tracker_goto_first_item(tk, NULL);
    for (c = 0; c < N_ELEM(radio_codenames) / 2; ++c) {
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);

        bt_ret = tracker_goto_next_item(tk, NULL);
    }
    code_idx = c;
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    check_codename_entry(info, tk, code_idx);

    bt_ret = tracker_goto_first_item(tk, NULL);
    for (c = 0; c < N_ELEM(radio_codenames); ++c) {
        code_idx = c;
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);

        check_codename_entry(info, tk, code_idx);

        bt_ret = tracker_goto_next_item(tk, NULL);
    }
    ck_assert_int_eq(bt_ret, BITPUNCH_NO_ITEM);

    for (c = 0; c < N_ELEM(radio_codenames); ++c) {
        code_idx = (c * 7) % N_ELEM(radio_codenames);

        bt_ret = tracker_goto_nth_item(tk, code_idx, NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);

        check_codename_entry(info, tk, code_idx);
    }
    tracker_delete(tk);
}

START_TEST(radio_iterate)
{
    int i;

    for (i = 0; i < N_ELEM(radio_sources); ++i) {
        testcase_radio_launch_test_iterate(&radio_source_info[i]);
    }
}
END_TEST


void testcase_radio_launch_test_index(struct radio_source_info *info)
{
    expr_dpath_t dpath;
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    int c;
    int code_idx;
    expr_value_t item_key;

    bt_ret = bitpunch_eval_expr(info->board, "Model", NULL, 0u,
                                NULL, NULL, &dpath, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    bt_ret = track_dpath_contents(dpath, &tk, NULL);
    expr_dpath_destroy(dpath);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    ck_assert_ptr_ne(tk, NULL);

    bt_ret = tracker_goto_named_item(tk, "codes", NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    bt_ret = tracker_enter_item(tk, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    for (c = 0; c < N_ELEM(radio_codenames); ++c) {
        code_idx = c;
        memset(&item_key, 0, sizeof(item_key));
        item_key.type = EXPR_VALUE_TYPE_STRING;
        item_key.string.str = radio_codenames[code_idx];
        item_key.string.len = strlen(radio_codenames[code_idx]);
        bt_ret = tracker_goto_first_item_with_key(tk, item_key, NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);

        bt_ret = tracker_goto_next_item_with_key(tk, item_key, NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_NO_ITEM);

        /* tracker shall not have moved after BITPUNCH_NO_ITEM was
         * returned */
        check_codename_entry(info, tk, code_idx);
    }

    for (c = N_ELEM(radio_codenames) - 1; c >= 0; --c) {
        code_idx = c;

        memset(&item_key, 0, sizeof(item_key));
        item_key.type = EXPR_VALUE_TYPE_STRING;
        item_key.string.str = radio_codenames[code_idx];
        item_key.string.len = strlen(radio_codenames[code_idx]);
        bt_ret = tracker_goto_first_item_with_key(tk, item_key, NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);

        bt_ret = tracker_goto_next_item_with_key(tk, item_key, NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_NO_ITEM);

        /* tracker shall not have moved after BITPUNCH_NO_ITEM was
         * returned */
        check_codename_entry(info, tk, code_idx);
    }

    for (c = 0; c < N_ELEM(radio_codenames); ++c) {
        code_idx = (c * 7) % 26;

        memset(&item_key, 0, sizeof(item_key));
        item_key.type = EXPR_VALUE_TYPE_STRING;
        item_key.string.str = radio_codenames[code_idx];
        item_key.string.len = strlen(radio_codenames[code_idx]);
        bt_ret = tracker_goto_first_item_with_key(tk, item_key, NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);

        bt_ret = tracker_goto_next_item_with_key(tk, item_key, NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_NO_ITEM);

        /* tracker shall not have moved after BITPUNCH_NO_ITEM was
         * returned */
        check_codename_entry(info, tk, code_idx);
    }
    tracker_delete(tk);
}

START_TEST(radio_index)
{
    int i;

    for (i = 0; i < N_ELEM(radio_sources); ++i) {
        testcase_radio_launch_test_index(&radio_source_info[i]);
    }
}
END_TEST


void testcase_radio_launch_test_slices(struct radio_source_info *info)
{
    expr_dpath_t dpath;
    struct tracker *tk;
    struct tracker *tk_end;
    bitpunch_status_t bt_ret;
    int c;
    int code_idx;

    bt_ret = bitpunch_eval_expr(info->board, "Model", NULL, 0u,
                                NULL, NULL, &dpath, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    bt_ret = track_dpath_contents(dpath, &tk, NULL);
    expr_dpath_destroy(dpath);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    ck_assert_ptr_ne(tk, NULL);

    bt_ret = tracker_goto_named_item(tk, "codes", NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    bt_ret = tracker_enter_item(tk, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    tk_end = tracker_dup(tk);

    /* basic test with empty slice */
    bt_ret = tracker_enter_slice(tk, tk_end, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    bt_ret = tracker_return(tk, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    /* slice [..7] */
    bt_ret = tracker_goto_nth_item(tk_end, 7, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    bt_ret = tracker_enter_slice(tk, tk_end, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    for (c = 0; c < 7; ++c) {
        code_idx = c;

        bt_ret = tracker_goto_next_item(tk, NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);

        check_codename_entry(info, tk, code_idx);
    }
    bt_ret = tracker_goto_next_item(tk, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_NO_ITEM);

    bt_ret = tracker_return(tk, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    /* slice [5..10] */
    bt_ret = tracker_goto_nth_item(tk, 5, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    bt_ret = tracker_goto_nth_item(tk_end, 10, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    bt_ret = tracker_enter_slice(tk, tk_end, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    tracker_rewind(tk);
    for (c = 5; c < 10; ++c) {
        code_idx = c;

        bt_ret = tracker_goto_next_item(tk, NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);

        check_codename_entry(info, tk, code_idx);
    }
    bt_ret = tracker_goto_next_item(tk, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_NO_ITEM);

    bt_ret = tracker_return(tk, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    tracker_delete(tk);
    tracker_delete(tk_end);
}

START_TEST(radio_slices)
{
    int i;

    for (i = 0; i < N_ELEM(radio_sources); ++i) {
        testcase_radio_launch_test_slices(&radio_source_info[i]);
    }
}
END_TEST

static void check_goto_dpath(struct radio_source_info *info,
                             struct tracker *tk,
                             const char *dpath_expr,
                             int code_idx)
{
    bitpunch_status_t bt_ret;

    bt_ret = tracker_goto_abs_dpath(tk, dpath_expr, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    check_codename_item(info, tk, code_idx);
}

void testcase_radio_launch_test_dpath(struct radio_source_info *info)
{
    expr_dpath_t dpath;
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    int c;
    int code_idx;
    char dpath_expr[128];

    bt_ret = bitpunch_eval_expr(info->board, "Model", NULL, 0u,
                                NULL, NULL, &dpath, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    bt_ret = track_dpath_contents(dpath, &tk, NULL);
    expr_dpath_destroy(dpath);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    ck_assert_ptr_ne(tk, NULL);

    for (c = 0; c < N_ELEM(radio_codenames); ++c) {
        code_idx = (c * 7) % 26;
        snprintf(dpath_expr, sizeof (dpath_expr),
                 "codes[%d].codename",
                 code_idx);
        check_goto_dpath(info, tk, dpath_expr, code_idx);
    }
    for (c = 0; c < N_ELEM(radio_codenames); ++c) {
        code_idx = (c * 7) % 26;
        snprintf(dpath_expr, sizeof (dpath_expr),
                 "codes['%s'].codename",
                 radio_codenames[code_idx]);
        check_goto_dpath(info, tk, dpath_expr, code_idx);
    }
    tracker_delete(tk);
}

START_TEST(radio_dpath)
{
    int i;

    for (i = 0; i < N_ELEM(radio_sources); ++i) {
        testcase_radio_launch_test_dpath(&radio_source_info[i]);
    }
}
END_TEST


static void
check_codename_range(struct radio_source_info *info,
                     struct tracker *tk,
                     int code_idx_start, int code_idx_end)
{
    struct tracker *tk2;
    int code_idx;
    int c;
    bitpunch_status_t bt_ret;

    if (-1 == code_idx_start) {
        code_idx_start = 0;
    }
    if (-1 == code_idx_end) {
        code_idx_end = N_ELEM(radio_codenames);
    }
    tk2 = tracker_dup(tk);
    tracker_rewind(tk2);
    for (c = code_idx_start; c < code_idx_end; ++c) {
        code_idx = c;

        bt_ret = tracker_goto_next_item(tk2, NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);

        check_codename_entry(info, tk2, code_idx);
    }
    bt_ret = tracker_goto_next_item(tk2, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_NO_ITEM);

    bt_ret = tracker_return(tk2, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    tracker_delete(tk2);
}

void testcase_radio_launch_test_slice_dpath(struct radio_source_info *info)
{
    expr_dpath_t dpath;
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    char dpath_expr[128];
    struct {
        int code_idx_start;
        int code_idx_end;
    } ranges_to_check[] = {
        {0, 1},
        {0, 5},
        {-1, 5},
        {-1, N_ELEM(radio_codenames)},
        {2, 4},
        {10, -1},
        {0, 0},
        {5, 5},
        {N_ELEM(radio_codenames), -1},
        {N_ELEM(radio_codenames), N_ELEM(radio_codenames)},
        {-1, -1}
   };
    int idx_start;
    int idx_end;
    int r;

    bt_ret = bitpunch_eval_expr(info->board, "Model", NULL, 0u,
                                NULL, NULL, &dpath, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    bt_ret = track_dpath_contents(dpath, &tk, NULL);
    expr_dpath_destroy(dpath);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    ck_assert_ptr_ne(tk, NULL);

    for (r = 0; r < N_ELEM(ranges_to_check); ++r) {
        char start_str[16];
        char end_str[16];

        idx_start = ranges_to_check[r].code_idx_start;
        if (-1 != idx_start) {
            snprintf(start_str, sizeof (start_str), "%d", idx_start);
        } else {
            start_str[0] = '\0';
        }
        idx_end = ranges_to_check[r].code_idx_end;
        if (-1 != idx_end) {
            snprintf(end_str, sizeof (end_str), "%d", idx_end);
        } else {
            end_str[0] = '\0';
        }
        snprintf(dpath_expr, sizeof (dpath_expr),
                 "codes[%s..%s]", start_str, end_str);

        bt_ret = tracker_goto_abs_dpath(tk, dpath_expr, NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);

        check_codename_range(info, tk, idx_start, idx_end);
    }
    for (r = 0; r < N_ELEM(ranges_to_check); ++r) {
        char start_str[16];
        char end_str[16];

        idx_start = ranges_to_check[r].code_idx_start;
        idx_end = ranges_to_check[r].code_idx_end;
        if (idx_start != -1 || idx_end != -1) {
            if (-1 == idx_start) {
                start_str[0] = '\0';
            } else if (idx_start < N_ELEM(radio_codenames)) {
                snprintf(start_str, sizeof (start_str),
                         "\"%s\"", radio_codenames[idx_start]);
            } else {
                snprintf(start_str, sizeof (start_str),
                         "%d", idx_start);
            }
            if (-1 != idx_end) {
                snprintf(end_str, sizeof (end_str),
                         "%d", idx_end);
            } else {
                end_str[0] = '\0';
            }
            snprintf(dpath_expr, sizeof (dpath_expr),
                     "codes[%s..%s]", start_str, end_str);

            bt_ret = tracker_goto_abs_dpath(tk, dpath_expr, NULL);
            ck_assert_int_eq(bt_ret, BITPUNCH_OK);

            check_codename_range(info, tk, idx_start, idx_end);

            if (-1 != idx_start) {
                snprintf(start_str, sizeof (start_str),
                         "%d", idx_start);
            } else {
                start_str[0] = '\0';
            }
            if (-1 == idx_end) {
                end_str[0] = '\0';
            } else if (idx_end < N_ELEM(radio_codenames)) {
                snprintf(end_str, sizeof (end_str),
                         "\"%s\"", radio_codenames[idx_end]);
            } else {
                snprintf(end_str, sizeof (end_str),
                         "%d", idx_end);
            }
            snprintf(dpath_expr, sizeof (dpath_expr),
                     "codes[%s..%s]", start_str, end_str);

            bt_ret = tracker_goto_abs_dpath(tk, dpath_expr, NULL);
            ck_assert_int_eq(bt_ret, BITPUNCH_OK);

            check_codename_range(info, tk, idx_start, idx_end);
        }
    }
    tracker_delete(tk);
}

START_TEST(radio_slice_dpath)
{
    int i;

    for (i = 0; i < N_ELEM(radio_sources); ++i) {
        testcase_radio_launch_test_slice_dpath(&radio_source_info[i]);
    }
}
END_TEST


void testcase_radio_add_tests(Suite *s)
{
    TCase *tc_radio;

    tc_radio = tcase_create("radio");
    tcase_add_unchecked_fixture(tc_radio,
                                testcase_radio_setup,
                                testcase_radio_teardown);
    tcase_add_test(tc_radio, radio_iterate);
    tcase_add_test(tc_radio, radio_index);
    tcase_add_test(tc_radio, radio_slices);
    tcase_add_test(tc_radio, radio_dpath);
    tcase_add_test(tc_radio, radio_slice_dpath);
    suite_add_tcase(s, tc_radio);
}
