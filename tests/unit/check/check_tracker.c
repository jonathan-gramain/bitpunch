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


#include <check.h>
#include <assert.h>
#include <stdlib.h>

#include "core/debug.h"
#include "check_bitpunch.h"
#include "check_tracker.h"

static int
is_dpath_prefix_of(const char *dpath_prefix,
                   const char *dpath_str)
{
    size_t prefix_len;

    if ('\0' == dpath_prefix[0]) {
        return TRUE;
    }
    prefix_len = strlen(dpath_prefix);
    return (0 == strncmp(dpath_prefix, dpath_str, prefix_len)
            && (dpath_str[prefix_len] == '.'
                || dpath_str[prefix_len] == '['
                || dpath_str[prefix_len] == '\0'));
}

void
check_tracker_launch_tests(const struct test_tracker_spec *test_specs[],
                           int n_test_specs)
{
    int test_idx;
    const struct test_tracker_spec *test_spec;

    for (test_idx = 0; test_idx < n_test_specs; ++test_idx) {
        test_spec = test_specs[test_idx];

        printf("Running test %s\n", test_spec->test_name);

        check_tracker_launch_test(test_spec);
    }
}

void
check_tracker_launch_test(const struct test_tracker_spec *test_spec)
{
    check_tracker_browse_depth_first(test_spec, TRUE);
    check_tracker_browse_depth_first(test_spec, FALSE);
    check_tracker_browse_sub_trackers(test_spec, TRUE);
    check_tracker_browse_sub_trackers(test_spec, FALSE);
    check_tracker_browse_random_dpath(test_spec, TRUE);
    check_tracker_browse_random_dpath(test_spec, FALSE);
}

void
check_tracker_browse_depth_first(const struct test_tracker_spec *test_spec,
                                 int only_browse)
{
    struct bitpunch_board *board;
    struct ast_node_hdl *ds_in;
    expr_dpath_t dpath;
    struct tracker *tk;
    int ret;
    int box_idx;
    const struct test_tracker_expect_box *expect_box;
    bitpunch_status_t bt_ret;
    int cur_depth;
    int64_t n_items;
    struct ast_node_hdl *item_filter;

    board = bitpunch_board_new();

    ret = bitpunch_data_source_create_from_memory(
        &ds_in, test_spec->contents, test_spec->contents_size, FALSE);
    ck_assert_int_eq(ret, 0);

    bitpunch_board_add_let_expression(board, "data", ds_in);
    bitpunch_board_add_let_expression(board, "Schema", *test_spec->schema_hdl);

    bt_ret = bitpunch_board_add_expr(board, "Model", "data <> Schema.Root");
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    bt_ret = bitpunch_eval_expr(board, "Model", NULL, 0u,
                                NULL, NULL, &dpath, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    bt_ret = track_dpath_contents(dpath, &tk, NULL);
    expr_dpath_destroy(dpath);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    ck_assert_ptr_ne(tk, NULL);

    bt_ret = tracker_goto_first_item(tk, NULL);
    cur_depth = 1;
    expect_box = NULL;
    for (box_idx = 0; box_idx < test_spec->n_expect_boxes; ++box_idx) {
        expect_box = &test_spec->expect_boxes[box_idx];

        if (BITPUNCH_OK != bt_ret &&
            BITPUNCH_NO_ITEM != bt_ret) {
            break ;
        }
        if (BITPUNCH_OK == bt_ret) {
            if (! only_browse) {
                check_tracker_item(tk, expect_box);
            }
            if (BITPUNCH_OK != expect_box->read_item_ret) {
                expr_value_t dummy_value;

                bt_ret = tracker_read_item_value(tk, &dummy_value, NULL);
                ck_assert_int_eq(bt_ret, expect_box->read_item_ret);
            }
            bt_ret = tracker_get_item_filter(tk, &item_filter, NULL);
            ck_assert_int_eq(bt_ret, BITPUNCH_OK);
            if (ast_node_is_trackable(item_filter)
                && AST_NODE_TYPE_BYTE_ARRAY != item_filter->ndat->type) {
                /* recurse in container */
                bt_ret = tracker_enter_item(tk, NULL);
                ck_assert(bt_ret == BITPUNCH_OK ||
                          bt_ret == expect_box->read_item_ret);
                if (BITPUNCH_OK == bt_ret) {
                    ++cur_depth;

                    bt_ret = tracker_get_n_items(tk, &n_items, NULL);
                    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
                    ck_assert_int_eq(n_items, expect_box->n_items);

                    bt_ret = tracker_goto_first_item(tk, NULL);
                }
            } else if (BITPUNCH_OK == bt_ret) {
                bt_ret = tracker_goto_next_item(tk, NULL);
            }
        } else {
            ck_assert_int_eq(bt_ret, expect_box->read_item_ret);
        }
        while (BITPUNCH_NO_ITEM == bt_ret && cur_depth > 1) {
            --cur_depth;
            bt_ret = tracker_return(tk, NULL);
            ck_assert_int_eq(bt_ret, BITPUNCH_OK);
            bt_ret = tracker_goto_next_item(tk, NULL);
        }
    }
    if (!test_spec->truncated) {
        ck_assert(bt_ret == BITPUNCH_OK ||
                  bt_ret == BITPUNCH_NO_ITEM);
        if (box_idx == test_spec->n_expect_boxes) {
            /* we shall have reached the end */
            bt_ret = tracker_goto_next_item(tk, NULL);
            ck_assert_int_eq(bt_ret, BITPUNCH_NO_ITEM);
            bt_ret = tracker_return(tk, NULL);
            ck_assert_int_eq(bt_ret, BITPUNCH_NO_ITEM);
        }
    }
    tracker_delete(tk);

    ret = bitpunch_data_source_release(ds_in);
    ck_assert_int_eq(ret, 0);

    bitpunch_board_free(board);
}


static void
check_tracker_browse_sub_trackers_recur(struct tracker *tk,
                                        const struct test_tracker_spec *test_spec,
                                        int start_box_idx,
                                        int only_browse)
{
    int box_idx;
    const struct test_tracker_expect_box *expect_box;
    bitpunch_status_t bt_ret;
    struct tracker *sub_tk = NULL;
    const char *dpath_prefix;
    const char *dpath_sub_prefix;
    int64_t n_items;

    if (-1 != start_box_idx) {
        expect_box = &test_spec->expect_boxes[start_box_idx];
        dpath_prefix = expect_box->path;
        bt_ret = tracker_get_n_items(tk, &n_items, NULL);
        ck_assert_int_eq(bt_ret, BITPUNCH_OK);
        ck_assert_int_eq(n_items, expect_box->n_items);
    } else {
        dpath_prefix = "";
    }
    bt_ret = tracker_goto_first_item(tk, NULL);
    for (box_idx = start_box_idx + 1;
         box_idx < test_spec->n_expect_boxes;
        ) {
        expect_box = &test_spec->expect_boxes[box_idx];
        if (! is_dpath_prefix_of(dpath_prefix, expect_box->path)) {
            break ;
        }
        if (! only_browse) {
            check_tracker_item(tk, expect_box);
        }
        if (BITPUNCH_OK != expect_box->read_item_ret) {
            bt_ret = tracker_read_item_value(tk, NULL, NULL);
            ck_assert_int_eq(bt_ret, expect_box->read_item_ret);
        } else {
            struct ast_node_hdl *item_filter;

            bt_ret = tracker_get_item_filter(tk, &item_filter, NULL);
            ck_assert_int_eq(bt_ret, BITPUNCH_OK);
            if (ast_node_is_trackable(item_filter)
                && AST_NODE_TYPE_BYTE_ARRAY != item_filter->ndat->type) {
                bt_ret = track_item_contents(tk, &sub_tk, NULL);
                ck_assert_int_eq(bt_ret, BITPUNCH_OK);
                ck_assert_ptr_ne(sub_tk, NULL);

                check_tracker_browse_sub_trackers_recur(sub_tk, test_spec,
                                                        box_idx,
                                                        only_browse);
                tracker_delete(sub_tk);
            }
        }
        bt_ret = tracker_goto_next_item(tk, NULL);
        if (BITPUNCH_OK != bt_ret) {
            return ;
        }
        dpath_sub_prefix = expect_box->path;
        ++box_idx;
        while (box_idx < test_spec->n_expect_boxes
               && is_dpath_prefix_of(dpath_sub_prefix,
                                     test_spec->expect_boxes[box_idx].path)) {
            ++box_idx;
        }
    }
    ck_assert(bt_ret == BITPUNCH_OK ||
              bt_ret == BITPUNCH_NO_ITEM);
    bt_ret = tracker_goto_next_item(tk, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_NO_ITEM);
}

void
check_tracker_browse_sub_trackers(const struct test_tracker_spec *test_spec,
                                  int only_browse)
{
    struct bitpunch_board *board;
    struct ast_node_hdl *ds_in;
    expr_dpath_t dpath;
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    int ret;

    board = bitpunch_board_new();

    ret = bitpunch_data_source_create_from_memory(
        &ds_in, test_spec->contents, test_spec->contents_size, FALSE);
    ck_assert_int_eq(ret, 0);

    bitpunch_board_add_let_expression(board, "data", ds_in);
    bitpunch_board_add_let_expression(board, "Schema", *test_spec->schema_hdl);

    bt_ret = bitpunch_board_add_expr(board, "Model", "data <> Schema.Root");
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    bt_ret = bitpunch_eval_expr(board, "Model", NULL, 0u,
                                NULL, NULL, &dpath, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    bt_ret = track_dpath_contents(dpath, &tk, NULL);
    expr_dpath_destroy(dpath);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    ck_assert_ptr_ne(tk, NULL);

    check_tracker_browse_sub_trackers_recur(tk, test_spec, -1,
                                            only_browse);

    tracker_delete(tk);

    ret = bitpunch_data_source_release(ds_in);
    ck_assert_int_eq(ret, 0);

    bitpunch_board_free(board);
}

void
check_tracker_browse_random_dpath(const struct test_tracker_spec *test_spec,
                                  int only_browse)
{
    struct bitpunch_board *board;
    struct ast_node_hdl *ds_in;
    expr_dpath_t dpath;
    struct tracker *tk;
    int ret;
    int box_idx;
    const struct test_tracker_expect_box *expect_box;
    bitpunch_status_t bt_ret;
    int64_t n_items;
    int *random_box_indices;
    int seed;
    int i;
    int j;
    int swap_idx;

    board = bitpunch_board_new();

    ret = bitpunch_data_source_create_from_memory(
        &ds_in, test_spec->contents, test_spec->contents_size, FALSE);
    ck_assert_int_eq(ret, 0);

    bitpunch_board_add_let_expression(board, "data", ds_in);
    bitpunch_board_add_let_expression(board, "Schema", *test_spec->schema_hdl);

    bt_ret = bitpunch_board_add_expr(board, "Model", "data <> Schema.Root");
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);

    bt_ret = bitpunch_eval_expr(board, "Model", NULL, 0u,
                                NULL, NULL, &dpath, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    bt_ret = track_dpath_contents(dpath, &tk, NULL);
    expr_dpath_destroy(dpath);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    ck_assert_ptr_ne(tk, NULL);

    random_box_indices = malloc_safe(test_spec->n_expect_boxes
                                     * sizeof(*random_box_indices));
    for (seed = 42; seed < 50; ++seed) {
        /* use a specific series of seeds to be able to replay the
         * same sequences (TODO make this configurable) */
        srand(seed);

        /* init indices */
        for (i = 0; i < test_spec->n_expect_boxes; ++i) {
            random_box_indices[i] = i;
        }
        /* shuffle indices */
        for (i = 0; i < test_spec->n_expect_boxes; ++i) {
            j = rand() % test_spec->n_expect_boxes;
            swap_idx = random_box_indices[j];
            random_box_indices[j] = random_box_indices[i];
            random_box_indices[i] = swap_idx;
        }
        /* start random test */
        for (i = 0; i < test_spec->n_expect_boxes; ++i) {
            box_idx = random_box_indices[i];
            expect_box = &test_spec->expect_boxes[box_idx];

            bt_ret = tracker_goto_abs_dpath(tk, expect_box->path, NULL);
            /* browse functions may catch out-of-bounds errors earlier
             * than read, so allow it */
            ck_assert(bt_ret == BITPUNCH_OK ||
                      bt_ret == expect_box->read_item_ret);
            if (BITPUNCH_OK == bt_ret) {
                if (! only_browse) {
                    check_tracker_item(tk, expect_box);
                }
                if (BITPUNCH_OK == expect_box->read_item_ret) {
                    struct ast_node_hdl *item_filter;

                    bt_ret = tracker_get_item_filter(tk, &item_filter, NULL);
                    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
                    if (ast_node_is_trackable(item_filter)
                        && AST_NODE_TYPE_BYTE_ARRAY
                        != item_filter->ndat->type) {
                        bt_ret = tracker_enter_item(tk, NULL);
                        ck_assert_int_eq(bt_ret, BITPUNCH_OK);

                        bt_ret = tracker_get_n_items(tk, &n_items, NULL);
                        ck_assert_int_eq(bt_ret, BITPUNCH_OK);
                        ck_assert_int_eq(n_items, expect_box->n_items);
                    }
                }
            }
        }
    }
    free(random_box_indices);

    tracker_delete(tk);

    ret = bitpunch_data_source_release(ds_in);
    ck_assert_int_eq(ret, 0);

    bitpunch_board_free(board);
}


void
check_tracker_item(struct tracker *tk,
                   const struct test_tracker_expect_box *expect_box)
{
    bitpunch_status_t bt_ret;
    expr_value_t value;
    int iret;
    char dpath_expr[256];
    int64_t item_offset;
    int64_t item_size;

    ck_assert_int_eq(tracker_is_dangling(tk), FALSE);
    if (check_verbose) {
        printf("=== Before read ===\n");
        dbg_tracker_dump("", tk);
    }
    bt_ret = tracker_get_item_key(tk, &value, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    ck_assert_int_eq(value.type, expect_box->key_type);
    check_expr_value_match(&value, &expect_box->key);
    expr_value_destroy(value);

    bt_ret = tracker_read_item_value(tk, &value, NULL);
    if (check_verbose) {
        printf("=== After read ===\n");
        dbg_tracker_dump("", tk);
    }
    ck_assert_int_eq(bt_ret, expect_box->read_item_ret);
    if (BITPUNCH_OK != expect_box->read_item_ret) {
        return ;
    }
    if (EXPR_VALUE_TYPE_UNSET != expect_box->value_type) {
        ck_assert_int_eq(value.type, expect_box->value_type);
        check_expr_value_match(&value, &expect_box->value);
    }
    expr_value_destroy(value);
    iret = tracker_get_abs_dpath(tk, dpath_expr, sizeof (dpath_expr));
    ck_assert_int_gt(iret, 0);
    ck_assert_int_eq(iret, strlen(expect_box->path));
    ck_assert_str_eq(dpath_expr, expect_box->path);
    bt_ret = tracker_get_item_location(tk, &item_offset, &item_size, NULL);
    ck_assert_int_eq(bt_ret, BITPUNCH_OK);
    ck_assert_int_eq(item_offset, expect_box->offset);
    ck_assert_int_eq(item_size, expect_box->size);
}

void
check_expr_value_match(const expr_value_t *value1,
                       const expr_value_t *value2)
{
    switch (value1->type) {
    case EXPR_VALUE_TYPE_INTEGER:
        ck_assert_int_eq(value1->integer, value2->integer);
        break ;
    case EXPR_VALUE_TYPE_BOOLEAN:
        ck_assert_int_eq(value1->boolean, value2->boolean);
        break ;
    case EXPR_VALUE_TYPE_STRING:
        ck_assert_int_eq(value1->string.len, value2->string.len);
        ck_assert(0 == memcmp(value1->string.str,
                              value2->string.str,
                              value1->string.len));
        break ;
    case EXPR_VALUE_TYPE_BYTES:
        ck_assert_int_eq(value1->bytes.len, value2->bytes.len);
        ck_assert(0 == memcmp(value1->bytes.buf,
                              value2->bytes.buf,
                              value1->bytes.len));
        break ;
    default:
        assert(0);
    }
}
