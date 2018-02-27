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

#ifndef __CHECK_TRACKER_H__
#define __CHECK_TRACKER_H__

#include "api/bitpunch_api.h"
#include "core/browse.h"

struct test_tracker_expect_box {
    const char *path;
    int64_t offset;
    int64_t size;
    enum expr_value_type key_type;
    expr_value_t key;
    enum expr_value_type value_type;
    expr_value_t value;
    int64_t n_items;
    bitpunch_status_t read_item_ret;
};

struct test_tracker_spec {
    const char *test_name;
    struct bitpunch_schema_hdl **contents_def;
    const char *contents;
    size_t contents_size;
    const struct test_tracker_expect_box *expect_boxes;
    int n_expect_boxes;
    bitpunch_status_t tracker_error;
};

void
check_tracker_launch_tests(const struct test_tracker_spec *test_specs[],
                           int n_test_specs);
void
check_tracker_launch_test(const struct test_tracker_spec *test_spec);
void
check_tracker_browse_depth_first(const struct test_tracker_spec *test_spec,
                                 int only_browse);
void
check_tracker_browse_sub_trackers(const struct test_tracker_spec *test_spec,
                                  int only_browse);
void
check_tracker_browse_random_dpath(const struct test_tracker_spec *test_spec,
                                  int only_browse);
void
check_tracker_item(struct tracker *tk,
                   const struct test_tracker_expect_box *expect_box);

void
check_expr_value_match(const expr_value_t *value1,
                       const expr_value_t *value2);

#endif /*__CHECK_TRACKER_H__*/
