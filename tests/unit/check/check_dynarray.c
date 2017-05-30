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

#include <stdlib.h>
#include <inttypes.h>
#include <check.h>
#include <stdio.h>

#include "utils/dynarray.h"

struct test_item {
    int a;
    char b;
};

ARRAY_HEAD(test_array, struct test_item);

START_TEST(test_dynarray)
{
    struct test_array array;
    struct test_item item;
    int i;

    ARRAY_INIT(&array, 0, struct test_item);
    for (i = 0; i < 26; ++i) {
        item.a = 1 + i;
        item.b = 'a' + i;
        ARRAY_APPEND(&array, item, struct test_item);
    }
    for (i = 25; i >= 0; --i) {
        ck_assert_int_eq(ARRAY_ITEM(&array, i).a, 1 + i);
        ck_assert_int_eq(ARRAY_ITEM(&array, i).b, 'a' + i);
    }
    ARRAY_DESTROY(&array);

    ARRAY_INIT(&array, 2, struct test_item);
    for (i = 0; i < 26; ++i) {
        item.a = 1 + i;
        item.b = 'a' + i;
        if (i < 2)
            ARRAY_ITEM(&array, i) = item;
        else
            ARRAY_APPEND(&array, item, struct test_item);
    }
    ck_assert_int_eq(ARRAY_FIRST(&array).a, 1);
    ck_assert_int_eq(ARRAY_LAST(&array).a, 26);
    for (i = 25; i >= 0; --i) {
        ck_assert_int_eq(ARRAY_ITEM(&array, i).a, 1 + i);
        ck_assert_int_eq(ARRAY_ITEM(&array, i).b, 'a' + i);
    }
    ARRAY_DESTROY(&array);

    ARRAY_INIT(&array, 1234, struct test_item);
    for (i = 0; i < 200000; ++i) {
        item.a = i;
        item.b = 42;
        if (i < 1234)
            ARRAY_ITEM(&array, i) = item;
        else
            ARRAY_APPEND(&array, item, struct test_item);
    }
    for (i = 199999; i >= 0; --i) {
        ck_assert_int_eq(ARRAY_ITEM(&array, i).b, 42);
        ck_assert_int_eq(ARRAY_POP(&array, struct test_item).a, i);

    }
    ARRAY_DESTROY(&array);
}
END_TEST


void check_dynarray_add_tcases(Suite *s)
{
    TCase *tc_dynarray;

    tc_dynarray = tcase_create("dynarray");
    tcase_add_test(tc_dynarray, test_dynarray);
    suite_add_tcase(s, tc_dynarray);
}
