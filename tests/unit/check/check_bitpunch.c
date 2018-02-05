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
#include <check.h>
#include <unistd.h>

#include "api/bitpunch_api.h"
#include "core/print.h"

#include "check_bitpunch.h"

static void
usage(void)
{
    fprintf(stderr,
            "usage: check_bitpunch [-v][-h]\n"
            "  -v: more verbose output\n"
            "  -h: show usage help\n");
}

int check_verbose = 0;

Suite *bitpunch_suite(void)
{
    Suite *s;

    s = suite_create("BitPunch");
    check_struct_add_tcases(s);
    check_array_add_tcases(s);
    check_slack_add_tcases(s);
    check_cond_add_tcases(s);
    check_index_add_tcases(s);
    check_dynarray_add_tcases(s);
    testcase_radio_add_tests(s);
    check_base64_add_tcases(s);
    check_dep_resolver_add_tcases(s);
    return s;
}

int main(int argc, char *argv[])
{
    int opt;
    int number_failed;
    Suite *s;
    SRunner *sr;

    while (-1 != (opt = getopt(argc, argv, "vdh"))) {
        switch (opt) {
        case 'v':
            ++check_verbose;
            break ;
        case 'd':
#if defined DEBUG
            tracker_debug_mode = TRUE;
#endif
            break ;
        case 'h':
        case '?':
        default:
            usage();
            exit(EXIT_FAILURE);
        }
    }
    if (-1 == bitpunch_init()) {
        exit(1);
    }
    s = bitpunch_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_VERBOSE);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    bitpunch_cleanup();
    return (number_failed == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
}
