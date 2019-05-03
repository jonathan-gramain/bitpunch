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
#include <assert.h>
#include <string.h>

#include "core/filter.h"

#define DECODED_BUFFER_MAX_SIZE (100*1024*1024)

/* lookup table indexed by ascii value of each base64-encoded char */
static const char lookup[256] = {
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1,
    -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
    -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1,

    /* non-ascii chars are all invalid */
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
};

static int64_t
base64_decode(const char *encoded, size_t encoded_size,
              char *decoded)
{
    const unsigned char *in;
    unsigned char *out;
    unsigned int decoded_quad;
    const unsigned char *encoded_end;

    in = (const unsigned char *)encoded;
    out = (unsigned char *)decoded;
    if ((encoded_size % 4) != 0) {
        return -1;
    }
    if (0 == encoded_size) {
        return 0;
    }
    encoded_end = (const unsigned char *)encoded + encoded_size - 4;
    while (in < encoded_end) {
        decoded_quad = ((unsigned int)lookup[*in++] << 18);
        decoded_quad |= ((unsigned int)lookup[*in++] << 12);
        decoded_quad |= ((unsigned int)lookup[*in++] << 6);
        decoded_quad |= ((unsigned int)lookup[*in++]);
        /* If any looked up value is -1, converted to unsigned int it
         * will set the top-most 8 bits (at least) of decoded_quad, so
         * those bits shall be 0 if and only if all input chars are
         * valid base64 chars. */
        if ((decoded_quad & 0xff000000u) != 0) {
            return -1;
        }
        *out++ = ((decoded_quad >> 16) & 0xff);
        *out++ = ((decoded_quad >> 8) & 0xff);
        *out++ = decoded_quad & 0xff;
    }
    decoded_quad = ((unsigned int)lookup[*in++] << 18);
    decoded_quad |= ((unsigned int)lookup[*in++] << 12);
    *out++ = (decoded_quad >> 16) & 0xff;
    if (*in != '=') {
        decoded_quad |= ((unsigned int)lookup[*in++] << 6);
        *out++ = (decoded_quad >> 8) & 0xff;
        if (*in != '=') {
            decoded_quad |= (unsigned int)lookup[*in++];
            *out++ = decoded_quad & 0xff;
        }
    } else if (in[1] != '=') {
        return -1;
    }
    if ((decoded_quad & 0xff000000u) != 0) {
        return -1;
    }
    return (char *)out - decoded;
}

static bitpunch_status_t
base64_read(
    struct ast_node_hdl *filter,
    struct box *scope,
    const char *buffer, size_t buffer_size,
    expr_value_t *valuep,
    struct browse_state *bst)
{
    size_t decoded_max_length;
    struct bitpunch_data_source *ds;

    decoded_max_length = (buffer_size + 3) & ~0x3;
    if (decoded_max_length > DECODED_BUFFER_MAX_SIZE) {
        return node_error(
            BITPUNCH_DATA_ERROR, filter, bst,
            "base64 decode buffer too large (%zu bytes, max %d)",
            decoded_max_length, DECODED_BUFFER_MAX_SIZE);
    }
    bitpunch_buffer_new(&ds, decoded_max_length);
    ds->ds_data_length = base64_decode(buffer, buffer_size, ds->ds_data);
    if (-1 == ds->ds_data_length) {
        // TODO add precision regarding the error
        return node_error(BITPUNCH_DATA_ERROR, filter, bst,
                          "invalid base64 input");
    }
    *valuep = expr_value_as_data(ds);
    return BITPUNCH_OK;
}

static struct filter_instance *
base64_filter_instance_build(struct ast_node_hdl *filter)
{
    struct filter_instance *f_instance;

    f_instance = new_safe(struct filter_instance);
    f_instance->b_item.read_value_from_buffer = base64_read;
    return f_instance;
}

void
builtin_filter_declare_base64(void)
{
    int ret;

    ret = builtin_filter_declare("base64",
                               EXPR_VALUE_TYPE_BYTES,
                               base64_filter_instance_build, NULL,
                               0u,
                               0);
    assert(0 == ret);
}


#ifndef DISABLE_UTESTS

#include <check.h>
#include <stdlib.h>


START_TEST(test_filter_base64)
{
    static const struct testcase {
        int encoded_length;
        const char *encoded;
        int decoded_length;
        const char *decoded;
    } testcases[] = {
        { 0, "", 0, "" },
        { 4, "Wg==", 1, "Z" },
        { 3, "Wg=", -1, NULL },
        { 2, "Wg", -1, NULL },
        { 4, "Wg  ", -1, NULL },
        { 4, "Wg =", -1, NULL },
        { 4, "Wg= ", -1, NULL },
        { 4, "Wlo=", 2, "ZZ" },
        { 4, "Wlpa", 3, "ZZZ" },
        { 4, "WlpaWg==", 3, "ZZZZ" },
        { 8, "aGVsbG8=", 5, "hello" },
        { 7, "aGVsbG8", -1, NULL },
        { 8, "AAAAAA==", 4, "\0\0\0\0" },
        { 108, "VGhpcyBpcyBhIHByZXR0eSBsb25nIHRleHQganVzdCB0byBtYWtlIHN1"
          "cmUgaXQgYWxzbyB3b3JrcyB3aXRoIGxvbmdlciBwaHJhc2VzLg==",
          79, "This is a pretty long text just to make sure it also "
          "works with longer phrases." },
        { 4, "*lpa", -1, NULL },
        { 4, "W*pa", -1, NULL },
        { 4, "Wl*a", -1, NULL },
        { 4, "Wlp*", -1, NULL },
        { 8, "Wlpa*g==", -1, NULL },
        { 8, "WlpaW*==", -1, NULL },
        { 8, "WlpaWg*=", -1, NULL },
        { 8, "WlpaWg=*", -1, NULL },
        { 8, "\0\012\023\034\045\056\067\077", -1, NULL },
    };
    int i;
    expr_value_t decoded;
    struct bitpunch_data_source *ds;
    const struct testcase *tcase;
    bitpunch_status_t bt_ret;

    for (i = 0; i < N_ELEM(testcases); ++i)
        {
            tcase = &testcases[i];
            bt_ret = base64_read(NULL, NULL,
                                 tcase->encoded, tcase->encoded_length,
                                 &decoded, NULL);
            if (tcase->decoded_length != -1) {
                ck_assert(BITPUNCH_OK == bt_ret);
                ck_assert(EXPR_VALUE_TYPE_DATA == decoded.type);
                ds = decoded.data.ds;
                ck_assert(ds->ds_data_length == tcase->decoded_length);
                ck_assert(0 == memcmp(ds->ds_data,
                                      tcase->decoded, ds->ds_data_length));
                expr_value_destroy(decoded);
            } else {
                ck_assert(BITPUNCH_DATA_ERROR == bt_ret);
            }
        }
}
END_TEST

void check_filter_base64_add_tcases(Suite *s)
{
    TCase *tc_filter_base64;

    tc_filter_base64 = tcase_create("filter:base64");
    tcase_add_test(tc_filter_base64, test_filter_base64);
    suite_add_tcase(s, tc_filter_base64);
}

#endif // #ifndef DISABLE_UTESTS
