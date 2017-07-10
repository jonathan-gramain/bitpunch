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

#define _BSD_SOURCE
#define _GNU_SOURCE
#include <sys/types.h>
#include <assert.h>
#include <string.h>

#include "core/interpreter.h"

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
        // TODO log: invalid input
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
            // TODO log: invalid input
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
        // invalid input
        return -1;
    }
    if ((decoded_quad & 0xff000000u) != 0) {
        // TODO log: invalid input
        return -1;
    }
    return (char *)out - decoded;
}

static int
base64_read(union expr_value *read_value,
            const char *data, size_t span_size,
            const struct ast_node *param_values)
{
    size_t decoded_max_length;
    int64_t decoded_length;
    char *decoded;

    decoded_max_length = (span_size + 3) & ~0x3;
    if (decoded_max_length > DECODED_BUFFER_MAX_SIZE) {
        //TODO log
        return -1;
    }
    decoded = malloc_safe(decoded_max_length);
    decoded_length = base64_decode(data, span_size, decoded);
    if (-1 == decoded_length) {
        return -1;
    }
    read_value->bytes.buf = decoded;
    read_value->bytes.len = decoded_length;
    return 0;
}

static int
base64_write(const union expr_value *write_value,
             char *data, size_t span_size,
             const struct ast_node *param_values)
{
    return -1;
}


static int
base64_rcall_build(struct ast_node *rcall,
                   const struct ast_node *call,
                   const struct ast_node *param_values)
{
    const struct ast_node *data_source;

    data_source = call->u.rexpr_filter.target;
    if (AST_NODE_TYPE_BYTE_ARRAY != data_source->type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &call->loc,
            "base64 interpreter expects a byte array");
        return -1;
    }
    rcall->u.rexpr_interpreter.read_func = base64_read;
    rcall->u.rexpr_interpreter.write_func = base64_write;
    return 0;
}

void
interpreter_declare_base64(void)
{
    int ret;

    ret = interpreter_declare("base64",
                              EXPR_VALUE_TYPE_BYTES,
                              base64_rcall_build,
                              0);
    assert(0 == ret);
}


#ifndef DISABLE_UTESTS

#include <check.h>
#include <stdlib.h>


START_TEST(test_base64)
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
    union expr_value decoded;
    const struct testcase *tcase;
    int ret;

    for (i = 0; i < N_ELEM(testcases); ++i)
        {
            tcase = &testcases[i];
            ret = base64_read(&decoded,
                              tcase->encoded, tcase->encoded_length,
                              NULL);
            if (tcase->decoded_length != -1) {
                ck_assert(ret != -1);
                ck_assert(decoded.bytes.len == tcase->decoded_length);
                ck_assert(0 == memcmp(decoded.bytes.buf,
                                      tcase->decoded, decoded.bytes.len));
                //expr_value_destroy(EXPR_VALUE_TYPE_BYTES, &decoded);
                free((char *)decoded.bytes.buf);
            } else {
                ck_assert(ret == -1);
            }
        }
}
END_TEST

void check_base64_add_tcases(Suite *s)
{
    TCase *tc_base64;

    tc_base64 = tcase_create("base64");
    tcase_add_test(tc_base64, test_base64);
    suite_add_tcase(s, tc_base64);
}

#endif // #ifndef DISABLE_UTESTS
