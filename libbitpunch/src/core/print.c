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
#include <string.h>
#include <assert.h>
#include <ctype.h>

#include "core/parser.h"
#include PATH_TO_PARSER_TAB_H
#include "core/browse.h"
#include "core/print.h"

int
print_expr_string(const struct expr_value_string *string, FILE *stream)
{
    int ret;

    fputc('"', stream);
    ret = print_bytes(string->str, string->len, stream, 20);
    fputc('"', stream);
    return 1 + ret + 1;
}

int
print_expr_bytes(const struct expr_value_bytes *bytes, FILE *stream)
{
    int ret;

    fputc('"', stream);
    ret = print_bytes(bytes->buf, bytes->len, stream, 20);
    fputc('"', stream);
    return 1 + ret + 1;
}

int
print_expr_value(enum expr_value_type type, union expr_value value,
                 FILE *stream)
{
    switch (type) {
    case EXPR_VALUE_TYPE_INTEGER:
        return fprintf(stream, "%"PRIi64"", value.integer);
    case EXPR_VALUE_TYPE_BOOLEAN:
        return fprintf(stream, "%s", (value.boolean ? "true" : "false"));
    case EXPR_VALUE_TYPE_STRING:
        return print_expr_string(&value.string, stream);
    case EXPR_VALUE_TYPE_BYTES:
        return print_expr_bytes(&value.bytes, stream);
    default:
        return fprintf(stream, "<data:%s>", expr_value_type_str(type));
    }
    /*NOT REACHED*/
}

int
print_bytes(const char *bytes, int64_t length, FILE *stream,
            int max_output_bytes)
{
#define ESCAPED_CHARS "\r\n\t\"\\"
#define ESCAPED_CHARS_REPR "rnt\"\\"
    static const char *escaped_chars = ESCAPED_CHARS;
    static const char *escaped_chars_repr = ESCAPED_CHARS_REPR;
    const unsigned char *p;
    const unsigned char *p_end;
    const char *escaped_char_p;
    int n_out;

    if (length <= max_output_bytes) {
        p_end = (const unsigned char *)bytes + length;
    } else {
        p_end = (const unsigned char *)bytes + max_output_bytes;
    }
    n_out = 0;
    for (p = (const unsigned char *)bytes; p < p_end; ++p) {
        if (isprint(*p)) {
            fputc(*p, stream);
            ++n_out;
        } else {
            fputc('\\', stream);
            escaped_char_p = memchr(escaped_chars, *p,
                                    sizeof (ESCAPED_CHARS) - 1);
            if (NULL != escaped_char_p) {
                /* standard escaped representation */
                fputc(escaped_chars_repr[escaped_char_p - escaped_chars],
                      stream);
                n_out += 2;
            } else {
                /* octal representation */
                if (p + 1 < p_end && isdigit(*(p + 1))) {
                    /* force output of three octal digits to
                     * disanbiguate or avoid confusion because next
                     * character is a digit */
                    n_out += 1 + fprintf(stream, "%03o", *p);
                } else {
                    n_out += 1 + fprintf(stream, "%o", *p);
                }
            }
        }
    }
    if (length > max_output_bytes) {
        fputs("...", stream);
        n_out += 3;
    }
    return n_out;
}
