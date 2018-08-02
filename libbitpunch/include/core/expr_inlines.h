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

// DO NOT INCLUDE THIS FILE DIRECTLY
// It shall only be included from expr.h

#include <string.h> // strlen()

static inline expr_dpath_t
expr_dpath_none(void)
{
  expr_dpath_t dpath;

  dpath.type = EXPR_DPATH_TYPE_NONE;
  return dpath;
}

static inline expr_dpath_t
expr_dpath_as_item(struct tracker *tk)
{
  expr_dpath_t dpath;

  dpath.type = EXPR_DPATH_TYPE_ITEM;
  dpath.item.tk = tk;
  return dpath;
}

static inline expr_dpath_t
expr_dpath_as_container(struct box *box)
{
  expr_dpath_t dpath;

  dpath.type = EXPR_DPATH_TYPE_CONTAINER;
  dpath.container.box = box;
  return dpath;
}

static inline expr_value_t
expr_value_unset(void)
{
    expr_value_t ev;

    ev.type = EXPR_VALUE_TYPE_UNSET;
    return ev;
}

static inline expr_value_t
expr_value_as_integer(int64_t value)
{
    expr_value_t ev;

    ev.type = EXPR_VALUE_TYPE_INTEGER;
    ev.integer = value;
    return ev;
}

static inline expr_value_t
expr_value_as_boolean(int value)
{
    expr_value_t ev;

    ev.type = EXPR_VALUE_TYPE_BOOLEAN;
    ev.boolean = value;
    return ev;
}

static inline expr_value_t
expr_value_as_string(const char *str)
{
    return expr_value_as_string_len(str, strlen(str));
}

static inline expr_value_t
expr_value_as_string_len(const char *str, int64_t len)
{
    expr_value_t ev;

    ev.type = EXPR_VALUE_TYPE_STRING;
    ev.string.str = str;
    ev.string.len = len;
    ev.string.from_box = NULL;
    return ev;
}

static inline expr_value_t
expr_value_as_bytes(const char *buf, int64_t len)
{
    expr_value_t ev;

    ev.type = EXPR_VALUE_TYPE_BYTES;
    ev.bytes.buf = buf;
    ev.bytes.len = len;
    ev.bytes.from_box = NULL;
    return ev;
}
