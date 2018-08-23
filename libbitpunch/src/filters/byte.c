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

#include <assert.h>

#include "core/filter.h"

static struct filter_instance *
byte_filter_instance_build(struct ast_node_hdl *filter)
{
    struct filter_instance *byte;

    byte = new_safe(struct filter_instance);
    return byte;
}

static int
compile_span_size_byte(struct ast_node_hdl *item,
                       struct filter_instance *byte,
                       struct compile_ctx *ctx)
{
    item->ndat->u.item.min_span_size = 1;
    return 0;
}

static int
byte_filter_instance_compile(struct ast_node_hdl *filter,
                             struct filter_instance *f_instance,
                             dep_resolver_tagset_t tags,
                             struct compile_ctx *ctx)
{
    if (0 != (tags & COMPILE_TAG_NODE_SPAN_SIZE)
        && -1 == compile_span_size_byte(filter, f_instance, ctx)) {
        return -1;
    }
    return 0;
}

void
filter_class_declare_byte(void)
{
    int ret;

    ret = filter_class_declare("byte",
                               EXPR_VALUE_TYPE_BYTES,
                               byte_filter_instance_build,
                               byte_filter_instance_compile,
                               0);
    assert(0 == ret);
}
