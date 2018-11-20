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

#include "filters/byte.h"
#include "filters/bytes.h"

struct ast_node_data shared_ast_node_data_byte = {
    .type = AST_NODE_TYPE_BYTE,
    .u = {
        .item = {
            .min_span_size = (int64_t)-1,
        }
    }
};
struct ast_node_hdl shared_ast_node_byte = {
    .ndat = &shared_ast_node_data_byte,
};

#define AST_NODE_BYTE &shared_ast_node_byte

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

static bitpunch_status_t
box_get_n_items__byte(
    struct box *box, int64_t *item_countp,
    struct browse_state *bst)
{
    *item_countp = 1;
    return BITPUNCH_OK;
}

static void
compile_node_backends__box__byte(struct ast_node_hdl *item)
{
    struct box_backend *b_box = NULL;

    b_box = &item->ndat->u.rexpr_filter.f_instance->b_box;
    memset(b_box, 0, sizeof (*b_box));

    b_box->compute_slack_size = box_compute_slack_size__as_container_slack;
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->compute_span_size = box_compute_span_size__const_size;
    b_box->compute_max_span_size = box_compute_max_span_size__as_span;
    b_box->get_n_items = box_get_n_items__byte;
    b_box->compute_used_size = box_compute_used_size__as_span;
}


void
compile_node_backends__byte(struct ast_node_hdl *item)
{
    struct filter_instance *f_instance;

    f_instance = item->ndat->u.rexpr_filter.f_instance;

    compile_node_backends__item__generic(item);
    f_instance->read_func = bytes__read;
    compile_node_backends__box__byte(item);
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
    if (0 != (tags & COMPILE_TAG_BROWSE_BACKENDS)) {
        compile_node_backends__byte(filter);
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
                               0u,
                               0);
    assert(0 == ret);

    ret = filter_instance_build_shared(AST_NODE_BYTE, "byte");
    assert(0 == ret);
}

void
compile_global_nodes__byte(struct compile_ctx *ctx)
{
    (void)compile_span_size_byte(AST_NODE_BYTE, NULL, NULL);

    compile_node_backends__byte(AST_NODE_BYTE);
}

struct ast_node_hdl *
filter_get_global_instance__byte(void)
{
    return AST_NODE_BYTE;
}
