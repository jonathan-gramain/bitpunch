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

#include "api/bitpunch_api.h"
#include "core/filter.h"
#include "filters/bytes.h"

struct filter_instance_data_source {
    struct filter_instance filter; /* inherits */
    struct bitpunch_data_source *data_source;
};

struct ast_node_hdl *
bitpunch_data_source_to_filter(struct bitpunch_data_source *ds)
{
    struct filter_class *filter_cls;
    struct ast_node_hdl *node;
    int ret;
    struct filter_instance_data_source *f_instance;

    filter_cls = filter_class_lookup("__data_source__");
    assert(NULL != filter_cls);

    node = ast_node_hdl_new();
    ret = filter_instance_build(node, filter_cls,
                                filter_def_create_empty("__data_source__"));
    assert(0 == ret);

    f_instance = (struct filter_instance_data_source *)
        node->ndat->u.rexpr_filter.f_instance;
    f_instance->data_source = ds;

    return node;
}

static bitpunch_status_t
data_source_get_data_source(
    struct ast_node_hdl *filter,
    struct box *scope,
    struct bitpunch_data_source **ds_outp,
    struct browse_state *bst)
{
    struct filter_instance_data_source *f_instance;

    f_instance = (struct filter_instance_data_source *)
        filter->ndat->u.rexpr_filter.f_instance;
    *ds_outp = f_instance->data_source;
    return BITPUNCH_OK;
}

static struct filter_instance *
data_source_filter_instance_build(struct ast_node_hdl *filter)
{
    struct filter_instance_data_source *f_instance;

    f_instance = new_safe(struct filter_instance_data_source);
    f_instance->filter.b_item.get_data_source = data_source_get_data_source;
    return (struct filter_instance *)f_instance;
}

void
filter_class_declare_data_source(void)
{
    int ret;

    ret = filter_class_declare("__data_source__",
                               EXPR_VALUE_TYPE_BYTES,
                               data_source_filter_instance_build, NULL,
                               0u,
                               0);
    assert(0 == ret);
}
