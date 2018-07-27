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

#include "filters/array.h"

static struct filter_instance *
array_filter_instance_build(struct ast_node_hdl *filter,
                            struct compile_ctx *ctx)
{
    struct named_expr *attr;
    struct ast_node_hdl *item_type;
    struct ast_node_hdl *item_count;
    struct filter_instance_array *array;

    item_type = NULL;
    item_count = NULL;
    // FIXME this does not support conditional attributes
    STATEMENT_FOREACH(
        named_expr, attr,
        filter->ndat->u.rexpr_filter.filter_def->block_stmt_list.attribute_list,
        list) {
        if (0 == strcmp(attr->nstmt.name, "@item")) {
            item_type = attr->expr;
        } else if (0 == strcmp(attr->nstmt.name, "@length")) {
            item_count = attr->expr;
        }
    }
    if (NULL != item_count) {
        compile_expr(item_count, ctx, FALSE);
    }
    array = new_safe(struct filter_instance_array);
    dpath_node_reset(&array->item_type);
    array->item_type.item = item_type;
    array->item_count = item_count;
    return (struct filter_instance *)array;
}

void
filter_class_declare_array(void)
{
    int ret;

    ret = filter_class_declare("array",
                               EXPR_VALUE_TYPE_UNSET,
                               array_filter_instance_build,
                               2,
                               "@item", REF_ITEM_ITEM,
                               EXPR_VALUE_TYPE_UNSET, 0,
                               "@length", REF_ITEM_LENGTH,
                               EXPR_VALUE_TYPE_INTEGER, 0);
    assert(0 == ret);
}
