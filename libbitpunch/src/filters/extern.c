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
#include <string.h>
#include <assert.h>

#include "core/filter.h"

static enum expr_value_type
expr_value_type_from_spec(struct ast_node_hdl *spec)
{
    expr_value_t attr_value_type;

    if (spec->ndat->type != AST_NODE_TYPE_REXPR_NATIVE
        || spec->ndat->u.rexpr_native.value.type != EXPR_VALUE_TYPE_STRING) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &spec->loc,
                       "attribute spec should be a native string type");
        return EXPR_VALUE_TYPE_UNSET;
    }
    attr_value_type = spec->ndat->u.rexpr_native.value;
    if (0 == expr_value_cmp_string(
            attr_value_type, expr_value_as_string("bytes"))) {
        return EXPR_VALUE_TYPE_BYTES;
    }
    if (0 == expr_value_cmp_string(
            attr_value_type, expr_value_as_string("integer"))) {
        return EXPR_VALUE_TYPE_INTEGER;
    }
    if (0 == expr_value_cmp_string(
            attr_value_type, expr_value_as_string("string"))) {
        return EXPR_VALUE_TYPE_STRING;
    }
    semantic_error(SEMANTIC_LOGLEVEL_ERROR, &spec->loc,
                   "invalid attribute type '%.*s'",
                   (int)attr_value_type.string.len, attr_value_type.string.str);
    return EXPR_VALUE_TYPE_UNSET;
}

static struct filter_instance *
extern_filter_instance_build(struct ast_node_hdl *filter)
{
    struct filter_instance *f_instance;

    // TODO add instance to list of instances backed by external
    // filters, to populate when the external filter is registered
    f_instance = new_safe(struct filter_instance);
    return f_instance;
}

static struct filter_class *
extern_filter_class_generate(struct ast_node_hdl *filter)
{
    struct filter_class *filter_cls;
    const struct block_stmt_list *stmt_lists;
    struct named_expr *attr;
    struct filter_attr_def *attr_def;
    enum expr_value_type value_type_mask;
    enum expr_value_type attr_value_type_mask;

    filter_cls = filter_class_new(NULL);
    if (NULL == filter_cls) {
        return NULL;
    }
    filter_cls->flags = 0u;
    filter_cls->n_attrs = 0;
    filter_cls->user_arg = (void *)filter;
    filter_cls->filter_instance_build_func = extern_filter_instance_build;

    value_type_mask = EXPR_VALUE_TYPE_UNSET;
    STAILQ_INIT(&filter_cls->attr_list);
    stmt_lists = &filter->ndat->u.scope_def.block_stmt_list;
    STATEMENT_FOREACH(named_expr, attr, stmt_lists->attribute_list, list) {
        attr_value_type_mask = expr_value_type_from_spec(attr->expr);
        if (EXPR_VALUE_TYPE_UNSET == attr_value_type_mask) {
            return NULL;
        }
        if (0 == strcmp(attr->nstmt.name, "@out")) {
            value_type_mask = attr_value_type_mask;
        } else {
            attr_def = filter_attr_def_new();
            attr_def->name = attr->nstmt.name;
            attr_def->value_type_mask = attr_value_type_mask;
            attr_def->flags = 0u;
            STAILQ_INSERT_TAIL(&filter_cls->attr_list, attr_def, list);
            ++filter_cls->n_attrs;
        }
    }
    if (EXPR_VALUE_TYPE_UNSET == value_type_mask) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &filter->loc,
                       "missing attribute spec '@out'");
        return NULL;
    }
    filter_cls->value_type_mask = value_type_mask;
    return filter_cls;
}

void
builtin_filter_declare_extern(void)
{
    int ret;

    ret = builtin_filter_declare_generator(
        "extern",
        extern_filter_class_generate,
        1,
        "@*", EXPR_VALUE_TYPE_STRING, 0u);
    assert(0 == ret);
}
