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

#ifndef __FILTER_H__
#define __FILTER_H__

#include <stddef.h>

#include "utils/queue.h"
#include "core/parser.h"
#include "core/expr.h"

#include PATH_TO_PARSER_TAB_H

enum filter_attr_flags {
    FILTER_ATTR_FLAG_MANDATORY = 1,
};

struct filter_attr_def {
    STAILQ_ENTRY(filter_attr_def) list;
    const char *name;
    int ref_idx;
    enum expr_value_type value_type_mask;
    enum filter_attr_flags flags;
};

typedef int
(*filter_instance_build_func_t)(
    struct ast_node_hdl *filter,
    const struct statement_list *attribute_list,
    struct compile_ctx *ctx);

struct filter_class {
    const char *name;
    enum expr_value_type value_type_mask;
    filter_instance_build_func_t filter_instance_build_func;
    int n_attrs;
    int max_attr_ref;
    STAILQ_HEAD(filter_attr_list, filter_attr_def) attr_list;
};

int
filter_class_declare(const char *name,
                    enum expr_value_type value_type_mask,
                    filter_instance_build_func_t filter_instance_build_func,
                    int n_attrs,
                    ... /* attrs: (name, ref_idx, type, flags) tuples */);

struct filter_class *
filter_class_lookup(const char *name);

void
filter_class_declare_std(void);

const struct filter_attr_def *
filter_class_get_attr(const struct filter_class *filter_cls,
                         const char *attr_name);
int
filter_instance_build(struct ast_node_hdl *node,
                        const struct filter_class *filter_cls,
                        struct statement_list *attribute_list);

int
filter_build_attrs(struct ast_node_hdl *node,
                        const struct filter_class *filter_cls,
                        struct statement_list *attribute_list);

bitpunch_status_t
filter_instance_evaluate_attrs(struct ast_node_hdl *expr,
                                 struct box *scope,
                                 int **attr_is_specifiedp,
                                 expr_value_t **attr_valuep,
                                 struct browse_state *bst);

void
filter_instance_destroy_attr_values(struct ast_node_hdl *expr,
                                      int *attr_is_specified,
                                      expr_value_t *attr_value);

bitpunch_status_t
filter_instance_read_value(struct ast_node_hdl *filter,
                             struct box *scope,
                             const char *item_data,
                             int64_t item_size,
                             expr_value_t *valuep,
                             struct browse_state *bst);

#endif
