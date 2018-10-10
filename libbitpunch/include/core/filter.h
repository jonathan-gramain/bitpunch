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
#include "core/browse_internal.h"

#include PATH_TO_PARSER_TAB_H

enum filter_attr_flags {
    FILTER_ATTR_FLAG_MANDATORY = 1,
};

struct filter_attr_def {
    STAILQ_ENTRY(filter_attr_def) list;
    const char *name;
    enum expr_value_type value_type_mask;
    enum filter_attr_flags flags;
};

struct filter_instance {
    struct item_node item; /* inherits */
    struct item_backend b_item;
    struct box_backend b_box;
    struct tracker_backend b_tk;
    filter_read_func_t read_func;
};

typedef struct filter_instance *
(*filter_instance_build_func_t)(struct ast_node_hdl *filter);

typedef int
(*filter_instance_compile_func_t)(
    struct ast_node_hdl *filter,
    struct filter_instance *f_instance,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx);

struct filter_class {
    const char *name;
    enum expr_value_type value_type_mask;
    filter_instance_build_func_t filter_instance_build_func;
    filter_instance_compile_func_t filter_instance_compile_func;
    int n_attrs;
    STAILQ_HEAD(filter_attr_list, filter_attr_def) attr_list;
};

int
filter_class_declare(
    const char *name,
    enum expr_value_type value_type_mask,
    filter_instance_build_func_t filter_instance_build_func,
    filter_instance_compile_func_t filter_instance_compile_func,
    int n_attrs,
    ... /* attrs: (name, type, flags) tuples */);

struct filter_class *
filter_class_lookup(const char *name);

void
filter_class_declare_std(void);

const struct filter_attr_def *
filter_class_get_attr(const struct filter_class *filter_cls,
                         const char *attr_name);

struct filter_def *
filter_def_create_empty(const char *filter_type);

int
filter_instance_build(struct ast_node_hdl *node,
                      const struct filter_class *filter_cls,
                      struct filter_def *filter_def);

bitpunch_status_t
filter_instance_read_value(struct ast_node_hdl *filter,
                           struct box *scope,
                           int64_t item_offset,
                           int64_t item_size,
                           expr_value_t *valuep,
                           struct browse_state *bst);

/* generic statement API */

enum statement_iterator_flag {
    STATEMENT_ITERATOR_FLAG_REVERSE = (1<<0),
};
struct statement_iterator {
    /** attribute name to iterate, or NULL for all statements */
    const char *identifier;
    struct box *scope;
    enum statement_type stmt_mask;
    enum statement_type stmt_remaining;
    enum statement_iterator_flag it_flags;
    const struct block_stmt_list *stmt_lists;
    const struct statement *next_stmt;
};

typedef struct statement_iterator tstatement_iterator;

struct statement_iterator
filter_iter_statements(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier);

struct statement_iterator
filter_iter_statements_from(
    struct ast_node_hdl *filter, struct box *scope,
    const struct statement *stmt, const char *identifier);

struct statement_iterator
filter_riter_statements(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier);

struct statement_iterator
filter_riter_statements_from(
    struct ast_node_hdl *filter, struct box *scope,
    const struct statement *stmt, const char *identifier);

bitpunch_status_t
filter_iter_statements_next_internal(
    struct statement_iterator *it,
    enum statement_type *stmt_typep, const struct statement **stmtp,
    struct browse_state *bst);

bitpunch_status_t
filter_lookup_statement_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    struct browse_state *bst);

bitpunch_status_t
filter_get_n_statements_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    int64_t *stmt_countp,
    struct browse_state *bst);

bitpunch_status_t
filter_evaluate_identifier_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst);

bitpunch_status_t
filter_evaluate_attribute_internal(
    struct ast_node_hdl *filter, struct box *scope,
    const char *attr_name,
    const struct named_expr **attrp,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst);

bitpunch_status_t
filter_evaluate_identifier(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct tracker_error **errp);

bitpunch_status_t
filter_iter_statements_next(
    struct statement_iterator *it,
    enum statement_type *stmt_typep, const struct statement **stmtp,
    struct tracker_error **errp);

void
filter_attach_native_attribute(
    struct ast_node_hdl *filter,
    const char *attr_name, expr_value_t value);


void
browse_setup_backends__filter__filter(struct ast_node_hdl *filter);

#endif
