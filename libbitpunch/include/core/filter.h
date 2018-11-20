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
#include "core/browse_internal.h"
#include "core/scope.h"

#include PATH_TO_PARSER_TAB_H

enum filter_attr_flag {
    FILTER_ATTR_MANDATORY = (1u<<0),
};

enum filter_class_flag {
    /** set when the filter maps to a list type */
    FILTER_CLASS_MAPS_LIST = (1u<<0),
};

struct filter_attr_def {
    STAILQ_ENTRY(filter_attr_def) list;
    const char *name;
    enum expr_value_type value_type_mask;
    enum filter_attr_flag flags;
};

typedef bitpunch_status_t (*filter_get_data_source_func_t)(
    struct ast_node_hdl *filter,
    struct box *scope,
    struct bitpunch_data_source **ds_outp,
    struct browse_state *bst);

typedef bitpunch_status_t (*filter_read_func_t)(
    struct ast_node_hdl *filter,
    struct box *scope,
    expr_value_t *read_value,
    const char *data, size_t span_size,
    struct browse_state *bst);

struct filter_instance {
    struct item_node item; /* inherits */
    struct item_backend b_item;
    struct box_backend b_box;
    struct tracker_backend b_tk;
    filter_get_data_source_func_t get_data_source_func;
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
    enum filter_class_flag flags;
    int n_attrs;
    STAILQ_HEAD(filter_attr_list, filter_attr_def) attr_list;
};

int
filter_class_declare(
    const char *name,
    enum expr_value_type value_type_mask,
    filter_instance_build_func_t filter_instance_build_func,
    filter_instance_compile_func_t filter_instance_compile_func,
    enum filter_class_flag flags,
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
int
filter_instance_build_shared(struct ast_node_hdl *node,
                             const char *filter_name);

bitpunch_status_t
filter_instance_read_value(struct ast_node_hdl *filter,
                           struct box *scope,
                           int64_t item_offset,
                           int64_t item_size,
                           expr_value_t *valuep,
                           struct browse_state *bst);
bitpunch_status_t
filter_instance_get_data_source(
    struct ast_node_hdl *filter, struct box *scope,
    struct bitpunch_data_source **ds_outp, struct browse_state *bst);


bitpunch_status_t
filter_read_value__bytes(struct ast_node_hdl *item_filter,
                         struct box *scope,
                         int64_t item_offset,
                         int64_t item_size,
                         expr_value_t *valuep,
                         struct browse_state *bst);
bitpunch_status_t
filter_read_value__filter(struct ast_node_hdl *filter,
                          struct box *scope,
                          int64_t item_offset,
                          int64_t item_size,
                          expr_value_t *valuep,
                          struct browse_state *bst);

void
compile_node_backends__filter_generic(struct ast_node_hdl *filter);
void
compile_node_backends__filter__filter(struct ast_node_hdl *filter);
void
compile_node_backends__tracker__filter(struct ast_node_hdl *item);


/*
 * scope API
 */

static inline struct scope_def *
filter_get_scope_def(struct ast_node_hdl *filter);

static inline const struct scope_def *
filter_get_const_scope_def(const struct ast_node_hdl *filter);

static inline struct statement_iterator
filter_iter_statements(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier);

static inline struct statement_iterator
filter_iter_statements_from(
    struct ast_node_hdl *filter, struct box *scope,
    const struct statement *stmt, const char *identifier);

static inline struct statement_iterator
filter_riter_statements(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier);

static inline struct statement_iterator
filter_riter_statements_from(
    struct ast_node_hdl *filter, struct box *scope,
    const struct statement *stmt, const char *identifier);

static inline bitpunch_status_t
filter_lookup_statement_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    struct browse_state *bst);

static inline bitpunch_status_t
filter_get_n_statements_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    int64_t *stmt_countp,
    struct browse_state *bst);

static inline bitpunch_status_t
filter_evaluate_identifier_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst);

static inline bitpunch_status_t
filter_evaluate_attribute_internal(
    struct ast_node_hdl *filter, struct box *scope,
    const char *attr_name,
    const struct named_expr **attrp,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst);

static inline bitpunch_status_t
filter_evaluate_identifier(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct tracker_error **errp);

static inline void
filter_attach_native_attribute(
    struct ast_node_hdl *filter,
    const char *attr_name, expr_value_t value);

static inline struct ast_node_hdl *
filter_get_first_declared_named_expr(
    const struct ast_node_hdl *filter,
    const char *name);

static inline struct ast_node_hdl *
filter_get_first_declared_attribute(
    const struct ast_node_hdl *filter,
    const char *attr_name);

#include "core/filter_inlines.h"

#endif
