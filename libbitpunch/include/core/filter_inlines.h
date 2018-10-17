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
// It shall only be included from filter.h

static inline struct scope_def *
filter_get_scope_def(struct ast_node_hdl *filter)
{
  return (struct scope_def *)filter->ndat->u.rexpr_filter.filter_def;
}

static inline const struct scope_def *
filter_get_const_scope_def(const struct ast_node_hdl *filter)
{
  return (const struct scope_def *)filter->ndat->u.rexpr_filter.filter_def;
}

static inline struct statement_iterator
filter_iter_statements(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier)
{
  return scope_iter_statements(
      filter_get_scope_def(filter), scope, stmt_mask, identifier);
}

static inline struct statement_iterator
filter_iter_statements_from(
    struct ast_node_hdl *filter, struct box *scope,
    const struct statement *stmt, const char *identifier)
{
  return scope_iter_statements_from(
      filter_get_scope_def(filter), scope, stmt, identifier);
}

static inline struct statement_iterator
filter_riter_statements(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier)
{
  return scope_riter_statements(
      filter_get_scope_def(filter), scope, stmt_mask, identifier);
}

static inline struct statement_iterator
filter_riter_statements_from(
    struct ast_node_hdl *filter, struct box *scope,
    const struct statement *stmt, const char *identifier)
{
  return scope_riter_statements_from(
      filter_get_scope_def(filter), scope, stmt, identifier);
}

static inline bitpunch_status_t
filter_lookup_statement_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    struct browse_state *bst)
{
  return scope_lookup_statement_internal(
      filter_get_scope_def(filter), scope, stmt_mask, identifier,
      stmt_typep, stmtp, scopep, bst);
}

static inline bitpunch_status_t
filter_get_n_statements_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    int64_t *stmt_countp,
    struct browse_state *bst)
{
  return scope_get_n_statements_internal(
      filter_get_scope_def(filter), scope, stmt_mask, identifier,
      stmt_countp, bst);
}

static inline bitpunch_status_t
filter_evaluate_identifier_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
  return scope_evaluate_identifier_internal(
      filter_get_scope_def(filter), scope, stmt_mask, identifier,
      stmt_typep, stmtp, scopep, valuep, dpathp, bst);
}

static inline bitpunch_status_t
filter_evaluate_attribute_internal(
    struct ast_node_hdl *filter, struct box *scope,
    const char *attr_name,
    const struct named_expr **attrp,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
  return scope_evaluate_attribute_internal(
      filter_get_scope_def(filter), scope, attr_name,
      attrp, valuep, dpathp, bst);
}

static inline bitpunch_status_t
filter_evaluate_identifier(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct tracker_error **errp)
{
  return scope_evaluate_identifier(
      filter_get_scope_def(filter), scope, stmt_mask, identifier,
      valuep, dpathp, errp);
}

static inline void
filter_attach_native_attribute(
    struct ast_node_hdl *filter,
    const char *attr_name, expr_value_t value)
{
  return scope_attach_native_attribute(
      filter_get_scope_def(filter), attr_name, value);
}

static inline struct ast_node_hdl *
filter_get_first_declared_attribute(
    const struct ast_node_hdl *filter,
    const char *attr_name)
{
  return scope_get_first_declared_attribute(
      filter_get_const_scope_def(filter), attr_name);
}
