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

#ifndef __SCOPE_H__
#define __SCOPE_H__


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
scope_iter_statements(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask, const char *identifier);

struct statement_iterator
scope_iter_statements_from(
    struct scope_def *scope_def, struct box *scope,
    const struct statement *stmt, const char *identifier);

struct statement_iterator
scope_riter_statements(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask, const char *identifier);

struct statement_iterator
scope_riter_statements_from(
    struct scope_def *scope_def, struct box *scope,
    const struct statement *stmt, const char *identifier);

bitpunch_status_t
scope_iter_statements_next_internal(
    struct statement_iterator *it,
    enum statement_type *stmt_typep, const struct statement **stmtp,
    struct browse_state *bst);

bitpunch_status_t
scope_lookup_statement_internal(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    struct browse_state *bst);

bitpunch_status_t
scope_get_n_statements_internal(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    int64_t *stmt_countp,
    struct browse_state *bst);

bitpunch_status_t
scope_evaluate_identifier_internal(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst);

bitpunch_status_t
scope_evaluate_attribute_internal(
    struct scope_def *scope_def, struct box *scope,
    const char *attr_name,
    const struct named_expr **attrp,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst);

bitpunch_status_t
scope_evaluate_identifier(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct tracker_error **errp);

bitpunch_status_t
scope_iter_statements_next(
    struct statement_iterator *it,
    enum statement_type *stmt_typep, const struct statement **stmtp,
    struct tracker_error **errp);

void
scope_attach_native_attribute(
    struct scope_def *scope_def,
    const char *attr_name, expr_value_t value);

struct ast_node_hdl *
scope_get_first_declared_named_expr(
    const struct scope_def *scope_def,
    const char *name);

struct ast_node_hdl *
scope_get_first_declared_attribute(
    const struct scope_def *scope_def,
    const char *attr_name);


/* browse backend */

bitpunch_status_t
box_get_n_items__scope(struct box *box, int64_t *item_countp,
                       struct browse_state *bst);
bitpunch_status_t
tracker_get_item_key__scope(struct tracker *tk,
                            expr_value_t *keyp,
                            int *nth_twinp,
                            struct browse_state *bst);
bitpunch_status_t
tracker_goto_first_item__scope(
    struct tracker *tk, struct browse_state *bst);
bitpunch_status_t
tracker_goto_next_item__scope(
    struct tracker *tk, struct browse_state *bst);
bitpunch_status_t
tracker_goto_nth_item__scope(
    struct tracker *tk, int64_t index, struct browse_state *bst);
bitpunch_status_t
tracker_goto_next_item_with_key__scope(
    struct tracker *tk, expr_value_t item_key, struct browse_state *bst);
bitpunch_status_t
tracker_goto_nth_item_with_key__scope(
    struct tracker *tk, expr_value_t item_key, int nth_twin,
    struct browse_state *bst);
bitpunch_status_t
tracker_goto_named_item__scope(struct tracker *tk, const char *name,
                               struct browse_state *bst);
bitpunch_status_t
tracker_goto_next_key_match__scope(struct tracker *tk,
                                   expr_value_t index,
                                   struct track_path search_boundary,
                                   struct browse_state *bst);
bitpunch_status_t
tracker_goto_end_path__scope(struct tracker *tk,
                             struct browse_state *bst);
void
tracker_goto_nil__scope(struct tracker *tk);


#endif
