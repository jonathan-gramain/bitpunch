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

#include "core/debug.h"
#include "core/expr_internal.h"
// FIXME temporary include until tracker_goto_field_internal() and
// family are generalized to scopes
#include "filters/composite.h"
#include "core/scope.h"


/*
 * statements API
 */


static const struct statement *
scope_iter_statements_advance_internal(
    struct statement_iterator *it,
    const struct statement *stmt)
{
    const struct statement *next_stmt;

    next_stmt = stmt;
    do {
        if ((it->it_flags & STATEMENT_ITERATOR_FLAG_REVERSE)) {
            next_stmt = TAILQ_PREV(next_stmt, statement_list, list);
        } else {
            next_stmt = TAILQ_NEXT(next_stmt, list);
        }
    } while (NULL != next_stmt
             && NULL != it->identifier
             && (NULL == ((struct named_statement *)next_stmt)->name
                 || 0 != strcmp(it->identifier,
                                ((struct named_statement *)next_stmt)->name)));
    return next_stmt;
}

static const struct statement *
scope_iter_statements_find_first_internal(
    struct statement_iterator *it,
    const struct statement *stmt)
{
    if (NULL == stmt
        || NULL == it->identifier
        || (NULL != ((struct named_statement *)stmt)->name
            && 0 == strcmp(it->identifier,
                           ((struct named_statement *)stmt)->name))) {
        return stmt;
    }
    return scope_iter_statements_advance_internal(it, stmt);
}

static void
scope_iter_start_list_internal(struct statement_iterator *it)
{
    if (0 != (it->stmt_remaining & STATEMENT_TYPE_NAMED_EXPR)) {
        it->next_stmt = scope_iter_statements_find_first_internal(
            it, TAILQ_FIRST(it->stmt_lists->named_expr_list));
        it->stmt_remaining &= ~STATEMENT_TYPE_NAMED_EXPR;
        return ;
    }
    if (0 != (it->stmt_remaining & STATEMENT_TYPE_FIELD)) {
        it->next_stmt = scope_iter_statements_find_first_internal(
            it, TAILQ_FIRST(it->stmt_lists->field_list));
        it->stmt_remaining &= ~STATEMENT_TYPE_FIELD;
        return ;
    }
    if (0 != (it->stmt_remaining & STATEMENT_TYPE_ATTRIBUTE)) {
        it->next_stmt = scope_iter_statements_find_first_internal(
            it, TAILQ_FIRST(it->stmt_lists->attribute_list));
        it->stmt_remaining &= ~STATEMENT_TYPE_ATTRIBUTE;
        return ;
    }
}

static void
scope_riter_start_list_internal(struct statement_iterator *it)
{
    if (0 != (it->stmt_remaining & STATEMENT_TYPE_NAMED_EXPR)) {
        it->next_stmt = scope_iter_statements_find_first_internal(
            it, TAILQ_LAST(it->stmt_lists->named_expr_list, statement_list));
        it->stmt_remaining &= ~STATEMENT_TYPE_NAMED_EXPR;
        return ;
    }
    if (0 != (it->stmt_remaining & STATEMENT_TYPE_FIELD)) {
        it->next_stmt = scope_iter_statements_find_first_internal(
            it, TAILQ_LAST(it->stmt_lists->field_list, statement_list));
        it->stmt_remaining &= ~STATEMENT_TYPE_FIELD;
        return ;
    }
    if (0 != (it->stmt_remaining & STATEMENT_TYPE_ATTRIBUTE)) {
        it->next_stmt = scope_iter_statements_find_first_internal(
            it, TAILQ_LAST(it->stmt_lists->attribute_list, statement_list));
        it->stmt_remaining &= ~STATEMENT_TYPE_ATTRIBUTE;
        return ;
    }
}

static enum statement_type
scope_iter_get_current_statement_type(struct statement_iterator *it)
{
    enum statement_type stmt_done_or_in_progress;

    // lookup in reverse priority order, since the higher priority
    // ones are already done so not current

    stmt_done_or_in_progress = it->stmt_mask & ~it->stmt_remaining;
    if (0 != (stmt_done_or_in_progress & STATEMENT_TYPE_ATTRIBUTE)) {
        return STATEMENT_TYPE_ATTRIBUTE;
    }
    if (0 != (stmt_done_or_in_progress & STATEMENT_TYPE_FIELD)) {
        return STATEMENT_TYPE_FIELD;
    }
    if (0 != (stmt_done_or_in_progress & STATEMENT_TYPE_NAMED_EXPR)) {
        return STATEMENT_TYPE_NAMED_EXPR;
    }
    return 0u;
}

struct statement_iterator
scope_iter_statements(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask, const char *identifier)
{
    struct statement_iterator it;

    memset(&it, 0, sizeof (it));
    it.identifier = identifier;
    it.stmt_mask = stmt_mask;
    if (NULL != scope_def) {
        it.scope = scope;
        it.stmt_lists = &scope_def->block_stmt_list;
        it.stmt_remaining = stmt_mask;
        scope_iter_start_list_internal(&it);
    }
    return it;
}

struct statement_iterator
scope_iter_statements_from(
    struct scope_def *scope_def, struct box *scope,
    const struct statement *stmt, const char *identifier)
{
    struct statement_iterator it;

    memset(&it, 0, sizeof (it));
    it.identifier = identifier;
    it.scope = scope;
    it.next_stmt = scope_iter_statements_advance_internal(&it, stmt);
    return it;
}

struct statement_iterator
scope_riter_statements(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask, const char *identifier)
{
    struct statement_iterator it;

    memset(&it, 0, sizeof (it));
    it.identifier = identifier;
    it.it_flags = STATEMENT_ITERATOR_FLAG_REVERSE;
    it.stmt_mask = stmt_mask;
    if (NULL != scope_def) {
        it.scope = scope;
        it.stmt_lists = &scope_def->block_stmt_list;
        it.stmt_remaining = stmt_mask;
        scope_riter_start_list_internal(&it);
    }
    return it;
}

struct statement_iterator
scope_riter_statements_from(
    struct scope_def *scope_def, struct box *scope,
    const struct statement *stmt, const char *identifier)
{
    struct statement_iterator it;

    memset(&it, 0, sizeof (it));
    it.identifier = identifier;
    it.scope = scope;
    it.it_flags = STATEMENT_ITERATOR_FLAG_REVERSE;
    it.next_stmt = scope_iter_statements_advance_internal(&it, stmt);
    return it;
}

bitpunch_status_t
scope_iter_statements_next_internal(
    struct statement_iterator *it,
    enum statement_type *stmt_typep, const struct statement **stmtp,
    struct browse_state *bst)
{
    const struct statement *stmt;

    stmt = it->next_stmt;
    while (NULL != stmt) {
        int cond_eval;
        bitpunch_status_t bt_ret;

        bt_ret = evaluate_conditional_internal(stmt->cond, it->scope,
                                               &cond_eval, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_error_add_box_context(it->scope, bst,
                                          "when evaluating condition");
            return bt_ret;
        }
        if (cond_eval) {
            it->next_stmt = scope_iter_statements_advance_internal(it, stmt);
            if (NULL != stmt_typep) {
                *stmt_typep = scope_iter_get_current_statement_type(it);
            }
            if (NULL != stmtp) {
                *stmtp = stmt;
            }
            return BITPUNCH_OK;
        }
        // condition is false: go on with next statement
        stmt = scope_iter_statements_advance_internal(it, stmt);
    }
    if (0 != it->stmt_remaining) {
        if (0 != (it->it_flags & STATEMENT_ITERATOR_FLAG_REVERSE)) {
            scope_riter_start_list_internal(it);
        } else {
            scope_iter_start_list_internal(it);
        }
        return scope_iter_statements_next_internal(it, stmt_typep, stmtp, bst);
    }
    return BITPUNCH_NO_ITEM;
}

static bitpunch_status_t
scope_lookup_statement_recur(
    struct scope_def *scope_def, struct box *scope,
    const struct block_stmt_list *stmt_lists,
    enum statement_type stmt_mask,
    const char *identifier,
    enum statement_type *stmt_typep,
    const struct named_statement **stmtp,
    struct box **scopep,
    struct browse_state *bst);

static bitpunch_status_t
scope_lookup_statement_in_anonymous_field_recur(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask,
    const char *identifier,
    const struct named_statement *stmt,
    enum statement_type *stmt_typep,
    const struct named_statement **stmtp,
    struct box **scopep,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int cond_eval;
    const struct field *field;
    struct ast_node_hdl *as_type;
    struct box *anon_scope;
    struct tracker *tk;

    // optimization: check if the anonymous struct or its anonymous
    // children (recursively) contain at least one field with the
    // requested name, to avoid creating a filtered box when it's
    // certain there will be no such named statement

    // we may optimize this further in the future (e.g. with a hash
    // table for identifiers for fast lookup)

    field = (const struct field *)stmt;
    as_type = ast_node_get_as_type(field->filter);
    assert(ast_node_is_rexpr_filter(as_type));
    if (!identifier_is_visible_in_block_stmt_lists(
            STATEMENT_TYPE_NAMED_EXPR | STATEMENT_TYPE_FIELD,
            identifier,
            &filter_get_scope_def(as_type)->block_stmt_list)) {
        return BITPUNCH_NO_ITEM;
    }

    bt_ret = evaluate_conditional_internal(stmt->stmt.cond, scope,
                                           &cond_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(scope, bst,
                                      "when evaluating condition");
        return bt_ret;
    }
    if (!cond_eval) {
        return BITPUNCH_NO_ITEM;
    }
    bt_ret = track_box_contents_internal(scope, &tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = tracker_goto_field_internal(
        tk, field, TRUE, bst);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_get_filtered_item_box_internal(tk, &anon_scope, bst);
    }
    tracker_delete(tk);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = scope_lookup_statement_recur(
            filter_get_scope_def(as_type), anon_scope,
            &filter_get_scope_def(as_type)->block_stmt_list,
            stmt_mask, identifier, stmt_typep, stmtp, scopep, bst);
    }
    box_delete(anon_scope);
    return bt_ret;
}

static bitpunch_status_t
scope_get_first_statement_internal(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct statement **stmtp,
    struct browse_state *bst)
{
    struct statement_iterator it;

    it = scope_iter_statements(scope_def, scope, stmt_mask, identifier);
    return scope_iter_statements_next_internal(&it, stmt_typep, stmtp, bst);
}

static bitpunch_status_t
scope_lookup_statement_recur(
    struct scope_def *scope_def, struct box *scope,
    const struct block_stmt_list *stmt_lists,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    struct browse_state *bst)
{
    struct statement *stmt;
    struct named_statement *nstmt;
    bitpunch_status_t bt_ret;

    bt_ret = scope_get_first_statement_internal(
        scope_def, scope, stmt_mask, identifier,
        stmt_typep, (const struct statement **)stmtp, bst);
    if (BITPUNCH_OK == bt_ret) {
        if (NULL != scopep) {
            *scopep = scope;
            box_acquire(scope);
        }
        return BITPUNCH_OK;
    }
    if (BITPUNCH_NO_ITEM != bt_ret) {
        return bt_ret;
    }
    // do not recurse anonymous fields to find attributes
    if (identifier[0] != '@') {
        // recurse in anonymous struct/union fields
        TAILQ_FOREACH(stmt, stmt_lists->field_list, list) {
            nstmt = (struct named_statement *)stmt;
            if (NULL == nstmt->name
                && !(nstmt->stmt.stmt_flags & FIELD_FLAG_HIDDEN)) {
                bt_ret = scope_lookup_statement_in_anonymous_field_recur(
                    scope_def, scope, stmt_mask, identifier, nstmt,
                    stmt_typep, stmtp, scopep, bst);
                if (BITPUNCH_NO_ITEM != bt_ret) {
                    return bt_ret;
                }
            }
        }
    }
    return BITPUNCH_NO_ITEM;
}

bitpunch_status_t
scope_lookup_statement_internal(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    struct browse_state *bst)
{
    if (NULL == scope_def) {
        return BITPUNCH_NO_ITEM;
    }
    return scope_lookup_statement_recur(
        scope_def, scope, &scope_def->block_stmt_list, stmt_mask, identifier,
        stmt_typep, stmtp, scopep, bst);
}

bitpunch_status_t
scope_get_n_statements_internal(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    int64_t *stmt_countp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct statement_iterator it;
    int64_t stmt_count;

    it = scope_iter_statements(scope_def, scope, stmt_mask, identifier);
    stmt_count = -1;
    do {
        ++stmt_count;
        bt_ret = scope_iter_statements_next_internal(&it, NULL, NULL, bst);
    } while (BITPUNCH_OK == bt_ret);
    if (BITPUNCH_NO_ITEM != bt_ret) {
        return bt_ret;
    }
    if (NULL != stmt_countp) {
        *stmt_countp = stmt_count;
    }
    return BITPUNCH_OK;
}


bitpunch_status_t
scope_evaluate_identifier_internal(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    enum statement_type stmt_type;
    const struct named_statement *named_stmt;
    struct box *stmt_scope;

    bt_ret = scope_lookup_statement_internal(
        scope_def, scope, stmt_mask, identifier,
        &stmt_type, &named_stmt, &stmt_scope, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = evaluate_scoped_statement_internal(
        stmt_scope, stmt_type, named_stmt, valuep, dpathp, bst);
    if (BITPUNCH_OK == bt_ret) {
        if (NULL != stmt_typep) {
            *stmt_typep = stmt_type;
        }
        if (NULL != stmtp) {
            *stmtp = named_stmt;
        }
        if (NULL != scopep) {
            *scopep = stmt_scope;
            stmt_scope = NULL;
        }
    }
    box_delete(stmt_scope);
    return bt_ret;
}

bitpunch_status_t
scope_evaluate_attribute_internal(
    struct scope_def *scope_def, struct box *scope,
    const char *attr_name,
    const struct named_expr **attrp,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    return scope_evaluate_identifier_internal(
        scope_def, scope, STATEMENT_TYPE_ATTRIBUTE, attr_name,
        NULL, (const struct named_statement **)attrp, NULL,
        valuep, dpathp, bst);
}

bitpunch_status_t
scope_evaluate_identifier(
    struct scope_def *scope_def, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        scope_evaluate_identifier_internal(
            scope_def, scope, stmt_mask, identifier,
            NULL, NULL, NULL, valuep, dpathp, &bst),
        &bst, errp);
}

bitpunch_status_t
scope_iter_statements_next(
    struct statement_iterator *it,
    enum statement_type *stmt_typep, const struct statement **stmtp,
    struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        scope_iter_statements_next_internal(it, stmt_typep, stmtp, &bst),
        &bst, errp);
}

/**
 * @brief attach a native value as an attribute to the scope
 *
 * @note target usage is mostly unit tests for filters
 */
void
scope_attach_native_attribute(
    struct scope_def *scope_def,
    const char *attr_name, expr_value_t value)
{
    struct statement_list *attribute_list;
    struct named_expr *attr;

    attribute_list = scope_def->block_stmt_list.attribute_list;
    attr = new_safe(struct named_expr);
    attr->nstmt.name = strdup_safe(attr_name);
    attr->expr = ast_node_new_rexpr_native(value);
    TAILQ_INSERT_TAIL(attribute_list, (struct statement *)attr, list);
}

struct ast_node_hdl *
scope_get_first_declared_attribute(
    const struct scope_def *scope_def,
    const char *attr_name)
{
    struct named_expr *attr_stmt;

    STATEMENT_FOREACH(
        named_expr, attr_stmt,
        scope_def->block_stmt_list.attribute_list, list) {
        if (0 == strcmp(attr_stmt->nstmt.name, attr_name)) {
            return attr_stmt->expr;
        }
    }
    return NULL;
}
