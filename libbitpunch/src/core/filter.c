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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stddef.h>
#include <assert.h>

#include "core/filter.h"
#include "core/browse_internal.h"
#include "core/expr_internal.h"
#include "filters/composite.h"

#define MAX_FILTER_COUNT           256

struct filter_class filter_class_table[MAX_FILTER_COUNT];
int                 filter_class_count = 0;

#define MAX_FILTER_ATTR_DEF_COUNT 1024

struct filter_attr_def filter_attr_def_table[MAX_FILTER_ATTR_DEF_COUNT];
int                    filter_attr_def_count = 0;

static struct filter_class *
filter_class_new(void)
{
    if (filter_class_count == MAX_FILTER_COUNT) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "Too many filters (max %d)\n",
                         MAX_FILTER_COUNT);
        return NULL;
    }
    return &filter_class_table[filter_class_count++];
}

static struct filter_attr_def *
filter_attr_def_new(void)
{
    if (filter_attr_def_count == MAX_FILTER_ATTR_DEF_COUNT) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "Global filter attribute definition limit reached "
                       "(max %d)\n", MAX_FILTER_ATTR_DEF_COUNT);
        return NULL;
    }
    return &filter_attr_def_table[filter_attr_def_count++];
}

int
filter_class_declare(
    const char *name,
    enum expr_value_type value_type_mask,
    filter_instance_build_func_t filter_instance_build_func,
    filter_instance_compile_func_t filter_instance_compile_func,
    int n_attrs,
    ... /* attrs: (name, type, flags) tuples */)
{
    struct filter_class *filter_cls;
    va_list ap;
    int i;
    struct filter_attr_def *attr_def;

    assert(NULL != filter_instance_build_func);

    filter_cls = filter_class_lookup(name);
    if (NULL != filter_cls) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "duplicate filter '%s'\n", name);
        return -1;
    }
    filter_cls = filter_class_new();
    if (NULL == filter_cls) {
        return -1;
    }
    filter_cls->name = name;
    filter_cls->value_type_mask = value_type_mask;
    filter_cls->filter_instance_build_func = filter_instance_build_func;
    filter_cls->filter_instance_compile_func = filter_instance_compile_func;
    filter_cls->n_attrs = n_attrs;
    STAILQ_INIT(&filter_cls->attr_list);
    va_start(ap, n_attrs);
    for (i = 0; i < n_attrs; ++i) {
        attr_def = filter_attr_def_new();
        if (NULL == attr_def) {
            return -1;
        }
        attr_def->name = va_arg(ap, const char *);
        attr_def->value_type_mask = va_arg(ap, enum ast_node_type);
        attr_def->flags = va_arg(ap, enum filter_attr_flags);
        STAILQ_INSERT_TAIL(&filter_cls->attr_list, attr_def, list);
    }
    va_end(ap);
    return 0;
}

struct filter_class *
filter_class_lookup(const char *name)
{
    int i;

    for (i = 0; i < filter_class_count; ++i) {
        if (0 == strcmp(filter_class_table[i].name, name)) {
            return &filter_class_table[i];
        }
    }
    return NULL;
}

void filter_class_declare_byte(void);
void filter_class_declare_struct(void);
void filter_class_declare_union(void);
void filter_class_declare_array(void);
void filter_class_declare_binary_integer(void);
void filter_class_declare_bytes(void);
void filter_class_declare_string(void);
void filter_class_declare_varint(void);
void filter_class_declare_base64(void);
void filter_class_declare_snappy(void);
void filter_class_declare_formatted_integer(void);

void
filter_class_declare_std(void)
{
    filter_class_declare_byte();
    filter_class_declare_struct();
    filter_class_declare_union();
    filter_class_declare_array();
    filter_class_declare_binary_integer();
    filter_class_declare_bytes();
    filter_class_declare_string();
    filter_class_declare_varint();
    filter_class_declare_base64();
    filter_class_declare_snappy();
    filter_class_declare_formatted_integer();
}

struct filter_def *
filter_def_create_empty(const char *filter_type)
{
    struct filter_def *filter_def;

    filter_def = new_safe(struct filter_def);
    filter_def->filter_type = filter_type;
    init_block_stmt_list(&filter_def->scope_block.block_stmt_list);
    return filter_def;
}

int
filter_instance_build(struct ast_node_hdl *node,
                      const struct filter_class *filter_cls,
                      struct filter_def *filter_def)
{
    struct ast_node_data *ndat;
    struct filter_instance *f_instance;

    ndat = new_safe(struct ast_node_data);
    // template flag may be removed when compiling OP_FILTER
    ndat->flags |= ASTFLAG_DATA_TEMPLATE;
    ndat->u.item.min_span_size = SPAN_SIZE_UNDEF;
    if (0 == strcmp(filter_def->filter_type, "struct") ||
        0 == strcmp(filter_def->filter_type, "union")) {
        ndat->type = AST_NODE_TYPE_COMPOSITE;
    } else if (0 == strcmp(filter_def->filter_type, "array")) {
        ndat->type = AST_NODE_TYPE_ARRAY;
    } else if (0 == strcmp(filter_def->filter_type, "byte")) {
        ndat->type = AST_NODE_TYPE_BYTE;
    } else {
        ndat->type = AST_NODE_TYPE_REXPR_FILTER;
    }
    ndat->u.rexpr.value_type_mask = filter_cls->value_type_mask;
    ndat->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_UNSET;
    if (0 != (filter_cls->value_type_mask & (EXPR_VALUE_TYPE_INTEGER |
                                             EXPR_VALUE_TYPE_BOOLEAN))) {
        ndat->u.rexpr.dpath_type_mask |= EXPR_DPATH_TYPE_NONE;
    }
    if (0 != (filter_cls->value_type_mask & (EXPR_VALUE_TYPE_BYTES |
                                             EXPR_VALUE_TYPE_STRING))) {
        ndat->u.rexpr.dpath_type_mask |= EXPR_DPATH_TYPE_CONTAINER;
    }
    ndat->u.rexpr_filter.filter_cls = filter_cls;
    ndat->u.rexpr_filter.filter_def = filter_def;

    node->ndat = ndat;

    f_instance = filter_cls->filter_instance_build_func(node);
    if (NULL == f_instance) {
        return -1;
    }
    ndat->u.rexpr_filter.f_instance = f_instance;
    // FIXME merge filter item with node item
    //ndat->u.item = f_instance->item;
    return 0;
}

int
filter_instance_build_shared(struct ast_node_hdl *node,
                             const char *filter_name)
{
    const struct filter_class *filter_cls;

    filter_cls = filter_class_lookup(filter_name);
    assert(NULL != filter_cls);
    if (-1 == filter_instance_build(node, filter_cls,
                                    filter_def_create_empty(filter_name))) {
        return -1;
    }
    return 0;
}

const struct filter_attr_def *
filter_class_get_attr(const struct filter_class *filter_cls,
                         const char *attr_name)
{
    struct filter_attr_def *attr_def;

    STAILQ_FOREACH(attr_def, &filter_cls->attr_list, list) {
        if (0 == strcmp(attr_def->name, attr_name)) {
            return attr_def;
        }
    }
    return NULL; /* not found */
}

bitpunch_status_t
filter_instance_read_value(struct ast_node_hdl *filter,
                           struct box *scope,
                           int64_t item_offset,
                           int64_t item_size,
                           expr_value_t *valuep,
                           struct browse_state *bst)
{
    struct filter_instance *f_instance;
    bitpunch_status_t bt_ret;
    int64_t span_size;
    const char *item_data;
    expr_value_t value;

    assert(-1 != item_offset);

    value.type = EXPR_VALUE_TYPE_UNSET;
    f_instance = filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_item.compute_item_size) {
        span_size = item_size;
        bt_ret = BITPUNCH_OK;
    } else {
        bt_ret = f_instance->b_item.compute_item_size(
            filter, scope, item_offset, item_offset + item_size,
            &span_size, bst);
    }
    if (BITPUNCH_OK == bt_ret) {
        memset(&value, 0, sizeof(value));
        item_data = scope->ds_in->ds_data + item_offset;
        if (NULL != f_instance->read_func) {
            bt_ret = f_instance->read_func(filter, scope,
                                           &value, item_data, span_size, bst);
        } else {
            value.type = EXPR_VALUE_TYPE_BYTES;
            value.bytes.buf = item_data;
            value.bytes.len = span_size;
            value.bytes.from_box = scope;
            box_acquire(scope);
        }
    }
    if (BITPUNCH_OK == bt_ret && NULL != valuep) {
        *valuep = value;
    } else {
        expr_value_destroy(value);
    }
    return bt_ret;
}


/*
 * statements API
 */


static const struct statement *
filter_iter_statements_advance_internal(
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
filter_iter_statements_find_first_internal(
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
    return filter_iter_statements_advance_internal(it, stmt);
}

static void
filter_iter_start_list_internal(struct statement_iterator *it)
{
    if (0 != (it->stmt_remaining & STATEMENT_TYPE_NAMED_EXPR)) {
        it->next_stmt = filter_iter_statements_find_first_internal(
            it, TAILQ_FIRST(it->stmt_lists->named_expr_list));
        it->stmt_remaining &= ~STATEMENT_TYPE_NAMED_EXPR;
        return ;
    }
    if (0 != (it->stmt_remaining & STATEMENT_TYPE_FIELD)) {
        it->next_stmt = filter_iter_statements_find_first_internal(
            it, TAILQ_FIRST(it->stmt_lists->field_list));
        it->stmt_remaining &= ~STATEMENT_TYPE_FIELD;
        return ;
    }
    if (0 != (it->stmt_remaining & STATEMENT_TYPE_ATTRIBUTE)) {
        it->next_stmt = filter_iter_statements_find_first_internal(
            it, TAILQ_FIRST(it->stmt_lists->attribute_list));
        it->stmt_remaining &= ~STATEMENT_TYPE_ATTRIBUTE;
        return ;
    }
}

static void
filter_riter_start_list_internal(struct statement_iterator *it)
{
    if (0 != (it->stmt_remaining & STATEMENT_TYPE_NAMED_EXPR)) {
        it->next_stmt = filter_iter_statements_find_first_internal(
            it, TAILQ_LAST(it->stmt_lists->named_expr_list, statement_list));
        it->stmt_remaining &= ~STATEMENT_TYPE_NAMED_EXPR;
        return ;
    }
    if (0 != (it->stmt_remaining & STATEMENT_TYPE_FIELD)) {
        it->next_stmt = filter_iter_statements_find_first_internal(
            it, TAILQ_LAST(it->stmt_lists->field_list, statement_list));
        it->stmt_remaining &= ~STATEMENT_TYPE_FIELD;
        return ;
    }
    if (0 != (it->stmt_remaining & STATEMENT_TYPE_ATTRIBUTE)) {
        it->next_stmt = filter_iter_statements_find_first_internal(
            it, TAILQ_LAST(it->stmt_lists->attribute_list, statement_list));
        it->stmt_remaining &= ~STATEMENT_TYPE_ATTRIBUTE;
        return ;
    }
}

static enum statement_type
filter_iter_get_current_statement_type(struct statement_iterator *it)
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
filter_iter_statements(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier)
{
    struct statement_iterator it;
    struct scope_block *scope_block;

    memset(&it, 0, sizeof (it));
    it.identifier = identifier;
    if (AST_NODE_TYPE_SCOPE_BLOCK == filter->ndat->type) {
        scope_block = &filter->ndat->u.scope_block;
    } else {
        assert(ast_node_is_rexpr_filter(filter));
        scope_block = &filter->ndat->u.rexpr_filter.filter_def->scope_block;
    }
    it.stmt_mask = stmt_mask;
    it.scope = scope;
    it.stmt_lists = &scope_block->block_stmt_list;
    it.stmt_remaining = stmt_mask;
    filter_iter_start_list_internal(&it);
    return it;
}

struct statement_iterator
filter_iter_statements_from(
    struct ast_node_hdl *filter, struct box *scope,
    const struct statement *stmt, const char *identifier)
{
    struct statement_iterator it;

    memset(&it, 0, sizeof (it));
    it.identifier = identifier;
    it.scope = scope;
    it.next_stmt = filter_iter_statements_advance_internal(&it, stmt);
    return it;
}

struct statement_iterator
filter_riter_statements(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier)
{
    struct statement_iterator it;
    struct filter_def *filter_def;

    assert(ast_node_is_rexpr_filter(filter));
    memset(&it, 0, sizeof (it));
    it.identifier = identifier;
    it.it_flags = STATEMENT_ITERATOR_FLAG_REVERSE;
    it.stmt_mask = stmt_mask;
    filter_def = filter->ndat->u.rexpr_filter.filter_def;
    if (NULL != filter_def) {
        it.scope = scope;
        it.stmt_lists = &filter_def->scope_block.block_stmt_list;
        it.stmt_remaining = stmt_mask;
        filter_riter_start_list_internal(&it);
    }
    return it;
}

struct statement_iterator
filter_riter_statements_from(
    struct ast_node_hdl *filter, struct box *scope,
    const struct statement *stmt, const char *identifier)
{
    struct statement_iterator it;

    memset(&it, 0, sizeof (it));
    it.identifier = identifier;
    it.scope = scope;
    it.it_flags = STATEMENT_ITERATOR_FLAG_REVERSE;
    it.next_stmt = filter_iter_statements_advance_internal(&it, stmt);
    return it;
}

bitpunch_status_t
filter_iter_statements_next_internal(
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
            it->next_stmt = filter_iter_statements_advance_internal(it, stmt);
            if (NULL != stmt_typep) {
                *stmt_typep = filter_iter_get_current_statement_type(it);
            }
            if (NULL != stmtp) {
                *stmtp = stmt;
            }
            return BITPUNCH_OK;
        }
        // condition is false: go on with next statement
        stmt = filter_iter_statements_advance_internal(it, stmt);
    }
    if (0 != it->stmt_remaining) {
        if (0 != (it->it_flags & STATEMENT_ITERATOR_FLAG_REVERSE)) {
            filter_riter_start_list_internal(it);
        } else {
            filter_iter_start_list_internal(it);
        }
        return filter_iter_statements_next_internal(it, stmt_typep, stmtp, bst);
    }
    return BITPUNCH_NO_ITEM;
}

static bitpunch_status_t
filter_lookup_statement_recur(
    struct ast_node_hdl *filter, struct box *scope,
    const struct block_stmt_list *stmt_lists,
    enum statement_type stmt_mask,
    const char *identifier,
    enum statement_type *stmt_typep,
    const struct named_statement **stmtp,
    struct box **scopep,
    struct browse_state *bst);

static bitpunch_status_t
filter_lookup_statement_in_anonymous_field_recur(
    struct ast_node_hdl *filter, struct box *scope,
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
            &as_type->ndat->u.rexpr_filter.filter_def
            ->scope_block.block_stmt_list)) {
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
        bt_ret = filter_lookup_statement_recur(
            as_type, anon_scope,
            &as_type->ndat->u.rexpr_filter.filter_def
            ->scope_block.block_stmt_list,
            stmt_mask, identifier, stmt_typep, stmtp, scopep, bst);
    }
    box_delete(anon_scope);
    return bt_ret;
}

static bitpunch_status_t
filter_get_first_statement_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct statement **stmtp,
    struct browse_state *bst)
{
    struct statement_iterator it;

    it = filter_iter_statements(filter, scope, stmt_mask, identifier);
    return filter_iter_statements_next_internal(&it, stmt_typep, stmtp, bst);
}

static bitpunch_status_t
filter_lookup_statement_recur(
    struct ast_node_hdl *filter, struct box *scope,
    const struct block_stmt_list *stmt_lists,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    struct browse_state *bst)
{
    struct statement *stmt;
    struct named_statement *nstmt;
    bitpunch_status_t bt_ret;

    bt_ret = filter_get_first_statement_internal(
        filter, scope, stmt_mask, identifier,
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
                bt_ret = filter_lookup_statement_in_anonymous_field_recur(
                    filter, scope, stmt_mask, identifier, nstmt,
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
filter_lookup_statement_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    struct browse_state *bst)
{
    struct filter_def *filter_def;

    assert(ast_node_is_rexpr_filter(filter));
    filter_def = filter->ndat->u.rexpr_filter.filter_def;
    if (NULL == filter_def) {
        return BITPUNCH_NO_ITEM;
    }
    return filter_lookup_statement_recur(
        filter, scope,
        &filter_def->scope_block.block_stmt_list, stmt_mask, identifier,
        stmt_typep, stmtp, scopep, bst);
}

bitpunch_status_t
filter_get_n_statements_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    int64_t *stmt_countp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct statement_iterator it;
    int64_t stmt_count;

    it = filter_iter_statements(filter, scope, stmt_mask, identifier);
    stmt_count = -1;
    do {
        ++stmt_count;
        bt_ret = filter_iter_statements_next_internal(&it, NULL, NULL, bst);
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
filter_evaluate_identifier_internal(
    struct ast_node_hdl *filter, struct box *scope,
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

    bt_ret = filter_lookup_statement_internal(
        filter, scope, stmt_mask, identifier,
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
filter_evaluate_attribute_internal(
    struct ast_node_hdl *filter, struct box *scope,
    const char *attr_name,
    const struct named_expr **attrp,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    return filter_evaluate_identifier_internal(
        filter, scope, STATEMENT_TYPE_ATTRIBUTE, attr_name,
        NULL, (const struct named_statement **)attrp, NULL,
        valuep, dpathp, bst);
}

bitpunch_status_t
filter_evaluate_identifier(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        filter_evaluate_identifier_internal(
            filter, scope, stmt_mask, identifier,
            NULL, NULL, NULL, valuep, dpathp, &bst),
        &bst, errp);
}

bitpunch_status_t
filter_iter_statements_next(
    struct statement_iterator *it,
    enum statement_type *stmt_typep, const struct statement **stmtp,
    struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        filter_iter_statements_next_internal(it, stmt_typep, stmtp, &bst),
        &bst, errp);
}

/**
 * @brief attach a native value as an attribute to the filter
 *
 * @note target usage is mostly unit tests for filters
 */
void
filter_attach_native_attribute(
    struct ast_node_hdl *filter,
    const char *attr_name, expr_value_t value)
{
    struct filter_def *filter_def;
    struct statement_list *attribute_list;
    struct named_expr *attr;

    filter_def = filter->ndat->u.rexpr_filter.filter_def;
    attribute_list = filter_def->scope_block.block_stmt_list.attribute_list;
    attr = new_safe(struct named_expr);
    attr->nstmt.name = strdup_safe(attr_name);
    attr->expr = ast_node_new_rexpr_native(value);
    TAILQ_INSERT_TAIL(attribute_list, (struct statement *)attr, list);
}

struct ast_node_hdl *
filter_get_first_declared_attribute(
    const struct ast_node_hdl *filter,
    const char *attr_name)
{
    struct named_expr *attr_stmt;

    STATEMENT_FOREACH(
        named_expr, attr_stmt,
        filter->ndat->u.rexpr_filter.filter_def
        ->scope_block.block_stmt_list.attribute_list, list) {
        if (0 == strcmp(attr_stmt->nstmt.name, attr_name)) {
            return attr_stmt->expr;
        }
    }
    return NULL;
}
