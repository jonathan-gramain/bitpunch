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
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include <stdio.h>

#include "api/bitpunch-structs.h"
#include "core/ast.h"
#include "core/expr.h"
#include "core/browse.h"
#include "core/browse_internal.h"
#include "core/print.h"
#include "core/parser.h"
#include "core/debug.h"
#include "filters/composite.h"
#include "filters/array.h"
#include "filters/array_slice.h"
#include "filters/byte.h"
#include "filters/byte_array.h"
#include "filters/byte_slice.h"
#include PATH_TO_PARSER_TAB_H

//#define OUTPUT_DEP_GRAPH

struct list_of_visible_refs {
    const struct list_of_visible_refs *outer_refs;
    const struct ast_node_hdl         *cur_filter;
    const struct block_stmt_list      *cur_lists;
};

typedef dep_resolver_tagset_t
(*compile_func_t)(struct compile_ctx *ctx,
                  enum resolve_expect_mask expect_mask,
                  struct dep_resolver_node *_node,
                  dep_resolver_tagset_t tags,
                  void *arg);

struct compile_req {
    compile_func_t compile_cb;
    struct compile_ctx *ctx;
    enum resolve_expect_mask expect_mask;
    void *arg;
};

static int
resolve_identifiers(struct ast_node_hdl *node,
                    const struct list_of_visible_refs *visible_refs,
                    enum resolve_expect_mask expect_mask,
                    enum resolve_identifiers_tag resolve_tags);
static int
compile_ast_node_all(struct ast_node_hdl *ast_root,
                     enum resolve_expect_mask expect_mask);
static dep_resolver_tagset_t
dep_resolver_cb(struct dep_resolver *dr,
                struct dep_resolver_node *_node,
                dep_resolver_tagset_t tags,
                void *arg);
static void
dep_resolver_free_arg(struct dep_resolver *dr,
                      void *arg);
static dep_resolver_tagset_t
compile_node_cb(struct compile_ctx *ctx,
                enum resolve_expect_mask expect_mask,
                struct dep_resolver_node *_node,
                dep_resolver_tagset_t tags,
                void *arg);
static dep_resolver_tagset_t
compile_node_once(struct ast_node_hdl *node,
                  dep_resolver_tagset_t tags,
                  struct compile_ctx *ctx,
                  enum resolve_expect_mask expect_mask);
static dep_resolver_tagset_t
compile_dpath_cb(struct compile_ctx *ctx,
                 enum resolve_expect_mask expect_mask,
                 struct dep_resolver_node *_node,
                 dep_resolver_tagset_t tags,
                 void *arg);
static int
compile_dpath_type(struct dpath_node *node,
                   struct compile_ctx *ctx);
static int
compile_dpath_span_size(struct dpath_node *node,
                        struct compile_ctx *ctx);
static dep_resolver_tagset_t
compile_field_cb(struct compile_ctx *ctx,
                 enum resolve_expect_mask expect_mask,
                 struct dep_resolver_node *_node,
                 dep_resolver_tagset_t tags,
                 void *arg);
static dep_resolver_tagset_t
compile_subscript_index_cb(struct compile_ctx *ctx,
                           enum resolve_expect_mask expect_mask,
                           struct dep_resolver_node *_node,
                           dep_resolver_tagset_t tags,
                           void *arg);

ARRAY_GENERATE_API_DEFS(ast_node_hdl_array, struct ast_node_hdl *)


int
bitpunch_compile_schema(struct ast_node_hdl *schema)
{
    if (-1 == resolve_identifiers(schema, NULL,
                                  RESOLVE_EXPECT_TYPE,
                                  RESOLVE_TYPE_IDENTIFIERS)) {
        return -1;
    }
    if (-1 == resolve_identifiers(schema, NULL,
                                  RESOLVE_EXPECT_TYPE,
                                  RESOLVE_EXPRESSION_IDENTIFIERS)) {
        return -1;
    }
    if (-1 == compile_ast_node_all(schema,
                                   RESOLVE_EXPECT_TYPE)) {
        return -1;
    }
    return 0;
}

static void
compile_ctx_init(struct compile_ctx *ctx)
{
    memset(ctx, 0, sizeof (*ctx));
    ctx->dep_resolver = dep_resolver_create(dep_resolver_cb,
                                            dep_resolver_free_arg);
}

static void
compile_ctx_destroy(struct compile_ctx *ctx)
{
    dep_resolver_destroy(ctx->dep_resolver);
}

static struct compile_req *
compile_req_create(compile_func_t compile_cb,
                   struct compile_ctx *ctx,
                   enum resolve_expect_mask expect_mask)
{
    struct compile_req *req;

    req = new_safe(struct compile_req);
    req->compile_cb = compile_cb;
    req->ctx = ctx;
    req->expect_mask = expect_mask;
    return req;
}

static void
compile_req_destroy(struct compile_req *req)
{
    free(req);
}

static const char *
reverse_lookup_typename(const struct ast_node_hdl *node,
                        const struct list_of_visible_refs *visible_refs)
{
    const struct list_of_visible_refs *refs_level;
    const struct statement_list *cur_named_exprs;
    struct named_expr *type_def;

    for (refs_level = visible_refs; NULL != refs_level;
         refs_level = refs_level->outer_refs) {
        cur_named_exprs = refs_level->cur_lists->named_expr_list;
        STATEMENT_FOREACH(named_expr, type_def, cur_named_exprs, list) {
            if (node == type_def->expr)
                return type_def->nstmt.name;
        }
    }
    return NULL;
}

static const struct statement_list *
block_stmt_lists_get_list(enum statement_type stmt_type,
                          const struct block_stmt_list *stmt_lists)
{
    switch (stmt_type) {
    case STATEMENT_TYPE_FIELD:
        return stmt_lists->field_list;
    case STATEMENT_TYPE_NAMED_EXPR:
        return stmt_lists->named_expr_list;
    case STATEMENT_TYPE_ATTRIBUTE:
        return stmt_lists->attribute_list;
    default:
        assert(0);
    }
}

#define MAX_N_VISIBLE_STATEMENTS 1000
#define LOOKUP_END -2

static int
append_named_statement_spec(
    enum statement_type stmt_type,
    struct named_statement *nstmt,
    struct ast_node_hdl *anchor_filter,
    int anonymous_member,
    struct named_statement_spec **visible_statementsp,
    int *visible_statements_indexp)
{
    struct named_statement_spec *statement_spec;

    if (NULL == visible_statementsp) {
        // condition used to know about item visibility (boolean)
        return -1;
    }
    if (*visible_statements_indexp == MAX_N_VISIBLE_STATEMENTS) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &nstmt->stmt.loc,
                       "too many polymorphic statements in scope (max %d)",
                       MAX_N_VISIBLE_STATEMENTS);
        return -1;
    }
    *visible_statementsp = realloc_safe(
        *visible_statementsp,
        (*visible_statements_indexp + 1) * sizeof (**visible_statementsp));
    statement_spec = &(*visible_statementsp)[*visible_statements_indexp];
    statement_spec->stmt_type = stmt_type;
    statement_spec->nstmt = nstmt;
    statement_spec->anchor_filter = anchor_filter;
    statement_spec->anonymous_member = anonymous_member;
    ++(*visible_statements_indexp);
    return 0;
}

static int
lookup_visible_statements_in_lists_internal(
    enum statement_type stmt_mask,
    const char *lookup_identifier, struct ast_node_hdl *lookup_filter,
    const struct block_stmt_list *stmt_lists,
    int anonymous_member,
    struct named_statement_spec **visible_statementsp,
    int *visible_statements_indexp);

static int
lookup_visible_statements_in_anonymous_field(
    enum statement_type stmt_mask,
    const char *lookup_identifier, struct ast_node_hdl *lookup_filter,
    struct field *field,
    struct named_statement_spec **visible_statementsp,
    int *visible_statements_indexp)
{
    const struct ast_node_hdl *filter;
    int ret;
    const struct block_stmt_list *stmt_lists;

    filter = ast_node_get_as_type(field->filter);
    if (NULL != filter) {
        if (filter == lookup_filter) {
            return -1;
        }
        if (AST_NODE_TYPE_FILTER_DEF == filter->ndat->type) {
            stmt_lists = &filter->ndat->u.scope_def.block_stmt_list;
        } else if (ast_node_is_rexpr_filter(filter)) {
            stmt_lists = &filter_get_const_scope_def(filter)->block_stmt_list;
        } else {
            stmt_lists = NULL;
        }
        if (NULL != stmt_lists) {
            ret = lookup_visible_statements_in_lists_internal(
                stmt_mask, lookup_identifier, lookup_filter, stmt_lists, TRUE,
                visible_statementsp, visible_statements_indexp);
            if (0 != ret) {
                return ret;
            }
        }
    }
    return 0;
}

static int
lookup_visible_statements_in_lists_internal(
    enum statement_type stmt_mask,
    const char *lookup_identifier, struct ast_node_hdl *lookup_filter,
    const struct block_stmt_list *stmt_lists,
    int anonymous_member,
    struct named_statement_spec **visible_statementsp,
    int *visible_statements_indexp)
{
    enum statement_type stmt_type;
    const struct statement_list *stmt_list;
    struct statement *stmt;
    struct named_statement *nstmt;
    enum statement_type named_stmt_types_by_prio[] = {
        STATEMENT_TYPE_NAMED_EXPR,
        STATEMENT_TYPE_FIELD,
    };
    enum statement_type attribute_types_by_prio[] = {
        STATEMENT_TYPE_ATTRIBUTE,
    };
    enum statement_type *stmt_types_by_prio;
    int n_stmt_types_by_prio;
    int i;
    int ret;

    if (NULL != lookup_identifier) {
        if (lookup_identifier[0] == '@') {
            stmt_types_by_prio = attribute_types_by_prio;
            n_stmt_types_by_prio = N_ELEM(attribute_types_by_prio);
        } else {
            stmt_types_by_prio = named_stmt_types_by_prio;
            n_stmt_types_by_prio = N_ELEM(named_stmt_types_by_prio);
        }
        for (i = 0; i < n_stmt_types_by_prio; ++i) {
            stmt_type = stmt_types_by_prio[i];
            if (0 == (stmt_mask & stmt_type)) {
                continue ;
            }
            stmt_list = block_stmt_lists_get_list(stmt_type, stmt_lists);
            // local fields with no anonymous component have priority
            TAILQ_FOREACH(stmt, stmt_list, list) {
                nstmt = (struct named_statement *)stmt;
                if (NULL != nstmt->name
                    && 0 == strcmp(lookup_identifier, nstmt->name)) {
                    if (-1 == append_named_statement_spec(
                            stmt_type, nstmt, NULL, anonymous_member,
                            visible_statementsp, visible_statements_indexp)) {
                        return -1;
                    }
                    if (NULL == nstmt->stmt.cond) {
                        // current scoped expression is unconditional, no
                        // need to go any further by decreasing visibility
                        // order
                        return LOOKUP_END;
                    }
                }
            }
        }
    }
    // recurse in anonymous struct/union fields
    TAILQ_FOREACH(stmt, stmt_lists->field_list, list) {
        nstmt = (struct named_statement *)stmt;
        if (NULL == nstmt->name
            && !(nstmt->stmt.stmt_flags & FIELD_FLAG_HIDDEN)) {
            ret = lookup_visible_statements_in_anonymous_field(
                stmt_mask, lookup_identifier, lookup_filter,
                (struct field *)nstmt,
                visible_statementsp, visible_statements_indexp);
            if (0 != ret) {
                return ret;
            }
        }
    }
    return 0;
}

/**
 * @brief gather statements of selected types with matching names in
 * current scope
 *
 * The visibility order is, by decreasing weight in priority:
 *
 * - from inner to outer block scope
 *
 * - in a given block, first explicit statements, then statements
 *   found in anonymous fields
 *
 * - finally, in the same block and explicit/anonymous context, named
 *   expressions prime over field names
 */
static int
lookup_all_visible_statements(
    enum statement_type stmt_mask,
    const char *identifier,
    const struct list_of_visible_refs *visible_refs,
    struct named_statement_spec **visible_statementsp)
{
    struct named_statement_spec *visible_statements;
    int visible_statements_index;
    int last_visible_statements_index;
    const struct list_of_visible_refs *refs_level;
    int ret;

    visible_statements = NULL;
    visible_statements_index = 0;
    ret = 0;
    for (refs_level = visible_refs;
         NULL != refs_level && LOOKUP_END != ret;
         refs_level = refs_level->outer_refs) {
        last_visible_statements_index = visible_statements_index;
        ret = lookup_visible_statements_in_lists_internal(
            stmt_mask, identifier, NULL, refs_level->cur_lists, FALSE,
            &visible_statements, &visible_statements_index);
        if (-1 == ret) {
            free(visible_statements);
            return -1;
        }
        while (last_visible_statements_index < visible_statements_index) {
            visible_statements[last_visible_statements_index].anchor_filter =
                refs_level->cur_filter;
            ++last_visible_statements_index;
        }
    }
    *visible_statementsp = visible_statements;
    return visible_statements_index;
}

static int
lookup_visible_statements_in_lists(
    enum statement_type stmt_mask,
    const char *identifier,
    const struct block_stmt_list *stmt_lists,
    struct named_statement_spec **visible_statementsp)
{
    struct named_statement_spec *visible_statements;
    int visible_statements_index;
    int ret;
    
    visible_statements = NULL;
    visible_statements_index = 0;
    ret = lookup_visible_statements_in_lists_internal(
        stmt_mask, identifier, NULL, stmt_lists, FALSE,
        &visible_statements, &visible_statements_index);
    if (-1 == ret) {
        free(visible_statements);
        return -1;
    }
    *visible_statementsp = visible_statements;
    return visible_statements_index;
}

int
identifier_is_visible_in_block_stmt_lists(
    enum statement_type stmt_mask,
    const char *identifier,
    const struct block_stmt_list *stmt_lists)
{
    // -1 means that there was more names to lookup than the maximum
    // requested (0), so at least one.
    return -1 == lookup_visible_statements_in_lists_internal(
        stmt_mask, identifier, NULL, stmt_lists, FALSE,
        NULL, NULL);
}

int
filter_exists_in_scope(
    struct ast_node_hdl *scope_node,
    struct ast_node_hdl *lookup_filter)
{
    struct scope_def *scope_def;

    if (scope_node->ndat == lookup_filter->ndat) {
        return TRUE;
    }
    scope_def = ast_node_get_scope_def(scope_node);
    if (NULL == scope_def) {
        return FALSE;
    }
    // -1 means that there was more names to lookup than the maximum
    // requested (0), so at least one.
    return -1 == lookup_visible_statements_in_lists_internal(
        STATEMENT_TYPE_FIELD,
        NULL, lookup_filter, &scope_def->block_stmt_list, FALSE,
        NULL, NULL);
}

static int
resolve_identifiers_in_expression(
    struct ast_node_hdl *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_identifiers_tag resolve_tags)
{
    return resolve_identifiers(expr, visible_refs,
                               RESOLVE_EXPECT_EXPRESSION, resolve_tags);
}

static int
resolve_identifier_as_scoped_statement(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs,
    enum statement_type stmt_mask)
{
    struct named_statement_spec *visible_statements;
    int n_visible_statements;
    struct named_statement_spec *stmt_spec;
    struct ast_node_data *resolved_type;

    n_visible_statements = lookup_all_visible_statements(
        stmt_mask, node->ndat->u.identifier, visible_refs,
        &visible_statements);
    if (-1 == n_visible_statements) {
        return -1;
    }
    if (n_visible_statements > 0) {
        resolved_type = new_safe(struct ast_node_data);
        if (1 == n_visible_statements) {
            stmt_spec = &visible_statements[0];
            resolved_type->u.rexpr_member_common.anchor_filter =
                (struct ast_node_hdl *)stmt_spec->anchor_filter;
            switch (stmt_spec->stmt_type) {
            case STATEMENT_TYPE_NAMED_EXPR:
            case STATEMENT_TYPE_ATTRIBUTE:
                resolved_type->type = AST_NODE_TYPE_REXPR_NAMED_EXPR;
                resolved_type->u.rexpr_named_expr.named_expr =
                    (struct named_expr *)stmt_spec->nstmt;
                break ;
            case STATEMENT_TYPE_FIELD:
                resolved_type->type = AST_NODE_TYPE_REXPR_FIELD;
                resolved_type->u.rexpr_field.field =
                    (struct field *)stmt_spec->nstmt;
                break ;
            default:
                assert(0);
            }
            if (stmt_spec->anonymous_member) {
                node->flags |= ASTFLAG_IS_ANONYMOUS_MEMBER;
            }
            free(visible_statements);
        } else {
            resolved_type->type = AST_NODE_TYPE_REXPR_POLYMORPHIC;
            resolved_type->u.rexpr_polymorphic.identifier =
                node->ndat->u.identifier;
            resolved_type->u.rexpr_polymorphic.visible_statements =
                visible_statements;
            resolved_type->u.rexpr_polymorphic.n_visible_statements =
                n_visible_statements;
        }
        node->ndat = resolved_type;
        return 0;
    }
    return -1;
}

static int
resolve_identifiers_identifier_as_type(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs)
{
    return resolve_identifier_as_scoped_statement(
        node, visible_refs, STATEMENT_TYPE_NAMED_EXPR);
}

static int
resolve_identifiers_identifier_as_expression(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs)
{
    const struct expr_builtin_fn *builtin;

    if (0 == resolve_identifier_as_scoped_statement(
            node, visible_refs,
            STATEMENT_TYPE_NAMED_EXPR | STATEMENT_TYPE_FIELD)) {
        return 0;
    }
    builtin = expr_lookup_builtin_fn(node->ndat->u.identifier, NULL);
    if (NULL != builtin) {
        struct ast_node_data *resolved_type;

        resolved_type = new_safe(struct ast_node_data);
        resolved_type->type = AST_NODE_TYPE_REXPR_BUILTIN;
        resolved_type->u.rexpr_builtin.builtin = builtin;
        node->ndat = resolved_type;
        return 0;
    }
    return -1;
}

static int
resolve_identifiers_identifier_as_builtin_filter(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs)
{
    struct filter_class *filter_cls;
    struct ast_node_data *resolved_type;

    filter_cls = builtin_filter_lookup(node->ndat->u.identifier);
    if (NULL != filter_cls) {
        resolved_type = new_safe(struct ast_node_data);
        resolved_type->type = AST_NODE_TYPE_FILTER_DEF;
        resolved_type->u.filter_def.filter_type = node->ndat->u.identifier;
        init_block_stmt_list(
            &resolved_type->u.filter_def.scope_def.block_stmt_list);
        resolved_type->u.filter_def.filter_cls = filter_cls;
        node->ndat = resolved_type;
        return 0;
    }
    return -1;
}

static int
resolve_identifiers_identifier(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    if (0 != (resolve_tags & RESOLVE_TYPE_IDENTIFIERS)) {
        if (0 != (expect_mask & RESOLVE_EXPECT_TYPE)) {
            if (0 == resolve_identifiers_identifier_as_type(
                    node, visible_refs)) {
                return 0;
            }
            if (0 == resolve_identifiers_identifier_as_builtin_filter(
                    node, visible_refs)) {
                return 0;
            }
        }
    }
    if (0 != (resolve_tags & RESOLVE_EXPRESSION_IDENTIFIERS)) {
        if (0 != (expect_mask & RESOLVE_EXPECT_EXPRESSION)
            && 0 == resolve_identifiers_identifier_as_expression(
                node, visible_refs)) {
            return 0;
        }
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &node->loc,
            "identifier '%s' undefined in the scope",
            node->ndat->u.identifier);
        return -1;
    }
    return 0;
}

static int
resolve_identifiers_in_statement_list(
    struct statement_list *stmt_list,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_identifiers_tag resolve_tags)
{
    struct statement *stmt;

    TAILQ_FOREACH(stmt, stmt_list, list) {
        if (NULL != stmt->cond
            && -1 == resolve_identifiers_in_expression(stmt->cond,
                                                       visible_refs,
                                                       resolve_tags)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve_identifiers_field(
    struct field *field,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_identifiers_tag resolve_tags)
{
    return resolve_identifiers(field->filter, visible_refs,
                               RESOLVE_EXPECT_TYPE, resolve_tags);
}

static int
resolve_identifiers_in_stmt_lists(
    struct block_stmt_list *stmt_lists,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_identifiers_tag resolve_tags)
{
    struct field *field;
    struct named_expr *named_expr;

    /* it's required to first resolve type names before names in
     * expressions, so that names can be found inside anonymous types
     * or filtered types (as-types) */
    if (-1 == resolve_identifiers_in_statement_list(
            stmt_lists->field_list, visible_refs, resolve_tags)) {
        return -1;
    }
    if (-1 == resolve_identifiers_in_statement_list(
            stmt_lists->named_expr_list, visible_refs, resolve_tags)) {
        return -1;
    }
    if (-1 == resolve_identifiers_in_statement_list(
            stmt_lists->attribute_list, visible_refs, resolve_tags)) {
        return -1;
    }
    STATEMENT_FOREACH(named_expr, named_expr,
                      stmt_lists->named_expr_list, list) {
        if (NULL != named_expr->expr
            && -1 == resolve_identifiers(named_expr->expr, visible_refs,
                                         RESOLVE_EXPECT_TYPE |
                                         RESOLVE_EXPECT_EXPRESSION,
                                         resolve_tags)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(field, field, stmt_lists->field_list, list) {
        if (-1 == resolve_identifiers_field(field, visible_refs,
                                            resolve_tags)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(named_expr, named_expr,
                      stmt_lists->attribute_list, list) {
        if (-1 == resolve_identifiers(named_expr->expr, visible_refs,
                                      RESOLVE_EXPECT_TYPE |
                                      RESOLVE_EXPECT_EXPRESSION,
                                      resolve_tags)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve_identifiers_scope_def(struct ast_node_hdl *scope_def,
                              const struct list_of_visible_refs *visible_refs,
                              enum resolve_expect_mask expect_mask,
                              enum resolve_identifiers_tag resolve_tags)
{
    struct block_stmt_list *stmt_lists;
    struct list_of_visible_refs inner_refs;

    stmt_lists = &scope_def->ndat->u.scope_def.block_stmt_list;
    /* add current refs to the chain of visible refs */
    inner_refs.outer_refs = visible_refs;
    inner_refs.cur_filter = scope_def;
    inner_refs.cur_lists = stmt_lists;

    return resolve_identifiers_in_stmt_lists(stmt_lists, &inner_refs,
                                             resolve_tags);
}

static int
resolve_identifiers_filter_def(struct ast_node_hdl *filter_def,
                               const struct list_of_visible_refs *visible_refs,
                               enum resolve_expect_mask expect_mask,
                               enum resolve_identifiers_tag resolve_tags)
{
    const char *filter_type;
    struct filter_class *filter_cls;

    if (-1 == resolve_identifiers_scope_def(
            filter_def, visible_refs, expect_mask, resolve_tags)) {
        return -1;
    }
    filter_type = filter_def->ndat->u.filter_def.filter_type;
    filter_cls = builtin_filter_lookup(filter_type);
    if (NULL == filter_cls) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &filter_def->loc,
            "no filter named '%s' exists",
            filter_type);
        return -1;
    }
    filter_def->ndat->u.filter_def.filter_cls = filter_cls;
    return 0;
}

static int
resolve_identifiers_conditional(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    if (0 == (expect_mask & RESOLVE_EXPECT_EXPRESSION)) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                       "expect an expression");
        return -1;
    }
    if (NULL != node->ndat->u.conditional.outer_cond
        && -1 == resolve_identifiers(node->ndat->u.conditional.outer_cond,
                                     visible_refs, expect_mask,
                                     resolve_tags)) {
        return -1;
    }
    if (-1 == resolve_identifiers_in_expression(
            node->ndat->u.conditional.cond_expr, visible_refs,
            resolve_tags)) {
        return -1;
    }
    return 0;
}

static int
resolve_identifiers_in_subscript_index(
    struct subscript_index *index,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_identifiers_tag resolve_tags)
{
    if (NULL != index->key
        && -1 == resolve_identifiers_in_expression(
            index->key, visible_refs, resolve_tags)) {
        return -1;
    }
    if (NULL != index->twin
        && -1 == resolve_identifiers_in_expression(
            index->twin, visible_refs, resolve_tags)) {
        return -1;
    }
    return 0;
}

static int
resolve_identifiers_operator_subscript(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    if (-1 == resolve_identifiers(node->ndat->u.op_subscript_common.anchor_expr,
                                  visible_refs, expect_mask,
                                  resolve_tags)) {
        return -1;
    }
    if (-1 == resolve_identifiers_in_subscript_index(
            &node->ndat->u.op_subscript.index, visible_refs, resolve_tags)) {
        return -1;
    }
    return 0;
}

static int
resolve_identifiers_operator_subscript_slice(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    if (-1 == resolve_identifiers(node->ndat->u.op_subscript_common.anchor_expr,
                                  visible_refs,
                                  RESOLVE_EXPECT_EXPRESSION,
                                  resolve_tags)) {
        return -1;
    }
    if (-1 == resolve_identifiers_in_subscript_index(
            &node->ndat->u.op_subscript_slice.start, visible_refs,
            resolve_tags)) {
        return -1;
    }
    if (-1 == resolve_identifiers_in_subscript_index(
            &node->ndat->u.op_subscript_slice.end, visible_refs,
            resolve_tags)) {
        return -1;
    }
    return 0;
}

static int
resolve_identifiers_op_fcall(
    struct ast_node_hdl *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    struct statement *stmt;
    struct named_expr *param;

    if (-1 == resolve_identifiers(expr->ndat->u.op_fcall.func, visible_refs,
                                  RESOLVE_EXPECT_EXPRESSION,
                                  resolve_tags)) {
        return -1;
    }
    /* resolve expressions in parameter list */
    TAILQ_FOREACH(stmt, expr->ndat->u.op_fcall.func_params, list) {
        param = (struct named_expr *)stmt;
        if (-1 == resolve_identifiers_in_expression(
                param->expr, visible_refs, resolve_tags)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve_identifiers_operator(
    struct ast_node_hdl *node,
    int n_operands,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    int opd_i;

    if (0 == (expect_mask & RESOLVE_EXPECT_EXPRESSION)) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                       "unexpected expression");
        return -1;
    }
    for (opd_i = 0; opd_i < n_operands; ++opd_i) {
        if (-1 == resolve_identifiers(node->ndat->u.op.operands[opd_i],
                                      visible_refs,
                                      RESOLVE_EXPECT_EXPRESSION,
                                      resolve_tags)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve_identifiers_operator_filter(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    if (-1 == resolve_identifiers(
            node->ndat->u.op.operands[0], visible_refs,
            expect_mask & (RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_DPATH_EXPRESSION),
            resolve_tags)) {
        return -1;
    }
    if (-1 == resolve_identifiers(node->ndat->u.op.operands[1], visible_refs,
                                  RESOLVE_EXPECT_TYPE,
                                  resolve_tags)) {
        return -1;
    }
    return 0;
}

static int
resolve_identifiers_expr_self(
    struct ast_node_hdl *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    struct ast_node_data *compiled_type;

    if (NULL == visible_refs) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                       "no scope to evaluate 'self' in expression");
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_SELF;
    // const-cast
    compiled_type->u.rexpr_member_common.anchor_filter =
        (struct ast_node_hdl *)visible_refs->cur_filter;
    expr->ndat = compiled_type;
    return 0;
}


static enum expr_value_type
expr_value_type_mask_from_node(const struct ast_node_hdl *node)
{
    if (ast_node_is_rexpr(node)) {
        return node->ndat->u.rexpr.value_type_mask;
    }
    return EXPR_VALUE_TYPE_UNSET;
}

static enum expr_dpath_type
expr_dpath_type_mask_from_node(const struct ast_node_hdl *node)
{
    if (ast_node_is_rexpr(node)) {
        return node->ndat->u.rexpr.dpath_type_mask;
    }
    return EXPR_DPATH_TYPE_UNSET;
}

static int
resolve_identifiers_operator_on_dpath(
    struct ast_node_hdl *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    return resolve_identifiers(expr->ndat->u.op.operands[0],
                               visible_refs,
                               RESOLVE_EXPECT_DPATH_EXPRESSION,
                               resolve_tags);
}

static int
resolve_identifiers_operator_member(
    struct ast_node_hdl *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    return resolve_identifiers(expr->ndat->u.op.operands[0],
                               visible_refs,
                               RESOLVE_EXPECT_TYPE |
                               RESOLVE_EXPECT_DPATH_EXPRESSION,
                               resolve_tags);
}

static int
resolve_identifiers_operator_scope(
    struct ast_node_hdl *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    return resolve_identifiers(expr->ndat->u.op.operands[0],
                               visible_refs,
                               RESOLVE_EXPECT_TYPE,
                               resolve_tags);
}

static int
resolve_identifiers_operator_sizeof(
    struct ast_node_hdl *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    return resolve_identifiers(expr->ndat->u.op.operands[0],
                               visible_refs,
                               RESOLVE_EXPECT_TYPE |
                               RESOLVE_EXPECT_DPATH_EXPRESSION,
                               resolve_tags);
}

static int
resolve_identifiers_internal(struct ast_node_hdl *node,
                             const struct list_of_visible_refs *visible_refs,
                             enum resolve_expect_mask expect_mask,
                             enum resolve_identifiers_tag resolve_tags)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_IDENTIFIER:
        return resolve_identifiers_identifier(node, visible_refs,
                                              expect_mask, resolve_tags);
    case AST_NODE_TYPE_SCOPE_DEF:
        return resolve_identifiers_scope_def(node, visible_refs,
                                             expect_mask, resolve_tags);
    case AST_NODE_TYPE_FILTER_DEF:
        return resolve_identifiers_filter_def(node, visible_refs,
                                              expect_mask, resolve_tags);
    case AST_NODE_TYPE_CONDITIONAL:
        return resolve_identifiers_conditional(node, visible_refs,
                                               expect_mask, resolve_tags);
        /* for all operators: resolve potential type names in operand
         * sub-expressions (e.g. sizeof) */
    case AST_NODE_TYPE_OP_UPLUS:
    case AST_NODE_TYPE_OP_UMINUS:
    case AST_NODE_TYPE_OP_LNOT:
    case AST_NODE_TYPE_OP_BWNOT:
        return resolve_identifiers_operator(node, 1, visible_refs,
                                            expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_EQ:
    case AST_NODE_TYPE_OP_NE:
    case AST_NODE_TYPE_OP_GT:
    case AST_NODE_TYPE_OP_LT:
    case AST_NODE_TYPE_OP_GE:
    case AST_NODE_TYPE_OP_LE:
    case AST_NODE_TYPE_OP_LOR:
    case AST_NODE_TYPE_OP_LAND:
    case AST_NODE_TYPE_OP_BWOR:
    case AST_NODE_TYPE_OP_BWXOR:
    case AST_NODE_TYPE_OP_BWAND:
    case AST_NODE_TYPE_OP_LSHIFT:
    case AST_NODE_TYPE_OP_RSHIFT:
    case AST_NODE_TYPE_OP_ADD:
    case AST_NODE_TYPE_OP_SUB:
    case AST_NODE_TYPE_OP_MUL:
    case AST_NODE_TYPE_OP_DIV:
    case AST_NODE_TYPE_OP_MOD:
        return resolve_identifiers_operator(node, 2, visible_refs,
                                            expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_FILTER:
        return resolve_identifiers_operator_filter(
            node, visible_refs, expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_ADDROF:
    case AST_NODE_TYPE_OP_ANCESTOR:
        return resolve_identifiers_operator_on_dpath(node, visible_refs,
                                                     expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_MEMBER:
        return resolve_identifiers_operator_member(node, visible_refs,
                                                   expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_SCOPE:
        return resolve_identifiers_operator_scope(node, visible_refs,
                                                  expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_SIZEOF:
        return resolve_identifiers_operator_sizeof(node, visible_refs,
                                                   expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_SUBSCRIPT:
        return resolve_identifiers_operator_subscript(
            node, visible_refs, expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
        return resolve_identifiers_operator_subscript_slice(
            node, visible_refs, expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_FCALL:
        return resolve_identifiers_op_fcall(node, visible_refs,
                                            expect_mask, resolve_tags);
    case AST_NODE_TYPE_EXPR_SELF:
        return resolve_identifiers_expr_self(node, visible_refs,
                                             expect_mask, resolve_tags);
    default:
        /* nothing to resolve */
        return 0;
    }
    /*NOT REACHED*/
}

static int
resolve_identifiers(struct ast_node_hdl *node,
                    const struct list_of_visible_refs *visible_refs,
                    enum resolve_expect_mask expect_mask,
                    enum resolve_identifiers_tag resolve_tags)
{
    int ret;

    if (0 == (resolve_tags & ~node->resolved_tags)) {
        return 0;
    }
    ret = resolve_identifiers_internal(
        node, visible_refs, expect_mask, resolve_tags);
    node->resolved_tags |= resolve_tags;
    return ret;
}

/* COMPILE */

static enum ast_node_type
op_type_ast2rexpr(enum ast_node_type type)
{
    switch (type) {
    case AST_NODE_TYPE_OP_EQ:
        return AST_NODE_TYPE_REXPR_OP_EQ;
    case AST_NODE_TYPE_OP_NE:
        return AST_NODE_TYPE_REXPR_OP_NE;
    case AST_NODE_TYPE_OP_GT:
        return AST_NODE_TYPE_REXPR_OP_GT;
    case AST_NODE_TYPE_OP_LT:
        return AST_NODE_TYPE_REXPR_OP_LT;
    case AST_NODE_TYPE_OP_GE:
        return AST_NODE_TYPE_REXPR_OP_GE;
    case AST_NODE_TYPE_OP_LE:
        return AST_NODE_TYPE_REXPR_OP_LE;
    case AST_NODE_TYPE_OP_LOR:
        return AST_NODE_TYPE_REXPR_OP_LOR;
    case AST_NODE_TYPE_OP_LAND:
        return AST_NODE_TYPE_REXPR_OP_LAND;
    case AST_NODE_TYPE_OP_BWOR:
        return AST_NODE_TYPE_REXPR_OP_BWOR;
    case AST_NODE_TYPE_OP_BWXOR:
        return AST_NODE_TYPE_REXPR_OP_BWXOR;
    case AST_NODE_TYPE_OP_BWAND:
        return AST_NODE_TYPE_REXPR_OP_BWAND;
    case AST_NODE_TYPE_OP_LSHIFT:
        return AST_NODE_TYPE_REXPR_OP_LSHIFT;
    case AST_NODE_TYPE_OP_RSHIFT:
        return AST_NODE_TYPE_REXPR_OP_RSHIFT;
    case AST_NODE_TYPE_OP_ADD:
        return AST_NODE_TYPE_REXPR_OP_ADD;
    case AST_NODE_TYPE_OP_SUB:
        return AST_NODE_TYPE_REXPR_OP_SUB;
    case AST_NODE_TYPE_OP_MUL:
        return AST_NODE_TYPE_REXPR_OP_MUL;
    case AST_NODE_TYPE_OP_DIV:
        return AST_NODE_TYPE_REXPR_OP_DIV;
    case AST_NODE_TYPE_OP_MOD:
        return AST_NODE_TYPE_REXPR_OP_MOD;
    case AST_NODE_TYPE_OP_UPLUS:
        return AST_NODE_TYPE_REXPR_OP_UPLUS;
    case AST_NODE_TYPE_OP_UMINUS:
        return AST_NODE_TYPE_REXPR_OP_UMINUS;
    case AST_NODE_TYPE_OP_LNOT:
        return AST_NODE_TYPE_REXPR_OP_LNOT;
    case AST_NODE_TYPE_OP_BWNOT:
        return AST_NODE_TYPE_REXPR_OP_BWNOT;
    case AST_NODE_TYPE_OP_SIZEOF:
        return AST_NODE_TYPE_REXPR_OP_SIZEOF;
    case AST_NODE_TYPE_OP_ADDROF:
        return AST_NODE_TYPE_REXPR_OP_ADDROF;
    case AST_NODE_TYPE_OP_ANCESTOR:
        return AST_NODE_TYPE_REXPR_OP_ANCESTOR;
    case AST_NODE_TYPE_OP_FILTER:
        return AST_NODE_TYPE_REXPR_OP_FILTER;
    case AST_NODE_TYPE_OP_SUBSCRIPT:
        return AST_NODE_TYPE_REXPR_OP_SUBSCRIPT;
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
        return AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE;
    case AST_NODE_TYPE_OP_MEMBER:
        return AST_NODE_TYPE_REXPR_OP_MEMBER;
    case AST_NODE_TYPE_OP_SCOPE:
        return AST_NODE_TYPE_REXPR_OP_SCOPE;
    case AST_NODE_TYPE_OP_FCALL:
        /*TODO*/
        return AST_NODE_TYPE_NONE;
    default:
        assert(0);
    }
    /*NOT REACHED*/
}

static int
resolve_expr_internal(struct ast_node_hdl *expr,
                      struct list_of_visible_refs *inmost_refs)
{
    if (-1 == resolve_identifiers_in_expression(expr, inmost_refs,
                                                RESOLVE_ALL_IDENTIFIERS)) {
        return -1;
    }
    if (-1 == compile_ast_node_all(expr, RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    return 0;
}

static int
resolve_expr_scoped_recur(struct ast_node_hdl *expr,
                          struct box *cur_scope,
                          struct list_of_visible_refs *inner_refs,
                          struct list_of_visible_refs *inmost_refs)
{
    struct list_of_visible_refs visible_refs;

    while (NULL != cur_scope
           && !(ast_node_is_filter(cur_scope->filter) ||
                ast_node_is_scope_def(cur_scope->filter))) {
        cur_scope = cur_scope->scope;
    }
    if (NULL == cur_scope) {
        return resolve_expr_internal(expr, inmost_refs);
    }
    visible_refs.outer_refs = NULL;
    visible_refs.cur_filter = cur_scope->filter;
    visible_refs.cur_lists =
        &ast_node_get_scope_def(cur_scope->filter)->block_stmt_list;
    if (NULL != inner_refs) {
        inner_refs->outer_refs = &visible_refs;
    }
    return resolve_expr_scoped_recur(expr, cur_scope->scope,
                                     &visible_refs,
                                     (NULL != inmost_refs ?
                                      inmost_refs : &visible_refs));
}

int
bitpunch_resolve_expr(struct ast_node_hdl *expr, struct box *scope)
{
    if (NULL != scope) {
        return resolve_expr_scoped_recur(expr, scope, NULL, NULL);
    }
    return resolve_expr_internal(expr, NULL);
}

__attribute__((unused))
static struct ast_node_hdl *
dep_resolver_node_to_ast_node(struct dep_resolver_node *node,
                              struct compile_req *req)
{
    struct dpath_node *dpath;
    struct subscript_index *subscript;

    if (req->compile_cb == compile_node_cb) {
        return container_of(node, struct ast_node_hdl, dr_node);
    }
    if (req->compile_cb == compile_dpath_cb) {
        dpath = container_of(node, struct dpath_node, dr_node);
        return (NULL != dpath->filter ? dpath->filter : dpath->item);
    }
    if (req->compile_cb == compile_field_cb) {
        return container_of(node, struct field, dr_node)->filter;
    }
    assert(req->compile_cb == compile_subscript_index_cb);
    subscript = container_of(node, struct subscript_index, dr_node);
    return subscript->key;
}

static struct parser_location *
dep_resolver_node_get_location(struct dep_resolver_node *node,
                               struct compile_req *req)
{
    struct subscript_index *subscript;

    if (req->compile_cb == compile_node_cb) {
        struct ast_node_hdl *_node;

        _node = container_of(node, struct ast_node_hdl, dr_node);
        return &_node->loc;
    }
    if (req->compile_cb == compile_dpath_cb) {
        struct dpath_node *dpath;

        dpath = container_of(node, struct dpath_node, dr_node);
        return &(NULL != dpath->filter ? dpath->filter : dpath->item)->loc;
    }
    if (req->compile_cb == compile_field_cb) {
        struct field *field;

        field = container_of(node, struct field, dr_node);
        return &field->nstmt.stmt.loc;
    }
    assert(req->compile_cb == compile_subscript_index_cb);
    subscript = container_of(node, struct subscript_index, dr_node);
    return &subscript->key->loc;
}

static const char *
compile_tags_str(enum compile_tag tags)
{
    static char buffer[128];
    int output_started = FALSE;

    buffer[0] = '\0';
    if (0 != (tags & COMPILE_TAG_NODE_TYPE)) {
        strcat(buffer, "type");
        output_started = TRUE;
    }
    if (0 != (tags & COMPILE_TAG_NODE_SPAN_SIZE)) {
        if (output_started) {
            strcat(buffer, "|");
        } else {
            output_started = TRUE;
        }
        strcat(buffer, "size");
    }
    if (0 != (tags & COMPILE_TAG_BROWSE_BACKENDS)) {
        if (output_started) {
            strcat(buffer, "|");
        } else {
            output_started = TRUE;
        }
        strcat(buffer, "backends");
    }
    return buffer;
}

static const char *
compile_func_to_node_family(compile_func_t compile_cb)
{
    if (compile_cb == compile_node_cb) {
        return "node";
    }
    if (compile_cb == compile_dpath_cb) {
        return "dpath";
    }
    if (compile_cb == compile_field_cb) {
        return "field";
    }
    assert(compile_cb == compile_subscript_index_cb);
    return "subscript";
}

#ifdef OUTPUT_DEP_GRAPH
static void
output_dep_graph_link(struct compile_ctx *ctx,
                      const char *node_family,
                      struct ast_node_hdl *node,
                      dep_resolver_tagset_t source_tag,
                      dep_resolver_tagset_t dep_tag,
                      int is_dep,
                      FILE *out)
{
    struct ast_node_hdl *origin_node;
    
    origin_node = dep_resolver_node_to_ast_node(ctx->current_node,
                                                ctx->current_req);
    if (NULL != origin_node) {
        fprintf(out, "\"");
        fdump_ast_dot(origin_node, ctx->current_node_family,
                      source_tag, out);
        fprintf(out, "\" -> \"");
        fdump_ast_dot(node, node_family, dep_tag, out);
        fprintf(out, "\"%s;\n", is_dep ? "" : "[style=dotted]");
    }
}

static void
output_dep_graph_links(struct compile_ctx *ctx,
                       dep_resolver_status_t ret,
                       const char *node_family,
                       struct ast_node_hdl *node,
                       dep_resolver_tagset_t tags_pre,
                       dep_resolver_tagset_t tags_post,
                       FILE *out)
{
    if (NULL != ctx->current_req) {
        dep_resolver_tagset_t source_tag;
        dep_resolver_tagset_t dep_tag;

        for (source_tag = 1;
             source_tag <= ctx->current_tags; source_tag <<= 1) {
            if (0 != (ctx->current_tags & source_tag)) {
                for (dep_tag = 1;
                     dep_tag <= tags_pre || dep_tag <= tags_post;
                     dep_tag <<= 1) {
                    if (0 != (tags_pre & dep_tag)) {
                        output_dep_graph_link(ctx, node_family, node,
                                              source_tag, dep_tag, TRUE,
                                              out);
                    }
                    if (0 != (tags_post & dep_tag)) {
                        output_dep_graph_link(ctx, node_family, node,
                                              source_tag, dep_tag, FALSE,
                                              out);
                    }
                }
            }
        }
    }
}
#endif

int
compile_node(struct ast_node_hdl *node,
             struct compile_ctx *ctx,
             dep_resolver_tagset_t tags_pre,
             dep_resolver_tagset_t tags_post,
             enum resolve_expect_mask expect_mask)
{
    struct compile_req *req;
    dep_resolver_status_t ret;
    
    assert(NULL != node);
    req = compile_req_create(compile_node_cb, ctx, expect_mask);
    ret = dep_resolver_schedule_tags(ctx->dep_resolver, &node->dr_node,
                                     tags_pre, tags_post, req);
#ifdef OUTPUT_DEP_GRAPH
    output_dep_graph_links(ctx, ret, "node", node, tags_pre, tags_post,
                           ctx->deps_dot);
#endif
    return (DEP_RESOLVER_OK == ret ? 0 : -1);
}

int
compile_expr(struct ast_node_hdl *node, struct compile_ctx *ctx,
             int is_dependency)
{
    return compile_expr_tags(node, COMPILE_TAG_NODE_TYPE, ctx, is_dependency);
}

int
compile_expr_tags(struct ast_node_hdl *node,
                  dep_resolver_tagset_t tags,
                  struct compile_ctx *ctx,
                  int is_dependency)
{
    return compile_node(node, ctx,
                        (is_dependency ? tags : 0u),
                        (is_dependency ? 0u : tags),
                        RESOLVE_EXPECT_TYPE |
                        RESOLVE_EXPECT_EXPRESSION);
}

int
compile_dpath(struct dpath_node *node,
              struct compile_ctx *ctx,
              dep_resolver_tagset_t tags_pre,
              dep_resolver_tagset_t tags_post)
{
    struct compile_req *req;
    dep_resolver_status_t ret;

    req = compile_req_create(compile_dpath_cb, ctx, 0);
    ret = dep_resolver_schedule_tags(ctx->dep_resolver, &node->dr_node,
                                      tags_pre, tags_post, req);
#ifdef OUTPUT_DEP_GRAPH
    output_dep_graph_links(ctx, ret, "dpath",
                           (NULL != node->filter ? node->filter : node->item),
                           tags_pre, tags_post, ctx->deps_dot);
#endif
    return (DEP_RESOLVER_OK == ret ? 0 : -1);
}

int
compile_continue(struct compile_ctx *ctx)
{
    if (NULL == ctx) {
        return TRUE;
    }
    return DEP_RESOLVER_OK == dep_resolver_get_status(ctx->dep_resolver);
}

static void
process_compile_res(dep_resolver_status_t ret, struct compile_ctx *ctx)
{
    switch (ret) {
    case DEP_RESOLVER_CIRCULAR_DEPENDENCY: {
        struct dep_resolver_node_entry **circ_entries;
        struct dep_resolver_node_entry *entry;
        struct compile_req *req;
        const struct parser_location *loc;
        int i;

        circ_entries = dep_resolver_get_circular_dependency(ctx->dep_resolver);
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "circular dependency across the following nodes:");
        for (i = 0; NULL != (entry = circ_entries[i]); ++i) {
            req = (struct compile_req *)entry->arg;
            loc = dep_resolver_node_get_location(entry->node, req);
            semantic_error(SEMANTIC_LOGLEVEL_INFO, loc,
                           "[node] %s, [tags] (%s)",
                           ast_node_type_str(
                               dep_resolver_node_to_ast_node(entry->node, req)
                               ->ndat->type),
                           compile_tags_str(entry->tags));
        }
        free(circ_entries);
        break ;
    }
    case DEP_RESOLVER_ERROR: {
        struct dep_resolver_node_entry *entry;
        struct compile_req *req;
        const struct parser_location *loc;

        entry = dep_resolver_get_error_entry(ctx->dep_resolver);
        req = (struct compile_req *)entry->arg;
        loc = dep_resolver_node_get_location(entry->node, req);
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, loc,
                       "compile error");
        break ;
    }
    case DEP_RESOLVER_OK:
    default:
        break ;
    }
}

static int
compile_ast_node_all(struct ast_node_hdl *ast_root,
                     enum resolve_expect_mask expect_mask)
{
    struct compile_ctx ctx;
    dep_resolver_status_t ret;

    compile_ctx_init(&ctx);
    compile_node(ast_root, &ctx,
                 COMPILE_TAG_NODE_TYPE |
                 COMPILE_TAG_NODE_SPAN_SIZE |
                 COMPILE_TAG_BROWSE_BACKENDS, 0u, expect_mask);
#ifdef OUTPUT_DEP_GRAPH
    ctx.deps_dot = fopen("deps.dot", "w");
    assert(NULL != ctx.deps_dot);
    fprintf(ctx.deps_dot, "digraph ast_deps {\n");
#endif
    ret = dep_resolver_resolve(ctx.dep_resolver);
#ifdef OUTPUT_DEP_GRAPH
    fprintf(ctx.deps_dot, "}\n");
    (void)fclose(ctx.deps_dot);
    ctx.deps_dot = NULL;
#endif
    process_compile_res(ret, &ctx);

    compile_ctx_destroy(&ctx);
    return DEP_RESOLVER_OK == ret ? 0 : -1;
}

static int
compile_stmt_list_generic(const struct statement_list *stmt_list,
                          dep_resolver_tagset_t tags,
                          struct compile_ctx *ctx)
{
    struct statement *stmt;

    TAILQ_FOREACH(stmt, stmt_list, list) {
        if (NULL != stmt->cond) {
            compile_expr_tags(stmt->cond, tags, ctx, FALSE);
        }
    }
    return 0;
}

static dep_resolver_tagset_t
compile_field_cb(struct compile_ctx *ctx,
                 enum resolve_expect_mask expect_mask,
                 struct dep_resolver_node *_node,
                 dep_resolver_tagset_t tags,
                 void *arg)
{
    struct field *field;
    struct ast_node_hdl_array field_items;
    struct ast_node_hdl *field_item;
    bitpunch_status_t bt_ret;

    field = container_of(_node, struct field, dr_node);

    if (-1 == compile_node(field->filter, ctx,
                           tags, 0u, RESOLVE_EXPECT_TYPE)) {
        return 0u;
    }
    // XXX support named expr polymorphism
    if (0 != (tags & COMPILE_TAG_NODE_TYPE) && NULL == field->nstmt.name) {
        const struct ast_node_hdl *as_type;

        // Unnamed fields allow direct child field and named
        // expression access without using the dot operator (like with
        // inheritance). If the unnamed field does not refer to a
        // filter or if the filter has no field nor named expression,
        // it becomes a hidden field (takes up space but not exposed
        // to API).

        // XXX only set flag if all polymorphic named expr targets are
        // hidden fields
        as_type = ast_node_get_named_expr_target(
            ast_node_get_as_type(field->filter));
        if (ast_node_is_rexpr_filter(as_type)) {
            const struct scope_def *scope_def;

            scope_def = filter_get_const_scope_def(as_type);
            if (TAILQ_EMPTY(scope_def->block_stmt_list.field_list) &&
                TAILQ_EMPTY(scope_def->block_stmt_list.named_expr_list)) {
                field->nstmt.stmt.stmt_flags |= FIELD_FLAG_HIDDEN;
            }
        } else {
            field->nstmt.stmt.stmt_flags |= FIELD_FLAG_HIDDEN;
        }
    }
    if (NULL != field->nstmt.stmt.cond) {
        bt_ret = ast_node_filter_get_items(field->filter, &field_items);
        if (BITPUNCH_OK != bt_ret) {
            return -1;
        }
        field_item = ARRAY_ITEM(&field_items, 0);
        ast_node_hdl_array_destroy(&field_items);
        if (0 != (field_item->ndat->u.item.flags & ITEMFLAG_SPREADS_SLACK)) {
            field_item->ndat->u.item.flags &= ~ITEMFLAG_SPREADS_SLACK;
            field_item->ndat->u.item.flags |= ITEMFLAG_CONDITIONALLY_SPREADS_SLACK;
        }
        if (0 != (field_item->ndat->u.item.flags & ITEMFLAG_FILLS_SLACK)) {
            field_item->ndat->u.item.flags &= ~ITEMFLAG_FILLS_SLACK;
            field_item->ndat->u.item.flags |= ITEMFLAG_CONDITIONALLY_FILLS_SLACK;
        }
    }
    return tags;
}

int
compile_field(struct field *field,
              struct compile_ctx *ctx,
              dep_resolver_tagset_t tags_pre,
              dep_resolver_tagset_t tags_post)
{
    struct compile_req *req;
    dep_resolver_status_t ret;

    req = compile_req_create(compile_field_cb, ctx, 0);
    ret = dep_resolver_schedule_tags(ctx->dep_resolver,
                                     &field->dr_node,
                                     tags_pre, tags_post, req);
#ifdef OUTPUT_DEP_GRAPH
    output_dep_graph_links(ctx, ret, "field", field->dpath.item,
                           tags_pre, tags_post, ctx->deps_dot);
#endif
    return (DEP_RESOLVER_OK == ret ? 0 : -1);
}

int
compile_attributes(const struct statement_list *attribute_list,
                   dep_resolver_tagset_t tags_pre,
                   dep_resolver_tagset_t tags_post,
                   struct compile_ctx *ctx)
{
    struct named_expr *attr;

    STATEMENT_FOREACH(named_expr, attr, attribute_list, list) {
        compile_node(attr->expr, ctx, tags_pre, tags_post,
                     RESOLVE_EXPECT_TYPE |
                     RESOLVE_EXPECT_EXPRESSION);
    }
    if (!compile_continue(ctx)) {
        return -1;
    }
    return 0;
}

int
compile_fields(const struct statement_list *field_list,
               dep_resolver_tagset_t tags_pre,
               dep_resolver_tagset_t tags_post,
               struct compile_ctx *ctx)
{
    struct field *field;

    STATEMENT_FOREACH(field, field, field_list, list) {
        compile_field(field, ctx, tags_pre, tags_post);
    }
    if (!compile_continue(ctx)) {
        return -1;
    }
    return 0;
}

int
compile_named_exprs(const struct statement_list *named_expr_list,
                    dep_resolver_tagset_t tags_pre,
                    dep_resolver_tagset_t tags_post,
                    struct compile_ctx *ctx)
{
    struct named_expr *named_expr;

    STATEMENT_FOREACH(named_expr, named_expr, named_expr_list, list) {
        compile_node(named_expr->expr, ctx,
                     tags_pre, tags_post,
                     RESOLVE_EXPECT_TYPE |
                     RESOLVE_EXPECT_EXPRESSION);
    }
    if (!compile_continue(ctx)) {
        return -1;
    }
    return 0;
}

int
compile_stmt_lists(const struct block_stmt_list *stmt_lists,
                   dep_resolver_tagset_t tags,
                   struct compile_ctx *ctx)
{
    compile_stmt_list_generic(stmt_lists->named_expr_list, tags, ctx);
    compile_stmt_list_generic(stmt_lists->field_list, tags, ctx);
    compile_stmt_list_generic(stmt_lists->attribute_list, tags, ctx);

    compile_attributes(stmt_lists->attribute_list, 0u, tags, ctx);
    compile_fields(stmt_lists->field_list, 0u, tags, ctx);
    compile_named_exprs(stmt_lists->named_expr_list, 0u, tags, ctx);

    if (!compile_continue(ctx)) {
        return -1;
    }
    return 0;
}

static int
compile_filter_def_validate_attributes(struct ast_node_hdl *filter,
                                       struct filter_class *filter_cls,
                                       struct compile_ctx *ctx)
{
    const struct block_stmt_list *stmt_lists;
    struct named_expr *attr;
    const struct filter_attr_def *attr_def;
    int sem_error = FALSE;

    stmt_lists = &filter->ndat->u.scope_def.block_stmt_list;

    // we need attribute types to be compiled to check their types
    if (-1 == compile_attributes(stmt_lists->attribute_list,
                                 COMPILE_TAG_NODE_TYPE, 0u, ctx)) {
        return -1;
    }

    STATEMENT_FOREACH(named_expr, attr, stmt_lists->attribute_list, list) {
        attr_def = filter_class_get_attr(filter_cls, attr->nstmt.name);
        if (NULL == attr_def) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &attr->nstmt.stmt.loc,
                "no such attribute \"%s\" for filter \"%s\"",
                attr->nstmt.name, filter_cls->name);
            sem_error = TRUE;
            continue ;
        }
        if (attr_def->value_type_mask != EXPR_VALUE_TYPE_UNSET
            && 0 == (attr_def->value_type_mask
                     & attr->expr->ndat->u.rexpr.value_type_mask)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &attr->expr->loc,
                "attribute \"%s\" passed to filter \"%s\" has "
                "an incompatible value-type '%s', acceptable "
                "value-types are '%s'",
                attr_def->name, filter_cls->name,
                expr_value_type_str(
                    attr->expr->ndat->u.rexpr.value_type_mask),
                expr_value_type_str(attr_def->value_type_mask));
            sem_error = TRUE;
            continue ;
        }
    }
    STAILQ_FOREACH(attr_def, &filter_cls->attr_list, list) {
        if (0 != (FILTER_ATTR_MANDATORY & attr_def->flags)) {
            if (!identifier_is_visible_in_block_stmt_lists(
                    STATEMENT_TYPE_ATTRIBUTE, attr_def->name,
                    stmt_lists)) {
                semantic_error(
                    SEMANTIC_LOGLEVEL_ERROR, &filter->loc,
                    "missing mandatory attribute \"%s\"",
                    attr_def->name);
                sem_error = TRUE;
            }
        }
    }
    if (sem_error) {
        return -1;
    }
    return 0;
}

static int
compile_scope_def(struct ast_node_hdl *expr,
                  dep_resolver_tagset_t tags,
                  struct compile_ctx *ctx)
{
    struct block_stmt_list *stmt_lists;

    stmt_lists = &expr->ndat->u.scope_def.block_stmt_list;
    return compile_stmt_lists(stmt_lists, tags, ctx);
}

static int
compile_filter_def(
    struct ast_node_hdl *filter,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx,
    enum resolve_expect_mask expect_mask)
{
    struct filter_class *filter_cls;

    filter_cls = filter->ndat->u.filter_def.filter_cls;
    if (-1 == compile_filter_def_validate_attributes(filter, filter_cls, ctx)) {
        return -1;
    }
    return filter_instance_build(filter, filter_cls,
                                 &filter->ndat->u.filter_def);
}

static int
compile_rexpr_filter_span_size(struct ast_node_hdl *expr,
                               struct compile_ctx *ctx)
{
    struct filter_instance *f_instance;

    f_instance = expr->ndat->u.rexpr_filter.f_instance;
    if (SPAN_SIZE_UNDEF == expr->ndat->u.item.min_span_size) {
        expr->ndat->u.item.min_span_size = 0;
        expr->ndat->u.item.flags |= (ITEMFLAG_IS_SPAN_SIZE_VARIABLE |
                                     ITEMFLAG_IS_USED_SIZE_VARIABLE);
    }
    if (NULL == f_instance->b_item.compute_item_size
        && NULL == f_instance->b_item.compute_item_size_from_buffer) {
        expr->ndat->u.item.flags |= (ITEMFLAG_USES_SLACK |
                                     ITEMFLAG_SPREADS_SLACK |
                                     ITEMFLAG_FILLS_SLACK);
    }
    return 0;
}

int
compile_rexpr_filter(struct ast_node_hdl *expr,
                     dep_resolver_tagset_t tags,
                     struct compile_ctx *ctx)
{
    struct block_stmt_list *stmt_lists;
    const struct filter_class *filter_cls;

    // Make sure we defer compilation of all filter's statements and
    // expressions. Filter implementations may request immediate
    // compilation of the subset needed for their own compilation
    // stages.
    stmt_lists = &filter_get_scope_def(expr)->block_stmt_list;
    if (-1 == compile_stmt_lists(stmt_lists, tags, ctx)) {
        return -1;
    }
    filter_cls = expr->ndat->u.rexpr_filter.filter_cls;
    if (NULL != filter_cls->filter_instance_compile_func) {
        return filter_cls->filter_instance_compile_func(
            expr, expr->ndat->u.rexpr_filter.f_instance, tags, ctx);
    } else {
        if (0 != (tags & COMPILE_TAG_NODE_SPAN_SIZE)) {
            compile_rexpr_filter_span_size(expr, ctx);
        }
        if (0 != (tags & COMPILE_TAG_BROWSE_BACKENDS)) {
            compile_node_backends__filter_generic(expr);
        }
    }
    return 0;
}

static int
compile_expr_native_internal(struct ast_node_hdl *node,
                             expr_value_t value)
{
    struct ast_node_data *compiled_type;

    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_NATIVE;
    compiled_type->u.rexpr.value_type_mask = value.type;
    compiled_type->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_NONE;
    compiled_type->u.rexpr_native.value = value;
    node->ndat = compiled_type;
    return 0;
}

static int
compile_expr_integer(
    struct ast_node_hdl *node,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx,
    enum resolve_expect_mask expect_mask)
{
    return compile_expr_native_internal(
        node, expr_value_as_integer(node->ndat->u.integer));
}

static int
compile_expr_boolean(
    struct ast_node_hdl *node,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx,
    enum resolve_expect_mask expect_mask)
{
    return compile_expr_native_internal(
        node, expr_value_as_boolean(node->ndat->u.boolean));
}

static int
compile_expr_string_literal(
    struct ast_node_hdl *node,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx,
    enum resolve_expect_mask expect_mask)
{
    return compile_expr_native_internal(
        node, expr_value_as_string_len(node->ndat->u.string.str,
                                       node->ndat->u.string.len));
}

struct ast_node_hdl *
ast_node_new_rexpr_native(expr_value_t value)
{
    struct ast_node_hdl *expr;

    expr = ast_node_hdl_new();
    expr->ndat = new_safe(struct ast_node_data);
    (void)compile_expr_native_internal(expr, value);
    return expr;
}

/**
 * @brief compile operator expression
 *
 * Its jobs:
 *
 * - find a match for the operator type along with its operand types,
 *   or raise an error if no match exists
 *
 * - prune pre-computable branches - in particular, prune sizeof
 *   operator operand: this allows to use sizeof on type names that
 *   have a static size but defined using expressions (typically,
 *   array size expressions that end up being a static, pre-computable
 *   value)
 */
static int
compile_expr_operator(
    struct ast_node_hdl *expr,
    int n_operands,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx)
{
    struct ast_node_data *resolved_type;
    int opd_i;

    for (opd_i = 0; opd_i < n_operands; ++opd_i) {
        compile_expr_tags(expr->ndat->u.op.operands[opd_i], tags, ctx, TRUE);
    }
    if (!compile_continue(ctx)) {
        return -1;
    }
    resolved_type = new_safe(struct ast_node_data);
    resolved_type->type = op_type_ast2rexpr(expr->ndat->type);
    resolved_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_UNSET;
    resolved_type->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_NONE;
    resolved_type->u.rexpr_op.op = expr->ndat->u.op;
    expr->ndat = resolved_type;
    return 0;
}

static int
compile_rexpr_operator_type(
    struct ast_node_hdl *expr,
    int n_operands,
    struct compile_ctx *ctx)
{
    int opd_i;
    struct ast_node_hdl *operand;
    enum expr_value_type opd_types[2]; /* max # operands is 2 */
    int only_native_operands;
    const struct expr_evaluator *evaluator;

    only_native_operands = TRUE;
    for (opd_i = 0; opd_i < n_operands; ++opd_i) {
        operand = ast_node_get_named_expr_target(
            expr->ndat->u.rexpr_op.op.operands[opd_i]);
        opd_types[opd_i] = operand->ndat->u.rexpr.value_type_mask;
        if (EXPR_VALUE_TYPE_UNSET == opd_types[opd_i]) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &operand->loc,
                           "operand has no value type");
            return -1;
        }
        if (AST_NODE_TYPE_REXPR_NATIVE != operand->ndat->type) {
            only_native_operands = FALSE;
        }
    }
    evaluator = expr_lookup_evaluator(expr->ndat->type, opd_types);
    if (NULL == evaluator) {
        switch (n_operands) {
        case 1:
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "no match for %s with operand of type '%s'",
                ast_node_type_str(expr->ndat->type),
                expr_value_type_str(opd_types[0]));
            break ;
        case 2:
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "no match for %s with operands of type '%s' and '%s'",
                ast_node_type_str(expr->ndat->type),
                expr_value_type_str(opd_types[0]),
                expr_value_type_str(opd_types[1]));
            break ;
        default:
            break ;
        }
        return -1;
    }
    expr->ndat->u.rexpr_op.evaluator = evaluator;
    expr->ndat->u.rexpr.value_type_mask = evaluator->res_type_mask;
    expr->ndat->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_NONE;

    if (only_native_operands) {
        struct ast_node_hdl *operand;
        expr_value_t operand_values[2];
        const struct expr_evaluator *evaluator;
        expr_value_t eval_value;

        for (opd_i = 0; opd_i < n_operands; ++opd_i) {
            operand = ast_node_get_named_expr_target(
                expr->ndat->u.rexpr_op.op.operands[opd_i]);
            assert(AST_NODE_TYPE_REXPR_NATIVE == operand->ndat->type);
            operand_values[opd_i] = operand->ndat->u.rexpr_native.value;
        }
        evaluator = expr->ndat->u.rexpr_op.evaluator;
        assert(NULL != evaluator);
        eval_value = evaluator->eval_fn(operand_values);

        return compile_expr_native_internal(expr, eval_value);
    }
    return 0;
}

static int
compile_rexpr_operator(
    struct ast_node_hdl *expr,
    int n_operands,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx)
{
    int opd_i;

    for (opd_i = 0; opd_i < n_operands; ++opd_i) {
        compile_node(expr->ndat->u.rexpr_op.op.operands[opd_i], ctx,
                     0u, tags, RESOLVE_EXPECT_EXPRESSION);
    }
    if (!compile_continue(ctx)) {
        return -1;
    }
    if (0 != (tags & COMPILE_TAG_NODE_TYPE)
        && -1 == compile_rexpr_operator_type(expr, n_operands, ctx)) {
        return -1;
    }
    return 0;
}

static int
compile_expr_operator_filter(
    struct ast_node_hdl *node,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx)
{
    struct ast_node_hdl *target;
    struct ast_node_hdl *filter;
    struct ast_node_data *compiled_type;

    target = node->ndat->u.op.operands[0];
    filter = node->ndat->u.op.operands[1];

    compile_node(target, ctx,
                 COMPILE_TAG_NODE_TYPE, 0u,
                 RESOLVE_EXPECT_TYPE |
                 RESOLVE_EXPECT_DPATH_EXPRESSION);
    compile_node(filter, ctx,
                 COMPILE_TAG_NODE_TYPE, 0u,
                 RESOLVE_EXPECT_TYPE);
    if (!compile_continue(ctx)) {
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_FILTER;
    compiled_type->u.rexpr.value_type_mask =
        filter->ndat->u.rexpr.value_type_mask;
    compiled_type->u.rexpr.dpath_type_mask = (EXPR_DPATH_TYPE_ITEM |
                                              EXPR_DPATH_TYPE_CONTAINER);
    compiled_type->u.rexpr_op_filter.target = target;
    compiled_type->u.rexpr_op_filter.filter_expr = filter;
    node->ndat = compiled_type;
    return 0;
}

static int
compile_subscript_index_type(struct ast_node_hdl *expr,
                             struct subscript_index *subscript,
                             struct compile_ctx *ctx)
{
    struct ast_node_hdl *key;
    struct ast_node_hdl *twin_idx;
    struct ast_node_hdl *anchor_expr;
    struct ast_node_hdl *anchor_item;
    struct ast_node_hdl *key_expr;

    key = subscript->key;
    assert(ast_node_is_rexpr(key));
    if (0 != (key->ndat->u.rexpr.value_type_mask
              & ~(EXPR_VALUE_TYPE_INTEGER | EXPR_VALUE_TYPE_STRING))) {
        // TODO log incompatible value types
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &key->loc,
                       "invalid expression type in array subscript: "
                       "expect 'integer' or 'string'");
        return 0u;
    }
    if (NULL != subscript->twin) {
        twin_idx = subscript->twin;
        assert(ast_node_is_rexpr(twin_idx));
        if (0 == (twin_idx->ndat->u.rexpr.value_type_mask
                  & EXPR_VALUE_TYPE_INTEGER)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &twin_idx->loc,
                "invalid expression type in array subscript: "
                "twin index must be of type 'integer', not '%s'",
                expr_value_type_str(twin_idx->ndat->u.rexpr.value_type_mask));
            return 0u;
        }
        if (AST_NODE_TYPE_REXPR_NATIVE == twin_idx->ndat->type
            && EXPR_VALUE_TYPE_INTEGER
            == twin_idx->ndat->u.rexpr.value_type_mask
            && twin_idx->ndat->u.rexpr_native.value.integer < 0) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &twin_idx->loc,
                "array integer twin index cannot be negative");
            return 0u;
        }
    }
    anchor_expr = expr->ndat->u.rexpr_op_subscript_common.anchor_expr;
    anchor_item = ast_node_get_as_type(anchor_expr);
    if (NULL != anchor_item) {
        if (-1 == compile_node(anchor_item, ctx,
                               COMPILE_TAG_NODE_TYPE, 0u,
                               RESOLVE_EXPECT_TYPE)) {
            return 0u;
        }
        if (ast_node_is_indexed(anchor_item)) {
            /* integer subscript accesses items by raw index; check
             * that other potential value types are compatible with
             * the index type */
            if (0 == (EXPR_VALUE_TYPE_INTEGER
                      & key->ndat->u.rexpr.value_type_mask)) {
                key_expr = ast_node_get_key_expr(anchor_item);
                if (-1 == compile_expr(key_expr, ctx, TRUE)) {
                    return 0u;
                }
                assert(ast_node_is_rexpr(key));
                assert(ast_node_is_rexpr(key_expr));
                if (0 == (key->ndat->u.rexpr.value_type_mask
                          & key_expr->ndat->u.rexpr.value_type_mask)) {
                    semantic_error(
                        SEMANTIC_LOGLEVEL_ERROR, &key->loc,
                        "invalid expression type in array subscript: "
                        "type mismatch between subscript type '%s' and "
                        "index type '%s'",
                        expr_value_type_str(
                            key->ndat->u.rexpr.value_type_mask),
                        expr_value_type_str(
                            key_expr->ndat->u.rexpr.value_type_mask));
                    return 0u;
                }
            }
        } else if (0 == (EXPR_VALUE_TYPE_INTEGER
                         & key->ndat->u.rexpr.value_type_mask)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &key->loc,
                "invalid expression type in array subscript: non-integer "
                "type requires array element type to be indexed");
            return -1;
        }
    }
    return 0;
}

static dep_resolver_tagset_t
compile_subscript_index_cb(struct compile_ctx *ctx,
                           enum resolve_expect_mask expect_mask,
                           struct dep_resolver_node *_node,
                           dep_resolver_tagset_t tags,
                           void *arg)
{
    struct ast_node_hdl *expr = arg;
    struct subscript_index *subscript;

    assert(AST_NODE_TYPE_REXPR_OP_SUBSCRIPT == expr->ndat->type ||
           AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE == expr->ndat->type);
    subscript = container_of(_node, struct subscript_index, dr_node);
    if (NULL == subscript->key) {
        return tags;
    }
    compile_expr_tags(subscript->key, tags, ctx, TRUE);
    if (NULL != subscript->twin) {
        compile_expr_tags(subscript->twin, tags, ctx, TRUE);
    }
    if (!compile_continue(ctx)) {
        return 0u;
    }
    if (0 != (tags & COMPILE_TAG_NODE_TYPE)
        && -1 == compile_subscript_index_type(expr, subscript, ctx)) {
        return 0u;
    }
    return tags;
}

static int
compile_subscript_index(struct ast_node_hdl *expr,
                        struct subscript_index *subscript,
                        dep_resolver_tagset_t tags,
                        struct compile_ctx *ctx)
{
    struct compile_req *req;
    dep_resolver_status_t ret;
    
    assert(NULL != expr);
    req = compile_req_create(compile_subscript_index_cb, ctx,
                             RESOLVE_EXPECT_EXPRESSION);
    req->arg = expr;
    ret = dep_resolver_schedule_tags(ctx->dep_resolver, &subscript->dr_node,
                                     0u, tags, req);
#ifdef OUTPUT_DEP_GRAPH
    if (NULL != subscript->key) {
        output_dep_graph_links(ctx, ret, "subscript", subscript->key,
                               tags, 0u, ctx->deps_dot);
    }
#endif
    return (DEP_RESOLVER_OK == ret ? 0 : -1);
}

static int
compile_expr_operator_subscript(
    struct ast_node_hdl *node,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx,
    enum resolve_expect_mask expect_mask)
{
    struct filter_instance_array *array;
    struct ast_node_hdl *anchor_expr;
    struct ast_node_data *compiled_type;
    struct subscript_index *index;
    const struct ast_node_hdl *anchor_item;
    enum expr_value_type value_type_mask = EXPR_VALUE_TYPE_UNSET;
    
    anchor_expr = node->ndat->u.op_subscript_common.anchor_expr;
    if (-1 == compile_node(anchor_expr, ctx, COMPILE_TAG_NODE_TYPE, 0u,
                           expect_mask)) {
        return -1;
    }
    index = &node->ndat->u.op_subscript.index;
    if (NULL == index->key) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                       "array subscript index cannot be empty");
        return -1;
    }
    assert(NULL != anchor_expr);
    if (-1 == compile_expr(anchor_expr, ctx, TRUE)) {
        return -1;
    }
    anchor_item = ast_node_get_as_type(anchor_expr);
    if (NULL != anchor_item) {
        switch (anchor_item->ndat->type) {
        case AST_NODE_TYPE_ARRAY:
        case AST_NODE_TYPE_ARRAY_SLICE:
            array = (struct filter_instance_array *)
                anchor_item->ndat->u.rexpr_filter.f_instance;
            if (-1 == compile_node(array->item_type,
                                   ctx, COMPILE_TAG_NODE_TYPE, 0u,
                                   RESOLVE_EXPECT_TYPE)) {
                return -1;
            }
            value_type_mask = expr_value_type_mask_from_node(array->item_type);
            break ;
        case AST_NODE_TYPE_BYTE_ARRAY:
        case AST_NODE_TYPE_BYTE_SLICE:
            value_type_mask = EXPR_VALUE_TYPE_BYTES;
            break ;
        default:
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                "invalid use of subscript operator on non-subscriptable "
                "path of type '%s'",
                ast_node_type_str(anchor_item->ndat->type));
            return -1;
        }
    }
    if (-1 == compile_subscript_index(node, index, tags, ctx)) {
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_SUBSCRIPT;
    compiled_type->u.rexpr.value_type_mask = value_type_mask;
    compiled_type->u.rexpr.dpath_type_mask = (EXPR_DPATH_TYPE_ITEM |
                                              EXPR_DPATH_TYPE_CONTAINER);
    assert(ast_node_is_rexpr(anchor_expr));
    compiled_type->u.rexpr_op_subscript_common.anchor_expr = anchor_expr;
    compiled_type->u.rexpr_op_subscript.index = *index;
    index = &compiled_type->u.op_subscript.index;
    node->ndat = compiled_type;
    return 0;
}

static int
compile_rexpr_operator_subscript(
    struct ast_node_hdl *node,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx)
{
    struct ast_node_hdl *anchor_expr;
    struct subscript_index *index;

    anchor_expr = node->ndat->u.rexpr_op_subscript_common.anchor_expr;
    index = &node->ndat->u.rexpr_op_subscript.index;

    compile_node(anchor_expr, ctx, tags, 0u, RESOLVE_EXPECT_DPATH_EXPRESSION);
    compile_subscript_index(node, index, tags, ctx);
    if (!compile_continue(ctx)) {
        return -1;
    }
    return 0;
}

static int
compile_expr_operator_subscript_slice(
    struct ast_node_hdl *node,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx,
    enum resolve_expect_mask expect_mask)
{
    struct ast_node_hdl *anchor_expr;
    struct ast_node_data *compiled_type;
    struct subscript_index *slice_start;
    struct subscript_index *slice_end;
    enum expr_value_type value_type_mask = EXPR_VALUE_TYPE_UNSET;

    anchor_expr = node->ndat->u.op_subscript_common.anchor_expr;
    slice_start = &node->ndat->u.op_subscript_slice.start;
    slice_end = &node->ndat->u.op_subscript_slice.end;

    assert(NULL != anchor_expr);

    if (-1 == compile_expr(anchor_expr, ctx, TRUE)) {
        return -1;
    }
    if (anchor_expr->ndat->u.rexpr.dpath_type_mask == EXPR_DPATH_TYPE_NONE) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &node->loc,
            "invalid use of subscript operator on value-type expression");
        return -1;
    }
    value_type_mask = anchor_expr->ndat->u.rexpr.value_type_mask;
    if (-1 == compile_subscript_index(node, slice_start, tags, ctx) ||
        -1 == compile_subscript_index(node, slice_end, tags, ctx)) {
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE;
    compiled_type->u.rexpr.value_type_mask = value_type_mask;
    compiled_type->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_CONTAINER;
    assert(ast_node_is_rexpr(anchor_expr));
    compiled_type->u.rexpr_op_subscript_common.anchor_expr = anchor_expr;
    compiled_type->u.rexpr_op_subscript_slice.start = *slice_start;
    compiled_type->u.rexpr_op_subscript_slice.end = *slice_end;
    node->ndat = compiled_type;
    return 0;
}

static int
compile_rexpr_operator_subscript_slice(
    struct ast_node_hdl *node,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx)
{
    struct ast_node_hdl *anchor_expr;
    struct subscript_index *slice_start;
    struct subscript_index *slice_end;

    anchor_expr = node->ndat->u.rexpr_op_subscript_common.anchor_expr;
    slice_start = &node->ndat->u.rexpr_op_subscript_slice.start;
    slice_end = &node->ndat->u.rexpr_op_subscript_slice.end;

    compile_node(anchor_expr, ctx, tags, 0u, RESOLVE_EXPECT_DPATH_EXPRESSION);
    compile_subscript_index(node, slice_start, tags, ctx);
    compile_subscript_index(node, slice_end, tags, ctx);
    if (!compile_continue(ctx)) {
        return -1;
    }
    return 0;
}

static struct filter_instance *
operator_filter_filter_instance_build(struct ast_node_hdl *item)
{
    struct filter_instance *filter;
    struct item_backend *b_item;

    filter = new_safe(struct filter_instance);
    b_item = &filter->b_item;
    memset(b_item, 0, sizeof (*b_item));

    return filter;
}

static void
compile_node_backends__operator_filter(struct ast_node_hdl *item)
{
    assert(NULL == item->ndat->u.rexpr_filter.f_instance);
    item->ndat->u.rexpr_filter.f_instance =
        operator_filter_filter_instance_build(item);
}

static int
compile_rexpr_op_filter(struct ast_node_hdl *filter,
                        dep_resolver_tagset_t tags,
                        struct compile_ctx *ctx)
{
    struct ast_node_hdl *target;
    struct ast_node_hdl *filter_expr;

    target = filter->ndat->u.rexpr_op_filter.target;
    filter_expr = filter->ndat->u.rexpr_op_filter.filter_expr;
    compile_node(target, ctx, tags, 0u,
                 RESOLVE_EXPECT_TYPE |
                 RESOLVE_EXPECT_DPATH_EXPRESSION);
    compile_node(filter_expr, ctx, tags, 0u,
                 RESOLVE_EXPECT_TYPE);
    if (!compile_continue(ctx)) {
        return -1;
    }
    if (0 != (tags & COMPILE_TAG_BROWSE_BACKENDS)) {
        compile_node_backends__operator_filter(filter);
    }
    return 0;
}

static int
compile_expr_operator_fcall(
    struct ast_node_hdl *expr,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx,
    enum resolve_expect_mask expect_mask)
{
    const struct expr_builtin_fn *builtin;
    struct statement *stmt;
    int n_params;
    struct statement_list *func_params;
    struct ast_node_data *compiled_type;
    struct named_expr *param;

    builtin = expr->ndat->u.op_fcall.func->ndat->u.rexpr_builtin.builtin;
    func_params = expr->ndat->u.op_fcall.func_params;

    n_params = 0;
    TAILQ_FOREACH(stmt, expr->ndat->u.op_fcall.func_params, list) {
        ++n_params;
    }
    if (n_params < builtin->min_n_params) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "not enough parameters for built-in function '%s': "
            "expects %s%d, got %d",
            builtin->builtin_name,
            builtin->max_n_params > builtin->min_n_params ? "at least " : "",
            builtin->min_n_params, n_params);
        return -1;
    }
    if (n_params > builtin->max_n_params) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "too many parameters for built-in function '%s': "
            "expects %s%d, got %d",
            builtin->builtin_name,
            builtin->max_n_params > builtin->min_n_params ? "at most " : "",
            builtin->max_n_params, n_params);
        return -1;
    }

    /* compile parameter list */
    TAILQ_FOREACH(stmt, func_params, list) {
        param = (struct named_expr *)stmt;
        compile_expr(param->expr, ctx, FALSE);
    }
    if (!compile_continue(ctx)) {
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_FCALL;
    compiled_type->u.rexpr.value_type_mask = builtin->res_value_type_mask;
    compiled_type->u.rexpr.dpath_type_mask = builtin->res_dpath_type_mask;
    compiled_type->u.rexpr_op_fcall.builtin = builtin;
    compiled_type->u.rexpr_op_fcall.func_params = func_params;
    compiled_type->u.rexpr_op_fcall.n_func_params = n_params;
    expr->ndat = compiled_type;
    return 0;
}

/**
 * @brief compile sizeof operator
 */
static int
compile_expr_operator_sizeof(
    struct ast_node_hdl *expr,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx,
    enum resolve_expect_mask expect_mask)
{
    struct ast_node_data *compiled_type;
    struct op op;
    struct ast_node_hdl *target;

    op = expr->ndat->u.op;
    if (-1 == compile_node(op.operands[0], ctx,
                           COMPILE_TAG_NODE_TYPE |
                           COMPILE_TAG_NODE_SPAN_SIZE, 0u,
                           RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    // lookup target item type
    target = ast_node_get_named_expr_target(op.operands[0]);
    while (AST_NODE_TYPE_REXPR_OP_FILTER == target->ndat->type) {
        target = ast_node_get_named_expr_target(
            target->ndat->u.rexpr_op_filter.target);
    }
    if (ast_node_is_filter(target)) {
        if (0 != (target->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "invalid use of sizeof operator on dynamic-sized type\n"
                "(use a dpath expression if size is dynamic)");
            return -1;
        }
        compiled_type = new_safe(struct ast_node_data);
        compiled_type->type = AST_NODE_TYPE_REXPR_NATIVE;
        compiled_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_INTEGER;
        compiled_type->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_NONE;
        compiled_type->u.rexpr_native.value =
            expr_value_as_integer(target->ndat->u.item.min_span_size);
        expr->ndat = compiled_type;
        return 0;
    }
    if (target->ndat->u.rexpr.dpath_type_mask == EXPR_DPATH_TYPE_UNSET ||
        target->ndat->u.rexpr.dpath_type_mask == EXPR_DPATH_TYPE_NONE) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "invalid use of sizeof operator on non-dpath operand");
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_SIZEOF;
    compiled_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_INTEGER;
    compiled_type->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_NONE;
    compiled_type->u.rexpr_op.op = op;
    expr->ndat = compiled_type;
    return 0;
}

/**
 * @brief compile addrof (&) operator
 */
static int
compile_expr_operator_addrof(
    struct ast_node_hdl *expr,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx,
    enum resolve_expect_mask expect_mask)
{
    struct ast_node_data *compiled_type;
    struct op op;

    op = expr->ndat->u.op;
    if (-1 == compile_node(op.operands[0], ctx,
                           COMPILE_TAG_NODE_TYPE, 0u,
                           RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    if (op.operands[0]->ndat->u.rexpr.dpath_type_mask == EXPR_DPATH_TYPE_NONE) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "invalid use of addrof (&) operator on value-type operand");
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_ADDROF;
    compiled_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_INTEGER;
    compiled_type->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_NONE;
    compiled_type->u.rexpr_op.op = op;
    expr->ndat = compiled_type;
    return 0;
}

static int
compile_rexpr_operator_fcall(struct ast_node_hdl *expr,
                             dep_resolver_tagset_t tags,
                             struct compile_ctx *ctx)
{
    struct statement *stmt;
    struct named_expr *param;

    /* resolve expressions in parameter list */
    TAILQ_FOREACH(stmt, expr->ndat->u.rexpr_op_fcall.func_params, list) {
        param = (struct named_expr *)stmt;
        if (-1 == compile_expr_tags(param->expr, tags, ctx, TRUE)) {
            return -1;
        }
    }
    return 0;
}

/**
 * @brief compile ancestor (unary ^) operator
 */
static int
compile_expr_operator_ancestor(
    struct ast_node_hdl *expr,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx,
    enum resolve_expect_mask expect_mask)
{
    struct ast_node_hdl *operand;
    struct ast_node_hdl *target;
    struct ast_node_hdl *target_filter;
    enum expr_value_type value_type_mask;
    struct ast_node_data *compiled_type;

    operand = expr->ndat->u.op.operands[0];
    if (-1 == compile_node(operand, ctx,
                           COMPILE_TAG_NODE_TYPE, 0u,
                           RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    assert(ast_node_is_rexpr(operand));
    target_filter = ast_node_get_target_filter(operand);
    if (NULL != target_filter) {
        if (AST_NODE_TYPE_REXPR_OP_FILTER == target_filter->ndat->type) {
            target = target_filter->ndat->u.rexpr_op_filter.target;
        } else {
            target = target_filter;
        }
    } else {
        target = operand;
    }
    value_type_mask = target->ndat->u.rexpr.value_type_mask;
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_ANCESTOR;
    compiled_type->u.rexpr.value_type_mask = value_type_mask;
    compiled_type->u.rexpr.dpath_type_mask = (EXPR_DPATH_TYPE_ITEM |
                                              EXPR_DPATH_TYPE_CONTAINER);
    compiled_type->u.rexpr_op.op = expr->ndat->u.op;
    expr->ndat = compiled_type;
    return 0;
}

static int
compile_conditional(
    struct ast_node_hdl *cond,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx)
{
    struct ast_node_hdl *cond_expr;

    if (-1 == compile_expr_tags(cond->ndat->u.conditional.cond_expr,
                                tags, ctx, TRUE)) {
        return -1;
    }
    if (NULL != cond->ndat->u.conditional.outer_cond
        && -1 == compile_expr_tags(cond->ndat->u.conditional.outer_cond,
                                   tags, ctx, FALSE)) {
        return -1;
    }
    if (0 != (tags & COMPILE_TAG_NODE_TYPE)) {
        cond_expr = cond->ndat->u.conditional.cond_expr;
        assert(ast_node_is_rexpr(cond_expr));
        if (EXPR_VALUE_TYPE_BOOLEAN
            != cond_expr->ndat->u.rexpr.value_type_mask) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &cond->loc,
                "expect a boolean expression in condition, not '%s'",
                expr_value_type_str(cond_expr->ndat->u.rexpr.value_type_mask));
            return -1;
        }
    }
    return 0;
}

static int
compile_extern_func(
    struct ast_node_hdl *extern_func,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx)
{
    struct ast_node_data *resolved_type;

    if (0 != (tags & COMPILE_TAG_NODE_TYPE)) {
        resolved_type = new_safe(struct ast_node_data);
        resolved_type->type = AST_NODE_TYPE_REXPR_EXTERN_FUNC;
        resolved_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_UNSET;
        resolved_type->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_UNSET;
        resolved_type->u.rexpr_extern_func.extern_func =
            extern_func->ndat->u.extern_func;
        extern_func->ndat = resolved_type;
    }
    return 0;
}

static int
compile_rexpr_named_expr(
    struct ast_node_hdl *expr,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx)
{
    struct ast_node_hdl *anchor_expr;
    struct named_expr *named_expr;
    struct ast_node_hdl *target;
    struct filter_class *filter_cls;

    anchor_expr = expr->ndat->u.rexpr_member_common.anchor_expr;
    if (NULL != anchor_expr
        && -1 == compile_node(anchor_expr, ctx,
                              (tags & COMPILE_TAG_NODE_TYPE),
                              (tags & ~COMPILE_TAG_NODE_TYPE),
                              RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    named_expr = (struct named_expr *)
        expr->ndat->u.rexpr_named_expr.named_expr;
    target = named_expr->expr;
    if (-1 == compile_node(target, ctx, tags, 0u,
                           RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    switch (target->ndat->type) {
    case AST_NODE_TYPE_EXTERN_FILTER:
        filter_cls = target->ndat->u.extern_filter.filter_cls;
        if (-1 == filter_instance_build(
                expr, filter_cls,
                filter_def_create_empty(named_expr->nstmt.name))) {
            return -1;
        }
        break ;
    default:
        if (0 != (tags & COMPILE_TAG_NODE_TYPE)) {
            expr->ndat->u.rexpr.value_type_mask =
                expr_value_type_mask_from_node(target);
            expr->ndat->u.rexpr.dpath_type_mask =
                expr_dpath_type_mask_from_node(target);
        }
        break ;
    }
    return 0;
}

static int
compile_rexpr_polymorphic_type(
    struct ast_node_hdl *expr,
    struct compile_ctx *ctx)
{
    enum expr_value_type value_type_mask;
    enum expr_dpath_type dpath_type_mask;
    int i;
    struct named_statement_spec *stmt_spec;
    struct field *field;
    struct named_expr *named_expr;
    struct ast_node_hdl *target_expr;

    value_type_mask = EXPR_VALUE_TYPE_UNSET;
    dpath_type_mask = EXPR_DPATH_TYPE_UNSET;
    for (i = 0; i < expr->ndat->u.rexpr_polymorphic.n_visible_statements;
         ++i) {
        stmt_spec = &expr->ndat->u.rexpr_polymorphic.visible_statements[i];
        switch (stmt_spec->stmt_type) {
        case STATEMENT_TYPE_NAMED_EXPR:
        case STATEMENT_TYPE_ATTRIBUTE:
            named_expr = (struct named_expr *)stmt_spec->nstmt;
            target_expr = named_expr->expr;
            break ;
        case STATEMENT_TYPE_FIELD:
            field = (struct field *)stmt_spec->nstmt;
            target_expr = field->filter;
            assert(ast_node_is_rexpr(expr));
            break ;
        default:
            assert(0);
        }
        assert(ast_node_is_rexpr(target_expr));
        // possible value and dpath types add up for each polymorphic target
        value_type_mask |= target_expr->ndat->u.rexpr.value_type_mask;
        dpath_type_mask |= target_expr->ndat->u.rexpr.dpath_type_mask;
    }
    if (0 != (expr->flags & ASTFLAG_HAS_POLYMORPHIC_ANCHOR)) {
        expr->ndat->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_ANY;
        expr->ndat->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_ANY;
    } else {
        expr->ndat->u.rexpr.value_type_mask = value_type_mask;
        expr->ndat->u.rexpr.dpath_type_mask = dpath_type_mask;
    }
    return 0;
}

static int
compile_rexpr_polymorphic(
    struct ast_node_hdl *expr,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx)
{
    struct ast_node_hdl *anchor_expr;
    int i;
    struct named_statement_spec *stmt_spec;
    struct field *field;
    struct named_expr *named_expr;

    anchor_expr = expr->ndat->u.rexpr_member_common.anchor_expr;
    if (NULL != anchor_expr
        && -1 == compile_node(anchor_expr, ctx, tags, 0u,
                              RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    // FIXME some circular dependency errors could be legit, rework this
    for (i = 0; i < expr->ndat->u.rexpr_polymorphic.n_visible_statements;
         ++i) {
        stmt_spec = &expr->ndat->u.rexpr_polymorphic.visible_statements[i];
        switch (stmt_spec->stmt_type) {
        case STATEMENT_TYPE_NAMED_EXPR:
        case STATEMENT_TYPE_ATTRIBUTE:
            named_expr = (struct named_expr *)stmt_spec->nstmt;
            compile_node(named_expr->expr, ctx, COMPILE_TAG_NODE_TYPE, 0u,
                         RESOLVE_EXPECT_TYPE |
                         RESOLVE_EXPECT_EXPRESSION);
            break ;
        case STATEMENT_TYPE_FIELD:
            field = (struct field *)stmt_spec->nstmt;
            compile_field(field, ctx, COMPILE_TAG_NODE_TYPE, 0u);
            break ;
        default:
            assert(0);
        }
    }
    if (!compile_continue(ctx)) {
        return -1;
    }
    if (0 != (tags & COMPILE_TAG_NODE_TYPE)
        && -1 == compile_rexpr_polymorphic_type(expr, ctx)) {
        return -1;
    }
    return 0;
}

static int
compile_rexpr_field(
    struct ast_node_hdl *expr,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx)
{
    struct ast_node_hdl *anchor_expr;
    struct field *field;

    anchor_expr = expr->ndat->u.rexpr_member_common.anchor_expr;
    if (NULL != anchor_expr
        && -1 == compile_node(anchor_expr, ctx, tags, 0u,
                              RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    field = (struct field *)expr->ndat->u.rexpr_field.field;

    if (-1 == compile_node(field->filter, ctx, tags, 0u,
                           RESOLVE_EXPECT_TYPE)) {
        return -1;
    }
    if (0 != (tags & COMPILE_TAG_NODE_TYPE)) {
        expr->ndat->u.rexpr.value_type_mask =
            expr_value_type_mask_from_node(field->filter);
        expr->ndat->u.rexpr.dpath_type_mask = (EXPR_DPATH_TYPE_ITEM |
                                               EXPR_DPATH_TYPE_CONTAINER);
    }
    return 0;
}

static int
compile_rexpr_member(
    struct ast_node_hdl *expr,
    int is_scope_operator,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx)
{
    struct op *op;
    struct named_statement_spec *visible_statements;
    int n_visible_statements;
    struct ast_node_hdl *anchor_expr;
    enum statement_type lookup_mask;
    struct ast_node_hdl *anchor_filter, *member;
    struct named_statement_spec *stmt_spec;
    struct ast_node_data *resolved_type;

    op = &expr->ndat->u.rexpr_op.op;
    if (-1 == compile_node(op->operands[0], ctx,
                           (tags & COMPILE_TAG_NODE_TYPE),
                           (tags & ~COMPILE_TAG_NODE_TYPE),
                           RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    if (0 == (tags & COMPILE_TAG_NODE_TYPE)) {
        return 0;
    }
    member = op->operands[1];
    anchor_expr = op->operands[0];
    anchor_filter = ast_node_get_as_type(anchor_expr);
    if (NULL != anchor_filter) {
        if (!ast_node_is_item(anchor_filter) &&
            !ast_node_is_rexpr_filter(anchor_filter)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "invalid use of member operator on non-filter dpath");
            return -1;
        }
        if (is_scope_operator
            && AST_NODE_TYPE_EXPR_SELF == member->ndat->type) {
            resolved_type = new_safe(struct ast_node_data);
            resolved_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_UNSET;
            resolved_type->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_UNSET;
            resolved_type->u.rexpr_member_common.anchor_filter =
                (struct ast_node_hdl *)anchor_filter;
            expr->ndat = resolved_type;
            resolved_type->type = AST_NODE_TYPE_REXPR_SELF;
            return 0;
        }
        /* checked by parser */
        assert(member->ndat->type == AST_NODE_TYPE_IDENTIFIER);
        if (member->ndat->u.identifier[0] == '@') {
            lookup_mask = STATEMENT_TYPE_ATTRIBUTE;
        } else {
            lookup_mask = STATEMENT_TYPE_NAMED_EXPR | STATEMENT_TYPE_FIELD;
        }
        n_visible_statements = lookup_visible_statements_in_lists(
            lookup_mask,
            member->ndat->u.identifier,
            &ast_node_get_scope_def(anchor_filter)->block_stmt_list,
            &visible_statements);
        if (-1 == n_visible_statements) {
            return -1;
        }
        if (0 == n_visible_statements) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &member->loc,
                "no attribute named '%s' exists in block",
                member->ndat->u.identifier);
            semantic_error(SEMANTIC_LOGLEVEL_INFO, &anchor_filter->loc,
                           "declared here");
            return -1;
        }
        if (n_visible_statements == 1) {
            stmt_spec = &visible_statements[0];
            resolved_type = new_safe(struct ast_node_data);
            resolved_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_UNSET;
            resolved_type->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_UNSET;
            // scope operator only affects anchor filter
            if (!is_scope_operator) {
                resolved_type->u.rexpr_member_common.anchor_expr = anchor_expr;
            }
            resolved_type->u.rexpr_member_common.anchor_filter =
                (struct ast_node_hdl *)anchor_filter;
            expr->ndat = resolved_type;
            if (stmt_spec->anonymous_member) {
                expr->flags |= ASTFLAG_IS_ANONYMOUS_MEMBER;
            }
            switch (stmt_spec->stmt_type) {
            case STATEMENT_TYPE_NAMED_EXPR:
            case STATEMENT_TYPE_ATTRIBUTE:
                resolved_type->type = AST_NODE_TYPE_REXPR_NAMED_EXPR;
                resolved_type->u.rexpr_named_expr.named_expr =
                    (struct named_expr *)stmt_spec->nstmt;
                break ;
            case STATEMENT_TYPE_FIELD:
                resolved_type->type = AST_NODE_TYPE_REXPR_FIELD;
                resolved_type->u.rexpr_field.field =
                    (struct field *)stmt_spec->nstmt;
                break ;
            default:
                assert(0);
            }
            free(visible_statements);
            return 0;
        }
        // n_visible_statements > 1 => polymorphic member
    } else {
        // dynamic anchor => polymorphic member
        if (ast_node_is_item(anchor_expr)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &member->loc,
                "polymorphic members of types not supported");
            return -1;
        }
        visible_statements = NULL;
        n_visible_statements = 0;
    }
    resolved_type = new_safe(struct ast_node_data);
    resolved_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_UNSET;
    resolved_type->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_UNSET;
    // scope operator only affects anchor filter
    if (!is_scope_operator) {
        resolved_type->u.rexpr_member_common.anchor_expr = anchor_expr;
    }
    resolved_type->u.rexpr_member_common.anchor_filter =
        (struct ast_node_hdl *)anchor_filter;
    resolved_type->type = AST_NODE_TYPE_REXPR_POLYMORPHIC;
    resolved_type->u.rexpr_polymorphic.identifier =
        member->ndat->u.identifier;
    if (NULL != anchor_filter) {
        resolved_type->u.rexpr_polymorphic.visible_statements =
            visible_statements;
        resolved_type->u.rexpr_polymorphic.n_visible_statements =
            n_visible_statements;
    } else {
        expr->flags |= ASTFLAG_HAS_POLYMORPHIC_ANCHOR;
    }
    expr->ndat = resolved_type;
    return 0;
}

static int
compile_extern_decl(
    struct ast_node_hdl *extern_decl,
    dep_resolver_tagset_t tags,
    struct compile_ctx *ctx)
{
    struct ast_node_hdl *filter_spec;
    const struct block_stmt_list *stmt_lists;
    struct filter_class *filter_cls;
    int ret;
    struct ast_node_data *resolved_type;

    filter_spec = extern_decl->ndat->u.extern_decl.filter_spec;
    stmt_lists = &filter_spec->ndat->u.scope_def.block_stmt_list;
    if (-1 == compile_attributes(stmt_lists->attribute_list, tags, 0u, ctx)) {
        return -1;
    }

    if (0 != (tags & COMPILE_TAG_NODE_TYPE)) {
        filter_cls = filter_class_new(NULL);
        if (NULL == filter_cls) {
            return -1;
        }
        ret = filter_class_construct_extern_internal(
            filter_cls, filter_spec);
        if (-1 == ret) {
            return -1;
        }
        resolved_type = new_safe(struct ast_node_data);
        resolved_type->type = AST_NODE_TYPE_REXPR_EXTERN_DECL;
        resolved_type->u.rexpr.value_type_mask =
            filter_cls->value_type_mask;
        resolved_type->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_UNSET;
        resolved_type->u.rexpr_extern_decl.extern_decl =
            extern_decl->ndat->u.extern_decl;
        extern_decl->ndat = resolved_type;
    }
    return 0;
}

static int
compile_node_type_int(struct ast_node_hdl *node,
                      dep_resolver_tagset_t tags,
                      struct compile_ctx *ctx,
                      enum resolve_expect_mask expect_mask)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_INTEGER:
        return compile_expr_integer(node, tags, ctx, expect_mask);
    case AST_NODE_TYPE_BOOLEAN:
        return compile_expr_boolean(node, tags, ctx, expect_mask);
    case AST_NODE_TYPE_STRING:
        return compile_expr_string_literal(node, tags, ctx, expect_mask);
    case AST_NODE_TYPE_SCOPE_DEF:
        return compile_scope_def(node, tags, ctx);
    case AST_NODE_TYPE_FILTER_DEF:
        return compile_filter_def(node, tags, ctx, expect_mask);
    case AST_NODE_TYPE_REXPR_FILTER:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_BYTE_ARRAY:
        return compile_rexpr_filter(node, tags, ctx);
    case AST_NODE_TYPE_CONDITIONAL:
        return compile_conditional(node, tags, ctx);
    case AST_NODE_TYPE_EXTERN_DECL:
        return compile_extern_decl(node, tags, ctx);
    case AST_NODE_TYPE_EXTERN_FUNC:
        return compile_extern_func(node, tags, ctx);
    case AST_NODE_TYPE_OP_UPLUS:
    case AST_NODE_TYPE_OP_UMINUS:
    case AST_NODE_TYPE_OP_LNOT:
    case AST_NODE_TYPE_OP_BWNOT:
    case AST_NODE_TYPE_OP_MEMBER:
    case AST_NODE_TYPE_OP_SCOPE:
        return compile_expr_operator(node, 1, tags, ctx);
    case AST_NODE_TYPE_OP_EQ:
    case AST_NODE_TYPE_OP_NE:
    case AST_NODE_TYPE_OP_GT:
    case AST_NODE_TYPE_OP_LT:
    case AST_NODE_TYPE_OP_GE:
    case AST_NODE_TYPE_OP_LE:
    case AST_NODE_TYPE_OP_LOR:
    case AST_NODE_TYPE_OP_LAND:
    case AST_NODE_TYPE_OP_BWOR:
    case AST_NODE_TYPE_OP_BWXOR:
    case AST_NODE_TYPE_OP_BWAND:
    case AST_NODE_TYPE_OP_LSHIFT:
    case AST_NODE_TYPE_OP_RSHIFT:
    case AST_NODE_TYPE_OP_ADD:
    case AST_NODE_TYPE_OP_SUB:
    case AST_NODE_TYPE_OP_MUL:
    case AST_NODE_TYPE_OP_DIV:
    case AST_NODE_TYPE_OP_MOD:
        return compile_expr_operator(node, 2, tags, ctx);
    case AST_NODE_TYPE_OP_FILTER:
        return compile_expr_operator_filter(node, tags, ctx);
    case AST_NODE_TYPE_OP_ADDROF:
        return compile_expr_operator_addrof(node, tags, ctx, expect_mask);
    case AST_NODE_TYPE_OP_ANCESTOR:
        return compile_expr_operator_ancestor(node, tags, ctx, expect_mask);
    case AST_NODE_TYPE_OP_SUBSCRIPT:
        return compile_expr_operator_subscript(node, tags, ctx, expect_mask);
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
        return compile_expr_operator_subscript_slice(node, tags, ctx,
                                                     expect_mask);
    case AST_NODE_TYPE_OP_FCALL:
        return compile_expr_operator_fcall(node, tags, ctx, expect_mask);
    case AST_NODE_TYPE_OP_SIZEOF:
        return compile_expr_operator_sizeof(node, tags, ctx, expect_mask);
    case AST_NODE_TYPE_REXPR_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT:
        return compile_rexpr_operator(node, 1, tags, ctx);
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
        return compile_rexpr_operator_subscript(node, tags, ctx);
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        return compile_rexpr_operator_subscript_slice(node, tags, ctx);
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        return compile_rexpr_op_filter(node, tags, ctx);
    case AST_NODE_TYPE_REXPR_OP_EQ:
    case AST_NODE_TYPE_REXPR_OP_NE:
    case AST_NODE_TYPE_REXPR_OP_GT:
    case AST_NODE_TYPE_REXPR_OP_LT:
    case AST_NODE_TYPE_REXPR_OP_GE:
    case AST_NODE_TYPE_REXPR_OP_LE:
    case AST_NODE_TYPE_REXPR_OP_LOR:
    case AST_NODE_TYPE_REXPR_OP_LAND:
    case AST_NODE_TYPE_REXPR_OP_BWOR:
    case AST_NODE_TYPE_REXPR_OP_BWXOR:
    case AST_NODE_TYPE_REXPR_OP_BWAND:
    case AST_NODE_TYPE_REXPR_OP_LSHIFT:
    case AST_NODE_TYPE_REXPR_OP_RSHIFT:
    case AST_NODE_TYPE_REXPR_OP_ADD:
    case AST_NODE_TYPE_REXPR_OP_SUB:
    case AST_NODE_TYPE_REXPR_OP_MUL:
    case AST_NODE_TYPE_REXPR_OP_DIV:
    case AST_NODE_TYPE_REXPR_OP_MOD:
        return compile_rexpr_operator(node, 2, tags, ctx);
    case AST_NODE_TYPE_REXPR_OP_MEMBER:
        return compile_rexpr_member(node, FALSE, tags, ctx);
    case AST_NODE_TYPE_REXPR_OP_SCOPE:
        return compile_rexpr_member(node, TRUE, tags, ctx);
    case AST_NODE_TYPE_REXPR_OP_FCALL:
        return compile_rexpr_operator_fcall(node, tags, ctx);
    case AST_NODE_TYPE_REXPR_FIELD:
        return compile_rexpr_field(node, tags, ctx);
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        return compile_rexpr_named_expr(node, tags, ctx);
    case AST_NODE_TYPE_REXPR_POLYMORPHIC:
        return compile_rexpr_polymorphic(node, tags, ctx);
    default:
        /* nothing to do */
        return 0;
    }
}

static dep_resolver_tagset_t
dep_resolver_cb(struct dep_resolver *dr,
                struct dep_resolver_node *_node,
                dep_resolver_tagset_t tags,
                void *arg)
{
    struct compile_req *req;
    compile_func_t compile_cb;
    struct compile_ctx *ctx;
    enum resolve_expect_mask expect_mask;
    void *req_arg;
    dep_resolver_tagset_t resolved_tags;

    req = (struct compile_req *)arg;
    compile_cb = req->compile_cb;
    ctx = req->ctx;
    expect_mask = req->expect_mask;
    req_arg = req->arg;

    ctx->current_req = req;
    ctx->current_node = _node;
    ctx->current_tags = tags;
    ctx->current_node_family = compile_func_to_node_family(compile_cb);
    resolved_tags = compile_cb(ctx, expect_mask, _node, tags, req_arg);
    ctx->current_req = NULL;
    ctx->current_node = NULL;
    ctx->current_tags = 0u;
    return resolved_tags;
}

static void
dep_resolver_free_arg(struct dep_resolver *dr,
                      void *arg)
{
    compile_req_destroy((struct compile_req *)arg);
}

static dep_resolver_tagset_t
compile_node_once(struct ast_node_hdl *node,
                  dep_resolver_tagset_t tags,
                  struct compile_ctx *ctx,
                  enum resolve_expect_mask expect_mask)
{
    dep_resolver_tagset_t resolved_tags = 0;
    dep_resolver_tagset_t cur_tag;

    for (cur_tag = 1u; cur_tag <= COMPILE_TAG_LAST; cur_tag <<= 1) {
        if (0 != (tags & cur_tag)) {
            if (-1 == compile_node_type_int(node, cur_tag, ctx,
                                            expect_mask)) {
                return resolved_tags;
            }
            resolved_tags |= cur_tag;
        }
    }
    return resolved_tags;
}

static dep_resolver_tagset_t
compile_node_cb(struct compile_ctx *ctx,
                enum resolve_expect_mask expect_mask,
                struct dep_resolver_node *_node,
                dep_resolver_tagset_t tags,
                void *arg)
{
    struct ast_node_hdl *node;
    struct ast_node_data *old_data;
    dep_resolver_tagset_t resolved_tags;

    node = container_of(_node, struct ast_node_hdl, dr_node);
    do {
        old_data = node->ndat;
        resolved_tags = compile_node_once(node, tags, ctx, expect_mask);
    } while (old_data != node->ndat);
    return resolved_tags;
}

static dep_resolver_tagset_t
compile_dpath_cb(struct compile_ctx *ctx,
                 enum resolve_expect_mask expect_mask,
                 struct dep_resolver_node *_node,
                 dep_resolver_tagset_t tags,
                 void *arg)
{
    struct dpath_node *node;
    dep_resolver_tagset_t resolved_tags = 0;

    node = container_of(_node, struct dpath_node, dr_node);
    if (0 != (tags & COMPILE_TAG_NODE_TYPE)) {
        if (-1 == compile_dpath_type(node, ctx)) {
            return resolved_tags;
        }
        resolved_tags |= COMPILE_TAG_NODE_TYPE;
    }
    if (0 != (tags & COMPILE_TAG_NODE_SPAN_SIZE)) {
        if (-1 == compile_dpath_span_size(node, ctx)) {
            return resolved_tags;
        }
        resolved_tags |= COMPILE_TAG_NODE_SPAN_SIZE;
    }
    if (0 != (tags & COMPILE_TAG_BROWSE_BACKENDS)) {
        resolved_tags |= COMPILE_TAG_BROWSE_BACKENDS;
    }
    return resolved_tags;
}

static int
compile_dpath_type(struct dpath_node *node,
                   struct compile_ctx *ctx)
{
    struct ast_node_hdl *expr;
    struct ast_node_hdl *target_item;

    assert(NULL == node->filter);
    if (-1 == compile_node(node->item, ctx,
                           COMPILE_TAG_NODE_TYPE, 0u,
                           RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    expr = node->item;
    assert(ast_node_is_item(expr) || ast_node_is_rexpr(expr));
    target_item = ast_node_get_target_item(expr);
    if (NULL == target_item) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "expression needs a target item to be a valid dpath");
        return -1;
    }
    node->item = target_item;
    node->filter = expr;
    return 0;
}

static int
compile_dpath_span_size(struct dpath_node *node, struct compile_ctx *ctx)
{
    assert(ast_node_is_item(node->item));
    if (-1 == compile_node(node->item, ctx,
                           COMPILE_TAG_NODE_SPAN_SIZE, 0u, 0u)) {
        return -1;
    }
    if (-1 == compile_node(node->filter, ctx,
                           COMPILE_TAG_NODE_SPAN_SIZE, 0u, 0u)) {
        return -1;
    }
    return 0;
}



/*
 *
 */

void
compile_global_nodes(void)
{
    struct compile_ctx ctx;

    compile_ctx_init(&ctx);
    compile_global_nodes__byte(&ctx);
    compile_global_nodes__array_slice(&ctx);
    compile_global_nodes__byte_slice(&ctx);
    compile_ctx_destroy(&ctx);
}


void
dpath_node_reset(struct dpath_node *dpath)
{
    memset(dpath, 0, sizeof (*dpath));
}

int
ast_node_is_rexpr(const struct ast_node_hdl *node)
{
    if (NULL == node) {
        return FALSE;
    }
    switch (node->ndat->type) {
    case AST_NODE_TYPE_REXPR_NATIVE:
    case AST_NODE_TYPE_REXPR_OP_EQ:
    case AST_NODE_TYPE_REXPR_OP_NE:
    case AST_NODE_TYPE_REXPR_OP_GT:
    case AST_NODE_TYPE_REXPR_OP_LT:
    case AST_NODE_TYPE_REXPR_OP_GE:
    case AST_NODE_TYPE_REXPR_OP_LE:
    case AST_NODE_TYPE_REXPR_OP_LOR:
    case AST_NODE_TYPE_REXPR_OP_LAND:
    case AST_NODE_TYPE_REXPR_OP_BWOR:
    case AST_NODE_TYPE_REXPR_OP_BWXOR:
    case AST_NODE_TYPE_REXPR_OP_BWAND:
    case AST_NODE_TYPE_REXPR_OP_LSHIFT:
    case AST_NODE_TYPE_REXPR_OP_RSHIFT:
    case AST_NODE_TYPE_REXPR_OP_ADD:
    case AST_NODE_TYPE_REXPR_OP_SUB:
    case AST_NODE_TYPE_REXPR_OP_MUL:
    case AST_NODE_TYPE_REXPR_OP_DIV:
    case AST_NODE_TYPE_REXPR_OP_MOD:
    case AST_NODE_TYPE_REXPR_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT:
    case AST_NODE_TYPE_REXPR_OP_SIZEOF:
    case AST_NODE_TYPE_REXPR_OP_ADDROF:
    case AST_NODE_TYPE_REXPR_OP_ANCESTOR:
    case AST_NODE_TYPE_REXPR_OP_FILTER:
    case AST_NODE_TYPE_REXPR_FIELD:
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
    case AST_NODE_TYPE_REXPR_POLYMORPHIC:
    case AST_NODE_TYPE_REXPR_BUILTIN:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
    case AST_NODE_TYPE_REXPR_OP_MEMBER:
    case AST_NODE_TYPE_REXPR_OP_FCALL:
    case AST_NODE_TYPE_REXPR_SELF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_REXPR_FILTER:
        return TRUE;
    default:
        return FALSE;
    }
}

int
ast_node_is_rexpr_filter(const struct ast_node_hdl *node)
{
    if (NULL == node) {
        return FALSE;
    }
    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_REXPR_OP_FILTER:
    case AST_NODE_TYPE_REXPR_FILTER:
        return TRUE;
    default:
        return FALSE;
    }
}

int
ast_node_filter_maps_list(const struct ast_node_hdl *node)
{
    if (!ast_node_is_rexpr_filter(node)) {
        return FALSE;
    }
    return 0 != (node->ndat->u.item.flags & ITEMFLAG_FILTER_MAPS_LIST);
}

int
ast_node_filter_maps_object(const struct ast_node_hdl *node)
{
    if (!ast_node_is_rexpr_filter(node)) {
        return FALSE;
    }
    return 0 != (node->ndat->u.item.flags & ITEMFLAG_FILTER_MAPS_OBJECT);
}

struct ast_node_hdl *
ast_node_get_target_item(struct ast_node_hdl *node)
{
    if (NULL == node || ast_node_is_item(node)) {
        return node;
    }
    switch (node->ndat->type) {
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT: {
        struct ast_node_hdl *anchor_target;
        struct filter_instance_array *array;
        struct ast_node_hdl_array item_types;
        struct ast_node_hdl *item_type;
        bitpunch_status_t bt_ret;

        anchor_target = ast_node_get_target_item(
            node->ndat->u.rexpr_op_subscript_common.anchor_expr);
        if (NULL == anchor_target) {
            return NULL;
        }
        switch (anchor_target->ndat->type) {
        case AST_NODE_TYPE_ARRAY:
        case AST_NODE_TYPE_BYTE_ARRAY:
            array = (struct filter_instance_array *)
                anchor_target->ndat->u.rexpr_filter.f_instance;
            bt_ret = ast_node_filter_get_items(array->item_type,
                                               &item_types);
            if (BITPUNCH_OK != bt_ret) {
                return NULL;
            }
            assert(ARRAY_SIZE(&item_types) >= 1);
            item_type = ARRAY_ITEM(&item_types, 0);
            ast_node_hdl_array_destroy(&item_types);
            return ast_node_get_target_item(item_type);
        case AST_NODE_TYPE_ARRAY_SLICE:
        case AST_NODE_TYPE_BYTE_SLICE:
            return anchor_target;
        default:
            assert(0);
        }
    }
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        return ast_node_get_target_item(
            node->ndat->u.rexpr_op_subscript_common.anchor_expr);
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        return ast_node_get_target_item(
            node->ndat->u.rexpr_named_expr.named_expr->expr);
    case AST_NODE_TYPE_REXPR_FIELD: {
        struct ast_node_hdl_array field_items;
        struct ast_node_hdl *field_item;
        bitpunch_status_t bt_ret;

        bt_ret = ast_node_filter_get_items(
            node->ndat->u.rexpr_field.field->filter, &field_items);
        if (BITPUNCH_OK != bt_ret) {
            return NULL;
        }
        assert(ARRAY_SIZE(&field_items) >= 1);
        field_item = ARRAY_ITEM(&field_items, 0);
        ast_node_hdl_array_destroy(&field_items);
        return ast_node_get_target_item(field_item);
    }
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        return ast_node_get_target_item(node->ndat->u.rexpr_op_filter.target);
    case AST_NODE_TYPE_REXPR_SELF:
        return node->ndat->u.rexpr_member_common.anchor_filter;
    case AST_NODE_TYPE_REXPR_OP_ANCESTOR:
        return ast_node_get_target_item(node->ndat->u.rexpr_op.op.operands[0]);
    default:
        return NULL;
    }
}

struct ast_node_hdl *
ast_node_get_target_filter(struct ast_node_hdl *node)
{
    if (NULL == node) {
        return NULL;
    }
    switch (node->ndat->type) {
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        return ast_node_get_target_filter(
            node->ndat->u.rexpr_named_expr.named_expr->expr);
    case AST_NODE_TYPE_REXPR_FIELD:
        return ast_node_get_target_filter(
            node->ndat->u.rexpr_field.field->filter);
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT: {
        struct ast_node_hdl *anchor_target;
        struct filter_instance_array *array;

        anchor_target = ast_node_get_target_item(
            node->ndat->u.rexpr_op_subscript_common.anchor_expr);
        if (NULL == anchor_target) {
            return NULL;
        }
        switch (anchor_target->ndat->type) {
        case AST_NODE_TYPE_ARRAY:
        case AST_NODE_TYPE_BYTE_ARRAY:
            array = (struct filter_instance_array *)
                anchor_target->ndat->u.rexpr_filter.f_instance;
            return ast_node_get_target_filter(array->item_type);
        case AST_NODE_TYPE_ARRAY_SLICE:
        case AST_NODE_TYPE_BYTE_SLICE:
            return anchor_target;
        default:
            return NULL;
        }
    }
    case AST_NODE_TYPE_REXPR_OP_ANCESTOR:
        return ast_node_get_target_filter(
            node->ndat->u.rexpr_op.op.operands[0]);
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        return ast_node_get_target_filter(
            node->ndat->u.rexpr_op_filter.filter_expr);
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_REXPR_FILTER:
        return node;
    default:
        return NULL;
    }
}

struct ast_node_hdl *
ast_node_get_named_expr_target(struct ast_node_hdl *node)
{
    if (NULL == node
        || AST_NODE_TYPE_REXPR_NAMED_EXPR != node->ndat->type) {
        return node;
    }
    return ast_node_get_named_expr_target(
        node->ndat->u.rexpr_named_expr.named_expr->expr);
}



static bitpunch_status_t
ast_node_filter_get_items_int(
    struct ast_node_hdl *filter,
    struct ast_node_hdl_array *itemsp);

static bitpunch_status_t
ast_node_filter_get_items__named_expr(
    struct ast_node_hdl *filter,
    struct ast_node_hdl_array *itemsp)
{
    const struct named_expr *named_expr;

    named_expr = filter->ndat->u.rexpr_named_expr.named_expr;
    return ast_node_filter_get_items_int(named_expr->expr, itemsp);
}

static bitpunch_status_t
ast_node_filter_get_items__polymorphic(
    struct ast_node_hdl *filter,
    struct ast_node_hdl_array *itemsp)
{
    // TODO
    semantic_error(SEMANTIC_LOGLEVEL_ERROR, &filter->loc,
                   "polymorphic fields not supported");
    return BITPUNCH_NOT_IMPLEMENTED;
}

static bitpunch_status_t
ast_node_filter_get_items__op_filter(
    struct ast_node_hdl *filter,
    struct ast_node_hdl_array *itemsp)
{
    return ast_node_filter_get_items_int(
        filter->ndat->u.rexpr_op_filter.target, itemsp);
}

static bitpunch_status_t
ast_node_filter_get_items_int(
    struct ast_node_hdl *filter,
    struct ast_node_hdl_array *itemsp)
{
    switch (filter->ndat->type) {
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        return ast_node_filter_get_items__named_expr(filter, itemsp);
    case AST_NODE_TYPE_REXPR_POLYMORPHIC:
        return ast_node_filter_get_items__polymorphic(filter, itemsp);
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        return ast_node_filter_get_items__op_filter(filter, itemsp);
    case AST_NODE_TYPE_REXPR_FILTER:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
        ast_node_hdl_array_push(itemsp, filter);
        return BITPUNCH_OK;
    default:
        return BITPUNCH_INVALID_PARAM;
    }
}

bitpunch_status_t
ast_node_filter_get_items(struct ast_node_hdl *filter,
                          struct ast_node_hdl_array *itemsp)
{
    struct ast_node_hdl_array items;
    bitpunch_status_t bt_ret;

    if (NULL == filter) {
        return BITPUNCH_INVALID_PARAM;
    }
    ast_node_hdl_array_init(&items, 0);
    bt_ret = ast_node_filter_get_items_int(filter, &items);
    if (BITPUNCH_OK == bt_ret) {
        if (NULL != itemsp) {
            *itemsp = items;
        } else {
            ast_node_hdl_array_destroy(&items);
        }
    }
    return bt_ret;
}

int
ast_node_is_slice_container(const struct ast_node_hdl *node)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
        return TRUE;
    default:
        return FALSE;
    }
}

int
ast_node_is_item(const struct ast_node_hdl *node)
{
    if (NULL == node) {
        return FALSE;
    }
    switch (node->ndat->type) {
    case AST_NODE_TYPE_SCOPE_DEF:
    case AST_NODE_TYPE_FILTER_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_REXPR_FILTER:
        return TRUE;
    default:
        return FALSE;
    }
}

int
ast_node_is_trackable(const struct ast_node_hdl *node)
{
    if (NULL == node) {
        return FALSE;
    }
    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
        return TRUE;
    case AST_NODE_TYPE_REXPR_FILTER:
        return (ast_node_filter_maps_list(node) ||
                ast_node_filter_maps_object(node));
    default:
        return FALSE;
    }
}

int
ast_node_is_type(const struct ast_node_hdl *node)
{
    if (NULL == node) {
        return FALSE;
    }
    return ast_node_is_item(node);
}


int
ast_node_is_scope_only(const struct ast_node_hdl *node)
{
    return AST_NODE_TYPE_SCOPE_DEF == node->ndat->type;
}

int
ast_node_is_scope_def(const struct ast_node_hdl *node)
{
    if (NULL == node) {
        return FALSE;
    }
    switch (node->ndat->type) {
    case AST_NODE_TYPE_SCOPE_DEF:
    case AST_NODE_TYPE_FILTER_DEF:
        return TRUE;
    default:
        return FALSE;
    }
}

struct scope_def *
ast_node_get_scope_def(struct ast_node_hdl *node)
{
    if (ast_node_is_filter(node)) {
        return filter_get_scope_def(node);
    }
    if (ast_node_is_scope_def(node)) {
        return &node->ndat->u.scope_def;
    }
    return NULL;
}

const struct scope_def *
ast_node_get_const_scope_def(const struct ast_node_hdl *node)
{
    if (ast_node_is_filter(node)) {
        return filter_get_const_scope_def(node);
    }
    if (ast_node_is_scope_def(node)) {
        return &node->ndat->u.scope_def;
    }
    return NULL;
}

int
ast_node_is_filter(const struct ast_node_hdl *node)
{
    if (NULL == node) {
        return FALSE;
    }
    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_REXPR_FILTER:
        return TRUE;
    default:
        return FALSE;
    }
}

static struct ast_node_hdl *
ast_node_get_as_type__rexpr(const struct ast_node_hdl *expr)
{
    struct ast_node_hdl *as_type;

    if (NULL == expr) {
        return NULL;
    }
    switch (expr->ndat->type) {
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        as_type = ast_node_get_as_type(
            expr->ndat->u.rexpr_named_expr.named_expr->expr);
        if (NULL != as_type) {
            return as_type;
        }
        return NULL;

    case AST_NODE_TYPE_REXPR_OP_FILTER:
        as_type = ast_node_get_as_type(
            expr->ndat->u.rexpr_op_filter.filter_expr);
        if (NULL != as_type) {
            return as_type;
        }
        return ast_node_get_as_type(expr->ndat->u.rexpr_op_filter.target);

    case AST_NODE_TYPE_REXPR_SELF:
        return expr->ndat->u.rexpr_member_common.anchor_filter;

    case AST_NODE_TYPE_REXPR_FIELD:
        return ast_node_get_as_type(
            expr->ndat->u.rexpr_field.field->filter);

    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT: {
        struct ast_node_hdl *anchor_target;
        struct filter_instance_array *array;

        anchor_target = ast_node_get_as_type(
            expr->ndat->u.rexpr_op_subscript_common.anchor_expr);
        if (NULL == anchor_target) {
            return NULL;
        }
        switch (anchor_target->ndat->type) {
        case AST_NODE_TYPE_ARRAY:
        case AST_NODE_TYPE_BYTE_ARRAY:
            array = (struct filter_instance_array *)
                anchor_target->ndat->u.rexpr_filter.f_instance;
            return ast_node_get_as_type(array->item_type);
        case AST_NODE_TYPE_ARRAY_SLICE:
        case AST_NODE_TYPE_BYTE_SLICE:
            return anchor_target;
        default:
            assert(0);
        }
    }
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        return ast_node_get_as_type(
            expr->ndat->u.rexpr_op_subscript_common.anchor_expr);

    case AST_NODE_TYPE_OP_FILTER:
        // used during identifier resolution to explore anonymous
        // struct/unions
        return ast_node_get_as_type(expr->ndat->u.op.operands[1]);
    default:
        return NULL;
    }
}


struct ast_node_hdl *
ast_node_get_as_type(struct ast_node_hdl *node)
{
    if (ast_node_is_item(node)) {
        return node;
    }
    return ast_node_get_as_type__rexpr(node);
}

int64_t
ast_node_get_min_span_size(const struct ast_node_hdl *node)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_REXPR_FILTER:
        assert(SPAN_SIZE_UNDEF != node->ndat->u.item.min_span_size);
        return node->ndat->u.item.min_span_size;
    default:
        return 0;
    }
    /*NOT REACHED*/
}

int
ast_node_is_indexed(const struct ast_node_hdl *node)
{
    const struct ast_node_hdl *target;
    struct filter_instance_array *array;

    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
        array = (struct filter_instance_array *)
            node->ndat->u.rexpr_filter.f_instance;
        target = ast_node_get_as_type(array->item_type);
        if (!ast_node_is_filter(target)) {
            return FALSE;
        }
        return NULL != filter_get_first_declared_attribute(target, "@key");
    default:
        return FALSE;
    }
}

struct ast_node_hdl *
ast_node_get_key_expr(const struct ast_node_hdl *node)
{
    const struct ast_node_hdl *target;
    struct filter_instance_array *array;

    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
        array = (struct filter_instance_array *)
            node->ndat->u.rexpr_filter.f_instance;
        target = ast_node_get_as_type(array->item_type);
        if (!ast_node_is_filter(target)) {
            return NULL;
        }
        // TODO: multiple or conditional key expressions currently not
        // supported (needs proper support in index cache)

        return filter_get_first_declared_attribute(target, "@key");
    default:
        return NULL;
    }
}

enum expr_value_type
ast_node_get_key_type(const struct ast_node_hdl *node)
{
    struct ast_node_hdl *key_expr;

    key_expr = ast_node_get_key_expr(node);
    assert(NULL != key_expr);
    return key_expr->ndat->u.rexpr.value_type_mask;
}

struct ast_node_hdl *
dpath_node_get_as_type(const struct dpath_node *dpath)
{
    struct ast_node_hdl *as_type;

    as_type = ast_node_get_as_type(dpath->filter);
    if (NULL != as_type) {
        return as_type;
    }
    return dpath->item;
}

struct ast_node_hdl *
dpath_node_get_as_type__pre_compile_stage(const struct dpath_node *dpath)
{
    struct ast_node_hdl *as_type;

    as_type = ast_node_get_as_type(dpath->filter);
    if (NULL != as_type) {
        return as_type;
    }
    return ast_node_get_as_type(dpath->item);
}

struct ast_node_hdl *
dpath_node_get_target_filter(const struct dpath_node *dpath)
{
    return ast_node_get_target_filter(dpath->filter);
}


/*
 * debug
 */

#define INDENT_N_SPACES   2

static void
fdump_ast_recur(const struct ast_node_hdl *node, int depth,
                struct list_of_visible_refs *visible_refs, FILE *stream);
static void
dump_filter_recur(const struct ast_node_hdl *filter,
                  int depth,
                  struct list_of_visible_refs *outer_refs,
                  FILE *stream);
static void
dump_block_stmt_list_recur(const struct ast_node_hdl *filter,
                           const struct block_stmt_list *block_lists,
                           int depth,
                           struct list_of_visible_refs *outer_refs,
                           FILE *stream);
static void
dump_ast_type(const struct ast_node_hdl *node, int depth,
              struct list_of_visible_refs *visible_refs, FILE *stream);

void
dump_ast_location(const struct ast_node_hdl *node)
{
    fdump_ast_location(node, stdout);
}

void
fdump_ast_location(const struct ast_node_hdl *node, FILE *out)
{
    bitpunch_parser_print_location(&node->loc, out);
}

void
dump_ast_dot(struct ast_node_hdl *node,
             const char *node_family, dep_resolver_tagset_t tag)
{
    fdump_ast_dot(node, node_family, tag, stdout);
}

void
fdump_ast_dot(struct ast_node_hdl *node,
              const char *node_family, dep_resolver_tagset_t tag,
              FILE *out)
{
    fprintf(out, "%s:%d:%s@(%d,%d)",
            node_family, tag,
            ast_node_type_str(node->ndat->type),
            node->loc.last_line, node->loc.last_column);
}

void
dump_ast(const struct ast_node_hdl *root)
{
    fdump_ast(root, stdout);
}

void
fdump_ast(const struct ast_node_hdl *root, FILE *out)
{
    if (debug_output_newline_before_locations) {
        fprintf(out, "\n");
    }
    if (NULL == root) {
        fprintf(out, "<null>\n");
        return ;
    }
    fdump_ast_location(root, out);
    fdump_ast_recur(root, 0, NULL, out);
}
void
dump_filter(const struct ast_node_hdl *filter, FILE *out)
{
    dump_filter_recur(filter, 0, NULL, out);
}

static void
dump_ast_rexpr(const struct ast_node_hdl *node, FILE *out) {
    fprintf(out, "value-type mask: %s (%d), dpath-type mask: %s (%d)",
            expr_value_type_str(node->ndat->u.rexpr.value_type_mask),
            node->ndat->u.rexpr.value_type_mask,
            expr_dpath_type_str(node->ndat->u.rexpr.dpath_type_mask),
            node->ndat->u.rexpr.dpath_type_mask);
}

static void
dump_ast_rexpr_member(const struct ast_node_hdl *node, int depth,
                      struct list_of_visible_refs *visible_refs, FILE *out)
{
    dump_ast_rexpr(node, out);
    if (NULL != node->ndat->u.rexpr_member_common.anchor_expr) {
        fprintf(out, "\n%*s\\_ anchor expr:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_type(node->ndat->u.rexpr_member_common.anchor_expr, depth + 2,
                      visible_refs, out);
    }
    fprintf(out, "\n");
}

static void
dump_ast_rexpr_scope(const struct ast_node_hdl *node, int depth,
                      struct list_of_visible_refs *visible_refs, FILE *out)
{
    dump_ast_rexpr(node, out);
    if (NULL != node->ndat->u.rexpr_member_common.anchor_expr) {
        fprintf(out, "\n%*s\\_ anchor expr:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_type(node->ndat->u.rexpr_member_common.anchor_expr, depth + 2,
                      visible_refs, out);
    }
    fprintf(out, "\n");
}

static const char *
span_size_str(int64_t span_size)
{
    static char size_buf[16];

    if (SPAN_SIZE_UNDEF == span_size) {
        return "undef";
    } else {
        sprintf(size_buf, "%"PRId64, span_size);
        return size_buf;
    }
    /*NOT REACHED*/
}

static void
dump_ast_item_info(const struct ast_node_hdl *node, FILE *out)
{
    fprintf(out, "min span size: %s%s%s",
            span_size_str(node->ndat->u.item.min_span_size),
            (0 != (node->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_VARIABLE) ?
             " (variable span)" : ""),
            (0 != (node->ndat->u.item.flags & ITEMFLAG_IS_USED_SIZE_VARIABLE) ?
             " (variable used)" : ""));
}

static void
fdump_ast_recur(const struct ast_node_hdl *node, int depth,
                struct list_of_visible_refs *visible_refs, FILE *out)
{
    static struct ast_node_data dummy_empty_node_data = {
        .type = AST_NODE_TYPE_NONE,
    };
    static struct ast_node_hdl dummy_empty_node = {
        .ndat = &dummy_empty_node_data,
    };
    struct ast_node_hdl *_node = (struct ast_node_hdl *)node;

    if (NULL == node) {
        node = &dummy_empty_node;
    }
    dump_ast_type(node, depth, visible_refs, out);
    if (node == &dummy_empty_node
        || 0 != (node->flags & ASTFLAG_DUMPING)) {
        fprintf(out, "\n");
        return ;
    }
    _node->flags |= ASTFLAG_DUMPING;
    switch (node->ndat->type) {
    case AST_NODE_TYPE_INTEGER:
        fprintf(out, "%"PRIi64"\n", node->ndat->u.integer);
        break ;
    case AST_NODE_TYPE_BOOLEAN:
        fprintf(out, "%s\n", (node->ndat->u.boolean ? "true" : "false"));
        break ;
    case AST_NODE_TYPE_STRING:
        print_expr_string(&node->ndat->u.string, out);
        fputc('\n', out);
        break ;
    case AST_NODE_TYPE_IDENTIFIER:
        fprintf(out, "\"%s\"\n", node->ndat->u.identifier);
        break ;
    case AST_NODE_TYPE_SCOPE_DEF:
        fprintf(out, "scope\n");
        dump_filter_recur(node, depth + 1, visible_refs, out);
        break ;
    case AST_NODE_TYPE_FILTER_DEF:
        fprintf(out, "filter type: %s\n",
                node->ndat->u.filter_def.filter_type);
        dump_filter_recur(node, depth + 1, visible_refs, out);
        break ;
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY: {
        struct filter_instance_array *array;

        array = (struct filter_instance_array *)
            node->ndat->u.rexpr_filter.f_instance;
        fprintf(out, "\n%*s\\_ ", (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_item_info(node, out);
        fprintf(out, ", item type:\n");
        fdump_ast_recur(array->item_type, depth + 2, visible_refs, out);
        fprintf(out, "%*s\\_ value count:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(array->item_count, depth + 2, visible_refs, out);
        break ;
    }
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
        fprintf(out, "\n%*s\\_ static node (%s)\n",
                (depth + 1) * INDENT_N_SPACES, "",
                ast_node_type_str(node->ndat->type));
        break ;
    case AST_NODE_TYPE_BYTE:
        fprintf(out, "\n");
        break ;
    case AST_NODE_TYPE_CONDITIONAL:
        fprintf(out, "\n%*s\\_ condition expr:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->ndat->u.conditional.cond_expr,
                        depth + 2, visible_refs, out);
        if (NULL != node->ndat->u.conditional.outer_cond) {
            fprintf(out, "%*s\\_ outer conditional:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->ndat->u.conditional.outer_cond, depth + 2,
                            visible_refs, out);
        }
        break ;
    case AST_NODE_TYPE_EXTERN_DECL:
        fprintf(out, "\n%*s\\_ %s\n",
                (depth + 1) * INDENT_N_SPACES, "",
                ast_node_type_str(node->ndat->type));
        fdump_ast_recur(node->ndat->u.extern_decl.filter_spec,
                        depth + 2, visible_refs, out);
        break ;
    case AST_NODE_TYPE_REXPR_EXTERN_DECL:
        fprintf(out, "\n%*s\\_ %s\n",
                (depth + 1) * INDENT_N_SPACES, "",
                ast_node_type_str(node->ndat->type));
        fdump_ast_recur(node->ndat->u.rexpr_extern_decl.extern_decl.filter_spec,
                        depth + 2, visible_refs, out);
        break ;
    case AST_NODE_TYPE_EXTERN_FUNC:
    case AST_NODE_TYPE_REXPR_EXTERN_FUNC:
        fprintf(out, "\n%*s\\_ %s\"\n",
                (depth + 1) * INDENT_N_SPACES, "",
                ast_node_type_str(node->ndat->type));
        break ;
    case AST_NODE_TYPE_EXTERN_FILTER:
        fprintf(out, "\n%*s\\_ %s\"\n",
                (depth + 1) * INDENT_N_SPACES, "",
                ast_node_type_str(node->ndat->type));
        break ;
    case AST_NODE_TYPE_OP_EQ:
    case AST_NODE_TYPE_OP_NE:
    case AST_NODE_TYPE_OP_GT:
    case AST_NODE_TYPE_OP_LT:
    case AST_NODE_TYPE_OP_GE:
    case AST_NODE_TYPE_OP_LE:
    case AST_NODE_TYPE_OP_LOR:
    case AST_NODE_TYPE_OP_LAND:
    case AST_NODE_TYPE_OP_BWOR:
    case AST_NODE_TYPE_OP_BWXOR:
    case AST_NODE_TYPE_OP_BWAND:
    case AST_NODE_TYPE_OP_LSHIFT:
    case AST_NODE_TYPE_OP_RSHIFT:
    case AST_NODE_TYPE_OP_ADD:
    case AST_NODE_TYPE_OP_SUB:
    case AST_NODE_TYPE_OP_MUL:
    case AST_NODE_TYPE_OP_DIV:
    case AST_NODE_TYPE_OP_MOD:
    case AST_NODE_TYPE_OP_MEMBER:
    case AST_NODE_TYPE_OP_SCOPE:
    case AST_NODE_TYPE_OP_FILTER:
        fprintf(out, "\n");
        fdump_ast_recur(node->ndat->u.op.operands[0], depth + 1, visible_refs,
                        out);
        fdump_ast_recur(node->ndat->u.op.operands[1], depth + 1, visible_refs,
                        out);
        break ;
    case AST_NODE_TYPE_OP_UPLUS:
    case AST_NODE_TYPE_OP_UMINUS:
    case AST_NODE_TYPE_OP_LNOT:
    case AST_NODE_TYPE_OP_BWNOT:
    case AST_NODE_TYPE_OP_SIZEOF:
    case AST_NODE_TYPE_OP_ADDROF:
    case AST_NODE_TYPE_OP_ANCESTOR:
        fprintf(out, "\n");
        fdump_ast_recur(node->ndat->u.op.operands[0], depth + 1, visible_refs,
                        out);
        break ;
    case AST_NODE_TYPE_OP_SUBSCRIPT:
        fprintf(out, "\n");
        fdump_ast_recur(node->ndat->u.op_subscript_common.anchor_expr, depth + 1,
                        visible_refs, out);
        fdump_ast_recur(node->ndat->u.op_subscript.index.key, depth + 1,
                        visible_refs, out);
        if (NULL != node->ndat->u.op_subscript.index.twin) {
            fprintf(out, "%*s\\_ twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->ndat->u.op_subscript.index.twin, depth + 1,
                            visible_refs, out);
        }
        break ;
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
        fprintf(out, "\n");
        fdump_ast_recur(node->ndat->u.op_subscript_common.anchor_expr, depth + 1,
                        visible_refs, out);
        if (NULL != node->ndat->u.op_subscript_slice.start.key) {
            fprintf(out, "%*s\\_ start index:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->ndat->u.op_subscript_slice.start.key, depth + 1,
                            visible_refs, out);
        }
        if (NULL != node->ndat->u.op_subscript_slice.start.twin) {
            fprintf(out, "%*s\\_ start twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->ndat->u.op_subscript_slice.start.twin, depth + 1,
                            visible_refs, out);
        }
        if (NULL != node->ndat->u.op_subscript_slice.end.key) {
            fprintf(out, "%*s\\_ end index:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->ndat->u.op_subscript_slice.end.key, depth + 1,
                            visible_refs, out);
        }
        if (NULL != node->ndat->u.op_subscript_slice.end.twin) {
            fprintf(out, "%*s\\_ end twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->ndat->u.op_subscript_slice.end.twin, depth + 1,
                            visible_refs, out);
        }
        break ;
    case AST_NODE_TYPE_OP_FCALL: {
        struct statement *stmt;
        struct named_expr *func_param;

        fprintf(out, "\n");
        fdump_ast_recur(node->ndat->u.op_fcall.func, depth + 1, visible_refs,
                        out);

        fprintf(out, "\n%*s\\_ params:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        TAILQ_FOREACH(stmt, node->ndat->u.op_fcall.func_params, list) {
            func_param = (struct named_expr *)stmt;
            fdump_ast_recur(func_param->expr, depth + 1, visible_refs,
                            out);
        }
        break ;
    }
    case AST_NODE_TYPE_REXPR_OP_FCALL: {
        struct statement *stmt;
        struct named_expr *func_param;

        fprintf(out, "\n%*s\\_ name: %s, params:\n",
                (depth + 1) * INDENT_N_SPACES, "",
                node->ndat->u.rexpr_op_fcall.builtin->builtin_name);
        TAILQ_FOREACH(stmt, node->ndat->u.rexpr_op_fcall.func_params, list) {
            func_param = (struct named_expr *)stmt;
            fdump_ast_recur(func_param->expr, depth + 1, visible_refs,
                            out);
        }
        break ;
    }
    case AST_NODE_TYPE_REXPR_NATIVE:
        switch (node->ndat->u.rexpr_native.rexpr.value_type_mask) {
        case EXPR_VALUE_TYPE_INTEGER:
            fprintf(out, "%"PRIi64"\n", node->ndat->u.rexpr_native.value.integer);
            break ;
        case EXPR_VALUE_TYPE_BOOLEAN:
            fprintf(out, "%s\n",
                    (node->ndat->u.rexpr_native.value.boolean ?
                     "true" : "false"));
            break ;
        case EXPR_VALUE_TYPE_STRING:
            print_expr_string(&node->ndat->u.rexpr_native.value.string, out);
            fputc('\n', out);
            break ;
        default:
            break ;
        }
        break ;
    case AST_NODE_TYPE_REXPR_OP_EQ:
    case AST_NODE_TYPE_REXPR_OP_NE:
    case AST_NODE_TYPE_REXPR_OP_GT:
    case AST_NODE_TYPE_REXPR_OP_LT:
    case AST_NODE_TYPE_REXPR_OP_GE:
    case AST_NODE_TYPE_REXPR_OP_LE:
    case AST_NODE_TYPE_REXPR_OP_LOR:
    case AST_NODE_TYPE_REXPR_OP_LAND:
    case AST_NODE_TYPE_REXPR_OP_BWOR:
    case AST_NODE_TYPE_REXPR_OP_BWXOR:
    case AST_NODE_TYPE_REXPR_OP_BWAND:
    case AST_NODE_TYPE_REXPR_OP_LSHIFT:
    case AST_NODE_TYPE_REXPR_OP_RSHIFT:
    case AST_NODE_TYPE_REXPR_OP_ADD:
    case AST_NODE_TYPE_REXPR_OP_SUB:
    case AST_NODE_TYPE_REXPR_OP_MUL:
    case AST_NODE_TYPE_REXPR_OP_DIV:
    case AST_NODE_TYPE_REXPR_OP_MOD:
        dump_ast_rexpr(node, out);
        fprintf(out, "\n");
        fdump_ast_recur(node->ndat->u.rexpr_op.op.operands[0], depth + 1,
                        visible_refs, out);
        fdump_ast_recur(node->ndat->u.rexpr_op.op.operands[1], depth + 1,
                        visible_refs, out);
        break ;
    case AST_NODE_TYPE_REXPR_FILTER: {
        const struct filter_class *filter_cls;

        dump_ast_rexpr(node, out);
        filter_cls = node->ndat->u.rexpr_filter.filter_cls;
        fprintf(out, " name: %s ", filter_cls->name);
        dump_ast_item_info(node, out);
        fprintf(out, "\n");
        dump_filter_recur(node, depth + 1, visible_refs, out);
        break ;
    }
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        dump_ast_rexpr(node, out);
        fprintf(out, "\n%*s\\_ target:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->ndat->u.rexpr_op_filter.target, depth + 2,
                        visible_refs, out);
        fprintf(out, "\n%*s\\_ filter expr:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->ndat->u.rexpr_op_filter.filter_expr, depth + 2,
                        visible_refs, out);
        break ;
    case AST_NODE_TYPE_REXPR_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT:
    case AST_NODE_TYPE_REXPR_OP_SIZEOF:
    case AST_NODE_TYPE_REXPR_OP_ADDROF:
    case AST_NODE_TYPE_REXPR_OP_ANCESTOR:
        dump_ast_rexpr(node, out);
        fprintf(out, "\n");
        fdump_ast_recur(node->ndat->u.rexpr_op.op.operands[0], depth + 1,
                        visible_refs, out);
        break ;
    case AST_NODE_TYPE_REXPR_OP_MEMBER:
        dump_ast_rexpr_member(node, depth, visible_refs, out);
        break ;
    case AST_NODE_TYPE_REXPR_OP_SCOPE:
        dump_ast_rexpr_scope(node, depth, visible_refs, out);
        break ;
    case AST_NODE_TYPE_REXPR_FIELD:
        dump_ast_rexpr_member(node, depth, visible_refs, out);
        fprintf(out, "%*s\\_ field:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        if (NULL != node->ndat->u.rexpr_field.field->filter) {
            fprintf(out, "%*s\\_ filter:\n",
                    (depth + 2) * INDENT_N_SPACES, "");
            dump_ast_type(node->ndat->u.rexpr_field.field->filter, depth + 3,
                          visible_refs, out);
            fprintf(out, "\n");
        }
        break ;
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        dump_ast_rexpr_member(node, depth, visible_refs, out);
        fprintf(out, "%*s\\_ named expr:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_type(node->ndat->u.rexpr_named_expr.named_expr->expr, depth + 2,
                      visible_refs, out);
        fprintf(out, "\n");
        break ;
    case AST_NODE_TYPE_REXPR_POLYMORPHIC:
        dump_ast_rexpr_member(node, depth, visible_refs, out);
        fprintf(out, "%*s\\_ polymorphic: %s [%d refs]\n",
                (depth + 1) * INDENT_N_SPACES, "",
                node->ndat->u.rexpr_polymorphic.identifier,
                node->ndat->u.rexpr_polymorphic.n_visible_statements);
        fprintf(out, "\n");
        break ;
    case AST_NODE_TYPE_REXPR_BUILTIN:
        dump_ast_rexpr(node, out);
        fprintf(out, "\n");
        break ;
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
        dump_ast_rexpr(node, out);
        fprintf(out, "\n");
        fdump_ast_recur(node->ndat->u.rexpr_op_subscript_common.anchor_expr, depth + 1,
                        visible_refs, out);
        fdump_ast_recur(node->ndat->u.rexpr_op_subscript.index.key, depth + 1,
                        visible_refs, out);
        if (NULL != node->ndat->u.rexpr_op_subscript.index.twin) {
            fprintf(out, "%*s\\_ twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->ndat->u.rexpr_op_subscript.index.twin, depth + 1,
                            visible_refs, out);
        }
        break ;
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        dump_ast_rexpr(node, out);
        fprintf(out, "\n");
        fdump_ast_recur(node->ndat->u.rexpr_op_subscript_common.anchor_expr, depth + 1,
                        visible_refs, out);
        if (NULL != node->ndat->u.rexpr_op_subscript_slice.start.key) {
            fprintf(out, "%*s\\_ start index:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->ndat->u.rexpr_op_subscript_slice.start.key,
                            depth + 2, visible_refs, out);
        }
        if (NULL != node->ndat->u.rexpr_op_subscript_slice.start.twin) {
            fprintf(out, "%*s\\_ start twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->ndat->u.rexpr_op_subscript_slice.start.twin,
                            depth + 2, visible_refs, out);
        }
        if (NULL != node->ndat->u.rexpr_op_subscript_slice.end.key) {
            fprintf(out, "%*s\\_ end index:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->ndat->u.rexpr_op_subscript_slice.end.key,
                            depth + 2, visible_refs, out);
        }
        if (NULL != node->ndat->u.rexpr_op_subscript_slice.end.twin) {
            fprintf(out, "%*s\\_ end twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->ndat->u.rexpr_op_subscript_slice.end.twin,
                            depth + 2, visible_refs, out);
        }
        break ;
    case AST_NODE_TYPE_REXPR_SELF:
        dump_ast_rexpr_member(node, depth, visible_refs, out);
        fprintf(out, "\n");
        break ;
    case AST_NODE_TYPE_EXPR_SELF:
    case AST_NODE_TYPE_NONE:
        fprintf(out, "\n");
        break ;
    }
    _node->flags &= ~ASTFLAG_DUMPING;
}

static void
dump_ast_type(const struct ast_node_hdl *node, int depth,
              struct list_of_visible_refs *visible_refs, FILE *out)
{
    const char *type_name;
    int noname = FALSE;

    switch (node->ndat->type) {
    case AST_NODE_TYPE_NONE:
    case AST_NODE_TYPE_INTEGER:
    case AST_NODE_TYPE_BOOLEAN:
    case AST_NODE_TYPE_STRING:
    case AST_NODE_TYPE_IDENTIFIER:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_EXPR_SELF:
    case AST_NODE_TYPE_REXPR_NATIVE:
    case AST_NODE_TYPE_REXPR_SELF:
    case AST_NODE_TYPE_EXTERN_DECL:
    case AST_NODE_TYPE_EXTERN_FUNC:
    case AST_NODE_TYPE_EXTERN_FILTER:
    case AST_NODE_TYPE_REXPR_EXTERN_DECL:
    case AST_NODE_TYPE_REXPR_EXTERN_FUNC:
        /* leaf */
        fprintf(out, "%*s|- (%s) ", depth * INDENT_N_SPACES, "",
                ast_node_type_str(node->ndat->type));
        break ;
    case AST_NODE_TYPE_REXPR_FILTER:
    case AST_NODE_TYPE_SCOPE_DEF:
    case AST_NODE_TYPE_FILTER_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_CONDITIONAL:
    case AST_NODE_TYPE_OP_EQ:
    case AST_NODE_TYPE_OP_NE:
    case AST_NODE_TYPE_OP_GT:
    case AST_NODE_TYPE_OP_LT:
    case AST_NODE_TYPE_OP_GE:
    case AST_NODE_TYPE_OP_LE:
    case AST_NODE_TYPE_OP_LOR:
    case AST_NODE_TYPE_OP_LAND:
    case AST_NODE_TYPE_OP_BWOR:
    case AST_NODE_TYPE_OP_BWXOR:
    case AST_NODE_TYPE_OP_BWAND:
    case AST_NODE_TYPE_OP_LSHIFT:
    case AST_NODE_TYPE_OP_RSHIFT:
    case AST_NODE_TYPE_OP_ADD:
    case AST_NODE_TYPE_OP_SUB:
    case AST_NODE_TYPE_OP_MUL:
    case AST_NODE_TYPE_OP_DIV:
    case AST_NODE_TYPE_OP_MOD:
    case AST_NODE_TYPE_OP_UPLUS:
    case AST_NODE_TYPE_OP_UMINUS:
    case AST_NODE_TYPE_OP_LNOT:
    case AST_NODE_TYPE_OP_BWNOT:
    case AST_NODE_TYPE_OP_SIZEOF:
    case AST_NODE_TYPE_OP_ADDROF:
    case AST_NODE_TYPE_OP_ANCESTOR:
    case AST_NODE_TYPE_OP_SUBSCRIPT:
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
    case AST_NODE_TYPE_OP_MEMBER:
    case AST_NODE_TYPE_OP_SCOPE:
    case AST_NODE_TYPE_OP_FCALL:
    case AST_NODE_TYPE_OP_FILTER:
    case AST_NODE_TYPE_REXPR_OP_EQ:
    case AST_NODE_TYPE_REXPR_OP_NE:
    case AST_NODE_TYPE_REXPR_OP_GT:
    case AST_NODE_TYPE_REXPR_OP_LT:
    case AST_NODE_TYPE_REXPR_OP_GE:
    case AST_NODE_TYPE_REXPR_OP_LE:
    case AST_NODE_TYPE_REXPR_OP_LOR:
    case AST_NODE_TYPE_REXPR_OP_LAND:
    case AST_NODE_TYPE_REXPR_OP_BWOR:
    case AST_NODE_TYPE_REXPR_OP_BWXOR:
    case AST_NODE_TYPE_REXPR_OP_BWAND:
    case AST_NODE_TYPE_REXPR_OP_LSHIFT:
    case AST_NODE_TYPE_REXPR_OP_RSHIFT:
    case AST_NODE_TYPE_REXPR_OP_ADD:
    case AST_NODE_TYPE_REXPR_OP_SUB:
    case AST_NODE_TYPE_REXPR_OP_MUL:
    case AST_NODE_TYPE_REXPR_OP_DIV:
    case AST_NODE_TYPE_REXPR_OP_MOD:
    case AST_NODE_TYPE_REXPR_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT:
    case AST_NODE_TYPE_REXPR_OP_SIZEOF:
    case AST_NODE_TYPE_REXPR_OP_ADDROF:
    case AST_NODE_TYPE_REXPR_OP_ANCESTOR:
    case AST_NODE_TYPE_REXPR_OP_MEMBER:
    case AST_NODE_TYPE_REXPR_OP_SCOPE:
    case AST_NODE_TYPE_REXPR_FIELD:
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
    case AST_NODE_TYPE_REXPR_POLYMORPHIC:
    case AST_NODE_TYPE_REXPR_BUILTIN:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
    case AST_NODE_TYPE_REXPR_OP_FCALL:
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        /* intermediate node */
        fprintf(out, "%*s\\_ (%s) ", depth * INDENT_N_SPACES, "",
                ast_node_type_str(node->ndat->type));
        break ;
    }

    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_REXPR_FILTER:
        type_name = reverse_lookup_typename(node, visible_refs);
        if (NULL == type_name) {
            noname = TRUE;
        }
        break ;
    default:
        type_name = NULL;
        break ;
    }
    if (NULL != type_name) {
        fprintf(out, "<%s> ", type_name);
    } else if (noname) {
        fprintf(out, "<noname> ");
    }
}

static void
dump_filter_recur(const struct ast_node_hdl *filter,
                  int depth,
                  struct list_of_visible_refs *outer_refs,
                  FILE *out)
{
    if (NULL == filter) {
        return ;
    }
    dump_block_stmt_list_recur(
        filter,
        &ast_node_get_const_scope_def(filter)->block_stmt_list,
        depth, outer_refs, out);
}

static void
dump_block_stmt_list_recur(const struct ast_node_hdl *filter,
                           const struct block_stmt_list *block_lists,
                           int depth,
                           struct list_of_visible_refs *outer_refs,
                           FILE *out)
{
    struct list_of_visible_refs visible_refs;
    const struct field *field;
    const struct named_expr *named_expr;
    struct named_expr *attr;

    /* add current refs to the chain of visible refs */
    visible_refs.outer_refs = outer_refs;
    visible_refs.cur_filter = filter;
    visible_refs.cur_lists = block_lists;

    fprintf(out, "%*s\\_ named expressions:\n",
            depth * INDENT_N_SPACES, "");
    STATEMENT_FOREACH(named_expr, named_expr,
                      block_lists->named_expr_list, list) {
        fprintf(out, "%*s\\_ name \"%s\"\n",
                (depth + 1) * INDENT_N_SPACES, "",
                named_expr->nstmt.name);
        fprintf(out, "%*s\\_ expr:\n",
                (depth + 2) * INDENT_N_SPACES, "");
        fdump_ast_recur(named_expr->expr, depth + 3, &visible_refs,
                        out);
    }
    fprintf(out, "%*s\\_ fields:\n", depth * INDENT_N_SPACES, "");
    STATEMENT_FOREACH(field, field, block_lists->field_list, list) {
        fprintf(out, "%*s\\_ name \"%s\"\n",
                (depth + 1) * INDENT_N_SPACES, "",
                (NULL != field->nstmt.name ? field->nstmt.name : "N/A"));
        fprintf(out, "%*s\\_ field filter:\n",
                (depth + 2) * INDENT_N_SPACES, "");
        fdump_ast_recur(field->filter, depth + 3, &visible_refs, out);
    }
    fprintf(out, "%*s\\_ attributes:\n", depth * INDENT_N_SPACES, "");
    STATEMENT_FOREACH(named_expr, attr, block_lists->attribute_list, list) {
        fprintf(out, "%*s\\_ \"%s\":\n",
                (depth + 2) * INDENT_N_SPACES, "",
                attr->nstmt.name);
        fdump_ast_recur(attr->expr, depth + 3, &visible_refs, out);
    }
}

void
dump_ast_node_input_text(const struct ast_node_hdl *node,
                         struct ast_node_hdl *schema,
                         FILE *out)
{
    const struct parser_ctx *parser_ctx;
    const char *text_start;
    const char *text_end;

    parser_ctx = node->loc.parser_ctx;
    if (NULL != parser_ctx) {
        text_start = parser_ctx->parser_data + node->loc.start_offset;
        text_end = parser_ctx->parser_data + node->loc.end_offset;
        fwrite(text_start, 1, text_end - text_start, out);
        fputs("\n", out);
    }
}
