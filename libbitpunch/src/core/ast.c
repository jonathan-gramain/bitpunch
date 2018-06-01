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
#include "interpreters/item.h"
#include PATH_TO_PARSER_TAB_H

//#define OUTPUT_DEP_GRAPH

struct list_of_visible_refs {
    const struct list_of_visible_refs *outer_refs;
    const struct ast_node_hdl         *cur_block;
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
resolve_identifiers_dpath_node(
    struct dpath_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_identifiers_tag resolve_tags);
static int
compile_ast_node_all(struct ast_node_hdl *ast_root,
                     enum resolve_expect_mask expect_mask);
static int
compile_dpath_node_all(struct dpath_node *dpath_root);
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
static int
compile_node_type(struct ast_node_hdl *node,
                  struct compile_ctx *ctx,
                  enum resolve_expect_mask expect_mask);
static int
compile_node_span_size(struct ast_node_hdl *node,
                       struct compile_ctx *ctx);
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

int
bitpunch_compile_schema(struct bitpunch_schema_hdl *schema)
{
    struct dpath_node *ast_root;

    ast_root = schema->df_file_block.root;
    if (-1 == resolve_identifiers_dpath_node(
            ast_root, NULL, RESOLVE_TYPE_IDENTIFIERS)) {
        return -1;
    }
    if (-1 == resolve_identifiers_dpath_node(
            ast_root, NULL, RESOLVE_EXPRESSION_IDENTIFIERS)) {
        return -1;
    }
    if (-1 == compile_dpath_node_all(ast_root)) {
        return -1;
    }
    if (-1 == browse_setup_global_backends()) {
        return -1;
    }
    if (-1 == browse_setup_backends_dpath(ast_root)) {
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
    case STATEMENT_TYPE_MATCH:
        return stmt_lists->match_list;
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
    struct ast_node_hdl *anchor_block,
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
    statement_spec->anchor_block = anchor_block;
    statement_spec->anonymous_member = anonymous_member;
    ++(*visible_statements_indexp);
    return 0;
}

static int
lookup_visible_statements_in_lists_internal(
    enum statement_type stmt_mask,
    const char *identifier,
    const struct block_stmt_list *stmt_lists,
    int anonymous_member,
    struct named_statement_spec **visible_statementsp,
    int *visible_statements_indexp);

static int
lookup_visible_statements_in_anonymous_field(
    enum statement_type stmt_mask,
    const char *identifier,
    struct field *field,
    struct named_statement_spec **visible_statementsp,
    int *visible_statements_indexp)
{
    const struct ast_node_hdl *field_type;
    int ret;

    field_type = dpath_node_get_as_type__pre_compile_stage(&field->dpath);
    if (NULL != field_type
        && AST_NODE_TYPE_BLOCK_DEF == field_type->ndat->type) {
        ret = lookup_visible_statements_in_lists_internal(
            stmt_mask, identifier,
            &field_type->ndat->u.block_def.block_stmt_list, TRUE,
            visible_statementsp, visible_statements_indexp);
        if (0 != ret) {
            return ret;
        }
    }
    return 0;
}

static int
lookup_visible_statements_in_lists_internal(
    enum statement_type stmt_mask,
    const char *identifier,
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

    if (identifier[0] == '@') {
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
                && 0 == strcmp(identifier, nstmt->name)) {
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
    // recurse in anonymous struct/union fields
    TAILQ_FOREACH(stmt, stmt_lists->field_list, list) {
        nstmt = (struct named_statement *)stmt;
        if (NULL == nstmt->name
            && !(nstmt->stmt.stmt_flags & FIELD_FLAG_HIDDEN)) {
            ret = lookup_visible_statements_in_anonymous_field(
                stmt_mask, identifier, (struct field *)nstmt,
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
            stmt_mask, identifier, refs_level->cur_lists, FALSE,
            &visible_statements, &visible_statements_index);
        if (-1 == ret) {
            free(visible_statements);
            return -1;
        }
        while (last_visible_statements_index < visible_statements_index) {
            visible_statements[last_visible_statements_index].anchor_block =
                refs_level->cur_block;
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
        stmt_mask, identifier, stmt_lists, FALSE,
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
        stmt_mask, identifier, stmt_lists, FALSE,
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
resolve_identifiers_dpath_node(
    struct dpath_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_identifiers_tag resolve_tags)
{
    if (NULL != node->item
        && -1 == resolve_identifiers(node->item, visible_refs,
                                     RESOLVE_EXPECT_TYPE, resolve_tags)) {
        return -1;
    }
    if (NULL != node->filter
        && -1 == resolve_identifiers(node->filter, visible_refs,
                                     RESOLVE_EXPECT_INTERPRETER,
                                     resolve_tags)) {
        return -1;
    }
    return 0;
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
        resolved_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_UNSET;
        if (1 == n_visible_statements) {
            stmt_spec = &visible_statements[0];
            resolved_type->u.rexpr_member_common.anchor_block =
                (struct ast_node_hdl *)stmt_spec->anchor_block;
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
    if (0 == strcmp(node->ndat->u.identifier, "byte")) {
        struct ast_node_data *resolved_type;

        /* native 'byte' type */
        resolved_type = new_safe(struct ast_node_data);
        resolved_type->type = AST_NODE_TYPE_BYTE;
        resolved_type->u.item.min_span_size = 1;
        node->ndat = resolved_type;
        return 0;
    }
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
        resolved_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_UNSET;
        resolved_type->u.rexpr_builtin.builtin = builtin;
        node->ndat = resolved_type;
        return 0;
    }
    return -1;
}

static int
resolve_identifiers_identifier_as_interpreter(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs)
{
    const struct interpreter *interpreter;

    interpreter = interpreter_lookup(node->ndat->u.identifier);
    if (NULL != interpreter) {
        struct statement_list empty_attr_list;

        TAILQ_INIT(&empty_attr_list);
        if (-1 == interpreter_rcall_build(node, interpreter,
                                          &empty_attr_list)) {
            return -1;
        }
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
        if (0 != (expect_mask & RESOLVE_EXPECT_TYPE)
            && 0 == resolve_identifiers_identifier_as_type(
                node, visible_refs)) {
            return 0;
        }
        if (0 != (expect_mask & RESOLVE_EXPECT_INTERPRETER)
            && 0 == resolve_identifiers_identifier_as_interpreter(
                node, visible_refs)) {
            return 0;
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
    int is_interpreter,
    enum resolve_identifiers_tag resolve_tags)
{
    if (is_interpreter) {
        return resolve_identifiers(field->dpath.item, visible_refs,
                                   RESOLVE_EXPECT_EXPRESSION, resolve_tags);
    } else {
        return resolve_identifiers_dpath_node(&field->dpath,
                                              visible_refs, resolve_tags);
    }
}

static int
resolve_identifiers_in_block_body(
    struct ast_node_hdl *block,
    const struct list_of_visible_refs *outer_refs,
    enum resolve_identifiers_tag resolve_tags)
{
    struct block_stmt_list *stmt_lists;
    struct list_of_visible_refs visible_refs;
    struct field *field;
    struct named_expr *named_expr;

    stmt_lists = &block->ndat->u.block_def.block_stmt_list;
    /* add current refs to the chain of visible refs */
    visible_refs.outer_refs = outer_refs;
    visible_refs.cur_block = block;
    visible_refs.cur_lists = stmt_lists;

    /* it's required to first resolve type names before names in
     * expressions, so that names can be found inside anonymous types
     * or interpreted types (as-types) */
    if (-1 == resolve_identifiers_in_statement_list(
            stmt_lists->field_list, &visible_refs, resolve_tags)) {
        return -1;
    }
    if (-1 == resolve_identifiers_in_statement_list(
            stmt_lists->named_expr_list, &visible_refs, resolve_tags)) {
        return -1;
    }
    if (-1 == resolve_identifiers_in_statement_list(
            stmt_lists->attribute_list, &visible_refs, resolve_tags)) {
        return -1;
    }
    STATEMENT_FOREACH(named_expr, named_expr,
                      stmt_lists->named_expr_list, list) {
        if (NULL != named_expr->expr
            && -1 == resolve_identifiers(named_expr->expr, &visible_refs,
                                         RESOLVE_EXPECT_TYPE |
                                         RESOLVE_EXPECT_INTERPRETER |
                                         RESOLVE_EXPECT_EXPRESSION,
                                         resolve_tags)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(field, field, stmt_lists->field_list, list) {
        if (-1 == resolve_identifiers_field(
                field, &visible_refs,
                block->ndat->u.block_def.type == BLOCK_TYPE_INTERPRETER,
                resolve_tags)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(named_expr, named_expr,
                      stmt_lists->attribute_list, list) {
        if (-1 == resolve_identifiers_in_expression(
                named_expr->expr, &visible_refs, resolve_tags)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve_identifiers_block(struct ast_node_hdl *block,
                          const struct list_of_visible_refs *visible_refs,
                          enum resolve_expect_mask expect_mask,
                          enum resolve_identifiers_tag resolve_tags)
{
    const char *filter_type;
    const struct interpreter *interpreter;

    filter_type = block->ndat->u.block_def.filter_type;
    // XXX can it be NULL?
    if (NULL != filter_type) {
        if (0 == strcmp(filter_type, "struct")) {
            block->ndat->u.block_def.type = BLOCK_TYPE_STRUCT;
        } else if (0 == strcmp(filter_type, "union")) {
            block->ndat->u.block_def.type = BLOCK_TYPE_UNION;
        } else {
            block->ndat->u.block_def.type = BLOCK_TYPE_INTERPRETER;
        }
    }
    if (-1 == resolve_identifiers_in_block_body(block, visible_refs,
                                                resolve_tags)) {
        return -1;
    }
    if (BLOCK_TYPE_INTERPRETER == block->ndat->u.block_def.type) {
        interpreter = interpreter_lookup(filter_type);
        if (NULL == interpreter) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &block->loc,
                "no interpreter named '%s' exists",
                filter_type);
            return -1;
        }
    } else {
        interpreter = interpreter_lookup("item");
        assert(NULL != interpreter);
    }
    if (-1 == interpreter_rcall_build(
            block, interpreter,
            block->ndat->u.block_def.block_stmt_list.attribute_list)) {
        return -1;
    }
    return 0;
}

static int
resolve_identifiers_array(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    /* resolve array type and count expression, if defined */
    if (-1 == resolve_identifiers_dpath_node(&node->ndat->u.array.item_type,
                                             visible_refs, resolve_tags)) {
        return -1;
    }
    if (NULL != node->ndat->u.array.item_count &&
        -1 == resolve_identifiers_in_expression(
            node->ndat->u.array.item_count, visible_refs,
            resolve_tags)) {
        return -1;
    }
    return 0;
}

static int
resolve_identifiers_byte_array(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    /* resolve array count expression, if defined */
    if (NULL != node->ndat->u.byte_array.size &&
        -1 == resolve_identifiers_in_expression(
            node->ndat->u.byte_array.size, visible_refs,
            resolve_tags)) {
        return -1;
    }
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
resolve_identifiers_operator_set_filter(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    if (-1 == resolve_identifiers(
            node->ndat->u.op.operands[0], visible_refs,
            expect_mask & (RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_INTERPRETER |
                           RESOLVE_EXPECT_DPATH_EXPRESSION),
            resolve_tags)) {
        return -1;
    }
    if (-1 == resolve_identifiers(node->ndat->u.op.operands[1], visible_refs,
                                  RESOLVE_EXPECT_TYPE |
                                  RESOLVE_EXPECT_INTERPRETER,
                                  resolve_tags)) {
        return -1;
    }
    return 0;
}

static int
resolve_identifiers_expr_file(
    struct ast_node_hdl *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    const struct list_of_visible_refs *toplevel_refs;
    struct ast_node_data *compiled_type;

    if (NULL == visible_refs) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                       "need a binary file loaded to use 'file' in "
                       "expression");
        return -1;
    }
    toplevel_refs = visible_refs;
    while (NULL != toplevel_refs->outer_refs) {
        toplevel_refs = toplevel_refs->outer_refs;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_FILE;
    compiled_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_UNSET;
    // const-cast
    compiled_type->u.rexpr_item.item_type =
        (struct ast_node_hdl *)toplevel_refs->cur_block;
    expr->ndat = compiled_type;
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
                       "need a binary file loaded to use 'self' in "
                       "expression");
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_SELF;
    compiled_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_UNSET;
    // const-cast
    compiled_type->u.rexpr_item.item_type =
        (struct ast_node_hdl *)visible_refs->cur_block;
    expr->ndat = compiled_type;
    return 0;
}

static int
resolve_identifiers_rexpr_interpreter(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    enum resolve_identifiers_tag resolve_tags)
{
    const struct interpreter *interpreter;
    int p_idx;
    struct named_expr *attr;

    interpreter = node->ndat->u.rexpr_interpreter.interpreter;
    for (p_idx = 0; p_idx < interpreter->n_attrs; ++p_idx) {
        attr = node->ndat->u.rexpr_interpreter.attr_set.attrs[p_idx];
        if (NULL != attr
            && -1 == resolve_identifiers(attr->expr, visible_refs,
                                         RESOLVE_EXPECT_EXPRESSION,
                                         resolve_tags)) {
            return -1;
        }
    }
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

static enum expr_value_type
expr_value_type_mask_from_dpath_node(const struct dpath_node *dpath)
{
    if (NULL != dpath->filter) {
        return expr_value_type_mask_from_node(dpath->filter);
    }
    return EXPR_VALUE_TYPE_UNSET;
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
resolve_identifiers(struct ast_node_hdl *node,
                    const struct list_of_visible_refs *visible_refs,
                    enum resolve_expect_mask expect_mask,
                    enum resolve_identifiers_tag resolve_tags)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_IDENTIFIER:
        return resolve_identifiers_identifier(node, visible_refs,
                                              expect_mask, resolve_tags);
    case AST_NODE_TYPE_BLOCK_DEF:
        return resolve_identifiers_block(node, visible_refs,
                                         expect_mask, resolve_tags);
    case AST_NODE_TYPE_ARRAY:
        return resolve_identifiers_array(node, visible_refs,
                                         expect_mask, resolve_tags);
    case AST_NODE_TYPE_BYTE_ARRAY:
        return resolve_identifiers_byte_array(node, visible_refs,
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
    case AST_NODE_TYPE_OP_SET_FILTER:
        return resolve_identifiers_operator_set_filter(
            node, visible_refs, expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_ADDROF:
    case AST_NODE_TYPE_OP_ANCESTOR:
        return resolve_identifiers_operator_on_dpath(node, visible_refs,
                                                     expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_MEMBER:
        return resolve_identifiers_operator_member(node, visible_refs,
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
    case AST_NODE_TYPE_EXPR_FILE:
        return resolve_identifiers_expr_file(node, visible_refs,
                                             expect_mask, resolve_tags);
    case AST_NODE_TYPE_EXPR_SELF:
        return resolve_identifiers_expr_self(node, visible_refs,
                                             expect_mask, resolve_tags);
    case AST_NODE_TYPE_REXPR_INTERPRETER:
        return resolve_identifiers_rexpr_interpreter(
            node, visible_refs, expect_mask, resolve_tags);
    default:
        /* nothing to resolve */
        return 0;
    }
    /*NOT REACHED*/
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
    case AST_NODE_TYPE_OP_SET_FILTER:
        return AST_NODE_TYPE_REXPR_FILTER;
    case AST_NODE_TYPE_OP_SUBSCRIPT:
        return AST_NODE_TYPE_REXPR_OP_SUBSCRIPT;
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
        return AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE;
    case AST_NODE_TYPE_OP_MEMBER:
        return AST_NODE_TYPE_REXPR_OP_MEMBER;
    case AST_NODE_TYPE_OP_FCALL:
        /*TODO*/
        return AST_NODE_TYPE_NONE;
    default:
        assert(0);
    }
    /*NOT REACHED*/
}

static int
resolve_user_expr_internal(struct ast_node_hdl *expr,
                           struct list_of_visible_refs *inmost_refs)
{
    if (-1 == resolve_identifiers_in_expression(expr, inmost_refs,
                                                RESOLVE_ALL_IDENTIFIERS)) {
        return -1;
    }
    if (-1 == compile_ast_node_all(expr, RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    if (-1 == browse_setup_backends_expr(expr)) {
        return -1;
    }
    return 0;
}

static int
resolve_user_expr_scoped_recur(struct ast_node_hdl *expr,
                               struct box *cur_scope,
                               struct list_of_visible_refs *inner_refs,
                               struct list_of_visible_refs *inmost_refs)
{
    struct list_of_visible_refs visible_refs;

    while (NULL != cur_scope
           && AST_NODE_TYPE_BLOCK_DEF != cur_scope->dpath.item->ndat->type) {
        cur_scope = cur_scope->parent_box;
    }
    if (NULL == cur_scope) {
        return resolve_user_expr_internal(expr, inmost_refs);
    }
    visible_refs.outer_refs = NULL;
    visible_refs.cur_block = cur_scope->dpath.item;
    visible_refs.cur_lists =
        &cur_scope->dpath.item->ndat->u.block_def.block_stmt_list;
    if (NULL != inner_refs) {
        inner_refs->outer_refs = &visible_refs;
    }
    return resolve_user_expr_scoped_recur(expr, cur_scope->parent_box,
                                          &visible_refs,
                                          (NULL != inmost_refs ?
                                           inmost_refs : &visible_refs));
}

int
resolve_user_expr(struct ast_node_hdl *expr, struct box *scope)
{
    if (NULL != scope) {
        return resolve_user_expr_scoped_recur(expr, scope, NULL, NULL);
    }
    return resolve_user_expr_internal(expr, NULL);
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
        dpath = &container_of(node, struct field, dr_node)->dpath;
        return (NULL != dpath->filter ? dpath->filter : dpath->item);
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

static struct ast_node_data *
ast_node_data_new_rexpr_item(struct ast_node_hdl *item)
{
    struct ast_node_data *ndat;

    ndat = new_safe(struct ast_node_data);
    ndat->type = AST_NODE_TYPE_REXPR_ITEM;
    ndat->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_BYTES;
    ndat->u.rexpr_item.item_type = item;
    return ndat;
}

static struct ast_node_hdl *
ast_node_new_rexpr_item(struct ast_node_hdl *item, struct compile_ctx *ctx)
{
    struct ast_node_hdl *node;

    node = ast_node_hdl_new();
    node->loc = item->loc;
    node->ndat = ast_node_data_new_rexpr_item(item);
    return node;
}

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
    return compile_node(node, ctx,
                        (is_dependency ? COMPILE_TAG_NODE_TYPE : 0u),
                        (is_dependency ? 0u : COMPILE_TAG_NODE_TYPE),
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
            semantic_error(SEMANTIC_LOGLEVEL_INFO, loc, NULL);
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
                 COMPILE_TAG_NODE_SPAN_SIZE, 0u, expect_mask);
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
compile_dpath_node_all(struct dpath_node *dpath_root)
{
    struct compile_ctx ctx;
    dep_resolver_status_t ret;

    compile_ctx_init(&ctx);
    compile_dpath(dpath_root, &ctx,
                  COMPILE_TAG_NODE_TYPE |
                  COMPILE_TAG_NODE_SPAN_SIZE, 0u);
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
compile_stmt_list_generic(struct statement_list *stmt_list,
                           struct compile_ctx *ctx)
{
    struct statement *stmt;

    TAILQ_FOREACH(stmt, stmt_list, list) {
        if (NULL != stmt->cond) {
            compile_expr(stmt->cond, ctx, FALSE);
        }
    }
    return 0;
}

static int
compile_named_expr(struct named_expr *named_expr, struct compile_ctx *ctx,
                   enum resolve_expect_mask expect_mask)
{
    struct ast_node_hdl *expr;

    expr = named_expr->expr;
    return compile_node(expr, ctx,
                        0u, (COMPILE_TAG_NODE_TYPE |
                             COMPILE_TAG_NODE_SPAN_SIZE), expect_mask);
}

static dep_resolver_tagset_t
compile_field_cb(struct compile_ctx *ctx,
                 enum resolve_expect_mask expect_mask,
                 struct dep_resolver_node *_node,
                 dep_resolver_tagset_t tags,
                 void *arg)
{
    struct field *field;
    struct dpath_node *dpath;
    struct ast_node_hdl *target_item;

    field = container_of(_node, struct field, dr_node);
    dpath = &field->dpath;

    if (-1 == compile_dpath(&field->dpath, ctx, tags, 0u)) {
        return 0u;
    }
    // XXX support named expr polymorphism
    target_item = ast_node_get_target_item(dpath->item);
    if (NULL == target_item) {
        assert(NULL != dpath->filter);
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &dpath->filter->loc,
                       "expect an item type as field type, not '%s'",
                       ast_node_type_str(dpath->filter->ndat->type));
        return 0u;
    }

    if (0 != (tags & COMPILE_TAG_NODE_TYPE) && NULL == field->nstmt.name) {
        const struct ast_node_hdl *as_type;

        // Unnamed fields with block types allow direct child
        // field access without using the dot operator (like
        // inheritance). For non-block types it becomes a hidden
        // field (takes up space but not exposed to API).

        // XXX only set flag if all polymorphic named expr targets are
        // hidden fields
        as_type = ast_node_get_named_expr_target(
            (struct ast_node_hdl *)
            dpath_node_get_as_type__pre_compile_stage(dpath));
        if (AST_NODE_TYPE_BLOCK_DEF != as_type->ndat->type) {
            field->nstmt.stmt.stmt_flags |= FIELD_FLAG_HIDDEN;
        }
    }
    if (NULL != field->nstmt.stmt.cond) {
        if (0 != (dpath->u.item.flags & ITEMFLAG_SPREADS_SLACK)) {
            dpath->u.item.flags &= ~ITEMFLAG_SPREADS_SLACK;
            dpath->u.item.flags |= ITEMFLAG_CONDITIONALLY_SPREADS_SLACK;
        }
        if (0 != (dpath->u.item.flags & ITEMFLAG_FILLS_SLACK)) {
            dpath->u.item.flags &= ~ITEMFLAG_FILLS_SLACK;
            dpath->u.item.flags |= ITEMFLAG_CONDITIONALLY_FILLS_SLACK;
        }
    }
    return tags;
}

static int
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

static int
compile_stmt_lists(struct block_stmt_list *stmt_lists,
                    struct compile_ctx *ctx)
{
    struct field *field;
    struct expr_stmt *match;
    struct named_expr *named_expr;

    compile_stmt_list_generic(stmt_lists->named_expr_list, ctx);

    STATEMENT_FOREACH(named_expr, named_expr,
                      stmt_lists->named_expr_list, list) {
        compile_named_expr(named_expr, ctx,
                           RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_INTERPRETER |
                           RESOLVE_EXPECT_EXPRESSION);
    }
    compile_stmt_list_generic(stmt_lists->field_list, ctx);
    compile_stmt_list_generic(stmt_lists->attribute_list, ctx);
    
    STATEMENT_FOREACH(field, field, stmt_lists->field_list, list) {
        compile_field(field, ctx, 0u, COMPILE_TAG_NODE_TYPE);
    }
    STATEMENT_FOREACH(named_expr, named_expr,
                      stmt_lists->attribute_list, list) {
        compile_expr(named_expr->expr, ctx, TRUE);
    }
    STATEMENT_FOREACH(expr_stmt, match, stmt_lists->match_list, list) {
        compile_expr(match->expr, ctx, FALSE);
    }
    if (!compile_continue(ctx)) {
        return -1;
    }
    return 0;
}

static int
compile_interpreter_attributes(struct ast_node_hdl *node,
                               const struct interpreter *interpreter,
                               struct attribute_set *attr_set,
                               struct compile_ctx *ctx)
{
    struct ast_node_hdl *attr_exprp;
    struct interpreter_attr_def *attr_def;
    int sem_error = FALSE;

    STAILQ_FOREACH(attr_def, &interpreter->attr_list, list) {
        attr_exprp = attr_set->attrs[attr_def->ref_idx]->expr;
        compile_expr(attr_exprp, ctx, TRUE);
    }
    if (!compile_continue(ctx)) {
        return -1;
    }
    STAILQ_FOREACH(attr_def, &interpreter->attr_list, list) {
        attr_exprp = attr_set->attrs[attr_def->ref_idx]->expr;
        if (AST_NODE_TYPE_NONE != attr_exprp->ndat->type) {
            assert(ast_node_is_rexpr(attr_exprp));
            if (0 == (attr_def->value_type_mask
                      & attr_exprp->ndat->u.rexpr.value_type_mask)) {
                semantic_error(
                    SEMANTIC_LOGLEVEL_ERROR, &attr_exprp->loc,
                    "attribute \"%s\" passed to interpreter \"%s\" has "
                    "an incompatible value-type '%s', acceptable "
                    "value-types are '%s'",
                    attr_def->name, interpreter->name,
                    expr_value_type_str(
                        attr_exprp->ndat->u.rexpr.value_type_mask),
                    expr_value_type_str(attr_def->value_type_mask));
                sem_error = TRUE;
                continue ;
            }
        }
    }
    if (sem_error) {
        return -1;
    }
    if (-1 == interpreter->rcall_build_func(node, attr_set, ctx)) {
        return -1;
    }
    return 0;
}

static int
compile_item(struct ast_node_hdl *item,
             struct statement_list *attribute_list,
             struct compile_ctx *ctx)
{
    if (-1 == interpreter_build_attrs(
            item,
            item->ndat->u.item.interpreter,
            attribute_list,
            &item->ndat->u.item.attr_set)) {
        return -1;
    }
    if (-1 == compile_interpreter_attributes(
            item,
            item->ndat->u.item.interpreter,
            &item->ndat->u.item.attr_set, ctx)) {
        return -1;
    }
    return 0;
}

static int
compile_block(struct ast_node_hdl *block,
              struct compile_ctx *ctx,
              enum resolve_expect_mask expect_mask)
{
    if (-1 == compile_stmt_lists(&block->ndat->u.block_def.block_stmt_list,
                                 ctx)) {
        return -1;
    }
    if (-1 == compile_item(
            block,
            block->ndat->u.block_def.block_stmt_list.attribute_list, ctx)) {
        return -1;
    }
    return 0;
}

static int
compile_array(struct ast_node_hdl *node,
              struct compile_ctx *ctx,
              enum resolve_expect_mask expect_mask)
{
    if (NULL != node->ndat->u.array.item_count) {
        compile_expr(node->ndat->u.array.item_count, ctx, FALSE);
    }
    compile_dpath(&node->ndat->u.array.item_type, ctx,
                  COMPILE_TAG_NODE_TYPE, 0u);
    if (!compile_continue(ctx)) {
        return -1;
    }
    assert(NULL != node->ndat->u.array.item_type.filter);
    if (AST_NODE_TYPE_BYTE == node->ndat->u.array.item_type.item->ndat->type
        && AST_NODE_TYPE_REXPR_ITEM ==
        node->ndat->u.array.item_type.filter->ndat->type) {
        struct ast_node_data *byte_array;

        byte_array = new_safe(struct ast_node_data);
        byte_array->type = AST_NODE_TYPE_BYTE_ARRAY;
        byte_array->u.item.min_span_size = SPAN_SIZE_UNDEF;
        byte_array->u.byte_array.size = node->ndat->u.array.item_count;
        node->ndat = byte_array;
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
    compiled_type->flags |= ASTFLAG_IS_VALUE_TYPE;
    compiled_type->u.rexpr.value_type_mask = value.type;
    compiled_type->u.rexpr_native.value = value;
    node->ndat = compiled_type;
    return 0;
}

static int
compile_expr_integer(struct ast_node_hdl *node,
                     struct compile_ctx *ctx,
                     enum resolve_expect_mask expect_mask)
{
    int64_t integer;

    integer = node->ndat->u.integer;
    return compile_expr_native_internal(node, expr_value_as_integer(integer));
}

static int
compile_expr_boolean(struct ast_node_hdl *node,
                     struct compile_ctx *ctx,
                     enum resolve_expect_mask expect_mask)
{
    int boolean;

    boolean = node->ndat->u.boolean;
    return compile_expr_native_internal(node, expr_value_as_boolean(boolean));
}

static int
compile_expr_string_literal(struct ast_node_hdl *node,
                            struct compile_ctx *ctx,
                            enum resolve_expect_mask expect_mask)
{
    struct expr_value_string string;

    string = node->ndat->u.string;
    return compile_expr_native_internal(
        node, expr_value_as_string(string.str, string.len, NULL));
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
    struct compile_ctx *ctx)
{
    struct ast_node_data *resolved_type;
    int opd_i;

    for (opd_i = 0; opd_i < n_operands; ++opd_i) {
        compile_expr(expr->ndat->u.op.operands[opd_i], ctx, TRUE);
    }
    if (!compile_continue(ctx)) {
        return -1;
    }
    resolved_type = new_safe(struct ast_node_data);
    resolved_type->type = op_type_ast2rexpr(expr->ndat->type);
    resolved_type->flags = ASTFLAG_IS_VALUE_TYPE;
    resolved_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_UNSET;
    resolved_type->u.rexpr_op.op = expr->ndat->u.op;
    expr->ndat = resolved_type;
    return 0;
}

static int
compile_rexpr_operator(
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
compile_expr_operator_set_filter(
    struct ast_node_hdl *node,
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
                 RESOLVE_EXPECT_INTERPRETER |
                 RESOLVE_EXPECT_DPATH_EXPRESSION);
    compile_node(filter, ctx,
                 COMPILE_TAG_NODE_TYPE, 0u,
                 RESOLVE_EXPECT_TYPE |
                 RESOLVE_EXPECT_INTERPRETER);
    if (!compile_continue(ctx)) {
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_FILTER;
    compiled_type->u.rexpr.value_type_mask =
        filter->ndat->u.rexpr.value_type_mask;
    if (ast_node_is_item(target)) {
        target = ast_node_new_rexpr_item(target, ctx);
    }
    if (ast_node_is_item(filter)) {
        filter = ast_node_new_rexpr_item(filter, ctx);
    }
    compiled_type->u.rexpr_filter.target = target;
    compiled_type->u.rexpr_filter.filter_expr = filter;
    node->ndat = compiled_type;
    node->flags |= ((filter->flags & ASTFLAG_IS_VALUE_TYPE)
                    & (target->flags & ASTFLAG_IS_VALUE_TYPE));
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
    struct ast_node_hdl *key;
    struct ast_node_hdl *twin_idx;
    struct ast_node_hdl *anchor_expr;
    struct ast_node_hdl *anchor_item;
    struct ast_node_hdl *key_expr;

    assert(AST_NODE_TYPE_REXPR_OP_SUBSCRIPT == expr->ndat->type ||
           AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE == expr->ndat->type);
    subscript = container_of(_node, struct subscript_index, dr_node);
    if (NULL == subscript->key) {
        return COMPILE_TAG_NODE_TYPE;
    }
    compile_expr(subscript->key, ctx, TRUE);
    if (NULL != subscript->twin) {
        compile_expr(subscript->twin, ctx, TRUE);
    }
    if (!compile_continue(ctx)) {
        return 0u;
    }
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
                  & EXPR_VALUE_TYPE_INTEGER)
            && AST_NODE_TYPE_REXPR_STAR_WILDCARD != twin_idx->ndat->type) {
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
    return COMPILE_TAG_NODE_TYPE;
}

static int
compile_subscript_index(struct ast_node_hdl *expr,
                        struct subscript_index *subscript,
                        struct compile_ctx *ctx)
{
    struct compile_req *req;
    dep_resolver_status_t ret;
    
    assert(NULL != expr);
    req = compile_req_create(compile_subscript_index_cb, ctx,
                             RESOLVE_EXPECT_EXPRESSION);
    req->arg = expr;
    ret = dep_resolver_schedule_tags(ctx->dep_resolver, &subscript->dr_node,
                                     0u, COMPILE_TAG_NODE_TYPE, req);
#ifdef OUTPUT_DEP_GRAPH
    if (NULL != subscript->key) {
        output_dep_graph_links(ctx, ret, "subscript", subscript->key,
                               COMPILE_TAG_NODE_TYPE, 0u, ctx->deps_dot);
    }
#endif
    return (DEP_RESOLVER_OK == ret ? 0 : -1);
}

static int
compile_expr_operator_subscript(struct ast_node_hdl *node,
                                struct compile_ctx *ctx,
                                enum resolve_expect_mask expect_mask)
{
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
            if (-1 == compile_dpath(&anchor_item->ndat->u.array.item_type,
                                    ctx, COMPILE_TAG_NODE_TYPE, 0u)) {
                return -1;
            }
            value_type_mask = expr_value_type_mask_from_dpath_node(
                &anchor_item->ndat->u.array.item_type);
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
    if (-1 == compile_subscript_index(node, index, ctx)) {
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_SUBSCRIPT;
    compiled_type->u.rexpr.value_type_mask = value_type_mask;
    assert(ast_node_is_rexpr(anchor_expr));
    compiled_type->u.rexpr_op_subscript_common.anchor_expr = anchor_expr;
    compiled_type->u.rexpr_op_subscript.index = *index;
    index = &compiled_type->u.op_subscript.index;
    node->ndat = compiled_type;
    return 0;
}

static int
compile_expr_operator_subscript_slice(struct ast_node_hdl *node,
                                      struct compile_ctx *ctx,
                                      enum resolve_expect_mask expect_mask)
{
    struct ast_node_hdl *anchor_expr;
    struct ast_node_hdl *anchor_item;
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
    anchor_item = ast_node_get_as_type(anchor_expr);
    if (NULL != anchor_item) {
        if (! ast_node_is_subscriptable_container(anchor_item)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                "invalid use of subscript operator on non-subscriptable "
                "path of type '%s'",
                ast_node_type_str(anchor_item->ndat->type));
            return -1;
        }
        value_type_mask = expr_value_type_mask_from_node(anchor_item);
    } else {
        if (0 != (anchor_expr->flags & ASTFLAG_IS_VALUE_TYPE)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                "invalid use of subscript operator on value-type expression");
            return -1;
        }
        value_type_mask = anchor_expr->ndat->u.rexpr.value_type_mask;
    }
    if (-1 == compile_subscript_index(node, slice_start, ctx) ||
        -1 == compile_subscript_index(node, slice_end, ctx)) {
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE;
    compiled_type->u.rexpr.value_type_mask = value_type_mask;
    assert(ast_node_is_rexpr(anchor_expr));
    compiled_type->u.rexpr_op_subscript_common.anchor_expr = anchor_expr;
    compiled_type->u.rexpr_op_subscript_slice.start = *slice_start;
    compiled_type->u.rexpr_op_subscript_slice.end = *slice_end;
    node->ndat = compiled_type;
    return 0;
}

static int
compile_expr_operator_fcall(struct ast_node_hdl *expr,
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
    compiled_type->u.rexpr_op_fcall.builtin = builtin;
    compiled_type->u.rexpr_op_fcall.func_params = func_params;
    compiled_type->u.rexpr_op_fcall.n_func_params = n_params;
    expr->ndat = compiled_type;
    expr->flags |= (NULL != builtin->eval_dpath_fn ?
                    0u : ASTFLAG_IS_VALUE_TYPE);
    return 0;
}

/**
 * @brief compile sizeof operator
 */
static int
compile_expr_operator_sizeof(struct ast_node_hdl *expr,
                             struct compile_ctx *ctx,
                             enum resolve_expect_mask expect_mask)
{
    struct ast_node_data *compiled_type;
    struct ast_node_hdl *operand;
    struct ast_node_hdl *target;
    struct ast_node_hdl *target_item;

    operand = expr->ndat->u.op.operands[0];
    if (-1 == compile_node(operand, ctx,
                           COMPILE_TAG_NODE_TYPE |
                           COMPILE_TAG_NODE_SPAN_SIZE, 0u,
                           RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_SIZEOF;
    compiled_type->flags = ASTFLAG_IS_VALUE_TYPE;
    compiled_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_INTEGER;
    compiled_type->u.rexpr_op.op = expr->ndat->u.op;
    expr->ndat = compiled_type;

    target = ast_node_get_named_expr_target(operand);
    if (AST_NODE_TYPE_REXPR_ITEM == target->ndat->type) {
        // XXX support named expr polymorphism
        target_item = ast_node_get_target_item(target);
        if (0 != (target_item->ndat->u.item.flags
                  & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "invalid use of sizeof operator on dynamic-sized type name\n"
                "(use a dpath expression for computing size dynamically)");
            return -1;
        }
        expr->ndat->u.rexpr_op.op.operands[0] = target_item;
    } else {
        if (0 != (target->flags & ASTFLAG_IS_VALUE_TYPE)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "invalid use of sizeof operator on value-type operand");
            return -1;
        }
        expr->ndat->u.rexpr_op.op.operands[0] = operand;
    }
    return 0;
}

/**
 * @brief compile addrof (&) operator
 */
static int
compile_expr_operator_addrof(struct ast_node_hdl *expr,
                             struct compile_ctx *ctx,
                             enum resolve_expect_mask expect_mask)
{
    struct ast_node_data *compiled_type;
    struct op op;

    if (-1 == compile_node(expr->ndat->u.op.operands[0], ctx,
                           COMPILE_TAG_NODE_TYPE, 0u,
                           RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_ADDROF;
    compiled_type->flags = ASTFLAG_IS_VALUE_TYPE;
    compiled_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_INTEGER;
    compiled_type->u.rexpr_op.op = expr->ndat->u.op;
    expr->ndat = compiled_type;

    op = expr->ndat->u.rexpr_op.op;
    if (0 != (op.operands[0]->flags & ASTFLAG_IS_VALUE_TYPE)) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "invalid use of addrof (&) operator on value-type operand");
        return -1;
    }
    return 0;
}

/**
 * @brief compile ancestor (unary ^) operator
 */
static int
compile_expr_operator_ancestor(struct ast_node_hdl *expr,
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
        if (AST_NODE_TYPE_REXPR_FILTER == target_filter->ndat->type) {
            target = target_filter->ndat->u.rexpr_filter.target;
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
    compiled_type->u.rexpr_op.op = expr->ndat->u.op;
    expr->ndat = compiled_type;
    return 0;
}

static int
compile_expr_star_wildcard(
    struct ast_node_hdl *expr,
    struct compile_ctx *ctx,
    enum resolve_expect_mask expect_mask)
{
    struct ast_node_data *compiled_type;

    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_STAR_WILDCARD;
    compiled_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_UNSET;
    expr->ndat = compiled_type;
    return 0;
}

static int
compile_conditional(struct ast_node_hdl *cond, struct compile_ctx *ctx)
{
    struct ast_node_hdl *cond_expr;

    if (-1 == compile_expr(cond->ndat->u.conditional.cond_expr, ctx, TRUE)) {
        return -1;
    }
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
    if (NULL != cond->ndat->u.conditional.outer_cond
        && -1 == compile_expr(cond->ndat->u.conditional.outer_cond, ctx,
                              FALSE)) {
        return -1;
    }
    return 0;
}

static int
compile_rexpr_interpreter(struct ast_node_hdl *expr,
                          struct compile_ctx *ctx)
{
    if (-1 == compile_interpreter_attributes(
            expr,
            expr->ndat->u.rexpr_interpreter.interpreter,
            &expr->ndat->u.rexpr_interpreter.attr_set, ctx)) {
        return -1;
    }
    // template flag may be removed when compiling OP_SET_FILTER
    expr->ndat->flags |= ASTFLAG_DATA_TEMPLATE;
    return 0;
}

static int
compile_rexpr_named_expr(struct ast_node_hdl *expr,
                         struct compile_ctx *ctx)
{
    struct named_expr *named_expr;
    struct ast_node_hdl *target;

    named_expr = (struct named_expr *)
        expr->ndat->u.rexpr_named_expr.named_expr;
    target = named_expr->expr;
    if (-1 == compile_expr(target, ctx, TRUE)) {
        return -1;
    }
    if (ast_node_is_item(target)) {
        named_expr->expr = ast_node_new_rexpr_item(target, ctx);
        target = named_expr->expr;
    }
    expr->ndat->u.rexpr.value_type_mask =
        expr_value_type_mask_from_node(target);
    expr->flags |= (target->flags & ASTFLAG_IS_VALUE_TYPE);
    return 0;
}

static int
compile_rexpr_polymorphic(struct ast_node_hdl *expr,
                          struct compile_ctx *ctx)
{
    int i;
    struct named_statement_spec *stmt_spec;
    struct field *field;
    struct named_expr *named_expr;
    struct ast_node_hdl *target_expr;
    enum expr_value_type value_type_mask;
    int is_value_type;
    
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
                         RESOLVE_EXPECT_EXPRESSION |
                         RESOLVE_EXPECT_INTERPRETER);
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
    value_type_mask = EXPR_VALUE_TYPE_UNSET;
    is_value_type = TRUE;
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
            target_expr = field->dpath.filter;
            assert(ast_node_is_rexpr(expr));
            break ;
        default:
            assert(0);
        }
        assert(ast_node_is_rexpr(target_expr));
        // possible value types add up for each polymorphic target
        value_type_mask |= target_expr->ndat->u.rexpr.value_type_mask;
        if (0 == (target_expr->flags & ASTFLAG_IS_VALUE_TYPE)) {
            is_value_type = FALSE;
        }
    }
    if (0 != (expr->flags & ASTFLAG_HAS_POLYMORPHIC_ANCHOR)) {
        expr->ndat->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_ANY;
    } else {
        expr->flags |= is_value_type ? ASTFLAG_IS_VALUE_TYPE : 0u;
        expr->ndat->u.rexpr.value_type_mask = value_type_mask;
    }
    return 0;
}

static int
compile_rexpr_field(struct ast_node_hdl *expr, struct compile_ctx *ctx)
{
    struct field *field;

    field = (struct field *)expr->ndat->u.rexpr_field.field;

    if (-1 == compile_dpath(&field->dpath, ctx, COMPILE_TAG_NODE_TYPE, 0u)) {
        return -1;
    }
    expr->ndat->u.rexpr.value_type_mask =
        expr_value_type_mask_from_dpath_node(&field->dpath);
    return 0;
}

static int
compile_rexpr_member(struct ast_node_hdl *expr, struct compile_ctx *ctx)
{
    struct op *op;
    struct named_statement_spec *visible_statements;
    int n_visible_statements;
    struct ast_node_hdl *anchor_expr;
    struct ast_node_hdl *anchor_target;
    enum statement_type lookup_mask;
    const struct ast_node_hdl *anchor_block, *member;
    struct named_statement_spec *stmt_spec;
    struct ast_node_data *resolved_type;

    op = &expr->ndat->u.rexpr_op.op;
    member = op->operands[1];
    /* checked by parser */
    assert(member->ndat->type == AST_NODE_TYPE_IDENTIFIER);

    if (-1 == compile_node(op->operands[0], ctx,
                           COMPILE_TAG_NODE_TYPE, 0u,
                           RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    anchor_expr = op->operands[0];
    anchor_block = ast_node_get_as_type(anchor_expr);
    anchor_target = ast_node_get_named_expr_target(anchor_expr);
    if (NULL != anchor_block) {
        if (anchor_block->ndat->type != AST_NODE_TYPE_BLOCK_DEF) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "invalid use of member operator on non-block dpath");
            return -1;
        }
        if (member->ndat->u.identifier[0] == '@') {
            lookup_mask = STATEMENT_TYPE_ATTRIBUTE;
        } else {
            lookup_mask = STATEMENT_TYPE_NAMED_EXPR | STATEMENT_TYPE_FIELD;
        }
        n_visible_statements = lookup_visible_statements_in_lists(
            lookup_mask,
            member->ndat->u.identifier,
            &anchor_block->ndat->u.block_def.block_stmt_list,
            &visible_statements);
        if (-1 == n_visible_statements) {
            return -1;
        }
        if (0 == n_visible_statements) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &member->loc,
                "no attribute named '%s' exists in block",
                member->ndat->u.identifier);
            semantic_error(SEMANTIC_LOGLEVEL_INFO, &anchor_block->loc,
                           "declared here");
            return -1;
        }
        if (n_visible_statements == 1) {
            stmt_spec = &visible_statements[0];
            resolved_type = new_safe(struct ast_node_data);
            resolved_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_UNSET;
            resolved_type->u.rexpr_member_common.anchor_expr = anchor_expr;
            resolved_type->u.rexpr_member_common.anchor_block =
                (struct ast_node_hdl *)anchor_block;
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
                free(visible_statements);
                return compile_rexpr_named_expr(expr, ctx);
            case STATEMENT_TYPE_FIELD:
                // this is for supporting type-as-expression
                // e.g. sizeof(Type.field)
                //
                // there is probably a cleaner way to support
                // type-as-expressions for future work
                if (AST_NODE_TYPE_REXPR_ITEM == anchor_target->ndat->type) {
                    expr->ndat = ast_node_data_new_rexpr_item(
                        ((struct field *)stmt_spec->nstmt)->dpath.item);
                    return 0;
                }
                resolved_type->type = AST_NODE_TYPE_REXPR_FIELD;
                resolved_type->u.rexpr_field.field =
                    (struct field *)stmt_spec->nstmt;
                free(visible_statements);
                return compile_rexpr_field(expr, ctx);
            default:
                assert(0);
            }
        }
        // n_visible_statements > 1 => polymorphic member
    } else {
        // dynamic anchor => polymorphic member
        visible_statements = NULL;
        n_visible_statements = 0;
    }
    resolved_type = new_safe(struct ast_node_data);
    resolved_type->u.rexpr.value_type_mask = EXPR_VALUE_TYPE_UNSET;
    resolved_type->u.rexpr_member_common.anchor_expr = anchor_expr;
    resolved_type->u.rexpr_member_common.anchor_block =
        (struct ast_node_hdl *)anchor_block;
    resolved_type->type = AST_NODE_TYPE_REXPR_POLYMORPHIC;
    resolved_type->u.rexpr_polymorphic.identifier =
        member->ndat->u.identifier;
    if (NULL != anchor_block) {
        resolved_type->u.rexpr_polymorphic.visible_statements =
            visible_statements;
        resolved_type->u.rexpr_polymorphic.n_visible_statements =
            n_visible_statements;
    } else {
        expr->flags |= ASTFLAG_HAS_POLYMORPHIC_ANCHOR;
    }
    expr->ndat = resolved_type;
    return compile_rexpr_polymorphic(expr, ctx);
}

static int
compile_node_type_int(struct ast_node_hdl *node,
                      struct compile_ctx *ctx,
                      enum resolve_expect_mask expect_mask)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_INTEGER:
        return compile_expr_integer(node, ctx, expect_mask);
    case AST_NODE_TYPE_BOOLEAN:
        return compile_expr_boolean(node, ctx, expect_mask);
    case AST_NODE_TYPE_STRING:
        return compile_expr_string_literal(node, ctx, expect_mask);
    case AST_NODE_TYPE_BLOCK_DEF:
        return compile_block(node, ctx, expect_mask);
    case AST_NODE_TYPE_ARRAY:
        return compile_array(node, ctx, expect_mask);
    case AST_NODE_TYPE_CONDITIONAL:
        return compile_conditional(node, ctx);
    case AST_NODE_TYPE_OP_UPLUS:
    case AST_NODE_TYPE_OP_UMINUS:
    case AST_NODE_TYPE_OP_LNOT:
    case AST_NODE_TYPE_OP_BWNOT:
    case AST_NODE_TYPE_OP_MEMBER:
        return compile_expr_operator(node, 1, ctx);
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
        return compile_expr_operator(node, 2, ctx);
    case AST_NODE_TYPE_OP_SET_FILTER:
        return compile_expr_operator_set_filter(node, ctx);
    case AST_NODE_TYPE_OP_ADDROF:
        return compile_expr_operator_addrof(node, ctx, expect_mask);
    case AST_NODE_TYPE_OP_ANCESTOR:
        return compile_expr_operator_ancestor(node, ctx, expect_mask);
    case AST_NODE_TYPE_OP_SUBSCRIPT:
        return compile_expr_operator_subscript(node, ctx, expect_mask);
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
        return compile_expr_operator_subscript_slice(node, ctx, expect_mask);
    case AST_NODE_TYPE_OP_FCALL:
        return compile_expr_operator_fcall(node, ctx, expect_mask);
    case AST_NODE_TYPE_OP_SIZEOF:
        return compile_expr_operator_sizeof(node, ctx, expect_mask);
    case AST_NODE_TYPE_EXPR_STAR_WILDCARD:
        return compile_expr_star_wildcard(node, ctx, expect_mask);
    case AST_NODE_TYPE_REXPR_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT:
        return compile_rexpr_operator(node, 1, ctx);
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
        return compile_rexpr_operator(node, 2, ctx);
    case AST_NODE_TYPE_REXPR_OP_MEMBER:
        return compile_rexpr_member(node, ctx);
    case AST_NODE_TYPE_REXPR_FIELD:
        return compile_rexpr_field(node, ctx);
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        return compile_rexpr_named_expr(node, ctx);
    case AST_NODE_TYPE_REXPR_POLYMORPHIC:
        return compile_rexpr_polymorphic(node, ctx);
    case AST_NODE_TYPE_REXPR_INTERPRETER:
        return compile_rexpr_interpreter(node, ctx);
    default:
        /* nothing to do */
        return 0;
    }
}

static int
compile_node_type(struct ast_node_hdl *node,
                  struct compile_ctx *ctx,
                  enum resolve_expect_mask expect_mask)
{
    struct ast_node_data *old_data;

    do {
        old_data = node->ndat;
        if (-1 == compile_node_type_int(node, ctx, expect_mask)) {
            return -1;
        }
    } while (old_data != node->ndat);
    return 0;
}

static int
compile_span_size_block(struct ast_node_hdl *item, struct compile_ctx *ctx)
{
    struct attribute_set *attr_set;
    struct named_expr *attr;
    struct ast_node_hdl *min_span_expr;
    struct ast_node_hdl *max_span_expr;
    const struct statement_list *field_list;
    struct field *field;
    struct dpath_node *field_type;
    int64_t min_span_size;
    int dynamic_span;
    int dynamic_used;
    int contains_last_attr;
    int child_uses_slack;
    int child_spreads_slack;
    int child_conditionally_spreads_slack;
    int child_fills_slack;
    int child_conditionally_fills_slack;
    struct field *first_trailer_field;

    attr_set = &item->ndat->u.item.attr_set;

    /* - Compute the minimum span size from the sum (struct) or
       max (union) of fields' minimum span sizes. If size is
       static, minimum size is actual size.

       - If an unconditional span size is given by an expression,
       resolve it

       - Set the dynamic flag if actual size may be greater than
       the minimum
    */

    min_span_size = 0;
    dynamic_span = FALSE;
    dynamic_used = FALSE;
    contains_last_attr = FALSE;
    child_uses_slack = FALSE;
    child_spreads_slack = FALSE;
    child_conditionally_spreads_slack = FALSE;
    child_fills_slack = FALSE;
    child_conditionally_fills_slack = FALSE;

    min_span_expr = NULL;
    max_span_expr = NULL;
    attr = attr_set->attrs[REF_ITEM_MINSPAN];
    if (NULL != attr) {
        if (NULL == attr->nstmt.stmt.cond) {
            min_span_expr = attr->expr;
        }
        dynamic_span = TRUE;
    }
    attr = attr_set->attrs[REF_ITEM_MAXSPAN];
    if (NULL != attr) {
        if (NULL == attr->nstmt.stmt.cond) {
            max_span_expr = attr->expr;
        }
        dynamic_span = TRUE;
    }
    attr = attr_set->attrs[REF_ITEM_SPAN];
    if (NULL != attr) {
        if (NULL == attr->nstmt.stmt.cond) {
            min_span_expr = attr->expr;
            max_span_expr = attr->expr;
        } else {
            dynamic_span = TRUE;
        }
    }
    attr = attr_set->attrs[REF_ITEM_LAST];
    if (NULL != attr) {
        contains_last_attr = TRUE;
    }
    field_list = item->ndat->u.block_def.block_stmt_list.field_list;
    first_trailer_field = NULL;
    STATEMENT_FOREACH(field, field, field_list, list) {
        compile_field(field, ctx,
                      COMPILE_TAG_NODE_TYPE |
                      COMPILE_TAG_NODE_SPAN_SIZE, 0u);
    }
    if (!compile_continue(ctx)) {
        return -1;
    }
    STATEMENT_FOREACH(field, field, field_list, list) {
        field_type = &field->dpath;
        if (0 != (field_type->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            dynamic_used = TRUE;
        } else if (NULL == field->nstmt.stmt.cond) {
            /* only update min span size if field is not conditional */
            assert(SPAN_SIZE_UNDEF != field_type->u.item.min_span_size);
            if (BLOCK_TYPE_UNION == item->ndat->u.block_def.type) {
                min_span_size = MAX(min_span_size,
                                    field_type->u.item.min_span_size);
            } else /* struct */ {
                min_span_size += field_type->u.item.min_span_size;
            }
        } else {
            // if at least one conditional field is present, the
            // used size is dynamic
            dynamic_used = TRUE;
        }
        if (0 != (field_type->u.item.flags & ITEMFLAG_USES_SLACK)) {
            child_uses_slack = TRUE;
        }
        if (0 != (field_type->u.item.flags & ITEMFLAG_SPREADS_SLACK)) {
            if (NULL != field->nstmt.stmt.cond) {
                child_conditionally_spreads_slack = TRUE;
            } else {
                child_spreads_slack = TRUE;
            }
        }
        if (0 != (field_type->u.item.flags & ITEMFLAG_FILLS_SLACK)) {
            if (NULL != field->nstmt.stmt.cond) {
                child_conditionally_fills_slack = TRUE;
            } else {
                child_fills_slack = TRUE;
            }
        }
        if (0 != (field_type->u.item.flags
                  & ITEMFLAG_CONDITIONALLY_SPREADS_SLACK)) {
            child_conditionally_spreads_slack = TRUE;
        }
        if (0 != (field_type->u.item.flags
                  & ITEMFLAG_CONDITIONALLY_FILLS_SLACK)) {
            child_conditionally_fills_slack = TRUE;
        }
        if (0 != (field_type->u.item.flags
                  & (ITEMFLAG_SPREADS_SLACK |
                     ITEMFLAG_CONDITIONALLY_SPREADS_SLACK))
            || NULL != field->nstmt.stmt.cond) {
            first_trailer_field = NULL;
        } else if (BLOCK_TYPE_STRUCT == item->ndat->u.block_def.type
                   && (child_spreads_slack ||
                       child_conditionally_spreads_slack)
                   && NULL == first_trailer_field
                   && 0 == (field_type->u.item.flags
                            & (ITEMFLAG_SPREADS_SLACK |
                               ITEMFLAG_CONDITIONALLY_SPREADS_SLACK))) {
            first_trailer_field = field;
        }
    }
    if (child_spreads_slack) {
        STATEMENT_FOREACH(field, field, field_list, list) {
            field_type = &field->dpath;
            if (0 == (field_type->u.item.flags
                      & (ITEMFLAG_SPREADS_SLACK |
                         ITEMFLAG_CONDITIONALLY_SPREADS_SLACK))) {
                field->nstmt.stmt.stmt_flags |= FIELD_FLAG_HEADER;
            } else {
                break ;
            }
        }
    }
    for (field = first_trailer_field;
         NULL != field;
         field = (struct field *)
             TAILQ_NEXT((struct statement *)field, list)) {
        field->nstmt.stmt.stmt_flags |= FIELD_FLAG_TRAILER;
    }
    if (NULL != min_span_expr) {
        assert(EXPR_VALUE_TYPE_INTEGER
               == min_span_expr->ndat->u.rexpr.value_type_mask);
        if (AST_NODE_TYPE_REXPR_NATIVE == min_span_expr->ndat->type) {
            int64_t user_min_span_size;

            user_min_span_size = min_span_expr->ndat->u.rexpr_native.value.integer;
            if (user_min_span_size < min_span_size) {
                semantic_error(SEMANTIC_LOGLEVEL_ERROR,
                               &min_span_expr->loc,
                               "declared min span size too small to "
                               "hold all contained fields (requires %"
                               PRIi64" bytes but only spans %"PRIi64")",
                               min_span_size, user_min_span_size);
                return -1;
            }
            min_span_size = user_min_span_size;
        } else {
            dynamic_span = TRUE;
        }
    }
    if (NULL != max_span_expr) {
        assert(EXPR_VALUE_TYPE_INTEGER
               == max_span_expr->ndat->u.rexpr.value_type_mask);
        if (AST_NODE_TYPE_REXPR_NATIVE == max_span_expr->ndat->type) {
            int64_t user_max_span_size;

            user_max_span_size = max_span_expr->ndat->u.rexpr_native.value.integer;
            if (user_max_span_size < min_span_size) {
                semantic_error(SEMANTIC_LOGLEVEL_ERROR,
                               &max_span_expr->loc,
                               "declared max span size smaller than "
                               "min span size (%"PRIi64" bytes < %"
                               PRIi64")",
                               user_max_span_size, min_span_size);
                return -1;
            }
            // override
            dynamic_span = (user_max_span_size != min_span_size);
        } else {
            dynamic_span = TRUE;
        }
    }
    // when no span expression is declared, span space matches
    // used space
    if (NULL == min_span_expr && NULL == max_span_expr && !dynamic_span) {
        dynamic_span = dynamic_used;
    }
    // if max span expression exists and is inconditional, slack
    // space claimed by children is allocated greedily by their
    // parent up to the max allowable, otherwise it has to be
    // claimed as well
    if (NULL == max_span_expr) {
        // if any field uses the remaining slack space, the block also
        // does, otherwise the same idea applies with fields that may
        // use the slack space (those that have filters defining their
        // span size)
        if (child_uses_slack) {
            item->ndat->u.item.flags |= ITEMFLAG_USES_SLACK;
        }
        if (child_spreads_slack) {
            item->ndat->u.item.flags |= ITEMFLAG_SPREADS_SLACK;
        } else if (child_conditionally_spreads_slack) {
            item->ndat->u.item.flags |= ITEMFLAG_CONDITIONALLY_SPREADS_SLACK;
        }
        if (child_fills_slack) {
            item->ndat->u.item.flags |= ITEMFLAG_FILLS_SLACK;
        } else if (child_conditionally_fills_slack) {
            item->ndat->u.item.flags |= ITEMFLAG_CONDITIONALLY_FILLS_SLACK;
        }
    }
    item->ndat->u.item.min_span_size = min_span_size;
    if (dynamic_span) {
        item->ndat->u.item.flags |= ITEMFLAG_IS_SPAN_SIZE_DYNAMIC;
    }
    if (dynamic_used) {
        item->ndat->u.item.flags |= ITEMFLAG_IS_USED_SIZE_DYNAMIC;
    }
    if (contains_last_attr) {
        item->flags |= ASTFLAG_CONTAINS_LAST_ATTR;
    }
    return 0;
}

static int
compile_span_size_array(struct ast_node_hdl *array, struct compile_ctx *ctx)
{
    struct dpath_node *item_dpath;
    struct ast_node_hdl *item_type;
    struct ast_node_hdl *item_count_expr;
    int64_t item_count;
    int64_t min_span_size;
    int dynamic_span;

    item_count_expr =
        ast_node_get_named_expr_target(array->ndat->u.array.item_count);
    if (NULL != item_count_expr) {
        if (-1 == compile_expr(item_count_expr, ctx, TRUE)) {
            return -1;
        }
        // XXX check all polymorphic named expr targets
        if (0 == (EXPR_VALUE_TYPE_INTEGER
                  & item_count_expr->ndat->u.rexpr.value_type_mask)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &item_count_expr->loc,
                "invalid array size type: expect an integer, got '%s'",
                expr_value_type_str(
                    item_count_expr->ndat->u.rexpr.value_type_mask));
            return -1;
        }
    }
    item_dpath = &array->ndat->u.array.item_type;
    item_type = ast_node_get_target_item(item_dpath->item);
    assert(ast_node_is_item(item_type));
    if (NULL != item_count_expr
        && item_count_expr->ndat->type == AST_NODE_TYPE_REXPR_NATIVE
        && item_count_expr->ndat->u.rexpr_native.value.integer > 0) {
        // compile item type as a dependency because the array
        // contains a fixed number of at least one item
        if (-1 == compile_dpath(item_dpath, ctx,
                                COMPILE_TAG_NODE_TYPE |
                                COMPILE_TAG_NODE_SPAN_SIZE, 0u)) {
            return -1;
        }
        assert(SPAN_SIZE_UNDEF != item_type->ndat->u.item.min_span_size);
        assert(EXPR_VALUE_TYPE_INTEGER
               == item_count_expr->ndat->u.rexpr.value_type_mask);
        assert(SPAN_SIZE_UNDEF != item_type->ndat->u.item.min_span_size);
        item_count = item_count_expr->ndat->u.rexpr_native.value.integer;
        min_span_size = item_count * item_type->ndat->u.item.min_span_size;
        dynamic_span =
            (0 != (item_dpath->u.item.flags & ASTFLAG_CONTAINS_LAST_ATTR)
             || 0 != (item_dpath->u.item.flags
                      & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC));
    } else {
        // schedule compilation of item type and size without
        // depending on it, so to allow recursive nesting of items and
        // possibly empty arrays
        if (-1 == compile_dpath(item_dpath, ctx,
                                0u, (COMPILE_TAG_NODE_TYPE |
                                     COMPILE_TAG_NODE_SPAN_SIZE))) {
            return -1;
        }
        min_span_size = 0;
        dynamic_span = TRUE;
        array->ndat->u.item.min_span_size = 0;
        array->ndat->u.item.flags |= (ITEMFLAG_IS_SPAN_SIZE_DYNAMIC |
                                      ITEMFLAG_IS_USED_SIZE_DYNAMIC);
    }
    array->ndat->u.item.min_span_size = min_span_size;
    if (dynamic_span) {
        array->ndat->u.item.flags |= (ITEMFLAG_IS_SPAN_SIZE_DYNAMIC |
                                      ITEMFLAG_IS_USED_SIZE_DYNAMIC);
    }
    if (0 == (item_type->flags & ASTFLAG_CONTAINS_LAST_ATTR)) {
        if (NULL == item_count_expr) {
            // because the array items can take more than one byte of
            // space, they may not allow to fill the whole slack
            // space, so we don't set ITEMFLAG_FILLS_SLACK (meaning
            // used size may be smaller than max span size)
            array->ndat->u.item.flags |= (ITEMFLAG_USES_SLACK |
                                          ITEMFLAG_SPREADS_SLACK);
        }
    }
    return 0;
}

static int
compile_span_size_byte(struct ast_node_hdl *byte, struct compile_ctx *ctx)
{
    return 0;
}

static int
compile_span_size_byte_array(struct ast_node_hdl *byte_array,
                             struct compile_ctx *ctx)
{
    struct ast_node_hdl *size_expr;
    int64_t min_span_size;

    size_expr = byte_array->ndat->u.byte_array.size;
    if (NULL != size_expr) {
        if (-1 == compile_expr(size_expr, ctx, TRUE)) {
            return -1;
        }
        assert(ast_node_is_rexpr(size_expr));
        if (0 == (EXPR_VALUE_TYPE_INTEGER
                  & size_expr->ndat->u.rexpr.value_type_mask)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &size_expr->loc,
                "invalid byte array size type: expect an integer, "
                "got '%s'",
                expr_value_type_str(size_expr->ndat->u.rexpr.value_type_mask));
            return -1;
        }
    }
    if (NULL != size_expr
        && AST_NODE_TYPE_REXPR_NATIVE == size_expr->ndat->type) {
        min_span_size = size_expr->ndat->u.rexpr_native.value.integer;
    } else {
        min_span_size = 0;
        byte_array->ndat->u.item.flags |= (ITEMFLAG_IS_SPAN_SIZE_DYNAMIC |
                                           ITEMFLAG_IS_USED_SIZE_DYNAMIC);
    }
    if (NULL == size_expr) {
        // for now we don't know if there's a filter defining span
        // size on top of the byte array which limits the amount of
        // slack used: assume for now it will use the entire space;
        // compilation of a size-defining filter may later adjust the
        // slack flags
        byte_array->ndat->u.item.flags |= (ITEMFLAG_USES_SLACK |
                                           ITEMFLAG_SPREADS_SLACK |
                                           ITEMFLAG_FILLS_SLACK);
    }
    byte_array->ndat->u.item.min_span_size = min_span_size;
    return 0;
}

static int
compile_span_size_rexpr_item(struct ast_node_hdl *filter,
                             struct compile_ctx *ctx)
{
    return compile_node(filter->ndat->u.rexpr_item.item_type, ctx,
                        COMPILE_TAG_NODE_TYPE |
                        COMPILE_TAG_NODE_SPAN_SIZE, 0u,
                        RESOLVE_EXPECT_TYPE);
}

enum filter_defining_size_status {
    FILTER_ALWAYS_DEFINES_SIZE,
    FILTER_CONDITIONALLY_DEFINES_SIZE,
    FILTER_NEVER_DEFINES_SIZE,
};

static enum filter_defining_size_status
get_filter_defining_size_status(struct ast_node_hdl *filter);

static enum filter_defining_size_status
get_filter_defining_size_status_interpreter(struct ast_node_hdl *filter)
{
    return NULL != filter->ndat->u.rexpr_interpreter.get_size_func ?
        FILTER_ALWAYS_DEFINES_SIZE : FILTER_NEVER_DEFINES_SIZE;
}
        
static enum filter_defining_size_status
get_filter_defining_size_status_named_expr(struct ast_node_hdl *filter)
{
    const struct named_expr *named_expr;
    enum filter_defining_size_status target_status;

    named_expr = filter->ndat->u.rexpr_named_expr.named_expr;
    target_status = get_filter_defining_size_status(named_expr->expr);
    switch (target_status) {
    case FILTER_ALWAYS_DEFINES_SIZE:
        return NULL != named_expr->nstmt.stmt.cond ?
            FILTER_CONDITIONALLY_DEFINES_SIZE : FILTER_ALWAYS_DEFINES_SIZE;
    case FILTER_CONDITIONALLY_DEFINES_SIZE:
    case FILTER_NEVER_DEFINES_SIZE:
        return target_status;
    default:
        assert(0);
    }
}

static enum filter_defining_size_status
get_filter_defining_size_status_polymorphic(struct ast_node_hdl *filter)
{
    return FILTER_CONDITIONALLY_DEFINES_SIZE;
}

static enum filter_defining_size_status
get_filter_defining_size_status(struct ast_node_hdl *filter)
{
    switch (filter->ndat->type) {
    case AST_NODE_TYPE_REXPR_INTERPRETER:
        return get_filter_defining_size_status_interpreter(filter);
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        return get_filter_defining_size_status_named_expr(filter);
    case AST_NODE_TYPE_REXPR_POLYMORPHIC:
        return get_filter_defining_size_status_polymorphic(filter);
    default:
        return FILTER_NEVER_DEFINES_SIZE;
    }
}

static int
compile_span_size_rexpr_filter(struct ast_node_hdl *filter,
                               struct compile_ctx *ctx)
{
    struct ast_node_hdl *target;
    struct ast_node_hdl *filter_expr;
    struct ast_node_hdl *target_item;

    target = filter->ndat->u.rexpr_filter.target;
    filter_expr = filter->ndat->u.rexpr_filter.filter_expr;
    compile_node(target, ctx,
                 COMPILE_TAG_NODE_TYPE |
                 COMPILE_TAG_NODE_SPAN_SIZE, 0u,
                 RESOLVE_EXPECT_TYPE |
                 RESOLVE_EXPECT_INTERPRETER |
                 RESOLVE_EXPECT_DPATH_EXPRESSION);
    compile_node(filter_expr, ctx,
                 COMPILE_TAG_NODE_TYPE |
                 COMPILE_TAG_NODE_SPAN_SIZE, 0u,
                 RESOLVE_EXPECT_INTERPRETER);
    if (!compile_continue(ctx)) {
        return -1;
    }
    if (AST_NODE_TYPE_REXPR_ITEM == target->ndat->type) {
        target_item = target->ndat->u.rexpr_item.item_type;
        if (0 != (target_item->ndat->u.item.flags & ITEMFLAG_SPREADS_SLACK)) {
            switch (get_filter_defining_size_status(filter_expr)) {
            case FILTER_ALWAYS_DEFINES_SIZE:
                // this filter always defines the span size of its
                // item, so the item is always using the
                // filter-defined space, nevertheless it claims some
                // slack space and needs to know how much is available
                // so keeps ITEMFLAG_USES_SLACK flag
                target_item->ndat->u.item.flags
                    &= ~(ITEMFLAG_SPREADS_SLACK |
                         ITEMFLAG_CONDITIONALLY_SPREADS_SLACK |
                         ITEMFLAG_FILLS_SLACK |
                         ITEMFLAG_CONDITIONALLY_FILLS_SLACK);
                break ;
            case FILTER_CONDITIONALLY_DEFINES_SIZE:
                // filter defines the span size conditionally, the
                // target item may or not spread on or fill the slack
                // space
                if (0 != (target_item->ndat->u.item.flags
                          & ITEMFLAG_SPREADS_SLACK)) {
                    target_item->ndat->u.item.flags
                        &= ~ITEMFLAG_SPREADS_SLACK;
                    target_item->ndat->u.item.flags
                        |= ITEMFLAG_CONDITIONALLY_SPREADS_SLACK;
                }
                if (0 != (target_item->ndat->u.item.flags
                          & ITEMFLAG_FILLS_SLACK)) {
                    target_item->ndat->u.item.flags
                        &= ~ITEMFLAG_FILLS_SLACK;
                    target_item->ndat->u.item.flags
                        |= ITEMFLAG_CONDITIONALLY_FILLS_SLACK;
                }
                break ;
            case FILTER_NEVER_DEFINES_SIZE:
                // keep slack flags as is since filters do not alter
                // the item spread behavior
                break ;
            default:
                assert(0);
            }
        }
    }
    return 0;
}

static int
compile_span_size_rexpr_named_expr(struct ast_node_hdl *filter,
                                   struct compile_ctx *ctx)
{
    const struct named_expr *named_expr;

    named_expr = filter->ndat->u.rexpr_named_expr.named_expr;
    return compile_node(named_expr->expr, ctx,
                        COMPILE_TAG_NODE_TYPE |
                        COMPILE_TAG_NODE_SPAN_SIZE, 0u,
                        RESOLVE_EXPECT_TYPE |
                        RESOLVE_EXPECT_INTERPRETER |
                        RESOLVE_EXPECT_DPATH_EXPRESSION);
}

static int
compile_node_span_size(struct ast_node_hdl *node,
                       struct compile_ctx *ctx)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        return compile_span_size_block(node, ctx);
    case AST_NODE_TYPE_ARRAY:
        return compile_span_size_array(node, ctx);
    case AST_NODE_TYPE_BYTE:
        return compile_span_size_byte(node, ctx);
    case AST_NODE_TYPE_BYTE_ARRAY:
        return compile_span_size_byte_array(node, ctx);
    case AST_NODE_TYPE_REXPR_ITEM:
        return compile_span_size_rexpr_item(node, ctx);
    case AST_NODE_TYPE_REXPR_FILTER:
        return compile_span_size_rexpr_filter(node, ctx);
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        return compile_span_size_rexpr_named_expr(node, ctx);
    default:
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
compile_node_cb(struct compile_ctx *ctx,
                enum resolve_expect_mask expect_mask,
                struct dep_resolver_node *_node,
                dep_resolver_tagset_t tags,
                void *arg)
{
    struct ast_node_hdl *node;
    dep_resolver_tagset_t resolved_tags = 0;

    node = container_of(_node, struct ast_node_hdl, dr_node);
    if (0 != (tags & COMPILE_TAG_NODE_TYPE)) {
        if (-1 == compile_node_type(node, ctx, expect_mask)) {
            return resolved_tags;
        }
        resolved_tags |= COMPILE_TAG_NODE_TYPE;
    }
    if (0 != (tags & COMPILE_TAG_NODE_SPAN_SIZE)) {
        if (-1 == compile_node_span_size(node, ctx)) {
            return resolved_tags;
        }
        resolved_tags |= COMPILE_TAG_NODE_SPAN_SIZE;
    }
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
    if (ast_node_is_item(expr)) {
        expr = ast_node_new_rexpr_item(expr, ctx);
    }
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
    node->u.item = node->item->ndat->u.item;
    return 0;
}


void
dpath_node_reset(struct dpath_node *dpath)
{
    memset(dpath, 0, sizeof (*dpath));
    dpath->u.item.min_span_size = SPAN_SIZE_UNDEF;
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
    case AST_NODE_TYPE_REXPR_FILTER:
    case AST_NODE_TYPE_REXPR_FIELD:
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
    case AST_NODE_TYPE_REXPR_POLYMORPHIC:
    case AST_NODE_TYPE_REXPR_BUILTIN:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
    case AST_NODE_TYPE_REXPR_OP_MEMBER:
    case AST_NODE_TYPE_REXPR_OP_FCALL:
    case AST_NODE_TYPE_REXPR_FILE:
    case AST_NODE_TYPE_REXPR_SELF:
    case AST_NODE_TYPE_REXPR_STAR_WILDCARD:
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_REXPR_ITEM:
        return TRUE;
    default:
        return FALSE;
    }
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

        anchor_target = ast_node_get_target_item(
            node->ndat->u.rexpr_op_subscript_common.anchor_expr);
        if (NULL == anchor_target) {
            return NULL;
        }
        switch (anchor_target->ndat->type) {
        case AST_NODE_TYPE_ARRAY:
            return ast_node_get_target_item(
                anchor_target->ndat->u.array.item_type.item);
        case AST_NODE_TYPE_ARRAY_SLICE:
        case AST_NODE_TYPE_BYTE_SLICE:
            return anchor_target;
        case AST_NODE_TYPE_BYTE_ARRAY:
            return AST_NODE_BYTE;
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
    case AST_NODE_TYPE_REXPR_FIELD:
        return ast_node_get_target_item(
            node->ndat->u.rexpr_field.field->dpath.item);
    case AST_NODE_TYPE_REXPR_FILTER:
        return ast_node_get_target_item(node->ndat->u.rexpr_filter.target);
    case AST_NODE_TYPE_REXPR_ITEM:
    case AST_NODE_TYPE_REXPR_FILE:
    case AST_NODE_TYPE_REXPR_SELF:
        return node->ndat->u.rexpr_item.item_type;
    case AST_NODE_TYPE_REXPR_OP_ANCESTOR:
        return ast_node_get_target_item(node->ndat->u.rexpr_op.op.operands[0]);
    default:
        return NULL;
    }
}

struct ast_node_hdl *
ast_node_get_target_type(struct ast_node_hdl *node)
{
    if (NULL == node || ast_node_is_item(node)) {
        return node;
    }
    switch (node->ndat->type) {
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        return ast_node_get_target_type(
            node->ndat->u.rexpr_named_expr.named_expr->expr);
    case AST_NODE_TYPE_REXPR_FILTER:
        return ast_node_get_target_type(node->ndat->u.rexpr_filter.target);
    case AST_NODE_TYPE_REXPR_ITEM:
        return node->ndat->u.rexpr_item.item_type;
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
        if (NULL != node->ndat->u.rexpr_field.field->dpath.filter) {
            return ast_node_get_target_filter(
                node->ndat->u.rexpr_field.field->dpath.filter);
        } else {
            return ast_node_get_target_filter(
                node->ndat->u.rexpr_field.field->dpath.item);
        }
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT: {
        struct ast_node_hdl *anchor_target;

        anchor_target = ast_node_get_target_item(
            node->ndat->u.rexpr_op_subscript_common.anchor_expr);
        if (NULL == anchor_target) {
            return NULL;
        }
        switch (anchor_target->ndat->type) {
        case AST_NODE_TYPE_ARRAY:
            return ast_node_get_target_filter(
                anchor_target->ndat->u.array.item_type.filter);
        case AST_NODE_TYPE_ARRAY_SLICE:
        case AST_NODE_TYPE_BYTE_SLICE:
            return anchor_target;
        case AST_NODE_TYPE_BYTE_ARRAY:
            return AST_NODE_BYTE;
        default:
            assert(0);
        }
    }
    case AST_NODE_TYPE_REXPR_OP_ANCESTOR:
        return ast_node_get_target_filter(
            node->ndat->u.rexpr_op.op.operands[0]);
    case AST_NODE_TYPE_REXPR_FILTER:
        return ast_node_get_target_filter(
            node->ndat->u.rexpr_filter.filter_expr);
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_REXPR_ITEM:
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

int
ast_node_is_container(const struct ast_node_hdl *node)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
        return TRUE;
    default:
        return FALSE;
    }
}

int
ast_node_is_origin_container(const struct ast_node_hdl *node)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
        return TRUE;
    default:
        return FALSE;
    }
}

int
ast_node_is_byte_container(const struct ast_node_hdl *node)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
        return TRUE;
    default:
        return FALSE;
    }
}

int
ast_node_is_subscriptable_container(const struct ast_node_hdl *node)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
        return TRUE;
    default:
        return FALSE;
    }
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
    case AST_NODE_TYPE_BLOCK_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
        return TRUE;
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
    return ast_node_is_item(node)
        || AST_NODE_TYPE_REXPR_ITEM == node->ndat->type;
}


int
ast_node_is_filter(const struct ast_node_hdl *node)
{
    if (NULL == node) {
        return FALSE;
    }
    switch (node->ndat->type) {
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_REXPR_ITEM:
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

    case AST_NODE_TYPE_REXPR_FILTER:
        as_type = ast_node_get_as_type(
            expr->ndat->u.rexpr_filter.filter_expr);
        if (NULL != as_type) {
            return as_type;
        }
        return ast_node_get_as_type(expr->ndat->u.rexpr_filter.target);

    case AST_NODE_TYPE_REXPR_ITEM:
    case AST_NODE_TYPE_REXPR_FILE:
    case AST_NODE_TYPE_REXPR_SELF:
        return expr->ndat->u.rexpr_item.item_type;

    case AST_NODE_TYPE_REXPR_FIELD:
        if (NULL == expr->ndat->u.rexpr_field.field->dpath.filter) {
            return expr->ndat->u.rexpr_field.field->dpath.item;
        }
        return ast_node_get_as_type(
            expr->ndat->u.rexpr_field.field->dpath.filter);

    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT: {
        struct ast_node_hdl *anchor_target;

        anchor_target = ast_node_get_as_type(
            expr->ndat->u.rexpr_op_subscript_common.anchor_expr);
        if (NULL == anchor_target) {
            return NULL;
        }
        switch (anchor_target->ndat->type) {
        case AST_NODE_TYPE_ARRAY:
            return dpath_node_get_as_type(
                &anchor_target->ndat->u.array.item_type);
        case AST_NODE_TYPE_ARRAY_SLICE:
        case AST_NODE_TYPE_BYTE_SLICE:
            return anchor_target;
        case AST_NODE_TYPE_BYTE_ARRAY:
            return AST_NODE_BYTE;
        default:
            assert(0);
        }
    }
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        return ast_node_get_as_type(
            expr->ndat->u.rexpr_op_subscript_common.anchor_expr);

    case AST_NODE_TYPE_OP_SET_FILTER:
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
    case AST_NODE_TYPE_BLOCK_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_BYTE_ARRAY:
        assert(SPAN_SIZE_UNDEF != node->ndat->u.item.min_span_size);
        return node->ndat->u.item.min_span_size;
    default:
        return 0;
    }
    /*NOT REACHED*/
}

int
ast_node_is_slack(const struct ast_node_hdl *node)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
        if (NULL == node->ndat->u.array.item_count) {
            return TRUE;
        }
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
        if (NULL == node->ndat->u.byte_array.size) {
            return TRUE;
        }
        break ;
    default:
        break ;
    }
    return FALSE;
}

struct ast_node_hdl *
block_get_first_attribute(const struct ast_node_hdl *block,
                          const char *attr_name)
{
    struct named_expr *attr_stmt;

    STATEMENT_FOREACH(
        named_expr, attr_stmt,
        block->ndat->u.block_def.block_stmt_list.attribute_list, list) {
        if (0 == strcmp(attr_stmt->nstmt.name, attr_name)) {
            return attr_stmt->expr;
        }
    }
    return NULL;
}

int
ast_node_is_indexed(const struct ast_node_hdl *node)
{
    const struct ast_node_hdl *target;

    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
        target = dpath_node_get_as_type(&node->ndat->u.array.item_type);
        if (AST_NODE_TYPE_BLOCK_DEF != target->ndat->type) {
            return FALSE;
        }
        return NULL != block_get_first_attribute(target, "@key");
    default:
        return FALSE;
    }
}

struct ast_node_hdl *
ast_node_get_key_expr(const struct ast_node_hdl *node)
{
    const struct ast_node_hdl *target;

    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
        target = dpath_node_get_as_type(&node->ndat->u.array.item_type);
        if (AST_NODE_TYPE_BLOCK_DEF != target->ndat->type) {
            return NULL;
        }
        // TODO: multiple or conditional key expressions currently not
        // supported (needs proper support in index cache)

        return block_get_first_attribute(target, "@key");
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
fdump_ast_recur(struct ast_node_hdl *node, int depth,
                struct list_of_visible_refs *visible_refs, FILE *stream);
static void
dump_block_recur(const struct ast_node_hdl *block,
                 int depth,
                 struct list_of_visible_refs *outer_refs,
                 FILE *stream);
static void
dump_block_stmt_list_recur(const struct ast_node_hdl *block,
                           const struct block_stmt_list *block_lists,
                           int depth,
                           struct list_of_visible_refs *outer_refs,
                           FILE *stream);
static void
dump_ast_type(const struct ast_node_hdl *node, int depth,
              struct list_of_visible_refs *visible_refs, FILE *stream);

void
dump_ast_location(struct ast_node_hdl *node)
{
    fdump_ast_location(node, stdout);
}

void
fdump_ast_location(struct ast_node_hdl *node, FILE *out)
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
dump_ast(struct ast_node_hdl *root)
{
    fdump_ast(root, stdout);
}

void
fdump_ast(struct ast_node_hdl *root, FILE *out)
{
    if (NULL == root) {
        fprintf(out, "<null>\n");
        return ;
    }
    fdump_ast_location(root, out);
    fdump_ast_recur(root, 0, NULL, out);
}
void
dump_block(const struct ast_node_hdl *block, FILE *out)
{
    dump_block_recur(block, 0, NULL, out);
}

static void
dump_ast_rexpr(const struct ast_node_hdl *node, FILE *out) {
    fprintf(out, "value_type: %s (%d)",
            expr_value_type_str(node->ndat->u.rexpr.value_type_mask),
            node->ndat->u.rexpr.value_type_mask);
}

static void
dump_ast_rexpr_member(const struct ast_node_hdl *node, int depth,
                      struct list_of_visible_refs *visible_refs, FILE *out)
{
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
dump_ast_item(struct ast_node_hdl *node, int depth,
              struct list_of_visible_refs *visible_refs, FILE *out)
{
}

static void
dump_ast_item_info(struct ast_node_hdl *node, FILE *out)
{
    fprintf(out, "min span size: %s%s%s",
            span_size_str(node->ndat->u.item.min_span_size),
            (0 != (node->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC) ?
             " (dynamic span)" : ""),
            (0 != (node->ndat->u.item.flags & ITEMFLAG_IS_USED_SIZE_DYNAMIC) ?
             " (dynamic used)" : ""));
}

static void
dump_ast_container(struct ast_node_hdl *node, int depth,
                   struct list_of_visible_refs *visible_refs, FILE *out)
{
    dump_ast_item(node, depth, visible_refs, out);
}

static void
fdump_ast_recur(struct ast_node_hdl *node, int depth,
                struct list_of_visible_refs *visible_refs, FILE *out)
{
    static struct ast_node_data dummy_empty_node_data = {
        .type = AST_NODE_TYPE_NONE,
    };
    static struct ast_node_hdl dummy_empty_node = {
        .ndat = &dummy_empty_node_data,
    };

    if (NULL == node) {
        node = &dummy_empty_node;
    }
    dump_ast_type(node, depth, visible_refs, out);
    if (node == &dummy_empty_node
        || 0 != (node->flags & ASTFLAG_DUMPING)) {
        fprintf(out, "\n");
        return ;
    }
    node->flags |= ASTFLAG_DUMPING;
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
    case AST_NODE_TYPE_BLOCK_DEF:
        fprintf(out, "filter type: %s, block type: %s, ",
                node->ndat->u.block_def.filter_type,
                block_type_str(node->ndat->u.block_def.type));
        dump_ast_item_info(node, out);
        fprintf(out, "\n");
        dump_block_recur(node, depth + 1, visible_refs, out);
        break ;
    case AST_NODE_TYPE_ARRAY:
        fprintf(out, "\n%*s\\_ ", (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_item_info(node, out);
        fprintf(out, ", item type:\n");
        fdump_ast_recur(node->ndat->u.array.item_type.item, depth + 2,
                        visible_refs, out);
        if (NULL != node->ndat->u.array.item_type.filter) {
            fprintf(out, "%*s\\_ filter:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_type(node->ndat->u.array.item_type.filter, depth + 2,
                          visible_refs, out);
            fprintf(out, "\n");
        }
        fprintf(out, "%*s\\_ value count:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->ndat->u.array.item_count, depth + 2, visible_refs,
                        out);
        dump_ast_container(node, depth, visible_refs, out);
        break ;
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
        fprintf(out, "\n%*s\\_ static node (%s)\n",
                (depth + 1) * INDENT_N_SPACES, "",
                ast_node_type_str(node->ndat->type));
        break ;
    case AST_NODE_TYPE_BYTE:
        fprintf(out, "\n");
        dump_ast_item(node, depth, visible_refs, out);
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
        dump_ast_item_info(node, out);
        fprintf(out, ", byte count:\n");
        fdump_ast_recur(node->ndat->u.byte_array.size, depth + 2, visible_refs,
                        out);
        dump_ast_container(node, depth, visible_refs, out);
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
    case AST_NODE_TYPE_OP_SET_FILTER:
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
    case AST_NODE_TYPE_REXPR_INTERPRETER: {
        const struct interpreter *interpreter;
        struct interpreter_attr_def *attr_def;
        struct named_expr *attr;
        int i;

        dump_ast_rexpr(node,out);
        interpreter = node->ndat->u.rexpr_interpreter.interpreter;
        fprintf(out, " name: %s\n%*s\\_ interpreter attrs:\n",
                interpreter->name, (depth + 1) * INDENT_N_SPACES, "");
        attr_def = STAILQ_FIRST(&interpreter->attr_list);
        for (i = 0; i < node->ndat->u.rexpr_interpreter.interpreter->n_attrs;
             ++i) {
            attr = node->ndat->u.rexpr_interpreter.attr_set.attrs[i];
            fprintf(out, "%*s\\_ \"%s\" [%d]:\n",
                    (depth + 2) * INDENT_N_SPACES, "",
                    attr_def->name, i);
            if (NULL != attr) {
                fdump_ast_recur(attr->expr, depth + 3, visible_refs, out);
            }
            attr_def = STAILQ_NEXT(attr_def, list);
        }
        break ;
    }
    case AST_NODE_TYPE_REXPR_ITEM:
        dump_ast_rexpr(node, out);
        fprintf(out, "\n%*s\\_ item_type:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->ndat->u.rexpr_item.item_type, depth + 2,
                        visible_refs, out);
        break ;
    case AST_NODE_TYPE_REXPR_FILTER:
        dump_ast_rexpr(node, out);
        fprintf(out, "\n%*s\\_ target:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->ndat->u.rexpr_filter.target, depth + 2,
                        visible_refs, out);
        fprintf(out, "\n%*s\\_ filter expr:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->ndat->u.rexpr_filter.filter_expr, depth + 2,
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
    case AST_NODE_TYPE_REXPR_FIELD:
        dump_ast_rexpr_member(node, depth, visible_refs, out);
        fprintf(out, "%*s\\_ field:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fprintf(out, "%*s\\_ item:\n",
                (depth + 2) * INDENT_N_SPACES, "");
        dump_ast_type(node->ndat->u.rexpr_field.field->dpath.item, depth + 3,
                      visible_refs, out);
        fprintf(out, "\n");
        if (NULL != node->ndat->u.rexpr_field.field->dpath.filter) {
            fprintf(out, "%*s\\_ filter:\n",
                    (depth + 2) * INDENT_N_SPACES, "");
            dump_ast_type(node->ndat->u.rexpr_field.field->dpath.filter, depth + 3,
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
    case AST_NODE_TYPE_REXPR_FILE:
    case AST_NODE_TYPE_REXPR_SELF:
    case AST_NODE_TYPE_REXPR_STAR_WILDCARD:
        dump_ast_rexpr(node, out);
        fprintf(out, "\n");
        break ;
    case AST_NODE_TYPE_EXPR_FILE:
    case AST_NODE_TYPE_EXPR_SELF:
    case AST_NODE_TYPE_EXPR_STAR_WILDCARD:
    case AST_NODE_TYPE_NONE:
        fprintf(out, "\n");
        break ;
    }
    node->flags &= ~ASTFLAG_DUMPING;
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
    case AST_NODE_TYPE_EXPR_FILE:
    case AST_NODE_TYPE_EXPR_SELF:
    case AST_NODE_TYPE_EXPR_STAR_WILDCARD:
    case AST_NODE_TYPE_REXPR_NATIVE:
    case AST_NODE_TYPE_REXPR_FILE:
    case AST_NODE_TYPE_REXPR_SELF:
    case AST_NODE_TYPE_REXPR_STAR_WILDCARD:
        /* leaf */
        fprintf(out, "%*s|- (%s) ", depth * INDENT_N_SPACES, "",
                ast_node_type_str(node->ndat->type));
        break ;
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_BLOCK_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
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
    case AST_NODE_TYPE_OP_FCALL:
    case AST_NODE_TYPE_OP_SET_FILTER:
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
    case AST_NODE_TYPE_REXPR_FIELD:
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
    case AST_NODE_TYPE_REXPR_POLYMORPHIC:
    case AST_NODE_TYPE_REXPR_BUILTIN:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
    case AST_NODE_TYPE_REXPR_OP_FCALL:
    case AST_NODE_TYPE_REXPR_ITEM:
    case AST_NODE_TYPE_REXPR_FILTER:
        /* intermediate node */
        fprintf(out, "%*s\\_ (%s) ", depth * INDENT_N_SPACES, "",
                ast_node_type_str(node->ndat->type));
        break ;
    }

    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BLOCK_DEF:
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
dump_block_recur(const struct ast_node_hdl *block,
                 int depth,
                 struct list_of_visible_refs *outer_refs,
                 FILE *out)
{
    if (NULL == block) {
        return ;
    }
    dump_block_stmt_list_recur(block, &block->ndat->u.block_def.block_stmt_list,
                               depth, outer_refs, out);
}

static void
dump_block_stmt_list_recur(const struct ast_node_hdl *block,
                           const struct block_stmt_list *block_lists,
                           int depth,
                           struct list_of_visible_refs *outer_refs,
                           FILE *out)
{
    struct list_of_visible_refs visible_refs;
    const struct field *field;
    const struct named_expr *named_expr;

    /* add current refs to the chain of visible refs */
    visible_refs.outer_refs = outer_refs;
    visible_refs.cur_block = block;
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
        fprintf(out, "%*s\\_ field type:\n",
                (depth + 2) * INDENT_N_SPACES, "");
        fdump_ast_recur(field->dpath.item, depth + 3, &visible_refs, out);
        if (NULL != field->dpath.filter) {
            fprintf(out, "%*s\\_ field filter:\n",
                    (depth + 2) * INDENT_N_SPACES, "");
            fdump_ast_recur(field->dpath.filter, depth + 3, &visible_refs, out);
        }
    }
}

void
dump_ast_node_input_text(const struct ast_node_hdl *node,
                         struct bitpunch_schema_hdl *schema,
                         FILE *out)
{
    const char *text_start;
    const char *text_end;

    text_start = schema->df_data + node->loc.start_offset;
    text_end = schema->df_data + node->loc.end_offset;

    fwrite(text_start, 1, text_end - text_start, out);
    fputs("\n", out);
}
