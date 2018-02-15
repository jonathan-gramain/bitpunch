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
#include "core/interpreter.h"
#include "core/expr.h"
#include "core/browse.h"
#include "core/print.h"
#include "core/parser.h"
#include PATH_TO_PARSER_TAB_H

#define OUTPUT_DEP_GRAPH

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
compile_all(struct ast_node_hdl *ast_root,
            enum resolve_expect_mask expect_mask);
static dep_resolver_tagset_t
dep_resolver_cb(struct dep_resolver *dr,
                struct dep_resolver_node *_node,
                dep_resolver_tagset_t tags,
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
static int
compile_dpath_flags(struct dpath_node *node, struct compile_ctx *ctx);
static int
compile_node_post_check(struct ast_node_hdl *expr,
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

static int
setup_global_track_backends(void);
static int
setup_track_backends_dpath(struct dpath_node *dpath);
static int
setup_track_backends_expr(struct ast_node_hdl *expr);

int
bitpunch_compile_schema(struct bitpunch_schema_hdl *schema)
{
    struct ast_node_hdl *ast_root;

    ast_root = schema->df_file_block.root->item;
    if (-1 == resolve_identifiers(ast_root, NULL, RESOLVE_EXPECT_TYPE,
                                  RESOLVE_TYPE_IDENTIFIERS)) {
        return -1;
    }
    if (-1 == resolve_identifiers(ast_root, NULL, RESOLVE_EXPECT_TYPE,
                                  RESOLVE_EXPRESSION_IDENTIFIERS)) {
        return -1;
    }
    if (-1 == compile_all(ast_root, RESOLVE_EXPECT_TYPE)) {
        return -1;
    }
    if (-1 == setup_global_track_backends()) {
        return -1;
    }
    if (-1 == setup_track_backends_expr(ast_root)) {
        return -1;
    }
    return 0;
}

static void
compile_ctx_init(struct compile_ctx *ctx,
                 struct ast_node_hdl *ast_root)
{
    memset(ctx, 0, sizeof (*ctx));
    ctx->dep_resolver = dep_resolver_create(dep_resolver_cb);
    ctx->ast_root = ast_root;
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


static struct named_statement *
find_statement_by_name(enum statement_type stmt_type,
                       const char *identifier,
                       const struct block_stmt_list *stmt_lists)
{
    const struct statement_list *stmt_list;
    struct statement *stmt;
    struct named_statement *nstmt;

    switch (stmt_type) {
    case STATEMENT_TYPE_FIELD:
        stmt_list = stmt_lists->field_list;
        break ;
    case STATEMENT_TYPE_NAMED_EXPR:
        stmt_list = stmt_lists->named_expr_list;
        break ;
    default:
        assert(0);
    }
    // explicit field names have priority
    TAILQ_FOREACH(stmt, stmt_list, list) {
        nstmt = (struct named_statement *)stmt;
        if (NULL != nstmt->name
            && 0 == strcmp(identifier, nstmt->name)) {
            return nstmt;
        }
    }
    // recurse in anonymous struct/union fields
    TAILQ_FOREACH(stmt, stmt_list, list) {
        nstmt = (struct named_statement *)stmt;
        if (NULL == nstmt->name) {
            struct field *field;
            const struct ast_node_hdl *field_type;

            assert(STATEMENT_TYPE_FIELD == stmt_type);
            field = (struct field *)nstmt;
            field_type = dpath_node_get_as_type(&field->dpath);
            if (NULL != field_type
                && AST_NODE_TYPE_BLOCK_DEF == field_type->ndat->type) {
                nstmt = find_statement_by_name(
                    STATEMENT_TYPE_FIELD, identifier,
                    &field_type->ndat->u.block_def.block_stmt_list);
                if (NULL != nstmt) {
                    return nstmt;
                }
            }
        }
    }
    return NULL;
}

static int
lookup_statement(enum statement_type stmt_type,
                 const char *identifier,
                 const struct list_of_visible_refs *visible_refs,
                 const struct ast_node_hdl **blockp,
                 const struct named_statement **stmtp)
{
    const struct list_of_visible_refs *refs_level;
    struct named_statement *stmt;

    /* try to find a matching statement name in current scope: browse
     * lists of block statements of requested type in upward direction
     * until a match is found */
    for (refs_level = visible_refs; NULL != refs_level;
         refs_level = refs_level->outer_refs) {
        stmt = find_statement_by_name(stmt_type, identifier,
                                      refs_level->cur_lists);
        if (NULL != stmt) {
            *blockp = refs_level->cur_block;
            *stmtp = stmt;
            return 0;
        }
    }
    return -1;
}

static int
lookup_field(const char *identifier,
             const struct list_of_visible_refs *visible_refs,
             const struct ast_node_hdl **blockp,
             const struct field **fieldp)
{
    return lookup_statement(STATEMENT_TYPE_FIELD, identifier, visible_refs,
                            blockp,
                            (const struct named_statement **)fieldp);
}

static int
lookup_named_expr(const char *identifier,
                  const struct list_of_visible_refs *visible_refs,
                  const struct ast_node_hdl **blockp,
                  const struct named_expr **named_exprp)
{
    return lookup_statement(STATEMENT_TYPE_NAMED_EXPR,
                            identifier, visible_refs, blockp,
                            (const struct named_statement **)named_exprp);
}

static struct named_statement *
find_unchained_statement_by_name(const struct statement_list *in_list,
                                 const char *identifier)
{
    struct statement *stmt;
    struct named_statement *nstmt;

    // explicit statement names have priority
    TAILQ_FOREACH(stmt, in_list, list) {
        nstmt = (struct named_statement *)stmt;
        if (NULL != nstmt->name
            && 0 == strcmp(identifier, nstmt->name)
            && NULL == nstmt->next_sibling) {
            return nstmt;
        }
    }
    // recurse in anonymous struct/union fields
    TAILQ_FOREACH(stmt, in_list, list) {
        nstmt = (struct named_statement *)stmt;
        if (NULL == nstmt->name) {
            struct field *field;

            field = (struct field *)nstmt;
            if (AST_NODE_TYPE_BLOCK_DEF == field->dpath.item->ndat->type) {
                nstmt = find_unchained_statement_by_name(
                    field->dpath.item->ndat->u.block_def.block_stmt_list.field_list,
                    identifier);
                if (NULL != nstmt) {
                    return nstmt;
                }
            }
        }
    }
    return NULL;
}

static int
chain_duplicate_statements_in(const struct statement_list *in_list,
                              const struct statement_list *toplevel_list)
{
    struct statement *stmt;
    struct named_statement *iter;
    struct named_statement *unchained;

    TAILQ_FOREACH(stmt, in_list, list) {
        iter = (struct named_statement *)stmt;
        if (NULL == iter->name) {
            const struct field *field;

            // recurse in anonymous struct/union
            field = (const struct field *)stmt;
            if (AST_NODE_TYPE_BLOCK_DEF == field->dpath.item->ndat->type) {
                if (-1 == chain_duplicate_statements_in(
                        field->dpath.item->ndat->u.block_def.block_stmt_list.field_list,
                        toplevel_list)) {
                    return -1;
                }
            }
        } else if (NULL == iter->next_sibling) {
            unchained = find_unchained_statement_by_name(toplevel_list,
                                                         iter->name);
            assert(NULL != unchained);
            if (unchained != iter) {
                if (NULL == unchained->stmt.cond) {
                    semantic_error(
                        SEMANTIC_LOGLEVEL_WARNING,
                        &unchained->stmt.loc,
                        "unconditional duplicate statement");
                }
                if (NULL == iter->stmt.cond) {
                    semantic_error(
                        SEMANTIC_LOGLEVEL_WARNING,
                        &iter->stmt.loc,
                        "unconditional duplicate statement");
                }
                unchained->next_sibling = iter;
            }
        }
    }
    return 0;
}

static int
chain_duplicate_statements(const struct block_stmt_list *stmt_lists)
{
    if (-1 == chain_duplicate_statements_in(stmt_lists->field_list,
                                            stmt_lists->field_list)) {
        return -1;
    }
    if (-1 == chain_duplicate_statements_in(stmt_lists->named_expr_list,
                                            stmt_lists->named_expr_list)) {
        return -1;
    }
    return 0;
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
resolve_identifiers_identifier_as_type(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs)
{
    struct ast_node_data *resolved_type;
    const struct ast_node_hdl *resolved_block;
    const struct named_expr *resolved_named_expr;
    
    if (0 == strcmp(node->ndat->u.identifier, "byte")) {
        /* native 'byte' type */
        resolved_type = new_safe(struct ast_node_data);
        resolved_type->type = AST_NODE_TYPE_BYTE;
        resolved_type->u.item.min_span_size = 1;
        node->ndat = resolved_type;
        return 0;
    }
    if (-1 != lookup_named_expr(node->ndat->u.identifier, visible_refs,
                                &resolved_block,
                                &resolved_named_expr)) {
        resolved_type = new_safe(struct ast_node_data);
        resolved_type->type = AST_NODE_TYPE_REXPR_NAMED_EXPR;
        resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_UNSET;
        resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
        assert(NULL != resolved_block);
        resolved_type->u.rexpr_member_common.anchor_block =
            (struct ast_node_hdl *)resolved_block;
        resolved_type->u.rexpr_named_expr.named_expr =
            resolved_named_expr;
        node->ndat = resolved_type;
        return 0;
    }
    return -1;
}

static int
resolve_identifiers_identifier_as_filter(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs)
{
    const struct interpreter *interpreter;

    interpreter = interpreter_lookup(node->ndat->u.identifier);
    if (NULL != interpreter) {
        struct statement_list empty_param_list;

        TAILQ_INIT(&empty_param_list);
        if (-1 == interpreter_rcall_build(node, interpreter,
                                          &empty_param_list)) {
            return -1;
        }
        return 0;
    }
    return -1;
}

static int
resolve_identifiers_identifier_as_expression(
    struct ast_node_hdl *node,
    const struct list_of_visible_refs *visible_refs)
{
    struct ast_node_data  *resolved_type;
    const struct ast_node_hdl *resolved_block;
    const struct named_expr *resolved_named_expr;
    const struct field *resolved_field;
    const struct expr_builtin_fn *builtin;

    if (-1 != lookup_named_expr(node->ndat->u.identifier, visible_refs,
                                &resolved_block,
                                &resolved_named_expr)) {
        resolved_type = new_safe(struct ast_node_data);
        resolved_type->type = AST_NODE_TYPE_REXPR_NAMED_EXPR;
        resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_UNSET;
        resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
        assert(NULL != resolved_block);
        resolved_type->u.rexpr_member_common.anchor_block =
            (struct ast_node_hdl *)resolved_block;
        resolved_type->u.rexpr_named_expr.named_expr =
            resolved_named_expr;
        node->ndat = resolved_type;
        return 0;
    }
    if (-1 != lookup_field(node->ndat->u.identifier, visible_refs,
                           &resolved_block, &resolved_field)) {
        resolved_type = new_safe(struct ast_node_data);
        resolved_type->type = AST_NODE_TYPE_REXPR_FIELD;
        resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_UNSET;
        resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
        assert(NULL != resolved_block);
        resolved_type->u.rexpr_member_common.anchor_block =
            (struct ast_node_hdl *)resolved_block;
        resolved_type->u.rexpr_field.field = resolved_field;
        node->ndat = resolved_type;
        return 0;
    }
    builtin = expr_lookup_builtin_fn(node->ndat->u.identifier, NULL);
    if (NULL != builtin) {
        resolved_type = new_safe(struct ast_node_data);
        resolved_type->type = AST_NODE_TYPE_REXPR_BUILTIN;
        resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
        resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_UNSET;
        resolved_type->u.rexpr_builtin.builtin = builtin;
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
        if (0 != (expect_mask & RESOLVE_EXPECT_TYPE)
            && 0 == resolve_identifiers_identifier_as_type(
                node, visible_refs)) {
            return 0;
        }
        if (0 != (expect_mask & RESOLVE_EXPECT_INTERPRETER)
            && 0 == resolve_identifiers_identifier_as_filter(
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
    enum resolve_identifiers_tag resolve_tags)
{
    return resolve_identifiers_dpath_node(&field->dpath,
                                          visible_refs, resolve_tags);
}

static int
resolve_identifiers_in_block_body(
    struct ast_node_hdl *block,
    const struct list_of_visible_refs *outer_refs,
    enum resolve_identifiers_tag resolve_tags)
{
    struct block_stmt_list *stmt_lists;
    struct list_of_visible_refs visible_refs;
    struct statement *stmt;
    struct field *field;
    struct span_stmt *span_stmt;
    struct key_stmt *key_stmt;
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
            stmt_lists->span_list, &visible_refs, resolve_tags)) {
        return -1;
    }
    if (-1 == resolve_identifiers_in_statement_list(
            stmt_lists->last_stmt_list, &visible_refs, resolve_tags)) {
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
        if (-1 == resolve_identifiers_field(field, &visible_refs,
                                            resolve_tags)) {
            return -1;
        }
    }
    if (-1 == chain_duplicate_statements(stmt_lists)) {
        return -1;
    }
    TAILQ_FOREACH(stmt, stmt_lists->span_list, list) {
        span_stmt = (struct span_stmt *)stmt;
        if (-1 == resolve_identifiers_in_expression(
                span_stmt->span_expr, &visible_refs, resolve_tags)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, stmt_lists->key_list, list) {
        key_stmt = (struct key_stmt *)stmt;
        if (-1 == resolve_identifiers_in_expression(
                key_stmt->key_expr, &visible_refs, resolve_tags)) {
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
        const struct interpreter *interpreter;

        interpreter = interpreter_lookup(filter_type);
        if (NULL == interpreter) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &block->loc,
                "no interpreter named '%s' exists",
                filter_type);
            return -1;
        }
        if (-1 == interpreter_rcall_build(
                block, interpreter,
                block->ndat->u.block_def.block_stmt_list.field_list)) {
            return -1;
        }
    } else {
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
    struct ast_node_hdl *file_block;
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
    // const-cast
    file_block = (struct ast_node_hdl *)toplevel_refs->cur_block;

    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_FILE;
    compiled_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    compiled_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    compiled_type->u.rexpr.target_item = file_block;
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
    struct ast_node_hdl *self_block;
    struct ast_node_data *compiled_type;

    if (NULL == visible_refs) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                       "need a binary file loaded to use 'self' in "
                       "expression");
        return -1;
    }
    // const-cast
    self_block = (struct ast_node_hdl *)visible_refs->cur_block;

    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_SELF;
    compiled_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    compiled_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    compiled_type->u.rexpr.target_item = self_block;
    expr->ndat = compiled_type;
    return 0;
}


static enum expr_value_type
expr_value_type_from_node(const struct ast_node_hdl *node)
{
    if (ast_node_is_rexpr(node)) {
        return node->ndat->u.rexpr.value_type;
    }
    return EXPR_VALUE_TYPE_UNSET;
}

static enum expr_value_type
expr_value_type_from_dpath_node(const struct dpath_node *dpath)
{
    if (NULL != dpath->filter) {
        return expr_value_type_from_node(dpath->filter);
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
        return resolve_identifiers_block(node, visible_refs, expect_mask, resolve_tags);
    case AST_NODE_TYPE_ARRAY:
        return resolve_identifiers_array(node, visible_refs, expect_mask, resolve_tags);
    case AST_NODE_TYPE_BYTE_ARRAY:
        return resolve_identifiers_byte_array(node, visible_refs, expect_mask, resolve_tags);
    case AST_NODE_TYPE_CONDITIONAL:
        return resolve_identifiers_conditional(node, visible_refs, expect_mask, resolve_tags);
        /* for all operators: resolve potential type names in operand
         * sub-expressions (e.g. sizeof) */
    case AST_NODE_TYPE_OP_UPLUS:
    case AST_NODE_TYPE_OP_UMINUS:
    case AST_NODE_TYPE_OP_LNOT:
    case AST_NODE_TYPE_OP_BWNOT:
    case AST_NODE_TYPE_OP_MEMBER:
        // for member operator, 2nd operand is resolved in compile phase
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
    case AST_NODE_TYPE_OP_SIZEOF:
        return resolve_identifiers_operator_sizeof(node, visible_refs,
                                                   expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_SUBSCRIPT:
        return resolve_identifiers_operator_subscript(node, visible_refs, expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
        return resolve_identifiers_operator_subscript_slice(node, visible_refs,
                                                expect_mask, resolve_tags);
    case AST_NODE_TYPE_OP_FCALL:
        return resolve_identifiers_op_fcall(node, visible_refs, expect_mask, resolve_tags);
    case AST_NODE_TYPE_EXPR_FILE:
        return resolve_identifiers_expr_file(node, visible_refs, expect_mask, resolve_tags);
    case AST_NODE_TYPE_EXPR_SELF:
        return resolve_identifiers_expr_self(node, visible_refs, expect_mask, resolve_tags);
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
resolve_user_expr_scoped_recur(struct ast_node_hdl *expr,
                               struct box *cur_scope,
                               struct list_of_visible_refs *inner_refs,
                               struct list_of_visible_refs *inmost_refs)
{
    struct list_of_visible_refs visible_refs;

    while (NULL != cur_scope
           && AST_NODE_TYPE_BLOCK_DEF != cur_scope->dpath.item->ndat->type) {
        cur_scope = (NULL != cur_scope->parent_box ?
                     cur_scope->parent_box :
                     cur_scope->unfiltered_box);
    }
    if (NULL == cur_scope) {
        if (-1 == resolve_identifiers_in_expression(expr, inmost_refs,
                                                    RESOLVE_ALL_IDENTIFIERS)) {
            return -1;
        }
        if (-1 == compile_all(expr, RESOLVE_EXPECT_EXPRESSION)) {
            return -1;
        }
        return 0;
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
    if (-1 == resolve_identifiers_in_expression(expr, NULL,
                                                RESOLVE_ALL_IDENTIFIERS)) {
        return -1;
    }
    if (-1 == compile_all(expr, RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    return 0;
}

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
    struct dpath_node *dpath;
    struct subscript_index *subscript;

    if (req->compile_cb == compile_node_cb) {
        return &container_of(node, struct ast_node_hdl, dr_node)->loc;
    }
    if (req->compile_cb == compile_dpath_cb) {
        dpath = container_of(node, struct dpath_node, dr_node);
        return &(NULL != dpath->filter ? dpath->filter : dpath->item)->loc;
    }
    if (req->compile_cb == compile_field_cb) {
        return &container_of(node, struct field, dr_node)->nstmt.stmt.loc;
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

static int
compile_all(struct ast_node_hdl *ast_root,
            enum resolve_expect_mask expect_mask)
{
    struct compile_ctx ctx;
    dep_resolver_status_t ret;

    compile_ctx_init(&ctx, ast_root);
    compile_node(ast_root, &ctx,
                 COMPILE_TAG_NODE_TYPE |
                 COMPILE_TAG_NODE_SPAN_SIZE |
                 COMPILE_TAG_NODE_FLAGS, 0u, expect_mask);
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
    switch (ret) {
    case DEP_RESOLVER_CIRCULAR_DEPENDENCY: {
        struct dep_resolver_node_entry **circ_entries;
        struct dep_resolver_node_entry *entry;
        struct compile_req *req;
        const struct parser_location *loc;
        int i;

        circ_entries = dep_resolver_get_circular_dependency(ctx.dep_resolver);
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "circular dependency across the following nodes:");
        for (i = 0; NULL != (entry = circ_entries[i]); ++i) {
            req = (struct compile_req *)entry->arg;
            if (req->compile_cb == compile_dpath_cb) {
                continue ;
            }
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

        entry = dep_resolver_get_error_entry(ctx.dep_resolver);
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
compile_key_stmt(struct key_stmt *key_stmt, struct compile_ctx *ctx)
{
    return compile_expr(key_stmt->key_expr, ctx, FALSE);
}

static int
compile_named_expr(struct named_expr *named_expr, struct compile_ctx *ctx,
                   enum resolve_expect_mask expect_mask)
{
    struct ast_node_hdl *expr;

    expr = named_expr->expr;
    return compile_node(expr, ctx,
                        0u, (COMPILE_TAG_NODE_TYPE |
                             COMPILE_TAG_NODE_SPAN_SIZE |
                             COMPILE_TAG_NODE_FLAGS), expect_mask);
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
    target_item = ast_node_get_target_item(dpath->item);
    if (NULL == target_item) {
        assert(NULL != dpath->filter);
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &dpath->filter->loc,
                       "expect an item type as field type, not '%s'",
                       ast_node_type_str(dpath->filter->ndat->type));
        return 0u;
    }

    if (0 != (tags & COMPILE_TAG_NODE_TYPE)) {
        if (NULL == field->nstmt.name) {
            const struct ast_node_hdl *as_type;

            as_type = ast_node_get_named_expr_target(
                (struct ast_node_hdl *)dpath_node_get_as_type(dpath));
            if (AST_NODE_TYPE_BLOCK_DEF != as_type->ndat->type) {
                field->nstmt.stmt.stmt_flags |= FIELD_FLAG_HIDDEN;
            }
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
    struct span_stmt *span_stmt;
    struct key_stmt *key_stmt;
    struct match *match;
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
    compile_stmt_list_generic(stmt_lists->span_list, ctx);
    compile_stmt_list_generic(stmt_lists->last_stmt_list, ctx);
    
    STATEMENT_FOREACH(field, field, stmt_lists->field_list, list) {
        compile_field(field, ctx, 0u, COMPILE_TAG_NODE_TYPE);
    }
    STATEMENT_FOREACH(span_stmt, span_stmt, stmt_lists->span_list, list) {
        compile_expr(span_stmt->span_expr, ctx, FALSE);
    }
    STATEMENT_FOREACH(key_stmt, key_stmt, stmt_lists->key_list, list) {
        compile_key_stmt(key_stmt, ctx);
    }
    STATEMENT_FOREACH(match, match, stmt_lists->match_list, list) {
        compile_expr(match->expr, ctx, FALSE);
    }
    return compile_continue(ctx) ? 0 : -1;
}

static int
compile_block(struct ast_node_hdl *block,
              struct compile_ctx *ctx,
              enum resolve_expect_mask expect_mask)
{
    return compile_stmt_lists(&block->ndat->u.block_def.block_stmt_list,
                              ctx);
}

static int
compile_array(struct ast_node_hdl *node,
              struct compile_ctx *ctx,
              enum resolve_expect_mask expect_mask)
{
    if (NULL != node->ndat->u.array.item_count
        && -1 == compile_expr(node->ndat->u.array.item_count, ctx, FALSE)) {
        return -1;
    }
    compile_dpath(&node->ndat->u.array.item_type, ctx,
                  0u, COMPILE_TAG_NODE_TYPE);
    if (AST_NODE_TYPE_BYTE == node->ndat->u.array.item_type.item->ndat->type
        && NULL == node->ndat->u.array.item_type.filter) {
        struct ast_node_data *byte_array;

        byte_array = new_safe(struct ast_node_data);
        byte_array->type = AST_NODE_TYPE_BYTE_ARRAY;
        byte_array->u.item.min_span_size = SPAN_SIZE_UNDEF;
        byte_array->u.byte_array.size = node->ndat->u.array.item_count;
        node->ndat = byte_array;
        return 0;
    }
    return 0;
}

static int
compile_expr_integer(struct ast_node_hdl *node,
                     struct compile_ctx *ctx,
                     enum resolve_expect_mask expect_mask)
{
    int64_t integer;
    struct ast_node_data *compiled_type;

    integer = node->ndat->u.integer;
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_NATIVE;
    compiled_type->u.rexpr.value_type = EXPR_VALUE_TYPE_INTEGER;
    compiled_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    compiled_type->u.rexpr_native.value.integer = integer;
    node->ndat = compiled_type;
    return 0;
}

static int
compile_expr_boolean(struct ast_node_hdl *node,
                     struct compile_ctx *ctx,
                     enum resolve_expect_mask expect_mask)
{
    int boolean;
    struct ast_node_data *compiled_type;

    boolean = node->ndat->u.boolean;
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_NATIVE;
    compiled_type->u.rexpr.value_type = EXPR_VALUE_TYPE_BOOLEAN;
    compiled_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    compiled_type->u.rexpr_native.value.boolean = boolean;
    node->ndat = compiled_type;
    return 0;
}

static int
compile_expr_string_literal(struct ast_node_hdl *node,
                            struct compile_ctx *ctx,
                            enum resolve_expect_mask expect_mask)
{
    struct expr_value_string string;
    struct ast_node_data *compiled_type;

    string = node->ndat->u.string;
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_NATIVE;
    compiled_type->u.rexpr.value_type = EXPR_VALUE_TYPE_STRING;
    compiled_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    compiled_type->u.rexpr_native.value.string = string;
    node->ndat = compiled_type;
    return 0;
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
    resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
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
        opd_types[opd_i] = operand->ndat->u.rexpr.value_type;
        if (EXPR_VALUE_TYPE_UNSET == opd_types[opd_i]) {
            const struct ast_node_hdl *target_item;

            target_item = operand->ndat->u.rexpr.target_item;
            if (NULL == target_item) {
                target_item = operand;
            }
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &operand->loc,
                           "cannot use '%s' in expression",
                           ast_node_type_str(target_item->ndat->type));
            semantic_error(SEMANTIC_LOGLEVEL_INFO, &target_item->loc,
                           "declared here");
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
    expr->ndat->u.rexpr.value_type = evaluator->res_type;

    if (only_native_operands) {
        struct ast_node_hdl *operand;
        union expr_value operand_values[2];
        const struct expr_evaluator *evaluator;
        union expr_value eval_value;

        for (opd_i = 0; opd_i < n_operands; ++opd_i) {
            operand = ast_node_get_named_expr_target(
                expr->ndat->u.rexpr_op.op.operands[opd_i]);
            assert(AST_NODE_TYPE_REXPR_NATIVE == operand->ndat->type);
            operand_values[opd_i] = operand->ndat->u.rexpr_native.value;
        }
        evaluator = expr->ndat->u.rexpr_op.evaluator;
        assert(NULL != evaluator);
        eval_value = evaluator->eval_fn(operand_values);

        // FIXME create a new node
        expr->ndat->type = AST_NODE_TYPE_REXPR_NATIVE;
        /* expr->loc already set */
        /* expr->u.rexpr.value_type already set */
        expr->ndat->u.rexpr_native.value = eval_value;
    }
    return 0;
}

int
interpreter_build_instance(struct ast_node_hdl *node,
                           struct ast_node_hdl *target,
                           struct compile_ctx *ctx)
{
    const struct interpreter *interpreter;
    struct ast_node_data *interp_inst;
    struct ast_node_hdl *filter_target;
    struct ast_node_hdl *target_item;

    assert(NULL != target);

    target_item = ast_node_get_target_item(target);
    if (NULL == target_item) {
        // pass the rexpr node if no target item configured
        target_item = target;
    }
    if (-1 == compile_node(target_item, ctx, COMPILE_TAG_NODE_TYPE, 0u,
                           RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_INTERPRETER |
                           RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    interpreter = node->ndat->u.rexpr_interpreter.interpreter;
    if (0 != (node->ndat->flags & ASTFLAG_DATA_TEMPLATE)) {
        interp_inst = interpreter_rcall_instanciate(node);
    } else {
        interp_inst = node->ndat;
    }

    filter_target = target;
    while (ast_node_is_filter(filter_target)) {
        filter_target = filter_target->ndat->u.rexpr_filter.target;
    }
    if (ast_node_is_item(filter_target)) {
        interp_inst->u.rexpr.dpath_type = filter_target->ndat->u.rexpr.dpath_type;
    } else {
        interp_inst->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    }
    interp_inst->u.rexpr_filter.target = target;

    node->ndat = interp_inst;
    if (-1 == interpreter->rcall_build_func(
            node, target_item,
            interpreter_rcall_get_params(node), ctx)) {
        return -1;
    }
    interp_inst->u.rexpr.target_item = target_item;
    return 0;
}

static int
compile_expr_operator_set_filter(
    struct ast_node_hdl *node,
    struct compile_ctx *ctx)
{
    struct ast_node_hdl *target;
    struct ast_node_hdl *filter;
    struct ast_node_hdl *filter_type;

    target = node->ndat->u.op.operands[0];
    filter = node->ndat->u.op.operands[1];

    compile_node(target, ctx, COMPILE_TAG_NODE_TYPE, 0u,
                 RESOLVE_EXPECT_TYPE |
                 RESOLVE_EXPECT_INTERPRETER |
                 RESOLVE_EXPECT_DPATH_EXPRESSION);
    compile_node(filter, ctx, COMPILE_TAG_NODE_TYPE, 0u,
                 RESOLVE_EXPECT_TYPE |
                 RESOLVE_EXPECT_INTERPRETER);
    if (!compile_continue(ctx)) {
        return -1;
    }
    filter_type = ast_node_get_named_expr_target(filter);

    if (ast_node_is_item(filter_type)) {
        struct ast_node_data *as_type;

        as_type = new_safe(struct ast_node_data);
        as_type->type = AST_NODE_TYPE_REXPR_AS_TYPE;
        as_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
        as_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
        as_type->u.rexpr_filter.filter_dpath.item = filter_type;
        as_type->u.rexpr_filter.target = target;
        node->ndat = as_type;
    } else if (AST_NODE_TYPE_REXPR_INTERPRETER == filter_type->ndat->type) {
        if (-1 == interpreter_build_instance(filter_type, target, ctx)) {
            return -1;
        }
        node->ndat = filter_type->ndat;
    } else if (AST_NODE_TYPE_REXPR_AS_TYPE == filter_type->ndat->type) {
        node->ndat = filter_type->ndat;
    } else {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &filter_type->loc,
            "invalid second operand of filter operator: "
            "expect type or interpreter, not '%s'",
            ast_node_type_str(filter_type->ndat->type));
        return -1;
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
    struct ast_node_hdl *key;
    struct ast_node_hdl *twin_idx;
    const struct ast_node_hdl *anchor_expr;
    const struct ast_node_hdl *anchor_item;
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
    if (EXPR_VALUE_TYPE_INTEGER != key->ndat->u.rexpr.value_type &&
        EXPR_VALUE_TYPE_STRING != key->ndat->u.rexpr.value_type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &key->loc,
            "invalid expression type in array subscript: "
            "expect 'integer' or 'string', not '%s'",
            expr_value_type_str(key->ndat->u.rexpr.value_type));
        return 0u;
    }
    if (NULL != subscript->twin) {
        twin_idx = subscript->twin;
        assert(ast_node_is_rexpr(twin_idx));
        if (EXPR_VALUE_TYPE_INTEGER != twin_idx->ndat->u.rexpr.value_type
            && AST_NODE_TYPE_REXPR_STAR_WILDCARD != twin_idx->ndat->type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &twin_idx->loc,
                "invalid expression type in array subscript: "
                "twin index must be of type 'integer', not '%s'",
                expr_value_type_str(twin_idx->ndat->u.rexpr.value_type));
            return 0u;
        }
        if (AST_NODE_TYPE_REXPR_NATIVE == twin_idx->ndat->type
            && EXPR_VALUE_TYPE_INTEGER == twin_idx->ndat->u.rexpr.value_type
            && twin_idx->ndat->u.rexpr_native.value.integer < 0) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &twin_idx->loc,
                "array integer twin index cannot be negative");
            return 0u;
        }
    }
    anchor_expr = expr->ndat->u.rexpr_op_subscript_common.anchor_expr;
    anchor_item = anchor_expr->ndat->u.rexpr.target_item;
    if (NULL != anchor_item) {
        if (ast_node_is_indexed(anchor_item)) {
            /* integer subscript accesses items by raw index */
            if (EXPR_VALUE_TYPE_INTEGER != key->ndat->u.rexpr.value_type) {
                key_expr = ast_node_get_key_expr(anchor_item);
                if (-1 == compile_expr(key_expr, ctx, TRUE)) {
                    return 0u;
                }
                assert(ast_node_is_rexpr(key));
                assert(ast_node_is_rexpr(key_expr));
                if (key->ndat->u.rexpr.value_type
                    != key_expr->ndat->u.rexpr.value_type) {
                    semantic_error(
                        SEMANTIC_LOGLEVEL_ERROR, &key->loc,
                        "invalid expression type in array subscript: "
                        "type mismatch between subscript type '%s' and "
                        "index type '%s'",
                        ast_node_type_str(key->ndat->u.rexpr.value_type),
                        ast_node_type_str(key_expr->ndat->u.rexpr.value_type));
                    return 0u;
                }
            }
        } else if (EXPR_VALUE_TYPE_STRING == key->ndat->u.rexpr.value_type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &key->loc,
                "invalid expression type in array subscript: "
                "'string' type requires array element type to be indexed");
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
    struct ast_node_hdl *anchor_target;
    struct ast_node_hdl *key;
    struct ast_node_hdl *twin;
    struct ast_node_data *compiled_type;
    
    anchor_expr = node->ndat->u.op_subscript_common.anchor_expr;
    key = node->ndat->u.op_subscript.index.key;
    twin = node->ndat->u.op_subscript.index.twin;
    if (-1 == compile_node(anchor_expr, ctx, COMPILE_TAG_NODE_TYPE, 0u,
                           expect_mask)) {
        return -1;
    }
    if (0 != (expect_mask & RESOLVE_EXPECT_TYPE)) {
        anchor_target = ast_node_get_target_type(anchor_expr);
        if (NULL != anchor_target) {
            assert(ast_node_is_item(anchor_target));
            if (NULL != twin) {
                semantic_error(SEMANTIC_LOGLEVEL_ERROR, &twin->loc,
                               "cannot have a twin index for array type "
                               "declaration");
                return -1;
            }
            if (NULL != key && -1 == compile_expr(key, ctx, TRUE)) {
                return -1;
            }
            compiled_type = new_safe(struct ast_node_data);
            compiled_type->type = AST_NODE_TYPE_ARRAY;
            compiled_type->u.item.min_span_size = SPAN_SIZE_UNDEF;
            dpath_node_reset(&compiled_type->u.array.item_type);
            compiled_type->u.array.item_type.item = anchor_expr;
            compiled_type->u.array.item_count = key;
            node->ndat = compiled_type;
            return 0;
        } else if (0 == (expect_mask & RESOLVE_EXPECT_EXPRESSION)) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &anchor_expr->loc,
                           "expecting type, got '%s'",
                           ast_node_type_str(
                               ast_node_get_named_expr_target(anchor_expr)
                               ->ndat->type));
            return -1;
        }
    }
    if (0 != (expect_mask & RESOLVE_EXPECT_EXPRESSION)) {
        struct subscript_index *index;
        const struct ast_node_hdl *anchor_item;
        struct ast_node_hdl *target_item = NULL;
        enum expr_value_type value_type = EXPR_VALUE_TYPE_UNSET;

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
        anchor_item = anchor_expr->ndat->u.rexpr.target_item;
        if (NULL != anchor_item) {
            switch (anchor_item->ndat->type) {
            case AST_NODE_TYPE_ARRAY:
            case AST_NODE_TYPE_ARRAY_SLICE:
                if (-1 == compile_dpath(&anchor_item->ndat->u.array.item_type,
                                        ctx, COMPILE_TAG_NODE_TYPE, 0u)) {
                    return -1;
                }
                target_item = (struct ast_node_hdl *)
                    dpath_node_get_as_type(&anchor_item->ndat->u.array.item_type);
                value_type = expr_value_type_from_dpath_node(
                    &anchor_item->ndat->u.array.item_type);
                break ;
            case AST_NODE_TYPE_BYTE_ARRAY:
            case AST_NODE_TYPE_BYTE_SLICE:
                target_item = AST_NODE_BYTE;
                value_type = EXPR_VALUE_TYPE_BYTES;
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
        compiled_type->u.rexpr.value_type = value_type;
        compiled_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_ITEM;
        compiled_type->u.rexpr.target_item = target_item;
        assert(ast_node_is_rexpr(anchor_expr));
        compiled_type->u.rexpr_op_subscript_common.anchor_expr = anchor_expr;
        compiled_type->u.rexpr_op_subscript.index = *index;
        index = &compiled_type->u.op_subscript.index;
        node->ndat = compiled_type;
    }
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
    struct ast_node_hdl *target_item = NULL;
    enum expr_value_type value_type = EXPR_VALUE_TYPE_UNSET;

    anchor_expr = node->ndat->u.op_subscript_common.anchor_expr;
    anchor_item = anchor_expr->ndat->u.rexpr.target_item;
    slice_start = &node->ndat->u.op_subscript_slice.start;
    slice_end = &node->ndat->u.op_subscript_slice.end;

    assert(NULL != anchor_expr);

    if (-1 == compile_expr(anchor_expr, ctx, TRUE)) {
        return -1;
    }
    anchor_item = anchor_expr->ndat->u.rexpr.target_item;
    if (NULL != anchor_item) {
        if (! ast_node_is_subscriptable_container(anchor_item)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                "invalid use of subscript operator on non-subscriptable "
                "path of type '%s'",
                ast_node_type_str(anchor_item->ndat->type));
            return -1;
        }
    } else {
        if (EXPR_DPATH_TYPE_CONTAINER != anchor_expr->ndat->u.rexpr.dpath_type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                "invalid use of subscript operator on non-dpath or "
                "non-container dpath expression");
            return -1;
        }
    }
    if (NULL != anchor_item) {
        target_item = ast_node_get_named_expr_target(anchor_item);
        value_type = expr_value_type_from_node(anchor_item);
    }
    if (-1 == compile_subscript_index(node, slice_start, ctx) ||
        -1 == compile_subscript_index(node, slice_end, ctx)) {
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE;
    compiled_type->u.rexpr.value_type = value_type;
    compiled_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    compiled_type->u.rexpr.target_item = target_item;
    assert(ast_node_is_rexpr(anchor_expr));
    compiled_type->u.rexpr_op_subscript_common.anchor_expr = anchor_expr;
    compiled_type->u.rexpr_op_subscript_slice.start = *slice_start;
    compiled_type->u.rexpr_op_subscript_slice.end = *slice_end;
    // a slice still references the anchor array
    compiled_type->u.rexpr.target_item =
        ast_node_get_named_expr_target(anchor_item);
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
    compiled_type->u.rexpr.value_type = builtin->res_value_type;
    compiled_type->u.rexpr.dpath_type = builtin->res_dpath_type;
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
compile_expr_operator_sizeof(struct ast_node_hdl *expr,
                             struct compile_ctx *ctx,
                             enum resolve_expect_mask expect_mask)
{
    struct ast_node_data *compiled_type;
    struct ast_node_hdl *operand;
    struct ast_node_hdl *target;
    struct ast_node_hdl *target_item;
    struct ast_node_hdl *anchor;

    operand = expr->ndat->u.op.operands[0];
    if (-1 == compile_node(operand, ctx,
                           COMPILE_TAG_NODE_TYPE, 0u,
                           RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_SIZEOF;
    compiled_type->u.rexpr.value_type = EXPR_VALUE_TYPE_INTEGER;
    compiled_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    compiled_type->u.rexpr_op.op = expr->ndat->u.op;
    expr->ndat = compiled_type;

    target = ast_node_get_named_expr_target(operand);
    target_item = ast_node_get_target_item(target);
    anchor = target;
    while (NULL != anchor
           && AST_NODE_TYPE_REXPR_FIELD == anchor->ndat->type) {
        anchor = ast_node_get_named_expr_target(
            anchor->ndat->u.rexpr_member_common.anchor_expr);
    }
    if ((NULL != anchor && ast_node_is_item(anchor))
        || (NULL == anchor && ast_node_is_item(target))) {
        if (0 != (target_item->ndat->u.item.flags
                  & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "invalid use of sizeof operator on dynamic-sized type name\n"
                "(use a dpath expression for computing size dynamically)");
            return -1;
        }
        expr->ndat->u.rexpr_op.op.operands[0] = target_item;
    } else if (!ast_node_is_rexpr(target)
               || EXPR_DPATH_TYPE_NONE == target->ndat->u.rexpr.dpath_type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "invalid use of sizeof operator on operand of type '%s'",
            ast_node_type_str(target->ndat->type));
        return -1;
    }
    return 0;
}

/**
 * @brief first compile pass of addrof (&) operator
 *
 * The first pass focuses on resolving data types and dpaths of addrof
 * argument expression. Second pass will compute static address or
 * set it to dynamic, as this requires all dpaths to be resolved
 * first.
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
    compiled_type->u.rexpr.value_type = EXPR_VALUE_TYPE_INTEGER;
    compiled_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    compiled_type->u.rexpr_op.op = expr->ndat->u.op;
    expr->ndat = compiled_type;

    op = expr->ndat->u.rexpr_op.op;
    if (EXPR_DPATH_TYPE_NONE == op.operands[0]->ndat->u.rexpr.dpath_type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "invalid use of addrof (&) operator on non-dpath node of type "
            "'%s'",
            ast_node_type_str(op.operands[0]->ndat->type));
        return -1;
    }
    return 0;
}

/**
 * @brief first compile pass of ancestor (unary ^) operator
 *
 * The first pass focuses on resolving data types and dpaths of addrof
 * argument expression. Second pass will compute static address or
 * set it to dynamic, as this requires all dpaths to be resolved
 * first.
 */
static int
compile_expr_operator_ancestor(struct ast_node_hdl *expr,
                               struct compile_ctx *ctx,
                               enum resolve_expect_mask expect_mask)
{
    struct ast_node_hdl *operand;
    struct ast_node_hdl *target;
    struct ast_node_data *compiled_type;

    operand = expr->ndat->u.op.operands[0];
    if (-1 == compile_node(operand, ctx,
                           COMPILE_TAG_NODE_TYPE, 0u,
                           RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    assert(ast_node_is_rexpr(operand));
    target = operand->ndat->u.rexpr.target_item;
    compiled_type = new_safe(struct ast_node_data);
    compiled_type->type = AST_NODE_TYPE_REXPR_OP_ANCESTOR;
    compiled_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    compiled_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    compiled_type->u.rexpr.target_item = target;
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
    compiled_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    compiled_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    expr->ndat = compiled_type;
    return 0;
}

static int
compile_rexpr_as_type(struct ast_node_hdl *expr,
                      struct compile_ctx *ctx,
                      enum resolve_expect_mask expect_mask)
{
    struct ast_node_hdl *target_item;
    struct dpath_node target_dpath;
    struct dpath_node *as_dpath;
    struct ast_node_hdl *as_item;

    if (NULL != expr->ndat->u.rexpr_filter.target
        && -1 == compile_node(
            expr->ndat->u.rexpr_filter.target, ctx,
            0u, (COMPILE_TAG_NODE_TYPE |
                 COMPILE_TAG_NODE_SPAN_SIZE |
                 COMPILE_TAG_NODE_FLAGS),
            expect_mask & (RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_DPATH_EXPRESSION))) {
        return -1;
    }
    if (-1 == compile_node(expr->ndat->u.rexpr_filter.filter_dpath.item,
                           ctx, COMPILE_TAG_NODE_TYPE, 0u,
                           RESOLVE_EXPECT_TYPE)) {
        return -1;
    }
    target_item = (struct ast_node_hdl *)
        dpath_node_get_as_type(&expr->ndat->u.rexpr_filter.filter_dpath);
    dpath_node_reset(&target_dpath);
    target_dpath.filter = expr;
    target_dpath.item = target_item;
    as_dpath = &expr->ndat->u.rexpr_filter.filter_dpath;
    as_item = ast_node_get_named_expr_target(as_dpath->item);
    assert(ast_node_is_item(as_item));
    if (-1 == compile_node(as_item, ctx,
                           0u, COMPILE_TAG_NODE_TYPE, RESOLVE_EXPECT_TYPE)) {
        return -1;
    }
    if (NULL != target_dpath.item) {
        assert(ast_node_is_item(target_dpath.item));
        if (-1 == compile_node(target_dpath.item, ctx,
                               0u, COMPILE_TAG_NODE_TYPE,
                               RESOLVE_EXPECT_TYPE)) {
            return -1;
        }
    }
    expr->ndat->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    expr->ndat->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    expr->ndat->u.rexpr.target_item = target_item;
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
    if (EXPR_VALUE_TYPE_BOOLEAN != cond_expr->ndat->u.rexpr.value_type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &cond->loc,
            "expect a boolean expression in condition, not '%s'",
            expr_value_type_str(cond_expr->ndat->u.rexpr.value_type));
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
compile_dpath_set_filter_defining_size(struct dpath_node *dpath,
                                       struct compile_ctx *ctx)
{
    struct ast_node_hdl *filter;
    struct ast_node_hdl *filter_type;

    filter = dpath->filter;
    while (NULL != filter) {
        filter_type = ast_node_get_named_expr_target(filter);
        if (!ast_node_is_filter(filter_type)) {
            break ;
        }
        if (AST_NODE_TYPE_REXPR_INTERPRETER == filter_type->ndat->type) {
            if (NULL != filter_type->ndat->u.rexpr_interpreter.get_size_func) {
                // overwrite potential existing value so that value
                // set is the interpreter closest to the item
                dpath->filter_defining_size = filter_type;
                dpath->u.item.flags &= ~ITEMFLAG_HAS_UNDETERMINED_SIZE;
            }
        }
        filter = filter_type->ndat->u.rexpr_filter.target;
    }
    return 0;
}

static int
compile_rexpr_interpreter(struct ast_node_hdl *expr,
                          struct compile_ctx *ctx)
{
    const struct interpreter *interpreter;
    struct ast_node_hdl *param_valuep;
    struct interpreter_param_def *param_def;
    int sem_error = FALSE;

    interpreter = expr->ndat->u.rexpr_interpreter.interpreter;
    STAILQ_FOREACH(param_def, &interpreter->param_list, list) {
        param_valuep = INTERPRETER_RCALL_PARAM(expr, param_def->ref_idx);
        compile_expr(param_valuep, ctx, TRUE);
    }
    if (!compile_continue(ctx)) {
        return -1;
    }
    STAILQ_FOREACH(param_def, &interpreter->param_list, list) {
        param_valuep = INTERPRETER_RCALL_PARAM(expr, param_def->ref_idx);
        if (AST_NODE_TYPE_NONE != param_valuep->ndat->type) {
            assert(ast_node_is_rexpr(param_valuep));
            if (param_valuep->ndat->u.rexpr.value_type != EXPR_VALUE_TYPE_UNSET
                && param_valuep->ndat->u.rexpr.value_type != param_def->type) {
                semantic_error(SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                               "wrong data type for parameter \"%s\": expected \"%s\", not \"%s\"",
                               param_def->name,
                               expr_value_type_str(param_def->type),
                               expr_value_type_str(
                                   param_valuep->ndat->u.rexpr.value_type));
                sem_error = TRUE;
                continue ;
            }
        }
    }
    if (sem_error) {
        return -1;
    }
    // template flag may be removed when compiling OP_SET_FILTER
    expr->ndat->flags |= ASTFLAG_DATA_TEMPLATE;
    return 0;
}

static int
compile_rexpr_named_expr(struct ast_node_hdl *expr,
                         struct compile_ctx *ctx,
                         enum resolve_expect_mask expect_mask)
{
    struct named_expr *named_expr;
    struct ast_node_hdl *target;
    struct ast_node_hdl *filter_target;

    named_expr = (struct named_expr *) expr->ndat->u.rexpr_named_expr.named_expr;
    target = named_expr->expr;
    filter_target = expr->ndat->u.rexpr_named_expr.filter_target;
    if (-1 == compile_expr(target, ctx, TRUE)) {
        return -1;
    }
    if (NULL != filter_target) {
        if (-1 == compile_node(filter_target, ctx,
                               COMPILE_TAG_NODE_TYPE |
                               COMPILE_TAG_NODE_SPAN_SIZE |
                               COMPILE_TAG_NODE_FLAGS, 0u,
                               RESOLVE_EXPECT_TYPE |
                               RESOLVE_EXPECT_INTERPRETER)) {
            return -1;
        }
        expr->ndat->u.rexpr.target_item = filter_target;
    } else {
        expr->ndat->u.rexpr.target_item =
            (struct ast_node_hdl *)ast_node_get_as_type(target);
    }
    if (0 == (expect_mask & RESOLVE_EXPECT_EXPRESSION)) {
        expr->ndat = ast_node_get_named_expr_target(expr)->ndat;
        return 0;
    }
    expr->ndat->u.rexpr.value_type = expr_value_type_from_node(target);

    if (EXPR_DPATH_TYPE_NONE == target->ndat->u.rexpr.dpath_type) {
        expr->ndat->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    } else {
        // default dpath type is container type
        // TODO: We may optimize with EXPR_DPATH_TYPE_ITEM whenever
        // all duplicate dpath expressions use item type, though this
        // requires post-processing when all types have been
        // resolved. Container type is more universal.

        if (NULL != named_expr->nstmt.next_sibling
            || ast_node_is_filter(
                ast_node_get_named_expr_target(named_expr->expr))) {
            expr->ndat->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
        } else {
            // use the original dpath type
            expr->ndat->u.rexpr.dpath_type = target->ndat->u.rexpr.dpath_type;
        }
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
    expr->ndat->u.rexpr.value_type =
        expr_value_type_from_dpath_node(&field->dpath);
    expr->ndat->u.rexpr.dpath_type = EXPR_DPATH_TYPE_ITEM;
    expr->ndat->u.rexpr.target_item = (struct ast_node_hdl *)
        dpath_node_get_as_type(&expr->ndat->u.rexpr_field.field->dpath);
    return 0;
}

static int
compile_rexpr_member(struct ast_node_hdl *expr, struct compile_ctx *ctx)
{
    struct op *op;
    const struct named_expr *named_expr;
    struct ast_node_hdl *anchor_expr;
    struct ast_node_hdl *anchor_target;
    const struct ast_node_hdl *anchor_block, *member;
    struct named_statement *resolved_member;
    struct ast_node_data *expr_ndat;

    op = &expr->ndat->u.rexpr_op.op;
    member = op->operands[1];
    /* checked by parser */
    assert(member->ndat->type == AST_NODE_TYPE_IDENTIFIER);

    if (-1 == compile_expr(op->operands[0], ctx, TRUE)) {
        return -1;
    }
    anchor_expr = op->operands[0];
    if (AST_NODE_TYPE_REXPR_NAMED_EXPR == op->operands[0]->ndat->type) {
        named_expr = op->operands[0]->ndat->u.rexpr_named_expr.named_expr;
        anchor_target = named_expr->expr;
    } else {
        anchor_target = op->operands[0];
        named_expr = NULL;
    }
    while (NULL != anchor_target) {
        if (ast_node_is_rexpr(anchor_target)) {
            anchor_block = anchor_target->ndat->u.rexpr.target_item;
            if (NULL == anchor_block) {
                semantic_error(
                    SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                    "member operator with computed dpath not supported");
                return -1;
            }
            anchor_block = ast_node_get_named_expr_target(
                (struct ast_node_hdl *)anchor_block);
        } else {
            anchor_block = anchor_target;
        }
        assert(ast_node_is_item(anchor_block));
        if (anchor_block->ndat->type != AST_NODE_TYPE_BLOCK_DEF) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "invalid use of member operator on non-block dpath");
            return -1;
        }
        resolved_member = find_statement_by_name(
            STATEMENT_TYPE_NAMED_EXPR, member->ndat->u.identifier,
            &anchor_block->ndat->u.block_def.block_stmt_list);
        if (NULL != resolved_member) {
            expr_ndat = new_safe(struct ast_node_data);
            expr_ndat->type = AST_NODE_TYPE_REXPR_NAMED_EXPR;
            assert(NULL != anchor_expr);
            expr_ndat->u.rexpr_member_common.anchor_block =
                (struct ast_node_hdl *)anchor_block;
            expr_ndat->u.rexpr_member_common.anchor_expr = anchor_expr;
            expr_ndat->u.rexpr_named_expr.named_expr =
                (struct named_expr *)resolved_member;
            expr->ndat = expr_ndat;
            return compile_rexpr_named_expr(expr, ctx,
                                            RESOLVE_EXPECT_EXPRESSION);
        }
        resolved_member = find_statement_by_name(
            STATEMENT_TYPE_FIELD, member->ndat->u.identifier,
            &anchor_block->ndat->u.block_def.block_stmt_list);
        if (NULL != resolved_member) {
            expr_ndat = new_safe(struct ast_node_data);
            expr_ndat->type = AST_NODE_TYPE_REXPR_FIELD;
            assert(NULL != anchor_expr);
            expr_ndat->u.rexpr_member_common.anchor_block =
                (struct ast_node_hdl *)anchor_block;
            expr_ndat->u.rexpr_member_common.anchor_expr = anchor_expr;
            expr_ndat->u.rexpr_field.field = (struct field *)resolved_member;
            expr->ndat = expr_ndat;
            return compile_rexpr_field(expr, ctx);
        }

        if (NULL != named_expr) {
            named_expr = (struct named_expr *)named_expr->nstmt.next_sibling;
        }
        if (NULL != named_expr) {
            anchor_target = named_expr->expr;
        } else {
            anchor_target = NULL;
        }
    }
    semantic_error(
        SEMANTIC_LOGLEVEL_ERROR, &member->loc,
        "no named expression or field named '%s' exists in block",
        member->ndat->u.identifier);
    semantic_error(SEMANTIC_LOGLEVEL_INFO, &anchor_block->loc,
                   "declared here");
    return -1;
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
        return compile_rexpr_named_expr(node, ctx, expect_mask);
    case AST_NODE_TYPE_REXPR_INTERPRETER:
        return compile_rexpr_interpreter(node, ctx);
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        return compile_rexpr_as_type(node, ctx, expect_mask);
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

    if (-1 == compile_node_post_check(node, ctx)) {
        return -1;
    }
    return 0;
}

static int
compile_span_size_block(struct ast_node_hdl *item, struct compile_ctx *ctx)
{
    struct span_stmt *span_stmt;
    struct ast_node_hdl *min_span_expr;
    struct ast_node_hdl *max_span_expr;
    const struct statement_list *field_list;
    struct field *field;
    struct dpath_node *field_type;
    struct field *compute_offset_backwards_from;
    int64_t min_span_size;
    int dynamic_span;
    int dynamic_used;
    int child_needs_slack;
    int child_has_undetermined_size;

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
    child_needs_slack = FALSE;
    child_has_undetermined_size = FALSE;

    min_span_expr = NULL;
    max_span_expr = NULL;
    STATEMENT_FOREACH(span_stmt, span_stmt,
                      item->ndat->u.block_def.block_stmt_list.span_list, list) {
        if (-1 == compile_expr(span_stmt->span_expr, ctx, TRUE)) {
            return -1;
        }
        if (NULL == span_stmt->stmt.cond) {
            if ((span_stmt->stmt.stmt_flags & SPAN_FLAG_MIN)) {
                min_span_expr = span_stmt->span_expr;
            }
            if ((span_stmt->stmt.stmt_flags & SPAN_FLAG_MAX)) {
                max_span_expr = span_stmt->span_expr;
            }
            if (min_span_expr != max_span_expr) {
                dynamic_span = TRUE;
            }
            break ;
        } else {
            dynamic_span = TRUE;
        }
    }
    field_list = item->ndat->u.block_def.block_stmt_list.field_list;
    compute_offset_backwards_from = NULL;
    STATEMENT_FOREACH(field, field, field_list, list) {
        if (-1 == compile_field(field, ctx,
                                COMPILE_TAG_NODE_TYPE |
                                COMPILE_TAG_NODE_SPAN_SIZE |
                                COMPILE_TAG_NODE_FLAGS, 0u)) {
            return -1;
        }
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
        /* FIXME issues with multiple conditions with slack
         * fields: browse does not go through the slack fields */
        if (0 != (field_type->u.item.flags & ITEMFLAG_NEED_SLACK)) {
            child_needs_slack = TRUE;
        }
        if (0 != (field_type->u.item.flags
                  & ITEMFLAG_HAS_UNDETERMINED_SIZE)
            && NULL == field_type->filter_defining_size) {
            child_has_undetermined_size = TRUE;
            compute_offset_backwards_from = NULL;
        } else if (BLOCK_TYPE_STRUCT == item->ndat->u.block_def.type
                   && child_has_undetermined_size
                   && NULL == compute_offset_backwards_from) {
            compute_offset_backwards_from = field;
        }
    }
    for (field = compute_offset_backwards_from;
         NULL != field;
         field = (struct field *)
             TAILQ_NEXT((struct statement *)field, list)) {
        field->nstmt.stmt.stmt_flags |= FIELD_FLAG_SLACK_TRAILER;
    }

    if (NULL != min_span_expr) {
        assert(EXPR_VALUE_TYPE_INTEGER == min_span_expr->ndat->u.rexpr.value_type);
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
        assert(EXPR_VALUE_TYPE_INTEGER == max_span_expr->ndat->u.rexpr.value_type);
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
    if (NULL == max_span_expr && child_needs_slack) {
        item->ndat->u.item.flags |= ITEMFLAG_NEED_SLACK;
    }
    item->ndat->u.item.min_span_size = min_span_size;
    if (dynamic_span) {
        item->ndat->u.item.flags |= ITEMFLAG_IS_SPAN_SIZE_DYNAMIC;
    }
    if (dynamic_used) {
        item->ndat->u.item.flags |= ITEMFLAG_IS_USED_SIZE_DYNAMIC;
    }
    if (child_has_undetermined_size && NULL == max_span_expr) {
        item->ndat->u.item.flags |= ITEMFLAG_HAS_UNDETERMINED_SIZE;
    }
    if (NULL != compute_offset_backwards_from) {
        item->flags |= ASTFLAG_HAS_FOOTER;
    }
    if (!TAILQ_EMPTY(item->ndat->u.block_def.block_stmt_list.last_stmt_list)) {
        item->flags |= ASTFLAG_CONTAINS_LAST_STMT;
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
        if (EXPR_VALUE_TYPE_INTEGER !=
            item_count_expr->ndat->u.rexpr.value_type) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &item_count_expr->loc,
                           "invalid array size type: expect '%s', got '%s'",
                           expr_value_type_str(EXPR_VALUE_TYPE_INTEGER),
                           expr_value_type_str(
                               item_count_expr->ndat->u.rexpr.value_type));
            return -1;
        }
    }
    item_dpath = &array->ndat->u.array.item_type;
    if (NULL != item_count_expr
        && item_count_expr->ndat->type == AST_NODE_TYPE_REXPR_NATIVE
        && item_count_expr->ndat->u.rexpr_native.value.integer > 0) {
        // compile item type as a dependency because the array
        // contains a fixed number of at least one item
        if (-1 == compile_dpath(item_dpath, ctx,
                                COMPILE_TAG_NODE_TYPE |
                                COMPILE_TAG_NODE_SPAN_SIZE |
                                COMPILE_TAG_NODE_FLAGS, 0u)) {
            return -1;
        }
        item_type = ast_node_get_target_item(item_dpath->item);
        assert(ast_node_is_item(item_type));
        assert(SPAN_SIZE_UNDEF != item_type->ndat->u.item.min_span_size);
        assert(EXPR_VALUE_TYPE_INTEGER
               == item_count_expr->ndat->u.rexpr.value_type);
        assert(SPAN_SIZE_UNDEF != item_type->ndat->u.item.min_span_size);
        item_count = item_count_expr->ndat->u.rexpr_native.value.integer;
        min_span_size = item_count * item_type->ndat->u.item.min_span_size;
        dynamic_span =
            (0 != (item_dpath->u.item.flags & ASTFLAG_CONTAINS_LAST_STMT)
             || 0 != (item_dpath->u.item.flags
                      & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC));
    } else {
        // schedule compilation of item type and size without
        // depending on it, so to allow recursive nesting of items and
        // possibly empty arrays
        if (-1 == compile_dpath(item_dpath, ctx,
                                0u, (COMPILE_TAG_NODE_TYPE |
                                     COMPILE_TAG_NODE_SPAN_SIZE |
                                     COMPILE_TAG_NODE_FLAGS))) {
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
    return compile_node(array, ctx, 0u, COMPILE_TAG_NODE_FLAGS,
                        RESOLVE_EXPECT_TYPE);
}

static int
compile_node_flags_array(struct ast_node_hdl *array, struct compile_ctx *ctx)
{
    struct ast_node_hdl *item_count_expr;
    struct dpath_node *item_dpath;
    struct ast_node_hdl *item_type;

    item_count_expr =
        ast_node_get_named_expr_target(array->ndat->u.array.item_count);
    item_dpath = &array->ndat->u.array.item_type;
    item_type = ast_node_get_target_item(item_dpath->item);
    assert(ast_node_is_item(item_type));

    if (0 == (item_type->flags & ASTFLAG_CONTAINS_LAST_STMT)
        && (NULL == item_count_expr
            || 0 != (item_dpath->u.item.flags & ITEMFLAG_NEED_SLACK))) {
        array->ndat->u.item.flags |= ITEMFLAG_NEED_SLACK;
    }
    if (NULL == item_count_expr
        || 0 != (item_dpath->u.item.flags
                 & ITEMFLAG_HAS_UNDETERMINED_SIZE)) {
        array->ndat->u.item.flags |= ITEMFLAG_HAS_UNDETERMINED_SIZE;
    }
    return 0;
}

static int
compile_span_size_common(struct ast_node_hdl *item,
                         struct ast_node_hdl *filter,
                         struct compile_ctx *ctx)
{
    struct ast_node_hdl *as_item;

    if (NULL != item) {
        assert(ast_node_is_item(item));
        if (-1 == compile_node(item, ctx,
                               COMPILE_TAG_NODE_TYPE |
                               COMPILE_TAG_NODE_SPAN_SIZE |
                               COMPILE_TAG_NODE_FLAGS, 0u,
                               RESOLVE_EXPECT_TYPE)) {
            return -1;
        }
    }
    if (NULL != filter) {
        struct ast_node_hdl *filter_expr;
        struct ast_node_hdl *filter_target;

        filter_expr = ast_node_get_named_expr_target(filter);
        assert(ast_node_is_filter(filter_expr));
        as_item = (struct ast_node_hdl *)ast_node_get_as_type(filter_expr);
        filter_target = filter_expr->ndat->u.rexpr_filter.target;
        if (NULL != filter_target) {
            compile_node(filter_target, ctx,
                         0u, (COMPILE_TAG_NODE_TYPE |
                              COMPILE_TAG_NODE_SPAN_SIZE |
                              COMPILE_TAG_NODE_FLAGS),
                         RESOLVE_EXPECT_TYPE |
                         RESOLVE_EXPECT_INTERPRETER);
        }
    } else {
        as_item = item;
    }
    as_item = ast_node_get_named_expr_target(as_item);
    if (NULL != as_item && as_item != item
        && ast_node_is_item(as_item)) {
        if (SPAN_SIZE_UNDEF == as_item->ndat->u.item.min_span_size) {
            if (-1 == compile_node(as_item, ctx,
                                   (COMPILE_TAG_NODE_TYPE |
                                    COMPILE_TAG_NODE_SPAN_SIZE |
                                    COMPILE_TAG_NODE_FLAGS), 0u,
                                   RESOLVE_EXPECT_TYPE)) {
                return -1;
            }
        }
        if (NULL != item
            && 0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)
            && as_item->ndat->u.item.min_span_size > item->ndat->u.item.min_span_size) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &as_item->loc,
                "invalid as-type filter: cast-to type minimum size is "
                "greater than static size of destination "
                "(as type size %s %"PRIi64", target size == %"PRIi64")",
                (0 != (as_item->ndat->u.item.flags
                       & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC) ?
                 ">=" : "=="),
                as_item->ndat->u.item.min_span_size,
                item->ndat->u.item.min_span_size);
            return -1;
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
        if (EXPR_VALUE_TYPE_INTEGER != size_expr->ndat->u.rexpr.value_type) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &size_expr->loc,
                           "invalid byte array size type: expect '%s', "
                           "got '%s'",
                           expr_value_type_str(EXPR_VALUE_TYPE_INTEGER),
                           expr_value_type_str(size_expr->ndat->u.rexpr.value_type));
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
        byte_array->ndat->u.item.flags |= (ITEMFLAG_NEED_SLACK |
                                           ITEMFLAG_HAS_UNDETERMINED_SIZE);
    }
    byte_array->ndat->u.item.min_span_size = min_span_size;
    return 0;
}

static int
compile_node_span_size(struct ast_node_hdl *node,
                       struct compile_ctx *ctx)
{
    struct ast_node_hdl *target;
    struct ast_node_hdl *item;
    struct ast_node_hdl *filter;

    switch (node->ndat->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        return compile_span_size_block(node, ctx);
    case AST_NODE_TYPE_ARRAY:
        return compile_span_size_array(node, ctx);
    case AST_NODE_TYPE_BYTE:
        return compile_span_size_byte(node, ctx);
    case AST_NODE_TYPE_BYTE_ARRAY:
        return compile_span_size_byte_array(node, ctx);
    default:
        target = ast_node_get_named_expr_target(node);
        item = ast_node_get_target_item(target);
        filter = (ast_node_is_filter(target) ? target : NULL);
        return compile_span_size_common(item, filter, ctx);
    }
}

static int
compile_node_flags(struct ast_node_hdl *node,
                   struct compile_ctx *ctx)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
        return compile_node_flags_array(node, ctx);
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
    if (resolved_tags == tags) {
        compile_req_destroy(req);
    }
    ctx->current_req = NULL;
    ctx->current_node = NULL;
    ctx->current_tags = 0u;
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
    if (0 != (tags & COMPILE_TAG_NODE_FLAGS)) {
        if (-1 == compile_node_flags(node, ctx)) {
            return resolved_tags;
        }
        resolved_tags |= COMPILE_TAG_NODE_FLAGS;
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
    if (0 != (tags & COMPILE_TAG_NODE_FLAGS)) {
        if (-1 == compile_dpath_flags(node, ctx)) {
            return resolved_tags;
        }
        resolved_tags |= COMPILE_TAG_NODE_FLAGS;
    }
    return resolved_tags;
}

static int
compile_dpath_type(struct dpath_node *node,
                   struct compile_ctx *ctx)
{
    struct ast_node_hdl *expr;
    struct ast_node_hdl *expr_target;
    struct ast_node_hdl *target_item;

    if (-1 == compile_node(node->item, ctx, COMPILE_TAG_NODE_TYPE, 0u,
                           RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    expr = node->item;
    assert(ast_node_is_item(expr) || ast_node_is_rexpr(expr));
    if (NULL != expr) {
        expr_target = ast_node_get_named_expr_target(expr);
        target_item = ast_node_get_target_item(expr);
        if (NULL == target_item) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "expression is not a valid dpath: no target item");
            return -1;
        }
        node->item = target_item;
        if (ast_node_is_filter(expr_target)) {
            node->filter = expr_target;
            if (-1 == compile_dpath_set_filter_defining_size(node, ctx)) {
                return -1;
            }
        }
    }
    return 0;
}

static int
compile_dpath_span_size(struct dpath_node *node, struct compile_ctx *ctx)
{
    if (NULL == node->item) {
        return 0;
    }
    if (-1 == compile_span_size_common(node->item, node->filter, ctx)) {
        return -1;
    }
    node->u.item = node->item->ndat->u.item;
    return 0;
}

static int
compile_dpath_flags(struct dpath_node *node, struct compile_ctx *ctx)
{
    if (NULL == node->item) {
        return 0;
    }
    if (-1 == compile_node(node->item, ctx,
                           COMPILE_TAG_NODE_FLAGS, 0u,
                           RESOLVE_EXPECT_TYPE)) {
        return -1;
    }
    node->u.item = node->item->ndat->u.item;
    return 0;
}

static int
compile_node_post_check_span_expr(struct ast_node_hdl *span_expr,
                                  struct compile_ctx *ctx)
{
    assert(ast_node_is_rexpr(span_expr));
    if (span_expr->ndat->u.rexpr.value_type != EXPR_VALUE_TYPE_INTEGER) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &span_expr->loc,
            "span expression must be of integer type, not '%s'",
            expr_value_type_str(span_expr->ndat->u.rexpr.value_type));
        return -1;
    }
    return 0;
}

static int
compile_node_post_check_key_expr(struct ast_node_hdl *key_expr,
                                 struct compile_ctx *ctx)
{
    assert(ast_node_is_rexpr(key_expr));
    if (key_expr->ndat->u.rexpr.value_type != EXPR_VALUE_TYPE_INTEGER &&
        key_expr->ndat->u.rexpr.value_type != EXPR_VALUE_TYPE_STRING) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &key_expr->loc,
            "index expression must be of integer or string type, not '%s'",
            expr_value_type_str(key_expr->ndat->u.rexpr.value_type));
        return -1;
    }
    return 0;
}

static int
compile_node_post_check_match_expr(struct ast_node_hdl *match_expr,
                                   struct compile_ctx *ctx)
{
    assert(ast_node_is_rexpr(match_expr));
    if (match_expr->ndat->u.rexpr.value_type != EXPR_VALUE_TYPE_BOOLEAN) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &match_expr->loc,
            "match expression must be of boolean type, not '%s'",
            expr_value_type_str(match_expr->ndat->u.rexpr.value_type));
        return -1;
    }
    if (match_expr->ndat->type == AST_NODE_TYPE_REXPR_NATIVE) {
        if (match_expr->ndat->u.rexpr_native.value.boolean) {
            semantic_error(SEMANTIC_LOGLEVEL_WARNING, &match_expr->loc,
                           "match expression always true");
        } else {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &match_expr->loc,
                           "match expression always false");
            return -1;
        }
    }
    return 0;
}

static int
compile_node_post_check(struct ast_node_hdl *expr,
                        struct compile_ctx *ctx)
{
    if (0 != (expr->flags & ASTFLAG_IS_SPAN_EXPR)
        && -1 == compile_node_post_check_span_expr(expr, ctx)) {
        return -1;
    }
    if (0 != (expr->flags & ASTFLAG_IS_KEY_EXPR)
        && -1 == compile_node_post_check_key_expr(expr, ctx)) {
        return -1;
    }
    if (0 != (expr->flags & ASTFLAG_IS_MATCH_EXPR)
        && -1 == compile_node_post_check_match_expr(expr, ctx)) {
        return -1;
    }
    return 0;
}


/*
 * track backend
 */

static int
setup_global_track_backends(void)
{
    if (-1 == setup_track_backends_dpath(DPATH_NODE_BYTE)) {
        return -1;
    }
    if (-1 == setup_track_backends_dpath(DPATH_NODE_ARRAY_SLICE)) {
        return -1;
    }
    if (-1 == setup_track_backends_dpath(DPATH_NODE_BYTE_SLICE)) {
        return -1;
    }
    if (-1 == setup_track_backends_dpath(DPATH_NODE_AS_BYTES)) {
        return -1;
    }
    return 0;
}

static int
setup_track_backends_dpath(struct dpath_node *dpath)
{
    int ret = 0;

    if (NULL != dpath->item) {
        ret = setup_track_backends_expr(dpath->item);
        if (0 != ret) {
            return ret;
        }
    }
    if (NULL != dpath->filter) {
        ret = setup_track_backends_expr(dpath->filter);
        if (0 != ret) {
            return ret;
        }
    }
    return 0;
}

static int
setup_track_backends_stmt_list_generic(struct statement_list *stmt_list)
{
    struct statement *stmt;

    TAILQ_FOREACH(stmt, stmt_list, list) {
        if (NULL != stmt->cond
            && -1 == setup_track_backends_expr(stmt->cond)) {
            return -1;
        }
    }
    return 0;
}

static int
setup_track_backends_recur_block(struct ast_node_hdl *block)
{
    struct block_stmt_list *stmt_lists;
    struct field *field;
    struct span_stmt *span_stmt;
    struct key_stmt *key_stmt;
    struct match *match;
    struct named_expr *named_expr;

    stmt_lists = &block->ndat->u.block_def.block_stmt_list;
    if (-1 == setup_track_backends_stmt_list_generic(
            stmt_lists->named_expr_list)) {
        return -1;
    }
    if (-1 == setup_track_backends_stmt_list_generic(
            stmt_lists->field_list)) {
        return -1;
    }
    if (-1 == setup_track_backends_stmt_list_generic(
            stmt_lists->span_list)) {
        return -1;
    }
    if (-1 == setup_track_backends_stmt_list_generic(
            stmt_lists->last_stmt_list)) {
        return -1;
    }
    STATEMENT_FOREACH(named_expr, named_expr,
                      stmt_lists->named_expr_list, list) {
        if (-1 == setup_track_backends_expr(named_expr->expr)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(field, field, stmt_lists->field_list, list) {
        if (-1 == setup_track_backends_dpath(
                (struct dpath_node *)&field->dpath)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(span_stmt, span_stmt, stmt_lists->span_list, list) {
        if (-1 == setup_track_backends_expr(span_stmt->span_expr)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(key_stmt, key_stmt, stmt_lists->key_list, list) {
        if (-1 == setup_track_backends_expr(key_stmt->key_expr)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(match, match, stmt_lists->match_list, list) {
        if (-1 == setup_track_backends_expr(match->expr)) {
            return -1;
        }
    }
    return 0;
}

static int
setup_track_backends_subscript_index(struct ast_node_hdl *expr,
                                     struct subscript_index *subscript)
{
    int ret;

    if (NULL == subscript->key) {
        return 0;
    }
    ret = setup_track_backends_expr(subscript->key);
    if (-1 == ret) {
        return -1;
    }
    if (NULL != subscript->twin) {
        ret = setup_track_backends_expr(subscript->twin);
        if (-1 == ret) {
            return -1;
        }
    }
    return 0;
}

static int
setup_track_backends_subscript(struct ast_node_hdl *expr)
{
    int ret;
    struct subscript_index *index;
    struct ast_node_hdl *anchor_expr;

    anchor_expr = expr->ndat->u.rexpr_op_subscript_common.anchor_expr;
    assert(NULL != anchor_expr);
    ret = setup_track_backends_expr(anchor_expr);
    if (-1 == ret) {
        return -1;
    }
    index = &expr->ndat->u.rexpr_op_subscript.index;
    ret = setup_track_backends_subscript_index(expr, index);
    if (-1 == ret) {
        return -1;
    }
    return 0;
}

static int
setup_track_backends_subscript_slice(struct ast_node_hdl *expr)
{
    int ret;
    struct ast_node_hdl *anchor_expr;
    struct subscript_index *slice_start;
    struct subscript_index *slice_end;

    anchor_expr = expr->ndat->u.rexpr_op_subscript_common.anchor_expr;
    assert(NULL != anchor_expr);
    ret = setup_track_backends_expr(anchor_expr);
    if (-1 == ret) {
        return -1;
    }
    slice_start = &expr->ndat->u.rexpr_op_subscript_slice.start;
    slice_end = &expr->ndat->u.rexpr_op_subscript_slice.end;
    ret = setup_track_backends_subscript_index(expr, slice_start);
    if (-1 == ret) {
        return -1;
    }
    ret = setup_track_backends_subscript_index(expr, slice_end);
    if (-1 == ret) {
        return -1;
    }
    return 0;
}

static int
setup_track_backends_fcall(struct ast_node_hdl *expr)
{
    struct statement *stmt;
    struct named_expr *param;

    /* resolve expressions in parameter list */
    TAILQ_FOREACH(stmt, expr->ndat->u.rexpr_op_fcall.func_params, list) {
        param = (struct named_expr *)stmt;
        if (-1 == setup_track_backends_expr(param->expr)) {
            return -1;
        }
    }
    return 0;
}

static int
setup_track_backends_expr(struct ast_node_hdl *expr)
{
    int ret = 0;

    if (0 != (expr->flags & (ASTFLAG_SETUP_TRACK_BACKENDS_IN_PROGRESS |
                             ASTFLAG_SETUP_TRACK_BACKENDS_COMPLETED))) {
        return 0;
    }
    expr->flags |= ASTFLAG_SETUP_TRACK_BACKENDS_IN_PROGRESS;
    switch (expr->ndat->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        ret = setup_track_backends_recur_block(expr);
        break ;
    case AST_NODE_TYPE_ARRAY:
        ret = setup_track_backends_dpath(&expr->ndat->u.array.item_type);
        break ;
    case AST_NODE_TYPE_CONDITIONAL:
        ret = setup_track_backends_expr(expr->ndat->u.conditional.cond_expr);
        break ;
    case AST_NODE_TYPE_REXPR_INTERPRETER:
        if (NULL != expr->ndat->u.rexpr_filter.target) {
            ret = setup_track_backends_expr(expr->ndat->u.rexpr_filter.target);
        }
        break ;
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        if (NULL != expr->ndat->u.rexpr_filter.target) {
            ret = setup_track_backends_expr(expr->ndat->u.rexpr_filter.target);
        }
        if (0 == ret) {
            ret = setup_track_backends_dpath(
                &expr->ndat->u.rexpr_filter.filter_dpath);
        }
        break ;
    case AST_NODE_TYPE_REXPR_FIELD:
        return setup_track_backends_dpath(
            (struct dpath_node *)&expr->ndat->u.rexpr_field.field->dpath);
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        return setup_track_backends_expr(
            expr->ndat->u.rexpr_named_expr.named_expr->expr);
    case AST_NODE_TYPE_REXPR_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT:
    case AST_NODE_TYPE_REXPR_OP_ANCESTOR:
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
    case AST_NODE_TYPE_REXPR_OP_SIZEOF:
    case AST_NODE_TYPE_REXPR_OP_ADDROF:
        ret = setup_track_backends_expr(expr->ndat->u.rexpr_op.op.operands[0]);
        if (0 == ret && NULL != expr->ndat->u.rexpr_op.op.operands[1]) {
            ret = setup_track_backends_expr(expr->ndat->u.rexpr_op.op.operands[1]);
        }
        break ;
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
        ret = setup_track_backends_subscript(expr);
        break ;
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        ret = setup_track_backends_subscript_slice(expr);
        break ;
    case AST_NODE_TYPE_REXPR_OP_FCALL:
        ret = setup_track_backends_fcall(expr);
        break ;
    default:
        break ;
    }
    if (-1 == browse_setup_backends(expr)) {
        return -1;
    }
    expr->flags |= ASTFLAG_SETUP_TRACK_BACKENDS_COMPLETED;
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
    case AST_NODE_TYPE_REXPR_FIELD:
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
    case AST_NODE_TYPE_REXPR_BUILTIN:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
    case AST_NODE_TYPE_REXPR_OP_MEMBER:
    case AST_NODE_TYPE_REXPR_OP_FCALL:
    case AST_NODE_TYPE_REXPR_AS_TYPE:
    case AST_NODE_TYPE_REXPR_FILE:
    case AST_NODE_TYPE_REXPR_SELF:
    case AST_NODE_TYPE_REXPR_STAR_WILDCARD:
    case AST_NODE_TYPE_REXPR_INTERPRETER:
        return TRUE;
    default:
        return FALSE;
    }
}

int
ast_node_is_rexpr_to_item(const struct ast_node_hdl *node)
{
    if (AST_NODE_TYPE_REXPR_NAMED_EXPR == node->ndat->type) {
        const struct ast_node_hdl *target;

        target = node->ndat->u.rexpr_named_expr.named_expr->expr;
        if (ast_node_is_item(target)) {
            return TRUE;
        }
        return ast_node_is_rexpr_to_item(target);
    }
    switch (node->ndat->type) {
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_REXPR_AS_TYPE:
    case AST_NODE_TYPE_REXPR_FIELD:
    case AST_NODE_TYPE_REXPR_FILE:
    case AST_NODE_TYPE_REXPR_SELF:
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
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        if (NULL != node->ndat->u.rexpr_named_expr.filter_target) {
            return ast_node_get_target_item(
                node->ndat->u.rexpr_named_expr.filter_target);
        } else {
            return ast_node_get_target_item(
                node->ndat->u.rexpr_named_expr.named_expr->expr);
        }
    case AST_NODE_TYPE_REXPR_FIELD:
        return ast_node_get_target_item(node->ndat->u.rexpr_field.field->dpath.item);
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        return ast_node_get_target_item(node->ndat->u.rexpr_filter.target);
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
        if (NULL != node->ndat->u.rexpr_named_expr.filter_target) {
            return ast_node_get_target_type(
                node->ndat->u.rexpr_named_expr.filter_target);
        } else {
            return ast_node_get_target_type(
                node->ndat->u.rexpr_named_expr.named_expr->expr);
        }
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        return ast_node_get_target_type(node->ndat->u.rexpr_filter.target);
    default:
        return NULL;
    }
}

struct ast_node_hdl *
ast_node_get_target_filter(struct ast_node_hdl *node)
{
    if (NULL == node || ast_node_is_item(node)) {
        return NULL;
    }
    switch (node->ndat->type) {
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        return ast_node_get_target_filter(
            node->ndat->u.rexpr_named_expr.named_expr->expr);
    case AST_NODE_TYPE_REXPR_FIELD:
        if (NULL != node->ndat->u.rexpr_field.field->dpath.filter) {
            return node->ndat->u.rexpr_field.field->dpath.filter;
        } else {
            return ast_node_get_target_filter(
                node->ndat->u.rexpr_field.field->dpath.item);
        }
    case AST_NODE_TYPE_REXPR_OP_ANCESTOR:
        return ast_node_get_target_filter(
            node->ndat->u.rexpr_op.op.operands[0]);
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        return node;
    default:
        return NULL;
    }
}

struct ast_node_hdl *
ast_node_get_named_expr_target(struct ast_node_hdl *node)
{
    if (NULL == node
        || AST_NODE_TYPE_REXPR_NAMED_EXPR != node->ndat->type
        || NULL != node->ndat->u.rexpr_named_expr.named_expr->nstmt.next_sibling) {
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
    return ast_node_is_item(node)
        || AST_NODE_TYPE_REXPR_AS_TYPE == node->ndat->type;
}


int
ast_node_is_filter(const struct ast_node_hdl *node)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        return TRUE;
    default:
        return FALSE;
    }
}


static const struct ast_node_hdl *
ast_node_get_as_type__rexpr(const struct ast_node_hdl *expr)
{
    const struct ast_node_hdl *as_type;

    switch (expr->ndat->type) {
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        as_type = ast_node_get_as_type(
            expr->ndat->u.rexpr_named_expr.named_expr->expr);
        if (NULL != as_type && ast_node_is_item(as_type)) {
            return as_type;
        }
        as_type = expr->ndat->u.rexpr_named_expr.filter_target;
        if (NULL != as_type) {
            return ast_node_get_as_type(as_type);
        }
        return NULL;

    case AST_NODE_TYPE_REXPR_AS_TYPE:
        return ast_node_get_as_type(expr->ndat->u.rexpr_filter.filter_dpath.item);

    case AST_NODE_TYPE_REXPR_FIELD:
        if (NULL == expr->ndat->u.rexpr_field.field->dpath.filter) {
            return expr->ndat->u.rexpr_field.field->dpath.item;
        }
        return ast_node_get_as_type(expr->ndat->u.rexpr_field.field->dpath.filter);

    case AST_NODE_TYPE_OP_SET_FILTER:
        return ast_node_get_as_type(expr->ndat->u.op.operands[1]);

    default:
        if (ast_node_is_rexpr(expr)) {
            return expr->ndat->u.rexpr.target_item;
        }
        if (ast_node_is_item(expr)) {
            return expr;
        }
        return NULL;
    }
}


const struct ast_node_hdl *
ast_node_get_as_type(const struct ast_node_hdl *node)
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
        return !TAILQ_EMPTY(target->ndat->u.block_def.block_stmt_list.key_list);
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

        if (TAILQ_EMPTY(target->ndat->u.block_def.block_stmt_list.key_list)) {
            return NULL;
        }
        return ((struct key_stmt *)TAILQ_FIRST(
                    target->ndat->u.block_def
                    .block_stmt_list.key_list))->key_expr;
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
    return key_expr->ndat->u.rexpr.value_type;
}

const struct ast_node_hdl *
dpath_node_get_as_type(const struct dpath_node *dpath)
{
    if (NULL != dpath->filter) {
        return ast_node_get_as_type(dpath->filter);
    } else {
        return ast_node_get_as_type(dpath->item);
    }
}

const struct ast_node_hdl *
dpath_node_get_value_node(const struct dpath_node *dpath)
{
    if (NULL != dpath->filter) {
        return dpath->filter;
    } else {
        return dpath->item;
    }
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
    fprintf(out, "value_type: %s, dpath_type: %s",
            expr_value_type_str(node->ndat->u.rexpr.value_type),
            expr_dpath_type_str(node->ndat->u.rexpr.dpath_type));
}

static void
dump_target_item(const struct ast_node_hdl *node, int depth,
                 struct list_of_visible_refs *visible_refs, FILE *out)
{
    if (NULL != node->ndat->u.rexpr.target_item) {
        fprintf(out, "\n%*s\\_ target:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_type(node->ndat->u.rexpr.target_item,
                      depth + 2, visible_refs, out);
    }
}

static void
dump_ast_rexpr_member(const struct ast_node_hdl *node, int depth,
                      struct list_of_visible_refs *visible_refs, FILE *out)
{
    dump_target_item(node, depth, visible_refs, out);
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
        assert(NULL != visible_refs); /* must be in a block */
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
        switch (node->ndat->u.rexpr_native.rexpr.value_type) {
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
        struct interpreter_param_def *param_def;
        struct ast_node_hdl *param;
        int i;

        dump_ast_rexpr(node,out);
        interpreter = node->ndat->u.rexpr_interpreter.interpreter;
        fprintf(out, " name: %s\n%*s\\_ interpreter params:\n",
                interpreter->name, (depth + 1) * INDENT_N_SPACES, "");
        param_def = STAILQ_FIRST(&interpreter->param_list);
        for (i = 0; i < node->ndat->u.rexpr_interpreter.interpreter->n_params;
             ++i) {
            param = INTERPRETER_RCALL_PARAM(node, i);
            fprintf(out, "%*s\\_ \"%s\" [%d]:\n",
                    (depth + 2) * INDENT_N_SPACES, "",
                    param_def->name, i);
            fdump_ast_recur(param, depth + 3, visible_refs, out);
            param_def = STAILQ_NEXT(param_def, list);
        }
        fprintf(out, "%*s\\_ target:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->ndat->u.rexpr_filter.target, depth + 2,
                        visible_refs, out);
        break ;
    }
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        dump_ast_rexpr(node, out);
        fprintf(out, "\n%*s\\_ target:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->ndat->u.rexpr_filter.target, depth + 2,
                        visible_refs, out);
        fprintf(out, "%*s\\_ as type:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->ndat->u.rexpr_filter.filter_dpath.item, depth + 2,
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
    case AST_NODE_TYPE_REXPR_BUILTIN:
        dump_ast_rexpr(node, out);
        dump_target_item(node, depth, visible_refs, out);
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
    case AST_NODE_TYPE_REXPR_BUILTIN:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
    case AST_NODE_TYPE_REXPR_OP_FCALL:
    case AST_NODE_TYPE_REXPR_AS_TYPE:
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
