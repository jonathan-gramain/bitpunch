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

#define RESOLVE2_CIRCULAR_DEPENDENCY -2

struct list_of_visible_refs {
    const struct list_of_visible_refs *outer_refs;
    const struct ast_node             *cur_block;
    const struct block_stmt_list      *cur_lists;
};

enum resolve_step {
    RESOLVE_NAMES_IN_BLOCKS,
    RESOLVE_NAMES_IN_EXPRESSIONS,
    RESOLVE_EXPRESSION_TYPES,
    RESOLVE_SPAN_SIZES,
    RESOLVE_CONSOLIDATE_NAMED_EXPRS,
};

enum resolve_expect_mask {
    RESOLVE_EXPECT_TYPE             = (1u<<0),
    RESOLVE_EXPECT_INTERPRETER      = (1u<<1),
    RESOLVE_EXPECT_VALUE_EXPRESSION = (1u<<2),
    RESOLVE_EXPECT_DPATH_EXPRESSION = (1u<<3),
    RESOLVE_EXPECT_EXPRESSION = (RESOLVE_EXPECT_DPATH_EXPRESSION |
                                 RESOLVE_EXPECT_VALUE_EXPRESSION),
};

struct resolve2_ctx;

typedef int (*resolve2_node_cbfunc_t)(struct ast_node *node,
                                      struct resolve2_ctx *ctx,
                                      void *cb_arg);

typedef int (*resolve2_dpath_cbfunc_t)(struct dpath_node *dpath,
                                       struct resolve2_ctx *ctx,
                                       void *cb_arg);

struct scheduled_entry {
    STAILQ_ENTRY(scheduled_entry) list;
    struct ast_node *node;
    struct dpath_node *dpath;
    enum resolve_step resolve_step;
    void *cb;
    void *cb_arg;
};

struct resolve2_ctx {
    STAILQ_HEAD(scheduled_list, scheduled_entry) scheduled;
};


static const char *
reverse_lookup_typename(const struct ast_node *node,
                        const struct list_of_visible_refs *visible_refs);
static int
chain_duplicate_statements_in(const struct statement_list *in_list,
                              const struct statement_list *toplevel_list);
static int
chain_duplicate_statements(const struct block_stmt_list *stmt_lists);
static int
resolve_names(struct ast_node **node_p,
              const struct list_of_visible_refs *visible_refs,
              enum resolve_step resolve_step,
              enum resolve_expect_mask expect_mask);
static int
resolve_names_int(struct ast_node *node,
                  const struct list_of_visible_refs *visible_refs,
                  enum resolve_step resolve_step,
                  enum resolve_expect_mask expect_mask,
                  struct ast_node **resolved_typep);
static int
resolve_names_identifier(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_in_blocks_identifier(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_byte(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_block(
    struct ast_node *block,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_in_blocks_block(
    struct ast_node *block,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_stmt_list_types_generic(
    struct statement_list *stmt_list,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step);
static int
resolve_block_stmt_lists_types(
    struct ast_node *block,
    const struct list_of_visible_refs *outer_refs,
    enum resolve_step resolve_step);
static int
resolve_names_dpath_node(
    struct dpath_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step);
static int
resolve_names_field(
    struct field *field,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step);
static int
resolve_names_array(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_in_expressions_array(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_byte_array(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_conditional(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_op_subscript(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_in_expressions_op_subscript(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_op_subscript_slice(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_in_expressions_op_subscript_slice(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_op_fcall(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_in_expressions_op_fcall(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_operator(
    struct ast_node *node,
    int n_operands,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_in_expressions_operator(
    struct ast_node *node,
    int n_operands,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_operator_set_filter(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_in_expressions_operator_set_filter(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);

static int
resolve_expr_types(struct ast_node **expr_p,
                   const struct list_of_visible_refs *visible_refs,
                   enum resolve_step resolve_step);
static int
resolve_expr_integer(struct ast_node *node,
                     enum resolve_step resolve_step,
                     enum resolve_expect_mask expect_mask,
                     struct ast_node **resolved_typep);
static int
resolve_expr_boolean(struct ast_node *node,
                     enum resolve_step resolve_step,
                     enum resolve_expect_mask expect_mask,
                     struct ast_node **resolved_typep);
static int
resolve_expr_string_literal(struct ast_node *node,
                            enum resolve_step resolve_step,
                            enum resolve_expect_mask expect_mask,
                            struct ast_node **resolved_typep);
static int
resolve_expr_operator_sizeof(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_expr_operator_addrof(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_expr_operator_filter(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_expr_file(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_expr_self(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_expr_star_wildcard(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_rexpr_named_expr(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_names_in_expressions_named_expr(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_rexpr_consolidate_named_expr(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_rexpr_field(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_rexpr_interpreter(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_rexpr_as_type(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_rexpr_operator(
    struct ast_node *expr,
    int n_operands,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_rexpr_operator_sizeof(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_rexpr_op_subscript(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_rexpr_op_subscript_slice(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_rexpr_op_fcall(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep);

static enum ast_node_type
op_type_ast2rexpr(enum ast_node_type type);

static int
resolve_user_expr_scoped_recur(struct ast_node **expr_p,
                               struct box *cur_scope,
                               struct list_of_visible_refs *inner_refs,
                               struct list_of_visible_refs *inmost_refs);

static int
resolve2_ast_node_block(struct ast_node *block, struct resolve2_ctx *ctx);
static int
resolve2_ast_node_array(struct ast_node *node, struct resolve2_ctx *ctx);
static int
resolve2_ast_node_byte_array(struct ast_node *node,
                             struct resolve2_ctx *ctx);
static int
named_expr_check_duplicates(struct named_expr *named_expr);
static int
named_exprs_check_duplicates(struct statement_list *named_expr_list);
static int
resolve2_stmt_list_generic(struct statement_list *stmt_list,
                           struct resolve2_ctx *ctx);
static int
resolve2_stmt_lists(struct block_stmt_list *stmt_lists,
                    struct resolve2_ctx *ctx);
static int
resolve2_dtype(struct ast_node *dtype, struct resolve2_ctx *ctx);
static int
resolve2_conditional(struct ast_node *if_node, struct resolve2_ctx *ctx);
static int
resolve2_dpath_node(struct dpath_node *node, struct resolve2_ctx *ctx);
static void
schedule_resolve_expression_types(struct ast_node *node,
                                  struct resolve2_ctx *ctx,
                                  resolve2_node_cbfunc_t cb, void *cb_arg);
static void
schedule_resolve_dpath_node(struct dpath_node *dpath,
                            struct resolve2_ctx *ctx,
                            resolve2_dpath_cbfunc_t cb, void *cb_arg);
static void
schedule_resolve_span_size_dpath(struct dpath_node *dpath,
                                 struct resolve2_ctx *ctx,
                                 resolve2_dpath_cbfunc_t cb, void *cb_arg);
static void
schedule_resolve_span_size_expr(struct ast_node *expr,
                                struct resolve2_ctx *ctx,
                                resolve2_node_cbfunc_t cb, void *cb_arg);
static int
resolve2_all(struct ast_node *ast_root);
static int
resolve2_ast_node(struct ast_node *expr, struct resolve2_ctx *ctx);
static int
resolve2_ast_node_member(struct ast_node *expr,
                         struct resolve2_ctx *ctx);
static int
resolve2_ast_node_named_expr(struct ast_node *expr,
                             struct resolve2_ctx *ctx);
static int
resolve2_ast_node_field(struct ast_node *expr, struct resolve2_ctx *ctx);
static int
resolve2_ast_node_subscript_index(struct ast_node *expr,
                                  struct subscript_index *subscript,
                                  struct resolve2_ctx *ctx);
static int
resolve2_ast_node_subscript(struct ast_node *expr,
                            struct resolve2_ctx *ctx);
static int
resolve2_ast_node_subscript_slice(struct ast_node *expr,
                                  struct resolve2_ctx *ctx);
static int
resolve2_ast_node_operator(struct ast_node *expr, int n_operands,
                           struct resolve2_ctx *ctx);
static int
resolve2_ast_node_operator_sizeof(struct ast_node *expr,
                                  struct resolve2_ctx *ctx);
static int
resolve2_ast_node_operator_addrof(struct ast_node *expr,
                                  struct resolve2_ctx *ctx);
static int
resolve2_ast_node_fcall(struct ast_node *expr, struct resolve2_ctx *ctx);
static int
resolve2_ast_node_interpreter(struct ast_node *expr,
                              struct resolve2_ctx *ctx);
static int
resolve2_ast_node_as_type(struct ast_node *expr,
                          struct resolve2_ctx *ctx);
static int
resolve2_span_size_dpath(struct dpath_node *node, struct resolve2_ctx *ctx);
static int
resolve2_span_size_expr(struct ast_node *expr, struct resolve2_ctx *ctx);
static int
resolve2_span_size_item(struct ast_node *item, struct resolve2_ctx *ctx);
static int
resolve2_span_size_block(struct ast_node *item, struct resolve2_ctx *ctx);
static int
resolve2_span_size_array(struct ast_node *array, struct resolve2_ctx *ctx);
static int
resolve2_span_size_byte(struct ast_node *byte, struct resolve2_ctx *ctx);
static int
resolve2_span_size_byte_array(struct ast_node *byte_array,
                              struct resolve2_ctx *ctx);
static int
resolve2_key_stmt(struct key_stmt *key_stmt, struct resolve2_ctx *ctx);
static int
resolve2_span_expr(struct ast_node *span_expr, struct resolve2_ctx *ctx);
static int
resolve2_named_expr(struct named_expr *named_expr, struct resolve2_ctx *ctx);
static int
resolve2_field(struct field *field, struct resolve2_ctx *ctx);
 
static int
setup_global_track_backends(void);
static int
setup_track_backends_dpath(struct dpath_node *dpath);
static int
setup_track_backends_expr(struct ast_node *expr);
static int
setup_track_backends_recur_block(struct ast_node *block);


static const char *
reverse_lookup_typename(const struct ast_node *node,
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
    TAILQ_FOREACH(stmt, stmt_list, list) {
        nstmt = (struct named_statement *)stmt;
        if (NULL == nstmt->name) {
            struct field *field;

            // recurse in anonymous struct/union fields
            assert(STATEMENT_TYPE_FIELD == stmt_type);
            field = (struct field *)nstmt;
            if (AST_NODE_TYPE_BLOCK_DEF == field->dpath.item->type) {
                nstmt = find_statement_by_name(
                    STATEMENT_TYPE_FIELD, identifier,
                    &field->dpath.item->u.block_def.block_stmt_list);
                if (NULL != nstmt) {
                    return nstmt;
                }
            }
        } else if (0 == strcmp(identifier, nstmt->name)) {
            return nstmt;
        }
    }
    return NULL;
}

static int
lookup_statement(enum statement_type stmt_type,
                 const char *identifier,
                 const struct list_of_visible_refs *visible_refs,
                 const struct ast_node **blockp,
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
             const struct ast_node **blockp,
             const struct field **fieldp)
{
    return lookup_statement(STATEMENT_TYPE_FIELD, identifier, visible_refs,
                            blockp,
                            (const struct named_statement **)fieldp);
}

static int
lookup_named_expr(const char *identifier,
                  const struct list_of_visible_refs *visible_refs,
                  const struct ast_node **blockp,
                  const struct named_expr **named_exprp)
{
    return lookup_statement(STATEMENT_TYPE_NAMED_EXPR,
                            identifier, visible_refs, blockp,
                            (const struct named_statement **)named_exprp);
}

int
resolve_schema_references(struct bitpunch_schema_hdl *schema)
{
    struct ast_node *ast_root;
    struct dpath_node dpath_root;

    ast_root = schema->df_file_block.root->item;
    if (-1 == resolve_names(&ast_root, NULL,
                            RESOLVE_NAMES_IN_BLOCKS,
                            RESOLVE_EXPECT_TYPE)) {
        return -1;
    }
    if (-1 == resolve_names(&ast_root, NULL,
                            RESOLVE_NAMES_IN_EXPRESSIONS,
                            RESOLVE_EXPECT_TYPE)) {
        return -1;
    }
    if (-1 == resolve2_all(ast_root)) {
        return -1;
    }
    if (-1 == resolve_names(&ast_root, NULL,
                            RESOLVE_CONSOLIDATE_NAMED_EXPRS,
                            RESOLVE_EXPECT_TYPE)) {
        return -1;
    }
    if (-1 == setup_global_track_backends()) {
        return -1;
    }
    dpath_node_reset(&dpath_root);
    dpath_root.item = ast_root;
    if (-1 == setup_track_backends_dpath(&dpath_root)) {
        return -1;
    }
    return 0;
}


static struct named_statement *
find_unchained_statement_by_name(const struct statement_list *in_list,
                                 const char *identifier)
{
    struct statement *stmt;
    struct named_statement *nstmt;

    TAILQ_FOREACH(stmt, in_list, list) {
        nstmt = (struct named_statement *)stmt;
        if (NULL == nstmt->name) {
            struct field *field;

            // recurse in anonymous struct/union fields
            field = (struct field *)nstmt;
            if (AST_NODE_TYPE_BLOCK_DEF == field->dpath.item->type) {
                nstmt = find_unchained_statement_by_name(
                    field->dpath.item->u.block_def.block_stmt_list.field_list,
                    identifier);
                if (NULL != nstmt) {
                    return nstmt;
                }
            }
        } else if (0 == strcmp(identifier, nstmt->name)
                   && NULL == nstmt->next_sibling) {
            return nstmt;
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
            if (AST_NODE_TYPE_BLOCK_DEF == field->dpath.item->type) {
                if (-1 == chain_duplicate_statements_in(
                        field->dpath.item->u.block_def.block_stmt_list.field_list,
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
resolve_names(struct ast_node **node_p,
              const struct list_of_visible_refs *visible_refs,
              enum resolve_step resolve_step,
              enum resolve_expect_mask expect_mask)
{
    struct ast_node *resolved_type;
    int ret;

    if (0 != ((*node_p)->flags & ASTFLAG_PROCESSING)) {
        return 0;
    }
    /* There might be types resolving to other non-native
     * types. That's why we must do a resolve loop until we find
     * the native type that the source type refers to. */
    while (TRUE) {
        (*node_p)->flags |= ASTFLAG_PROCESSING;
        ret = resolve_names_int(*node_p, visible_refs,
                                resolve_step, expect_mask,
                                &resolved_type);
        (*node_p)->flags &= ~ASTFLAG_PROCESSING;
        if (-1 == ret || NULL == resolved_type) {
            return ret;
        }
        assert(*node_p != resolved_type);
        (*node_p) = resolved_type;
    }
    /*NOT REACHED*/
}

static int
resolve_names_int(struct ast_node *node,
                  const struct list_of_visible_refs *visible_refs,
                  enum resolve_step resolve_step,
                  enum resolve_expect_mask expect_mask,
                  struct ast_node **resolved_typep)
{
    switch (node->type) {
    case AST_NODE_TYPE_IDENTIFIER:
        return resolve_names_identifier(node, visible_refs,
                                        resolve_step, expect_mask,
                                        resolved_typep);
    case AST_NODE_TYPE_INTEGER:
        return resolve_expr_integer(node, resolve_step, expect_mask,
                                    resolved_typep);
    case AST_NODE_TYPE_BOOLEAN:
        return resolve_expr_boolean(node, resolve_step, expect_mask,
                                    resolved_typep);
    case AST_NODE_TYPE_STRING:
        return resolve_expr_string_literal(node, resolve_step,
                                           expect_mask, resolved_typep);
    case AST_NODE_TYPE_BYTE:
        return resolve_names_byte(node, visible_refs, resolve_step,
                                  expect_mask, resolved_typep);
    case AST_NODE_TYPE_BLOCK_DEF:
        return resolve_names_block(node, visible_refs, resolve_step,
                                   expect_mask, resolved_typep);
    case AST_NODE_TYPE_ARRAY:
        return resolve_names_array(node, visible_refs, resolve_step,
                                   expect_mask, resolved_typep);
    case AST_NODE_TYPE_BYTE_ARRAY:
        return resolve_names_byte_array(node, visible_refs,
                                        resolve_step, expect_mask,
                                        resolved_typep);
    case AST_NODE_TYPE_CONDITIONAL:
        return resolve_names_conditional(node, visible_refs,
                                         resolve_step, expect_mask,
                                         resolved_typep);
        /* for all operators: resolve potential type names in operand
         * sub-expressions (e.g. sizeof) */
    case AST_NODE_TYPE_OP_UPLUS:
    case AST_NODE_TYPE_OP_UMINUS:
    case AST_NODE_TYPE_OP_LNOT:
    case AST_NODE_TYPE_OP_BWNOT:
    case AST_NODE_TYPE_OP_MEMBER:
        // for member operator, 2nd operand is resolved in resolve2 phase
        return resolve_names_operator(node, 1, visible_refs,
                                      resolve_step, expect_mask,
                                      resolved_typep);
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
        return resolve_names_operator(node, 2, visible_refs,
                                      resolve_step, expect_mask,
                                      resolved_typep);
    case AST_NODE_TYPE_OP_SET_FILTER:
        return resolve_names_operator_set_filter(
            node, visible_refs, resolve_step, expect_mask,
            resolved_typep);
    case AST_NODE_TYPE_OP_ADDROF:
        return resolve_expr_operator_addrof(node, visible_refs,
                                            resolve_step, expect_mask,
                                            resolved_typep);
    case AST_NODE_TYPE_OP_FILTER:
        return resolve_expr_operator_filter(node, visible_refs,
                                            resolve_step, expect_mask,
                                            resolved_typep);
    case AST_NODE_TYPE_OP_SUBSCRIPT:
        return resolve_names_op_subscript(node, visible_refs,
                                          resolve_step, expect_mask,
                                          resolved_typep);
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
        return resolve_names_op_subscript_slice(
            node, visible_refs, resolve_step, expect_mask,
            resolved_typep);
    case AST_NODE_TYPE_OP_FCALL:
        return resolve_names_op_fcall(node, visible_refs,
                                      resolve_step, expect_mask,
                                      resolved_typep);
    case AST_NODE_TYPE_OP_SIZEOF:
        return resolve_expr_operator_sizeof(node, visible_refs,
                                            resolve_step, expect_mask,
                                            resolved_typep);
    case AST_NODE_TYPE_EXPR_FILE:
        return resolve_expr_file(node, visible_refs, resolve_step,
                                 expect_mask, resolved_typep);
    case AST_NODE_TYPE_EXPR_SELF:
        return resolve_expr_self(node, visible_refs, resolve_step,
                                 expect_mask, resolved_typep);
    case AST_NODE_TYPE_EXPR_STAR_WILDCARD:
        return resolve_expr_star_wildcard(node, visible_refs,
                                          resolve_step,
                                          expect_mask, resolved_typep);
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        return resolve_rexpr_named_expr(node, visible_refs,
                                        resolve_step,
                                        expect_mask, resolved_typep);
    case AST_NODE_TYPE_REXPR_FIELD:
        return resolve_rexpr_field(node, visible_refs, resolve_step,
                                   expect_mask, resolved_typep);
    case AST_NODE_TYPE_REXPR_INTERPRETER:
        return resolve_rexpr_interpreter(node, visible_refs,
                                         resolve_step,
                                         expect_mask, resolved_typep);
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        return resolve_rexpr_as_type(node, visible_refs, resolve_step,
                                     expect_mask, resolved_typep);
        break ;
    case AST_NODE_TYPE_REXPR_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT:
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        return resolve_rexpr_operator(node, 1, visible_refs, resolve_step,
                                      expect_mask, resolved_typep);
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
        return resolve_rexpr_operator(node, 2, visible_refs, resolve_step,
                                      expect_mask, resolved_typep);
    case AST_NODE_TYPE_REXPR_OP_SIZEOF:
        return resolve_rexpr_operator_sizeof(node, visible_refs,
                                             resolve_step,
                                             expect_mask, resolved_typep);
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
        return resolve_rexpr_op_subscript(node, visible_refs, resolve_step,
                                          expect_mask, resolved_typep);
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        return resolve_rexpr_op_subscript_slice(node, visible_refs,
                                                resolve_step,
                                                expect_mask, resolved_typep);
    case AST_NODE_TYPE_REXPR_OP_FCALL:
        return resolve_rexpr_op_fcall(node, visible_refs, resolve_step,
                                      expect_mask, resolved_typep);
    default:
        /* nothing to resolve */
        *resolved_typep = NULL;
        return 0;
    }
    /*NOT REACHED*/
}

static int
resolve_names_identifier(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    switch (resolve_step) {
    case RESOLVE_NAMES_IN_BLOCKS:
        return resolve_names_in_blocks_identifier(
            node, visible_refs, expect_mask, resolved_typep);
    default:
        return 0;
    }
}

static int
resolve_names_in_blocks_identifier(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct ast_node *resolved_type;
    const struct ast_node *resolved_block;
    const struct named_expr *resolved_named_expr;
    const struct field *resolved_field;
    const struct interpreter *interpreter;
    const struct expr_builtin_fn *builtin;

    if (0 != (expect_mask & RESOLVE_EXPECT_TYPE)) {
        if (0 == strcmp(node->u.identifier, "byte")) {
            /* native 'byte' type */
            resolved_type = new_safe(struct ast_node);
            resolved_type->type = AST_NODE_TYPE_BYTE;
            resolved_type->loc = node->loc;
            resolved_type->u.item.min_span_size = 1;
            *resolved_typep = resolved_type;
            return 0;
        }
    }
    if (-1 != lookup_named_expr(node->u.identifier, visible_refs,
                                &resolved_block,
                                &resolved_named_expr)) {
        resolved_type = new_safe(struct ast_node);
        resolved_type->type = AST_NODE_TYPE_REXPR_NAMED_EXPR;
        resolved_type->loc = node->loc;
        resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_UNSET;
        resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
        assert(NULL != resolved_block);
        resolved_type->u.rexpr_member_common.anchor_block =
            (struct ast_node *)resolved_block;
        resolved_type->u.rexpr_named_expr.named_expr =
            resolved_named_expr;
        *resolved_typep = resolved_type;
        return 0;
    }
    if (0 != (expect_mask & RESOLVE_EXPECT_EXPRESSION)) {
        if (-1 != lookup_field(node->u.identifier, visible_refs,
                               &resolved_block, &resolved_field)) {
            resolved_type = new_safe(struct ast_node);
            resolved_type->type = AST_NODE_TYPE_REXPR_FIELD;
            resolved_type->loc = node->loc;
            resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_UNSET;
            resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
            assert(NULL != resolved_block);
            resolved_type->u.rexpr_member_common.anchor_block =
                (struct ast_node *)resolved_block;
            resolved_type->u.rexpr_field.field = resolved_field;
            *resolved_typep = resolved_type;
            return 0;
        }
        builtin = expr_lookup_builtin_fn(node->u.identifier, NULL);
        if (NULL != builtin) {
            resolved_type = new_safe(struct ast_node);
            resolved_type->type = AST_NODE_TYPE_REXPR_BUILTIN;
            resolved_type->loc = node->loc;
            resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
            resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_UNSET;
            resolved_type->u.rexpr_builtin.builtin = builtin;
            *resolved_typep = resolved_type;
            return 0;
        }
    }
    if (0 != (expect_mask & RESOLVE_EXPECT_INTERPRETER)) {
        interpreter = interpreter_lookup(node->u.identifier);
        if (NULL != interpreter) {
            struct statement_list empty_param_list;

            TAILQ_INIT(&empty_param_list);
            resolved_type = interpreter_rcall_build(interpreter,
                                                    &empty_param_list);
            if (NULL == resolved_type) {
                return -1;
            }
            resolved_type->loc = node->loc;
            *resolved_typep = resolved_type;
            return 0;
        }
    }
    semantic_error(
        SEMANTIC_LOGLEVEL_ERROR, &node->loc,
        "no type named '%s' exists in the scope",
        node->u.identifier);
    return -1;
}

static int
resolve_names_byte(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_names_block(struct ast_node *block,
                    const struct list_of_visible_refs *visible_refs,
                    enum resolve_step resolve_step,
                    enum resolve_expect_mask expect_mask,
                    struct ast_node **resolved_typep)
{
    if (-1 == resolve_block_stmt_lists_types(block, visible_refs,
                                             resolve_step)) {
        return -1;
    }
    switch (resolve_step) {
    case RESOLVE_NAMES_IN_BLOCKS:
        return resolve_names_in_blocks_block(block, visible_refs,
                                             expect_mask, resolved_typep);
    default:
        *resolved_typep = NULL;
        return 0;
    }
}

static int
resolve_names_in_blocks_block(
    struct ast_node *block,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    const char *filter_type;

    filter_type = block->u.block_def.filter_type;
    // XXX can it be NULL?
    if (NULL != filter_type) {
        if (0 == strcmp(filter_type, "struct")) {
            block->u.block_def.type = BLOCK_TYPE_STRUCT;
        } else if (0 == strcmp(filter_type, "union")) {
            block->u.block_def.type = BLOCK_TYPE_UNION;
        } else {
            block->u.block_def.type = BLOCK_TYPE_INTERPRETER;
        }
    }
    if (BLOCK_TYPE_INTERPRETER == block->u.block_def.type) {
        const struct interpreter *interpreter;
        struct ast_node *resolved_type;

        interpreter = interpreter_lookup(filter_type);
        if (NULL == interpreter) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &block->loc,
                "no interpreter named '%s' exists",
                filter_type);
            return -1;
        }
        resolved_type = interpreter_rcall_build(
            interpreter, block->u.block_def.block_stmt_list.field_list);
        if (NULL == resolved_type) {
            return -1;
        }
        resolved_type->loc = block->loc;
        *resolved_typep = resolved_type;
        return 0;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_stmt_list_types_generic(
    struct statement_list *stmt_list,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step)
{
    struct statement *stmt;

    TAILQ_FOREACH(stmt, stmt_list, list) {
        if (NULL != stmt->cond
            && -1 == resolve_expr_types(&stmt->cond, visible_refs,
                                        resolve_step)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve_block_stmt_lists_types(
    struct ast_node *block,
    const struct list_of_visible_refs *outer_refs,
    enum resolve_step resolve_step)
{
    struct block_stmt_list *stmt_lists;
    struct list_of_visible_refs visible_refs;
    struct statement *stmt;
    struct field *field;
    struct span_stmt *span_stmt;
    struct key_stmt *key_stmt;
    struct named_expr *named_expr;

    stmt_lists = &block->u.block_def.block_stmt_list;
    /* add current refs to the chain of visible refs */
    visible_refs.outer_refs = outer_refs;
    visible_refs.cur_block = block;
    visible_refs.cur_lists = stmt_lists;

    STATEMENT_FOREACH(named_expr, named_expr,
                      stmt_lists->named_expr_list, list) {
        if (NULL != named_expr->expr
            && -1 == resolve_names(&named_expr->expr, &visible_refs,
                                   resolve_step,
                                   RESOLVE_EXPECT_TYPE |
                                   RESOLVE_EXPECT_INTERPRETER |
                                   RESOLVE_EXPECT_EXPRESSION)) {
            return -1;
        }
    }
    if (-1 == resolve_stmt_list_types_generic(
            stmt_lists->named_expr_list, &visible_refs, resolve_step)) {
        return -1;
    }
    if (-1 == resolve_stmt_list_types_generic(
            stmt_lists->field_list, &visible_refs, resolve_step)) {
        return -1;
    }
    if (-1 == resolve_stmt_list_types_generic(
            stmt_lists->span_list, &visible_refs, resolve_step)) {
        return -1;
    }
    if (-1 == resolve_stmt_list_types_generic(
            stmt_lists->last_stmt_list, &visible_refs, resolve_step)) {
        return -1;
    }
    STATEMENT_FOREACH(field, field, stmt_lists->field_list, list) {
        if (-1 == resolve_names_field(field, &visible_refs, resolve_step)) {
            return -1;
        }
    }
    if (-1 == chain_duplicate_statements(stmt_lists)) {
        return -1;
    }
    TAILQ_FOREACH(stmt, stmt_lists->span_list, list) {
        span_stmt = (struct span_stmt *)stmt;
        if (-1 == resolve_expr_types(&span_stmt->span_expr, &visible_refs,
                                     resolve_step)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, stmt_lists->key_list, list) {
        key_stmt = (struct key_stmt *)stmt;
        if (-1 == resolve_expr_types(&key_stmt->key_expr, &visible_refs,
                                     resolve_step)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve_names_field(
    struct field *field,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step)
{
    if (-1 == resolve_names_dpath_node(&field->dpath, visible_refs,
                                       resolve_step)) {
        return -1;
    }
    return 0;
}

static int
resolve_names_dpath_node(
    struct dpath_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step)
{
    if (NULL != node->item
        && -1 == resolve_names(&node->item, visible_refs, resolve_step,
                               RESOLVE_EXPECT_TYPE)) {
        return -1;
    }
    if (NULL != node->filter
        && -1 == resolve_names(&node->filter, visible_refs, resolve_step,
                               RESOLVE_EXPECT_INTERPRETER)) {
        return -1;
    }
    return 0;
}

static int
resolve_names_array(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    /* resolve array type and count expression, if defined */
    if (-1 == resolve_names_dpath_node(
            &node->u.array.item_type, visible_refs, resolve_step)) {
        return -1;
    }
    if (NULL != node->u.array.item_count &&
        -1 == resolve_expr_types(&node->u.array.item_count, visible_refs,
                                 resolve_step)) {
        return -1;
    }
    switch (resolve_step) {
    case RESOLVE_NAMES_IN_EXPRESSIONS:
        return resolve_names_in_expressions_array(node, visible_refs,
                                                  expect_mask,
                                                  resolved_typep);
    default:
        *resolved_typep = NULL;
        return 0;
    }
}

static int
resolve_names_in_expressions_array(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    if (AST_NODE_TYPE_BYTE == node->u.array.item_type.item->type
        && NULL == node->u.array.item_type.filter) {
        struct ast_node *byte_array;

        byte_array = new_safe(struct ast_node);
        byte_array->type = AST_NODE_TYPE_BYTE_ARRAY;
        byte_array->loc = node->loc;
        byte_array->u.item.min_span_size = SPAN_SIZE_UNDEF;
        byte_array->u.byte_array.size = node->u.array.item_count;
        *resolved_typep = byte_array;
        return 0;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_names_byte_array(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    /* resolve array count expression, if defined */
    if (NULL != node->u.byte_array.size &&
        -1 == resolve_expr_types(&node->u.byte_array.size, visible_refs,
                                 resolve_step)) {
        return -1;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_names_conditional(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    if (0 == (expect_mask & RESOLVE_EXPECT_EXPRESSION)) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                       "expect an expression");
        return -1;
    }
    if (NULL != node->u.conditional.outer_cond
        && -1 == resolve_names(&node->u.conditional.outer_cond,
                               visible_refs, resolve_step,
                               expect_mask)) {
        return -1;
    }
    if (-1 == resolve_expr_types(&node->u.conditional.cond_expr,
                                 visible_refs, resolve_step)) {
        return -1;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_expr_subscript_index(
    struct subscript_index *index,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step)
{
    if (NULL != index->key
        && -1 == resolve_expr_types(&index->key, visible_refs,
                                    resolve_step)) {
        return -1;
    }
    if (NULL != index->twin
        && -1 == resolve_expr_types(&index->twin, visible_refs,
                                    resolve_step)) {
        return -1;
    }
    return 0;
}

static int
resolve_names_op_subscript(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    if (-1 == resolve_names(&node->u.op_subscript_common.anchor_expr,
                            visible_refs, resolve_step, expect_mask)) {
        return -1;
    }
    if (-1 == resolve_expr_subscript_index(&node->u.op_subscript.index,
                                           visible_refs, resolve_step)) {
        return -1;
    }
    switch (resolve_step) {
    case RESOLVE_NAMES_IN_EXPRESSIONS:
        return resolve_names_in_expressions_op_subscript(
            node, visible_refs, expect_mask, resolved_typep);
    default:
        *resolved_typep = NULL;
        return 0;
    }
}

static int
resolve_names_in_expressions_op_subscript(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct ast_node *anchor_expr;
    struct ast_node *anchor_target;
    struct ast_node *resolved_type;

    anchor_expr = node->u.op_subscript_common.anchor_expr;
    if (0 != (expect_mask & RESOLVE_EXPECT_TYPE)) {
        anchor_target = ast_node_get_target_type(anchor_expr);
        if (NULL != anchor_target) {
            assert(ast_node_is_item(anchor_target));
            resolved_type = new_safe(struct ast_node);
            resolved_type->type = AST_NODE_TYPE_ARRAY;
            resolved_type->loc = node->loc;
            resolved_type->u.item.min_span_size = SPAN_SIZE_UNDEF;
            dpath_node_reset(&resolved_type->u.array.item_type);
            resolved_type->u.array.item_type.item = anchor_expr;
            if (NULL != node->u.op_subscript.index.twin) {
                semantic_error(SEMANTIC_LOGLEVEL_ERROR,
                               &node->u.op_subscript.index.twin->loc,
                               "cannot have a twin index for array type "
                               "declaration");
                return -1;
            }
            resolved_type->u.array.item_count = node->u.op_subscript.index.key;
            *resolved_typep = resolved_type;
            return 0;
        } else if (0 == (expect_mask & RESOLVE_EXPECT_EXPRESSION)) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                           "expecting type, got '%s'",
                           ast_node_type_str(node->type));
            return -1;
        }
    }
    if (0 != (expect_mask & RESOLVE_EXPECT_EXPRESSION)) {
        if (NULL == node->u.op_subscript.index.key) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                           "array subscript index cannot be empty");
            return -1;
        }
        resolved_type = new_safe(struct ast_node);
        resolved_type->type = AST_NODE_TYPE_REXPR_OP_SUBSCRIPT;
        resolved_type->loc = node->loc;
        resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
        resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_ITEM;
        assert(ast_node_is_rexpr(anchor_expr));
        resolved_type->u.rexpr_op_subscript_common.anchor_expr = anchor_expr;
        resolved_type->u.rexpr_op_subscript.index =
            node->u.op_subscript.index;
        *resolved_typep = resolved_type;
        return 0;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_names_op_subscript_slice(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    if (-1 == resolve_names(&node->u.op_subscript_common.anchor_expr,
                            visible_refs, resolve_step,
                            RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    if (-1 == resolve_expr_subscript_index(
            &node->u.op_subscript_slice.start, visible_refs,
            resolve_step)) {
        return -1;
    }
    if (-1 == resolve_expr_subscript_index(
            &node->u.op_subscript_slice.end, visible_refs,
            resolve_step)) {
        return -1;
    }
    switch (resolve_step) {
    case RESOLVE_NAMES_IN_EXPRESSIONS:
        return resolve_names_in_expressions_op_subscript_slice(
            node, visible_refs, expect_mask, resolved_typep);
    default:
        *resolved_typep = NULL;
        return 0;
    }
}

static int
resolve_names_in_expressions_op_subscript_slice(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct ast_node *anchor_expr;
    struct ast_node *anchor_item;
    struct subscript_index start;
    struct subscript_index end;
    struct ast_node *resolved_type;

    anchor_expr = node->u.op_subscript_common.anchor_expr;
    anchor_item = anchor_expr->u.rexpr.target_item;
    start = node->u.op_subscript_slice.start;
    end = node->u.op_subscript_slice.end;

    resolved_type = new_safe(struct ast_node);
    resolved_type->type = AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE;
    resolved_type->loc = node->loc;
    resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    assert(ast_node_is_rexpr(anchor_expr));
    resolved_type->u.rexpr_op_subscript_common.anchor_expr = anchor_expr;
    resolved_type->u.rexpr_op_subscript_slice.start = start;
    resolved_type->u.rexpr_op_subscript_slice.end = end;
    // a slice still references the anchor array
    resolved_type->u.rexpr.target_item =
        ast_node_get_named_expr_target(anchor_item);

    *resolved_typep = resolved_type;
    return 0;
}

static int
resolve_names_op_fcall(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct statement *stmt;
    struct named_expr *param;

    if (-1 == resolve_names(&expr->u.op_fcall.func, visible_refs,
                            resolve_step,
                            RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    /* resolve expressions in parameter list */
    TAILQ_FOREACH(stmt, expr->u.op_fcall.func_params, list) {
        param = (struct named_expr *)stmt;
        if (-1 == resolve_expr_types(&param->expr, visible_refs,
                                     resolve_step)) {
            return -1;
        }
    }
    switch (resolve_step) {
    case RESOLVE_NAMES_IN_EXPRESSIONS:
        return resolve_names_in_expressions_op_fcall(
            expr, visible_refs, expect_mask, resolved_typep);
    default:
        *resolved_typep = NULL;
        return 0;
    }
}

static int
resolve_names_in_expressions_op_fcall(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    const struct expr_builtin_fn *builtin;
    struct statement *stmt;
    int n_params;
    struct statement_list *func_params;
    struct ast_node *resolved_type;

    builtin = expr->u.op_fcall.func->u.rexpr_builtin.builtin;

    n_params = 0;
    TAILQ_FOREACH(stmt, expr->u.op_fcall.func_params, list) {
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

    func_params = expr->u.op_fcall.func_params;
    resolved_type = new_safe(struct ast_node);
    resolved_type->type = AST_NODE_TYPE_REXPR_OP_FCALL;
    resolved_type->loc = expr->loc;
    resolved_type->u.rexpr.value_type = builtin->res_value_type;
    resolved_type->u.rexpr.dpath_type = builtin->res_dpath_type;
    resolved_type->u.rexpr_op_fcall.builtin = builtin;
    resolved_type->u.rexpr_op_fcall.func_params = func_params;
    resolved_type->u.rexpr_op_fcall.n_func_params = n_params;
    *resolved_typep = resolved_type;
    return 0;
}

static int
resolve_names_operator(
    struct ast_node *node,
    int n_operands,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    int opd_i;

    if (0 == (expect_mask & RESOLVE_EXPECT_EXPRESSION)) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &node->loc,
                       "unexpected expression");
        return -1;
    }
    for (opd_i = 0; opd_i < n_operands; ++opd_i) {
        if (-1 == resolve_names(&node->u.op.operands[opd_i],
                                visible_refs, resolve_step,
                                RESOLVE_EXPECT_EXPRESSION)) {
            return -1;
        }
    }
    switch (resolve_step) {
    case RESOLVE_NAMES_IN_EXPRESSIONS:
        return resolve_names_in_expressions_operator(
            node, n_operands, visible_refs, expect_mask, resolved_typep);
    default:
        *resolved_typep = NULL;
        return 0;
    }
}

static int
resolve_names_in_expressions_operator(
    struct ast_node *node,
    int n_operands,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct ast_node *resolved_type;

    resolved_type = new_safe(struct ast_node);
    resolved_type->type = op_type_ast2rexpr(node->type);
    resolved_type->loc = node->loc;
    resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    resolved_type->u.rexpr_op.op = node->u.op;
    *resolved_typep = resolved_type;
    return 0;
}

static int
resolve_names_operator_set_filter(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    *resolved_typep = NULL; // default
    if (-1 == resolve_names(
            &node->u.op.operands[0], visible_refs,
            resolve_step,
            expect_mask & (RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_INTERPRETER |
                           RESOLVE_EXPECT_DPATH_EXPRESSION))) {
        return -1;
    }
    if (-1 == resolve_names(&node->u.op.operands[1], visible_refs,
                            resolve_step,
                            RESOLVE_EXPECT_TYPE |
                            RESOLVE_EXPECT_INTERPRETER)) {
        return -1;
    }
    switch (resolve_step) {
    case RESOLVE_NAMES_IN_EXPRESSIONS:
        return resolve_names_in_expressions_operator_set_filter(
            node, visible_refs, expect_mask, resolved_typep);
    default:
        *resolved_typep = NULL;
        return 0;
    }
}

static int
resolve_names_in_expressions_operator_set_filter(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct ast_node *target;
    struct ast_node *filter;
    struct ast_node *filter_type;

    target = node->u.op.operands[0];
    filter = node->u.op.operands[1];
    filter_type = ast_node_get_named_expr_target(filter);

    if (ast_node_is_item(filter_type)) {
        struct ast_node *as_type;

        as_type = new_safe(struct ast_node);
        as_type->type = AST_NODE_TYPE_REXPR_AS_TYPE;
        as_type->loc = filter->loc;
        as_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
        as_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
        as_type->u.rexpr_filter.filter_dpath.item = filter;
        filter = as_type;
    }
    *resolved_typep = filter;

    if (AST_NODE_TYPE_REXPR_NAMED_EXPR == filter->type
        && filter != filter_type) {
        filter->u.rexpr_named_expr.filter_target = target;
    } else {
        filter->u.rexpr_filter.target = target;
    }
    return 0;
}

/*
 * resolve expressions
 */

static int
resolve_expr_types(struct ast_node **expr_p,
                   const struct list_of_visible_refs *visible_refs,
                   enum resolve_step resolve_step)
{
    return resolve_names(expr_p, visible_refs,
                         resolve_step, RESOLVE_EXPECT_EXPRESSION);
}

static int
resolve_expr_integer(struct ast_node *node,
                     enum resolve_step resolve_step,
                     enum resolve_expect_mask expect_mask,
                     struct ast_node **resolved_typep)
{
    int64_t integer;
    struct ast_node *resolved_type;

    if (resolve_step != RESOLVE_NAMES_IN_EXPRESSIONS) {
        *resolved_typep = NULL;
        return 0;
    }
    integer = node->u.integer;
    resolved_type = new_safe(struct ast_node);
    resolved_type->type = AST_NODE_TYPE_REXPR_NATIVE;
    resolved_type->loc = node->loc;
    resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_INTEGER;
    resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    resolved_type->u.rexpr_native.value.integer = integer;
    *resolved_typep = resolved_type;
    return 0;
}

static int
resolve_expr_boolean(struct ast_node *node,
                     enum resolve_step resolve_step,
                     enum resolve_expect_mask expect_mask,
                     struct ast_node **resolved_typep)
{
    int boolean;
    struct ast_node *resolved_type;

    if (resolve_step != RESOLVE_NAMES_IN_EXPRESSIONS) {
        *resolved_typep = NULL;
        return 0;
    }
    boolean = node->u.boolean;
    resolved_type = new_safe(struct ast_node);
    resolved_type->type = AST_NODE_TYPE_REXPR_NATIVE;
    resolved_type->loc = node->loc;
    resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_BOOLEAN;
    resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    resolved_type->u.rexpr_native.value.boolean = boolean;
    *resolved_typep = resolved_type;
    return 0;
}

static int
resolve_expr_string_literal(struct ast_node *node,
                            enum resolve_step resolve_step,
                            enum resolve_expect_mask expect_mask,
                            struct ast_node **resolved_typep)
{
    struct expr_value_string string;
    struct ast_node *resolved_type;

    if (resolve_step != RESOLVE_NAMES_IN_EXPRESSIONS) {
        *resolved_typep = NULL;
        return 0;
    }
    string = node->u.string;
    resolved_type = new_safe(struct ast_node);
    resolved_type->type = AST_NODE_TYPE_REXPR_NATIVE;
    resolved_type->loc = node->loc;
    resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_STRING;
    resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    resolved_type->u.rexpr_native.value.string = string;
    *resolved_typep = resolved_type;
    return 0;
}

static enum expr_value_type
expr_value_type_from_node(const struct ast_node *node)
{
    if (ast_node_is_rexpr(node)) {
        return node->u.rexpr.value_type;
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

/**
 * @brief first resolve pass of sizeof operator
 *
 * The first pass focuses on resolving data types and dpaths of sizeof
 * argument expression. Second pass will compute static span size or
 * set it to dynamic, as this requires all dpaths to be resolved
 * first.
 */
static int
resolve_expr_operator_sizeof(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct ast_node *resolved_type;

    if (-1 == resolve_names(&expr->u.op.operands[0],
                            visible_refs, resolve_step,
                            RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    if (resolve_step != RESOLVE_NAMES_IN_EXPRESSIONS) {
        *resolved_typep = NULL;
        return 0;
    }
    resolved_type = new_safe(struct ast_node);
    resolved_type->type = AST_NODE_TYPE_REXPR_OP_SIZEOF;
    resolved_type->loc = expr->loc;
    resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_INTEGER;
    resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    resolved_type->u.rexpr_op.op = expr->u.op;
    *resolved_typep = resolved_type;
    return 0;
}
/**
 * @brief first resolve pass of addrof (&) operator
 *
 * The first pass focuses on resolving data types and dpaths of addrof
 * argument expression. Second pass will compute static address or
 * set it to dynamic, as this requires all dpaths to be resolved
 * first.
 */
static int
resolve_expr_operator_addrof(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct ast_node *resolved_type;

    if (-1 == resolve_names(&expr->u.op.operands[0],
                            visible_refs, resolve_step,
                            RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    if (resolve_step != RESOLVE_NAMES_IN_EXPRESSIONS) {
        *resolved_typep = NULL;
        return 0;
    }
    resolved_type = new_safe(struct ast_node);
    resolved_type->type = AST_NODE_TYPE_REXPR_OP_ADDROF;
    resolved_type->loc = expr->loc;
    resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_INTEGER;
    resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    resolved_type->u.rexpr_op.op = expr->u.op;
    *resolved_typep = resolved_type;
    return 0;
}

/**
 * @brief first resolve pass of filter (unary *) operator
 *
 * The first pass focuses on resolving data types and dpaths of addrof
 * argument expression. Second pass will compute static address or
 * set it to dynamic, as this requires all dpaths to be resolved
 * first.
 */
static int
resolve_expr_operator_filter(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct op op;
    struct ast_node *target;
    struct ast_node *filter;
    const struct interpreter *interpreter;
    struct ast_node *resolved_type;

    if (-1 == resolve_names(&expr->u.op.operands[0], visible_refs,
                            resolve_step,
                            RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    if (resolve_step != RESOLVE_NAMES_IN_EXPRESSIONS) {
        *resolved_typep = NULL;
        return 0;
    }
    //FIXME the following should be in resolve2
    op = expr->u.op;
    target = op.operands[0]->u.rexpr.target_item;
    if (NULL == target
        || EXPR_DPATH_TYPE_NONE == op.operands[0]->u.rexpr.dpath_type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "invalid use of filter (*) operator on non-dpath node of type "
            "'%s'",
            ast_node_type_str(op.operands[0]->type));
        return -1;
    }
    assert(ast_node_is_item(target));
    filter = ast_node_get_target_filter(op.operands[0]);
    if (NULL == filter
        || AST_NODE_TYPE_REXPR_OP_FILTER == op.operands[0]->type
        || AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE == op.operands[0]->type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "filter (*) operand has no attached interpreter");
        return -1;
    }
    interpreter = filter->u.rexpr_interpreter.interpreter;

    resolved_type = new_safe(struct ast_node);
    resolved_type->type = AST_NODE_TYPE_REXPR_OP_FILTER;
    resolved_type->loc = expr->loc;
    resolved_type->u.rexpr.value_type = interpreter->semantic_type;
    resolved_type->u.rexpr.dpath_type =
        (EXPR_VALUE_TYPE_BYTES == interpreter->semantic_type ?
         EXPR_DPATH_TYPE_CONTAINER : EXPR_DPATH_TYPE_NONE);
    resolved_type->u.rexpr.target_item = target;
    resolved_type->u.rexpr_op.op = op;
    *resolved_typep = resolved_type;
    return 0;
}

static int
resolve_expr_file(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    const struct list_of_visible_refs *toplevel_refs;
    struct ast_node *file_block;
    struct ast_node *resolved_type;

    if (resolve_step != RESOLVE_NAMES_IN_EXPRESSIONS) {
        *resolved_typep = NULL;
        return 0;
    }
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
    file_block = (struct ast_node *)toplevel_refs->cur_block;

    resolved_type = new_safe(struct ast_node);
    resolved_type->type = AST_NODE_TYPE_REXPR_FILE;
    resolved_type->loc = expr->loc;
    resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    resolved_type->u.rexpr.target_item = file_block;
    *resolved_typep = resolved_type;
    return 0;
}

static int
resolve_expr_self(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct ast_node *self_block;
    struct ast_node *resolved_type;

    if (resolve_step != RESOLVE_NAMES_IN_EXPRESSIONS) {
        *resolved_typep = NULL;
        return 0;
    }
    if (NULL == visible_refs) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                       "need a binary file loaded to use 'self' in "
                       "expression");
        return -1;
    }
    // const-cast
    self_block = (struct ast_node *)visible_refs->cur_block;

    resolved_type = new_safe(struct ast_node);
    resolved_type->type = AST_NODE_TYPE_REXPR_SELF;
    resolved_type->loc = expr->loc;
    resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    resolved_type->u.rexpr.target_item = self_block;
    *resolved_typep = resolved_type;
    return 0;
}

static int
resolve_expr_star_wildcard(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct ast_node *resolved_type;

    if (resolve_step != RESOLVE_NAMES_IN_EXPRESSIONS) {
        *resolved_typep = NULL;
        return 0;
    }
    resolved_type = new_safe(struct ast_node);
    resolved_type->type = AST_NODE_TYPE_REXPR_STAR_WILDCARD;
    resolved_type->loc = expr->loc;
    resolved_type->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    resolved_type->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    *resolved_typep = resolved_type;
    return 0;
}

static int
resolve_rexpr_named_expr(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    switch (resolve_step) {
    case RESOLVE_NAMES_IN_EXPRESSIONS:
        return resolve_names_in_expressions_named_expr(
            expr, visible_refs, expect_mask, resolved_typep);
    case RESOLVE_CONSOLIDATE_NAMED_EXPRS:
        return resolve_rexpr_consolidate_named_expr(
            expr, visible_refs, expect_mask, resolved_typep);
    default:
        *resolved_typep = NULL;
        return 0;
    }
}

static int
resolve_names_in_expressions_named_expr(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct named_expr *named_expr;

    named_expr = (struct named_expr *)expr->u.rexpr_named_expr.named_expr;

    *resolved_typep = NULL;
    return resolve_names(&named_expr->expr, NULL,
                         RESOLVE_NAMES_IN_EXPRESSIONS, expect_mask);
}

static int
resolve_rexpr_consolidate_named_expr(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct ast_node *target;
    struct ast_node *filter_target;

    // replace named expressions by their target item where types are
    // expected
    target = expr;
    filter_target = NULL;
    assert(NULL != target);
    while (AST_NODE_TYPE_REXPR_NAMED_EXPR == target->type) {
        filter_target = target->u.rexpr_named_expr.filter_target;
        target = target->u.rexpr_named_expr.named_expr->expr;
        assert(NULL != target);
        if (NULL != filter_target) {
            break ;
        }
    }
    *resolved_typep = NULL; // default
    if (NULL != filter_target) {
        assert(AST_NODE_TYPE_REXPR_INTERPRETER == target->type);
        if (0 != (target->flags & ASTFLAG_TEMPLATE)) {
            if (-1 == interpreter_build_instance(&target, filter_target)) {
                return -1;
            }
            *resolved_typep = target;
        }
    }
    if (NULL == *resolved_typep
        && 0 == (expect_mask & RESOLVE_EXPECT_EXPRESSION)) {
        *resolved_typep = target;
    }
    return 0;
}

static int
resolve_rexpr_field(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_rexpr_interpreter(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    const struct interpreter *interpreter;
    struct ast_node *param_valuep;
    struct ast_node *new_param_value;
    struct interpreter_param_def *param_def;

    if (NULL != expr->u.rexpr_filter.target
        && -1 == resolve_names(&expr->u.rexpr_filter.target,
                               visible_refs, resolve_step,
                               RESOLVE_EXPECT_TYPE |
                               RESOLVE_EXPECT_INTERPRETER |
                               RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    interpreter = expr->u.rexpr_interpreter.interpreter;
    STAILQ_FOREACH(param_def, &interpreter->param_list, list) {
        param_valuep = INTERPRETER_RCALL_PARAM(expr, param_def->ref_idx);
        new_param_value = param_valuep;
        if (-1 == resolve_expr_types(&new_param_value, visible_refs,
                                     resolve_step)) {
            return -1;
        }
        if (new_param_value != param_valuep) {
            memcpy(param_valuep, new_param_value, sizeof(struct ast_node));
        }
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_rexpr_as_type(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    if (NULL != expr->u.rexpr_filter.target
        && -1 == resolve_names(
            &expr->u.rexpr_filter.target,
            visible_refs, resolve_step,
            expect_mask & (RESOLVE_EXPECT_TYPE |
                           RESOLVE_EXPECT_DPATH_EXPRESSION))) {
        return -1;
    }
    if (-1 == resolve_names(&expr->u.rexpr_filter.filter_dpath.item,
                            visible_refs, resolve_step,
                            RESOLVE_EXPECT_TYPE)) {
        return -1;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_rexpr_operator(
    struct ast_node *expr,
    int n_operands,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    int opd_i;

    for (opd_i = 0; opd_i < n_operands; ++opd_i) {
        if (-1 == resolve_names(&expr->u.rexpr_op.op.operands[opd_i],
                                visible_refs, resolve_step,
                                RESOLVE_EXPECT_EXPRESSION)) {
            return -1;
        }
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_rexpr_operator_sizeof(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    if (-1 == resolve_names(&expr->u.rexpr_op.op.operands[0],
                            visible_refs, resolve_step,
                            RESOLVE_EXPECT_DPATH_EXPRESSION)) {
        return -1;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_rexpr_op_subscript(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    if (-1 == resolve_names(&expr->u.rexpr_op_subscript_common.anchor_expr,
                            visible_refs, resolve_step, expect_mask)) {
        return -1;
    }
    if (-1 == resolve_expr_subscript_index(&expr->u.rexpr_op_subscript.index,
                                           visible_refs, resolve_step)) {
        return -1;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_rexpr_op_subscript_slice(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    if (-1 == resolve_names(&expr->u.rexpr_op_subscript_common.anchor_expr,
                            visible_refs, resolve_step,
                            RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    if (-1 == resolve_expr_subscript_index(
            &expr->u.rexpr_op_subscript_slice.start, visible_refs,
            resolve_step)) {
        return -1;
    }
    if (-1 == resolve_expr_subscript_index(
            &expr->u.rexpr_op_subscript_slice.end, visible_refs,
            resolve_step)) {
        return -1;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_rexpr_op_fcall(
    struct ast_node *expr,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_step resolve_step,
    enum resolve_expect_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct statement *stmt;
    struct named_expr *param;

    /* resolve expressions in parameter list */
    TAILQ_FOREACH(stmt, expr->u.rexpr_op_fcall.func_params, list) {
        param = (struct named_expr *)stmt;
        if (-1 == resolve_expr_types(&param->expr, visible_refs,
                                     resolve_step)) {
            return -1;
        }
    }
    *resolved_typep = NULL;
    return 0;
}



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
    case AST_NODE_TYPE_OP_FILTER:
        return AST_NODE_TYPE_REXPR_OP_FILTER;
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
resolve_user_expr_scoped_recur(struct ast_node **expr_p,
                               struct box *cur_scope,
                               struct list_of_visible_refs *inner_refs,
                               struct list_of_visible_refs *inmost_refs)
{
    struct list_of_visible_refs visible_refs;

    while (NULL != cur_scope
           && AST_NODE_TYPE_BLOCK_DEF != cur_scope->dpath.item->type) {
        cur_scope = (NULL != cur_scope->parent_box ?
                     cur_scope->parent_box :
                     cur_scope->unfiltered_box);
    }
    if (NULL == cur_scope) {
        if (-1 == resolve_expr_types(expr_p, inmost_refs,
                                     RESOLVE_NAMES_IN_BLOCKS)) {
            return -1;
        }
        if (-1 == resolve_expr_types(expr_p, inmost_refs,
                                     RESOLVE_NAMES_IN_EXPRESSIONS)) {
            return -1;
        }
        if (-1 == resolve2_all(*expr_p)) {
            return -1;
        }
        return 0;
    }
    visible_refs.outer_refs = NULL;
    visible_refs.cur_block = cur_scope->dpath.item;
    visible_refs.cur_lists =
        &cur_scope->dpath.item->u.block_def.block_stmt_list;
    if (NULL != inner_refs) {
        inner_refs->outer_refs = &visible_refs;
    }
    return resolve_user_expr_scoped_recur(expr_p, cur_scope->parent_box,
                                          &visible_refs,
                                          (NULL != inmost_refs ?
                                           inmost_refs : &visible_refs));
}

int
resolve_user_expr(struct ast_node **expr_p, struct box *scope)
{
    if (NULL != scope) {
        return resolve_user_expr_scoped_recur(expr_p, scope, NULL, NULL);
    }
    if (-1 == resolve_expr_types(expr_p, NULL,
                                 RESOLVE_NAMES_IN_BLOCKS)) {
        return -1;
    }
    if (-1 == resolve_expr_types(expr_p, NULL,
                                 RESOLVE_NAMES_IN_EXPRESSIONS)) {
        return -1;
    }
    if (-1 == resolve2_all(*expr_p)) {
        return -1;
    }
    return 0;
}

/*
 * resolve2: second pass
 */

static void
schedule_resolve_expression_types(struct ast_node *node,
                                  struct resolve2_ctx *ctx,
                                  resolve2_node_cbfunc_t cb, void *cb_arg)
{
    struct scheduled_entry *entry;

    entry = new_safe(struct scheduled_entry);
    entry->node = node;
    entry->resolve_step = RESOLVE_EXPRESSION_TYPES;
    entry->cb = cb;
    entry->cb_arg = cb_arg;
    STAILQ_INSERT_TAIL(&ctx->scheduled, entry, list);
}

static void
schedule_resolve_dpath_node(struct dpath_node *dpath,
                            struct resolve2_ctx *ctx,
                            resolve2_dpath_cbfunc_t cb, void *cb_arg)
{
    struct scheduled_entry *entry;

    entry = new_safe(struct scheduled_entry);
    entry->dpath = dpath;
    entry->resolve_step = RESOLVE_EXPRESSION_TYPES;
    entry->cb = cb;
    entry->cb_arg = cb_arg;
    STAILQ_INSERT_TAIL(&ctx->scheduled, entry, list);
}

static void
schedule_resolve_span_size_dpath(struct dpath_node *dpath,
                                 struct resolve2_ctx *ctx,
                                 resolve2_dpath_cbfunc_t cb, void *cb_arg)
{
    struct scheduled_entry *entry;

    entry = new_safe(struct scheduled_entry);
    entry->dpath = dpath;
    entry->resolve_step = RESOLVE_SPAN_SIZES;
    entry->cb = cb;
    entry->cb_arg = cb_arg;
    STAILQ_INSERT_TAIL(&ctx->scheduled, entry, list);
}

static void
schedule_resolve_span_size_expr(struct ast_node *expr,
                                struct resolve2_ctx *ctx,
                                resolve2_node_cbfunc_t cb, void *cb_arg)
{
    struct scheduled_entry *entry;

    entry = new_safe(struct scheduled_entry);
    entry->node = expr;
    entry->resolve_step = RESOLVE_SPAN_SIZES;
    entry->cb = cb;
    entry->cb_arg = cb_arg;
    STAILQ_INSERT_TAIL(&ctx->scheduled, entry, list);
}

static int
resolve2_scheduled_entries(struct resolve2_ctx *ctx)
{
    struct scheduled_entry *entry;
    struct scheduled_entry *tentry;
    int ret;
    int again;

    again = FALSE;
    while (!STAILQ_EMPTY(&ctx->scheduled)) {
        STAILQ_FOREACH_SAFE(entry, &ctx->scheduled, list, tentry) {
            switch (entry->resolve_step) {
            case RESOLVE_EXPRESSION_TYPES:
                if (NULL != entry->node) {
                    ret = resolve2_ast_node(entry->node, ctx);
                } else {
                    ret = resolve2_dpath_node(entry->dpath, ctx);
                }
                STAILQ_REMOVE(&ctx->scheduled, entry, scheduled_entry, list);
                if (-1 == ret) {
                    return -1;
                }
                if (NULL != entry->cb) {
                    if (NULL != entry->node) {
                        resolve2_node_cbfunc_t cb = entry->cb;
                        if (-1 == cb(entry->node, ctx, entry->cb_arg)) {
                            return -1;
                        }
                    } else {
                        resolve2_dpath_cbfunc_t cb = entry->cb;
                        if (-1 == cb(entry->dpath, ctx, entry->cb_arg)) {
                            return -1;
                        }
                    }
                }
                free(entry);
                again = TRUE;
                break ;
            default:
                break ;
            }
        }
        if (!again) {
            break ;
        }
        again = FALSE;
    }

    again = FALSE;
    while (!STAILQ_EMPTY(&ctx->scheduled)) {
        STAILQ_FOREACH_SAFE(entry, &ctx->scheduled, list, tentry) {
            switch (entry->resolve_step) {
            case RESOLVE_SPAN_SIZES:
                if (NULL != entry->node) {
                    ret = resolve2_span_size_expr(entry->node, ctx);
                } else {
                    ret = resolve2_span_size_dpath(entry->dpath, ctx);
                }
                STAILQ_REMOVE(&ctx->scheduled, entry, scheduled_entry, list);
                if (-1 == ret) {
                    return -1;
                }
                if (NULL != entry->cb) {
                    if (NULL != entry->node) {
                        resolve2_node_cbfunc_t cb = entry->cb;
                        if (-1 == cb(entry->node, ctx, entry->cb_arg)) {
                            return -1;
                        }
                    } else {
                        resolve2_dpath_cbfunc_t cb = entry->cb;
                        if (-1 == cb(entry->dpath, ctx, entry->cb_arg)) {
                            return -1;
                        }
                    }
                }
                free(entry);
                again = TRUE;
                break ;
            default:
                break ;
            }
        }
        if (!again) {
            break ;
        }
        again = FALSE;
    }
    return 0;
}

static int
resolve2_all(struct ast_node *ast_root)
{
    struct resolve2_ctx ctx;
    struct dpath_node dpath;
    int ret;

    STAILQ_INIT(&ctx.scheduled);

    ret = resolve2_ast_node(ast_root, &ctx);
    if (-1 == ret) {
        return -1;
    }
    if (-1 == resolve2_scheduled_entries(&ctx)) {
        return -1;
    }
    if (!ast_node_is_item(ast_root)) {
        return 0;
    }
    dpath_node_reset(&dpath);
    dpath.item = ast_root;
    ret = resolve2_span_size_dpath(&dpath, &ctx);
    if (-1 == ret) {
        return -1;
    }
    if (-1 == resolve2_scheduled_entries(&ctx)) {
        return -1;
    }
    return 0;
}

static int
resolve2_ast_node(struct ast_node *expr, struct resolve2_ctx *ctx)
{
    int ret;

    if (0 != (expr->flags & (ASTFLAG_PROCESSING | ASTFLAG_RESOLVED))) {
        return 0;
    }
    if (ast_node_is_item(expr)) {
        ret = resolve2_dtype(expr, ctx);
    } else {
        expr->flags |= ASTFLAG_PROCESSING;

        switch (expr->type) {
        case AST_NODE_TYPE_REXPR_INTERPRETER:
            ret = resolve2_ast_node_interpreter(expr, ctx);
            break ;
        case AST_NODE_TYPE_REXPR_AS_TYPE:
            ret = resolve2_ast_node_as_type(expr, ctx);
            break ;
        case AST_NODE_TYPE_REXPR_OP_MEMBER:
            ret = resolve2_ast_node_member(expr, ctx);
            break ;
        case AST_NODE_TYPE_REXPR_FIELD:
            ret = resolve2_ast_node_field(expr, ctx);
            break ;
        case AST_NODE_TYPE_REXPR_NAMED_EXPR:
            ret = resolve2_ast_node_named_expr(expr, ctx);
            break ;
        case AST_NODE_TYPE_REXPR_OP_UPLUS:
        case AST_NODE_TYPE_REXPR_OP_UMINUS:
        case AST_NODE_TYPE_REXPR_OP_LNOT:
        case AST_NODE_TYPE_REXPR_OP_BWNOT:
        case AST_NODE_TYPE_REXPR_OP_FILTER:
            ret = resolve2_ast_node_operator(expr, 1, ctx);
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
            ret = resolve2_ast_node_operator(expr, 2, ctx);
            break ;
        case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
            ret = resolve2_ast_node_subscript(expr, ctx);
            break ;
        case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
            ret = resolve2_ast_node_subscript_slice(expr, ctx);
            break ;
        case AST_NODE_TYPE_REXPR_OP_SIZEOF:
            ret = resolve2_ast_node_operator_sizeof(expr, ctx);
            break ;
        case AST_NODE_TYPE_REXPR_OP_ADDROF:
            ret = resolve2_ast_node_operator_addrof(expr, ctx);
            break ;
        case AST_NODE_TYPE_REXPR_OP_FCALL:
            ret = resolve2_ast_node_fcall(expr, ctx);
            break ;
        default:
            /* nothing to do */
            ret = 0;
            break ;
        }
        expr->flags &= ~ASTFLAG_PROCESSING;
    }
    expr->flags |= ASTFLAG_RESOLVED;
    return ret;
}

static int
resolve2_dtype(struct ast_node *dtype, struct resolve2_ctx *ctx)
{
    int ret;

    switch (dtype->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        ret = resolve2_ast_node_block(dtype, ctx);
        break ;
    case AST_NODE_TYPE_ARRAY:
        ret = resolve2_ast_node_array(dtype, ctx);
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
        ret = resolve2_ast_node_byte_array(dtype, ctx);
        break ;
    default:
        /* nothing to do */
        ret = 0;
        break ;
    }
    if (-1 == ret) {
        return -1;
    }
    return 0;
}

static int
resolve2_ast_node_block(struct ast_node *block, struct resolve2_ctx *ctx)
{
    if (-1 == resolve2_stmt_lists(&block->u.block_def.block_stmt_list,
                                  ctx)) {
        return -1;
    }
    return 0;
}

static int
resolve2_stmt_list_generic(struct statement_list *stmt_list,
                           struct resolve2_ctx *ctx)
{
    struct statement *stmt;

    TAILQ_FOREACH(stmt, stmt_list, list) {
        if (NULL != stmt->cond
            && -1 == resolve2_conditional(stmt->cond, ctx)) {
            return -1;
        }
    }
    return 0;
}

static int
named_expr_check_duplicates(struct named_expr *named_expr)
{
    const struct ast_node *dst_item;
    struct named_expr *next_named_expr;
    const struct ast_node *next_dst_item;

    dst_item = named_expr->expr->u.rexpr.target_item;
    for (next_named_expr =
             (struct named_expr *)named_expr->nstmt.next_sibling;
         NULL != next_named_expr;
         next_named_expr =
             (struct named_expr *)next_named_expr->nstmt.next_sibling) {
        next_dst_item =
            next_named_expr->expr->u.rexpr.target_item;
        if (dst_item != next_dst_item) {
            if (dst_item->type == next_dst_item->type) {
                if (AST_NODE_TYPE_BYTE_ARRAY == dst_item->type) {
                    continue ;
                }
            }
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &next_named_expr->nstmt.stmt.loc,
                "different target types across duplicate named expressions");
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &named_expr->nstmt.stmt.loc,
                "first declaration here");
            return -1;
        }
    }
    return 0;
}

static int
named_exprs_check_duplicates(struct statement_list *named_expr_list)
{
    struct named_expr *named_expr;

    STATEMENT_FOREACH(named_expr, named_expr, named_expr_list, list) {
        if (-1 == named_expr_check_duplicates(named_expr)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve2_stmt_lists(struct block_stmt_list *stmt_lists,
                    struct resolve2_ctx *ctx)
{
    struct field *field;
    struct span_stmt *span_stmt;
    struct key_stmt *key_stmt;
    struct match *match;
    struct named_expr *named_expr;

    if (-1 == resolve2_stmt_list_generic(stmt_lists->named_expr_list, ctx)) {
        return -1;
    }
    if (-1 == named_exprs_check_duplicates(stmt_lists->named_expr_list)) {
        return -1;
    }
    STATEMENT_FOREACH(named_expr, named_expr,
                      stmt_lists->named_expr_list, list) {
        if (-1 == resolve2_named_expr(named_expr, ctx)) {
            return -1;
        }
    }
    if (-1 == resolve2_stmt_list_generic(stmt_lists->field_list, ctx)) {
        return -1;
    }
    if (-1 == resolve2_stmt_list_generic(stmt_lists->span_list, ctx)) {
        return -1;
    }
    if (-1 == resolve2_stmt_list_generic(stmt_lists->last_stmt_list, ctx)) {
        return -1;
    }
    STATEMENT_FOREACH(field, field, stmt_lists->field_list, list) {
        if (-1 == resolve2_field(field, ctx)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(span_stmt, span_stmt, stmt_lists->span_list, list) {
        if (-1 == resolve2_span_expr(span_stmt->span_expr, ctx)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(key_stmt, key_stmt, stmt_lists->key_list, list) {
        if (-1 == resolve2_key_stmt(key_stmt, ctx)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(match, match, stmt_lists->match_list, list) {
        if (-1 == resolve2_ast_node(match->expr, ctx)) {
            return -1;
        }
        if (match->expr->type == AST_NODE_TYPE_REXPR_NATIVE) {
            if (match->expr->u.rexpr_native.value.boolean) {
                semantic_error(SEMANTIC_LOGLEVEL_WARNING, &match->expr->loc,
                               "match expression always true");
            } else {
                semantic_error(SEMANTIC_LOGLEVEL_ERROR, &match->expr->loc,
                               "match expression always false");
                return -1;
            }
        }
    }
    return 0;
}

static int
resolve2_conditional(struct ast_node *cond, struct resolve2_ctx *ctx)
{
    struct ast_node *cond_expr;

    if (-1 == resolve2_ast_node(cond->u.conditional.cond_expr, ctx)) {
        return -1;
    }
    cond_expr = cond->u.conditional.cond_expr;
    assert(ast_node_is_rexpr(cond_expr));
    if (EXPR_VALUE_TYPE_BOOLEAN != cond_expr->u.rexpr.value_type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &cond->loc,
            "expect a boolean expression in condition, not '%s'",
            expr_value_type_str(cond_expr->u.rexpr.value_type));
        return -1;
    }
    if (NULL != cond->u.conditional.outer_cond
        && -1 == resolve2_conditional(cond->u.conditional.outer_cond, ctx)) {
        return -1;
    }
    return 0;
}

static int
resolve2_dpath_node_set_filter_defining_size(struct dpath_node *dpath,
                                             struct resolve2_ctx *ctx)
{
    struct ast_node *filter;
    struct ast_node *filter_type;

    filter = dpath->filter;
    while (NULL != filter) {
        filter_type = ast_node_get_named_expr_target(filter);
        if (!ast_node_is_filter(filter_type)) {
            break ;
        }
        if (AST_NODE_TYPE_REXPR_INTERPRETER == filter_type->type) {
            if (NULL != filter_type->u.rexpr_interpreter.get_size_func) {
                // overwrite potential existing value so that value
                // set is the interpreter closest to the item
                dpath->filter_defining_size = filter_type;
                dpath->u.item.flags &= ~ITEMFLAG_HAS_UNDETERMINED_SIZE;
            }
        }
        filter = filter_type->u.rexpr_filter.target;
    }
    return 0;
}

static int
resolve2_dpath_node(struct dpath_node *node, struct resolve2_ctx *ctx)
{
    struct ast_node *expr;
    struct ast_node *expr_target;
    struct ast_node *target_item;

    if (NULL != node->filter) {
        if (-1 == resolve2_ast_node(node->filter, ctx)) {
            return -1;
        }
    } else {
        if (-1 == resolve2_ast_node(node->item, ctx)) {
            return -1;
        }
    }
    expr = node->item;
    if (NULL != expr) {
        expr_target = ast_node_get_named_expr_target(expr);
        target_item = ast_node_get_target_item(expr);
        node->item = target_item;
        if (ast_node_is_filter(expr_target)) {
            node->filter = expr;
            if (-1 == resolve2_dpath_node_set_filter_defining_size(
                    node, ctx)) {
                return -1;
            }
        }
    }
    return 0;
}

static int
resolve2_ast_node_array(struct ast_node *node, struct resolve2_ctx *ctx)
{
    if (-1 == resolve2_dpath_node(&node->u.array.item_type, ctx)) {
        return -1;
    }
    if (NULL != node->u.array.item_count &&
        -1 == resolve2_ast_node(node->u.array.item_count, ctx)) {
        return -1;
    }
    return 0;
}

static int
resolve2_ast_node_byte_array(struct ast_node *node,
                             struct resolve2_ctx *ctx)
{
    if (NULL != node->u.byte_array.size &&
        -1 == resolve2_ast_node(node->u.byte_array.size, ctx)) {
        return -1;
    }
    return 0;
}


static int
resolve2_ast_node_interpreter(struct ast_node *expr,
                              struct resolve2_ctx *ctx)
{
    const struct interpreter *interpreter;
    struct ast_node *param_valuep;
    struct interpreter_param_def *param_def;
    int sem_error = FALSE;
    struct ast_node *target;

    interpreter = expr->u.rexpr_interpreter.interpreter;
    STAILQ_FOREACH(param_def, &interpreter->param_list, list) {
        param_valuep = INTERPRETER_RCALL_PARAM(expr, param_def->ref_idx);
        if (-1 == resolve2_ast_node(param_valuep, ctx)) {
            return -1;
        }
        if (param_valuep->u.rexpr.value_type != EXPR_VALUE_TYPE_UNSET
            && param_valuep->u.rexpr.value_type != param_def->type) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                           "wrong data type for parameter \"%s\": expected \"%s\", not \"%s\"",
                           param_def->name,
                           expr_value_type_str(param_def->type),
                           expr_value_type_str(
                               param_valuep->u.rexpr.value_type));
            sem_error = TRUE;
            continue ;
        }
    }
    if (sem_error) {
        return -1;
    }
    target = expr->u.rexpr_filter.target;
    if (NULL == target) {
        expr->flags |= ASTFLAG_TEMPLATE;
        return 0;
    }

    if (-1 == resolve2_ast_node(target, ctx)) {
        return -1;
    }
    return interpreter_build_instance(&expr, target);
}

static int
resolve2_ast_node_as_type(struct ast_node *expr,
                          struct resolve2_ctx *ctx)
{
    struct ast_node *target_item;
    struct dpath_node target_dpath;
    struct dpath_node *as_dpath;
    struct ast_node *as_item;

    if (-1 == resolve2_ast_node(expr->u.rexpr_filter.filter_dpath.item,
                                ctx)) {
        return -1;
    }
    if (-1 == resolve2_ast_node(expr->u.rexpr_filter.target, ctx)) {
        return -1;
    }
    target_item = ast_node_get_target_item(expr->u.rexpr_filter.target);
    dpath_node_reset(&target_dpath);
    target_dpath.filter = expr;
    target_dpath.item = target_item;
    as_dpath = &expr->u.rexpr_filter.filter_dpath;
    as_item = ast_node_get_named_expr_target(as_dpath->item);
    assert(ast_node_is_item(as_item));
    if (-1 == resolve2_dtype(as_item, ctx)) {
        return -1;
    }
    if (NULL != target_dpath.item) {
        assert(ast_node_is_item(target_dpath.item));
        if (-1 == resolve2_dtype(target_dpath.item, ctx)) {
            return -1;
        }
    }
    expr->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    expr->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    expr->u.rexpr.target_item = target_item;
    return 0;
}

static int
resolve2_ast_node_member(struct ast_node *expr, struct resolve2_ctx *ctx)
{
    int ret;
    struct op *op;
    struct ast_node *anchor_expr;
    const struct ast_node *anchor_block, *member;
    struct named_statement *resolved_member;

    op = &expr->u.rexpr_op.op;
    anchor_expr = ast_node_get_named_expr_target(op->operands[0]);
    member = op->operands[1];
    ret = resolve2_ast_node(anchor_expr, ctx);
    if (-1 == ret) {
        return -1;
    }
    anchor_block = ast_node_get_as_type(anchor_expr);
    if (NULL == anchor_block) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "member operator with computed dpath not supported");
        return -1;
    }
    anchor_block = ast_node_get_named_expr_target(
        (struct ast_node *)anchor_block);
    assert(ast_node_is_item(anchor_block));
    if (anchor_block->type != AST_NODE_TYPE_BLOCK_DEF) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "invalid use of member operator on non-block dpath");
        return -1;
    }
    /* checked by parser */
    assert(member->type == AST_NODE_TYPE_IDENTIFIER);
    resolved_member = find_statement_by_name(
        STATEMENT_TYPE_NAMED_EXPR, member->u.identifier,
        &anchor_block->u.block_def.block_stmt_list);
    if (NULL != resolved_member) {
        expr->type = AST_NODE_TYPE_REXPR_NAMED_EXPR;
        assert(NULL != anchor_expr);
        expr->u.rexpr_member_common.anchor_expr = anchor_expr;
        expr->u.rexpr_named_expr.named_expr =
            (struct named_expr *)resolved_member;
        return resolve2_ast_node_named_expr(expr, ctx);
    }
    resolved_member = find_statement_by_name(
        STATEMENT_TYPE_FIELD, member->u.identifier,
        &anchor_block->u.block_def.block_stmt_list);
    if (NULL != resolved_member) {
        expr->type = AST_NODE_TYPE_REXPR_FIELD;
        assert(NULL != anchor_expr);
        expr->u.rexpr_member_common.anchor_expr = anchor_expr;
        expr->u.rexpr_field.field = 
            (struct field *)resolved_member;
        return resolve2_ast_node_field(expr, ctx);
    }
    semantic_error(
        SEMANTIC_LOGLEVEL_ERROR, &member->loc,
        "no named expression or field named '%s' exists in block",
        member->u.identifier);
    semantic_error(SEMANTIC_LOGLEVEL_INFO, &anchor_block->loc,
                   "declared here");
    return -1;
}

static int
resolve2_ast_node_named_expr(struct ast_node *expr,
                             struct resolve2_ctx *ctx)
{
    struct named_expr *named_expr;
    struct ast_node *target;
    struct ast_node *filter_target;

    named_expr = (struct named_expr *) expr->u.rexpr_named_expr.named_expr;
    target = named_expr->expr;
    filter_target = expr->u.rexpr_named_expr.filter_target;
    if (-1 == resolve2_ast_node(target, ctx)) {
        return -1;
    }
    if (NULL != filter_target) {
        if (-1 == resolve2_ast_node(filter_target, ctx)) {
            return -1;
        }
        expr->u.rexpr.target_item = filter_target;
    } else {
        expr->u.rexpr.target_item =
            (struct ast_node *)ast_node_get_as_type(target);
    }
    expr->u.rexpr.value_type = expr_value_type_from_node(target);
    if (NULL == named_expr->nstmt.next_sibling) {
        // when named expression has no duplicate in the block
        expr->u.rexpr.dpath_type = target->u.rexpr.dpath_type;
    } else {
        // TODO: We may optimize with EXPR_DPATH_TYPE_ITEM whenever
        // all duplicate dpath expressions use item type, though this
        // requires post-processing when all types have been
        // resolved. Container type is more universal.
        expr->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    }
    return 0;
}

static int
resolve2_ast_node_field(struct ast_node *expr, struct resolve2_ctx *ctx)
{
    struct field *field;

    field = (struct field *)expr->u.rexpr_field.field;

    if (-1 == resolve2_dpath_node(&field->dpath, ctx)) {
        return -1;
    }
    expr->u.rexpr.value_type = expr_value_type_from_dpath_node(&field->dpath);
    expr->u.rexpr.dpath_type = EXPR_DPATH_TYPE_ITEM;
    expr->u.rexpr.target_item = expr->u.rexpr_field.field->dpath.item;
    return 0;
}

static int
resolve2_ast_node_subscript_index(struct ast_node *expr,
                                  struct subscript_index *subscript,
                                  struct resolve2_ctx *ctx)
{
    struct ast_node *key;
    struct ast_node *twin_idx;
    const struct ast_node *anchor_expr;
    const struct ast_node *anchor_item;
    struct ast_node *key_expr;
    int ret;

    if (NULL == subscript->key) {
        return 0;
    }
    ret = resolve2_ast_node(subscript->key, ctx);
    if (-1 == ret) {
        return -1;
    }
    key = subscript->key;
    assert(ast_node_is_rexpr(key));
    if (EXPR_VALUE_TYPE_INTEGER != key->u.rexpr.value_type &&
        EXPR_VALUE_TYPE_STRING != key->u.rexpr.value_type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &key->loc,
            "invalid expression type in array subscript: "
            "expect 'integer' or 'string', not '%s'",
            expr_value_type_str(key->u.rexpr.value_type));
        return -1;
    }
    if (NULL != subscript->twin) {
        ret = resolve2_ast_node(subscript->twin, ctx);
        if (-1 == ret) {
            return -1;
        }
        twin_idx = subscript->twin;
        assert(ast_node_is_rexpr(twin_idx));
        if (EXPR_VALUE_TYPE_INTEGER != twin_idx->u.rexpr.value_type
            && AST_NODE_TYPE_REXPR_STAR_WILDCARD != twin_idx->type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &twin_idx->loc,
                "invalid expression type in array subscript: "
                "twin index must be of type 'integer', not '%s'",
                expr_value_type_str(twin_idx->u.rexpr.value_type));
            return -1;
        }
        if (AST_NODE_TYPE_REXPR_NATIVE == twin_idx->type
            && EXPR_VALUE_TYPE_INTEGER == twin_idx->u.rexpr.value_type
            && twin_idx->u.rexpr_native.value.integer < 0) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &twin_idx->loc,
                "array integer twin index cannot be negative");
            return -1;
        }
    }
    anchor_expr = expr->u.rexpr_op_subscript_common.anchor_expr;
    anchor_item = anchor_expr->u.rexpr.target_item;
    if (NULL != anchor_item) {
        if (ast_node_is_indexed(anchor_item)) {
            /* integer subscript accesses items by raw index */
            if (EXPR_VALUE_TYPE_INTEGER != key->u.rexpr.value_type) {
                key_expr = ast_node_get_key_expr(anchor_item);
                assert(ast_node_is_rexpr(key));
                assert(ast_node_is_rexpr(key_expr));
                if (key->u.rexpr.value_type
                    != key_expr->u.rexpr.value_type) {
                    semantic_error(
                        SEMANTIC_LOGLEVEL_ERROR, &key->loc,
                        "invalid expression type in array subscript: "
                        "type mismatch between subscript type '%s' and "
                        "index type '%s'",
                        ast_node_type_str(key->u.rexpr.value_type),
                        ast_node_type_str(key_expr->u.rexpr.value_type));
                    return -1;
                }
            }
        } else if (EXPR_VALUE_TYPE_STRING == key->u.rexpr.value_type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &key->loc,
                "invalid expression type in array subscript: "
                "'string' type requires array element type to be indexed");
            return -1;
        }
    }
    return 0;
}

static int
resolve2_ast_node_subscript(struct ast_node *expr,
                            struct resolve2_ctx *ctx)
{
    int ret;
    struct subscript_index *index;
    struct ast_node *anchor_expr;
    const struct ast_node *anchor_item;

    assert(NULL != expr->u.rexpr_op_subscript_common.anchor_expr);
    ret = resolve2_ast_node(
        expr->u.rexpr_op_subscript_common.anchor_expr, ctx);
    if (-1 == ret) {
        return -1;
    }
    index = &expr->u.rexpr_op_subscript.index;
    ret = resolve2_ast_node_subscript_index(expr, index, ctx);
    if (-1 == ret) {
        return -1;
    }
    anchor_expr = expr->u.rexpr_op_subscript_common.anchor_expr;
    anchor_item = anchor_expr->u.rexpr.target_item;
    if (NULL != anchor_item) {
        struct ast_node *target_item;

        switch (anchor_item->type) {
        case AST_NODE_TYPE_ARRAY:
        case AST_NODE_TYPE_ARRAY_SLICE:
            target_item = anchor_item->u.array.item_type.item;
            ret = resolve2_dtype(target_item, ctx);
            if (-1 == ret) {
                return -1;
            }
            break ;
        case AST_NODE_TYPE_BYTE_ARRAY:
        case AST_NODE_TYPE_BYTE_SLICE:
            target_item = AST_NODE_BYTE;
            break ;
        default:
            assert(0);
        }
        expr->u.rexpr.target_item = target_item;
        expr->u.rexpr.value_type =
            expr_value_type_from_dpath_node(&anchor_item->u.array.item_type);
    }
    return 0;
}

static int
resolve2_ast_node_subscript_slice(struct ast_node *expr,
                                  struct resolve2_ctx *ctx)
{
    int ret;
    struct ast_node *anchor_expr;
    struct ast_node *anchor_item;
    struct subscript_index *slice_start;
    struct subscript_index *slice_end;

    anchor_expr = expr->u.rexpr_op_subscript_common.anchor_expr;
    assert(NULL != anchor_expr);
    ret = resolve2_ast_node(anchor_expr, ctx);
    if (-1 == ret) {
        return -1;
    }
    anchor_item = anchor_expr->u.rexpr.target_item;
    if (NULL != anchor_item) {
        if (! ast_node_is_subscriptable_container(anchor_item)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "invalid use of subscript operator on non-subscriptable "
                "path");
            return -1;
        }
    } else {
        if (EXPR_DPATH_TYPE_CONTAINER != anchor_expr->u.rexpr.dpath_type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "invalid use of subscript operator on non-dpath or "
                "non-container dpath expression");
            return -1;
        }
    }
    if (NULL != anchor_item) {
        expr->u.rexpr.target_item =
            ast_node_get_named_expr_target(anchor_item);
        expr->u.rexpr.value_type =
            expr_value_type_from_node(anchor_item);
    }
    slice_start = &expr->u.rexpr_op_subscript_slice.start;
    slice_end = &expr->u.rexpr_op_subscript_slice.end;
    if (NULL != slice_start->key) {
        ret = resolve2_ast_node(slice_start->key, ctx);
        if (-1 == ret) {
            return -1;
        }
    }
    ret = resolve2_ast_node_subscript_index(expr, slice_start, ctx);
    if (-1 == ret) {
        return -1;
    }
    ret = resolve2_ast_node_subscript_index(expr, slice_end, ctx);
    if (-1 == ret) {
        return -1;
    }
    expr->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    return 0;
}

/**
 * @brief second pass of resolve on expressions nodes of type
 * 'operator'
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
resolve2_ast_node_operator(struct ast_node *expr, int n_operands,
                           struct resolve2_ctx *ctx)
{
    int opd_i;
    struct ast_node *operand;
    enum expr_value_type opd_types[2]; /* max # operands is 2 */
    int only_native_operands;
    const struct expr_evaluator *evaluator;

    only_native_operands = TRUE;
    for (opd_i = 0; opd_i < n_operands; ++opd_i) {
        if (-1 == resolve2_ast_node(
                expr->u.rexpr_op.op.operands[opd_i], ctx)) {
            return -1;
        }
        operand = ast_node_get_named_expr_target(
            expr->u.rexpr_op.op.operands[opd_i]);
        opd_types[opd_i] = operand->u.rexpr.value_type;
        if (EXPR_VALUE_TYPE_UNSET == opd_types[opd_i]) {
            const struct ast_node *target_item;

            target_item = operand->u.rexpr.target_item;
            if (NULL == target_item) {
                target_item = operand;
            }
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &operand->loc,
                           "cannot use '%s' in expression",
                           ast_node_type_str(target_item->type));
            semantic_error(SEMANTIC_LOGLEVEL_INFO, &target_item->loc,
                           "declared here");
            return -1;
        }
        if (AST_NODE_TYPE_REXPR_NATIVE != operand->type) {
            only_native_operands = FALSE;
        }
    }
    evaluator = expr_lookup_evaluator(expr->type, opd_types);
    if (NULL == evaluator) {
        switch (n_operands) {
        case 1:
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "no match for %s with operand of type '%s'",
                ast_node_type_str(expr->type),
                expr_value_type_str(opd_types[0]));
            break ;
        case 2:
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "no match for %s with operands of type '%s' and '%s'",
                ast_node_type_str(expr->type),
                expr_value_type_str(opd_types[0]),
                expr_value_type_str(opd_types[1]));
            break ;
        default:
            break ;
        }
        return -1;
    }
    expr->u.rexpr_op.evaluator = evaluator;
    expr->u.rexpr.value_type = evaluator->res_type;

    if (only_native_operands) {
        struct ast_node *operand;
        union expr_value operand_values[2];
        const struct expr_evaluator *evaluator;
        union expr_value eval_value;

        for (opd_i = 0; opd_i < n_operands; ++opd_i) {
            operand = ast_node_get_named_expr_target(
                expr->u.rexpr_op.op.operands[opd_i]);
            assert(AST_NODE_TYPE_REXPR_NATIVE == operand->type);
            operand_values[opd_i] = operand->u.rexpr_native.value;
        }
        evaluator = expr->u.rexpr_op.evaluator;
        assert(NULL != evaluator);
        eval_value = evaluator->eval_fn(operand_values);

        expr->type = AST_NODE_TYPE_REXPR_NATIVE;
        /* expr->loc already set */
        /* expr->u.rexpr.value_type already set */
        expr->u.rexpr_native.value = eval_value;
    }
    return 0;
}

static int
resolve2_ast_node_operator_sizeof(struct ast_node *expr,
                                  struct resolve2_ctx *ctx)
{
    struct ast_node *operand;
    struct ast_node *target;
    struct ast_node *anchor;
    struct dpath_node dpath;

    operand = expr->u.rexpr_op.op.operands[0];
    if (-1 == resolve2_ast_node(operand, ctx)) {
        return -1;
    }
    target = ast_node_get_named_expr_target(operand);
    dpath_node_reset(&dpath);
    dpath.item = ast_node_get_target_item(target);
    if (ast_node_is_filter(target)) {
        dpath.filter = target;
    }
    anchor = target;
    while (NULL != anchor
           && AST_NODE_TYPE_REXPR_FIELD == anchor->type) {
        anchor = ast_node_get_named_expr_target(
            anchor->u.rexpr_member_common.anchor_expr);
    }
    if ((NULL != anchor && ast_node_is_item(anchor))
        || (NULL == anchor && ast_node_is_item(target))) {
        if (0 != (dpath.item->u.item.flags
                  & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                "invalid use of sizeof operator on dynamic-sized type name\n"
                "(use a dpath expression for computing size dynamically)");
            return -1;
        }
        expr->u.rexpr_op.op.operands[0] = dpath.item;
    } else if (!ast_node_is_rexpr(target)
               || EXPR_DPATH_TYPE_NONE == target->u.rexpr.dpath_type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "invalid use of sizeof operator on operand of type '%s'",
            ast_node_type_str(target->type));
        return -1;
    }
    if (NULL != dpath.item) {
        if (-1 == resolve2_span_size_dpath(&dpath, ctx)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve2_ast_node_operator_addrof(struct ast_node *expr,
                                  struct resolve2_ctx *ctx)
{
    struct op op;

    if (-1 == resolve2_ast_node(expr->u.rexpr_op.op.operands[0],
                                ctx)) {
        return -1;
    }
    op = expr->u.rexpr_op.op;
    if (EXPR_DPATH_TYPE_NONE == op.operands[0]->u.rexpr.dpath_type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
            "invalid use of addrof (&) operator on non-dpath node of type "
            "'%s'",
            ast_node_type_str(op.operands[0]->type));
        return -1;
    }
    return 0;
}

static int
resolve2_ast_node_fcall(struct ast_node *expr, struct resolve2_ctx *ctx)
{
    struct statement *stmt;
    struct named_expr *param;

    /* resolve expressions in parameter list */
    TAILQ_FOREACH(stmt, expr->u.rexpr_op_fcall.func_params, list) {
        param = (struct named_expr *)stmt;
        if (-1 == resolve2_ast_node(param->expr, ctx)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve2_span_size_common(struct ast_node *item,
                          struct ast_node *filter,
                          struct resolve2_ctx *ctx)
{
    struct ast_node *as_item;
    int ret;

    if (NULL != item) {
        assert(ast_node_is_item(item));
        if (SPAN_SIZE_UNDEF == item->u.item.min_span_size) {
            ret = resolve2_span_size_item(item, ctx);
            if (0 != ret) {
                return ret;
            }
        }
    }
    if (NULL != filter) {
        struct ast_node *filter_expr;
        struct ast_node *filter_target;

        filter_expr = ast_node_get_named_expr_target(filter);
        assert(ast_node_is_filter(filter_expr));
        as_item = (struct ast_node *)ast_node_get_as_type(filter_expr);
        filter_target = filter_expr->u.rexpr_filter.target;
        if (NULL != filter_target) {
            schedule_resolve_span_size_expr(filter_target, ctx, NULL, NULL);
        }
    } else {
        as_item = item;
    }
    as_item = ast_node_get_named_expr_target(as_item);
    if (NULL != as_item && as_item != item
        && ast_node_is_item(as_item)) {
        if (SPAN_SIZE_UNDEF == as_item->u.item.min_span_size) {
            ret = resolve2_span_size_item(as_item, ctx);
            if (0 != ret) {
                return ret;
            }
        }
        if (NULL != item
            && 0 == (item->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)
            && as_item->u.item.min_span_size > item->u.item.min_span_size) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &as_item->loc,
                "invalid as-type filter: cast-to type minimum size is "
                "greater than static size of destination "
                "(as type size %s %"PRIi64", target size == %"PRIi64")",
                (0 != (as_item->u.item.flags
                       & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC) ?
                 ">=" : "=="),
                as_item->u.item.min_span_size,
                item->u.item.min_span_size);
            return -1;
        }
    }
    return 0;
}

static int
resolve2_span_size_dpath(struct dpath_node *node, struct resolve2_ctx *ctx)
{
    struct ast_node *item;
    int ret;

    item = ast_node_get_target_item(node->item);
    if (NULL == item) {
        return -1;
    }
    ret = resolve2_span_size_common(item, node->filter, ctx);
    if (0 == ret) {
        node->u.item = item->u.item;
    }
    return ret;
}

static int
resolve2_span_size_expr(struct ast_node *expr, struct resolve2_ctx *ctx)
{
    struct ast_node *expr_target;
    struct ast_node *item;
    struct ast_node *filter;

    expr_target = ast_node_get_named_expr_target(expr);
    item = ast_node_get_target_item(expr_target);
    filter = (ast_node_is_filter(expr_target) ? expr_target : NULL);
    return resolve2_span_size_common(item, filter, ctx);
}

static int
resolve2_span_size_item(struct ast_node *item, struct resolve2_ctx *ctx)
{
    int ret;

    if (0 != (item->flags & ASTFLAG_PROCESSING)) {
        return RESOLVE2_CIRCULAR_DEPENDENCY;
    }
    item->flags |= ASTFLAG_PROCESSING;
    switch (item->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        ret = resolve2_span_size_block(item, ctx);
        break ;
    case AST_NODE_TYPE_ARRAY:
        ret = resolve2_span_size_array(item, ctx);
        break ;
    case AST_NODE_TYPE_BYTE:
        ret = resolve2_span_size_byte(item, ctx);
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
        ret = resolve2_span_size_byte_array(item, ctx);
        break ;
    default:
        assert(0);
    }
    if (0 == ret) {
        assert(SPAN_SIZE_UNDEF != item->u.item.min_span_size);
    }
    item->flags &= ~ASTFLAG_PROCESSING;
    return ret;
}

static int
resolve2_span_size_block(struct ast_node *item, struct resolve2_ctx *ctx)
{
    struct span_stmt *span_stmt;
    struct ast_node *min_span_expr;
    struct ast_node *max_span_expr;
    const struct statement_list *field_list;
    struct field *field;
    struct dpath_node *field_type;
    struct field *compute_offset_backwards_from;
    int ret;
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
                      item->u.block_def.block_stmt_list.span_list, list) {
        if (NULL == span_stmt->stmt.cond) {
            if (NULL != span_stmt->span_expr
                && -1 == resolve2_ast_node(span_stmt->span_expr, ctx)) {
                return -1;
            }
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
    field_list = item->u.block_def.block_stmt_list.field_list;
    compute_offset_backwards_from = NULL;
    STATEMENT_FOREACH(field, field, field_list, list) {
        ret = resolve2_span_size_dpath(&field->dpath, ctx);
        if (-1 == ret) {
            return -1;
        }
        field_type = &field->dpath;
        /* for recursive structures, the size has to be dynamic */
        if (RESOLVE2_CIRCULAR_DEPENDENCY == ret
            || 0 != (field_type->u.item.flags
                     & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            dynamic_used = TRUE;
        } else if (NULL == field->nstmt.stmt.cond) {
            /* only update min span size if field is not conditional */
            assert(SPAN_SIZE_UNDEF != field_type->u.item.min_span_size);
            if (BLOCK_TYPE_UNION == item->u.block_def.type) {
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
        } else if (BLOCK_TYPE_STRUCT == item->u.block_def.type
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
        assert(EXPR_VALUE_TYPE_INTEGER == min_span_expr->u.rexpr.value_type);
        if (AST_NODE_TYPE_REXPR_NATIVE == min_span_expr->type) {
            int64_t user_min_span_size;

            user_min_span_size = min_span_expr->u.rexpr_native.value.integer;
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
        assert(EXPR_VALUE_TYPE_INTEGER == max_span_expr->u.rexpr.value_type);
        if (AST_NODE_TYPE_REXPR_NATIVE == max_span_expr->type) {
            int64_t user_max_span_size;

            user_max_span_size = max_span_expr->u.rexpr_native.value.integer;
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
        item->u.item.flags |= ITEMFLAG_NEED_SLACK;
    }
    item->u.item.min_span_size = min_span_size;
    if (dynamic_span) {
        item->u.item.flags |= ITEMFLAG_IS_SPAN_SIZE_DYNAMIC;
    }
    if (dynamic_used) {
        item->u.item.flags |= ITEMFLAG_IS_USED_SIZE_DYNAMIC;
    }
    if (child_has_undetermined_size && NULL == max_span_expr) {
        item->u.item.flags |= ITEMFLAG_HAS_UNDETERMINED_SIZE;
    }
    if (NULL != compute_offset_backwards_from) {
        item->flags |= ASTFLAG_HAS_FOOTER;
    }
    if (!TAILQ_EMPTY(item->u.block_def.block_stmt_list.last_stmt_list)) {
        item->flags |= ASTFLAG_CONTAINS_LAST_STMT;
    }
    return 0;
}

static int
resolve2_span_size_array(struct ast_node *array, struct resolve2_ctx *ctx)
{
    struct dpath_node *item_dpath;
    struct ast_node *item_type;
    struct ast_node *item_count_expr;
    int ret;
    int64_t item_count;
    int64_t min_span_size;
    int dynamic_span;

    if (-1 == resolve2_dpath_node(&array->u.array.item_type, ctx)) {
        return -1;
    }
    if (NULL != array->u.array.item_count &&
        -1 == resolve2_ast_node(array->u.array.item_count, ctx)) {
        return -1;
    }
    item_count_expr =
        ast_node_get_named_expr_target(array->u.array.item_count);
    if (NULL != item_count_expr
        && (EXPR_VALUE_TYPE_INTEGER
            != item_count_expr->u.rexpr.value_type)) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &item_count_expr->loc,
                       "invalid array size type: expect '%s', got '%s'",
                       expr_value_type_str(EXPR_VALUE_TYPE_INTEGER),
                       expr_value_type_str(item_count_expr->u.rexpr.value_type));
        return -1;
    }
    item_dpath = &array->u.array.item_type;
    ret = resolve2_span_size_dpath(item_dpath, ctx);
    if (-1 == ret) {
        return -1;
    }
    item_type = ast_node_get_target_item(item_dpath->item);
    assert(ast_node_is_item(item_type));
    if (ret != RESOLVE2_CIRCULAR_DEPENDENCY
        && NULL != item_count_expr
        && AST_NODE_TYPE_REXPR_NATIVE == item_count_expr->type) {
        assert(SPAN_SIZE_UNDEF != item_type->u.item.min_span_size);
        assert(EXPR_VALUE_TYPE_INTEGER
               == item_count_expr->u.rexpr.value_type);
        assert(SPAN_SIZE_UNDEF != item_type->u.item.min_span_size);
        item_count = item_count_expr->u.rexpr_native.value.integer;
        min_span_size = item_count * item_type->u.item.min_span_size;
        dynamic_span = (0 != (item_dpath->u.item.flags
                              & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC));
    } else {
        if (RESOLVE2_CIRCULAR_DEPENDENCY == ret) {
            // in case some child item's span size is already being
            // computed (because of type recursion) we still want to
            // resolve its span size, so we reschedule this at a later
            // time.
            schedule_resolve_span_size_dpath(item_dpath, ctx, NULL, NULL);
        }
        min_span_size = 0;
        dynamic_span = TRUE;
    }
    if (0 != (item_type->flags & ASTFLAG_CONTAINS_LAST_STMT)) {
        dynamic_span = TRUE;
    } else if (0 != (item_dpath->u.item.flags & ITEMFLAG_NEED_SLACK)
               || NULL == item_count_expr) {
        array->u.item.flags |= ITEMFLAG_NEED_SLACK;
    }
    array->u.item.min_span_size = min_span_size;
    if (dynamic_span) {
        array->u.item.flags |= (ITEMFLAG_IS_SPAN_SIZE_DYNAMIC |
                                ITEMFLAG_IS_USED_SIZE_DYNAMIC);
    }
    if (NULL == item_count_expr
        || 0 != (item_dpath->u.item.flags
                 & ITEMFLAG_HAS_UNDETERMINED_SIZE)) {
        array->u.item.flags |= ITEMFLAG_HAS_UNDETERMINED_SIZE;
    }
    return 0;
}

static int
resolve2_span_size_byte(struct ast_node *byte, struct resolve2_ctx *ctx)
{
    return 0;
}

static int
resolve2_span_size_byte_array(struct ast_node *byte_array,
                              struct resolve2_ctx *ctx)
{
    struct ast_node *size_expr;
    int64_t min_span_size;

    if (NULL != byte_array->u.byte_array.size &&
        -1 == resolve2_ast_node(byte_array->u.byte_array.size, ctx)) {
        return -1;
    }
    size_expr = byte_array->u.byte_array.size;
    if (NULL != size_expr) {
        assert(ast_node_is_rexpr(size_expr));
        if (EXPR_VALUE_TYPE_INTEGER != size_expr->u.rexpr.value_type) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &size_expr->loc,
                           "invalid byte array size type: expect '%s', "
                           "got '%s'",
                           expr_value_type_str(EXPR_VALUE_TYPE_INTEGER),
                           expr_value_type_str(size_expr->u.rexpr.value_type));
            return -1;
        }
    }
    if (NULL != size_expr
        && AST_NODE_TYPE_REXPR_NATIVE == size_expr->type) {
        min_span_size = size_expr->u.rexpr_native.value.integer;
    } else {
        min_span_size = 0;
        byte_array->u.item.flags |= (ITEMFLAG_IS_SPAN_SIZE_DYNAMIC |
                                     ITEMFLAG_IS_USED_SIZE_DYNAMIC);
    }
    if (NULL == size_expr) {
        byte_array->u.item.flags |= (ITEMFLAG_NEED_SLACK |
                                     ITEMFLAG_HAS_UNDETERMINED_SIZE);
    }
    byte_array->u.item.min_span_size = min_span_size;
    return 0;
}

static int
resolve2_key_stmt_cb(struct ast_node *key_expr, struct resolve2_ctx *ctx,
                     void *cb_arg)
{
    assert(ast_node_is_rexpr(key_expr));
    if (key_expr->u.rexpr.value_type != EXPR_VALUE_TYPE_INTEGER &&
        key_expr->u.rexpr.value_type != EXPR_VALUE_TYPE_STRING) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &key_expr->loc,
            "index expression must be of integer or string type, not '%s'",
            expr_value_type_str(key_expr->u.rexpr.value_type));
        return -1;
    }
    return 0;
}

static int
resolve2_key_stmt(struct key_stmt *key_stmt, struct resolve2_ctx *ctx)
{
    schedule_resolve_expression_types(key_stmt->key_expr, ctx,
                                      resolve2_key_stmt_cb, NULL);
    return 0;
}

static int
resolve2_span_expr(struct ast_node *span_expr, struct resolve2_ctx *ctx)
{
    if (-1 == resolve2_ast_node(span_expr, ctx)) {
        return -1;
    }
    assert(ast_node_is_rexpr(span_expr));
    if (span_expr->u.rexpr.value_type != EXPR_VALUE_TYPE_INTEGER) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &span_expr->loc,
            "span expression must be of integer type, not '%s'",
            expr_value_type_str(span_expr->u.rexpr.value_type));
        return -1;
    }
    return 0;
}

static int
resolve2_named_expr(struct named_expr *named_expr, struct resolve2_ctx *ctx)
{
    schedule_resolve_expression_types(named_expr->expr, ctx, NULL, NULL);
    schedule_resolve_span_size_expr(named_expr->expr, ctx, NULL, NULL);
    return 0;
}

static int
resolve2_field_cb(struct dpath_node *dpath,
                  struct resolve2_ctx *ctx,
                  void *cb_arg)
{
    const struct ast_node *target_item;
    struct field *field;

    field = (struct field *)cb_arg;
    target_item = ast_node_get_target_item(dpath->item);
    if (NULL == target_item) {
        assert(NULL != dpath->filter);
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &dpath->filter->loc,
                       "expect an item type as field type, not '%s'",
                       ast_node_type_str(dpath->filter->type));
        return -1;
    }
    if (NULL == field->nstmt.name) {
        const struct ast_node *as_type;

        as_type = ast_node_get_named_expr_target(
            (struct ast_node *)dpath_node_get_as_type(&field->dpath));
        if (AST_NODE_TYPE_BLOCK_DEF != as_type->type) {
            field->nstmt.stmt.stmt_flags |= FIELD_FLAG_HIDDEN;
        }
    }
    return 0;
}

static int
resolve2_field(struct field *field, struct resolve2_ctx *ctx)
{
    schedule_resolve_dpath_node(&field->dpath, ctx,
                                resolve2_field_cb, field);
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
    if (-1 == setup_track_backends_dpath(DPATH_NODE_FILTERED)) {
        return -1;
    }
    return 0;
}

static int
setup_track_backends_dpath(struct dpath_node *dpath)
{
    int ret = 0;

    if (NULL != dpath->item
        && 0 == (dpath->item->flags & (ASTFLAG_PROCESSING |
                                       ASTFLAG_PROCESSED_TRACK_BACKEND))) {
        dpath->item->flags |= ASTFLAG_PROCESSING;
        switch (dpath->item->type) {
        case AST_NODE_TYPE_BLOCK_DEF:
            ret = setup_track_backends_recur_block(dpath->item);
            break ;
        case AST_NODE_TYPE_ARRAY:
            ret = setup_track_backends_dpath(&dpath->item->u.array.item_type);
            break ;
        default:
            break ;
        }
        dpath->item->flags &= ~ASTFLAG_PROCESSING;
        if (-1 == ret) {
            return -1;
        }
        dpath->item->flags |= ASTFLAG_PROCESSED_TRACK_BACKEND;
    }
    if (NULL != dpath->filter
        && AST_NODE_TYPE_REXPR_AS_TYPE == dpath->filter->type
        && 0 == (dpath->filter->flags & (ASTFLAG_PROCESSING |
                                         ASTFLAG_PROCESSED_TRACK_BACKEND))) {
        struct dpath_node *as_dpath;

        dpath->filter->flags |= ASTFLAG_PROCESSING;
        as_dpath = &dpath->filter->u.rexpr_filter.filter_dpath;
        ret = setup_track_backends_dpath(as_dpath);
        dpath->filter->flags &= ~ASTFLAG_PROCESSING;
        if (0 == ret) {
            ret = setup_track_backends_expr(
                dpath->filter->u.rexpr_filter.target);
        }
        if (-1 == ret) {
            return -1;
        }
        dpath->filter->flags |= ASTFLAG_PROCESSED_TRACK_BACKEND;
    }
    if (-1 == browse_setup_backends(dpath)) {
        return -1;
    }
    return 0;
}

static int
setup_track_backends_expr(struct ast_node *expr)
{
    struct dpath_node dpath;

    dpath_node_reset(&dpath);
    dpath.item = ast_node_get_target_item(expr);
    if (ast_node_is_filter(expr)) {
        dpath.filter = expr;
    }
    return setup_track_backends_dpath(&dpath);
}

static int
setup_track_backends_recur_block(struct ast_node *block)
{
    struct block_stmt_list *block_lists;
    const struct field *field;
    struct named_expr *named_expr;

    block_lists = &block->u.block_def.block_stmt_list;
    STATEMENT_FOREACH(named_expr, named_expr,
                      block_lists->named_expr_list, list) {
        if (-1 == setup_track_backends_expr(named_expr->expr)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(field, field, block_lists->field_list, list) {
        if (-1 == setup_track_backends_dpath(
                (struct dpath_node *)&field->dpath)) {
            return -1;
        }
    }
    return 0;
}


void
dpath_node_reset(struct dpath_node *dpath)
{
    memset(dpath, 0, sizeof (*dpath));
    dpath->u.item.min_span_size = SPAN_SIZE_UNDEF;
}

int
ast_node_is_rexpr(const struct ast_node *node)
{
    switch (node->type) {
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
    case AST_NODE_TYPE_REXPR_OP_FILTER:
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
ast_node_is_rexpr_to_item(const struct ast_node *node)
{
    if (AST_NODE_TYPE_REXPR_NAMED_EXPR == node->type) {
        const struct ast_node *target;

        target = node->u.rexpr_named_expr.named_expr->expr;
        if (ast_node_is_item(target)) {
            return TRUE;
        }
        return ast_node_is_rexpr_to_item(target);
    }
    switch (node->type) {
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

struct ast_node *
ast_node_get_target_item(struct ast_node *node)
{
    if (NULL == node || ast_node_is_item(node)) {
        return node;
    }
    switch (node->type) {
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        if (NULL != node->u.rexpr_named_expr.filter_target) {
            return ast_node_get_target_item(
                node->u.rexpr_named_expr.filter_target);
        } else {
            return ast_node_get_target_item(
                node->u.rexpr_named_expr.named_expr->expr);
        }
    case AST_NODE_TYPE_REXPR_FIELD:
        return ast_node_get_target_item(node->u.rexpr_field.field->dpath.item);
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        return ast_node_get_target_item(node->u.rexpr_filter.target);
    default:
        return NULL;
    }
}

struct ast_node *
ast_node_get_target_type(struct ast_node *node)
{
    if (NULL == node || ast_node_is_item(node)) {
        return node;
    }
    switch (node->type) {
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        if (NULL != node->u.rexpr_named_expr.filter_target) {
            return ast_node_get_target_type(
                node->u.rexpr_named_expr.filter_target);
        } else {
            return ast_node_get_target_type(
                node->u.rexpr_named_expr.named_expr->expr);
        }
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        return ast_node_get_target_type(node->u.rexpr_filter.target);
    default:
        return NULL;
    }
}

struct ast_node *
ast_node_get_target_filter(struct ast_node *node)
{
    if (NULL == node || ast_node_is_item(node)) {
        return NULL;
    }
    switch (node->type) {
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        return ast_node_get_target_filter(
            node->u.rexpr_named_expr.named_expr->expr);
    case AST_NODE_TYPE_REXPR_FIELD:
        if (NULL != node->u.rexpr_field.field->dpath.filter) {
            return node->u.rexpr_field.field->dpath.filter;
        } else {
            return ast_node_get_target_filter(
                node->u.rexpr_field.field->dpath.item);
        }
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        return node;
    default:
        return NULL;
    }
}

struct ast_node *
ast_node_get_named_expr_target(struct ast_node *node)
{
    if (NULL == node || AST_NODE_TYPE_REXPR_NAMED_EXPR != node->type) {
        return node;
    }
    return ast_node_get_named_expr_target(
        node->u.rexpr_named_expr.named_expr->expr);
}

int
ast_node_is_container(const struct ast_node *node)
{
    switch (node->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_FILTERED:
        return TRUE;
    default:
        return FALSE;
    }
}

int
ast_node_is_origin_container(const struct ast_node *node)
{
    switch (node->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
        return TRUE;
    default:
        return FALSE;
    }
}

int
ast_node_is_byte_container(const struct ast_node *node)
{
    switch (node->type) {
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_FILTERED:
        return TRUE;
    default:
        return FALSE;
    }
}

int
ast_node_is_subscriptable_container(const struct ast_node *node)
{
    switch (node->type) {
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
ast_node_is_slice_container(const struct ast_node *node)
{
    switch (node->type) {
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
        return TRUE;
    default:
        return FALSE;
    }
}

int
ast_node_is_item(const struct ast_node *node)
{
    switch (node->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_FILTERED:
        return TRUE;
    default:
        return FALSE;
    }
}

int
ast_node_is_type(const struct ast_node *node)
{
    return ast_node_is_item(node)
        || AST_NODE_TYPE_REXPR_AS_TYPE == node->type;
}


int
ast_node_is_filter(const struct ast_node *node)
{
    switch (node->type) {
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        return TRUE;
    default:
        return FALSE;
    }
}


static const struct ast_node *
ast_node_get_as_type__rexpr(const struct ast_node *expr)
{
    const struct ast_node *as_type;

    assert(ast_node_is_rexpr(expr));
    switch (expr->type) {
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        as_type = ast_node_get_as_type(
            expr->u.rexpr_named_expr.named_expr->expr);
        if (NULL != as_type && ast_node_is_item(as_type)) {
            return as_type;
        }
        as_type = expr->u.rexpr_named_expr.filter_target;
        if (NULL != as_type) {
            return ast_node_get_as_type(as_type);
        }
        return NULL;

    case AST_NODE_TYPE_REXPR_AS_TYPE:
        return ast_node_get_as_type(expr->u.rexpr_filter.filter_dpath.item);

    case AST_NODE_TYPE_REXPR_FIELD:
        if (NULL == expr->u.rexpr_field.field->dpath.filter) {
            return expr->u.rexpr_field.field->dpath.item;
        }
        return ast_node_get_as_type(expr->u.rexpr_field.field->dpath.filter);

    default:
        return expr->u.rexpr.target_item;
    }
}


const struct ast_node *
ast_node_get_as_type(const struct ast_node *node)
{
    if (ast_node_is_item(node)) {
        return node;
    }
    return ast_node_get_as_type__rexpr(node);
}

int64_t
ast_node_get_min_span_size(const struct ast_node *node)
{
    switch (node->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_BYTE_ARRAY:
        assert(SPAN_SIZE_UNDEF != node->u.item.min_span_size);
        return node->u.item.min_span_size;
    default:
        return 0;
    }
    /*NOT REACHED*/
}

int
ast_node_is_slack(const struct ast_node *node)
{
    switch (node->type) {
    case AST_NODE_TYPE_ARRAY:
        if (NULL == node->u.array.item_count) {
            return TRUE;
        }
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
        if (NULL == node->u.byte_array.size) {
            return TRUE;
        }
        break ;
    default:
        break ;
    }
    return FALSE;
}

int
ast_node_is_indexed(const struct ast_node *node)
{
    struct ast_node *target;

    switch (node->type) {
    case AST_NODE_TYPE_ARRAY:
        target = ast_node_get_named_expr_target(
            node->u.array.item_type.item);
        if (AST_NODE_TYPE_BLOCK_DEF != target->type) {
            return FALSE;
        }
        return !TAILQ_EMPTY(target->u.block_def.block_stmt_list.key_list);
    default:
        return FALSE;
    }
}

struct ast_node *
ast_node_get_key_expr(const struct ast_node *node)
{
    struct ast_node *target;

    switch (node->type) {
    case AST_NODE_TYPE_ARRAY:
        target = ast_node_get_named_expr_target(
            node->u.array.item_type.item);
        if (AST_NODE_TYPE_BLOCK_DEF != target->type) {
            return NULL;
        }
        // TODO: multiple or conditional key expressions currently not
        // supported (needs proper support in index cache)

        if (TAILQ_EMPTY(target->u.block_def.block_stmt_list.key_list)) {
            return NULL;
        }
        return ((struct key_stmt *)TAILQ_FIRST(
                    target->u.block_def
                    .block_stmt_list.key_list))->key_expr;
    default:
        return NULL;
    }
}

enum expr_value_type
ast_node_get_key_type(const struct ast_node *node)
{
    struct ast_node *key_expr;

    key_expr = ast_node_get_key_expr(node);
    assert(NULL != key_expr);
    return key_expr->u.rexpr.value_type;
}

const struct ast_node *
dpath_node_get_as_type(const struct dpath_node *dpath)
{
    if (NULL != dpath->filter) {
        return ast_node_get_as_type(dpath->filter);
    } else {
        return ast_node_get_target_item(dpath->item);
    }
}

const struct ast_node *
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
fdump_ast_recur(struct ast_node *node, int depth,
                struct list_of_visible_refs *visible_refs, FILE *stream);
static void
dump_block_recur(const struct ast_node *block,
                 int depth,
                 struct list_of_visible_refs *outer_refs,
                 FILE *stream);
static void
dump_block_stmt_list_recur(const struct ast_node *block,
                           const struct block_stmt_list *block_lists,
                           int depth,
                           struct list_of_visible_refs *outer_refs,
                           FILE *stream);
static void
dump_ast_type(const struct ast_node *node, int depth,
              struct list_of_visible_refs *visible_refs, FILE *stream);

void
dump_ast_location(struct ast_node *node)
{
    fdump_ast_location(node, stdout);
}

void
fdump_ast_location(struct ast_node *node, FILE *out)
{
    bitpunch_parser_print_location(&node->loc, out);
}

void
dump_ast(struct ast_node *root)
{
    fdump_ast(root, stdout);
}

void
fdump_ast(struct ast_node *root, FILE *out)
{
    fdump_ast_location(root, out);
    fdump_ast_recur(root, 0, NULL, out);
}
void
dump_block(const struct ast_node *block, FILE *out)
{
    dump_block_recur(block, 0, NULL, out);
}

static void
dump_ast_rexpr(const struct ast_node *node, FILE *out) {
    fprintf(out, "value_type: %s, dpath_type: %s",
            expr_value_type_str(node->u.rexpr.value_type),
            expr_dpath_type_str(node->u.rexpr.dpath_type));
}

static void
dump_target_item(const struct ast_node *node, int depth,
                 struct list_of_visible_refs *visible_refs, FILE *out)
{
    if (NULL != node->u.rexpr.target_item) {
        fprintf(out, "\n%*s\\_ target:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_type(node->u.rexpr.target_item,
                      depth + 2, visible_refs, out);
    }
}

static void
dump_ast_rexpr_member(const struct ast_node *node, int depth,
                      struct list_of_visible_refs *visible_refs, FILE *out)
{
    dump_target_item(node, depth, visible_refs, out);
    if (NULL != node->u.rexpr_member_common.anchor_expr) {
        fprintf(out, "\n%*s\\_ anchor expr:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_type(node->u.rexpr_member_common.anchor_expr, depth + 2,
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
dump_ast_item(struct ast_node *node, int depth,
              struct list_of_visible_refs *visible_refs, FILE *out)
{
}

static void
dump_ast_item_info(struct ast_node *node, FILE *out)
{
    fprintf(out, "min span size: %s%s%s",
            span_size_str(node->u.item.min_span_size),
            (0 != (node->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC) ?
             " (dynamic span)" : ""),
            (0 != (node->u.item.flags & ITEMFLAG_IS_USED_SIZE_DYNAMIC) ?
             " (dynamic used)" : ""));
}

static void
dump_ast_container(struct ast_node *node, int depth,
                   struct list_of_visible_refs *visible_refs, FILE *out)
{
    dump_ast_item(node, depth, visible_refs, out);
}

static void
fdump_ast_recur(struct ast_node *node, int depth,
                struct list_of_visible_refs *visible_refs, FILE *out)
{
    static struct ast_node dummy_empty_node = {
        .type = AST_NODE_TYPE_NONE
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
    switch (node->type) {
    case AST_NODE_TYPE_INTEGER:
        fprintf(out, "%"PRIi64"\n", node->u.integer);
        break ;
    case AST_NODE_TYPE_BOOLEAN:
        fprintf(out, "%s\n", (node->u.boolean ? "true" : "false"));
        break ;
    case AST_NODE_TYPE_STRING:
        print_expr_string(&node->u.string, out);
        fputc('\n', out);
        break ;
    case AST_NODE_TYPE_IDENTIFIER:
        fprintf(out, "\"%s\"\n", node->u.identifier);
        break ;
    case AST_NODE_TYPE_BLOCK_DEF:
        fprintf(out, "filter type: %s, block type: %s, ",
                node->u.block_def.filter_type,
                block_type_str(node->u.block_def.type));
        dump_ast_item_info(node, out);
        fprintf(out, "\n");
        dump_block_recur(node, depth + 1, visible_refs, out);
        break ;
    case AST_NODE_TYPE_ARRAY:
        fprintf(out, "\n%*s\\_ ", (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_item_info(node, out);
        fprintf(out, ", value type:\n");
        fdump_ast_recur(node->u.array.item_type.item, depth + 2,
                        visible_refs, out);
        if (NULL != node->u.array.item_type.filter) {
            fprintf(out, "%*s\\_ filter:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_type(node->u.array.item_type.filter, depth + 2,
                          visible_refs, out);
            fprintf(out, "\n");
        }
        fprintf(out, "%*s\\_ value count:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->u.array.item_count, depth + 2, visible_refs,
                        out);
        dump_ast_container(node, depth, visible_refs, out);
        break ;
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_FILTERED:
        fprintf(out, "\n%*s\\_ static node (%s)\n",
                (depth + 1) * INDENT_N_SPACES, "",
                ast_node_type_str(node->type));
        break ;
    case AST_NODE_TYPE_BYTE:
        fprintf(out, "\n");
        dump_ast_item(node, depth, visible_refs, out);
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
        dump_ast_item_info(node, out);
        fprintf(out, ", byte count:\n");
        fdump_ast_recur(node->u.byte_array.size, depth + 2, visible_refs,
                        out);
        dump_ast_container(node, depth, visible_refs, out);
        break ;
    case AST_NODE_TYPE_CONDITIONAL:
        assert(NULL != visible_refs); /* must be in a block */
        fprintf(out, "\n%*s\\_ condition expr:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->u.conditional.cond_expr,
                        depth + 2, visible_refs, out);
        if (NULL != node->u.conditional.outer_cond) {
            fprintf(out, "%*s\\_ outer conditional:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->u.conditional.outer_cond, depth + 2,
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
        fdump_ast_recur(node->u.op.operands[0], depth + 1, visible_refs,
                        out);
        fdump_ast_recur(node->u.op.operands[1], depth + 1, visible_refs,
                        out);
        break ;
    case AST_NODE_TYPE_OP_UPLUS:
    case AST_NODE_TYPE_OP_UMINUS:
    case AST_NODE_TYPE_OP_LNOT:
    case AST_NODE_TYPE_OP_BWNOT:
    case AST_NODE_TYPE_OP_SIZEOF:
    case AST_NODE_TYPE_OP_ADDROF:
    case AST_NODE_TYPE_OP_FILTER:
        fprintf(out, "\n");
        fdump_ast_recur(node->u.op.operands[0], depth + 1, visible_refs,
                        out);
        break ;
    case AST_NODE_TYPE_OP_SUBSCRIPT:
        fprintf(out, "\n");
        fdump_ast_recur(node->u.op_subscript_common.anchor_expr, depth + 1,
                        visible_refs, out);
        fdump_ast_recur(node->u.op_subscript.index.key, depth + 1,
                        visible_refs, out);
        if (NULL != node->u.op_subscript.index.twin) {
            fprintf(out, "%*s\\_ twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->u.op_subscript.index.twin, depth + 1,
                            visible_refs, out);
        }
        break ;
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
        fprintf(out, "\n");
        fdump_ast_recur(node->u.op_subscript_common.anchor_expr, depth + 1,
                        visible_refs, out);
        if (NULL != node->u.op_subscript_slice.start.key) {
            fprintf(out, "%*s\\_ start index:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->u.op_subscript_slice.start.key, depth + 1,
                            visible_refs, out);
        }
        if (NULL != node->u.op_subscript_slice.start.twin) {
            fprintf(out, "%*s\\_ start twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->u.op_subscript_slice.start.twin, depth + 1,
                            visible_refs, out);
        }
        if (NULL != node->u.op_subscript_slice.end.key) {
            fprintf(out, "%*s\\_ end index:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->u.op_subscript_slice.end.key, depth + 1,
                            visible_refs, out);
        }
        if (NULL != node->u.op_subscript_slice.end.twin) {
            fprintf(out, "%*s\\_ end twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->u.op_subscript_slice.end.twin, depth + 1,
                            visible_refs, out);
        }
        break ;
    case AST_NODE_TYPE_OP_FCALL: {
        struct statement *stmt;
        struct named_expr *func_param;

        fprintf(out, "\n");
        fdump_ast_recur(node->u.op_fcall.func, depth + 1, visible_refs,
                        out);

        fprintf(out, "\n%*s\\_ params:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        TAILQ_FOREACH(stmt, node->u.op_fcall.func_params, list) {
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
                node->u.rexpr_op_fcall.builtin->builtin_name);
        TAILQ_FOREACH(stmt, node->u.rexpr_op_fcall.func_params, list) {
            func_param = (struct named_expr *)stmt;
            fdump_ast_recur(func_param->expr, depth + 1, visible_refs,
                            out);
        }
        break ;
    }
    case AST_NODE_TYPE_REXPR_NATIVE:
        switch (node->u.rexpr_native.rexpr.value_type) {
        case EXPR_VALUE_TYPE_INTEGER:
            fprintf(out, "%"PRIi64"\n", node->u.rexpr_native.value.integer);
            break ;
        case EXPR_VALUE_TYPE_BOOLEAN:
            fprintf(out, "%s\n",
                    (node->u.rexpr_native.value.boolean ?
                     "true" : "false"));
            break ;
        case EXPR_VALUE_TYPE_STRING:
            print_expr_string(&node->u.rexpr_native.value.string, out);
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
        fdump_ast_recur(node->u.rexpr_op.op.operands[0], depth + 1,
                        visible_refs, out);
        fdump_ast_recur(node->u.rexpr_op.op.operands[1], depth + 1,
                        visible_refs, out);
        break ;
    case AST_NODE_TYPE_REXPR_INTERPRETER: {
        const struct interpreter *interpreter;
        struct interpreter_param_def *param_def;
        struct ast_node *param;
        int i;

        dump_ast_rexpr(node,out);
        interpreter = node->u.rexpr_interpreter.interpreter;
        fprintf(out, " name: %s\n%*s\\_ interpreter params:\n",
                interpreter->name, (depth + 1) * INDENT_N_SPACES, "");
        param_def = STAILQ_FIRST(&interpreter->param_list);
        for (i = 0; i < node->u.rexpr_interpreter.interpreter->n_params;
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
        fdump_ast_recur(node->u.rexpr_filter.target, depth + 2,
                        visible_refs, out);
        break ;
    }
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        dump_ast_rexpr(node, out);
        fprintf(out, "\n%*s\\_ target:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->u.rexpr_filter.target, depth + 2,
                        visible_refs, out);
        fprintf(out, "%*s\\_ as type:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        fdump_ast_recur(node->u.rexpr_filter.filter_dpath.item, depth + 2,
                        visible_refs, out);
        break ;
    case AST_NODE_TYPE_REXPR_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT:
    case AST_NODE_TYPE_REXPR_OP_SIZEOF:
    case AST_NODE_TYPE_REXPR_OP_ADDROF:
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        dump_ast_rexpr(node, out);
        fprintf(out, "\n");
        fdump_ast_recur(node->u.rexpr_op.op.operands[0], depth + 1,
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
        dump_ast_type(node->u.rexpr_field.field->dpath.item, depth + 3,
                      visible_refs, out);
        fprintf(out, "\n");
        if (NULL != node->u.rexpr_field.field->dpath.filter) {
            fprintf(out, "%*s\\_ filter:\n",
                    (depth + 2) * INDENT_N_SPACES, "");
            dump_ast_type(node->u.rexpr_field.field->dpath.filter, depth + 3,
                          visible_refs, out);
            fprintf(out, "\n");
        }
        break ;
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        dump_ast_rexpr_member(node, depth, visible_refs, out);
        fprintf(out, "%*s\\_ named expr:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_type(node->u.rexpr_named_expr.named_expr->expr, depth + 2,
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
        fdump_ast_recur(node->u.rexpr_op_subscript_common.anchor_expr, depth + 1,
                        visible_refs, out);
        fdump_ast_recur(node->u.rexpr_op_subscript.index.key, depth + 1,
                        visible_refs, out);
        if (NULL != node->u.rexpr_op_subscript.index.twin) {
            fprintf(out, "%*s\\_ twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->u.rexpr_op_subscript.index.twin, depth + 1,
                            visible_refs, out);
        }
        break ;
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        dump_ast_rexpr(node, out);
        fprintf(out, "\n");
        fdump_ast_recur(node->u.rexpr_op_subscript_common.anchor_expr, depth + 1,
                        visible_refs, out);
        if (NULL != node->u.rexpr_op_subscript_slice.start.key) {
            fprintf(out, "%*s\\_ start index:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->u.rexpr_op_subscript_slice.start.key,
                            depth + 2, visible_refs, out);
        }
        if (NULL != node->u.rexpr_op_subscript_slice.start.twin) {
            fprintf(out, "%*s\\_ start twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->u.rexpr_op_subscript_slice.start.twin,
                            depth + 2, visible_refs, out);
        }
        if (NULL != node->u.rexpr_op_subscript_slice.end.key) {
            fprintf(out, "%*s\\_ end index:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->u.rexpr_op_subscript_slice.end.key,
                            depth + 2, visible_refs, out);
        }
        if (NULL != node->u.rexpr_op_subscript_slice.end.twin) {
            fprintf(out, "%*s\\_ end twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            fdump_ast_recur(node->u.rexpr_op_subscript_slice.end.twin,
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
dump_ast_type(const struct ast_node *node, int depth,
              struct list_of_visible_refs *visible_refs, FILE *out)
{
    const char *type_name;
    int noname = FALSE;

    switch (node->type) {
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
                ast_node_type_str(node->type));
        break ;
    case AST_NODE_TYPE_REXPR_INTERPRETER:
    case AST_NODE_TYPE_BLOCK_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_FILTERED:
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
    case AST_NODE_TYPE_OP_FILTER:
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
    case AST_NODE_TYPE_REXPR_OP_FILTER:
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
                ast_node_type_str(node->type));
        break ;
    }

    switch (node->type) {
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
dump_block_recur(const struct ast_node *block,
                 int depth,
                 struct list_of_visible_refs *outer_refs,
                 FILE *out)
{
    if (NULL == block) {
        return ;
    }
    dump_block_stmt_list_recur(block, &block->u.block_def.block_stmt_list,
                               depth, outer_refs, out);
}

static void
dump_block_stmt_list_recur(const struct ast_node *block,
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
dump_ast_node_input_text(const struct ast_node *node,
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
