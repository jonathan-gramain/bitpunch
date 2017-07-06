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

struct list_of_visible_refs {
    const struct list_of_visible_refs *outer_refs;
    const struct ast_node             *cur_block;
    const struct block_stmt_list      *cur_lists;
};

enum resolve_expect_node_mask {
    RESOLVE_EXPECT_TYPENAME    = (1u<<1),
    RESOLVE_EXPECT_TYPEDEF     = (1u<<2),
    RESOLVE_EXPECT_EXPRESSION  = (1u<<3),
    RESOLVE_EXPECT_INTERPRETER = (1u<<4),
};

/* keep a stack of useful contexts for error reporting */

enum call_stack_entry_type {
    CALL_RESOLVE2_DTYPE = (1u<<0),
    CALL_RESOLVE2_EXPR = (1u<<1),
    CALL_RESOLVE2_SPAN_SIZE = (1u<<2),
};

struct call_stack_error_ctx {
    struct call_stack_error_ctx *prev;
    struct call_stack_error_ctx *next;
    enum call_stack_entry_type  call_type;
    union {
        const struct ast_node   *dtype;
        const struct ast_node   *expr;
    } u;
};

static __thread
struct call_stack_error_ctx *error_ctx_stack_base = NULL;
static __thread
struct call_stack_error_ctx *error_ctx_stack_top = NULL;

static void
push_error_ctx(struct call_stack_error_ctx *error_ctx);
static void
pop_error_ctx(void);

static void
ast_node_free(struct ast_node *node);

static struct ast_node *
lookup_type(const char *identifier,
            const struct list_of_visible_refs *visible_refs);
static const char *
reverse_lookup_typename(const struct ast_node *node,
                        const struct list_of_visible_refs *visible_refs);
static int
resolve_block_types(struct ast_node *block,
                    struct list_of_visible_refs *outer_refs);
static int
resolve_stmt_list_types_generic(
    struct statement_list *stmt_list,
    const struct list_of_visible_refs *visible_refs);
static int
resolve_stmt_lists_types(struct block_stmt_list *stmt_lists,
                         const struct list_of_visible_refs *outer_refs);
static int
check_duplicate_type_name(const struct named_type *named_type,
                          const struct list_of_visible_refs *visible_refs);
static int
chain_duplicate_statements_in(const struct statement_list *in_list,
                              const struct statement_list *toplevel_list);
static int
chain_duplicate_statements(const struct block_stmt_list *stmt_lists);
static int
resolve_node_types(struct ast_node **node_p,
                   const struct list_of_visible_refs *visible_refs,
                   enum resolve_expect_node_mask expect_mask);
static int
resolve_node_types_int(struct ast_node *node,
                       const struct list_of_visible_refs *visible_refs,
                       enum resolve_expect_node_mask expect_mask,
                       struct ast_node **resolved_typep);
static int
resolve_node_types_identifier(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_node_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_node_types_typename(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_node_mask expect_mask,
    struct ast_node **resolved_typep);
static int
resolve_node_types_array(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    struct ast_node **resolved_typep);
static int
resolve_node_types_byte_array(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    struct ast_node **resolved_typep);
static int
resolve_node_types_conditional(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    struct ast_node **resolved_typep);
static int
resolve_node_types_op_subscript(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    struct ast_node **resolved_typep);
static int
resolve_node_types_op_subscript_slice(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    struct ast_node **resolved_typep);
static int
resolve_node_types_op_fcall(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    struct ast_node **resolved_typep);
static int
resolve_node_types_operator(
    struct ast_node *node,
    int n_operands,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_node_mask operand_mask,
    struct ast_node **resolved_typep);
static int
resolve_node_types_filter(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_node_mask expect_mask);

static int
resolve_block_expressions(struct ast_node *block,
                          struct list_of_visible_refs *outer_refs);
static int
resolve_stmt_list_expressions_generic(
    struct statement_list *stmt_list,
    struct list_of_visible_refs *visible_refs);
static int
resolve_stmt_lists_expressions(const struct ast_node *block,
                               struct block_stmt_list *stmt_lists,
                               struct list_of_visible_refs *outer_refs);
static int
resolve_dtype_expressions(struct ast_node **dtype_p,
                          struct list_of_visible_refs *visible_refs);
static int
resolve_dtype_expressions_array(struct ast_node **dtype_p,
                                struct list_of_visible_refs *visible_refs);
static int
resolve_dtype_expressions_byte_array(struct ast_node **dtype_p,
                                     struct list_of_visible_refs *visible_refs);
static int
resolve_dtype_expressions_conditional(struct ast_node **dtype_p,
                                      struct list_of_visible_refs *visible_refs);
static int
resolve_expr(struct ast_node **expr_p,
             struct list_of_visible_refs *visible_refs);
static void
resolve_expr_integer(struct ast_node **expr_p);
static void
resolve_expr_boolean(struct ast_node **expr_p);
static void
resolve_expr_value_string(struct ast_node **expr_p);
static int
resolve_expr_identifier(struct ast_node **expr_p,
                        struct list_of_visible_refs *visible_refs);
static int
resolve_expr_filter(struct ast_node **expr_p,
                    struct list_of_visible_refs *visible_refs);
static int
resolve_expr_operator(struct ast_node **expr_p,
                      int n_operands,
                      struct list_of_visible_refs *visible_refs);
static int
resolve_expr_operator_sizeof(struct ast_node **expr_p,
                             struct list_of_visible_refs *visible_refs);
static int
resolve_expr_operator_addrof(struct ast_node **expr_p,
                             struct list_of_visible_refs *visible_refs);
static int
resolve_expr_operator_filter(struct ast_node **expr_p,
                             struct list_of_visible_refs *visible_refs);
static int
resolve_expr_file(struct ast_node **expr_p,
                  struct list_of_visible_refs *visible_refs);
static int
resolve_expr_self(struct ast_node **expr_p,
                  struct list_of_visible_refs *visible_refs);
static int
resolve_expr_star_wildcard(struct ast_node **expr_p,
                           struct list_of_visible_refs *visible_refs);
static enum ast_node_type
op_type_ast2rexpr(enum ast_node_type type);
static int
resolve_expr_subscript(struct ast_node **expr_p,
                       struct list_of_visible_refs *visible_refs);
static int
resolve_expr_subscript_slice(struct ast_node **expr_p,
                             struct list_of_visible_refs *visible_refs);
static int
resolve_expr_op_member(struct ast_node **expr_p,
                       struct list_of_visible_refs *visible_refs);
static int
resolve_expr_fcall(struct ast_node **expr_p,
                   struct list_of_visible_refs *visible_refs);
static int
resolve_expr_operator_set_filter(struct ast_node **expr_p,
                                  struct list_of_visible_refs *visible_refs);
static int
resolve_span_expr(struct ast_node **span_expr_p,
                  struct list_of_visible_refs *visible_refs);
static int
resolve_link(struct link *link,
             struct list_of_visible_refs *visible_refs);

static int
resolve2_block(struct ast_node *block);
static int
link_check_duplicates(struct link *link);
static int
links_check_duplicates(struct statement_list *link_list);
static int
resolve2_stmt_list_generic(struct statement_list *stmt_list);
static int
resolve2_stmt_lists(struct block_stmt_list *stmt_lists);
static int
resolve2_dtype(struct ast_node *dtype);
static int
resolve2_conditional(struct ast_node *if_node);
static int
resolve2_filter(struct ast_node *filter);
static int
resolve2_expr(struct ast_node **expr_p);
static int
resolve2_expr_field(struct ast_node **expr_p);
static int
resolve2_expr_link(struct ast_node **expr_p);
static int
resolve2_expr_operator_subscript(struct ast_node **expr_p);
static int
resolve2_expr_operator_subscript_slice(struct ast_node **expr_p);
static int
resolve2_expr_operator(struct ast_node **expr_p, int n_operands);
static int
resolve2_expr_operator_sizeof(struct ast_node **expr_p);
static int
resolve2_expr_operator_addrof(struct ast_node **expr_p);
static int
resolve2_expr_operator_filter(struct ast_node **expr_p);
static int
resolve2_expr_fcall(struct ast_node **expr_p);
static int
resolve2_expr_interpreter(struct ast_node **expr_p);
static int
resolve2_expr_as_type(struct ast_node **expr_p);
static int
resolve2_span_size(struct ast_node *node);
static int
resolve2_span_size_block(struct ast_node *node);
static int
resolve2_span_size_array(struct ast_node *node);
static int
resolve2_span_size_byte(struct ast_node *node);
static int
resolve2_span_size_byte_array(struct ast_node *node);
static int
resolve2_key_stmt(struct key_stmt *key_stmt);
static int
resolve2_link(struct link *link);
 
static int
setup_global_track_backends(void);
static int
setup_track_backends(struct ast_node *node);
static int
setup_track_backends_recur_block(struct ast_node *block);

static void
dump_error_circular_dependency_on_node_size(const struct ast_node *node);


static void
push_error_ctx(struct call_stack_error_ctx *error_ctx)
{
    error_ctx->prev = error_ctx_stack_top;
    error_ctx->next = NULL;
    if (NULL != error_ctx_stack_top) {
        error_ctx_stack_top->next = error_ctx;
    } else {
        error_ctx_stack_base = error_ctx;
    }
    error_ctx_stack_top = error_ctx;
}

static void
pop_error_ctx(void)
{
    assert(NULL != error_ctx_stack_top);
    error_ctx_stack_top = error_ctx_stack_top->prev;
    if (NULL != error_ctx_stack_top) {
        error_ctx_stack_top->next = NULL;
    } else {
        error_ctx_stack_base = NULL;
    }
}


static void
ast_node_free(struct ast_node *node)
{
    switch (node->type) {
    case AST_NODE_TYPE_IDENTIFIER:
        free(node->u.identifier);
        break ;
    case AST_NODE_TYPE_FILTER: {
        struct param *param;
        struct param *tparam;

        STAILQ_FOREACH_SAFE(
            param,
            node->u.filter.param_list,
            list, tparam) {
            free(param->name);
            free(param);
        }
        break ;
    }
    default:
        break ;
    }
    free(node);
}

static void
ast_node_clear(struct ast_node *node)
{
    memset(&node->u, 0, sizeof (node->u));
}

static struct ast_node *
lookup_type(const char *identifier,
            const struct list_of_visible_refs *visible_refs)
{
    const struct list_of_visible_refs *refs_level;
    const struct named_type_list *cur_named_types;
    struct named_type *type_def;

    /* try to find a matching type in current scope: browse lists of
     * named blocks in upward direction until a match is found */
    for (refs_level = visible_refs; NULL != refs_level;
         refs_level = refs_level->outer_refs) {
        cur_named_types = refs_level->cur_lists->named_type_list;
        STAILQ_FOREACH(type_def, cur_named_types, list) {
            if (0 == strcmp(identifier, type_def->name)) {
                return type_def->type;
            }
        }
    }
    return NULL;
}

static const char *
reverse_lookup_typename(const struct ast_node *node,
                        const struct list_of_visible_refs *visible_refs)
{
    const struct list_of_visible_refs *refs_level;
    const struct named_type_list *cur_named_types;
    struct named_type *type_def;

    for (refs_level = visible_refs; NULL != refs_level;
         refs_level = refs_level->outer_refs) {
        cur_named_types = refs_level->cur_lists->named_type_list;
        STAILQ_FOREACH(type_def, cur_named_types, list) {
            if (node == type_def->type)
                return type_def->name;
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
    case STATEMENT_TYPE_LINK:
        stmt_list = stmt_lists->link_list;
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
            if (AST_NODE_TYPE_BLOCK_DEF == field->field_type->type) {
                nstmt = find_statement_by_name(
                    STATEMENT_TYPE_FIELD, identifier,
                    &field->field_type->u.block_def.block_stmt_list);
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
lookup_link(const char *identifier,
            const struct list_of_visible_refs *visible_refs,
            const struct ast_node **blockp,
            const struct link **linkp)
{
    return lookup_statement(STATEMENT_TYPE_LINK, identifier, visible_refs,
                            blockp,
                            (const struct named_statement **)linkp);
}

int
resolve_schema_references(struct bitpunch_schema_hdl *schema)
{
    struct ast_node *ast_root;

    ast_root = schema->df_file_block.root;
    if (-1 == resolve_block_types(ast_root, NULL)) {
        return -1;
    }
    if (-1 == resolve_block_expressions(ast_root, NULL)) {
        return -1;
    }
    if (-1 == resolve2_block(ast_root)) {
        return -1;
    }
    if (-1 == setup_global_track_backends()) {
        return -1;
    }
    if (-1 == setup_track_backends(ast_root)) {
        return -1;
    }
    return 0;
}


static int
resolve_block_types(struct ast_node *block,
                    struct list_of_visible_refs *outer_refs)
{
    return resolve_stmt_lists_types(&block->u.block_def.block_stmt_list,
                                    outer_refs);
}

static int
resolve_stmt_list_types_generic(
    struct statement_list *stmt_list,
    const struct list_of_visible_refs *visible_refs)
{
    struct statement *stmt;

    TAILQ_FOREACH(stmt, stmt_list, list) {
        if (NULL != stmt->cond
            && -1 == resolve_node_types(&stmt->cond, visible_refs,
                                        (RESOLVE_EXPECT_EXPRESSION))) {
            return -1;
        }
    }
    return 0;
}

static int
resolve_stmt_lists_types(struct block_stmt_list *stmt_lists,
                         const struct list_of_visible_refs *outer_refs)
{
    struct list_of_visible_refs visible_refs;
    struct named_type *named_type;
    struct statement *stmt;
    struct field *field;
    struct ast_node *block_def;
    struct span_stmt *span_stmt;
    struct key_stmt *key_stmt;
    struct link *link;

    /* add current refs to the chain of visible refs */
    visible_refs.outer_refs = outer_refs;
    visible_refs.cur_block = NULL; /* unused */
    visible_refs.cur_lists = stmt_lists;

    STAILQ_FOREACH(named_type, stmt_lists->named_type_list, list) {
        if (-1 == check_duplicate_type_name(named_type, &visible_refs)) {
            return -1;
        }
        if (-1 == resolve_node_types(&named_type->type, &visible_refs,
                                     (RESOLVE_EXPECT_TYPENAME |
                                      RESOLVE_EXPECT_TYPEDEF))) {
            return -1;
        }
    }
    if (-1 == resolve_stmt_list_types_generic(stmt_lists->field_list,
                                              &visible_refs)) {
        return -1;
    }
    if (-1 == resolve_stmt_list_types_generic(stmt_lists->span_list,
                                              &visible_refs)) {
        return -1;
    }
    if (-1 == resolve_stmt_list_types_generic(stmt_lists->last_stmt_list,
                                              &visible_refs)) {
        return -1;
    }
    if (-1 == resolve_stmt_list_types_generic(stmt_lists->link_list,
                                              &visible_refs)) {
        return -1;
    }
    TAILQ_FOREACH(stmt, stmt_lists->field_list, list) {
        field = (struct field *)stmt;
        if (-1 == resolve_node_types(&field->field_type, &visible_refs,
                                     (RESOLVE_EXPECT_TYPENAME |
                                      RESOLVE_EXPECT_TYPEDEF))) {
            return -1;
        }
    }
    if (-1 == chain_duplicate_statements(stmt_lists)) {
        return -1;
    }
    /* resolve types of all nested blocks */
    STAILQ_FOREACH(block_def, stmt_lists->block_def_list, stmt_list) {
        if (-1 == resolve_block_types(block_def, &visible_refs)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, stmt_lists->span_list, list) {
        span_stmt = (struct span_stmt *)stmt;
        if (-1 == resolve_node_types(&span_stmt->span_expr, &visible_refs,
                                     RESOLVE_EXPECT_EXPRESSION)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, stmt_lists->key_list, list) {
        key_stmt = (struct key_stmt *)stmt;
        if (-1 == resolve_node_types(&key_stmt->key_expr, &visible_refs,
                                     RESOLVE_EXPECT_EXPRESSION)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, stmt_lists->link_list, list) {
        link = (struct link *)stmt;
        if (NULL != link->dst_expr
            && -1 == resolve_node_types(&link->dst_expr, &visible_refs,
                                        RESOLVE_EXPECT_EXPRESSION)) {
            return -1;
        }
    }
    return 0;
}

static int
check_duplicate_type_name(const struct named_type *named_type,
                          const struct list_of_visible_refs *visible_refs)
{
    const struct list_of_visible_refs *refs_level;
    const struct named_type_list *cur_named_types;
    struct named_type *iter;

    for (refs_level = visible_refs; NULL != refs_level;
         refs_level = refs_level->outer_refs) {
        cur_named_types = refs_level->cur_lists->named_type_list;
        STAILQ_FOREACH(iter, cur_named_types, list) {
            if (refs_level == visible_refs && iter == named_type) {
                /* suceeding duplicates will be catched by next function
                 * calls */
                break ;
            }
            if (0 == strcmp(iter->name, named_type->name)) {
                semantic_error(SEMANTIC_LOGLEVEL_ERROR, &named_type->loc,
                               "duplicate type \"%s\"",
                               named_type->name);
                semantic_error(SEMANTIC_LOGLEVEL_INFO, &iter->loc,
                               "already declared here");
                return -1;
            }
        }
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
            if (AST_NODE_TYPE_BLOCK_DEF == field->field_type->type) {
                nstmt = find_unchained_statement_by_name(
                    field->field_type
                    ->u.block_def.block_stmt_list.field_list,
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
            if (AST_NODE_TYPE_BLOCK_DEF == field->field_type->type) {
                if (-1 == chain_duplicate_statements_in(
                        field->field_type
                        ->u.block_def.block_stmt_list.field_list,
                        toplevel_list)) {
                    return -1;
                }
            }
        } else {
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
    if (-1 == chain_duplicate_statements_in(stmt_lists->link_list,
                                            stmt_lists->link_list)) {
        return -1;
    }
    return 0;
}

static int
resolve_node_types(struct ast_node **node_p,
                   const struct list_of_visible_refs *visible_refs,
                   enum resolve_expect_node_mask expect_mask)
{
    struct ast_node *resolved_type;

    if (0 != ((*node_p)->flags & ASTFLAG_PROCESSING)) {
        return 0;
    }
    (*node_p)->flags |= ASTFLAG_PROCESSING;
    if (-1 == resolve_node_types_int(*node_p, visible_refs, expect_mask,
                                     &resolved_type)) {
        (*node_p)->flags &= ~ASTFLAG_PROCESSING;
        return -1;
    }
    if (NULL == resolved_type) {
        /* nothing to resolve, keep node as-is */
        (*node_p)->flags &= ~ASTFLAG_PROCESSING;
        return 0;
    }
    ast_node_free(*node_p);
    while (NULL != resolved_type) {
        /* There might be types resolving to other non-native
         * types. That's why we must do a resolve loop until we find
         * the native type that the source type refers to. */
        assert(*node_p != resolved_type);
        (*node_p) = resolved_type;
        if (-1 == resolve_node_types_int(*node_p, visible_refs,
                                         expect_mask, &resolved_type)) {
            (*node_p)->flags &= ~ASTFLAG_PROCESSING;
            return -1;
        }
    }
    (*node_p)->flags &= ~ASTFLAG_PROCESSING;
    return 0;
}

static int
resolve_node_types_int2(struct ast_node *node,
                       const struct list_of_visible_refs *visible_refs,
                       enum resolve_expect_node_mask expect_mask,
                       struct ast_node **resolved_typep)
{
    switch (node->type) {
    case AST_NODE_TYPE_IDENTIFIER:
        return resolve_node_types_identifier(node, visible_refs,
                                             expect_mask, resolved_typep);
    case AST_NODE_TYPE_TYPENAME:
        return resolve_node_types_typename(node, visible_refs,
                                           expect_mask, resolved_typep);
    case AST_NODE_TYPE_ARRAY:
        return resolve_node_types_array(node, visible_refs, resolved_typep);
    case AST_NODE_TYPE_BYTE_ARRAY:
        return resolve_node_types_byte_array(node, visible_refs,
                                             resolved_typep);
    case AST_NODE_TYPE_CONDITIONAL:
        return resolve_node_types_conditional(node, visible_refs,
                                              resolved_typep);
    case AST_NODE_TYPE_FILTER:
        *resolved_typep = NULL;
        return resolve_node_types_filter(node, visible_refs, expect_mask);
    /* for all operators: resolve potential type names in operand
     * sub-expressions (e.g. sizeof) */
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
    case AST_NODE_TYPE_OP_SET_FILTER:
        return resolve_node_types_operator(node, 2, visible_refs,
                                           RESOLVE_EXPECT_EXPRESSION,
                                           resolved_typep);
    case AST_NODE_TYPE_OP_UPLUS:
    case AST_NODE_TYPE_OP_UMINUS:
    case AST_NODE_TYPE_OP_LNOT:
    case AST_NODE_TYPE_OP_BWNOT:
    case AST_NODE_TYPE_OP_ADDROF:
    case AST_NODE_TYPE_OP_MEMBER:
    case AST_NODE_TYPE_OP_FILTER:
        return resolve_node_types_operator(node, 1, visible_refs,
                                           RESOLVE_EXPECT_EXPRESSION,
                                           resolved_typep);
    case AST_NODE_TYPE_OP_SUBSCRIPT:
        return resolve_node_types_op_subscript(node, visible_refs,
                                               resolved_typep);
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
        return resolve_node_types_op_subscript_slice(node, visible_refs,
                                                     resolved_typep);
    case AST_NODE_TYPE_OP_FCALL:
        return resolve_node_types_op_fcall(node, visible_refs,
                                           resolved_typep);
    case AST_NODE_TYPE_OP_SIZEOF:
        return resolve_node_types_operator(node, 1, visible_refs,
                                           (RESOLVE_EXPECT_TYPENAME |
                                            RESOLVE_EXPECT_EXPRESSION),
                                           resolved_typep);
    default:
        /* nothing to resolve */
        *resolved_typep = NULL;
        return 0;
    }
    /*NOT REACHED*/
}

static int
resolve_node_types_int(struct ast_node *node,
                       const struct list_of_visible_refs *visible_refs,
                       enum resolve_expect_node_mask expect_mask,
                       struct ast_node **resolved_typep)
{
    int ret;

    ret = resolve_node_types_int2(node, visible_refs, expect_mask,
                                  resolved_typep);
    if (0 == ret && ast_node_is_item(node)
        && NULL != node->u.item.filter) {
        ret = resolve_node_types(&node->u.item.filter,
                                 visible_refs, expect_mask);
    }
    return ret;
}

static int
resolve_node_types_identifier(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_node_mask expect_mask,
    struct ast_node **resolved_typep)
{
    if (0 != (expect_mask & RESOLVE_EXPECT_TYPENAME)) {
        struct ast_node *resolved_type;

        if (0 == strcmp(node->u.identifier, "byte")) {
            /* native 'byte' type */
            node->type = AST_NODE_TYPE_BYTE;
            node->u.item.min_span_size = 1;
            *resolved_typep = NULL;
            return 0;
        }
        /* try to match identifier with an existing type */
        resolved_type = lookup_type(node->u.identifier, visible_refs);
        if (NULL != resolved_type) {
            *resolved_typep = resolved_type;
            return 0;
        }
    }
    if (!(expect_mask & RESOLVE_EXPECT_EXPRESSION)) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &node->loc,
            "no type named '%s' exists in the scope",
            node->u.identifier);
        return -1;
    }
    // expression types will be resolved in 2nd pass
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_node_types_typename(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_node_mask expect_mask,
    struct ast_node **resolved_typep)
{
    struct ast_node *resolved_type;

    if (0 != (expect_mask & RESOLVE_EXPECT_TYPENAME)) {
        if (0 == strcmp(node->u.type_name.name, "byte")) {
            /* native 'byte' type */
            node->type = AST_NODE_TYPE_BYTE;
            node->u.item.min_span_size = 1;
            *resolved_typep = NULL;
            return 0;
        }
        /* try to match identifier with an existing type */
        resolved_type = lookup_type(node->u.type_name.name, visible_refs);
        if (NULL != resolved_type) {
            *resolved_typep = resolved_type;
            return 0;
        }
    }
    if (0 != (expect_mask & RESOLVE_EXPECT_INTERPRETER)) {
        const struct interpreter *interpreter;

        interpreter = interpreter_lookup(node->u.type_name.name);
        if (NULL != interpreter) {
            *resolved_typep = NULL;
            return 0;
        }
    }
    semantic_error(
        SEMANTIC_LOGLEVEL_ERROR, &node->loc,
        "no type named '%s' exists in the scope",
        node->u.type_name.name);
    return -1;
}

static int
resolve_node_types_array(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    struct ast_node **resolved_typep)
{
    /* resolve potential types of array element type */
    if (-1 == resolve_node_types(
            &node->u.array.value_type, visible_refs,
            (RESOLVE_EXPECT_TYPENAME |
             RESOLVE_EXPECT_TYPEDEF))) {
        return -1;
    }
    if (NULL != node->u.array.value_count &&
        -1 == resolve_node_types(
            &node->u.array.value_count, visible_refs,
            RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    if (AST_NODE_TYPE_BYTE == node->u.array.value_type->type
        && NULL == node->u.array.value_type->u.item.filter) {
        struct ast_node *byte_count = node->u.array.value_count;

        node->type = AST_NODE_TYPE_BYTE_ARRAY;
        node->u.byte_array.size = byte_count;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_node_types_byte_array(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    struct ast_node **resolved_typep)
{
    if (NULL != node->u.byte_array.size &&
        -1 == resolve_node_types(
            &node->u.byte_array.size, visible_refs,
            RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_node_types_conditional(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    struct ast_node **resolved_typep)
{
    struct ast_node *outer_type;

    if (NULL != node->u.conditional.outer_cond
        && -1 == resolve_node_types_conditional(
            node->u.conditional.outer_cond, visible_refs,
            &outer_type)) {
        return -1;
    }
    if (-1 == resolve_node_types(&node->u.conditional.cond_expr,
                                 visible_refs,
                                 RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_node_types_op_subscript(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    struct ast_node **resolved_typep)
{
    if (-1 == resolve_node_types(&node->u.op_subscript_common.anchor_expr,
                                 visible_refs,
                                 RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    if (-1 == resolve_node_types(&node->u.op_subscript.index.key,
                                 visible_refs,
                                 RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    if (NULL != node->u.op_subscript.index.twin
        && -1 == resolve_node_types(&node->u.op_subscript.index.twin,
                                    visible_refs,
                                    RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_node_types_op_subscript_slice(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    struct ast_node **resolved_typep)
{
    if (-1 == resolve_node_types(&node->u.op_subscript_common.anchor_expr,
                                 visible_refs,
                                 RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    if (NULL != node->u.op_subscript_slice.start.key
        && -1 == resolve_node_types(&node->u.op_subscript_slice.start.key,
                                    visible_refs,
                                    RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    if (NULL != node->u.op_subscript_slice.start.twin
        && -1 == resolve_node_types(&node->u.op_subscript_slice.start.twin,
                                    visible_refs,
                                    RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    if (NULL != node->u.op_subscript_slice.end.key
        && -1 == resolve_node_types(&node->u.op_subscript_slice.end.key,
                                    visible_refs,
                                    RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    if (NULL != node->u.op_subscript_slice.end.twin
        && -1 == resolve_node_types(&node->u.op_subscript_slice.end.twin,
                                    visible_refs,
                                    RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_node_types_op_fcall(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    struct ast_node **resolved_typep)
{
    struct func_param *param;

    if (-1 == resolve_node_types(&node->u.op_fcall.func, visible_refs,
                                 RESOLVE_EXPECT_EXPRESSION)) {
        return -1;
    }
    for (param = STAILQ_FIRST(node->u.op_fcall.func_params);
         NULL != param;
         param = STAILQ_NEXT(param, list)) {
        if (-1 == resolve_node_types(&param->expr, visible_refs,
                                     RESOLVE_EXPECT_EXPRESSION)) {
            return -1;
        }
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_node_types_operator(
    struct ast_node *node,
    int n_operands,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_node_mask operand_mask,
    struct ast_node **resolved_typep)
{
    int opd_i;

    for (opd_i = 0; opd_i < n_operands; ++opd_i) {
        if (-1 == resolve_node_types(
                &node->u.op.operands[opd_i], visible_refs, operand_mask)) {
            return -1;
        }
    }
    *resolved_typep = NULL;
    return 0;
}

static int
resolve_node_types_filter(
    struct ast_node *node,
    const struct list_of_visible_refs *visible_refs,
    enum resolve_expect_node_mask expect_mask)
{
    assert(AST_NODE_TYPE_FILTER == node->type);
    assert(NULL != node->u.filter.target);
    if (-1 == resolve_node_types(&node->u.filter.target, visible_refs,
                                 expect_mask)) {
        return -1;
    }
    if (-1 == resolve_node_types(&node->u.filter.filter_type,
                                 visible_refs,
                                 (RESOLVE_EXPECT_TYPENAME |
                                  RESOLVE_EXPECT_INTERPRETER))) {
        return -1;
    }
    return 0;
}


/*
 * resolve expressions
 */

static int
resolve_block_expressions(struct ast_node *block,
                          struct list_of_visible_refs *outer_refs)
{
    return resolve_stmt_lists_expressions(block,
                                          &block->u.block_def.block_stmt_list,
                                          outer_refs);
}

static int
resolve_stmt_list_expressions_generic(
    struct statement_list *stmt_list,
    struct list_of_visible_refs *visible_refs)
{
    struct statement *stmt;

    TAILQ_FOREACH(stmt, stmt_list, list) {
        if (NULL != stmt->cond
            && -1 == resolve_dtype_expressions(&stmt->cond, visible_refs)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve_stmt_lists_expressions(const struct ast_node *block,
                               struct block_stmt_list *stmt_lists,
                               struct list_of_visible_refs *outer_refs)
{
    struct list_of_visible_refs visible_refs;
    struct named_type *named_type;
    struct statement *stmt;
    struct field *field;
    struct ast_node *block_def;
    struct span_stmt *span_stmt;
    struct key_stmt *key_stmt;
    struct match *match;
    struct link *link;

    /* add current refs to the chain of visible refs */
    visible_refs.outer_refs = outer_refs;
    visible_refs.cur_block = block;
    visible_refs.cur_lists = stmt_lists;

    STAILQ_FOREACH(named_type, stmt_lists->named_type_list, list) {
        if (-1 == resolve_dtype_expressions(&named_type->type,
                                            &visible_refs)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, stmt_lists->field_list, list) {
        field = (struct field *)stmt;
        if (-1 == resolve_dtype_expressions(&field->field_type,
                                            &visible_refs)) {
            return -1;
        }
    }
    /* resolve types of all nested blocks */
    STAILQ_FOREACH(block_def, stmt_lists->block_def_list, stmt_list) {
        if (-1 == resolve_block_expressions(block_def, &visible_refs)) {
            return -1;
        }
    }
    if (-1 == resolve_stmt_list_expressions_generic(
            stmt_lists->field_list, &visible_refs)) {
        return -1;
    }
    if (-1 == resolve_stmt_list_expressions_generic(
            stmt_lists->span_list, &visible_refs)) {
        return -1;
    }
    if (-1 == resolve_stmt_list_expressions_generic(
            stmt_lists->last_stmt_list, &visible_refs)) {
        return -1;
    }
    if (-1 == resolve_stmt_list_expressions_generic(
            stmt_lists->link_list, &visible_refs)) {
        return -1;
    }

    // resolve expressions
    TAILQ_FOREACH(stmt, stmt_lists->match_list, list) {
        match = (struct match *)stmt;
        if (-1 == resolve_expr(&match->expr, &visible_refs)) {
            return -1;
        }
        assert(ast_node_is_rexpr(match->expr));
        if (match->expr->u.rexpr.value_type != EXPR_VALUE_TYPE_BOOLEAN) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &match->expr->loc,
                           "match expects a boolean expression, not "
                           "'%s'",
                           expr_value_type_str(
                               match->expr->u.rexpr.value_type));
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, stmt_lists->span_list, list) {
        span_stmt = (struct span_stmt *)stmt;
        if (-1 == resolve_span_expr(&span_stmt->span_expr,
                                    &visible_refs)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, stmt_lists->key_list, list) {
        key_stmt = (struct key_stmt *)stmt;
        if (-1 == resolve_expr(&key_stmt->key_expr, &visible_refs)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, stmt_lists->link_list, list) {
        link = (struct link *)stmt;
        if (-1 == resolve_link(link, &visible_refs)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve_dtype_expressions(struct ast_node **dtype_p,
                          struct list_of_visible_refs *visible_refs)
{
    int ret;

    if (0 != ((*dtype_p)->flags & ASTFLAG_PROCESSING)) {
        return 0;
    }
    (*dtype_p)->flags |= ASTFLAG_PROCESSING;
    switch ((*dtype_p)->type) {
    case AST_NODE_TYPE_ARRAY:
        ret = resolve_dtype_expressions_array(dtype_p, visible_refs);
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
        ret = resolve_dtype_expressions_byte_array(dtype_p, visible_refs);
        break ;
    case AST_NODE_TYPE_CONDITIONAL:
        ret = resolve_dtype_expressions_conditional(dtype_p, visible_refs);
        break ;
    default:
        /* nothing to resolve */
        ret = 0;
        break ;
    }
    if (-1 == ret) {
        (*dtype_p)->flags &= ~ASTFLAG_PROCESSING;
        return -1;
    }
    if (ast_node_is_item(*dtype_p)
        && NULL != (*dtype_p)->u.item.filter
        && -1 == resolve_expr(&(*dtype_p)->u.item.filter,
                              visible_refs)) {
        (*dtype_p)->flags &= ~ASTFLAG_PROCESSING;
        return -1;
    }
    (*dtype_p)->flags &= ~ASTFLAG_PROCESSING;
    return 0;
}

static int
resolve_dtype_expressions_array(struct ast_node **dtype_p,
                                struct list_of_visible_refs *visible_refs)
{
    /* resolve array type and count expression, if defined */
    if (-1 == resolve_dtype_expressions(&(*dtype_p)->u.array.value_type,
                                        visible_refs)) {
        return -1;
    }
    if (NULL != (*dtype_p)->u.array.value_count &&
        -1 == resolve_expr(
            &(*dtype_p)->u.array.value_count, visible_refs)) {
        return -1;
    }
    return 0;
}

static int
resolve_dtype_expressions_byte_array(struct ast_node **dtype_p,
                                     struct list_of_visible_refs *visible_refs)
{
    /* resolve array count expression, if defined */
    if (NULL != (*dtype_p)->u.byte_array.size &&
        -1 == resolve_expr(
            &(*dtype_p)->u.byte_array.size, visible_refs)) {
        return -1;
    }
    return 0;
}

static int
resolve_dtype_expressions_conditional(struct ast_node **dtype_p,
                                      struct list_of_visible_refs *visible_refs)
{
    struct ast_node *cond;

    if (NULL != (*dtype_p)->u.conditional.outer_cond
        && -1 == resolve_dtype_expressions_conditional(
            &(*dtype_p)->u.conditional.outer_cond, visible_refs)) {
        return -1;
    }
    if (-1 == resolve_expr(&(*dtype_p)->u.conditional.cond_expr,
                           visible_refs)) {
        return -1;
    }
    cond = (*dtype_p)->u.conditional.cond_expr;
    assert(ast_node_is_rexpr(cond));
    if (EXPR_VALUE_TYPE_BOOLEAN != cond->u.rexpr.value_type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &cond->loc,
            "expect a boolean expression in condition, not '%s'",
            expr_value_type_str(cond->u.rexpr.value_type));
        return -1;
    }
    return 0;
}

static int
resolve_expr(struct ast_node **expr_p,
             struct list_of_visible_refs *visible_refs)
{
    int ret;

    if (0 != ((*expr_p)->flags & ASTFLAG_PROCESSING)) {
        return 0;
    }
    (*expr_p)->flags |= ASTFLAG_PROCESSING;
    ret = 0;
    switch ((*expr_p)->type) {
    case AST_NODE_TYPE_NONE:
        break ;
    case AST_NODE_TYPE_INTEGER:
        resolve_expr_integer(expr_p);
        break ;
    case AST_NODE_TYPE_BOOLEAN:
        resolve_expr_boolean(expr_p);
        break ;
    case AST_NODE_TYPE_STRING:
        resolve_expr_value_string(expr_p);
        break ;
    case AST_NODE_TYPE_IDENTIFIER:
        ret = resolve_expr_identifier(expr_p, visible_refs);
        break ;
    case AST_NODE_TYPE_FILTER:
        ret = resolve_expr_filter(expr_p, visible_refs);
        break ;
    case AST_NODE_TYPE_OP_UPLUS:
    case AST_NODE_TYPE_OP_UMINUS:
    case AST_NODE_TYPE_OP_LNOT:
    case AST_NODE_TYPE_OP_BWNOT:
        ret = resolve_expr_operator(expr_p, 1, visible_refs);
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
        ret = resolve_expr_operator(expr_p, 2, visible_refs);
        break ;
    case AST_NODE_TYPE_OP_SUBSCRIPT:
        ret = resolve_expr_subscript(expr_p, visible_refs);
        break ;
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
        ret = resolve_expr_subscript_slice(expr_p, visible_refs);
        break ;
    case AST_NODE_TYPE_OP_MEMBER:
        ret = resolve_expr_op_member(expr_p, visible_refs);
        break ;
    case AST_NODE_TYPE_OP_FCALL:
        ret = resolve_expr_fcall(expr_p, visible_refs);
        break ;
    case AST_NODE_TYPE_OP_SET_FILTER:
        ret = resolve_expr_operator_set_filter(expr_p, visible_refs);
        break ;
    case AST_NODE_TYPE_OP_SIZEOF:
        /* first resolve pass */
        ret = resolve_expr_operator_sizeof(expr_p, visible_refs);
        break ;
    case AST_NODE_TYPE_OP_ADDROF:
        /* first resolve pass */
        ret = resolve_expr_operator_addrof(expr_p, visible_refs);
        break ;
    case AST_NODE_TYPE_OP_FILTER:
        /* first resolve pass */
        ret = resolve_expr_operator_filter(expr_p, visible_refs);
        break ;
    case AST_NODE_TYPE_EXPR_FILE:
        ret = resolve_expr_file(expr_p, visible_refs);
        break ;
    case AST_NODE_TYPE_EXPR_SELF:
        ret = resolve_expr_self(expr_p, visible_refs);
        break ;
    case AST_NODE_TYPE_EXPR_STAR_WILDCARD:
        ret = resolve_expr_star_wildcard(expr_p, visible_refs);
        break ;
    default:
        /* nothing to do */
        break ;
    }
    (*expr_p)->flags &= ~ASTFLAG_PROCESSING;
    return ret;
}

static void
resolve_expr_integer(struct ast_node **expr_p)
{
    int64_t integer;

    integer = (*expr_p)->u.integer;
    ast_node_clear(*expr_p);
    (*expr_p)->type = AST_NODE_TYPE_REXPR_NATIVE;
    (*expr_p)->u.rexpr.value_type = EXPR_VALUE_TYPE_INTEGER;
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    (*expr_p)->u.rexpr_native.value.integer = integer;
}

static void
resolve_expr_boolean(struct ast_node **expr_p)
{
    int boolean;

    boolean = (*expr_p)->u.boolean;
    ast_node_clear(*expr_p);
    (*expr_p)->type = AST_NODE_TYPE_REXPR_NATIVE;
    (*expr_p)->u.rexpr.value_type = EXPR_VALUE_TYPE_BOOLEAN;
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    (*expr_p)->u.rexpr_native.value.boolean = boolean;
}

static void
resolve_expr_value_string(struct ast_node **expr_p)
{
    struct expr_value_string string;

    string = (*expr_p)->u.string;
    ast_node_clear(*expr_p);
    (*expr_p)->type = AST_NODE_TYPE_REXPR_NATIVE;
    (*expr_p)->u.rexpr.value_type = EXPR_VALUE_TYPE_STRING;
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    (*expr_p)->u.rexpr_native.value.string = string;
}

static enum expr_value_type
expr_value_type_from_node(const struct ast_node *node)
{
    if (ast_node_is_rexpr(node)) {
        return node->u.rexpr.value_type;
    }
    if (ast_node_is_item(node) && NULL != node->u.item.filter) {
        return expr_value_type_from_node(node->u.item.filter);
    }
    return EXPR_VALUE_TYPE_UNSET;
}

static void
set_node_as_rexpr_field(struct ast_node **expr_p,
                         const struct ast_node *block,
                         const struct field *field,
                         struct ast_node *anchor_expr)
{
    struct ast_node *target_item;

    target_item = field->field_type;
    (*expr_p)->type = AST_NODE_TYPE_REXPR_FIELD;
    (*expr_p)->u.rexpr.value_type = expr_value_type_from_node(target_item);
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_ITEM;
    (*expr_p)->u.rexpr.target_item = target_item;
    (*expr_p)->u.rexpr_field.block = block;
    (*expr_p)->u.rexpr_field.field = field;
    (*expr_p)->u.rexpr_field.anchor_expr = anchor_expr;
}

static void
set_node_as_rexpr_link(struct ast_node **expr_p,
                       const struct ast_node *block,
                       const struct link *link,
                       struct ast_node *anchor_expr)
{
    (*expr_p)->type = AST_NODE_TYPE_REXPR_MEMBER;
    // dpath type will be set to the link's dest type at resolve2 phase
    (*expr_p)->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_UNSET;
    (*expr_p)->u.rexpr_link.block = block;
    (*expr_p)->u.rexpr_link.link = link;
    (*expr_p)->u.rexpr.target_item =
        link->dst_expr->u.rexpr.target_item;
    (*expr_p)->u.rexpr_link.anchor_expr = anchor_expr;
}

static void
set_node_as_rexpr_builtin(struct ast_node **expr_p,
                          const struct expr_builtin_fn *builtin,
                          struct ast_node *anchor_expr)
{
    (*expr_p)->type = AST_NODE_TYPE_REXPR_BUILTIN;
    (*expr_p)->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_UNSET;
    (*expr_p)->u.rexpr_builtin.builtin = builtin;
    (*expr_p)->u.rexpr_builtin.anchor_expr = anchor_expr;
}

static int
resolve_expr_identifier(struct ast_node **expr_p,
                        struct list_of_visible_refs *visible_refs)
{
    const struct ast_node *resolved_block;

    /* try to match identifier with an existing field */
    if ((*expr_p)->u.identifier[0] == '?') {
        const struct link *resolved_link;

        if (-1 == lookup_link((*expr_p)->u.identifier + 1, visible_refs,
                              &resolved_block, &resolved_link)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
                "no member named '%s' exists in the expression scope",
                (*expr_p)->u.identifier);
            return -1;
        }
        free((*expr_p)->u.identifier);
        set_node_as_rexpr_link(expr_p,
                               resolved_block, resolved_link, NULL);
    } else {
        const struct field *resolved_field;
        const struct expr_builtin_fn *builtin;

        if (-1 != lookup_field((*expr_p)->u.identifier, visible_refs,
                               &resolved_block, &resolved_field)) {
            free((*expr_p)->u.identifier);
            set_node_as_rexpr_field(expr_p,
                                    resolved_block, resolved_field, NULL);
            return 0;
        }
        builtin = expr_lookup_builtin_fn((*expr_p)->u.identifier, NULL);
        if (NULL != builtin) {
            free((*expr_p)->u.identifier);
            set_node_as_rexpr_builtin(expr_p, builtin, NULL);
            return 0;
        }
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
            "no field or builtin named '%s' exists in the expression scope",
            (*expr_p)->u.identifier);
        return -1;
    }
    return 0;
}

static int
resolve_expr_filter(struct ast_node **expr_p,
                    struct list_of_visible_refs *visible_refs)
{
    struct ast_node *node;
    struct ast_node *target;
    struct ast_node *filter_type;
    struct interpreter *interpreter;
    struct ast_node *resolved_type;

    node = *expr_p;
    assert(AST_NODE_TYPE_FILTER == node->type);
    target = node->u.filter.target;
    assert(NULL != target);
    if (AST_NODE_TYPE_FILTER == target->type
        && -1 == resolve_expr_filter(&target, visible_refs)) {
        return -1;
    }
    filter_type = node->u.filter.filter_type;
    if (AST_NODE_TYPE_TYPENAME == filter_type->type) {
        interpreter = interpreter_lookup(filter_type->u.type_name.name);
        assert(NULL != interpreter); // already checked earlier

        resolved_type = interpreter_rcall_build(interpreter, node);
        if (NULL == resolved_type) {
            return -1;
        }
        assert(resolved_type->type == AST_NODE_TYPE_REXPR_INTERPRETER);
        resolved_type->loc = node->loc;
        resolved_type->u.rexpr_filter.target = target;
        ast_node_free(node);
        *expr_p = resolved_type;
        return 0;
    }
    if (-1 == resolve_dtype_expressions(&filter_type, visible_refs)) {
        return -1;
    }
    node->type = AST_NODE_TYPE_REXPR_AS_TYPE;
    if (ast_node_is_item(target)) {
        node->u.rexpr.target_item = target;
    }
    node->u.rexpr_filter.target = target;
    node->u.rexpr_as_type.as_type = filter_type;
    node->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    node->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    return 0;
}

static int
resolve_expr_operator(struct ast_node **expr_p,
                      int n_operands,
                      struct list_of_visible_refs *visible_refs)
{
    int opd_i;
    struct ast_node *operand;
    enum expr_value_type opd_types[2]; /* max # operands is 2 */
    const struct expr_evaluator *evaluator;
    struct op op;
    enum ast_node_type rexpr_node_type;

    assert(n_operands <= 2);
    /* resolve operand expressions */
    for (opd_i = 0; opd_i < n_operands; ++opd_i) {
        if (-1 == resolve_expr(&(*expr_p)->u.op.operands[opd_i],
                               visible_refs)) {
            return -1;
        }
        operand = (*expr_p)->u.op.operands[opd_i];
        opd_types[opd_i] = operand->u.rexpr.value_type;
        if (EXPR_VALUE_TYPE_UNSET == opd_types[opd_i]) {
            const struct ast_node *target_item;
            const char *operand_typename;

            target_item = operand->u.rexpr.target_item;
            if (NULL != target_item) {
                operand_typename = reverse_lookup_typename(target_item,
                                                           visible_refs);
            } else {
                target_item = operand;
                operand_typename = NULL;
            }
            semantic_error(
                    SEMANTIC_LOGLEVEL_ERROR, &operand->loc,
                    "cannot use '%s %s' in expression",
                    ast_node_type_str(target_item->type),
                    (NULL != operand_typename ?
                     operand_typename : "(unnamed)"));
            return -1;
        }
    }
    evaluator = expr_lookup_evaluator((*expr_p)->type, opd_types);
    if (NULL == evaluator) {
        switch (n_operands) {
        case 1:
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
                "no match for %s with operand of type '%s'",
                ast_node_type_str((*expr_p)->type),
                expr_value_type_str(opd_types[0]));
            break ;
        case 2:
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
                "no match for %s with operands of type '%s' and '%s'",
                ast_node_type_str((*expr_p)->type),
                expr_value_type_str(opd_types[0]),
                expr_value_type_str(opd_types[1]));
            break ;
        default:
            break ;
        }
        return -1;
    }
    op = (*expr_p)->u.op;
    rexpr_node_type = op_type_ast2rexpr((*expr_p)->type);
    ast_node_clear(*expr_p);
    (*expr_p)->type = rexpr_node_type;
    (*expr_p)->u.rexpr.value_type = evaluator->res_type;
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    (*expr_p)->u.rexpr_op.op = op;
    (*expr_p)->u.rexpr_op.evaluator = evaluator;
    return 0;
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
resolve_expr_operator_sizeof(struct ast_node **expr_p,
                             struct list_of_visible_refs *visible_refs)
{
    struct op op;

    if (-1 == resolve_expr(&(*expr_p)->u.op.operands[0], visible_refs)) {
        return -1;
    }
    if (-1 == resolve2_expr(&(*expr_p)->u.op.operands[0])) {
        return -1;
    }
    op = (*expr_p)->u.op;
    switch (op.operands[0]->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
        break ;
    default:
        if (!ast_node_is_rexpr(op.operands[0])
            || EXPR_DPATH_TYPE_NONE == op.operands[0]->u.rexpr.dpath_type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
                "invalid use of sizeof operator on operand of type '%s'",
                ast_node_type_str(op.operands[0]->type));
            return -1;
        }
        break ;
    }
    ast_node_clear(*expr_p);
    (*expr_p)->type = AST_NODE_TYPE_REXPR_OP_SIZEOF;
    (*expr_p)->u.rexpr.value_type = EXPR_VALUE_TYPE_INTEGER;
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    (*expr_p)->u.rexpr_op.op = op;
    /* sizeof has specific expression evaluator */
    (*expr_p)->u.rexpr_op.evaluator = NULL;
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
resolve_expr_operator_addrof(struct ast_node **expr_p,
                             struct list_of_visible_refs *visible_refs)
{
    struct op op;

    if (-1 == resolve_expr(&(*expr_p)->u.op.operands[0], visible_refs)) {
        return -1;
    }
    if (-1 == resolve2_expr(&(*expr_p)->u.op.operands[0])) {
        return -1;
    }
    op = (*expr_p)->u.op;
    if (EXPR_DPATH_TYPE_NONE == op.operands[0]->u.rexpr.dpath_type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
            "invalid use of addrof (&) operator on non-dpath node of type "
            "'%s'",
            ast_node_type_str(op.operands[0]->type));
        return -1;
    }
    ast_node_clear(*expr_p);
    (*expr_p)->type = AST_NODE_TYPE_REXPR_OP_ADDROF;
    (*expr_p)->u.rexpr.value_type = EXPR_VALUE_TYPE_INTEGER;
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
    (*expr_p)->u.rexpr_op.op = op;
    /* addrof has specific expression evaluator */
    (*expr_p)->u.rexpr_op.evaluator = NULL;
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
resolve_expr_operator_filter(struct ast_node **expr_p,
                             struct list_of_visible_refs *visible_refs)
{
    struct op op;
    struct ast_node *target;
    const struct interpreter *interpreter;

    if (-1 == resolve_expr(&(*expr_p)->u.op.operands[0], visible_refs)) {
        return -1;
    }
    if (-1 == resolve2_expr(&(*expr_p)->u.op.operands[0])) {
        return -1;
    }
    op = (*expr_p)->u.op;
    target = op.operands[0]->u.rexpr.target_item;
    if (NULL == target
        || EXPR_DPATH_TYPE_NONE == op.operands[0]->u.rexpr.dpath_type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
            "invalid use of filter (*) operator on non-dpath node of type "
            "'%s'",
            ast_node_type_str(op.operands[0]->type));
        return -1;
    }
    assert(ast_node_is_item(target));
    if (!ast_node_has_interpreter(target)
        || AST_NODE_TYPE_REXPR_OP_FILTER == op.operands[0]->type
        || AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE == op.operands[0]->type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
            "filter (*) operand has no attached interpreter");
        return -1;
    }
    interpreter =
        target->u.item.filter->u.rexpr_interpreter.interpreter;
    ast_node_clear(*expr_p);
    (*expr_p)->type = AST_NODE_TYPE_REXPR_OP_FILTER;
    (*expr_p)->u.rexpr.value_type = interpreter->semantic_type;
    (*expr_p)->u.rexpr.dpath_type =
        (EXPR_VALUE_TYPE_BYTES == interpreter->semantic_type ?
         EXPR_DPATH_TYPE_CONTAINER : EXPR_DPATH_TYPE_NONE);
    (*expr_p)->u.rexpr.target_item = target;
    (*expr_p)->u.rexpr_op.op = op;
    /* filter has specific expression evaluator */
    (*expr_p)->u.rexpr_op.evaluator = NULL;
    return 0;
}

static int
resolve_expr_fcall(struct ast_node **expr_p,
                   struct list_of_visible_refs *visible_refs)
{
    const struct expr_builtin_fn *builtin;
    struct ast_node *object;
    struct func_param *param;
    int n_params;
    struct func_param_list *func_params;

    if (-1 == resolve_expr(&(*expr_p)->u.op_fcall.func, visible_refs)) {
        return -1;
    }
    if (AST_NODE_TYPE_REXPR_BUILTIN != (*expr_p)->u.op_fcall.func->type) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
            "invalid use of () operator: '%s' not callable",
            ast_node_type_str((*expr_p)->u.op_fcall.func->type));
        return -1;
    }
    builtin = (*expr_p)->u.op_fcall.func->u.rexpr_builtin.builtin;
    object = (*expr_p)->u.op_fcall.func->u.rexpr_builtin.anchor_expr;
    /* resolve expressions in parameter list */
    n_params = 0;
    for (param = STAILQ_FIRST((*expr_p)->u.op_fcall.func_params);
         NULL != param;
         param = STAILQ_NEXT(param, list)) {
        if (-1 == resolve_expr(&param->expr, visible_refs)) {
            return -1;
        }
        ++n_params;
    }
    if (n_params < builtin->min_n_params) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
            "not enough parameters for built-in function '%s': "
            "expects %s%d, got %d",
            builtin->builtin_name,
            builtin->max_n_params > builtin->min_n_params ? "at least " : "",
            builtin->min_n_params, n_params);
        return -1;
    }
    if (n_params > builtin->max_n_params) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
            "too many parameters for built-in function '%s': "
            "expects %s%d, got %d",
            builtin->builtin_name,
            builtin->max_n_params > builtin->min_n_params ? "at most " : "",
            builtin->max_n_params, n_params);
        return -1;
    }

    func_params = (*expr_p)->u.op_fcall.func_params;
    ast_node_clear(*expr_p);
    (*expr_p)->type = AST_NODE_TYPE_REXPR_OP_FCALL;
    (*expr_p)->u.rexpr.value_type = builtin->res_value_type;
    (*expr_p)->u.rexpr.dpath_type = builtin->res_dpath_type;
    (*expr_p)->u.rexpr_op_fcall.builtin = builtin;
    (*expr_p)->u.rexpr_op_fcall.object = object;
    (*expr_p)->u.rexpr_op_fcall.func_params = func_params;
    (*expr_p)->u.rexpr_op_fcall.n_func_params = n_params;
    return 0;
}

static int
resolve_expr_operator_set_filter(struct ast_node **expr_p,
                                  struct list_of_visible_refs *visible_refs)
{
    struct ast_node *target_expr;
    struct ast_node *target_item;
    struct ast_node *filter;

    target_expr = (*expr_p)->u.op.operands[0];
    if (-1 == resolve_expr(&target_expr, visible_refs)) {
        return -1;
    }
    filter = (*expr_p)->u.op.operands[1];
    assert(AST_NODE_TYPE_FILTER == filter->type);
    if (-1 == resolve_expr(&filter, visible_refs)) {
        return -1;
    }
    target_item = target_expr->u.rexpr.target_item;
    assert(NULL != target_item);
    // in expressions, dpath type has to be set to the target
    // expression's one
    filter->u.rexpr.dpath_type = target_expr->u.rexpr.dpath_type;
    filter->u.rexpr.target_item = target_item;
    filter->u.rexpr_filter.target = target_expr;

    ast_node_free(*expr_p);
    (*expr_p) = filter;
    return 0;
}

static int
resolve_expr_file(struct ast_node **expr_p,
                  struct list_of_visible_refs *visible_refs)
{
    const struct list_of_visible_refs *toplevel_refs;
    struct ast_node *file_block;

    if (NULL == visible_refs) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
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

    ast_node_clear(*expr_p);
    (*expr_p)->type = AST_NODE_TYPE_REXPR_FILE;
    /* location ok */
    (*expr_p)->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    (*expr_p)->u.rexpr.target_item = file_block;
    return 0;
}

static int
resolve_expr_self(struct ast_node **expr_p,
                  struct list_of_visible_refs *visible_refs)
{
    struct ast_node *self_block;

    if (NULL == visible_refs) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
                       "need a binary file loaded to use 'self' in "
                       "expression");
        return -1;
    }
    // const-cast
    self_block = (struct ast_node *)visible_refs->cur_block;

    ast_node_clear(*expr_p);
    (*expr_p)->type = AST_NODE_TYPE_REXPR_SELF;
    /* location ok */
    (*expr_p)->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    (*expr_p)->u.rexpr.target_item = self_block;
    return 0;
}

static int
resolve_expr_star_wildcard(struct ast_node **expr_p,
                           struct list_of_visible_refs *visible_refs)
{
    (*expr_p)->type = AST_NODE_TYPE_REXPR_STAR_WILDCARD;
    /* location ok */
    (*expr_p)->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
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
    case AST_NODE_TYPE_OP_FCALL:
        /*TODO*/
        return AST_NODE_TYPE_NONE;
    default:
        assert(0);
    }
    /*NOT REACHED*/
}

static int
resolve_expr_subscript_common(struct ast_node **expr_p,
                              struct list_of_visible_refs *visible_refs)
{
    struct ast_node *anchor_expr;
    const struct ast_node *anchor_item;

    if (-1 == resolve_expr(&(*expr_p)->u.op_subscript_common.anchor_expr,
                           visible_refs)) {
        return -1;
    }
    if (-1 == resolve2_expr(&(*expr_p)->u.op_subscript_common.anchor_expr)) {
        return -1;
    }
    anchor_expr = (*expr_p)->u.op_subscript_common.anchor_expr;
    anchor_item = anchor_expr->u.rexpr.target_item;
    if (NULL != anchor_item) {
        if (anchor_item->type != AST_NODE_TYPE_ARRAY &&
            anchor_item->type != AST_NODE_TYPE_ARRAY_SLICE &&
            anchor_item->type != AST_NODE_TYPE_BYTE_ARRAY &&
            anchor_item->type != AST_NODE_TYPE_BYTE_SLICE) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
                "invalid use of subscript operator on non-array path");
            return -1;
        }
    } else {
        if (EXPR_DPATH_TYPE_CONTAINER != anchor_expr->u.rexpr.dpath_type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
                "invalid use of subscript operator on non-dpath or "
                "non-container dpath expression");
            return -1;
        }
    }
    return 0;
}

static int
resolve_expr_subscript_index(struct ast_node *expr,
                             struct ast_node **index_key_p,
                             struct ast_node **index_twin_p,
                             struct list_of_visible_refs *visible_refs)
{
    /* TODO: static bound checking on array subscript value */
    if (NULL != *index_key_p) {
        if (-1 == resolve_expr(index_key_p, visible_refs)) {
            return -1;
        }
        if (-1 == resolve2_expr(index_key_p)) {
            return -1;
        }
        assert(ast_node_is_rexpr(*index_key_p));
        if (EXPR_VALUE_TYPE_INTEGER != (*index_key_p)->u.rexpr.value_type &&
            EXPR_VALUE_TYPE_STRING != (*index_key_p)->u.rexpr.value_type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &(*index_key_p)->loc,
                "invalid expression type in array subscript: "
                "expect 'integer' or 'string', not '%s'",
                expr_value_type_str((*index_key_p)->u.rexpr.value_type));
            return -1;
        }
    }
    if (NULL != *index_twin_p) {
        if (-1 == resolve_expr(index_twin_p, visible_refs)) {
            return -1;
        }
        if (-1 == resolve2_expr(index_twin_p)) {
            return -1;
        }
        assert(ast_node_is_rexpr(*index_twin_p));
        if (EXPR_VALUE_TYPE_INTEGER != (*index_twin_p)->u.rexpr.value_type
            && AST_NODE_TYPE_REXPR_STAR_WILDCARD != (*index_twin_p)->type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &(*index_twin_p)->loc,
                "invalid expression type in array subscript: "
                "twin index must be of type 'integer', not '%s'",
                expr_value_type_str((*index_twin_p)->u.rexpr.value_type));
            return -1;
        }
    }
    return 0;
}

static int
resolve_expr_subscript(struct ast_node **expr_p,
                       struct list_of_visible_refs *visible_refs)
{
    struct ast_node *anchor_expr;
    struct ast_node *anchor_item;
    struct parser_location op_loc;
    struct subscript_index index;

    if (-1 == resolve_expr_subscript_common(expr_p, visible_refs)) {
        return -1;
    }
    if (-1 == resolve_expr_subscript_index(
            *expr_p,
            &(*expr_p)->u.op_subscript.index.key,
            &(*expr_p)->u.op_subscript.index.twin,
            visible_refs)) {
        return -1;
    }
    // parser would have been unhappy with '[]'
    assert(NULL != (*expr_p)->u.op_subscript.index.key);

    anchor_expr = (*expr_p)->u.op_subscript_common.anchor_expr;
    anchor_item = anchor_expr->u.rexpr.target_item;
    op_loc = (*expr_p)->loc;
    index = (*expr_p)->u.op_subscript.index;
    parser_location_make_span(&(*expr_p)->loc, &anchor_expr->loc, &op_loc);

    ast_node_clear(*expr_p);
    (*expr_p)->type = AST_NODE_TYPE_REXPR_OP_SUBSCRIPT;
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_ITEM;
    (*expr_p)->u.rexpr_op_subscript_common.anchor_expr = anchor_expr;
    (*expr_p)->u.rexpr_op_subscript.index = index;
    if (NULL != anchor_item) {
        struct ast_node *target_item;

        switch (anchor_item->type) {
        case AST_NODE_TYPE_ARRAY:
        case AST_NODE_TYPE_ARRAY_SLICE:
            target_item = anchor_item->u.array.value_type;
            break ;
        case AST_NODE_TYPE_BYTE_ARRAY:
        case AST_NODE_TYPE_BYTE_SLICE:
            target_item = AST_NODE_BYTE;
            break ;
        default:
            assert(0);
        }
        (*expr_p)->u.rexpr.value_type =
            expr_value_type_from_node(target_item);
        (*expr_p)->u.rexpr.target_item = target_item;
    } else {
        (*expr_p)->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    }
    return 0;
}

static int
resolve_expr_subscript_slice(struct ast_node **expr_p,
                             struct list_of_visible_refs *visible_refs)
{
    struct ast_node *anchor_expr;
    struct ast_node *anchor_item;
    struct parser_location op_loc;
    struct subscript_index start;
    struct subscript_index end;

    if (-1 == resolve_expr_subscript_common(expr_p, visible_refs)) {
        return -1;
    }
    if (-1 == resolve_expr_subscript_index(
            *expr_p,
            &(*expr_p)->u.op_subscript_slice.start.key,
            &(*expr_p)->u.op_subscript_slice.start.twin,
            visible_refs)) {
        return -1;
    }
    if (-1 == resolve_expr_subscript_index(
            *expr_p,
            &(*expr_p)->u.op_subscript_slice.end.key,
            &(*expr_p)->u.op_subscript_slice.end.twin,
            visible_refs)) {
        return -1;
    }
    anchor_expr = (*expr_p)->u.op_subscript_common.anchor_expr;
    anchor_item = anchor_expr->u.rexpr.target_item;
    op_loc = (*expr_p)->loc;
    start = (*expr_p)->u.op_subscript_slice.start;
    end = (*expr_p)->u.op_subscript_slice.end;
    parser_location_make_span(&(*expr_p)->loc, &anchor_expr->loc, &op_loc);

    ast_node_clear(*expr_p);
    (*expr_p)->type = AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE;
    (*expr_p)->u.rexpr.value_type = EXPR_VALUE_TYPE_UNSET;
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    (*expr_p)->u.rexpr_op_subscript_common.anchor_expr = anchor_expr;
    (*expr_p)->u.rexpr_op_subscript_slice.start = start;
    (*expr_p)->u.rexpr_op_subscript_slice.end = end;

    // a slice still references the anchor array
    (*expr_p)->u.rexpr.target_item = anchor_item;

    return 0;
}

static int
resolve_expr_op_member(struct ast_node **expr_p,
                       struct list_of_visible_refs *visible_refs)
{
    struct op *op;
    const struct ast_node *anchor_item;
    const struct block_def *block_def;
    struct ast_node *opd1, *opd2;

    /* recursively resolve references of member operator, sanity
     * checks reflect left-to-right associativity declared in the
     * parser rules */
    op = &(*expr_p)->u.op;
    if (-1 == resolve_expr(&op->operands[0], visible_refs)) {
        return -1;
    }
    // need resolve2 to get the final expression dpath type
    if (-1 == resolve2_expr(&op->operands[0])) {
        return -1;
    }
    opd1 = op->operands[0];
    opd2 = op->operands[1];
    /* checked by parser */
    assert(opd2->type == AST_NODE_TYPE_IDENTIFIER);
    anchor_item = opd1->u.rexpr.target_item;
    if (NULL == anchor_item) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
            "member operator with computed dpath not supported");
        return -1;
    }
    if (anchor_item->type != AST_NODE_TYPE_BLOCK_DEF) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
            "invalid use of member operator on non-block dpath");
        return -1;
    }
    block_def = &anchor_item->u.block_def;
    if (*opd2->u.identifier == '?') {
        struct link *resolved_link;

        resolved_link = (struct link *)find_statement_by_name(
            STATEMENT_TYPE_LINK, opd2->u.identifier + 1,
            &block_def->block_stmt_list);
        if (NULL == resolved_link) {
            const char *block_typename;

            block_typename = reverse_lookup_typename(anchor_item,
                                                     visible_refs);
            if (NULL != block_typename) {
                semantic_error(
                    SEMANTIC_LOGLEVEL_ERROR, &opd2->loc,
                    "no member named '%s' is associated to block of type "
                    "'%s'",
                    opd2->u.identifier, block_typename);
            } else {
                semantic_error(
                    SEMANTIC_LOGLEVEL_ERROR, &opd2->loc,
                    "no member named '%s' is associated to block",
                    opd2->u.identifier);
            }
            semantic_error(SEMANTIC_LOGLEVEL_INFO, &anchor_item->loc,
                           "declared here");
            return -1;
        }
        ast_node_clear(*expr_p);
        set_node_as_rexpr_link(expr_p, anchor_item, resolved_link, opd1);
    } else {
        struct field *resolved_field;

        resolved_field = (struct field *)find_statement_by_name(
            STATEMENT_TYPE_FIELD, opd2->u.identifier,
            &block_def->block_stmt_list);
        if (NULL == resolved_field) {
            const char *block_typename;

            block_typename = reverse_lookup_typename(anchor_item,
                                                     visible_refs);
            if (NULL != block_typename) {
                semantic_error(
                    SEMANTIC_LOGLEVEL_ERROR, &opd2->loc,
                    "no field named '%s' exists in block of type '%s'",
                    opd2->u.identifier, block_typename);
            } else {
                semantic_error(
                    SEMANTIC_LOGLEVEL_ERROR, &opd2->loc,
                    "no field named '%s' exists in block",
                    opd2->u.identifier);
            }
            semantic_error(SEMANTIC_LOGLEVEL_INFO, &anchor_item->loc,
                           "declared here");
            return -1;
        }
        ast_node_clear(*expr_p);
        set_node_as_rexpr_field(expr_p, anchor_item, resolved_field, opd1);
    }

    parser_location_make_span(&(*expr_p)->loc, &opd1->loc, &opd2->loc);
    free(opd2->u.identifier);
    free(opd2);
    return 0;
}

static int
resolve_span_expr(struct ast_node **span_expr_p,
                  struct list_of_visible_refs *visible_refs)
{
    if (-1 == resolve_expr(span_expr_p, visible_refs)) {
        return -1;
    }
    assert(ast_node_is_rexpr(*span_expr_p));
    if ((*span_expr_p)->u.rexpr.value_type != EXPR_VALUE_TYPE_INTEGER) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &(*span_expr_p)->loc,
            "span expression must be of integer type, not '%s'",
            expr_value_type_str((*span_expr_p)->u.rexpr.value_type));
        return -1;
    }
    return 0;
}

static int
resolve_link(struct link *link,
             struct list_of_visible_refs *visible_refs)
{
    if (NULL != link->dst_expr) {
        if (-1 == resolve_expr(&link->dst_expr, visible_refs)) {
            return -1;
        }
        if (-1 == resolve2_expr(&link->dst_expr)) {
            return -1;
        }
        assert(ast_node_is_rexpr(link->dst_expr));
        if (EXPR_DPATH_TYPE_NONE == link->dst_expr->u.rexpr.dpath_type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &link->nstmt.stmt.loc,
                "invalid link target: '%s' is not a dpath type",
                ast_node_type_str(link->dst_expr->type));
            return -1;
        }
    }
    return 0;
}

int
resolve_user_expr(struct ast_node **expr_p,
                  const struct ast_node *top_level_block)
{
    struct list_of_visible_refs visible_refs;
    int ret;

    if (NULL != top_level_block) {
        visible_refs.outer_refs = NULL;
        visible_refs.cur_block = top_level_block;
        visible_refs.cur_lists =
            &top_level_block->u.block_def.block_stmt_list;
        if (-1 == resolve_node_types(expr_p, &visible_refs,
                                     RESOLVE_EXPECT_EXPRESSION)) {
            return -1;
        }
        ret = resolve_expr(expr_p, &visible_refs);
    } else {
        if (-1 == resolve_node_types(expr_p, NULL,
                                     RESOLVE_EXPECT_EXPRESSION)) {
            return -1;
        }
        ret = resolve_expr(expr_p, NULL);
    }
    if (-1 == ret) {
        return -1;
    }
    return resolve2_expr(expr_p);
}

/*
 * resolve2: second pass
 */

static int
resolve2_block(struct ast_node *block)
{
    struct statement_list *field_list;
    struct statement *stmt;
    struct field *field;
    const struct ast_node *as_type;

    if (-1 == resolve2_dtype(block)) {
        return -1;
    }
    if (-1 == resolve2_stmt_lists(&block->u.block_def.block_stmt_list)) {
        return -1;
    }
    field_list = block->u.block_def.block_stmt_list.field_list;
    TAILQ_FOREACH(stmt, field_list, list) {
        field = (struct field *)stmt;
        if (NULL != field->nstmt.name) {
            continue ;
        }
        as_type = ast_node_get_as_type(field->field_type);
        if (AST_NODE_TYPE_BLOCK_DEF == as_type->type) {
            continue ;
        }
        stmt->stmt_flags |= FIELD_FLAG_HIDDEN;
    }
    return 0;
}

static int
resolve2_stmt_list_generic(struct statement_list *stmt_list)
{
    struct statement *stmt;

    TAILQ_FOREACH(stmt, stmt_list, list) {
        if (NULL != stmt->cond
            && -1 == resolve2_conditional(stmt->cond)) {
            return -1;
        }
    }
    return 0;
}

static int
link_check_duplicates(struct link *link)
{
    const struct ast_node *dst_item;
    struct link *next_link;
    const struct ast_node *next_dst_item;

    dst_item = link->dst_expr->u.rexpr.target_item;
    for (next_link = (struct link *)link->nstmt.next_sibling;
         NULL != next_link;
         next_link = (struct link *)next_link->nstmt.next_sibling) {
        next_dst_item =
            next_link->dst_expr->u.rexpr.target_item;
        if (dst_item != next_dst_item) {
            if (dst_item->type == next_dst_item->type) {
                if (AST_NODE_TYPE_BYTE_ARRAY == dst_item->type) {
                    continue ;
                }
            }
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &next_link->nstmt.stmt.loc,
                "different target types across duplicate link names");
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &link->nstmt.stmt.loc,
                "first declaration here");
            return -1;
        }
    }
    return 0;
}

static int
links_check_duplicates(struct statement_list *link_list)
{
    struct statement *stmt;

    TAILQ_FOREACH(stmt, link_list, list) {
        if (-1 == link_check_duplicates((struct link *)stmt)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve2_stmt_lists(struct block_stmt_list *stmt_lists)
{
    struct statement *stmt;
    struct named_type *named_type;
    struct ast_node *block_def;
    struct field *field;
    struct span_stmt *span_stmt;
    struct key_stmt *key_stmt;
    struct match *match;
    struct link *link;

    STAILQ_FOREACH(named_type, stmt_lists->named_type_list, list) {
        if (-1 == resolve2_dtype(named_type->type)) {
            return -1;
        }
    }
    if (-1 == resolve2_stmt_list_generic(stmt_lists->field_list)) {
        return -1;
    }
    if (-1 == resolve2_stmt_list_generic(stmt_lists->span_list)) {
        return -1;
    }
    if (-1 == resolve2_stmt_list_generic(stmt_lists->last_stmt_list)) {
        return -1;
    }
    if (-1 == resolve2_stmt_list_generic(stmt_lists->link_list)) {
        return -1;
    }
    if (-1 == links_check_duplicates(stmt_lists->link_list)) {
        return -1;
    }
    /* resolve2 pass on references of all nested blocks */
    STAILQ_FOREACH(block_def, stmt_lists->block_def_list, stmt_list) {
        if (-1 == resolve2_block(block_def)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, stmt_lists->match_list, list) {
        match = (struct match *)stmt;
        if (-1 == resolve2_expr(&match->expr)) {
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
    TAILQ_FOREACH(stmt, stmt_lists->field_list, list) {
        field = (struct field *)stmt;
        if (-1 == resolve2_dtype(field->field_type)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, stmt_lists->span_list, list) {
        span_stmt = (struct span_stmt *)stmt;
        if (-1 == resolve2_expr(&span_stmt->span_expr)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, stmt_lists->key_list, list) {
        key_stmt = (struct key_stmt *)stmt;
        if (-1 == resolve2_key_stmt(key_stmt)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, stmt_lists->link_list, list) {
        link = (struct link *)stmt;
        if (-1 == resolve2_link(link)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve2_dtype(struct ast_node *dtype)
{
    struct call_stack_error_ctx error_ctx;

    error_ctx.call_type = CALL_RESOLVE2_DTYPE;
    error_ctx.u.dtype = dtype;
    push_error_ctx(&error_ctx);

    if (-1 == resolve2_span_size(dtype)) {
        pop_error_ctx();
        return -1;
    }
    if (ast_node_is_item(dtype)
        && NULL != dtype->u.item.filter
        && -1 == resolve2_filter(dtype->u.item.filter)) {
        pop_error_ctx();
        return -1;
    }
    pop_error_ctx();
    return 0;
}

static int
resolve2_conditional(struct ast_node *cond)
{
    if (-1 == resolve2_expr(&cond->u.conditional.cond_expr)) {
        return -1;
    }
    if (NULL != cond->u.conditional.outer_cond
        && -1 == resolve2_conditional(cond->u.conditional.outer_cond)) {
        return -1;
    }
    return 0;
}

static int
resolve2_filter(struct ast_node *filter)
{
    switch (filter->type) {
    case AST_NODE_TYPE_REXPR_INTERPRETER:
        break ;
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        if (-1 == resolve2_dtype(filter->u.rexpr_as_type.as_type)) {
            return -1;
        }
        break ;
    default:
        assert(0);
    }
    return 0;
}

static int
resolve2_expr(struct ast_node **expr_p)
{
    struct call_stack_error_ctx error_ctx;
    int ret;

    error_ctx.call_type = CALL_RESOLVE2_EXPR;
    error_ctx.u.expr = *expr_p;
    push_error_ctx(&error_ctx);
    ret = 0;
    switch ((*expr_p)->type) {
    case AST_NODE_TYPE_REXPR_FIELD:
        ret = resolve2_expr_field(expr_p);
        break ;
    case AST_NODE_TYPE_REXPR_MEMBER:
        ret = resolve2_expr_link(expr_p);
        break ;
    case AST_NODE_TYPE_REXPR_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT:
        ret = resolve2_expr_operator(expr_p, 1);
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
        ret = resolve2_expr_operator(expr_p, 2);
        break ;
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
        ret = resolve2_expr_operator_subscript(expr_p);
        break ;
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        ret = resolve2_expr_operator_subscript_slice(expr_p);
        break ;
    case AST_NODE_TYPE_REXPR_OP_SIZEOF:
        ret = resolve2_expr_operator_sizeof(expr_p);
        break ;
    case AST_NODE_TYPE_REXPR_OP_ADDROF:
        ret = resolve2_expr_operator_addrof(expr_p);
        break ;
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        ret = resolve2_expr_operator_filter(expr_p);
        break ;
    case AST_NODE_TYPE_REXPR_OP_FCALL:
        ret = resolve2_expr_fcall(expr_p);
        break ;
    case AST_NODE_TYPE_REXPR_INTERPRETER:
        ret = resolve2_expr_interpreter(expr_p);
        break ;
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        ret = resolve2_expr_as_type(expr_p);
        break ;
    default:
        /* nothing to do */
        break ;
    }
    pop_error_ctx();
    return ret;
}

static int
resolve2_expr_dpath_check_subscript_internal(
    struct ast_node *expr, struct subscript_index *subscript)
{
    struct ast_node *twin_idx;
    const struct ast_node *anchor_expr;
    const struct ast_node *anchor_item;
    struct ast_node *key_expr;
    int ret;

    if (NULL == subscript->key) {
        return 0;
    }
    if (NULL != subscript->twin) {
        ret = resolve2_expr(&subscript->twin);
        if (-1 == ret) {
            return -1;
        }
        twin_idx = subscript->twin;
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
            if (EXPR_VALUE_TYPE_INTEGER
                != subscript->key->u.rexpr.value_type) {
                key_expr = ast_node_get_key_expr(anchor_item);
                assert(ast_node_is_rexpr(subscript->key));
                assert(ast_node_is_rexpr(key_expr));
                if (subscript->key->u.rexpr.value_type
                    != key_expr->u.rexpr.value_type) {
                    semantic_error(
                        SEMANTIC_LOGLEVEL_ERROR, &subscript->key->loc,
                        "invalid expression type in array subscript: "
                        "type mismatch between subscript type '%s' and "
                        "index type '%s'",
                        ast_node_type_str(subscript->key->u.rexpr.value_type),
                        ast_node_type_str(key_expr->u.rexpr.value_type));
                    return -1;
                }
            }
        } else if (EXPR_VALUE_TYPE_STRING
                   == subscript->key->u.rexpr.value_type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &subscript->key->loc,
                "invalid expression type in array subscript: "
                "'string' type requires array element type to be indexed");
            return -1;
        }
    }
    return 0;
}

static int
resolve2_expr_field(struct ast_node **expr_p)
{
    int ret;

    //FIXME is it necessary?
#if 0
    if (NULL != (*expr_p)->u.rexpr.target_item) {
        ret = resolve2_dtype(
            (*expr_p)->u.rexpr.target_item);
        if (-1 == ret) {
            return -1;
        }
    }
#endif
    if (NULL != (*expr_p)->u.rexpr_field.anchor_expr) {
        ret = resolve2_expr(&(*expr_p)->u.rexpr_field.anchor_expr);
        if (-1 == ret) {
            return -1;
        }
    }
    return 0;
}

static int
resolve2_expr_link(struct ast_node **expr_p)
{
    int ret;
    struct link *link;
    struct ast_node *dst_item;

    //FIXME is it necessary?
#if 0
    if (NULL != (*expr_p)->u.rexpr.target_item) {
        ret = resolve2_dtype(
            (*expr_p)->u.rexpr.target_item);
        if (-1 == ret) {
            return -1;
        }
    }
#endif
    if (NULL != (*expr_p)->u.rexpr_link.anchor_expr) {
        ret = resolve2_expr(&(*expr_p)->u.rexpr_link.anchor_expr);
        if (-1 == ret) {
            return -1;
        }
    }
    link = (struct link *)(*expr_p)->u.rexpr_link.link;
    ret = resolve2_expr(&link->dst_expr);
    if (-1 == ret) {
        return -1;
    }
    dst_item = link->dst_expr->u.rexpr.target_item;
    (*expr_p)->u.rexpr.target_item = dst_item;
    (*expr_p)->u.rexpr.value_type = link->dst_expr->u.rexpr.value_type;
    if (NULL == link->nstmt.next_sibling) {
        (*expr_p)->u.rexpr.dpath_type =
            link->dst_expr->u.rexpr.dpath_type;
    } else {
        // TODO: We may optimize with EXPR_DPATH_TYPE_ITEM
        // whenever all duplicate links use item type, though this
        // requires post-processing when all types have been
        // resolved. Container type is more universal.
        (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    }
    return 0;
}

static int
resolve2_expr_operator_subscript(struct ast_node **expr_p)
{
    int ret;
    struct subscript_index *index;

    //FIXME is it necessary?
#if 0
    if (NULL != (*expr_p)->u.rexpr.target_item) {
        ret = resolve2_dtype(
            (*expr_p)->u.rexpr.target_item);
        if (-1 == ret) {
            return -1;
        }
    }
#endif
    assert(NULL != (*expr_p)->u.rexpr_op_subscript_common.anchor_expr);
    ret = resolve2_expr(&(*expr_p)->u.rexpr_op_subscript_common.anchor_expr);
    if (-1 == ret) {
        return -1;
    }
    ret = resolve2_expr(&(*expr_p)->u.rexpr_op_subscript.index.key);
    if (-1 == ret) {
        return -1;
    }
    index = &(*expr_p)->u.rexpr_op_subscript.index;
    ret = resolve2_expr_dpath_check_subscript_internal(*expr_p, index);
    if (-1 == ret) {
        return -1;
    }
    return 0;
}

static int
resolve2_expr_operator_subscript_slice(struct ast_node **expr_p)
{
    int ret;
    struct subscript_index *slice_start;
    struct subscript_index *slice_end;

    //FIXME is it necessary?
#if 0
    if (NULL != (*expr_p)->u.rexpr.target_item) {
        ret = resolve2_dtype(
            (*expr_p)->u.rexpr.target_item);
        if (-1 == ret) {
            return -1;
        }
    }
#endif
    assert(NULL != (*expr_p)->u.rexpr_op_subscript_common.anchor_expr);
    ret = resolve2_expr(&(*expr_p)->u.rexpr_op_subscript_common.anchor_expr);
    if (-1 == ret) {
        return -1;
    }
    slice_start = &(*expr_p)->u.rexpr_op_subscript_slice.start;
    slice_end = &(*expr_p)->u.rexpr_op_subscript_slice.end;
    if (NULL != slice_start->key) {
        ret = resolve2_expr(&slice_start->key);
        if (-1 == ret) {
            return -1;
        }
    }
    ret = resolve2_expr_dpath_check_subscript_internal(*expr_p,
                                                       slice_start);
    if (-1 == ret) {
        return -1;
    }
    ret = resolve2_expr_dpath_check_subscript_internal(*expr_p,
                                                       slice_end);
    if (-1 == ret) {
        return -1;
    }
    return 0;
}

/**
 * @brief second pass of resolve on expressions nodes of type
 * 'operator'
 *
 * Its job:
 *
 * - prune pre-computable branches
 * 
 * - in particular, prune sizeof operator operand: this allows to use
 *   sizeof on type names that have a static size but defined using
 *   expressions (typically, array size expressions that end up being
 *   a static, pre-computable value)
 */
static int
resolve2_expr_operator(struct ast_node **expr_p, int n_operands)
{
    int opd_i;
    int only_native_operands;

    only_native_operands = TRUE;
    for (opd_i = 0; opd_i < n_operands; ++opd_i) {
        if (-1 == resolve2_expr(&(*expr_p)->u.rexpr_op.op.operands[opd_i])) {
            return -1;
        }
        if (AST_NODE_TYPE_REXPR_NATIVE !=
            (*expr_p)->u.rexpr_op.op.operands[opd_i]->type) {
            only_native_operands = FALSE;
        }
    }
    if (only_native_operands) {
        struct ast_node *operand;
        union expr_value operand_values[2];
        const struct expr_evaluator *evaluator;
        union expr_value eval_value;

        for (opd_i = 0; opd_i < n_operands; ++opd_i) {
            operand = (*expr_p)->u.rexpr_op.op.operands[opd_i];
            operand_values[opd_i] = operand->u.rexpr_native.value;
        }
        evaluator = (*expr_p)->u.rexpr_op.evaluator;
        assert(NULL != evaluator);
        eval_value = evaluator->eval_fn(operand_values);

        for (opd_i = 0; opd_i < n_operands; ++opd_i) {
            operand = (*expr_p)->u.rexpr_op.op.operands[opd_i];
            ast_node_free(operand);
        }
        (*expr_p)->type = AST_NODE_TYPE_REXPR_NATIVE;
        /* (*expr_p)->loc already set */
        /* (*expr_p)->u.rexpr.value_type already set */
        (*expr_p)->u.rexpr_native.value = eval_value;
    }
    return 0;
}

static int
resolve2_expr_operator_sizeof(struct ast_node **expr_p)
{
    struct ast_node *operand;
    struct ast_node *item;
    int allow_dynamic_span;

    switch ((*expr_p)->u.rexpr_op.op.operands[0]->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
        if (-1 == resolve2_dtype((*expr_p)->u.rexpr_op.op.operands[0])) {
            return -1;
        }
        operand = (*expr_p)->u.rexpr_op.op.operands[0];
        item = operand;
        if (-1 == resolve2_span_size(item)) {
            return -1;
        }
        allow_dynamic_span = FALSE;
        break ;
    default:
        if (-1 == resolve2_expr(&(*expr_p)->u.rexpr_op.op.operands[0])) {
            return -1;
        }
        operand = (*expr_p)->u.rexpr_op.op.operands[0];
        item = operand->u.rexpr.target_item;
        if (NULL != item && -1 == resolve2_span_size(item)) {
            return -1;
        }
        allow_dynamic_span = TRUE;
        break ;
    }

    if (! allow_dynamic_span
        && 0 != (item->flags & ASTFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
            "invalid use of sizeof operator on dynamic-sized type name\n"
            "(use a path to field for computing size dynamically)");
        return -1;
    }
    if (0 == (item->flags & ASTFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        (*expr_p)->type = AST_NODE_TYPE_REXPR_NATIVE;
        (*expr_p)->u.rexpr.value_type = EXPR_VALUE_TYPE_INTEGER;
        (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_NONE;
        (*expr_p)->u.rexpr_native.value.integer = item->u.item.min_span_size;
    }
    return 0;
}

static int
resolve2_expr_operator_addrof(struct ast_node **expr_p)
{
    if (-1 == resolve2_expr(&(*expr_p)->u.rexpr_op.op.operands[0])) {
        return -1;
    }
    return 0;
}

static int
resolve2_expr_operator_filter(struct ast_node **expr_p)
{
    if (-1 == resolve2_expr(&(*expr_p)->u.rexpr_op.op.operands[0])) {
        return -1;
    }
    return 0;
}

static int
resolve2_expr_fcall(struct ast_node **expr_p)
{
    struct func_param *param;

    /* resolve expressions in parameter list */
    for (param = STAILQ_FIRST((*expr_p)->u.rexpr_op_fcall.func_params);
         NULL != param;
         param = STAILQ_NEXT(param, list)) {
        if (-1 == resolve2_expr(&param->expr)) {
            return -1;
        }
    }
    return 0;
}

static int
resolve2_expr_interpreter(struct ast_node **expr_p)
{
    struct ast_node *target;

    target = (*expr_p)->u.rexpr_filter.target;
    assert(NULL != target);
    if (-1 == resolve2_span_size(target)) {
        return -1;
    }
    return 0;
}

static int
resolve2_expr_as_type(struct ast_node **expr_p)
{
    struct ast_node *as_type;
    struct ast_node *target_item;

    if (-1 == resolve2_expr(&(*expr_p)->u.rexpr_filter.target)) {
        return -1;
    }
    target_item = (*expr_p)->u.rexpr.target_item;
    as_type = (*expr_p)->u.rexpr_as_type.as_type;
    assert(ast_node_is_item(as_type));
    if (-1 == resolve2_span_size(as_type)) {
        return -1;
    }
    if (NULL != as_type
        && !(ASTFLAG_IS_SPAN_SIZE_DYNAMIC & as_type->flags)
        && (ast_node_get_min_span_size(as_type)
            > ast_node_get_min_span_size(target_item))) {
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR, &(*expr_p)->loc,
            "invalid link cast: cast-to type minimum size is greater "
            "than static size of destination (%"PRIi64" > %"PRIi64")",
            ast_node_get_min_span_size(as_type),
            ast_node_get_min_span_size(target_item));
        return -1;
    }
    (*expr_p)->u.rexpr.dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    (*expr_p)->u.rexpr.value_type = expr_value_type_from_node(as_type);
    (*expr_p)->u.rexpr.target_item = as_type;
    return 0;
}

static int
resolve2_span_size(struct ast_node *node)
{
    struct call_stack_error_ctx error_ctx;
    int ret;

    if (SPAN_SIZE_UNDEF != node->u.item.min_span_size) {
        return 0;
    }
    error_ctx.call_type = CALL_RESOLVE2_SPAN_SIZE;
    error_ctx.u.dtype = node;
    push_error_ctx(&error_ctx);

    if ((node->flags & ASTFLAG_PROCESSING)) {
        /* TODO restore proper sanity check on circular dependency */
        //dump_error_circular_dependency_on_node_size(node);
        pop_error_ctx();
        //return -1;
        return 0;
    }
    node->flags |= ASTFLAG_PROCESSING;
    switch (node->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        ret = resolve2_span_size_block(node);
        break ;
    case AST_NODE_TYPE_ARRAY:
        ret = resolve2_span_size_array(node);
        break ;
    case AST_NODE_TYPE_BYTE:
        ret = resolve2_span_size_byte(node);
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
        ret = resolve2_span_size_byte_array(node);
        break ;
    default:
        assert(0);
    }
    pop_error_ctx();
    node->flags &= ~ASTFLAG_PROCESSING;
    return ret;
}

static int
resolve2_span_size_block(struct ast_node *node)
{
    struct span_stmt *span_stmt;
    struct ast_node *min_span_expr;
    struct ast_node *max_span_expr;
    const struct statement_list *field_list;
    struct statement *stmt;
    const struct field *field;
    struct ast_node *field_type;
    const struct field *compute_offset_backwards_from;
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
    TAILQ_FOREACH(stmt, node->u.block_def.block_stmt_list.span_list, list) {
        span_stmt = (struct span_stmt *)stmt;
        if (NULL == stmt->cond) {
            if ((stmt->stmt_flags & SPAN_FLAG_MIN)) {
                min_span_expr = span_stmt->span_expr;
            }
            if ((stmt->stmt_flags & SPAN_FLAG_MAX)) {
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
    field_list = node->u.block_def.block_stmt_list.field_list;
    compute_offset_backwards_from = NULL;
    TAILQ_FOREACH(stmt, field_list, list) {
        field = (const struct field *)stmt;
        field_type = field->field_type;
        if (-1 == resolve2_span_size(field_type)) {
            return -1;
        }
        if (0 != (field_type->flags & ASTFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            dynamic_used = TRUE;
        }
        if (NULL == stmt->cond) {
            /* only update min span size if field is not conditional */
            if (BLOCK_TYPE_UNION == node->u.block_def.type) {
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
        if (0 != (field_type->flags & ASTFLAG_NEED_SLACK)) {
            child_needs_slack = TRUE;
        }
        if (0 != (field_type->flags & ASTFLAG_HAS_UNDETERMINED_SIZE)) {
            child_has_undetermined_size = TRUE;
            compute_offset_backwards_from = NULL;
        } else if (BLOCK_TYPE_STRUCT == node->u.block_def.type
                   && child_has_undetermined_size
                   && NULL == compute_offset_backwards_from) {
            compute_offset_backwards_from = field;
        }
    }
    for (stmt = (struct statement *)compute_offset_backwards_from;
         NULL != stmt; stmt = TAILQ_NEXT(stmt, list)) {
        stmt->stmt_flags |= FIELD_FLAG_SLACK_TRAILER;
    }

    if (NULL != min_span_expr) {
        assert(EXPR_VALUE_TYPE_INTEGER == min_span_expr->u.rexpr.value_type);
        if (-1 == resolve2_expr(&min_span_expr)) {
            return -1;
        }
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
        if (-1 == resolve2_expr(&max_span_expr)) {
            return -1;
        }
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
        node->flags |= ASTFLAG_NEED_SLACK;
    }
    node->u.item.min_span_size = min_span_size;
    if (dynamic_span) {
        node->flags |= ASTFLAG_IS_SPAN_SIZE_DYNAMIC;
    }
    if (dynamic_used) {
        node->flags |= ASTFLAG_IS_USED_SIZE_DYNAMIC;
    }
    if (child_has_undetermined_size
        && (NULL == node->u.item.filter
            || NULL == (node->u.item.filter
                        ->u.rexpr_interpreter.get_size_func))
        && NULL == max_span_expr) {
        node->flags |= ASTFLAG_HAS_UNDETERMINED_SIZE;
    }
    if (NULL != compute_offset_backwards_from) {
        node->flags |= ASTFLAG_HAS_FOOTER;
    }
    if (!TAILQ_EMPTY(node->u.block_def.block_stmt_list.last_stmt_list)) {
        node->flags |= ASTFLAG_CONTAINS_LAST_STMT;
    }
    return 0;
}

static int
resolve2_span_size_array(struct ast_node *node)
{
    struct ast_node *value_type;
    struct ast_node *value_count_expr;
    int64_t value_count;
    int64_t min_span_size;
    int dynamic_span;

    if (-1 == resolve2_expr(&node->u.array.value_type)) {
        return -1;
    }
    if (NULL != node->u.array.value_count &&
        -1 == resolve2_expr(&node->u.array.value_count)) {
        return -1;
    }
    value_type = node->u.array.value_type;
    if (-1 == resolve2_span_size(value_type)) {
        return -1;
    }
    value_count_expr = node->u.array.value_count;
    if (NULL != value_count_expr
        && (EXPR_VALUE_TYPE_INTEGER
            != value_count_expr->u.rexpr.value_type)) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &value_count_expr->loc,
                       "invalid array size type: expect '%s', got '%s'",
                       expr_value_type_str(EXPR_VALUE_TYPE_INTEGER),
                       expr_value_type_str(value_count_expr->u.rexpr.value_type));
        return -1;
    }
    if (NULL != value_count_expr &&
        AST_NODE_TYPE_REXPR_NATIVE == value_count_expr->type) {
        assert(EXPR_VALUE_TYPE_INTEGER
               == value_count_expr->u.rexpr.value_type);
        value_count = value_count_expr->u.rexpr_native.value.integer;
        min_span_size = value_count * value_type->u.item.min_span_size;
        dynamic_span =
            (0 != (value_type->flags & ASTFLAG_IS_SPAN_SIZE_DYNAMIC));
    } else {
        min_span_size = 0;
        dynamic_span = TRUE;
    }
    if (0 != (value_type->flags & ASTFLAG_CONTAINS_LAST_STMT)) {
        dynamic_span = TRUE;
    } else if (0 != (value_type->flags & ASTFLAG_NEED_SLACK)
               || NULL == value_count_expr) {
        node->flags |= ASTFLAG_NEED_SLACK;
    }
    node->u.item.min_span_size = min_span_size;
    if (dynamic_span) {
        node->flags |= (ASTFLAG_IS_SPAN_SIZE_DYNAMIC |
                        ASTFLAG_IS_USED_SIZE_DYNAMIC);
    }
    if ((NULL == node->u.item.filter
         || NULL == (node->u.item.filter
                     ->u.rexpr_interpreter.get_size_func))
        && (NULL == value_count_expr
            || 0 != (value_type->flags & ASTFLAG_HAS_UNDETERMINED_SIZE))) {
        node->flags |= ASTFLAG_HAS_UNDETERMINED_SIZE;
    }
    return 0;
}

static int
resolve2_span_size_byte(struct ast_node *node)
{
    return 0;
}

static int
resolve2_span_size_byte_array(struct ast_node *node)
{
    struct ast_node *size_expr;
    int64_t min_span_size;

    if (NULL != node->u.byte_array.size &&
        -1 == resolve2_expr(&node->u.byte_array.size)) {
        return -1;
    }
    size_expr = node->u.byte_array.size;
    if (NULL != size_expr
        && EXPR_VALUE_TYPE_INTEGER != size_expr->u.rexpr.value_type) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &size_expr->loc,
                       "invalid byte array size type: expect '%s', "
                       "got '%s'",
                       expr_value_type_str(EXPR_VALUE_TYPE_INTEGER),
                       expr_value_type_str(size_expr->u.rexpr.value_type));
        return -1;
    }
    if (NULL != size_expr
        && AST_NODE_TYPE_REXPR_NATIVE == size_expr->type) {
        min_span_size = size_expr->u.rexpr_native.value.integer;
    } else {
        min_span_size = 0;
        node->flags |= (ASTFLAG_IS_SPAN_SIZE_DYNAMIC |
                        ASTFLAG_IS_USED_SIZE_DYNAMIC);
    }
    if (NULL == size_expr) {
        node->flags |= ASTFLAG_NEED_SLACK;
        if (NULL == node->u.item.filter
            || NULL == (node->u.item.filter
                        ->u.rexpr_interpreter.get_size_func)) {
            node->flags |= ASTFLAG_HAS_UNDETERMINED_SIZE;
        }
    }
    node->u.item.min_span_size = min_span_size;
    return 0;
}

static int
resolve2_key_stmt(struct key_stmt *key_stmt)
{
    struct ast_node *key_expr;

    if (-1 == resolve2_expr(&key_stmt->key_expr)) {
        return -1;
    }
    key_expr = key_stmt->key_expr;
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
resolve2_link(struct link *link)
{
    if (-1 == resolve2_expr(&link->dst_expr)) {
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
    if (-1 == setup_track_backends(AST_NODE_BYTE)) {
        return -1;
    }
    if (-1 == setup_track_backends(AST_NODE_ARRAY_SLICE)) {
        return -1;
    }
    if (-1 == setup_track_backends(AST_NODE_BYTE_SLICE)) {
        return -1;
    }
    if (-1 == setup_track_backends(AST_NODE_AS_BYTES)) {
        return -1;
    }
    if (-1 == setup_track_backends(AST_NODE_FILTERED)) {
        return -1;
    }
    return 0;
}

static int
setup_track_backends(struct ast_node *node)
{
    int ret;

    if (0 != (node->flags & (ASTFLAG_PROCESSING |
                             ASTFLAG_PROCESSED_TRACK_BACKEND))) {
        /* only process once all nodes */
        return 0;
    }
    node->flags |= ASTFLAG_PROCESSING;
    switch (node->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        ret = setup_track_backends_recur_block(node);
        break ;
    case AST_NODE_TYPE_ARRAY:
        ret = setup_track_backends(node->u.array.value_type);
        break ;
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        ret = setup_track_backends(node->u.rexpr_as_type.as_type);
        if (0 == ret) {
            ret = setup_track_backends(node->u.rexpr_filter.target);
        }
        break ;
    default:
        ret = 0;
        break ;
    }
    node->flags &= ~ASTFLAG_PROCESSING;
    if (-1 == ret) {
        return -1;
    }
    if (ast_node_is_item(node)) {
        if (-1 == browse_setup_backends(node)) {
            return -1;
        }
        if (NULL != node->u.item.filter
            && AST_NODE_TYPE_REXPR_AS_TYPE == node->u.item.filter->type
            && -1 == browse_setup_backends(
                node->u.item.filter->u.rexpr_as_type.as_type)) {
            return -1;
        }
    }
    node->flags |= ASTFLAG_PROCESSED_TRACK_BACKEND;
    return 0;
}

static int
setup_track_backends_recur_block(struct ast_node *block)
{
    struct block_stmt_list *block_lists;
    struct named_type *named_type;
    struct statement *stmt;
    const struct field *field;
    struct ast_node *block_def;
    struct link *link;

    block_lists = &block->u.block_def.block_stmt_list;
    STAILQ_FOREACH(named_type, block_lists->named_type_list, list) {
        if (-1 == setup_track_backends(named_type->type)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, block_lists->field_list, list) {
        field = (const struct field *)stmt;
        if (-1 == setup_track_backends(field->field_type)) {
            return -1;
        }
    }
    STAILQ_FOREACH(block_def, block_lists->block_def_list, stmt_list) {
        if (-1 == setup_track_backends(block_def)) {
            return -1;
        }
    }
    TAILQ_FOREACH(stmt, block_lists->link_list, list) {
        link = (struct link *)stmt;
        if (-1 == setup_track_backends(link->dst_expr)) {
            return -1;
        }
    }
    return 0;
}

/*
 * error reporting
 */


static void
dump_error_circular_dependency_on_node_size(const struct ast_node *node)
    __attribute__((unused));

static void
dump_error_circular_dependency_on_node_size(const struct ast_node *node)
{
    const struct call_stack_error_ctx *first_error_ctx;
    const struct call_stack_error_ctx *next_error_ctx;

    semantic_error(
        SEMANTIC_LOGLEVEL_ERROR, &node->loc,
        "circular dependency to compute this type's size");
    first_error_ctx = error_ctx_stack_top->prev;
    assert(NULL != first_error_ctx);
    while (CALL_RESOLVE2_SPAN_SIZE != first_error_ctx->call_type ||
        first_error_ctx->u.dtype != node) {
        first_error_ctx = first_error_ctx->prev;
        assert(NULL != first_error_ctx);
    }
    next_error_ctx = first_error_ctx->next;
    while (NULL != next_error_ctx) {
        switch (next_error_ctx->call_type) {
        case CALL_RESOLVE2_EXPR:
            if (0 != (next_error_ctx->u.expr->flags & ASTFLAG_IS_SPAN_EXPR)) {
                semantic_error(
                    SEMANTIC_LOGLEVEL_INFO, &next_error_ctx->u.expr->loc,
                    "which is defined by its span size declared here");
            } else if (AST_NODE_TYPE_REXPR_OP_SIZEOF ==
                       next_error_ctx->u.expr->type) {
                semantic_error(
                    SEMANTIC_LOGLEVEL_INFO, &next_error_ctx->u.expr->loc,
                    "which depends on the value of this sizeof expression");
            } /* else do not comment */
            break ;
        case CALL_RESOLVE2_SPAN_SIZE:
            semantic_error(
                SEMANTIC_LOGLEVEL_INFO, &next_error_ctx->u.dtype->loc,
                "which depends on this type's size");
            break ;
        case CALL_RESOLVE2_DTYPE:
            break ;
        default:
            assert(0);
        }
        next_error_ctx = next_error_ctx->next;
    }
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
    case AST_NODE_TYPE_REXPR_MEMBER:
    case AST_NODE_TYPE_REXPR_BUILTIN:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
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
    if (AST_NODE_TYPE_REXPR_MEMBER == node->type) {
        return ast_node_is_rexpr_to_item(node->u.rexpr_link.link->dst_expr);
    }
    switch (node->type) {
    case AST_NODE_TYPE_REXPR_AS_TYPE:
    case AST_NODE_TYPE_REXPR_FIELD:
    case AST_NODE_TYPE_REXPR_MEMBER:
    case AST_NODE_TYPE_REXPR_FILE:
    case AST_NODE_TYPE_REXPR_SELF:
        return TRUE;
    default:
        return FALSE;
    }
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
    case AST_NODE_TYPE_TYPENAME:
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
ast_node_has_interpreter(const struct ast_node *node)
{
    return (ast_node_is_origin_container(node)
            && NULL != node->u.item.filter);
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

const struct ast_node *
ast_node_get_as_type(const struct ast_node *node)
{
    const struct ast_node *filter;

    assert(ast_node_is_item(node));
    filter = node->u.item.filter;
    if (NULL != filter && AST_NODE_TYPE_REXPR_AS_TYPE == filter->type) {
        return node->u.item.filter->u.rexpr_as_type.as_type;
    }
    return node;
}

int64_t
ast_node_get_min_span_size(const struct ast_node *node)
{
    switch (node->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_BYTE_ARRAY:
        assert(-1 != node->u.item.min_span_size);
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
        if (NULL == node->u.array.value_count) {
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
        target = node->u.array.value_type;
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
        target = node->u.array.value_type;
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


/*
 * debug
 */

static void
dump_ast_recur(struct ast_node *node, int depth,
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
dump_ast(struct ast_node *root, FILE *stream)
{
    dump_ast_recur(root, 0, NULL, stream);
}

void
dump_block(const struct ast_node *block, FILE *stream)
{
    dump_block_recur(block, 0, NULL, stream);
}

static void
dump_ast_rexpr(const struct ast_node *node, FILE *stream) {
    fprintf(stream, "value_type: %s, dpath_type: %s",
            expr_value_type_str(node->u.rexpr.value_type),
            expr_dpath_type_str(node->u.rexpr.dpath_type));
}

#define INDENT_N_SPACES   2

static void
dump_target_item(const struct ast_node *node, int depth,
                 struct list_of_visible_refs *visible_refs, FILE *stream)
{
    if (NULL != node->u.rexpr.target_item) {
        fprintf(stream, "\n%*s\\_ target:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_type(node->u.rexpr.target_item,
                      depth + 2, visible_refs, stream);
        fprintf(stream, "\n");
    }
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
              struct list_of_visible_refs *visible_refs, FILE *stream)
{
    if (NULL != node->u.item.filter) {
        fprintf(stream, "%*s\\_ filter:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_recur(node->u.item.filter, depth + 2,
                       visible_refs, stream);
    }
}

static void
dump_ast_container(struct ast_node *node, int depth,
                   struct list_of_visible_refs *visible_refs, FILE *stream)
{
    dump_ast_item(node, depth, visible_refs, stream);
}

static void
dump_ast_recur(struct ast_node *node, int depth,
               struct list_of_visible_refs *visible_refs, FILE *stream)
{
    static struct ast_node dummy_empty_node = {
        .type = AST_NODE_TYPE_NONE
    };

    if (NULL == node) {
        node = &dummy_empty_node;
    }
    dump_ast_type(node, depth, visible_refs, stream);
    if (node == &dummy_empty_node
        || 0 != (node->flags & ASTFLAG_PROCESSING)) {
        fprintf(stream, "\n");
        return ;
    }
    node->flags |= ASTFLAG_PROCESSING;
    switch (node->type) {
    case AST_NODE_TYPE_INTEGER:
        fprintf(stream, "%"PRIi64"\n", node->u.integer);
        break ;
    case AST_NODE_TYPE_BOOLEAN:
        fprintf(stream, "%s\n", (node->u.boolean ? "true" : "false"));
        break ;
    case AST_NODE_TYPE_STRING:
        print_expr_string(&node->u.string, stream);
        fputc('\n', stream);
        break ;
    case AST_NODE_TYPE_IDENTIFIER:
        fprintf(stream, "\"%s\"\n", node->u.identifier);
        break ;
    case AST_NODE_TYPE_TYPENAME:
        fprintf(stream, "\"%s\"\n", node->u.type_name.name);
        dump_ast_item(node, depth, visible_refs, stream);
        break ;
    case AST_NODE_TYPE_FILTER: {
        struct param *param;

        fprintf(stream, "\n%*s\\_ type:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_recur(node->u.filter.filter_type, depth + 2,
                       visible_refs, stream);
        fprintf(stream, "%*s\\_ params:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        STAILQ_FOREACH(param, node->u.filter.param_list, list) {
            fprintf(stream, "%*s\\_ \"%s\":\n",
                    (depth + 2) * INDENT_N_SPACES, "", param->name);
            dump_ast_recur(param->value, depth + 3, visible_refs, stream);
        }
        if (NULL != node->u.filter.target) {
            fprintf(stream, "%*s\\_ target:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_recur(node->u.filter.target,
                           depth + 2, visible_refs, stream);
            fprintf(stream, "\n");
        } else {
            fprintf(stream, "%*s\\_ target: <none>\n",
                    (depth + 1) * INDENT_N_SPACES, "");
        }
        break ;
    }
    case AST_NODE_TYPE_REXPR_INTERPRETER: {
        const struct interpreter *interpreter;
        struct interpreter_param_def *param_def;
        struct ast_node *param;
        int i;

        dump_ast_rexpr(node,stream);
        interpreter = node->u.rexpr_interpreter.interpreter;
        fprintf(stream, " name: %s\n%*s\\_ interpreter params:\n",
                interpreter->name, (depth + 1) * INDENT_N_SPACES, "");
        param_def = STAILQ_FIRST(&interpreter->param_list);
        for (i = 0; i < node->u.rexpr_interpreter.interpreter->n_params;
             ++i) {
            param = INTERPRETER_RCALL_PARAM(node, i);
            fprintf(stream, "%*s\\_ \"%s\" [%d]:\n",
                    (depth + 2) * INDENT_N_SPACES, "",
                    param_def->name, i);
            dump_ast_recur(param, depth + 3, visible_refs, stream);
            param_def = STAILQ_NEXT(param_def, list);
        }
        break ;
    }
    case AST_NODE_TYPE_BLOCK_DEF:
        fprintf(stream, "block type: %s, min span size: %s%s%s\n",
                block_type_str(node->u.block_def.type),
                span_size_str(node->u.item.min_span_size),
                (0 != (node->flags & ASTFLAG_IS_SPAN_SIZE_DYNAMIC) ?
                 " (dynamic span)" : ""),
                (0 != (node->flags & ASTFLAG_IS_USED_SIZE_DYNAMIC) ?
                 " (dynamic used)" : ""));
        dump_block_recur(node, depth + 1, visible_refs, stream);
        break ;
    case AST_NODE_TYPE_ARRAY:
        fprintf(stream, "\n%*s\\_ min span size: %s%s%s, value type:\n",
                (depth + 1) * INDENT_N_SPACES, "",
                span_size_str(node->u.item.min_span_size),
                (0 != (node->flags & ASTFLAG_IS_SPAN_SIZE_DYNAMIC) ?
                 " (dynamic span)" : ""),
                (0 != (node->flags & ASTFLAG_IS_USED_SIZE_DYNAMIC) ?
                 " (dynamic used)" : ""));
        dump_ast_recur(node->u.array.value_type, depth + 2, visible_refs,
                       stream);
        fprintf(stream, "%*s\\_ value count:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_recur(node->u.array.value_count, depth + 2, visible_refs,
                       stream);
        dump_ast_container(node, depth, visible_refs, stream);
        break ;
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_FILTERED:
        fprintf(stream, "\n%*s\\_ static node (%s)\n",
                (depth + 1) * INDENT_N_SPACES, "",
                ast_node_type_str(node->type));
        break ;
    case AST_NODE_TYPE_BYTE:
        fprintf(stream, "\n");
        dump_ast_item(node, depth, visible_refs, stream);
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
        fprintf(stream, "\n%*s\\_ byte count:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_recur(node->u.byte_array.size, depth + 2, visible_refs,
                       stream);
        dump_ast_container(node, depth, visible_refs, stream);
        break ;
    case AST_NODE_TYPE_CONDITIONAL:
        assert(NULL != visible_refs); /* must be in a block */
        fprintf(stream, "\n%*s\\_ condition expr:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_recur(node->u.conditional.cond_expr,
                       depth + 2, visible_refs, stream);
        if (NULL != node->u.conditional.outer_cond) {
            fprintf(stream, "%*s\\_ outer conditional:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_recur(node->u.conditional.outer_cond, depth + 2,
                           visible_refs, stream);
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
        fprintf(stream, "\n");
        dump_ast_recur(node->u.op.operands[0], depth + 1, visible_refs,
                       stream);
        dump_ast_recur(node->u.op.operands[1], depth + 1, visible_refs,
                       stream);
        break ;
    case AST_NODE_TYPE_OP_UPLUS:
    case AST_NODE_TYPE_OP_UMINUS:
    case AST_NODE_TYPE_OP_LNOT:
    case AST_NODE_TYPE_OP_BWNOT:
    case AST_NODE_TYPE_OP_SIZEOF:
    case AST_NODE_TYPE_OP_ADDROF:
    case AST_NODE_TYPE_OP_FILTER:
        fprintf(stream, "\n");
        dump_ast_recur(node->u.op.operands[0], depth + 1, visible_refs,
                       stream);
        break ;
    case AST_NODE_TYPE_OP_SUBSCRIPT:
        fprintf(stream, "\n");
        dump_ast_recur(node->u.op_subscript_common.anchor_expr, depth + 1,
                       visible_refs, stream);
        dump_ast_recur(node->u.op_subscript.index.key, depth + 1,
                       visible_refs, stream);
        if (NULL != node->u.op_subscript.index.twin) {
            fprintf(stream, "%*s\\_ twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_recur(node->u.op_subscript.index.twin, depth + 1,
                           visible_refs, stream);
        }
        break ;
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
        fprintf(stream, "\n");
        dump_ast_recur(node->u.op_subscript_common.anchor_expr, depth + 1,
                       visible_refs, stream);
        if (NULL != node->u.op_subscript_slice.start.key) {
            fprintf(stream, "%*s\\_ start index:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_recur(node->u.op_subscript_slice.start.key, depth + 1,
                           visible_refs, stream);
        }
        if (NULL != node->u.op_subscript_slice.start.twin) {
            fprintf(stream, "%*s\\_ start twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_recur(node->u.op_subscript_slice.start.twin, depth + 1,
                           visible_refs, stream);
        }
        if (NULL != node->u.op_subscript_slice.end.key) {
            fprintf(stream, "%*s\\_ end index:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_recur(node->u.op_subscript_slice.end.key, depth + 1,
                           visible_refs, stream);
        }
        if (NULL != node->u.op_subscript_slice.end.twin) {
            fprintf(stream, "%*s\\_ end twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_recur(node->u.op_subscript_slice.end.twin, depth + 1,
                           visible_refs, stream);
        }
        break ;
    case AST_NODE_TYPE_OP_FCALL: {
        struct func_param *func_param;

        fprintf(stream, "\n");
        dump_ast_recur(node->u.op_fcall.func, depth + 1, visible_refs,
                       stream);

        fprintf(stream, "\n%*s\\_ params:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        STAILQ_FOREACH(func_param,
                       node->u.op_fcall.func_params, list) {
            dump_ast_recur(func_param->expr, depth + 1, visible_refs,
                           stream);
        }
        break ;
    }
    case AST_NODE_TYPE_REXPR_OP_FCALL: {
        struct func_param *func_param;

        fprintf(stream, "\n");
        dump_ast_recur(node->u.op_fcall.func, depth + 1, visible_refs,
                       stream);

        fprintf(stream, "\n%*s\\_ params:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        STAILQ_FOREACH(func_param,
                       node->u.rexpr_op_fcall.func_params, list) {
            dump_ast_recur(func_param->expr, depth + 1, visible_refs,
                           stream);
        }
        break ;
    }
    case AST_NODE_TYPE_REXPR_NATIVE:
        switch (node->u.rexpr_native.rexpr.value_type) {
        case EXPR_VALUE_TYPE_INTEGER:
            fprintf(stream, "%"PRIi64"\n", node->u.rexpr_native.value.integer);
            break ;
        case EXPR_VALUE_TYPE_BOOLEAN:
            fprintf(stream, "%s\n",
                    (node->u.rexpr_native.value.boolean ?
                     "true" : "false"));
            break ;
        case EXPR_VALUE_TYPE_STRING:
            print_expr_string(&node->u.rexpr_native.value.string, stream);
            fputc('\n', stream);
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
        dump_ast_rexpr(node, stream);
        fprintf(stream, "\n");
        dump_ast_recur(node->u.rexpr_op.op.operands[0], depth + 1,
                       visible_refs, stream);
        dump_ast_recur(node->u.rexpr_op.op.operands[1], depth + 1,
                       visible_refs, stream);
        break ;
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        dump_ast_rexpr(node, stream);
        fprintf(stream, "\n%*s\\_ target:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_recur(node->u.rexpr_filter.target, depth + 2,
                       visible_refs, stream);
        fprintf(stream, "%*s\\_ as type:\n",
                (depth + 1) * INDENT_N_SPACES, "");
        dump_ast_recur(node->u.rexpr_as_type.as_type, depth + 2,
                       visible_refs, stream);
        break ;
    case AST_NODE_TYPE_REXPR_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT:
    case AST_NODE_TYPE_REXPR_OP_SIZEOF:
    case AST_NODE_TYPE_REXPR_OP_ADDROF:
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        dump_ast_rexpr(node, stream);
        fprintf(stream, "\n");
        dump_ast_recur(node->u.rexpr_op.op.operands[0], depth + 1,
                       visible_refs, stream);
        break ;
    case AST_NODE_TYPE_REXPR_FIELD:
    case AST_NODE_TYPE_REXPR_MEMBER:
    case AST_NODE_TYPE_REXPR_BUILTIN:
        dump_ast_rexpr(node, stream);
        dump_target_item(node, depth, visible_refs, stream);
        if (NULL != node->u.rexpr_field.anchor_expr) {
            fprintf(stream, "%*s\\_ prev_item:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_type(node->u.rexpr_field.anchor_expr, depth + 2,
                          visible_refs, stream);
            fprintf(stream, "\n");
        }
        break ;
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
        dump_ast_rexpr(node, stream);
        fprintf(stream, "\n");
        dump_ast_recur(node->u.rexpr_op_subscript_common.anchor_expr, depth + 1,
                       visible_refs, stream);
        dump_ast_recur(node->u.rexpr_op_subscript.index.key, depth + 1,
                       visible_refs, stream);
        if (NULL != node->u.rexpr_op_subscript.index.twin) {
            fprintf(stream, "%*s\\_ twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_recur(node->u.rexpr_op_subscript.index.twin, depth + 1,
                           visible_refs, stream);
        }
        break ;
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        dump_ast_rexpr(node, stream);
        fprintf(stream, "\n");
        dump_ast_recur(node->u.rexpr_op_subscript_common.anchor_expr, depth + 1,
                       visible_refs, stream);
        if (NULL != node->u.rexpr_op_subscript_slice.start.key) {
            fprintf(stream, "%*s\\_ start index:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_recur(node->u.rexpr_op_subscript_slice.start.key, depth + 1,
                           visible_refs, stream);
        }
        if (NULL != node->u.rexpr_op_subscript_slice.start.twin) {
            fprintf(stream, "%*s\\_ start twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_recur(node->u.rexpr_op_subscript_slice.start.twin, depth + 1,
                           visible_refs, stream);
        }
        if (NULL != node->u.rexpr_op_subscript_slice.end.key) {
            fprintf(stream, "%*s\\_ end index:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_recur(node->u.rexpr_op_subscript_slice.end.key, depth + 1,
                           visible_refs, stream);
        }
        if (NULL != node->u.rexpr_op_subscript_slice.end.twin) {
            fprintf(stream, "%*s\\_ end twin:\n",
                    (depth + 1) * INDENT_N_SPACES, "");
            dump_ast_recur(node->u.rexpr_op_subscript_slice.end.twin, depth + 1,
                           visible_refs, stream);
        }
        break ;
    case AST_NODE_TYPE_REXPR_FILE:
    case AST_NODE_TYPE_REXPR_SELF:
    case AST_NODE_TYPE_REXPR_STAR_WILDCARD:
        dump_ast_rexpr(node, stream);
        fprintf(stream, "\n");
        break ;
    case AST_NODE_TYPE_EXPR_FILE:
    case AST_NODE_TYPE_EXPR_SELF:
    case AST_NODE_TYPE_EXPR_STAR_WILDCARD:
    case AST_NODE_TYPE_NONE:
        fprintf(stream, "\n");
        break ;
    }
    node->flags &= ~ASTFLAG_PROCESSING;
}

static void
dump_ast_type(const struct ast_node *node, int depth,
              struct list_of_visible_refs *visible_refs, FILE *stream)
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
        fprintf(stream, "%*s|- (%s) ", depth * INDENT_N_SPACES, "",
                ast_node_type_str(node->type));
        break ;
    case AST_NODE_TYPE_TYPENAME:
    case AST_NODE_TYPE_FILTER:
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
    case AST_NODE_TYPE_REXPR_FIELD:
    case AST_NODE_TYPE_REXPR_MEMBER:
    case AST_NODE_TYPE_REXPR_BUILTIN:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
    case AST_NODE_TYPE_REXPR_OP_FCALL:
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        /* intermediate node */
        fprintf(stream, "%*s\\_ (%s) ", depth * INDENT_N_SPACES, "",
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
        fprintf(stream, "<%s> ", type_name);
    } else if (noname) {
        fprintf(stream, "<noname> ");
    }
}

static void
dump_block_recur(const struct ast_node *block,
                 int depth,
                 struct list_of_visible_refs *outer_refs,
                 FILE *stream)
{
    if (NULL == block) {
        return ;
    }
    dump_block_stmt_list_recur(block, &block->u.block_def.block_stmt_list,
                               depth, outer_refs, stream);
}

static void
dump_block_stmt_list_recur(const struct ast_node *block,
                           const struct block_stmt_list *block_lists,
                           int depth,
                           struct list_of_visible_refs *outer_refs,
                           FILE *stream)
{
    struct list_of_visible_refs visible_refs;
    const struct named_type *named_type;
    const struct statement *stmt;
    const struct field *field;
    const struct link *link;

    /* add current refs to the chain of visible refs */
    visible_refs.outer_refs = outer_refs;
    visible_refs.cur_block = block;
    visible_refs.cur_lists = block_lists;

    fprintf(stream, "%*s\\_ named types:\n", depth * INDENT_N_SPACES, "");
    STAILQ_FOREACH(named_type, block_lists->named_type_list, list) {
        fprintf(stream, "%*s\\_ \"%s\"\n", (depth + 1) * INDENT_N_SPACES, "",
                named_type->name);
    }
    fprintf(stream, "%*s\\_ fields:\n", depth * INDENT_N_SPACES, "");
    TAILQ_FOREACH(stmt, block_lists->field_list, list) {
        field = (const struct field *)stmt;
        fprintf(stream, "%*s\\_ name \"%s\", field type:\n",
                (depth + 1) * INDENT_N_SPACES, "",
                (NULL != field->nstmt.name ? field->nstmt.name : "N/A"));
        dump_ast_recur(field->field_type, depth + 2, &visible_refs,
                       stream);
    }
    fprintf(stream, "%*s\\_ links:\n", depth * INDENT_N_SPACES, "");
    TAILQ_FOREACH(stmt, block_lists->link_list, list) {
        link = (const struct link *)stmt;
        fprintf(stream, "%*s\\_ name \"%s\"\n",
                (depth + 1) * INDENT_N_SPACES, "",
                link->nstmt.name);
        fprintf(stream, "%*s\\_ dst expr:\n",
                (depth + 2) * INDENT_N_SPACES, "");
        dump_ast_recur(link->dst_expr, depth + 3, &visible_refs,
                       stream);
    }
}

void
dump_ast_node_input_text(const struct ast_node *node,
                         struct bitpunch_schema_hdl *schema,
                         FILE *stream)
{
    const char *text_start;
    const char *text_end;

    text_start = schema->df_data + node->loc.start_offset;
    text_end = schema->df_data + node->loc.end_offset;

    fwrite(text_start, 1, text_end - text_start, stream);
    fputs("\n", stream);
}
