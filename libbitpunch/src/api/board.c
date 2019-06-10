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

/**
 * @file
 * @brief bitpunch schema API
 */

#include <assert.h>

#include "core/parser.h"
#include "core/browse.h"
#include "core/scope.h"
#include "filters/data_source.h"
#include "api/bitpunch_api.h"

static void
board_refresh(
    struct bitpunch_board *board);

struct bitpunch_board *
bitpunch_board_new(void)
{
    struct bitpunch_board *board;

    board = new_safe(struct bitpunch_board);
    board->ast_root = ast_node_hdl_create_scope(NULL);
    board->extern_defs = new_safe(struct statement_list);
    TAILQ_INIT(board->extern_defs);
    return board;
}

void
bitpunch_board_free(
    struct bitpunch_board *board)
{
    free(board->ast_root);
    free(board);
}

static void
board_add_named_expr(
    struct bitpunch_board *board,
    const char *name,
    struct ast_node_hdl *expr)
{
    board_refresh(board);
    scope_add_named_expr(&board->ast_root->ndat->u.scope_def, name, expr);
}

void
bitpunch_board_add_let_expression(
    struct bitpunch_board *board,
    const char *name,
    struct ast_node_hdl *expr)
{
    board_add_named_expr(board, name, expr);
}

static int
board_remove_named_exprs_with_name(
    struct bitpunch_board *board,
    const char *name)
{
    int n_removed;

    n_removed = scope_remove_named_exprs_with_name(
        &board->ast_root->ndat->u.scope_def, name);
    if (n_removed > 0) {
        board_refresh(board);
    }
    return n_removed;
}

int
bitpunch_board_remove_by_name(
    struct bitpunch_board *board,
    const char *name)
{
    return board_remove_named_exprs_with_name(board, name);
}

static void
board_add_external_def(
    struct bitpunch_board *board,
    const char *name,
    struct ast_node_hdl *external_def)
{
    struct named_expr *external_item;

    external_item = new_safe(struct named_expr);
    external_item->nstmt.name = strdup_safe(name);
    external_item->expr = external_def;
    TAILQ_INSERT_TAIL(board->extern_defs,
                      (struct statement *)external_item, list);
}

void
bitpunch_board_add_external_def(
    struct bitpunch_board *board,
    const char *name,
    struct ast_node_hdl *external_def)
{
    board_add_external_def(board, name, external_def);
}

void
bitpunch_board_use_spec(
    struct bitpunch_board *board,
    struct ast_node_hdl *spec)
{
    assert(ast_node_is_scope_def(spec));
    bitpunch_board_forget_spec(board);

    scope_import_all_named_exprs_from_scope(
        &board->ast_root->ndat->u.scope_def, &spec->ndat->u.scope_def);
    board->used_spec = spec;
}

void
bitpunch_board_forget_spec(
    struct bitpunch_board *board)
{
    if (NULL != board->used_spec) {
        scope_remove_all_named_exprs_in_scope(
            &board->ast_root->ndat->u.scope_def,
            &board->used_spec->ndat->u.scope_def);
        board->used_spec = NULL;
        board_refresh(board);
    }
}

bitpunch_status_t
bitpunch_board_add_expr(
    struct bitpunch_board *board,
    const char *name,
    const char *expr)
{
    struct ast_node_hdl *expr_node;

    if (-1 == bitpunch_parse_expr(expr, &expr_node)) {
        return BITPUNCH_INVALID_PARAM;
    }
    board_add_named_expr(board, name, expr_node);
    return BITPUNCH_OK;
}

struct ast_node_hdl *
bitpunch_board_get_item(
    struct bitpunch_board *board,
    const char *name)
{
    struct block_stmt_list *lists;
    struct named_expr *named_expr;

    lists = &board->ast_root->ndat->u.scope_def.block_stmt_list;
    STATEMENT_FOREACH(named_expr, named_expr, lists->named_expr_list, list) {
        if (0 == strcmp(name, named_expr->nstmt.name)) {
            return named_expr->expr;
        }
    }
    return NULL;
}

struct ast_node_hdl *
bitpunch_board_get_external_item(
    struct bitpunch_board *board,
    const char *name)
{
    struct named_expr *named_expr;

    STATEMENT_FOREACH(named_expr, named_expr, board->extern_defs, list) {
        if (0 == strcmp(name, named_expr->nstmt.name)) {
            return named_expr->expr;
        }
    }
    return NULL;
}

bitpunch_status_t
bitpunch_eval_expr(
    struct bitpunch_board *board,
    const char *expr,
    struct box *scope,
    enum bitpunch_eval_flag flags,
    struct ast_node_hdl **parsed_exprp,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct bitpunch_error **errp)
{
    struct ast_node_hdl *expr_node = NULL;
    struct box *_scope = NULL;
    bitpunch_status_t bt_ret = BITPUNCH_ERROR;

    assert(NULL != expr);

    if (-1 == bitpunch_parse_expr(expr, &expr_node)) {
        return BITPUNCH_INVALID_PARAM;
    }
    if (NULL == scope && NULL != board) {
        _scope = box_new_root_box(board->ast_root, board);
        if (NULL == _scope) {
            // on resolve error, the board may need a refresh since
            // its internal nodes may have been affected by failing
            // resolve/compile stage
            board_refresh(board);
            goto end;
        }
    } else {
        _scope = scope;
    }
    if (-1 == bitpunch_resolve_expr(expr_node, _scope)) {
        // on resolve error, the board may need a refresh since its
        // internal nodes may have been affected by failing
        // resolve/compile stage
        board_refresh(board);
        goto end;
    }
    if (NULL != parsed_exprp) {
        *parsed_exprp = expr_node;
    }
    bt_ret = expr_evaluate(expr_node, _scope, board,
                           0 != (flags & BITPUNCH_EVAL_DPATH_XOR_VALUE) ?
                           EXPR_EVALFLAG_DPATH_XOR_VALUE : 0u,
                           valuep, dpathp, errp);

  end:
    if (NULL == scope) {
        box_delete(_scope);
    }
    /* TODO free expr_node */
    return bt_ret;
}

static void
board_refresh(
    struct bitpunch_board *board)
{
    struct parser_ctx *used_spec_parser_ctx = NULL;
    struct named_expr *named_expr;
    struct parser_ctx *parser_ctx;
    struct ast_node_hdl *used_spec;

    if (NULL == board) {
        return ;
    }
    if (NULL != board->used_spec) {
        used_spec_parser_ctx = board->used_spec->loc.parser_ctx;
    }

    board->ast_root->resolved_tags = 0;
    dep_resolver_node_init(&board->ast_root->dr_node);

    STATEMENT_FOREACH(
        named_expr, named_expr,
        board->ast_root->ndat->u.scope_def.block_stmt_list.named_expr_list,
        list) {
        parser_ctx = named_expr->expr->loc.parser_ctx;
        if (NULL == parser_ctx || parser_ctx == used_spec_parser_ctx) {
            continue ;
        }
        // re-parse original text to forget about old compilation state
        (void) bitpunch_parse(parser_ctx, &named_expr->expr);
    }
    if (NULL != board->used_spec) {
        if (-1 == bitpunch_parse(used_spec_parser_ctx, &used_spec)) {
            return ;
        }
        scope_remove_all_named_exprs_in_scope(
            &board->ast_root->ndat->u.scope_def,
            &board->used_spec->ndat->u.scope_def);
        scope_import_all_named_exprs_from_scope(
            &board->ast_root->ndat->u.scope_def,
            &used_spec->ndat->u.scope_def);
        board->used_spec = used_spec;
    }
}
