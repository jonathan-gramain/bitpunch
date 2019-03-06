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

struct bitpunch_board *
bitpunch_board_new(void)
{
    struct bitpunch_board *board;

    board = new_safe(struct bitpunch_board);
    board->ast_root = ast_node_hdl_create_scope(NULL);
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
    scope_add_named_expr(&board->ast_root->ndat->u.scope_def, name, expr);

    board->ast_root->resolved_tags = 0;
    dep_resolver_node_init(&board->ast_root->dr_node);
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
        board->ast_root->resolved_tags = 0;
        dep_resolver_node_init(&board->ast_root->dr_node);
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
    }
}

bitpunch_status_t
bitpunch_board_add_expr(
    struct bitpunch_board *board,
    const char *name,
    const char *expr)
{
    struct ast_node_hdl *expr_node;
    bitpunch_status_t bt_ret;

    bt_ret = bitpunch_compile_expr(board, expr, &expr_node);
    if (BITPUNCH_OK == bt_ret) {
        board_add_named_expr(board, name, expr_node);
    }
    return bt_ret;
}

bitpunch_status_t
bitpunch_compile_expr(
    struct bitpunch_board *board,
    const char *expr,
    struct ast_node_hdl **expr_nodep)
{
    struct ast_node_hdl *expr_node = NULL;

    if (-1 == bitpunch_parse_expr(expr, &expr_node)) {
        return BITPUNCH_INVALID_PARAM;
    }
    *expr_nodep = expr_node;
    return BITPUNCH_OK;
}

bitpunch_status_t
bitpunch_eval_expr(
    struct bitpunch_board *board,
    const char *expr,
    struct box *scope,
    struct ast_node_hdl **parsed_exprp,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct tracker_error **errp)
{
    struct ast_node_hdl *expr_node = NULL;
    struct box *_scope = NULL;
    bitpunch_status_t bt_ret = BITPUNCH_ERROR;

    assert(NULL != expr);

    if (-1 == bitpunch_parse_expr(expr, &expr_node)) {
        return BITPUNCH_INVALID_PARAM;
    }
    if (NULL == scope && NULL != board) {
        _scope = box_new_root_box(board->ast_root, board, FALSE);
        if (NULL == _scope) {
            goto end;
        }
    } else {
        _scope = scope;
    }
    if (-1 == bitpunch_resolve_expr(expr_node, _scope)) {
        goto end;
    }
    assert(ast_node_is_rexpr(expr_node));
    if (NULL != parsed_exprp) {
        *parsed_exprp = expr_node;
    }
    bt_ret = expr_evaluate(expr_node, _scope, board, valuep, dpathp, errp);

  end:
    if (NULL == scope) {
        box_delete(_scope);
    }
    /* TODO free expr_node */
    return bt_ret;
}
