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
    struct scope_def *scope_def;
    struct named_expr *named_expr;

    scope_def = &board->ast_root->ndat->u.scope_def;
    named_expr = new_safe(struct named_expr);
    named_expr->nstmt.name = strdup_safe(name);
    named_expr->expr = expr;

    TAILQ_INSERT_TAIL(scope_def->block_stmt_list.named_expr_list,
                      (struct statement *)named_expr, list);

    board->ast_root->resolved_tags = 0;
    dep_resolver_node_init(&board->ast_root->dr_node);
}

void
bitpunch_board_add_item(
    struct bitpunch_board *board,
    const char *name,
    struct ast_node_hdl *item)
{
    board_add_named_expr(board, name, item);
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
bitpunch_eval_expr2(struct bitpunch_board *board,
                    const char *expr,
                    expr_value_t *valuep, expr_dpath_t *dpathp,
                    struct tracker_error **errp)
{
    struct ast_node_hdl *expr_node = NULL;
    struct parser_ctx *parser_ctx = NULL;
    struct box *scope = NULL;
    bitpunch_status_t bt_ret = BITPUNCH_ERROR;

    assert(NULL != expr);

    if (-1 == bitpunch_parse_expr(expr, &expr_node, &parser_ctx)) {
        return -1;
    }
    scope = box_new_root_box(board->ast_root, board, FALSE);
    if (NULL == scope) {
        goto end;
    }
    box_acquire(scope);
    if (-1 == bitpunch_resolve_expr(expr_node, scope)) {
        goto end;
    }
    assert(ast_node_is_rexpr(expr_node));
    bt_ret = expr_evaluate(expr_node, scope, board, valuep, dpathp, errp);

  end:
    box_delete(scope);
    /* TODO free expr_node */
    free(parser_ctx);
    return bt_ret;
}
