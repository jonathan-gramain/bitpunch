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

#include "core/parser.h"
#include "api/bitpunch_api.h"

struct bitpunch_env *
bitpunch_env_new(void)
{
    struct bitpunch_env *env;

    env = new_safe(struct bitpunch_env);
    env->ast_root = ast_node_hdl_create(AST_NODE_TYPE_FILTER_DEF, NULL);
    env->ast_root->ndat->u.filter_def.filter_type = "scope";
    init_block_stmt_list(&env->ast_root->ndat->u.scope_def.block_stmt_list);
    return env;
}

void
bitpunch_env_free(
    struct bitpunch_env *env)
{
    free(env->ast_root);
    free(env);
}

static void
env_add_named_expr(
    struct bitpunch_env *env,
    const char *name,
    struct ast_node_hdl *expr)
{
    struct scope_def *scope_def;
    struct named_expr *named_expr;

    env->flags &= ~BITPUNCH_ENV_COMPILED;
    scope_def = &env->ast_root->ndat->u.scope_def;
    named_expr = new_safe(struct named_expr);
    named_expr->nstmt.name = strdup_safe(name);
    named_expr->expr = expr;

    TAILQ_INSERT_TAIL(scope_def->block_stmt_list.named_expr_list,
                      (struct statement *)named_expr, list);
}

void
bitpunch_env_add_data_source(
    struct bitpunch_env *env,
    const char *name,
    struct bitpunch_data_source *ds)
{
    struct ast_node_hdl *source_node;

    source_node = ast_node_hdl_create(AST_NODE_TYPE_REXPR_DATA_SOURCE, NULL);
    source_node->ndat->u.rexpr_data_source.data_source = ds;
    env_add_named_expr(env, name, source_node);
}
