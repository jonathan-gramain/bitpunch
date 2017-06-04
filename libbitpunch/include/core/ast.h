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

#ifndef __AST_H__
#define __AST_H__

#include <stdio.h>

struct ast_node;
struct bitpunch_schema_hdl;

int
resolve_schema_references(struct bitpunch_schema_hdl *schema);
int
resolve_user_expr(struct ast_node **expr_p,
                  const struct ast_node *top_level_block);
int
ast_node_is_rexpr(const struct ast_node *node);
int
ast_node_is_container(const struct ast_node *node);
int
ast_node_is_origin_container(const struct ast_node *node);
int
ast_node_is_byte_container(const struct ast_node *node);
int
ast_node_is_slice_container(const struct ast_node *node);
int
ast_node_is_item(const struct ast_node *node);
int
ast_node_has_interpreter(const struct ast_node *node);
int64_t
ast_node_get_min_span_size(const struct ast_node *node);
int
ast_node_is_slack(const struct ast_node *node);
int
ast_node_is_indexed(const struct ast_node *node);
struct ast_node *
ast_node_get_key_expr(const struct ast_node *node);
enum expr_value_type
ast_node_get_key_type(const struct ast_node *node);
void
dump_ast(const struct ast_node *root, FILE *stream);
void
dump_block(const struct ast_node *block, FILE *stream);
void
dump_ast_node_input_text(const struct ast_node *node,
                         struct bitpunch_schema_hdl *schema,
                         FILE *stream);

#endif /* defined __AST_H__ */
