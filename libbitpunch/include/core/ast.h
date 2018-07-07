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

#include "utils/dep_resolver.h"

struct ast_node_data;
struct ast_node_hdl;
struct block_stmt_list;
struct dpath_node;
struct bitpunch_schema_hdl;
struct box;
struct field;
struct parser_location;
enum statement_type;

enum resolve_expect_mask {
    RESOLVE_EXPECT_TYPE             = (1u<<0),
    RESOLVE_EXPECT_FILTER           = (1u<<1),
    RESOLVE_EXPECT_VALUE_EXPRESSION = (1u<<2),
    RESOLVE_EXPECT_DPATH_EXPRESSION = (1u<<3),
    RESOLVE_EXPECT_EXPRESSION = (RESOLVE_EXPECT_DPATH_EXPRESSION |
                                 RESOLVE_EXPECT_VALUE_EXPRESSION),
};

enum resolve_identifiers_tag {
    RESOLVE_TYPE_IDENTIFIERS = (1u<<0),
    RESOLVE_EXPRESSION_IDENTIFIERS = (1u<<1),
    RESOLVE_ALL_IDENTIFIERS = ~0u,
};

enum compile_tag {
    COMPILE_TAG_NODE_TYPE = (1u<<0),
    COMPILE_TAG_NODE_SPAN_SIZE = (1u<<1),
};

struct compile_ctx {
    struct dep_resolver *dep_resolver;
    struct compile_req *current_req;
    struct dep_resolver_node *current_node;
    dep_resolver_tagset_t current_tags;
    const char *current_node_family;
    FILE *deps_dot;
};

int
bitpunch_compile_schema(struct bitpunch_schema_hdl *schema);

int
identifier_is_visible_in_block_stmt_lists(
    enum statement_type stmt_mask,
    const char *identifier,
    const struct block_stmt_list *stmt_lists);

int
compile_node(struct ast_node_hdl *node,
             struct compile_ctx *ctx,
             dep_resolver_tagset_t tags_pre,
             dep_resolver_tagset_t tags_post,
             enum resolve_expect_mask expect_mask);
int
compile_expr(struct ast_node_hdl *node, struct compile_ctx *ctx,
             int is_dependency);
int
compile_dpath(struct dpath_node *node,
              struct compile_ctx *ctx,
              dep_resolver_tagset_t tags_pre,
              dep_resolver_tagset_t tags_post);

int
compile_continue(struct compile_ctx *ctx);

int
resolve_user_expr(struct ast_node_hdl *expr, struct box *scope);
void
dpath_node_reset(struct dpath_node *dpath);
int
ast_node_is_rexpr(const struct ast_node_hdl *node);
struct ast_node_hdl *
ast_node_get_target_item(struct ast_node_hdl *node);
struct ast_node_hdl *
ast_node_get_target_type(struct ast_node_hdl *node);
struct ast_node_hdl *
ast_node_get_target_filter(struct ast_node_hdl *node);
struct ast_node_hdl *
ast_node_get_named_expr_target(struct ast_node_hdl *node);
int
ast_node_is_container(const struct ast_node_hdl *node);
int
ast_node_is_origin_container(const struct ast_node_hdl *node);
int
ast_node_is_byte_container(const struct ast_node_hdl *node);
int
ast_node_is_subscriptable_container(const struct ast_node_hdl *node);
int
ast_node_is_slice_container(const struct ast_node_hdl *node);
int
ast_node_is_item(const struct ast_node_hdl *node);
int
ast_node_is_type(const struct ast_node_hdl *node);
int
ast_node_is_filter(const struct ast_node_hdl *node);
struct ast_node_hdl *
ast_node_filter_get_last_in_chain(struct ast_node_hdl *filter);
struct ast_node_hdl *
ast_node_get_as_type(struct ast_node_hdl *node);
int64_t
ast_node_get_min_span_size(const struct ast_node_hdl *node);
int
ast_node_is_slack(const struct ast_node_hdl *node);
struct ast_node_hdl *
block_get_first_attribute(const struct ast_node_hdl *filter,
                          const char *attr_name);
int
ast_node_is_indexed(const struct ast_node_hdl *node);
struct ast_node_hdl *
ast_node_get_key_expr(const struct ast_node_hdl *node);
enum expr_value_type
ast_node_get_key_type(const struct ast_node_hdl *node);
struct ast_node_hdl *
dpath_node_get_as_type(const struct dpath_node *dpath);
struct ast_node_hdl *
dpath_node_get_as_type__pre_compile_stage(const struct dpath_node *dpath);
struct ast_node_hdl *
dpath_node_get_target_filter(const struct dpath_node *dpath);
void
dump_ast_location(struct ast_node_hdl *node);
void
fdump_ast_location(struct ast_node_hdl *node, FILE *stream);
void
dump_ast_dot(struct ast_node_hdl *node,
             const char *node_family, dep_resolver_tagset_t tag);
void
fdump_ast_dot(struct ast_node_hdl *node,
              const char *node_family, dep_resolver_tagset_t tag,
              FILE *out);
void
dump_ast(struct ast_node_hdl *root);
void
fdump_ast(struct ast_node_hdl *root, FILE *stream);
void
dump_filter(const struct ast_node_hdl *filter, FILE *stream);
void
dump_ast_node_input_text(const struct ast_node_hdl *node,
                         struct bitpunch_schema_hdl *schema,
                         FILE *stream);

#endif /* defined __AST_H__ */
