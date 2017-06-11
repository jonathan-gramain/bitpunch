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

#ifndef __EXPR_H__
#define __EXPR_H__

#include <stdio.h>

#include "utils/port.h"
#include "api/bitpunch-structs.h"
#include "core/parser.h"

enum ast_node_type;
struct ast_node;
struct browse_state;
struct box;
struct func_param_list;
struct link;

enum expr_value_type {
    EXPR_VALUE_TYPE_UNSET = 0,
    EXPR_VALUE_TYPE_INTEGER,
    EXPR_VALUE_TYPE_BOOLEAN,
    EXPR_VALUE_TYPE_STRING,
    EXPR_VALUE_TYPE_BYTES,
};

struct expr_value_string {
    const char *str;
    int64_t len;
};

struct expr_value_bytes {
    const char *buf;
    int64_t len;
};

union expr_value {
    int64_t integer;
    int boolean;
    struct expr_value_string string;
    struct expr_value_bytes bytes;
};

enum expr_dpath_type {
    EXPR_DPATH_TYPE_UNSET = 0,
    EXPR_DPATH_TYPE_NONE,
    EXPR_DPATH_TYPE_ITEM,
    EXPR_DPATH_TYPE_CONTAINER,
};

union expr_dpath {
    struct expr_dpath_item {
        struct tracker *tk;
    } item;
    struct expr_dpath_container {
        struct box *box;
    } container;
};

const struct expr_builtin_fn *
expr_lookup_builtin_fn(const char *name,
                       const struct ast_node *object);
const char *
expr_find_first_builtin(const char *prefix,
                        const struct ast_node *object);
const char *
expr_find_next_builtin(const char *name,
                       const struct ast_node *object);

void
expr_dpath_destroy_item(union expr_dpath dpath);
void
expr_dpath_destroy_container(union expr_dpath dpath);
bitpunch_status_t
expr_dpath_to_tracker(enum expr_dpath_type type, union expr_dpath dpath,
                      struct tracker **tkp);
bitpunch_status_t
expr_dpath_to_box(enum expr_dpath_type type, union expr_dpath dpath,
                  struct box **boxp,
                  struct browse_state *bst);
bitpunch_status_t
expr_dpath_to_container(enum expr_dpath_type type,
                        union expr_dpath dpath,
                        union expr_dpath *dpathp,
                        struct browse_state *bst);
bitpunch_status_t
expr_dpath_to_item(enum expr_dpath_type type,
                   union expr_dpath dpath,
                   union expr_dpath *dpathp,
                   struct browse_state *bst);
bitpunch_status_t
expr_dpath_to_dpath(enum expr_dpath_type src_type,
                    union expr_dpath src_dpath,
                    enum expr_dpath_type dst_type,
                    union expr_dpath *dst_dpathp,
                    struct browse_state *bst);
struct box *
expr_dpath_get_parent_box(enum expr_dpath_type type,
                          union expr_dpath dpath);
bitpunch_status_t
expr_dpath_get_size(enum expr_dpath_type type, union expr_dpath dpath,
                    int64_t *dpath_sizep,
                    struct browse_state *bst);
const struct ast_node *
expr_dpath_get_node(enum expr_dpath_type type, union expr_dpath dpath);
struct track_path
expr_dpath_get_track_path(enum expr_dpath_type type,
                          union expr_dpath dpath);
int
expr_dpath_is(enum expr_dpath_type type1,
              union expr_dpath dpath1,
              enum expr_dpath_type type2,
              union expr_dpath dpath2);
void
expr_dpath_find_common_ancestor(enum expr_dpath_type type1,
                                union expr_dpath dpath1,
                                enum expr_dpath_type type2,
                                union expr_dpath dpath2,
                                enum expr_dpath_type *ancestor1_typep,
                                union expr_dpath *ancestor1_dpathp,
                                enum expr_dpath_type *ancestor2_typep,
                                union expr_dpath *ancestor2_dpathp);

static inline void
expr_dpath_destroy(enum expr_dpath_type type, union expr_dpath dpath)
{
    switch (type) {
    case EXPR_DPATH_TYPE_ITEM:
        expr_dpath_destroy_item(dpath);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        expr_dpath_destroy_container(dpath);
        break ;
    default:
        break ;
    }
}

static inline void
expr_value_destroy(enum expr_value_type type, union expr_value value)
{
    // nothing to destroy for now
}

int
expr_value_cmp_integer(union expr_value expr1, union expr_value expr2);
int
expr_value_cmp_string(union expr_value expr1, union expr_value expr2);
int
expr_value_cmp_bytes(union expr_value expr1, union expr_value expr2);
int
expr_value_cmp(enum expr_value_type type,
               union expr_value expr1, union expr_value expr2);

bitpunch_status_t
expr_evaluate_value(struct ast_node *expr, struct box *scope,
                    union expr_value *eval_valuep);
bitpunch_status_t
expr_evaluate_dpath(struct ast_node *expr, struct box *scope,
                    union expr_dpath *eval_dpathp);
bitpunch_status_t
link_evaluate_dpath(const struct link *link, struct box *scope,
                    enum expr_dpath_type *dpath_typep,
                    union expr_dpath *eval_dpathp);
bitpunch_status_t
link_evaluate_value(const struct link *link, struct box *scope,
                    enum expr_value_type *value_typep,
                    union expr_value *eval_valuep);
bitpunch_status_t
evaluate_conditional(struct ast_node *cond, struct box *scope,
                     int *evalp);
bitpunch_status_t
expr_read_dpath_value(struct ast_node *expr, union expr_dpath dpath,
                      union expr_value *expr_valuep);
const char *
expr_value_type_str(enum expr_value_type type);

const char *
expr_dpath_type_str(enum expr_dpath_type type);

void
expr_value_to_hashable(enum expr_value_type type,
                       union expr_value value,
                       const char **bufp, int64_t *lenp);

#endif /*__EXPR_H__*/
