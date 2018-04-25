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

struct tracker_error;

enum ast_node_type;
struct ast_node_hdl;
struct browse_state;
struct box;
struct statement_list;
struct named_expr;

enum expr_value_type {
    EXPR_VALUE_TYPE_UNSET = 0,
    EXPR_VALUE_TYPE_INTEGER = (1<<0),
    EXPR_VALUE_TYPE_BOOLEAN = (1<<1),
    EXPR_VALUE_TYPE_STRING = (1<<2),
    EXPR_VALUE_TYPE_BYTES = (1<<3),
    EXPR_VALUE_TYPE_ANY = (EXPR_VALUE_TYPE_INTEGER |
                           EXPR_VALUE_TYPE_BOOLEAN |
                           EXPR_VALUE_TYPE_STRING |
                           EXPR_VALUE_TYPE_BYTES),
};

struct expr_value_from_box {
    struct box *box;
};

struct expr_value_string {
    struct expr_value_from_box from_box; /* inherits */
    const char *str;
    int64_t len;
};

struct expr_value_bytes {
    struct expr_value_from_box from_box; /* inherits */
    const char *buf;
    int64_t len;
};

struct expr_value {
    enum expr_value_type type;
    union {
        int64_t integer;
        int boolean;
        struct expr_value_from_box from_box;
        struct expr_value_string string;
        struct expr_value_bytes bytes;
    };
};

typedef struct expr_value expr_value_t;

enum expr_dpath_type {
    EXPR_DPATH_TYPE_UNSET = 0,
    EXPR_DPATH_TYPE_NONE = (1<<0),
    EXPR_DPATH_TYPE_ITEM = (1<<1),
    EXPR_DPATH_TYPE_CONTAINER = (1<<2),
};

struct expr_dpath {
    enum expr_dpath_type type;
    union {
        struct expr_dpath_item {
            struct tracker *tk;
        } item;
        struct expr_dpath_container {
            struct box *box;
        } container;
    };
};

typedef struct expr_dpath expr_dpath_t;

const struct expr_builtin_fn *
expr_lookup_builtin_fn(const char *name,
                       const struct ast_node_hdl *object);
const char *
expr_find_first_builtin(const char *prefix,
                        const struct ast_node_hdl *object);
const char *
expr_find_next_builtin(const char *name,
                       const struct ast_node_hdl *object);

void
expr_dpath_destroy_item(expr_dpath_t dpath);
void
expr_dpath_destroy_container(expr_dpath_t dpath);
bitpunch_status_t
expr_dpath_to_tracker(expr_dpath_t dpath,
                      struct tracker **tkp, struct browse_state *bst);
bitpunch_status_t
expr_dpath_to_box(expr_dpath_t dpath,
                  struct box **boxp,
                  struct browse_state *bst);
bitpunch_status_t
expr_dpath_to_box_direct(expr_dpath_t dpath,
                         struct box **boxp,
                         struct browse_state *bst);
bitpunch_status_t
expr_dpath_to_container(expr_dpath_t dpath,
                        expr_dpath_t *dpathp,
                        struct browse_state *bst);
bitpunch_status_t
expr_dpath_to_item(expr_dpath_t dpath,
                   expr_dpath_t *dpathp,
                   struct browse_state *bst);
bitpunch_status_t
expr_dpath_to_dpath(expr_dpath_t src_dpath,
                    enum expr_dpath_type dst_type,
                    expr_dpath_t *dst_dpathp,
                    struct browse_state *bst);
expr_dpath_t
expr_dpath_dup(expr_dpath_t src_dpath);
struct box *
expr_dpath_get_parent_box(expr_dpath_t dpath);
bitpunch_status_t
expr_dpath_get_size(expr_dpath_t dpath,
                    int64_t *dpath_sizep,
                    struct browse_state *bst);
const struct ast_node_hdl *
expr_dpath_get_item_node(expr_dpath_t dpath);
const struct ast_node_hdl *
expr_dpath_get_as_type(expr_dpath_t dpath);
const struct ast_node_hdl *
expr_dpath_get_target_filter(expr_dpath_t dpath);
struct track_path
expr_dpath_get_track_path(expr_dpath_t dpath);
int
expr_dpath_is(expr_dpath_t dpath1, expr_dpath_t dpath2);
void
expr_dpath_find_common_ancestor(expr_dpath_t dpath1,
                                expr_dpath_t dpath2,
                                expr_dpath_t *ancestor1_dpathp,
                                expr_dpath_t *ancestor2_dpathp);
void
expr_dpath_destroy(expr_dpath_t dpath);
void
expr_value_destroy(expr_value_t value);
static inline expr_value_t
expr_value_as_integer(int64_t value);
static inline expr_value_t
expr_value_as_boolean(int value);
static inline expr_value_t
expr_value_as_string(const char *str, int64_t len, struct box *from_box);
void
expr_value_attach_box(expr_value_t *value, struct box *box);
int
expr_value_cmp_integer(expr_value_t value1, expr_value_t value2);
int
expr_value_cmp_string(expr_value_t value1, expr_value_t value2);
int
expr_value_cmp_bytes(expr_value_t value1, expr_value_t value2);
int
expr_value_cmp(expr_value_t value1, expr_value_t value2);

bitpunch_status_t
expr_evaluate_value(struct ast_node_hdl *expr, struct box *scope,
                    expr_value_t *eval_valuep,
                    struct tracker_error **errp);
bitpunch_status_t
expr_evaluate_dpath(struct ast_node_hdl *expr, struct box *scope,
                    expr_dpath_t *eval_dpathp,
                    struct tracker_error **errp);
bitpunch_status_t
evaluate_conditional(struct ast_node_hdl *cond, struct box *scope,
                     int *evalp,
                     struct tracker_error **errp);
bitpunch_status_t
dpath_read_value(expr_dpath_t dpath,
                 expr_value_t *expr_valuep,
                 struct tracker_error **errp);
const char *
expr_value_type_str(enum expr_value_type type);

const char *
expr_dpath_type_str(enum expr_dpath_type type);

void
expr_value_to_hashable(enum expr_value_type type,
                       expr_value_t value,
                       const char **bufp, int64_t *lenp);

static inline expr_value_t
expr_value_as_integer(int64_t value)
{
    expr_value_t expr;

    expr.type = EXPR_VALUE_TYPE_INTEGER;
    expr.integer = value;
    return expr;
}

static inline expr_value_t
expr_value_as_boolean(int value)
{
    expr_value_t expr;

    expr.type = EXPR_VALUE_TYPE_BOOLEAN;
    expr.boolean = value;
    return expr;
}

static inline expr_value_t
expr_value_as_string(const char *str, int64_t len, struct box *from_box)
{
    expr_value_t expr;

    expr.type = EXPR_VALUE_TYPE_STRING;
    expr.string.str = str;
    expr.string.len = len;
    expr.from_box.box = from_box;
    return expr;
}

static inline expr_value_t
expr_value_as_bytes(const char *buf, int64_t len, struct box *from_box)
{
    expr_value_t expr;

    expr.type = EXPR_VALUE_TYPE_STRING;
    expr.bytes.buf = buf;
    expr.bytes.len = len;
    expr.from_box.box = from_box;
    return expr;
}

#endif /*__EXPR_H__*/
