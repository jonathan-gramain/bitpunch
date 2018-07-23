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

struct expr_value_string {
    const char *str;
    int64_t len;
    struct box *from_box;
};

struct expr_value_bytes {
    const char *buf;
    int64_t len;
    struct box *from_box;
};

struct expr_value {
    enum expr_value_type type;
    union {
        int64_t integer;
        int boolean;
        struct expr_value_string string;
        struct expr_value_bytes bytes;
    };
};

typedef struct expr_value expr_value_t;

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
expr_dpath_destroy(expr_dpath_t dpath);
expr_dpath_t
expr_dpath_dup(expr_dpath_t src_dpath);
static inline expr_dpath_t
expr_dpath_none(void);
static inline expr_dpath_t
expr_dpath_as_item(struct tracker *tk);
static inline expr_dpath_t
expr_dpath_as_container(struct box *box);
void
expr_value_destroy(expr_value_t value);
static inline expr_value_t
expr_value_as_integer(int64_t value);
static inline expr_value_t
expr_value_as_boolean(int value);
static inline expr_value_t
expr_value_as_string(const char *str);
static inline expr_value_t
expr_value_as_string_len(const char *str, int64_t len);
static inline expr_value_t
expr_value_as_bytes(const char *buf, int64_t len);
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
expr_evaluate(struct ast_node_hdl *expr, struct box *scope,
              expr_value_t *valuep, expr_dpath_t *dpathp,
              struct tracker_error **errp);
bitpunch_status_t
expr_evaluate_value(struct ast_node_hdl *expr, struct box *scope,
                    expr_value_t *valuep,
                    struct tracker_error **errp);
bitpunch_status_t
expr_evaluate_dpath(struct ast_node_hdl *expr, struct box *scope,
                    expr_dpath_t *dpathp,
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
expr_value_to_hashable(expr_value_t value,
                       const char **bufp, int64_t *lenp);
int
expr_value_type_mask_contains_dpath(enum expr_value_type value_type_mask);

#include "core/expr_inlines.h"

#endif /*__EXPR_H__*/
