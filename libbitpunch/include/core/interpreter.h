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

#ifndef __INTERPRETER_H__
#define __INTERPRETER_H__

#include <stddef.h>

#include "utils/queue.h"
#include "core/parser.h"
#include "core/expr.h"

#include PATH_TO_PARSER_TAB_H

enum interpreter_attr_flags {
    INTERPRETER_ATTR_FLAG_MANDATORY = 1,
};

struct interpreter_attr_def {
    STAILQ_ENTRY(interpreter_attr_def) list;
    const char *name;
    int ref_idx;
    enum expr_value_type value_type_mask;
    enum interpreter_attr_flags flags;
};

typedef int
(*interpreter_rcall_build_func_t)(
    struct ast_node_hdl *rcall,
    const struct ast_node_hdl *attr_values,
    struct compile_ctx *ctx);

struct interpreter {
    const char *name;
    enum expr_value_type value_type_mask;
    interpreter_rcall_build_func_t rcall_build_func;
    int n_attrs;
    int max_attr_ref;
    STAILQ_HEAD(interpreter_attr_list, interpreter_attr_def) attr_list;
};

int
interpreter_declare(const char *name,
                    enum expr_value_type value_type_mask,
                    interpreter_rcall_build_func_t rcall_build_func,
                    int n_attrs,
                    ... /* attrs: (name, ref_idx, type, flags) tuples */);

struct interpreter *
interpreter_lookup(const char *name);

void
interpreter_declare_std(void);

int
interpreter_rcall_build(struct ast_node_hdl *node,
                        const struct interpreter *interpreter,
                        struct statement_list *attr_list);

bitpunch_status_t
interpreter_rcall_evaluate_attrs(struct ast_node_hdl *expr,
                                 struct box *scope,
                                 int **attr_is_specifiedp,
                                 expr_value_t **attr_valuep,
                                 struct browse_state *bst);

void
interpreter_rcall_destroy_attr_values(struct ast_node_hdl *expr,
                                      int *attr_is_specified,
                                      expr_value_t *attr_value);

bitpunch_status_t
interpreter_rcall_read_value(struct ast_node_hdl *interpreter,
                             struct box *scope,
                             const char *item_data,
                             int64_t item_size,
                             expr_value_t *valuep,
                             struct browse_state *bst);

/* following declarations match definitions in interpreter_*.c
 * files */

void
interpreter_declare_binary_integer(void);

#endif
