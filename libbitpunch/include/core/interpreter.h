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

#define INTERPRETER_RCALL_BASE_SIZE                     \
    offsetof(struct ast_node, u.rexpr_interpreter)      \
    + sizeof (struct rexpr_interpreter)

#define INTERPRETER_RCALL_PARAM(rcall, n)               \
    ((struct ast_node *) (                         \
        (((char *)(rcall))                              \
         + INTERPRETER_RCALL_BASE_SIZE                  \
         + (n) * sizeof (struct ast_node))))

enum interpreter_param_flags {
    INTERPRETER_PARAM_FLAG_MANDATORY = 1,
};

struct interpreter_param_def {
    STAILQ_ENTRY(interpreter_param_def) list;
    const char *name;
    int ref_idx;
    enum expr_value_type type;
    enum interpreter_param_flags flags;
};

typedef int
(*interpreter_rcall_build_func_t)(struct ast_node *rcall,
                                  const struct ast_node *data_source,
                                  const struct ast_node *param_values);

struct interpreter {
    const char *name;
    enum expr_value_type semantic_type;
    interpreter_rcall_build_func_t rcall_build_func;
    int n_params;
    int max_param_ref;
    STAILQ_HEAD(interpreter_param_list, interpreter_param_def) param_list;
};

int
interpreter_declare(const char *name,
                    enum expr_value_type semantic_type,
                    interpreter_rcall_build_func_t rcall_build_func,
                    int n_params,
                    ... /* params: (name, ref_idx, type, flags) tuples */);

struct interpreter *
interpreter_lookup(const char *name);

void
interpreter_declare_std(void);

struct ast_node *
interpreter_rcall_build(const struct interpreter *interpreter,
                        struct statement_list *param_list);
int
interpreter_build_instance(struct ast_node **template_p,
                           struct ast_node *target);

static inline struct ast_node *
interpreter_rcall_get_params(const struct ast_node *rcall) {
    return INTERPRETER_RCALL_PARAM(rcall, 0);
}

bitpunch_status_t
interpreter_rcall_read_value(const struct ast_node *interpreter,
                             const char *item_data,
                             int64_t item_size,
                             enum expr_value_type *typep,
                             union expr_value *valuep,
                             struct browse_state *bst);

/* following declarations match definitions in interpreter_*.c
 * files */

void
interpreter_declare_binary_integer(void);

#endif
