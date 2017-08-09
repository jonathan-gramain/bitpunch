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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stddef.h>
#include <assert.h>

#include "core/interpreter.h"
#include "core/browse_internal.h"

#define MAX_INTERPRETER_COUNT           256

struct interpreter interpreter_table[MAX_INTERPRETER_COUNT];
int                interpreter_count = 0;

#define MAX_INTERPRETER_PARAM_DEF_COUNT 1024
#define INTERPRETER_MAX_PARAM_REF       256

struct interpreter_param_def interpreter_param_def_table[MAX_INTERPRETER_PARAM_DEF_COUNT];
int                          interpreter_param_def_count = 0;

static struct interpreter *
interpreter_new(void)
{
    if (interpreter_count == MAX_INTERPRETER_COUNT) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "Too many interpreters (max %d)\n",
                         MAX_INTERPRETER_COUNT);
        return NULL;
    }
    return &interpreter_table[interpreter_count++];
}

static struct interpreter_param_def *
interpreter_param_def_new(void)
{
    if (interpreter_param_def_count == MAX_INTERPRETER_PARAM_DEF_COUNT) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "Global interpreter param def limit reached (max %d)\n",
                         MAX_INTERPRETER_PARAM_DEF_COUNT);
        return NULL;
    }
    return &interpreter_param_def_table[interpreter_param_def_count++];
}

int
interpreter_declare(const char *name,
                    enum expr_value_type semantic_type,
                    interpreter_rcall_build_func_t rcall_build_func,
                    int n_params,
                    ... /* params: (name, type, flags) tuples */)
{
    struct interpreter *interpreter;
    int max_param_ref;
    va_list ap;
    int n_params_remain;
    struct interpreter_param_def *param_def;

    assert(NULL != rcall_build_func);

    interpreter = interpreter_lookup(name);
    if (NULL != interpreter) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "duplicate interpreter '%s'\n", name);
        return -1;
    }
    interpreter = interpreter_new();
    if (NULL == interpreter) {
        return -1;
    }
    interpreter->name = name;
    interpreter->semantic_type = semantic_type;
    interpreter->rcall_build_func = rcall_build_func;
    interpreter->n_params = n_params;
    max_param_ref = -1;
    STAILQ_INIT(&interpreter->param_list);
    va_start(ap, n_params);
    for (n_params_remain = n_params;
         n_params_remain > 0; --n_params_remain) {
        param_def = interpreter_param_def_new();
        if (NULL == param_def) {
            return -1;
        }
        param_def->name = va_arg(ap, const char *);
        param_def->ref_idx = va_arg(ap, int);
        assert(param_def->ref_idx >= 0 &&
               param_def->ref_idx <= INTERPRETER_MAX_PARAM_REF);
        param_def->type = va_arg(ap, enum ast_node_type);
        param_def->flags = va_arg(ap, enum interpreter_param_flags);
        STAILQ_INSERT_TAIL(&interpreter->param_list, param_def, list);
        max_param_ref = MAX(max_param_ref, param_def->ref_idx);
    }
    va_end(ap);
    interpreter->max_param_ref = max_param_ref;
    return 0;
}

struct interpreter *
interpreter_lookup(const char *name)
{
    int i;

    for (i = 0; i < interpreter_count; ++i) {
        if (0 == strcmp(interpreter_table[i].name, name)) {
            return &interpreter_table[i];
        }
    }
    return NULL;
}

void interpreter_declare_binary_integer(void);
void interpreter_declare_string(void);
void interpreter_declare_varint(void);
void interpreter_declare_base64(void);
void interpreter_declare_snappy(void);

void
interpreter_declare_std(void)
{
    interpreter_declare_binary_integer();
    interpreter_declare_string();
    interpreter_declare_varint();
    interpreter_declare_base64();
    interpreter_declare_snappy();
}

static int
rcall_build_params(struct ast_node *rcall,
                   const struct interpreter *interpreter,
                   struct statement_list *param_list);


struct ast_node *
interpreter_rcall_build(const struct interpreter *interpreter,
                        struct statement_list *param_list)
{
    struct ast_node *rcall;

    rcall = malloc0_safe(INTERPRETER_RCALL_BASE_SIZE
                         + (interpreter->max_param_ref + 1)
                         * sizeof (struct ast_node));
    rcall->type = AST_NODE_TYPE_REXPR_INTERPRETER;
    // default dpath type for type-declared interpreters, may be
    // overriden in expressions
    rcall->u.rexpr.dpath_type = EXPR_DPATH_TYPE_ITEM;
    rcall->u.rexpr.value_type = interpreter->semantic_type;
    rcall->u.rexpr_interpreter.interpreter = interpreter;
    rcall->u.rexpr_interpreter.get_size_func = NULL;
    if (-1 == rcall_build_params(rcall, interpreter, param_list)) {
        free(rcall);
        return NULL;
    }
    return rcall;
}

static struct ast_node *
interpreter_rcall_instanciate(const struct ast_node *rcall)
{
    const struct interpreter *interpreter;
    struct ast_node *inst;

    assert(AST_NODE_TYPE_REXPR_INTERPRETER == rcall->type);
    interpreter = rcall->u.rexpr_interpreter.interpreter;
    inst = malloc0_safe(INTERPRETER_RCALL_BASE_SIZE
                         + (interpreter->max_param_ref + 1)
                         * sizeof (struct ast_node));
    memcpy(inst, rcall, INTERPRETER_RCALL_BASE_SIZE
           + (interpreter->max_param_ref + 1)
           * sizeof (struct ast_node));
    inst->flags &= ~ASTFLAG_TEMPLATE;
    return inst;
}

int
interpreter_build_instance(struct ast_node **template_p,
                           struct ast_node *target)
{
    const struct interpreter *interpreter;
    struct ast_node *interp_inst;
    struct ast_node *target_item;

    assert(NULL != target);

    interpreter = (*template_p)->u.rexpr_interpreter.interpreter;
    if (0 != ((*template_p)->flags & ASTFLAG_TEMPLATE)) {
        interp_inst = interpreter_rcall_instanciate(*template_p);
    } else {
        interp_inst = *template_p;
    }

    interp_inst->u.rexpr.dpath_type = target->u.rexpr.dpath_type;
    interp_inst->u.rexpr_filter.target = target;

    target_item = ast_node_get_target_item(target);
    if (NULL == target_item) {
        // pass the rexpr node if no target item configured
        target_item = target;
    }
    if (-1 == interpreter->rcall_build_func(
            interp_inst, target_item,
            interpreter_rcall_get_params(interp_inst))) {
        return -1;
    }

    *template_p = interp_inst;
    return 0;
}

static int
get_param_index(const struct interpreter *interpreter,
                const char *param_name)
{
    struct interpreter_param_def *param_def;

    STAILQ_FOREACH(param_def, &interpreter->param_list, list) {
        if (0 == strcmp(param_def->name, param_name)) {
            return param_def->ref_idx;
        }
    }
    return -1; /* not found */
}

static int
rcall_build_params(struct ast_node *rcall,
                   const struct interpreter *interpreter,
                   struct statement_list *param_list)
{
    struct named_expr *param;
    struct ast_node *param_valuep;
    int param_ref;
    struct interpreter_param_def *param_def;
    int sem_error = FALSE;

    for (param_ref = 0;
         param_ref <= interpreter->max_param_ref; ++param_ref) {
        param_valuep = INTERPRETER_RCALL_PARAM(rcall, param_ref);
        param_valuep->type = AST_NODE_TYPE_NONE;
    }
    STATEMENT_FOREACH(named_expr, param, param_list, list) {
        param_ref = get_param_index(interpreter, param->nstmt.name);
        if (-1 == param_ref) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &param->nstmt.stmt.loc,
                           "no such parameter \"%s\" for interpreter \"%s\"",
                           param->nstmt.name, interpreter->name);
            sem_error = TRUE;
            continue ;
        }
        assert(param_ref >= 0 && param_ref <= interpreter->max_param_ref);
        param_valuep = INTERPRETER_RCALL_PARAM(rcall, param_ref);
        if (param_valuep->type != AST_NODE_TYPE_NONE) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &param->nstmt.stmt.loc,
                           "duplicate parameter \"%s\"", param->nstmt.name);
            sem_error = TRUE;
            continue ;
        }
        *param_valuep = *param->expr;
    }
    STAILQ_FOREACH(param_def, &interpreter->param_list, list) {
        param_valuep = INTERPRETER_RCALL_PARAM(rcall, param_def->ref_idx);
        if ((INTERPRETER_PARAM_FLAG_MANDATORY & param_def->flags)
            && AST_NODE_TYPE_NONE == param_valuep->type) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &rcall->loc,
                           "missing mandatory parameter \"%s\"",
                           param_def->name);
            sem_error = TRUE;
            continue ;
        }
    }
    if (!sem_error) {
        return 0;
    } else {
        return -1;
    }
}


bitpunch_status_t
interpreter_rcall_read_value(const struct ast_node *interpreter,
                             const char *item_data,
                             int64_t item_size,
                             enum expr_value_type *typep,
                             union expr_value *valuep,
                             struct browse_state *bst)
{
    struct ast_node *params;
    size_t value_size;
    union expr_value value;

    params = interpreter_rcall_get_params(interpreter);

    if (NULL == interpreter->u.rexpr_interpreter.get_size_func) {
        value_size = (size_t)item_size;
    } else if (-1 == interpreter->u.rexpr_interpreter.get_size_func(
                   &value_size, item_data, item_size, params)) {
        return BITPUNCH_DATA_ERROR;
    }
    if (-1 == interpreter->u.rexpr_interpreter.read_func(
            &value, item_data, value_size, params)) {
        return BITPUNCH_DATA_ERROR;
    }
    if (NULL != typep) {
        *typep = interpreter->u.rexpr.value_type;
    }
    if (NULL != valuep) {
        *valuep = value;
    } else {
        expr_value_destroy(interpreter->u.rexpr.value_type, value);
    }
    return BITPUNCH_OK;
}
