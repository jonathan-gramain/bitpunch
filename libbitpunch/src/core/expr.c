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
#include <string.h>
#include <assert.h>
#include <err.h>
#include <alloca.h>

#include "core/ast.h"
#include "core/parser.h"
#include PATH_TO_PARSER_TAB_H
#include "core/interpreter.h"
#include "core/browse_internal.h"
#include "core/expr_internal.h"


// value evaluation
static bitpunch_status_t
expr_evaluate_value_from_dpath(struct ast_node *expr, struct box *scope,
                               union expr_value *eval_valuep,
                               struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_binary_operator(struct ast_node *expr, struct box *scope,
                              union expr_value *eval_valuep,
                              struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_unary_operator(struct ast_node *expr, struct box *scope,
                             union expr_value *eval_valuep,
                             struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_sizeof(struct ast_node *expr, struct box *scope,
                     union expr_value *eval_valuep,
                     struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_addrof(struct ast_node *expr, struct box *scope,
                     union expr_value *eval_valuep,
                     struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_value_filter(struct ast_node *expr, struct box *scope,
                           union expr_value *eval_valuep,
                           struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_value_fcall(struct ast_node *expr, struct box *scope,
                          union expr_value *eval_valuep,
                          struct browse_state *bst);

// dpath evaluation
static bitpunch_status_t
expr_evaluate_dpath_file(struct ast_node *expr, struct box *scope,
                         union expr_dpath *eval_dpathp,
                         struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_dpath_self(struct ast_node *expr, struct box *scope,
                         union expr_dpath *eval_dpathp,
                         struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_dpath_field(struct ast_node *expr, struct box *scope,
                           union expr_dpath *eval_dpathp,
                          struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_dpath_link(struct ast_node *expr, struct box *scope,
                         union expr_dpath *eval_dpathp,
                         struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_dpath_subscript(struct ast_node *expr, struct box *scope,
                              union expr_dpath *eval_dpathp,
                              struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_dpath_subscript_slice(struct ast_node *expr,
                                    struct box *scope,
                                    union expr_dpath *eval_dpathp,
                                    struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_dpath_filter(struct ast_node *expr, struct box *scope,
                           union expr_dpath *eval_dpathp,
                           struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_dpath_fcall(struct ast_node *expr, struct box *scope,
                          union expr_dpath *eval_dpathp,
                          struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_dpath_interpreter(struct ast_node *expr, struct box *scope,
                                union expr_dpath *eval_dpathp,
                                struct browse_state *bst);
static bitpunch_status_t
expr_evaluate_dpath_as_type(struct ast_node *expr, struct box *scope,
                            union expr_dpath *eval_dpathp,
                            struct browse_state *bst);
static bitpunch_status_t
expr_read_dpath_value_internal(struct ast_node *expr,
                               union expr_dpath dpath,
                               union expr_value *expr_valuep,
                               struct browse_state *bst);
static bitpunch_status_t
expr_read_dpath_value_default(struct ast_node *expr,
                              union expr_dpath dpath,
                              union expr_value *expr_valuep,
                              struct browse_state *bst);
static bitpunch_status_t
expr_read_dpath_value_link(struct ast_node *expr,
                           union expr_dpath dpath,
                           union expr_value *expr_valuep,
                           struct browse_state *bst);
static bitpunch_status_t
expr_read_dpath_value_interpreter(struct ast_node *expr,
                                  union expr_dpath dpath,
                                  union expr_value *expr_valuep,
                                  struct browse_state *bst);


struct expr_evalop_match_item {
    enum ast_node_type    op_type;
    int                   n_opd;       /*!< number of operands */
    enum expr_value_type        opd_types[2]; /*!< type of operands */
    struct expr_evaluator evaluator;
};

/* == */

static union expr_value
expr_evalop_eq_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int)(operands[0].integer == operands[1].integer);
}

static struct expr_evalop_match_item match_eq_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_EQ,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_eq_integer_integer,
    },
};

static union expr_value
expr_evalop_eq_boolean_boolean(union expr_value operands[])
{
    return (union expr_value)
        (int)(operands[0].boolean == operands[1].boolean);
}

static struct expr_evalop_match_item match_eq_boolean_boolean = {
    .op_type = AST_NODE_TYPE_OP_EQ,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_BOOLEAN,
        EXPR_VALUE_TYPE_BOOLEAN
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_eq_boolean_boolean,
    },
};

static union expr_value
expr_evalop_eq_string_string(union expr_value operands[])
{
    return (union expr_value)
        (int)(operands[0].string.len == operands[1].string.len &&
              0 == memcmp(operands[0].string.str,
                          operands[1].string.str, operands[0].string.len));
}

static struct expr_evalop_match_item match_eq_string_string = {
    .op_type = AST_NODE_TYPE_OP_EQ,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_STRING,
        EXPR_VALUE_TYPE_STRING
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_eq_string_string,
    },
};


/* != */

static union expr_value
expr_evalop_ne_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int)(operands[0].integer != operands[1].integer);
}

static struct expr_evalop_match_item match_ne_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_NE,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_ne_integer_integer,
    },
};

static union expr_value
expr_evalop_ne_boolean_boolean(union expr_value operands[])
{
    return (union expr_value)
        (int)(operands[0].boolean != operands[1].boolean);
}

static struct expr_evalop_match_item match_ne_boolean_boolean = {
    .op_type = AST_NODE_TYPE_OP_NE,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_BOOLEAN,
        EXPR_VALUE_TYPE_BOOLEAN
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_ne_boolean_boolean,
    },
};

static union expr_value
expr_evalop_ne_string_string(union expr_value operands[])
{
    return (union expr_value)
        (int)(operands[0].string.len != operands[1].string.len ||
              0 != memcmp(operands[0].string.str,
                          operands[1].string.str, operands[0].string.len));
}

static struct expr_evalop_match_item match_ne_string_string = {
    .op_type = AST_NODE_TYPE_OP_NE,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_STRING,
        EXPR_VALUE_TYPE_STRING
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_ne_string_string,
    },
};


/* < */

static union expr_value
expr_evalop_lt_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int)(operands[0].integer < operands[1].integer);
}

static struct expr_evalop_match_item match_lt_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_LT,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_lt_integer_integer,
    },
};

/* <= */

static union expr_value
expr_evalop_le_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int)(operands[0].integer <= operands[1].integer);
}

static struct expr_evalop_match_item match_le_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_LE,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_le_integer_integer,
    },
};

/* > */

static union expr_value
expr_evalop_gt_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int)(operands[0].integer > operands[1].integer);
}

static struct expr_evalop_match_item match_gt_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_GT,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_gt_integer_integer,
    },
};

/* >= */

static union expr_value
expr_evalop_ge_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int)(operands[0].integer >= operands[1].integer);
}

static struct expr_evalop_match_item match_ge_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_GE,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_ge_integer_integer,
    },
};


/* || */

static union expr_value
expr_evalop_lor_boolean_boolean(union expr_value operands[])
{
    return (union expr_value)
        (int)(operands[0].boolean || operands[1].boolean);
}

static struct expr_evalop_match_item match_lor_boolean_boolean = {
    .op_type = AST_NODE_TYPE_OP_LOR,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_BOOLEAN,
        EXPR_VALUE_TYPE_BOOLEAN
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_lor_boolean_boolean,
    },
};

/* && */

static union expr_value
expr_evalop_land_boolean_boolean(union expr_value operands[])
{
    return (union expr_value)
        (int)(operands[0].boolean && operands[1].boolean);
}

static struct expr_evalop_match_item match_land_boolean_boolean = {
    .op_type = AST_NODE_TYPE_OP_LAND,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_BOOLEAN,
        EXPR_VALUE_TYPE_BOOLEAN
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_land_boolean_boolean,
    },
};


/* ! */

static union expr_value
expr_evalop_lnot_boolean(union expr_value operands[])
{
    return (union expr_value)
        (int)( ! operands[0].boolean);
}

static struct expr_evalop_match_item match_lnot_boolean = {
    .op_type = AST_NODE_TYPE_OP_LNOT,
    .n_opd = 1,
    .opd_types = {
        EXPR_VALUE_TYPE_BOOLEAN
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_lnot_boolean,
    },
};


/* | */

static union expr_value
expr_evalop_bwor_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int64_t)(operands[0].integer | operands[1].integer);
}

static struct expr_evalop_match_item match_bwor_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_BWOR,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_bwor_integer_integer,
    },
};

/* & */

static union expr_value
expr_evalop_bwand_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int64_t)(operands[0].integer & operands[1].integer);
}

static struct expr_evalop_match_item match_bwand_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_BWAND,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_bwand_integer_integer,
    },
};

/* ^ */

static union expr_value
expr_evalop_bwxor_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int64_t)(operands[0].integer ^ operands[1].integer);
}

static struct expr_evalop_match_item match_bwxor_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_BWXOR,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_bwxor_integer_integer,
    },
};

/* ~ */

static union expr_value
expr_evalop_bwnot_integer(union expr_value operands[])
{
    return (union expr_value)
        (int64_t)( ~ operands[0].integer);
}

static struct expr_evalop_match_item match_bwnot_integer = {
    .op_type = AST_NODE_TYPE_OP_BWNOT,
    .n_opd = 1,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_bwnot_integer,
    },
};


/* << */

static union expr_value
expr_evalop_lshift_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int64_t)(operands[0].integer << operands[1].integer);
}

static struct expr_evalop_match_item match_lshift_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_LSHIFT,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_lshift_integer_integer,
    },
};

/* >> */

static union expr_value
expr_evalop_rshift_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int64_t)(operands[0].integer >> operands[1].integer);
}

static struct expr_evalop_match_item match_rshift_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_RSHIFT,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_rshift_integer_integer,
    },
};


/* + */

static union expr_value
expr_evalop_add_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int64_t)(operands[0].integer + operands[1].integer);
}

static struct expr_evalop_match_item match_add_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_ADD,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_add_integer_integer,
    },
};

/* - */

static union expr_value
expr_evalop_sub_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int64_t)(operands[0].integer - operands[1].integer);
}

static struct expr_evalop_match_item match_sub_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_SUB,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_sub_integer_integer,
    },
};

/* * */

static union expr_value
expr_evalop_mul_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int64_t)(operands[0].integer * operands[1].integer);
}

static struct expr_evalop_match_item match_mul_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_MUL,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_mul_integer_integer,
    },
};

/* / */

static union expr_value
expr_evalop_div_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int64_t)(operands[0].integer / operands[1].integer);
}

static struct expr_evalop_match_item match_div_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_DIV,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_div_integer_integer,
    },
};

/* % */

static union expr_value
expr_evalop_mod_integer_integer(union expr_value operands[])
{
    return (union expr_value)
        (int64_t)(operands[0].integer % operands[1].integer);
}

static struct expr_evalop_match_item match_mod_integer_integer = {
    .op_type = AST_NODE_TYPE_OP_MOD,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_mod_integer_integer,
    },
};

/* unary - */

static union expr_value
expr_evalop_uplus_integer(union expr_value operands[])
{
    return (union expr_value)
        (int64_t)(operands[0].integer);
}

static struct expr_evalop_match_item match_uplus_integer = {
    .op_type = AST_NODE_TYPE_OP_UPLUS,
    .n_opd = 1,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_uplus_integer,
    },
};

static union expr_value
expr_evalop_uminus_integer(union expr_value operands[])
{
    return (union expr_value)
        (int64_t)( - operands[0].integer);
}

static struct expr_evalop_match_item match_uminus_integer = {
    .op_type = AST_NODE_TYPE_OP_UMINUS,
    .n_opd = 1,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_uminus_integer,
    },
};

/******/

static const struct expr_evalop_match_item * const
expr_evalop_match_items[] = {
    &match_eq_integer_integer,
    &match_eq_boolean_boolean,
    &match_eq_string_string,
    &match_ne_integer_integer,
    &match_ne_boolean_boolean,
    &match_ne_string_string,
    &match_lt_integer_integer,
    &match_le_integer_integer,
    &match_gt_integer_integer,
    &match_ge_integer_integer,
    &match_lor_boolean_boolean,
    &match_land_boolean_boolean,
    &match_lnot_boolean,
    &match_bwor_integer_integer,
    &match_bwand_integer_integer,
    &match_bwxor_integer_integer,
    &match_bwnot_integer,
    &match_lshift_integer_integer,
    &match_rshift_integer_integer,
    &match_add_integer_integer,
    &match_sub_integer_integer,
    &match_mul_integer_integer,
    &match_div_integer_integer,
    &match_mod_integer_integer,
    &match_uplus_integer,
    &match_uminus_integer,
};

const struct expr_evaluator *
expr_lookup_evaluator(enum ast_node_type op_type,
                      enum expr_value_type opd_types[])
{
    int match_i;
    const struct expr_evalop_match_item *match;
    int opd_i;

    for (match_i = 0;
         match_i < N_ELEM(expr_evalop_match_items); ++match_i) {
        match = expr_evalop_match_items[match_i];
        if (match->op_type == op_type) {
            for (opd_i = 0; opd_i < match->n_opd; ++opd_i) {
                if (opd_types[opd_i] != match->opd_types[opd_i]) {
                    break ;
                }
            }
            if (opd_i == match->n_opd) {
                return &match->evaluator;
            }
            /* try next matches */
        }
    }
    return NULL; /* no op type + operand types match */
}




static bitpunch_status_t
expr_eval_builtin_bytes(struct ast_node *object,
                        struct func_param_list *params,
                        int n_params,
                        struct box *scope,
                        union expr_dpath *eval_dpathp,
                        struct browse_state *bst)
{
    struct ast_node *expr;
    bitpunch_status_t bt_ret;
    union expr_dpath dpath_eval;
    struct box *bytes_box;

    expr = STAILQ_FIRST(params)->expr;
    if (EXPR_DPATH_TYPE_NONE == expr->u.rexpr.dpath_type) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                       "cannot evaluate 'bytes': expression is not "
                       "a dpath");
        return BITPUNCH_INVALID_PARAM;
    }
    bt_ret = expr_evaluate_dpath_internal(expr, scope, &dpath_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    switch (expr->u.rexpr.dpath_type) {
    case EXPR_DPATH_TYPE_ITEM:
        bytes_box = box_new_bytes_box_from_item(dpath_eval.item.tk, bst);
        tracker_delete(dpath_eval.item.tk);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        bytes_box = box_new_bytes_box_from_box(dpath_eval.container.box,
                                               bst);
        box_delete(dpath_eval.container.box);
        break ;
    default:
        assert(0);
    }
    if (NULL == bytes_box) {
        // FIXME transmit code from above functions
        return BITPUNCH_DATA_ERROR;
    }
    eval_dpathp->container.box = bytes_box;
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_eval_builtin_index(struct ast_node *object,
                        struct func_param_list *params,
                        int n_params,
                        struct box *scope,
                        union expr_value *eval_valuep,
                        struct browse_state *bst)
{
    struct ast_node *array_dpath;
    struct ast_node *item_dpath;
    bitpunch_status_t bt_ret;
    union expr_dpath array_dpath_eval;
    union expr_dpath item_dpath_eval;
    const struct ast_node *array_node;
    enum expr_dpath_type array_ancestor_type, item_ancestor_type;
    union expr_dpath array_ancestor, item_ancestor;
    int ancestor_is_array;
    enum expr_dpath_type array_item_dpath_type;
    union expr_dpath array_item_dpath;
    struct track_path item_track;
    struct track_path cur_track;

    array_dpath = STAILQ_FIRST(params)->expr;
    item_dpath = STAILQ_NEXT(STAILQ_FIRST(params), list)->expr;
    if (EXPR_DPATH_TYPE_NONE == array_dpath->u.rexpr.dpath_type) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &array_dpath->loc,
                       "cannot evaluate 'index': 1st argument is not "
                       "a dpath");
        return BITPUNCH_INVALID_PARAM;
    }
    if (EXPR_DPATH_TYPE_NONE == item_dpath->u.rexpr.dpath_type) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &item_dpath->loc,
                       "cannot evaluate 'index': 2nd argument is not "
                       "a dpath");
        return BITPUNCH_INVALID_PARAM;
    }
    bt_ret = expr_evaluate_dpath_internal(array_dpath, scope,
                                          &array_dpath_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    array_node = expr_dpath_get_node(array_dpath->u.rexpr.dpath_type,
                                     array_dpath_eval);
    if (AST_NODE_TYPE_ARRAY != array_node->type) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &array_dpath->loc,
                       "cannot evaluate 'index': 1st argument is not "
                       "an array");
        expr_dpath_destroy(array_dpath->u.rexpr.dpath_type,
                           array_dpath_eval);
        return BITPUNCH_INVALID_PARAM;
    }
    bt_ret = expr_evaluate_dpath_internal(item_dpath, scope,
                                          &item_dpath_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        expr_dpath_destroy(array_dpath->u.rexpr.dpath_type,
                           array_dpath_eval);
        return bt_ret;
    }
    expr_dpath_find_common_ancestor(array_dpath->u.rexpr.dpath_type,
                                    array_dpath_eval,
                                    item_dpath->u.rexpr.dpath_type,
                                    item_dpath_eval,
                                    &array_ancestor_type,
                                    &array_ancestor,
                                    &item_ancestor_type,
                                    &item_ancestor);

    ancestor_is_array = expr_dpath_is(array_ancestor_type,
                                      array_ancestor,
                                      array_dpath->u.rexpr.dpath_type,
                                      array_dpath_eval);

    if (!ancestor_is_array
        || expr_dpath_is(item_dpath->u.rexpr.dpath_type, item_dpath_eval,
                         item_ancestor_type, item_ancestor)) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &array_dpath->loc,
                       "cannot evaluate 'index': 2nd argument is not a "
                       "descendent of array");
        expr_dpath_destroy(array_dpath->u.rexpr.dpath_type,
                           array_dpath_eval);
        expr_dpath_destroy(item_dpath->u.rexpr.dpath_type,
                           item_dpath_eval);
        return BITPUNCH_INVALID_PARAM;
    }
    array_item_dpath_type = item_dpath->u.rexpr.dpath_type;
    array_item_dpath = item_dpath_eval;
    item_track.type = TRACK_PATH_NOTYPE;
    do {
        cur_track = expr_dpath_get_track_path(array_item_dpath_type,
                                              array_item_dpath);
        if (TRACK_PATH_ARRAY == cur_track.type) {
            item_track = cur_track;
        }
        array_item_dpath.container.box =
            expr_dpath_get_parent_box(array_item_dpath_type,
                                      array_item_dpath);
        assert(NULL != array_item_dpath.container.box);
        array_item_dpath_type = EXPR_DPATH_TYPE_CONTAINER;
    }
    while (!expr_dpath_is(array_item_dpath_type, array_item_dpath,
                          item_ancestor_type, item_ancestor));
    assert(TRACK_PATH_ARRAY == item_track.type);
    eval_valuep->integer = item_track.u.array.index;

    expr_dpath_destroy(array_dpath->u.rexpr.dpath_type,
                       array_dpath_eval);
    expr_dpath_destroy(item_dpath->u.rexpr.dpath_type,
                       item_dpath_eval);
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_eval_builtin_len(struct ast_node *object,
                      struct func_param_list *params,
                      int n_params,
                      struct box *scope,
                      union expr_value *eval_valuep,
                      struct browse_state *bst)
{
    struct ast_node *expr;
    int64_t item_count = 0;
    bitpunch_status_t bt_ret;
    union expr_dpath dpath_eval;
    struct box *box = NULL;

    expr = STAILQ_FIRST(params)->expr;
    if (EXPR_DPATH_TYPE_NONE == expr->u.rexpr.dpath_type) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                       "cannot evaluate 'len' on expression: not a dpath");
        return BITPUNCH_INVALID_PARAM;
    }
    bt_ret = expr_evaluate_dpath_internal(expr, scope, &dpath_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_dpath_to_box(expr->u.rexpr.dpath_type, dpath_eval, &box,
                               bst);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = box_get_n_items_internal(box, &item_count, bst);
    }
    box_delete(box);
    expr_dpath_destroy(expr->u.rexpr.dpath_type, dpath_eval);
    if (BITPUNCH_OK != bt_ret) {
        //TODO log
        return bt_ret;
    }
    eval_valuep->integer = item_count;
    return BITPUNCH_OK;
}

/* this array must be alphabetically ordered by builtin name */
static const struct expr_builtin_fn const
expr_builtin_fns[] = {
    {
        .builtin_name = "bytes",
        .res_value_type = EXPR_VALUE_TYPE_UNSET,
        .res_dpath_type = EXPR_DPATH_TYPE_CONTAINER,
        .eval_dpath_fn = expr_eval_builtin_bytes,
        .min_n_params = 1,
        .max_n_params = 1,
    },
    {
        .builtin_name = "index",
        .res_value_type = EXPR_VALUE_TYPE_INTEGER,
        .res_dpath_type = EXPR_DPATH_TYPE_NONE,
        .eval_value_fn = expr_eval_builtin_index,
        .min_n_params = 2,
        .max_n_params = 2,
    },
    {
        .builtin_name = "len",
        .res_value_type = EXPR_VALUE_TYPE_INTEGER,
        .res_dpath_type = EXPR_DPATH_TYPE_NONE,
        .eval_value_fn = expr_eval_builtin_len,
        .min_n_params = 1,
        .max_n_params = 1,
    },
};

const struct expr_builtin_fn *
expr_lookup_builtin_fn(const char *name,
                       const struct ast_node *object)
{
    int builtin_i;
    const struct expr_builtin_fn *builtin;

    for (builtin_i = 0;
         builtin_i < N_ELEM(expr_builtin_fns); ++builtin_i) {
        builtin = &expr_builtin_fns[builtin_i];
        if (0 == strcmp(builtin->builtin_name, name)
            && ((NULL == object
                 && AST_NODE_TYPE_NONE == builtin->member_of)
                || (NULL != object
                    && object->type == builtin->member_of))) {
            return builtin;
        }
    }
    return NULL; /* no builtin found with this name */
}

/**
 * @brief find the first builtin starting with @ref prefix in
 * alphabetical order
 *
 * @param name the builtin name of a previously returned result, or
 * NULL, or any alphabetical name or prefix
 *
 * @return the name of the found builtin, or NULL if none found
 */
const char *
expr_find_first_builtin(const char *prefix,
                        const struct ast_node *object)
{
    int prefix_len;
    int builtin_i;
    const struct expr_builtin_fn *builtin;

    if (NULL == prefix) {
        /* return the first builtin */
        return expr_builtin_fns[0].builtin_name;
    }
    prefix_len = strlen(prefix);
    for (builtin_i = 0;
         builtin_i < N_ELEM(expr_builtin_fns); ++builtin_i) {
        builtin = &expr_builtin_fns[builtin_i];
        if (strncmp(builtin->builtin_name, prefix, prefix_len) == 0
            && ((NULL == object
                 && AST_NODE_TYPE_NONE == builtin->member_of)
                || (NULL != object
                    && object->type == builtin->member_of))) {
            return builtin->builtin_name;
        }
    }
    return NULL; /* no builtin found with such prefix */
}

/**
 * @brief find the builtin which name immediately follows @ref name
 * alphabetically.
 *
 * @param name the builtin name of a previously returned result
 *
 * @return the name of the found builtin, or NULL if none found
 */
const char *
expr_find_next_builtin(const char *name,
                       const struct ast_node *object)
{
    int builtin_i;
    const struct expr_builtin_fn *builtin;

    assert(NULL != name);
    for (builtin_i = 0;
         builtin_i < N_ELEM(expr_builtin_fns); ++builtin_i) {
        builtin = &expr_builtin_fns[builtin_i];
        if (strcmp(builtin->builtin_name, name) > 0
            && ((NULL == object
                 && AST_NODE_TYPE_NONE == builtin->member_of)
                || (NULL != object
                    && object->type == builtin->member_of))) {
            return builtin->builtin_name;
        }
    }
    return NULL; /* last builtin reached */
}

void
expr_dpath_destroy_item(union expr_dpath dpath)
{
    tracker_delete(dpath.item.tk);
}

void
expr_dpath_destroy_container(union expr_dpath dpath)
{
    box_delete(dpath.container.box);
}

bitpunch_status_t
expr_dpath_to_tracker(enum expr_dpath_type type, union expr_dpath dpath,
                      struct tracker **tkp)
{
    struct tracker *tk;

    switch (type) {
    case EXPR_DPATH_TYPE_ITEM:
        tk = tracker_dup(dpath.item.tk);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        tk = track_box(dpath.container.box);
        break ;
    default:
        assert(0);
    }
    *tkp = tk;
    return BITPUNCH_OK;
}

bitpunch_status_t
expr_dpath_to_box(enum expr_dpath_type type, union expr_dpath dpath,
                  struct box **boxp,
                  struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *box;

    switch (type) {
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = tracker_create_item_box(dpath.item.tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        box = dpath.item.tk->item_box;
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        box = dpath.container.box;
        break ;
    default:
        assert(0);
    }
    box_acquire(box);
    *boxp = box;
    return BITPUNCH_OK;
}

bitpunch_status_t
expr_dpath_to_container(enum expr_dpath_type type,
                        union expr_dpath dpath,
                        union expr_dpath *dpathp,
                        struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *box;

    bt_ret = expr_dpath_to_box(type, dpath, &box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    dpathp->container.box = box;
    return BITPUNCH_OK;
}

bitpunch_status_t
expr_dpath_to_item(enum expr_dpath_type type,
                   union expr_dpath dpath,
                   union expr_dpath *dpathp,
                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct tracker *tk;

    bt_ret = expr_dpath_to_tracker(type, dpath, &tk);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    dpathp->item.tk = tk;
    return BITPUNCH_OK;
}

bitpunch_status_t
expr_dpath_to_dpath(enum expr_dpath_type src_type,
                    union expr_dpath src_dpath,
                    enum expr_dpath_type dst_type,
                    union expr_dpath *dst_dpathp,
                    struct browse_state *bst)
{
    switch (dst_type) {
    case EXPR_DPATH_TYPE_ITEM:
        return expr_dpath_to_item(src_type, src_dpath, dst_dpathp, bst);
    case EXPR_DPATH_TYPE_CONTAINER:
        return expr_dpath_to_container(src_type, src_dpath, dst_dpathp, bst);
    default:
        assert(0);
    }
}

struct box *
expr_dpath_get_parent_box(enum expr_dpath_type type,
                          union expr_dpath dpath)
{
    switch (type) {
    case EXPR_DPATH_TYPE_ITEM:
        return dpath.item.tk->box;
    case EXPR_DPATH_TYPE_CONTAINER:
        return dpath.container.box->parent_box;
    default:
        assert(0);
    }
}

bitpunch_status_t
expr_dpath_get_size(enum expr_dpath_type type, union expr_dpath dpath,
                    int64_t *dpath_sizep,
                    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    switch (type) {
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = tracker_get_item_size_internal(dpath.item.tk,
                                                dpath_sizep, bst);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        bt_ret = box_get_used_size(dpath.container.box, dpath_sizep, bst);
        break ;
    default:
        assert(0);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return BITPUNCH_OK;
}

const struct ast_node *
expr_dpath_get_node(enum expr_dpath_type type, union expr_dpath dpath)
{
    switch (type) {
    case EXPR_DPATH_TYPE_ITEM:
        return dpath.item.tk->item_node;
    case EXPR_DPATH_TYPE_CONTAINER:
        return dpath.container.box->node;
    default:
        assert(0);
    }
}

struct track_path
expr_dpath_get_track_path(enum expr_dpath_type type,
                          union expr_dpath dpath)
{
    switch (type) {
    case EXPR_DPATH_TYPE_ITEM:
        return dpath.item.tk->cur;
    case EXPR_DPATH_TYPE_CONTAINER:
        return dpath.container.box->track_path;
    default:
        assert(0);
    }
}

int
expr_dpath_is(enum expr_dpath_type type1,
              union expr_dpath dpath1,
              enum expr_dpath_type type2,
              union expr_dpath dpath2)
{
    if (type1 != type2) {
        return FALSE;
    }
    switch (type1) {
    case EXPR_DPATH_TYPE_CONTAINER:
        return dpath1.container.box == dpath2.container.box;
    case EXPR_DPATH_TYPE_ITEM:
        return dpath1.item.tk == dpath2.item.tk;
    default:
        return FALSE;
    }
}

/**
 * @brief find the common ancestor box of two dpath expressions
 *
 * @param[out] ancestor1_typep dpath type of closest common ancestor
 * (from dpath1's ancestors)
 * @param[out] ancestor1_dpathp dpath of closest common ancestor (from
 * dpath1's ancestors)
 * @param[out] ancestor2_typep dpath type of closest common ancestor
 * (from dpath2's ancestors)
 * @param[out] ancestor2_dpathp dpath of closest common ancestor (from
 * dpath2's ancestors)
 */
void
expr_dpath_find_common_ancestor(enum expr_dpath_type type1,
                                union expr_dpath dpath1,
                                enum expr_dpath_type type2,
                                union expr_dpath dpath2,
                                enum expr_dpath_type *ancestor1_typep,
                                union expr_dpath *ancestor1_dpathp,
                                enum expr_dpath_type *ancestor2_typep,
                                union expr_dpath *ancestor2_dpathp)
{
    struct track_path path1, path2;
    struct box *pbox1, *pbox2;
    struct box *box1, *box2;
    struct box **ancestors1, **ancestors2;
    int path_eq;
    enum expr_dpath_type ancestor1_type, ancestor2_type;
    union expr_dpath ancestor1_dpath, ancestor2_dpath;

    pbox1 = expr_dpath_get_parent_box(type1, dpath1);
    pbox2 = expr_dpath_get_parent_box(type2, dpath2);

    if (NULL != pbox1) {
        ancestors1 = alloca((pbox1->depth_level + 1) * sizeof(*ancestors1));
        box1 = pbox1;
        while (NULL != box1) {
            *ancestors1 = box1;
            ++ancestors1;
            box1 = box1->parent_box;
        }
    }
    if (NULL != pbox2) {
        ancestors2 = alloca((pbox2->depth_level + 1) * sizeof(*ancestors2));
        box2 = pbox2;
        while (NULL != box2) {
            *ancestors2 = box2;
            ++ancestors2;
            box2 = box2->parent_box;
        }
    }
    if (NULL == pbox1) {
        ancestor1_type = type1;
        ancestor1_dpath = dpath1;
        if (NULL == pbox2) {
            ancestor2_type = type2;
            ancestor2_dpath = dpath2;
        } else {
            ancestor2_type = EXPR_DPATH_TYPE_CONTAINER;
            ancestor2_dpath.container.box = ancestors2[-1];
        }
        goto end;
    }
    if (NULL == pbox2) {
        ancestor1_type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor1_dpath.container.box = ancestors1[-1];
        ancestor2_type = type2;
        ancestor2_dpath = dpath2;
        goto end;
    }
    do {
        --ancestors1;
        --ancestors2;
        path_eq = track_path_eq((*ancestors1)->track_path,
                                (*ancestors2)->track_path);
    }
    while (path_eq && *ancestors1 != pbox1 && *ancestors2 != pbox2);

    if (!path_eq) {
        ancestor1_type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor1_dpath.container.box = ancestors1[1];
        ancestor2_type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor2_dpath.container.box = ancestors2[1];
        goto end;
    }
    if (*ancestors1 == pbox1) {
        path1 = expr_dpath_get_track_path(type1, dpath1);
    } else {
        path1 = ancestors1[-1]->track_path;
    }
    if (*ancestors2 == pbox2) {
        path2 = expr_dpath_get_track_path(type2, dpath2);
    } else {
        path2 = ancestors2[-1]->track_path;
    }
    path_eq = track_path_eq(path1, path2);
    if (!path_eq) {
        ancestor1_type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor1_dpath.container.box = *ancestors1;
        ancestor2_type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor2_dpath.container.box = *ancestors2;
        goto end;
    }
    if (*ancestors1 == pbox1) {
        ancestor1_type = type1;
        ancestor1_dpath = dpath1;
    } else {
        ancestor1_type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor1_dpath.container.box = ancestors1[-1];
    }
    if (*ancestors2 == pbox2) {
        ancestor2_type = type2;
        ancestor2_dpath = dpath2;
    } else {
        ancestor2_type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor2_dpath.container.box = ancestors2[-1];
    }

  end:
    if (NULL != ancestor1_typep) {
        *ancestor1_typep = ancestor1_type;
    }
    if (NULL != ancestor1_dpathp) {
        *ancestor1_dpathp = ancestor1_dpath;
    }
    if (NULL != ancestor2_typep) {
        *ancestor2_typep = ancestor2_type;
    }
    if (NULL != ancestor2_dpathp) {
        *ancestor2_dpathp = ancestor2_dpath;
    }
}

int
expr_value_cmp_integer(union expr_value value1, union expr_value value2)
{
    return ((value1.integer > value2.integer)
            - (value1.integer < value2.integer));
}

int
expr_value_cmp_string(union expr_value value1, union expr_value value2)
{
    int cmp_ret;

    cmp_ret = memcmp(value1.string.str, value2.string.str,
                     MIN(value1.string.len, value2.string.len));
    if (0 != cmp_ret) {
        return cmp_ret;
    }
    return ((value1.string.len > value2.string.len)
            - (value1.string.len < value2.string.len));
}

int
expr_value_cmp_bytes(union expr_value value1, union expr_value value2)
{
    int cmp_ret;

    cmp_ret = memcmp(value1.bytes.buf, value2.bytes.buf,
                     MIN(value1.bytes.len, value2.bytes.len));
    if (0 != cmp_ret) {
        return cmp_ret;
    }
    return ((value1.bytes.len > value2.bytes.len)
            - (value1.bytes.len < value2.bytes.len));
}

/**
 * @brief compare two expression values of the same type @ref type
 *
 * @retval < 0 if value1 < value2
 * @retval 0 if value1 == value2
 * @retval > 0 if value1 > value2
 */
int
expr_value_cmp(enum expr_value_type type,
               union expr_value value1, union expr_value value2)
{
    switch (type) {
    case EXPR_VALUE_TYPE_INTEGER:
        return expr_value_cmp_integer(value1, value2);
    case EXPR_VALUE_TYPE_STRING:
        return expr_value_cmp_string(value1, value2);
    case EXPR_VALUE_TYPE_BYTES:
        return expr_value_cmp_bytes(value1, value2);
    default:
        assert(0);
    }
}


bitpunch_status_t
expr_evaluate_value_internal(struct ast_node *expr, struct box *scope,
                             union expr_value *eval_valuep,
                             struct browse_state *bst)
{
    switch (expr->type) {
    case AST_NODE_TYPE_REXPR_NATIVE:
        *eval_valuep = expr->u.rexpr_native.value;
        return BITPUNCH_OK;
    case AST_NODE_TYPE_REXPR_OP_EQ:
    case AST_NODE_TYPE_REXPR_OP_NE:
    case AST_NODE_TYPE_REXPR_OP_GT:
    case AST_NODE_TYPE_REXPR_OP_LT:
    case AST_NODE_TYPE_REXPR_OP_GE:
    case AST_NODE_TYPE_REXPR_OP_LE:
    case AST_NODE_TYPE_REXPR_OP_LOR:
    case AST_NODE_TYPE_REXPR_OP_LAND:
    case AST_NODE_TYPE_REXPR_OP_BWOR:
    case AST_NODE_TYPE_REXPR_OP_BWXOR:
    case AST_NODE_TYPE_REXPR_OP_BWAND:
    case AST_NODE_TYPE_REXPR_OP_LSHIFT:
    case AST_NODE_TYPE_REXPR_OP_RSHIFT:
    case AST_NODE_TYPE_REXPR_OP_ADD:
    case AST_NODE_TYPE_REXPR_OP_SUB:
    case AST_NODE_TYPE_REXPR_OP_MUL:
    case AST_NODE_TYPE_REXPR_OP_DIV:
    case AST_NODE_TYPE_REXPR_OP_MOD:
        return expr_evaluate_binary_operator(expr, scope, eval_valuep, bst);
    case AST_NODE_TYPE_REXPR_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT:
        return expr_evaluate_unary_operator(expr, scope, eval_valuep, bst);
    case AST_NODE_TYPE_REXPR_OP_SIZEOF:
        return expr_evaluate_sizeof(expr, scope, eval_valuep, bst);
    case AST_NODE_TYPE_REXPR_OP_ADDROF:
        return expr_evaluate_addrof(expr, scope, eval_valuep, bst);
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        return expr_evaluate_value_filter(expr, scope, eval_valuep, bst);
    case AST_NODE_TYPE_REXPR_OP_FCALL:
        return expr_evaluate_value_fcall(expr, scope, eval_valuep, bst);
    default:
        if (EXPR_DPATH_TYPE_NONE != expr->u.rexpr.dpath_type) {
            return expr_evaluate_value_from_dpath(expr, scope, eval_valuep,
                                                  bst);
        }
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                       "cannot evaluate expression value: "
                       "not implemented on type '%s'",
                       ast_node_type_str(expr->type));
        return BITPUNCH_NOT_IMPLEMENTED;
    }
    /*NOT REACHED*/
}

static bitpunch_status_t
expr_evaluate_value_from_dpath(struct ast_node *expr, struct box *scope,
                               union expr_value *eval_valuep,
                               struct browse_state *bst)
{
    union expr_dpath eval_dpath;
    bitpunch_status_t bt_ret;

    bt_ret = expr_evaluate_dpath_internal(expr, scope, &eval_dpath, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_read_dpath_value_internal(expr, eval_dpath,
                                            eval_valuep, bst);
    expr_dpath_destroy(expr->u.rexpr.dpath_type, eval_dpath);
    return bt_ret;
}

static bitpunch_status_t
expr_evaluate_binary_operator(struct ast_node *expr, struct box *scope,
                              union expr_value *eval_valuep,
                              struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    union expr_value operands[2];

    bt_ret = expr_evaluate_value_internal(expr->u.rexpr_op.op.operands[0],
                                          scope, &operands[0], bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_evaluate_value_internal(expr->u.rexpr_op.op.operands[1],
                                          scope, &operands[1], bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    *eval_valuep = expr->u.rexpr_op.evaluator->eval_fn(operands);
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_unary_operator(struct ast_node *expr, struct box *scope,
                             union expr_value *eval_valuep,
                             struct browse_state *bst)
{
    union expr_value operands[1];
    bitpunch_status_t bt_ret;

    bt_ret = expr_evaluate_value_internal(expr->u.rexpr_op.op.operands[0],
                                          scope, &operands[0], bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    *eval_valuep = expr->u.rexpr_op.evaluator->eval_fn(operands);
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_sizeof(struct ast_node *expr, struct box *scope,
                     union expr_value *eval_valuep,
                     struct browse_state *bst)
{
    struct ast_node *opd;
    int64_t item_size;

    opd = expr->u.rexpr_op.op.operands[0];
    if (ast_node_is_rexpr(opd)) {
        bitpunch_status_t bt_ret;
        union expr_dpath dpath_eval;

        bt_ret = expr_evaluate_dpath_internal(opd, scope, &dpath_eval, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = expr_dpath_get_size(opd->u.rexpr.dpath_type, dpath_eval,
                                     &item_size, bst);
        expr_dpath_destroy(opd->u.rexpr.dpath_type, dpath_eval);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    } else {
        /* We assert a static size here, because dynamic sizes shall
         * always be specified to sizeof() through use of dpath
         * expressions.
         *
         * Note: the parser should have replaced the static sizeof by
         * constants. But anyway, fallback here in case it hasn't. */
        assert(0 == (opd->flags & ASTFLAG_IS_SPAN_SIZE_DYNAMIC));
        item_size = ast_node_get_min_span_size(opd);
    }
    eval_valuep->integer = item_size;
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_addrof(struct ast_node *expr, struct box *scope,
                     union expr_value *eval_valuep,
                     struct browse_state *bst)
{
    struct ast_node *opd;
    bitpunch_status_t bt_ret;
    union expr_dpath dpath_eval;
    int64_t item_offset;

    opd = expr->u.rexpr_op.op.operands[0];
    bt_ret = expr_evaluate_dpath_internal(opd, scope, &dpath_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    switch (opd->u.rexpr.dpath_type) {
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = tracker_get_item_offset_internal(dpath_eval.item.tk,
                                                  &item_offset, bst);
        expr_dpath_destroy_item(dpath_eval);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        item_offset = dpath_eval.container.box->start_offset_used;
        expr_dpath_destroy_container(dpath_eval);
        break ;
    default:
        assert(0);
    }
    eval_valuep->integer = item_offset;
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_value_filter(struct ast_node *expr, struct box *scope,
                           union expr_value *eval_valuep,
                           struct browse_state *bst)
{
    struct ast_node *opd;
    bitpunch_status_t bt_ret;
    union expr_dpath unfiltered_dpath;

    opd = expr->u.rexpr_op.op.operands[0];
    bt_ret = expr_evaluate_dpath_internal(opd, scope, &unfiltered_dpath,
                                          bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_read_dpath_value_internal(opd, unfiltered_dpath,
                                            eval_valuep, bst);
    expr_dpath_destroy(opd->u.rexpr.dpath_type, unfiltered_dpath);
    return bt_ret;
}

static bitpunch_status_t
expr_evaluate_value_fcall(struct ast_node *expr, struct box *scope,
                          union expr_value *eval_valuep,
                          struct browse_state *bst)
{
    const struct expr_builtin_fn *builtin;
    struct ast_node *object;
    struct func_param_list *params;
    int n_params;

    builtin = expr->u.rexpr_op_fcall.builtin;
    object = expr->u.rexpr_op_fcall.object;
    params = expr->u.rexpr_op_fcall.func_params;
    n_params = expr->u.rexpr_op_fcall.n_func_params;

    assert(NULL != builtin->eval_value_fn);
    return builtin->eval_value_fn(object, params, n_params,
                                  scope, eval_valuep, bst);
}

bitpunch_status_t
expr_evaluate_dpath_internal(struct ast_node *expr, struct box *scope,
                             union expr_dpath *eval_dpathp,
                             struct browse_state *bst)
{
    switch (expr->type) {
    case AST_NODE_TYPE_REXPR_FILE:
        return expr_evaluate_dpath_file(expr, scope, eval_dpathp, bst);
    case AST_NODE_TYPE_REXPR_SELF:
        return expr_evaluate_dpath_self(expr, scope, eval_dpathp, bst);
    case AST_NODE_TYPE_REXPR_FIELD:
        return expr_evaluate_dpath_field(expr, scope, eval_dpathp, bst);
    case AST_NODE_TYPE_REXPR_MEMBER:
        return expr_evaluate_dpath_link(expr, scope, eval_dpathp, bst);
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
        return expr_evaluate_dpath_subscript(expr, scope, eval_dpathp, bst);
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        return expr_evaluate_dpath_subscript_slice(expr, scope,
                                                   eval_dpathp, bst);
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        return expr_evaluate_dpath_filter(expr, scope, eval_dpathp, bst);
    case AST_NODE_TYPE_REXPR_OP_FCALL:
        return expr_evaluate_dpath_fcall(expr, scope, eval_dpathp, bst);
    case AST_NODE_TYPE_REXPR_INTERPRETER:
        return expr_evaluate_dpath_interpreter(expr, scope,
                                               eval_dpathp, bst);
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        return expr_evaluate_dpath_as_type(expr, scope,
                                           eval_dpathp, bst);
    default:
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                       "cannot evaluate dpath on expression of type '%s'",
                       ast_node_type_str(expr->type));
        return BITPUNCH_INVALID_PARAM;
    }
    /*NOT REACHED*/
}

static bitpunch_status_t
expr_evaluate_dpath_anchor_common(struct ast_node *anchor_expr,
                                  const struct ast_node *anchor_block,
                                  struct box *scope,
                                  struct tracker **tkp,
                                  struct browse_state *bst)
{
    union expr_dpath anchor_eval;
    bitpunch_status_t bt_ret;
    struct tracker *tk;

    if (NULL != anchor_expr) {
        bt_ret = expr_evaluate_dpath_internal(anchor_expr, scope,
                                              &anchor_eval, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        switch (anchor_expr->u.rexpr.dpath_type) {
        case EXPR_DPATH_TYPE_ITEM:
            tk = anchor_eval.item.tk;
            assert(NULL != tk->item_node);
            bt_ret = tracker_enter_item_internal(tk, bst);
            if (BITPUNCH_OK != bt_ret) {
                tracker_delete(tk);
                return bt_ret;
            }
            break ;
        case EXPR_DPATH_TYPE_CONTAINER:
            tk = track_box(anchor_eval.container.box);
            break ;
        default:
            assert(0);
        }
        if (AST_NODE_TYPE_BLOCK_DEF != tk->box->node->type) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR, &anchor_expr->loc,
                "left-side of member operator does not evaluate to a "
                "block type");
            tracker_delete(tk);
            return BITPUNCH_INVALID_PARAM;
        }
    } else {
        struct box *anchor_box;

        /* find the closest dpath's field in the scope, browsing boxes
         * upwards */
        anchor_box = scope;
        while (anchor_box->node != anchor_block) {
            anchor_box = (NULL != anchor_box->unfiltered_box ?
                          anchor_box->unfiltered_box :
                          anchor_box->parent_box);
            assert(NULL != anchor_box);
        }
        tk = track_box(anchor_box);
    }
    *tkp = tk;
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_dpath_file(struct ast_node *expr, struct box *scope,
                         union expr_dpath *eval_dpathp,
                         struct browse_state *bst)
{
    struct box *file_box;

    assert(EXPR_DPATH_TYPE_CONTAINER == expr->u.rexpr.dpath_type);
    file_box = scope;
    while (NULL != file_box->parent_box ||
           NULL != file_box->unfiltered_box) {
        if (NULL != file_box->parent_box) {
            file_box = file_box->parent_box;
        } else {
            file_box = file_box->unfiltered_box;
        }
    }
    box_acquire(file_box);
    eval_dpathp->container.box = file_box;
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_dpath_self(struct ast_node *expr, struct box *scope,
                         union expr_dpath *eval_dpathp,
                         struct browse_state *bst)
{
    struct box *self_box;

    assert(EXPR_DPATH_TYPE_CONTAINER == expr->u.rexpr.dpath_type);
    self_box = scope;
    box_acquire(self_box);
    eval_dpathp->container.box = self_box;
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_dpath_field(struct ast_node *expr, struct box *scope,
                           union expr_dpath *eval_dpathp,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct tracker *tk;

    assert(EXPR_DPATH_TYPE_ITEM == expr->u.rexpr.dpath_type);
    bt_ret = expr_evaluate_dpath_anchor_common(
        expr->u.rexpr_field.anchor_expr,
        expr->u.rexpr_field.block, scope, &tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = tracker_goto_field_internal(tk,
                                         expr->u.rexpr_field.field, FALSE,
                                         bst);
    if (BITPUNCH_OK != bt_ret) {
        tracker_delete(tk);
        return bt_ret;
    }
    eval_dpathp->item.tk = tk;
    return BITPUNCH_OK;
}

bitpunch_status_t
link_evaluate_dpath_internal(const struct link *link, struct box *scope,
                             enum expr_dpath_type *dpath_typep,
                             union expr_dpath *eval_dpathp,
                             struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    union expr_dpath dst_path_eval;

    bt_ret = expr_evaluate_dpath_internal(link->dst_expr, scope,
                                          &dst_path_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    *dpath_typep = link->dst_expr->u.rexpr.dpath_type;
    *eval_dpathp = dst_path_eval;
    return BITPUNCH_OK;
}

bitpunch_status_t
link_evaluate_value_internal(const struct link *link, struct box *scope,
                             enum expr_value_type *value_typep,
                             union expr_value *eval_valuep,
                             struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    union expr_value dst_value_eval;

    bt_ret = expr_evaluate_value_internal(link->dst_expr, scope,
                                          &dst_value_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    *value_typep = link->dst_expr->u.rexpr.value_type;
    *eval_valuep = dst_value_eval;
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_dpath_link(struct ast_node *expr, struct box *scope,
                         union expr_dpath *eval_dpathp,
                         struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    bitpunch_status_t bt_ret_1st_failure;
    struct tracker *tk;
    struct box *member_scope;
    const struct link *link;
    int cond_eval;
    enum expr_dpath_type dpath_type;
    union expr_dpath eval_dpath;
    union expr_dpath converted_dpath;

    bt_ret = expr_evaluate_dpath_anchor_common(
        expr->u.rexpr_link.anchor_expr,
        expr->u.rexpr_link.block, scope, &tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    member_scope = tk->box;
    box_acquire(member_scope);
    tracker_delete(tk);

    bt_ret_1st_failure = BITPUNCH_OK;
    for (link = expr->u.rexpr_link.link; NULL != link;
         link = (const struct link *)link->nstmt.next_sibling) {
        bt_ret = evaluate_conditional_internal(link->nstmt.stmt.cond,
                                               member_scope, &cond_eval,
                                               bst);
        if (BITPUNCH_OK == bt_ret) {
            if (cond_eval) {
                break ;
            }
        } else {
            if (BITPUNCH_OK == bt_ret_1st_failure) {
                bt_ret_1st_failure = bt_ret;
            }
        }
    }
    if (NULL == link) {
        box_delete(member_scope);
        if (BITPUNCH_OK == bt_ret_1st_failure) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &expr->loc,
                           "no member named '%s' is associated to block in "
                           "current evaluation context",
                           expr->u.rexpr_link.link->nstmt.name);
            return BITPUNCH_INVALID_PARAM;
        } else {
            return bt_ret;
        }
    }
    bt_ret = link_evaluate_dpath_internal(link, member_scope,
                                          &dpath_type, &eval_dpath, bst);
    box_delete(member_scope);
    if (BITPUNCH_OK == bt_ret && dpath_type != expr->u.rexpr.dpath_type) {
        bt_ret = expr_dpath_to_dpath(dpath_type, eval_dpath,
                                     expr->u.rexpr.dpath_type,
                                     &converted_dpath, bst);
        expr_dpath_destroy(dpath_type, eval_dpath);
        eval_dpath = converted_dpath;
    }
    if (BITPUNCH_OK == bt_ret && NULL != eval_dpathp) {
        *eval_dpathp = eval_dpath;
    }
    return bt_ret;
}

static bitpunch_status_t
expr_evaluate_dpath_subscript(struct ast_node *expr, struct box *scope,
                              union expr_dpath *eval_dpathp,
                              struct browse_state *bst)
{
    struct ast_node *anchor_expr;
    union expr_dpath anchor_eval;
    bitpunch_status_t bt_ret;
    struct tracker *tk;

    assert(EXPR_DPATH_TYPE_ITEM == expr->u.rexpr.dpath_type);
    anchor_expr = expr->u.rexpr_op_subscript_common.anchor_expr;

    bt_ret = expr_evaluate_dpath_internal(anchor_expr, scope,
                                          &anchor_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    switch (anchor_expr->u.rexpr.dpath_type) {
    case EXPR_DPATH_TYPE_ITEM:
        tk = anchor_eval.item.tk;
        bt_ret = tracker_enter_item_internal(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(tk);
            return bt_ret;
        }
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        tk = track_box(anchor_eval.container.box);
        box_delete(anchor_eval.container.box);
        break ;
    default:
        assert(0);
    }
    bt_ret = tracker_goto_index_internal(
        tk, expr->u.rexpr_op_subscript.index, "subscript index",
        scope, FALSE, FALSE, bst);
    if (BITPUNCH_OK != bt_ret) {
        //TODO log
        tracker_delete(tk);
        return bt_ret;
    }
    eval_dpathp->item.tk = tk;
    return BITPUNCH_OK;
}


static bitpunch_status_t
expr_evaluate_dpath_subscript_slice(struct ast_node *expr,
                                    struct box *scope,
                                    union expr_dpath *eval_dpathp,
                                    struct browse_state *bst)
{
    struct ast_node *anchor_expr;
    union expr_dpath anchor_eval;
    bitpunch_status_t bt_ret;
    struct tracker *tk_slice_start = NULL;
    struct tracker *tk_slice_end = NULL;
    struct box *slice_box;

    assert(EXPR_DPATH_TYPE_CONTAINER == expr->u.rexpr.dpath_type);
    anchor_expr = expr->u.rexpr_op_subscript_common.anchor_expr;

    bt_ret = expr_evaluate_dpath_internal(anchor_expr, scope,
                                          &anchor_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    switch (anchor_expr->u.rexpr.dpath_type) {
    case EXPR_DPATH_TYPE_ITEM:
        tk_slice_start = anchor_eval.item.tk;
        bt_ret = tracker_enter_item_internal(tk_slice_start, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(tk_slice_start);
            return bt_ret;
        }
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        tk_slice_start = track_box(anchor_eval.container.box);
        box_delete(anchor_eval.container.box);
        break ;
    default:
        assert(0);
    }

    bt_ret = tracker_goto_index_internal(
        tk_slice_start,
        expr->u.rexpr_op_subscript_slice.start, "start index of slice",
        scope, TRUE, FALSE, bst);
    if (BITPUNCH_OK != bt_ret) {
        //TODO log
        goto end;
    }

    tk_slice_end = tracker_dup(tk_slice_start);
    bt_ret = tracker_goto_index_internal(
        tk_slice_end,
        expr->u.rexpr_op_subscript_slice.end, "end index of slice",
        scope, TRUE, TRUE, bst);
    if (BITPUNCH_OK != bt_ret) {
        //TODO log
        goto end;
    }
    if (tk_slice_start->cur.u.array.index
        > tk_slice_end->cur.u.array.index) {
        assert(NULL != expr->u.rexpr_op_subscript_slice.start.key);
        semantic_error(
            SEMANTIC_LOGLEVEL_ERROR,
            &expr->u.rexpr_op_subscript_slice.start.key->loc,
            "start index of slice is greater than end index");
        return BITPUNCH_DATA_ERROR;
    }
    slice_box = box_new_slice_box(tk_slice_start, tk_slice_end, bst);
    if (NULL == slice_box) {
        //TODO log
        bt_ret = BITPUNCH_DATA_ERROR;
        goto end;
    }
    eval_dpathp->container.box = slice_box;
    bt_ret = BITPUNCH_OK;

  end:
    tracker_delete(tk_slice_start);
    tracker_delete(tk_slice_end);
    return bt_ret;
}

static bitpunch_status_t
expr_evaluate_dpath_filter(struct ast_node *expr, struct box *scope,
                           union expr_dpath *eval_dpathp,
                           struct browse_state *bst)
{
    struct ast_node *opd;
    bitpunch_status_t bt_ret;
    union expr_dpath unfiltered_dpath;
    union expr_dpath unfiltered_dcont;
    struct box *filtered_box;

    opd = expr->u.rexpr_op.op.operands[0];
    bt_ret = expr_evaluate_dpath_internal(opd, scope, &unfiltered_dpath,
                                          bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_dpath_to_container(opd->u.rexpr.dpath_type,
                                     unfiltered_dpath,
                                     &unfiltered_dcont, bst);
    if (BITPUNCH_OK != bt_ret) {
        expr_dpath_destroy(opd->u.rexpr.dpath_type, unfiltered_dpath);
        return bt_ret;
    }
    bt_ret = box_apply_filters(unfiltered_dcont.container.box,
                               &filtered_box, bst);
    if (BITPUNCH_OK == bt_ret) {
        eval_dpathp->container.box = filtered_box;
    }
    expr_dpath_destroy(opd->u.rexpr.dpath_type, unfiltered_dpath);
    expr_dpath_destroy(EXPR_DPATH_TYPE_CONTAINER, unfiltered_dcont);
    return bt_ret;
}

static bitpunch_status_t
expr_evaluate_dpath_fcall(struct ast_node *expr, struct box *scope,
                          union expr_dpath *eval_dpathp,
                          struct browse_state *bst)
{
    const struct expr_builtin_fn *builtin;
    struct ast_node *object;
    struct func_param_list *params;
    int n_params;

    builtin = expr->u.rexpr_op_fcall.builtin;
    object = expr->u.rexpr_op_fcall.object;
    params = expr->u.rexpr_op_fcall.func_params;
    n_params = expr->u.rexpr_op_fcall.n_func_params;

    assert(NULL != builtin->eval_dpath_fn);
    return builtin->eval_dpath_fn(object, params, n_params,
                                  scope, eval_dpathp, bst);
}

static bitpunch_status_t
expr_evaluate_dpath_interpreter(struct ast_node *expr, struct box *scope,
                                union expr_dpath *eval_dpathp,
                                struct browse_state *bst)
{
    struct ast_node *target;

    target = expr->u.rexpr_filter.target;
    assert(NULL != target);
    return expr_evaluate_dpath_internal(target, scope,
                                        eval_dpathp, bst);
}

static bitpunch_status_t
expr_evaluate_dpath_as_type(struct ast_node *expr, struct box *scope,
                            union expr_dpath *eval_dpathp,
                            struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct ast_node *as_type;
    struct ast_node *target;
    union expr_dpath source_eval;
    struct box *dst_box;
    struct box *as_box;

    as_type = expr->u.rexpr_as_type.as_type;
    target = expr->u.rexpr_filter.target;
    bt_ret = expr_evaluate_dpath_internal(target,
                                          scope, &source_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (!ast_node_is_item(as_type)) {
        *eval_dpathp = source_eval;
        return BITPUNCH_OK;
    }
    bt_ret = expr_dpath_to_box(target->u.rexpr.dpath_type,
                               source_eval, &dst_box, bst);
    expr_dpath_destroy(target->u.rexpr.dpath_type, source_eval);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    as_box = box_new_as_box(dst_box, as_type,
                            dst_box->start_offset_used, bst);
    box_delete(dst_box);
    if (NULL == as_box) {
        return BITPUNCH_DATA_ERROR;
    }
    eval_dpathp->container.box = as_box;
    return BITPUNCH_OK;
}

bitpunch_status_t
evaluate_conditional_internal(struct ast_node *cond, struct box *scope,
                              int *evalp, struct browse_state *bst)
{
    int outer_cond_eval;
    union expr_value cond_eval;
    bitpunch_status_t bt_ret;

    if (NULL == cond) {
        // no condition = always true
        *evalp = TRUE;
        return BITPUNCH_OK;
    }
    if (NULL != cond->u.conditional.outer_cond) {
        bt_ret = evaluate_conditional_internal(
            cond->u.conditional.outer_cond, scope, &outer_cond_eval, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        if (!outer_cond_eval) {
            return FALSE;
        }
    }
    bt_ret = expr_evaluate_value_internal(cond->u.conditional.cond_expr,
                                          scope, &cond_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if ((cond->flags & ASTFLAG_REVERSE_COND)) {
        *evalp = !cond_eval.boolean;
    } else {
        *evalp = cond_eval.boolean;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_read_dpath_value_internal(struct ast_node *expr,
                               union expr_dpath dpath,
                               union expr_value *expr_valuep,
                               struct browse_state *bst)
{
    switch (expr->type) {
    case AST_NODE_TYPE_REXPR_MEMBER:
        return expr_read_dpath_value_link(expr, dpath, expr_valuep, bst);
    case AST_NODE_TYPE_REXPR_INTERPRETER:
        return expr_read_dpath_value_interpreter(expr, dpath,
                                                 expr_valuep, bst);
    default:
        return expr_read_dpath_value_default(expr, dpath, expr_valuep, bst);
    }
}

static bitpunch_status_t
expr_read_dpath_value_default(struct ast_node *expr,
                              union expr_dpath dpath,
                              union expr_value *expr_valuep,
                              struct browse_state *bst)
{
    enum expr_value_type value_type;
    bitpunch_status_t bt_ret;

    switch (expr->u.rexpr.dpath_type) {
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = tracker_read_item_value_internal(
            dpath.item.tk, &value_type, expr_valuep, bst);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        bt_ret = box_read_value_internal(
            dpath.container.box, &value_type, expr_valuep, bst);
        break ;
    default:
        assert(0);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(value_type == expr->u.rexpr.value_type);
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_read_dpath_value_link(struct ast_node *expr,
                           union expr_dpath dpath,
                           union expr_value *expr_valuep,
                           struct browse_state *bst)
{
    return expr_read_dpath_value_internal(
        expr->u.rexpr_link.link->dst_expr, dpath, expr_valuep, bst);
}

static bitpunch_status_t
expr_read_dpath_value_interpreter(struct ast_node *expr,
                                  union expr_dpath dpath,
                                  union expr_value *expr_valuep,
                                  struct browse_state *bst)
{
    enum expr_value_type value_type;
    bitpunch_status_t bt_ret;
    struct ast_node *target;
    const char *item_data;
    int64_t item_size;

    target = expr->u.rexpr_filter.target;

    switch (target->u.rexpr.dpath_type) {
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = tracker_read_item_raw_internal(
            dpath.item.tk, &item_data, &item_size, bst);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        item_data = (dpath.container.box->file_hdl->bf_data
                     + box_get_start_offset(dpath.container.box));
        bt_ret = box_get_used_size(dpath.container.box, &item_size, bst);
        break;
    default:
        assert(0);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = interpreter_rcall_read_value(expr, item_data, item_size,
                                          &value_type, expr_valuep, bst);

    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(value_type == expr->u.rexpr.value_type);
    return BITPUNCH_OK;
}

const char *
expr_value_type_str(enum expr_value_type type)
{
    switch (type) {
    case EXPR_VALUE_TYPE_UNSET: return "unset";
    case EXPR_VALUE_TYPE_INTEGER: return "integer";
    case EXPR_VALUE_TYPE_BOOLEAN: return "boolean";
    case EXPR_VALUE_TYPE_STRING: return "string";
    case EXPR_VALUE_TYPE_BYTES: return "bytes";
    }
    return "!!bad expression value type!!";
}

const char *
expr_dpath_type_str(enum expr_dpath_type type)
{
    switch (type) {
    case EXPR_DPATH_TYPE_UNSET: return "unset";
    case EXPR_DPATH_TYPE_NONE: return "none";
    case EXPR_DPATH_TYPE_ITEM: return "item";
    case EXPR_DPATH_TYPE_CONTAINER: return "container";
    }
    return "!!bad expression dpath type!!";
}

void
expr_value_to_hashable(enum expr_value_type type,
                       union expr_value value,
                       const char **bufp, int64_t *lenp)
{
    switch (type) {
    case EXPR_VALUE_TYPE_INTEGER:
        *bufp = (char *)&value.integer;
        *lenp = sizeof (value.integer);
        return ;
    case EXPR_VALUE_TYPE_BOOLEAN:
        *bufp = (char *)&value.boolean;
        *lenp = sizeof (value.boolean);
        return ;
    case EXPR_VALUE_TYPE_STRING:
        *bufp = value.string.str;
        *lenp = value.string.len;
        return ;
    case EXPR_VALUE_TYPE_BYTES:
        *bufp = value.bytes.buf;
        *lenp = value.bytes.len;
        return ;
    default:
        assert(0);
    }
}



/*
 * external API wrappers
 */

bitpunch_status_t
expr_evaluate_value(struct ast_node *expr, struct box *scope,
                    union expr_value *eval_valuep)
{
    struct browse_state st;

    assert(NULL != eval_valuep);

    browse_state_init(&st);
    return expr_evaluate_value_internal(expr, scope, eval_valuep, &st);
}

bitpunch_status_t
expr_evaluate_dpath(struct ast_node *expr, struct box *scope,
                    union expr_dpath *eval_dpathp)
{
    struct browse_state st;

    assert(NULL != eval_dpathp);

    browse_state_init(&st);
    return expr_evaluate_dpath_internal(expr, scope, eval_dpathp, &st);
}

bitpunch_status_t
link_evaluate_dpath(const struct link *link, struct box *scope,
                    enum expr_dpath_type *dpath_typep,
                    union expr_dpath *eval_dpathp)
{
    struct browse_state st;

    browse_state_init(&st);
    return link_evaluate_dpath_internal(link, scope,
                                        dpath_typep, eval_dpathp, &st);
}

bitpunch_status_t
link_evaluate_value(const struct link *link, struct box *scope,
                    enum expr_value_type *value_typep,
                    union expr_value *eval_valuep)
{
    struct browse_state st;

    browse_state_init(&st);
    return link_evaluate_value_internal(link, scope,
                                        value_typep, eval_valuep, &st);
}

bitpunch_status_t
evaluate_conditional(struct ast_node *cond, struct box *scope,
                     int *evalp)
{
    struct browse_state st;

    browse_state_init(&st);
    return evaluate_conditional_internal(cond, scope, evalp, &st);
}
bitpunch_status_t
expr_read_dpath_value(struct ast_node *expr, union expr_dpath dpath,
                      union expr_value *expr_valuep)
{
    struct browse_state st;

    browse_state_init(&st);
    return expr_read_dpath_value_internal(expr, dpath, expr_valuep, &st);
}
