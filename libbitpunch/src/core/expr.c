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
#include "core/filter.h"
#include "core/browse_internal.h"
#include "core/expr_internal.h"
#include "api/bitpunch_api.h"
#include "filters/composite.h"
#include "filters/array_slice.h"


expr_dpath_t shared_expr_dpath_none = {
    .type = EXPR_DPATH_TYPE_NONE,
};

struct expr_evalop_match_item {
    enum ast_node_type    op_type;
    int                   n_opd;       /*!< number of operands */
    enum expr_value_type  opd_types[2]; /*!< type of operands */
    struct expr_evaluator evaluator;
};

/* == */

static expr_value_t
expr_evalop_eq_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_BOOLEAN;
    res.boolean = (operands[0].integer == operands[1].integer);
    return res;
}

static struct expr_evalop_match_item match_eq_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_EQ,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_eq_integer_integer,
    },
};

static expr_value_t
expr_evalop_eq_boolean_boolean(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_BOOLEAN;
    res.boolean = (operands[0].boolean == operands[1].boolean);
    return res;
}

static struct expr_evalop_match_item match_eq_boolean_boolean = {
    .op_type = AST_NODE_TYPE_REXPR_OP_EQ,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_BOOLEAN,
        EXPR_VALUE_TYPE_BOOLEAN
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_eq_boolean_boolean,
    },
};

static expr_value_t
expr_evalop_eq_string_string(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_BOOLEAN;
    res.boolean =
        operands[0].string.len == operands[1].string.len &&
        0 == memcmp(operands[0].string.str,
                    operands[1].string.str, operands[0].string.len);
    return res;
}

static struct expr_evalop_match_item match_eq_string_string = {
    .op_type = AST_NODE_TYPE_REXPR_OP_EQ,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_STRING,
        EXPR_VALUE_TYPE_STRING
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_eq_string_string,
    },
};


/* != */

static expr_value_t
expr_evalop_ne_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_BOOLEAN;
    res.boolean = (operands[0].integer != operands[1].integer);
    return res;
}

static struct expr_evalop_match_item match_ne_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_NE,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_ne_integer_integer,
    },
};

static expr_value_t
expr_evalop_ne_boolean_boolean(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_BOOLEAN;
    res.boolean = (operands[0].boolean != operands[1].boolean);
    return res;
}

static struct expr_evalop_match_item match_ne_boolean_boolean = {
    .op_type = AST_NODE_TYPE_REXPR_OP_NE,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_BOOLEAN,
        EXPR_VALUE_TYPE_BOOLEAN
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_ne_boolean_boolean,
    },
};

static expr_value_t
expr_evalop_ne_string_string(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_BOOLEAN;
    res.boolean =
        operands[0].string.len != operands[1].string.len ||
        0 != memcmp(operands[0].string.str,
                    operands[1].string.str, operands[0].string.len);
    return res;
}

static struct expr_evalop_match_item match_ne_string_string = {
    .op_type = AST_NODE_TYPE_REXPR_OP_NE,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_STRING,
        EXPR_VALUE_TYPE_STRING
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_ne_string_string,
    },
};


/* < */

static expr_value_t
expr_evalop_lt_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_BOOLEAN;
    res.boolean = (operands[0].integer < operands[1].integer);
    return res;
}

static struct expr_evalop_match_item match_lt_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_LT,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_lt_integer_integer,
    },
};

/* <= */

static expr_value_t
expr_evalop_le_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_BOOLEAN;
    res.boolean = (operands[0].integer <= operands[1].integer);
    return res;
}

static struct expr_evalop_match_item match_le_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_LE,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_le_integer_integer,
    },
};

/* > */

static expr_value_t
expr_evalop_gt_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_BOOLEAN;
    res.boolean = (operands[0].integer > operands[1].integer);
    return res;
}

static struct expr_evalop_match_item match_gt_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_GT,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_gt_integer_integer,
    },
};

/* >= */

static expr_value_t
expr_evalop_ge_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_BOOLEAN;
    res.boolean = (operands[0].integer >= operands[1].integer);
    return res;
}

static struct expr_evalop_match_item match_ge_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_GE,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_ge_integer_integer,
    },
};


/* || */

static expr_value_t
expr_evalop_lor_boolean_boolean(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_BOOLEAN;
    res.boolean = (operands[0].boolean || operands[1].boolean);
    return res;
}

static struct expr_evalop_match_item match_lor_boolean_boolean = {
    .op_type = AST_NODE_TYPE_REXPR_OP_LOR,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_BOOLEAN,
        EXPR_VALUE_TYPE_BOOLEAN
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_lor_boolean_boolean,
    },
};

/* && */

static expr_value_t
expr_evalop_land_boolean_boolean(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_BOOLEAN;
    res.boolean = (operands[0].boolean && operands[1].boolean);
    return res;
}

static struct expr_evalop_match_item match_land_boolean_boolean = {
    .op_type = AST_NODE_TYPE_REXPR_OP_LAND,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_BOOLEAN,
        EXPR_VALUE_TYPE_BOOLEAN
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_land_boolean_boolean,
    },
};


/* ! */

static expr_value_t
expr_evalop_lnot_boolean(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_BOOLEAN;
    res.boolean = !operands[0].boolean;
    return res;
}

static struct expr_evalop_match_item match_lnot_boolean = {
    .op_type = AST_NODE_TYPE_REXPR_OP_LNOT,
    .n_opd = 1,
    .opd_types = {
        EXPR_VALUE_TYPE_BOOLEAN
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_BOOLEAN,
        .eval_fn = expr_evalop_lnot_boolean,
    },
};


/* | */

static expr_value_t
expr_evalop_bwor_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_INTEGER;
    res.integer = operands[0].integer | operands[1].integer;
    return res;
}

static struct expr_evalop_match_item match_bwor_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_BWOR,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_bwor_integer_integer,
    },
};

/* & */

static expr_value_t
expr_evalop_bwand_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_INTEGER;
    res.integer = operands[0].integer & operands[1].integer;
    return res;
}

static struct expr_evalop_match_item match_bwand_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_BWAND,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_bwand_integer_integer,
    },
};

/* ^ */

static expr_value_t
expr_evalop_bwxor_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_INTEGER;
    res.integer = operands[0].integer ^ operands[1].integer;
    return res;
}

static struct expr_evalop_match_item match_bwxor_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_BWXOR,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_bwxor_integer_integer,
    },
};

/* ~ */

static expr_value_t
expr_evalop_bwnot_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_INTEGER;
    res.integer = ~operands[0].integer;
    return res;
}

static struct expr_evalop_match_item match_bwnot_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_BWNOT,
    .n_opd = 1,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_bwnot_integer,
    },
};


/* << */

static expr_value_t
expr_evalop_lshift_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_INTEGER;
    res.integer = operands[0].integer << operands[1].integer;
    return res;
}

static struct expr_evalop_match_item match_lshift_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_LSHIFT,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_lshift_integer_integer,
    },
};

/* >> */

static expr_value_t
expr_evalop_rshift_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_INTEGER;
    res.integer = operands[0].integer >> operands[1].integer;
    return res;
}

static struct expr_evalop_match_item match_rshift_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_RSHIFT,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_rshift_integer_integer,
    },
};


/* + */

static expr_value_t
expr_evalop_add_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_INTEGER;
    res.integer = operands[0].integer + operands[1].integer;
    return res;
}

static struct expr_evalop_match_item match_add_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_ADD,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_add_integer_integer,
    },
};

/* - */

static expr_value_t
expr_evalop_sub_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_INTEGER;
    res.integer = operands[0].integer - operands[1].integer;
    return res;
}

static struct expr_evalop_match_item match_sub_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_SUB,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_sub_integer_integer,
    },
};

/* * */

static expr_value_t
expr_evalop_mul_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_INTEGER;
    res.integer = operands[0].integer * operands[1].integer;
    return res;
}

static struct expr_evalop_match_item match_mul_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_MUL,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_mul_integer_integer,
    },
};

/* / */

static expr_value_t
expr_evalop_div_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_INTEGER;
    res.integer = operands[0].integer / operands[1].integer;
    return res;
}

static struct expr_evalop_match_item match_div_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_DIV,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_div_integer_integer,
    },
};

/* % */

static expr_value_t
expr_evalop_mod_integer_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_INTEGER;
    res.integer = operands[0].integer % operands[1].integer;
    return res;
}

static struct expr_evalop_match_item match_mod_integer_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_MOD,
    .n_opd = 2,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER,
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_mod_integer_integer,
    },
};

/* unary - */

static expr_value_t
expr_evalop_uplus_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_INTEGER;
    res.integer = operands[0].integer;
    return res;
}

static struct expr_evalop_match_item match_uplus_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_UPLUS,
    .n_opd = 1,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .eval_fn = expr_evalop_uplus_integer,
    },
};

static expr_value_t
expr_evalop_uminus_integer(expr_value_t operands[])
{
    expr_value_t res;

    res.type = EXPR_VALUE_TYPE_INTEGER;
    res.integer = -operands[0].integer;
    return res;
}

static struct expr_evalop_match_item match_uminus_integer = {
    .op_type = AST_NODE_TYPE_REXPR_OP_UMINUS,
    .n_opd = 1,
    .opd_types = {
        EXPR_VALUE_TYPE_INTEGER
    },
    .evaluator = {
        .res_type_mask = EXPR_VALUE_TYPE_INTEGER,
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
                // there's a match if all possible operand types are
                // supported by the evaluator
                if ((opd_types[opd_i] & match->opd_types[opd_i])
                    != opd_types[opd_i]) {
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
expr_eval_builtin_env(
    struct ast_node_hdl *object,
    struct statement_list *params,
    int n_params,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep,
    expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    struct ast_node_hdl *expr;
    bitpunch_status_t bt_ret;
    expr_value_t name_eval;
    char name_buf[256];
    struct ast_node_hdl *board_node;
    struct ast_node_hdl *board_value_node = NULL;
    struct scope_def *scope_def;

    expr = ((struct named_expr *)TAILQ_FIRST(params))->expr;
    if (expr->ndat->u.rexpr.value_type_mask != EXPR_VALUE_TYPE_STRING) {
        return node_error(BITPUNCH_INVALID_PARAM, expr, bst,
                          "cannot evaluate 'env' with parameter of type '%s': "
                          "must be a string",
                          expr_value_type_str(
                              expr->ndat->u.rexpr.value_type_mask));
    }
    bt_ret = expr_evaluate_value_internal(expr, NULL, &name_eval, bst);
    if (BITPUNCH_OK == bt_ret) {
        assert(EXPR_VALUE_TYPE_STRING == name_eval.type);
        if (name_eval.string.len < sizeof (name_buf)) {
            memcpy(name_buf, name_eval.string.str, name_eval.string.len);
            name_buf[name_eval.string.len] = '\0';
            if (NULL != bst->board) {
                board_node = bst->board->ast_root;
                assert(AST_NODE_TYPE_REXPR_FILTER == board_node->ndat->type);
                scope_def = filter_get_scope_def(board_node);
                assert(NULL != scope_def);
                board_value_node = scope_get_first_declared_named_expr(
                    scope_def, name_buf);
            } else {
                board_value_node = NULL;
            }
            if (NULL == board_value_node) {
                bt_ret = node_error(
                    BITPUNCH_NO_ITEM, expr, bst,
                    "cannot evaluate 'env': no value '%s' present "
                    "in environment",
                    name_buf);
            }
        } else {
            bt_ret = node_error(
                BITPUNCH_INVALID_PARAM, expr, bst,
                "cannot evaluate 'env': name too long '%.20s' (max %zu chars)",
                name_eval.string.str, sizeof (name_buf) - 1);
        }
        expr_value_destroy(name_eval);
    }
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = expr_evaluate_internal(board_value_node, NULL, flags,
                                        valuep, dpathp, bst);
    }
    return bt_ret;
}

static bitpunch_status_t
expr_eval_builtin_index(
    struct ast_node_hdl *object,
    struct statement_list *params,
    int n_params,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep,
    expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    struct ast_node_hdl *array_dpath;
    struct ast_node_hdl *item_dpath;
    bitpunch_status_t bt_ret;
    expr_dpath_t array_dpath_eval;
    expr_dpath_t item_dpath_eval;
    struct ast_node_hdl *array_item;
    expr_dpath_t array_ancestor, item_ancestor;
    int ancestor_is_array;
    expr_dpath_t array_item_dpath;
    struct track_path item_track;
    struct track_path cur_track;

    array_dpath = ((struct named_expr *)TAILQ_FIRST(params))->expr;
    item_dpath = ((struct named_expr *)
                  TAILQ_NEXT(TAILQ_FIRST(params), list))->expr;
    if (array_dpath->ndat->u.rexpr.dpath_type_mask == EXPR_DPATH_TYPE_NONE) {
        return node_error(BITPUNCH_INVALID_PARAM, array_dpath, bst,
                          "cannot evaluate 'index': 1st argument is a "
                          "value-type expression");
    }
    if (item_dpath->ndat->u.rexpr.dpath_type_mask == EXPR_DPATH_TYPE_NONE) {
        return node_error(BITPUNCH_INVALID_PARAM, item_dpath, bst,
                          "cannot evaluate 'index': 2nd argument is a "
                          "value-type expression");
    }
    bt_ret = expr_evaluate_dpath_internal(array_dpath, NULL,
                                          &array_dpath_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_dpath_evaluate_filter_internal(
        array_dpath_eval, &array_item, bst);
    if (BITPUNCH_OK != bt_ret) {
        expr_dpath_destroy(array_dpath_eval);
        return bt_ret;
    }
    if (AST_NODE_TYPE_ARRAY != array_item->ndat->type) {
        expr_dpath_destroy(array_dpath_eval);
        return node_error(BITPUNCH_INVALID_PARAM, array_dpath, bst,
                          "cannot evaluate 'index': 1st argument is not "
                          "an array");
    }
    bt_ret = expr_evaluate_dpath_internal(item_dpath, NULL,
                                          &item_dpath_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        expr_dpath_destroy(array_dpath_eval);
        return bt_ret;
    }
    expr_dpath_find_common_ancestor(array_dpath_eval, item_dpath_eval,
                                    &array_ancestor, &item_ancestor);

    ancestor_is_array = expr_dpath_is(array_ancestor, array_dpath_eval);

    if (!ancestor_is_array
        || expr_dpath_is(item_dpath_eval, item_ancestor)) {
        expr_dpath_destroy(array_dpath_eval);
        expr_dpath_destroy(item_dpath_eval);
        return node_error(BITPUNCH_INVALID_PARAM, array_dpath, bst,
                          "cannot evaluate 'index': 2nd argument is not a "
                          "descendent of array");
    }
    array_item_dpath = item_dpath_eval;
    item_track.type = TRACK_PATH_NOTYPE;
    do {
        cur_track = expr_dpath_get_track_path(array_item_dpath);
        if (TRACK_PATH_ARRAY == cur_track.type) {
            item_track = cur_track;
        }
        array_item_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        array_item_dpath.box =
            expr_dpath_get_parent_box(array_item_dpath);
        assert(NULL != array_item_dpath.box);
    }
    while (!expr_dpath_is(array_item_dpath, item_ancestor));
    assert(TRACK_PATH_ARRAY == item_track.type);
    if (NULL != valuep) {
        *valuep = expr_value_as_integer(item_track.u.array.index);
    }
    if (NULL != dpathp) {
        *dpathp = expr_dpath_none();
    }
    expr_dpath_destroy(array_dpath_eval);
    expr_dpath_destroy(item_dpath_eval);
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_eval_builtin_len(
    struct ast_node_hdl *object,
    struct statement_list *params,
    int n_params,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep,
    expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    struct ast_node_hdl *expr;
    int64_t item_count = 0;
    bitpunch_status_t bt_ret;
    expr_dpath_t dpath_eval;
    struct box *box = NULL;

    expr = ((struct named_expr *)TAILQ_FIRST(params))->expr;
    if (expr->ndat->u.rexpr.dpath_type_mask == EXPR_DPATH_TYPE_NONE) {
        return node_error(BITPUNCH_INVALID_PARAM, expr, bst,
                          "cannot evaluate 'len' on value-type expression");
    }
    bt_ret = expr_evaluate_dpath_internal(expr, NULL, &dpath_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_dpath_to_box_internal(dpath_eval, &box, bst);
    expr_dpath_destroy(dpath_eval);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = box_get_n_items_internal(box, &item_count, bst);
        box_delete(box);
    }
    if (BITPUNCH_OK != bt_ret) {
        //TODO log
        return bt_ret;
    }
    if (NULL != valuep) {
        *valuep = expr_value_as_integer(item_count);
    }
    if (NULL != dpathp) {
        *dpathp = expr_dpath_none();
    }
    return BITPUNCH_OK;
}

/* this array must be alphabetically ordered by builtin name */
static const struct expr_builtin_fn
expr_builtin_fns[] = {
    {
        .builtin_name = "env",
        .res_value_type_mask = EXPR_VALUE_TYPE_BYTES,
        .res_dpath_type_mask = EXPR_DPATH_TYPE_CONTAINER,
        .eval_fn = expr_eval_builtin_env,
        .min_n_params = 1,
        .max_n_params = 1,
    },
    {
        .builtin_name = "index",
        .res_value_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .res_dpath_type_mask = EXPR_DPATH_TYPE_NONE,
        .eval_fn = expr_eval_builtin_index,
        .min_n_params = 2,
        .max_n_params = 2,
    },
    {
        .builtin_name = "len",
        .res_value_type_mask = EXPR_VALUE_TYPE_INTEGER,
        .res_dpath_type_mask = EXPR_DPATH_TYPE_NONE,
        .eval_fn = expr_eval_builtin_len,
        .min_n_params = 1,
        .max_n_params = 1,
    },
};

const struct expr_builtin_fn *
expr_lookup_builtin_fn(const char *name,
                       const struct ast_node_hdl *object)
{
    int builtin_i;
    const struct expr_builtin_fn *builtin;

    for (builtin_i = 0;
         builtin_i < N_ELEM(expr_builtin_fns); ++builtin_i) {
        builtin = &expr_builtin_fns[builtin_i];
        if (0 == strcmp(builtin->builtin_name, name)) {
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
                        const struct ast_node_hdl *object)
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
        if (strncmp(builtin->builtin_name, prefix, prefix_len) == 0) {
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
                       const struct ast_node_hdl *object)
{
    int builtin_i;
    const struct expr_builtin_fn *builtin;

    assert(NULL != name);
    for (builtin_i = 0;
         builtin_i < N_ELEM(expr_builtin_fns); ++builtin_i) {
        builtin = &expr_builtin_fns[builtin_i];
        if (strcmp(builtin->builtin_name, name) > 0) {
            return builtin->builtin_name;
        }
    }
    return NULL; /* last builtin reached */
}

void
expr_dpath_destroy(expr_dpath_t dpath)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        tracker_delete(dpath.tk);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        box_delete(dpath.box);
        break ;
    default:
        break ;
    }
}

void
expr_value_destroy(expr_value_t value)
{
    switch (value.type) {
    case EXPR_VALUE_TYPE_BYTES:
        box_delete(value.bytes.from_box);
        break ;
    case EXPR_VALUE_TYPE_STRING:
        box_delete(value.string.from_box);
        break ;
    case EXPR_VALUE_TYPE_DATA:
    case EXPR_VALUE_TYPE_DATA_RANGE:
        bitpunch_data_source_release(value.data.ds);
        break ;
    default:
        break ;
    }
}

expr_dpath_t
expr_dpath_dup(expr_dpath_t src_dpath)
{
    expr_dpath_t res;

    res.type = src_dpath.type;
    switch (res.type) {
    case EXPR_DPATH_TYPE_ITEM:
        res.tk = tracker_dup(src_dpath.tk);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        res.box = src_dpath.box;
        box_acquire(res.box);
        break ;
    case EXPR_DPATH_TYPE_NONE:
        break ;
    default:
        assert(0);
    }
    return res;
}

/*
 * dpath
 */

bitpunch_status_t
expr_dpath_to_tracker_internal(expr_dpath_t dpath,
                               struct tracker **tkp, struct browse_state *bst)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        *tkp = tracker_dup(dpath.tk);
        return BITPUNCH_OK;
    case EXPR_DPATH_TYPE_CONTAINER:
        return track_box_contents_internal(dpath.box, tkp, bst);
    case EXPR_DPATH_TYPE_NONE:
        return BITPUNCH_NO_DATA;
    default:
        assert(0);
    }
}

bitpunch_status_t
expr_dpath_to_box_internal(expr_dpath_t dpath,
                           struct box **boxp,
                           struct browse_state *bst)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return tracker_get_filtered_item_box_internal(dpath.tk, boxp,
                                                      bst);
    case EXPR_DPATH_TYPE_CONTAINER:
        box_acquire(dpath.box);
        *boxp = dpath.box;
        return BITPUNCH_OK;
    case EXPR_DPATH_TYPE_NONE:
        return BITPUNCH_NO_DATA;
    default:
        assert(0);
    }
}

bitpunch_status_t
expr_dpath_to_box_direct(expr_dpath_t dpath,
                         struct box **boxp,
                         struct browse_state *bst)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return tracker_create_item_box_internal(dpath.tk, boxp, bst);
    case EXPR_DPATH_TYPE_CONTAINER:
        box_acquire(dpath.box);
        *boxp = dpath.box;
        return BITPUNCH_OK;
    case EXPR_DPATH_TYPE_NONE:
        return BITPUNCH_NO_DATA;
    default:
        assert(0);
    }
}

bitpunch_status_t
expr_dpath_to_container_internal(expr_dpath_t dpath,
                                 expr_dpath_t *dpathp,
                                 struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *box;

    bt_ret = expr_dpath_to_box_internal(dpath, &box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    dpathp->type = EXPR_DPATH_TYPE_CONTAINER;
    dpathp->box = box;
    return BITPUNCH_OK;
}

bitpunch_status_t
expr_dpath_to_item_internal(expr_dpath_t dpath,
                            expr_dpath_t *dpathp,
                            struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct tracker *tk;

    bt_ret = expr_dpath_to_tracker_internal(dpath, &tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    dpathp->type = EXPR_DPATH_TYPE_ITEM;
    dpathp->tk = tk;
    return BITPUNCH_OK;
}

bitpunch_status_t
expr_dpath_to_dpath_internal(expr_dpath_t src_dpath,
                             enum expr_dpath_type dst_type,
                             expr_dpath_t *dst_dpathp,
                             struct browse_state *bst)
{
    switch (dst_type) {
    case EXPR_DPATH_TYPE_ITEM:
        return expr_dpath_to_item_internal(src_dpath, dst_dpathp, bst);
    case EXPR_DPATH_TYPE_CONTAINER:
        return expr_dpath_to_container_internal(src_dpath, dst_dpathp, bst);
    default:
        assert(0);
    }
}

struct box *
expr_dpath_get_parent_box(expr_dpath_t dpath)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return dpath.tk->box;
    case EXPR_DPATH_TYPE_CONTAINER:
        return dpath.box->parent_box;
    default:
        assert(0);
    }
}

 bitpunch_status_t
expr_dpath_get_size_internal(expr_dpath_t dpath,
                             int64_t *dpath_sizep,
                             struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = tracker_get_item_size_internal(dpath.tk,
                                                dpath_sizep, bst);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        bt_ret = box_get_span_size(dpath.box, dpath_sizep, bst);
        break ;
    default:
        assert(0);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
expr_dpath_get_location_internal(expr_dpath_t dpath,
                                 int64_t *offsetp, int64_t *sizep,
                                 struct browse_state *bst)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return tracker_get_item_location_internal(dpath.tk,
                                                  offsetp, sizep, bst);
    case EXPR_DPATH_TYPE_CONTAINER:
        return box_get_location_internal(dpath.box,
                                         offsetp, sizep, bst);
    default:
        assert(0);
    }
}

bitpunch_status_t
expr_dpath_get_filtered_data_internal(
    expr_dpath_t dpath,
    struct bitpunch_data_source **dsp, int64_t *offsetp, int64_t *sizep,
    struct box **exported_data_boxp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    switch (dpath.type) {
    case EXPR_DPATH_TYPE_CONTAINER:
        bt_ret = box_get_filtered_data_internal(dpath.box,
                                                dsp, offsetp, sizep, bst);
        if (BITPUNCH_OK == bt_ret) {
            *exported_data_boxp = dpath.box;
            box_acquire(dpath.box);
        }
        return bt_ret;
    case EXPR_DPATH_TYPE_ITEM:
        return tracker_get_filtered_data_internal(dpath.tk,
                                                  dsp, offsetp, sizep,
                                                  exported_data_boxp, bst);
    default:
        assert(0);
    }
}

bitpunch_status_t
expr_dpath_evaluate_filter_internal(
    expr_dpath_t dpath,
    struct ast_node_hdl **filter_typep,
    struct browse_state *bst)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return expr_evaluate_filter_type_internal(
            dpath.tk->dpath.filter, dpath.tk->box,
            FILTER_KIND_FILTER, filter_typep, bst);

    case EXPR_DPATH_TYPE_CONTAINER:
        *filter_typep = dpath.box->filter;
        return BITPUNCH_OK;

    default:
        assert(0);
    }
}

int
expr_dpath_contains_indexed_items(expr_dpath_t dpath)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return ast_node_is_indexed(dpath.tk->dpath.item);
    case EXPR_DPATH_TYPE_CONTAINER:
        return box_contains_indexed_items(dpath.box);
    default:
        assert(0);
    }
}

const struct ast_node_hdl *
expr_dpath_get_as_type(expr_dpath_t dpath)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return dpath_node_get_as_type(&dpath.tk->dpath);
    case EXPR_DPATH_TYPE_CONTAINER:
        return ast_node_get_as_type(dpath.box->filter);
    default:
        assert(0);
    }
}

const struct ast_node_hdl *
expr_dpath_get_target_filter(expr_dpath_t dpath)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return dpath_node_get_target_filter(&dpath.tk->dpath);
    case EXPR_DPATH_TYPE_CONTAINER:
        return ast_node_get_target_filter(dpath.box->filter);
    default:
        assert(0);
    }
}

struct track_path
expr_dpath_get_track_path(expr_dpath_t dpath)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return dpath.tk->cur;
    case EXPR_DPATH_TYPE_CONTAINER:
        return dpath.box->track_path;
    default:
        assert(0);
    }
}

int
expr_dpath_is(expr_dpath_t dpath1, expr_dpath_t dpath2)
{
    if (dpath1.type != dpath2.type) {
        return FALSE;
    }
    switch (dpath1.type) {
    case EXPR_DPATH_TYPE_CONTAINER:
        return dpath1.box == dpath2.box;
    case EXPR_DPATH_TYPE_ITEM:
        return dpath1.tk == dpath2.tk;
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
expr_dpath_find_common_ancestor(expr_dpath_t dpath1,
                                expr_dpath_t dpath2,
                                expr_dpath_t *ancestor1_dpathp,
                                expr_dpath_t *ancestor2_dpathp)
{
    struct track_path path1, path2;
    struct box *pbox1, *pbox2;
    struct box *box1, *box2;
    struct box **ancestors1, **ancestors2;
    int path_eq;
    expr_dpath_t ancestor1_dpath, ancestor2_dpath;

    pbox1 = expr_dpath_get_parent_box(dpath1);
    pbox2 = expr_dpath_get_parent_box(dpath2);

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
        ancestor1_dpath = dpath1;
        if (NULL == pbox2) {
            ancestor2_dpath = dpath2;
        } else {
            ancestor2_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
            ancestor2_dpath.box = ancestors2[-1];
        }
        goto end;
    }
    if (NULL == pbox2) {
        ancestor1_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor1_dpath.box = ancestors1[-1];
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
        ancestor1_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor1_dpath.box = ancestors1[1];
        ancestor2_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor2_dpath.box = ancestors2[1];
        goto end;
    }
    if (*ancestors1 == pbox1) {
        path1 = expr_dpath_get_track_path(dpath1);
    } else {
        path1 = ancestors1[-1]->track_path;
    }
    if (*ancestors2 == pbox2) {
        path2 = expr_dpath_get_track_path(dpath2);
    } else {
        path2 = ancestors2[-1]->track_path;
    }
    path_eq = track_path_eq(path1, path2);
    if (!path_eq) {
        ancestor1_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor1_dpath.box = *ancestors1;
        ancestor2_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor2_dpath.box = *ancestors2;
        goto end;
    }
    if (*ancestors1 == pbox1) {
        ancestor1_dpath = dpath1;
    } else {
        ancestor1_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor1_dpath.box = ancestors1[-1];
    }
    if (*ancestors2 == pbox2) {
        ancestor2_dpath = dpath2;
    } else {
        ancestor2_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor2_dpath.box = ancestors2[-1];
    }

  end:
    if (NULL != ancestor1_dpathp) {
        *ancestor1_dpathp = ancestor1_dpath;
    }
    if (NULL != ancestor2_dpathp) {
        *ancestor2_dpathp = ancestor2_dpath;
    }
}

void
expr_value_attach_box(expr_value_t *value, struct box *box)
{
    switch (value->type) {
    case EXPR_VALUE_TYPE_BYTES:
        value->bytes.from_box = box;
        box_acquire(box);
        break ;
    case EXPR_VALUE_TYPE_STRING:
        value->string.from_box = box;
        box_acquire(box);
        break ;
    default:
        break ;
    }
}

int
expr_value_cmp_integer(expr_value_t value1, expr_value_t value2)
{
    return ((value1.integer > value2.integer)
            - (value1.integer < value2.integer));
}

int
expr_value_cmp_string(expr_value_t value1, expr_value_t value2)
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
expr_value_cmp_bytes(expr_value_t value1, expr_value_t value2)
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
expr_value_cmp(expr_value_t value1, expr_value_t value2)
{
    switch (value1.type) {
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

static bitpunch_status_t
expr_evaluate_dpath_anchor_common(struct ast_node_hdl *expr,
                                  expr_dpath_t *dpathp,
                                  struct browse_state *bst)
{
    struct ast_node_hdl *anchor_expr;
    struct ast_node_hdl *anchor_filter;
    expr_dpath_t anchor_dpath;
    bitpunch_status_t bt_ret;
    struct box *anchor_scope;

    anchor_expr = expr->ndat->u.rexpr_member_common.anchor_expr;
    anchor_filter = expr->ndat->u.rexpr_member_common.anchor_filter;
    if (NULL != anchor_expr) {
        bt_ret = expr_evaluate_dpath_internal(anchor_expr, NULL,
                                              &anchor_dpath, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        if (EXPR_DPATH_TYPE_NONE != anchor_dpath.type
            && !ast_node_is_rexpr_filter(
                expr_dpath_get_as_type(anchor_dpath))) {
            expr_dpath_destroy(anchor_dpath);
            return node_error(
                BITPUNCH_DATA_ERROR, anchor_expr, bst,
                "left-side of member operator does not evaluate to a "
                "filter type");
        }
        *dpathp = anchor_dpath;
        return BITPUNCH_OK;
    }
    /* find the closest dpath's field in the scope, browsing boxes
     * from inner to outer scope */
    anchor_scope = bst->scope;
    while (NULL != anchor_scope) {
        if (filter_exists_in_scope(anchor_scope->filter, anchor_filter)) {
            *dpathp = expr_dpath_as_container(anchor_scope);
            box_acquire(anchor_scope);
            return BITPUNCH_OK;
        }
        anchor_scope = (NULL != anchor_scope->parent_box ?
                        anchor_scope->parent_box :
                        anchor_scope->scope);
    }
    // no dpath associated to anchor (i.e. anchor is a data type) ->
    // to be double-checked
    dpathp->type = EXPR_DPATH_TYPE_NONE;
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_compile_named_expr_internal(
    struct ast_node_hdl *expr,
    struct browse_state *bst)
{
    const struct named_expr *named_expr;
    struct ast_node_hdl *target;
    struct ast_node_hdl *extern_item;
    int ret;
    struct filter_class *filter_cls;

    named_expr = expr->ndat->u.rexpr_named_expr.named_expr;
    target = named_expr->expr;
    if (AST_NODE_TYPE_EXTERN_NAME == target->ndat->type) {
        extern_item = bitpunch_board_get_external_item(
            bst->board, target->ndat->u.extern_name.name);
        if (NULL == extern_item) {
            return node_error(
                BITPUNCH_NOT_IMPLEMENTED, target, bst,
                "extern name '%s' not bound to an external reference",
                target->ndat->u.extern_name.name);
        }
        target = extern_item;
    }
    if (AST_NODE_TYPE_EXTERN_FILTER == target->ndat->type) {
        filter_cls = target->ndat->u.extern_filter.filter_cls;
        ret = filter_instance_build(
            expr, filter_cls, filter_def_create_empty(named_expr->nstmt.name));
        if (-1 == ret) {
            return node_error(
                BITPUNCH_ERROR, expr, bst,
                "error during external filter build");
        }
        ret = compile_rexpr_filter(expr, (COMPILE_TAG_NODE_SPAN_SIZE |
                                          COMPILE_TAG_BROWSE_BACKENDS), NULL);
        if (-1 == ret) {
            return node_error(
                BITPUNCH_ERROR, expr, bst,
                "error during external filter compile");
        }
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_named_expr_internal(
    struct ast_node_hdl *expr,
    const struct named_expr **named_exprp,
    struct box **member_scopep,
    struct browse_state *bst)
{
    const struct named_expr *named_expr;
    bitpunch_status_t bt_ret;
    expr_dpath_t anchor_dpath;
    int cond_eval;
    struct box *member_scope;

    named_expr = expr->ndat->u.rexpr_named_expr.named_expr;
    bt_ret = expr_evaluate_dpath_anchor_common(expr, &anchor_dpath, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (EXPR_DPATH_TYPE_NONE != anchor_dpath.type) {
        bt_ret = expr_dpath_to_box_internal(anchor_dpath, &member_scope, bst);
        expr_dpath_destroy(anchor_dpath);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    } else {
        member_scope = bst->scope;
        box_acquire(member_scope);
    }
    if (0 != (expr->flags & ASTFLAG_IS_ANONYMOUS_MEMBER)) {
        struct box *direct_scope;

        bt_ret = filter_lookup_statement_internal(
            member_scope->filter, member_scope, STATEMENT_TYPE_NAMED_EXPR,
            named_expr->nstmt.name, NULL, NULL, &direct_scope, bst);
        box_delete(member_scope);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        member_scope = direct_scope;
    }
    bt_ret = evaluate_conditional_internal(named_expr->nstmt.stmt.cond,
                                           member_scope, &cond_eval,
                                           bst);
    if (BITPUNCH_OK != bt_ret) {
        box_delete(member_scope);
        return bt_ret;
    }
    if (!cond_eval) {
        box_delete(member_scope);
        return node_error(BITPUNCH_DATA_ERROR, expr, bst,
                          "no member named '%s' is associated to block in "
                          "current evaluation context",
                          named_expr->nstmt.name);
    }
    *named_exprp = named_expr;
    *member_scopep = member_scope;
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_polymorphic_internal(
    struct ast_node_hdl *expr,
    enum statement_type *stmt_typep,
    const struct named_statement **nstmtp,
    struct box **member_scopep,
    struct browse_state *bst)
{
    struct ast_node_hdl *anchor_expr;
    enum statement_type lookup_mask;
    expr_dpath_t anchor_dpath;
    bitpunch_status_t bt_ret;
    struct box *anchor_scope;

    anchor_expr = expr->ndat->u.rexpr_member_common.anchor_expr;
    if (expr->ndat->u.rexpr_polymorphic.identifier[0] == '@') {
        lookup_mask = STATEMENT_TYPE_ATTRIBUTE;
    } else {
        lookup_mask = STATEMENT_TYPE_NAMED_EXPR | STATEMENT_TYPE_FIELD;
    }
    if (NULL != anchor_expr) {
        bt_ret = expr_evaluate_dpath_internal(anchor_expr, NULL,
                                              &anchor_dpath, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        if (EXPR_DPATH_TYPE_NONE == anchor_dpath.type) {
            return node_error(
                BITPUNCH_DATA_ERROR, expr, bst,
                "anchor of polymorphic member '%s' does not evaluate to a "
                "dpath type",
                expr->ndat->u.rexpr_polymorphic.identifier);
        }
        bt_ret = expr_dpath_to_box_internal(anchor_dpath, &anchor_scope, bst);
        expr_dpath_destroy(anchor_dpath);
        if (BITPUNCH_OK != bt_ret) {
            goto error;
        }
        bt_ret = filter_lookup_statement_internal(
            anchor_scope->filter, anchor_scope, lookup_mask,
            expr->ndat->u.rexpr_polymorphic.identifier,
            stmt_typep, nstmtp, member_scopep, bst);
        if (BITPUNCH_OK != bt_ret) {
            if (BITPUNCH_NO_ITEM == bt_ret) {
                bt_ret = box_error(
                    BITPUNCH_DATA_ERROR, anchor_scope, expr, bst,
                    "polymorphic member '%s' does not exist in "
                    "block in current evaluation context",
                    expr->ndat->u.rexpr_polymorphic.identifier);
            }
            box_delete(anchor_scope);
            goto error;
        }
        box_delete(anchor_scope);
        return BITPUNCH_OK;
    }
    /* look for attribute in turn from inner to outer scope */
    anchor_scope = bst->scope;
    while (NULL != anchor_scope) {
        bt_ret = filter_lookup_statement_internal(
            anchor_scope->filter, anchor_scope, lookup_mask,
            expr->ndat->u.rexpr_polymorphic.identifier,
            stmt_typep, nstmtp, member_scopep, bst);
        if (BITPUNCH_OK == bt_ret) {
            return BITPUNCH_OK;
        }
        if (BITPUNCH_NO_ITEM != bt_ret) {
            goto error;
        }
        anchor_scope = anchor_scope->scope;
    }
    bt_ret = box_error(
        BITPUNCH_DATA_ERROR, bst->scope, expr, bst,
        "polymorphic identifier '%s' does not exist in "
        "current evaluation context",
        expr->ndat->u.rexpr_polymorphic.identifier);

  error:
    bitpunch_error_add_node_context(
        expr, bst, "while evaluating polymorphic expression");
    return bt_ret;
}

static bitpunch_status_t
expr_evaluate_native(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    if (NULL != valuep) {
        *valuep = expr->ndat->u.rexpr_native.value;
    }
    if (NULL != dpathp) {
        *dpathp = expr_dpath_none();
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_binary_operator(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    expr_value_t operands[2];

    // TODO: when introducing dpath operators, will need to check here
    // if evaluating operands dpath is needed

    if (NULL != valuep) {
        bt_ret = expr_evaluate_value_internal(
            expr->ndat->u.rexpr_op.op.operands[0], NULL, &operands[0], bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = expr_evaluate_value_internal(
            expr->ndat->u.rexpr_op.op.operands[1], NULL, &operands[1], bst);
        if (BITPUNCH_OK != bt_ret) {
            expr_value_destroy(operands[0]);
            return bt_ret;
        }
        *valuep = expr->ndat->u.rexpr_op.evaluator->eval_fn(operands);
        expr_value_destroy(operands[0]);
        expr_value_destroy(operands[1]);
    }
    if (NULL != dpathp) {
        *dpathp = expr_dpath_none();
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_unary_operator(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    expr_value_t operands[1];
    bitpunch_status_t bt_ret;

    // TODO: when introducing dpath operators, will need to check here
    // if evaluating operand dpath is needed

    if (NULL != valuep) {
        bt_ret = expr_evaluate_value_internal(
            expr->ndat->u.rexpr_op.op.operands[0], NULL, &operands[0], bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        *valuep = expr->ndat->u.rexpr_op.evaluator->eval_fn(operands);
        expr_value_destroy(operands[0]);
    }
    if (NULL != dpathp) {
        *dpathp = expr_dpath_none();
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_sizeof_internal(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    int64_t *item_sizep, struct browse_state *bst)
{
    struct ast_node_hdl *opd;
    struct ast_node_hdl *filter;
    bitpunch_status_t bt_ret;
    expr_dpath_t dpath_eval;

    opd = expr->ndat->u.rexpr_op.op.operands[0];
    if (ast_node_is_rexpr(opd)) {
        if (AST_NODE_TYPE_REXPR_FIELD == opd->ndat->type) {
            bt_ret = expr_evaluate_filter_type_internal(
                opd->ndat->u.rexpr_field.field->filter, NULL,
                FILTER_KIND_ITEM,
                &filter, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
            assert(ast_node_is_filter(filter));
            if (0 == (filter->ndat->u.item.flags
                      & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
                *item_sizep = ast_node_get_min_span_size(filter);
                return BITPUNCH_OK;
            }
        }
        bt_ret = expr_evaluate_dpath_internal(opd, NULL, &dpath_eval, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = expr_dpath_get_size_internal(dpath_eval, item_sizep, bst);
        expr_dpath_destroy(dpath_eval);
        return bt_ret;
    }
    // static sized item
    assert(ast_node_is_item(opd));
    assert(0 == (opd->ndat->u.item.flags
                 & ITEMFLAG_IS_SPAN_SIZE_VARIABLE));
    *item_sizep = ast_node_get_min_span_size(opd);
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_sizeof(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    int64_t item_size;
    bitpunch_status_t bt_ret;

    if (NULL != valuep) {
        bt_ret = expr_evaluate_sizeof_internal(expr, flags, &item_size, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        *valuep = expr_value_as_integer(item_size);
    }
    if (NULL != dpathp) {
        *dpathp = expr_dpath_none();
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_addrof(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    struct ast_node_hdl *opd;
    bitpunch_status_t bt_ret;
    expr_dpath_t dpath_eval;
    int64_t item_offset;

    if (NULL != valuep) {
        opd = expr->ndat->u.rexpr_op.op.operands[0];
        bt_ret = expr_evaluate_dpath_internal(opd, NULL, &dpath_eval, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        switch (dpath_eval.type) {
        case EXPR_DPATH_TYPE_ITEM:
            bt_ret = tracker_get_item_offset_internal(dpath_eval.tk,
                                                      &item_offset, bst);
            break ;
        case EXPR_DPATH_TYPE_CONTAINER:
            bt_ret = box_apply_filter_internal(dpath_eval.box, bst);
            item_offset = dpath_eval.box->start_offset_span;
            break ;
        default:
            assert(0);
        }
        expr_dpath_destroy(dpath_eval);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        assert(-1 != item_offset);
        *valuep = expr_value_as_integer(item_offset);
    }
    if (NULL != dpathp) {
        *dpathp = expr_dpath_none();
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_fcall(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    const struct expr_builtin_fn *builtin;
    struct statement_list *params;
    int n_params;

    builtin = expr->ndat->u.rexpr_op_fcall.builtin;
    params = expr->ndat->u.rexpr_op_fcall.func_params;
    n_params = expr->ndat->u.rexpr_op_fcall.n_func_params;

    return builtin->eval_fn(NULL, params, n_params, flags,
                            valuep, dpathp, bst);
}

static bitpunch_status_t
expr_evaluate_named_expr(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *member_scope;
    const struct named_expr *named_expr;

    bt_ret = expr_compile_named_expr_internal(expr, bst);
    if (BITPUNCH_OK == bt_ret) {
        if (AST_NODE_TYPE_REXPR_NAMED_EXPR != expr->ndat->type) {
            return expr_evaluate_internal(
                expr, NULL, flags, valuep, dpathp, bst);
        }
        bt_ret = expr_evaluate_named_expr_internal(
            expr, &named_expr, &member_scope, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_evaluate_internal(named_expr->expr, member_scope, flags,
                                    valuep, dpathp, bst);
    box_delete(member_scope);
    return bt_ret;
}

static bitpunch_status_t
expr_evaluate_polymorphic(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    enum statement_type stmt_type;
    const struct named_statement *nstmt;
    struct box *member_scope;

    bt_ret = expr_evaluate_polymorphic_internal(
        expr, &stmt_type, &nstmt, &member_scope, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = evaluate_scoped_statement_internal(
        member_scope, stmt_type, nstmt, flags, valuep, dpathp, bst);
    box_delete(member_scope);
    return bt_ret;
}

static bitpunch_status_t
expr_evaluate_self(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    expr_dpath_t dpath;
    struct box *self_box;

    bt_ret = expr_evaluate_dpath_anchor_common(expr, &dpath, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (EXPR_DPATH_TYPE_NONE == dpath.type) {
        self_box = bst->scope;
        if (NULL == self_box) {
            // TODO error message
            return BITPUNCH_NO_ITEM;
        }
        box_acquire(self_box);
        bt_ret = BITPUNCH_OK;
        dpath = expr_dpath_as_container(self_box);
    }
    if (NULL != valuep) {
        bt_ret = dpath_read_value_internal(dpath, valuep, bst);
    }
    if (BITPUNCH_OK == bt_ret && NULL != dpathp) {
        *dpathp = dpath;
    } else {
        expr_dpath_destroy(dpath);
    }
    return bt_ret;
}

static bitpunch_status_t
expr_evaluate_field(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    expr_dpath_t anchor_dpath;
    struct box *anchor_scope;

    bt_ret = expr_evaluate_dpath_anchor_common(expr, &anchor_dpath, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (EXPR_DPATH_TYPE_NONE == anchor_dpath.type) {
        return BITPUNCH_NO_DATA;
    }
    bt_ret = expr_dpath_to_box_internal(anchor_dpath, &anchor_scope, bst);
    expr_dpath_destroy(anchor_dpath);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = evaluate_scoped_statement_internal(
        anchor_scope, STATEMENT_TYPE_FIELD,
        (struct named_statement *)expr->ndat->u.rexpr_field.field, flags,
        valuep, dpathp, bst);
    box_delete(anchor_scope);
    return bt_ret;
}

static bitpunch_status_t
expr_evaluate_subscript(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    struct ast_node_hdl *anchor_expr;
    expr_dpath_t anchor_eval;
    bitpunch_status_t bt_ret;
    struct tracker *tk;
    expr_dpath_t dpath;

    anchor_expr = expr->ndat->u.rexpr_op_subscript_common.anchor_expr;
    bt_ret = expr_evaluate_dpath_internal(anchor_expr, NULL,
                                          &anchor_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    switch (anchor_eval.type) {
    case EXPR_DPATH_TYPE_ITEM:
        tk = anchor_eval.tk;
        bt_ret = tracker_enter_item_internal(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(tk);
            return bt_ret;
        }
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        bt_ret = track_box_contents_internal(anchor_eval.box,
                                             &tk, bst);
        box_delete(anchor_eval.box);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        break ;
    default:
        assert(0);
    }
    bt_ret = tracker_goto_index_internal(
        tk, expr->ndat->u.rexpr_op_subscript.index, "subscript index",
        FALSE, FALSE, bst);
    if (BITPUNCH_OK != bt_ret) {
        //TODO log
        tracker_delete(tk);
        return bt_ret;
    }
    dpath = expr_dpath_as_item(tk);
    if (NULL != valuep) {
        bt_ret = dpath_read_value_internal(dpath, valuep, bst);
    }
    if (BITPUNCH_OK == bt_ret && NULL != dpathp) {
        *dpathp = dpath;
    } else {
        expr_dpath_destroy(dpath);
    }
    return bt_ret;
}


static bitpunch_status_t
expr_evaluate_subscript_slice(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    struct ast_node_hdl *anchor_expr;
    expr_dpath_t anchor_eval;
    bitpunch_status_t bt_ret;
    struct tracker *tk_slice_start = NULL;
    struct tracker *tk_slice_end = NULL;
    struct box *slice_box;
    expr_dpath_t dpath;

    anchor_expr = expr->ndat->u.rexpr_op_subscript_common.anchor_expr;
    bt_ret = expr_evaluate_dpath_internal(anchor_expr, NULL,
                                          &anchor_eval, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    switch (anchor_eval.type) {
    case EXPR_DPATH_TYPE_ITEM:
        tk_slice_start = anchor_eval.tk;
        bt_ret = tracker_enter_item_internal(tk_slice_start, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(tk_slice_start);
            return bt_ret;
        }
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        bt_ret = track_box_contents_internal(anchor_eval.box,
                                             &tk_slice_start, bst);
        box_delete(anchor_eval.box);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        break ;
    default:
        assert(0);
    }

    bt_ret = tracker_goto_index_internal(
        tk_slice_start,
        expr->ndat->u.rexpr_op_subscript_slice.start, "start index of slice",
        TRUE, FALSE, bst);
    if (BITPUNCH_OK != bt_ret) {
        //TODO log
        goto end;
    }

    tk_slice_end = tracker_dup(tk_slice_start);
    bt_ret = tracker_goto_index_internal(
        tk_slice_end,
        expr->ndat->u.rexpr_op_subscript_slice.end, "end index of slice",
        TRUE, TRUE, bst);
    if (BITPUNCH_OK != bt_ret) {
        //TODO log
        goto end;
    }
    if (tk_slice_start->cur.u.array.index
        > tk_slice_end->cur.u.array.index) {
        bt_ret = node_error(
            BITPUNCH_DATA_ERROR,
            expr->ndat->u.rexpr_op_subscript_slice.start.key, bst,
            "start index of slice is greater than end index");
        goto end;
    }
    slice_box = box_new_slice_box(tk_slice_start, tk_slice_end, bst);
    if (NULL == slice_box) {
        //TODO log
        bt_ret = BITPUNCH_DATA_ERROR;
        goto end;
    }
    dpath = expr_dpath_as_container(slice_box);
    if (NULL != valuep) {
        bt_ret = dpath_read_value_internal(dpath, valuep, bst);
    }
    if (BITPUNCH_OK == bt_ret && NULL != dpathp) {
        *dpathp = dpath;
    } else {
        expr_dpath_destroy(dpath);
    }

  end:
    tracker_delete(tk_slice_start);
    tracker_delete(tk_slice_end);
    return bt_ret;
}

static bitpunch_status_t
expr_evaluate_ancestor(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    struct ast_node_hdl *opd;
    bitpunch_status_t bt_ret;
    expr_dpath_t opd_dpath;
    expr_dpath_t dpath;
    struct tracker *tk = NULL;
    struct box *box = NULL;
    struct box *ancestor_box;

    opd = expr->ndat->u.rexpr_op.op.operands[0];
    if (opd->ndat->u.rexpr.dpath_type_mask == EXPR_DPATH_TYPE_NONE) {
        return node_error(BITPUNCH_INVALID_PARAM, expr, bst,
                          "cannot evaluate ancestor operator on value-type "
                          "expression");
    }
    bt_ret = expr_evaluate_dpath_internal(opd, NULL, &opd_dpath, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (EXPR_DPATH_TYPE_ITEM == opd_dpath.type) {
        tk = opd_dpath.tk;
        bt_ret = tracker_get_filtered_item_box_internal(tk, &box, bst);
        tracker_delete(tk);
        tk = NULL;
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    } else {
        box = opd_dpath.box;
    }
    if (0 != (box->flags & BOX_FILTER) && NULL != box->parent_box) {
        ancestor_box = box->parent_box;
        assert(NULL != ancestor_box);
        box_acquire(ancestor_box);
        box_delete(box);
    } else {
        ancestor_box = box;
    }
    dpath = expr_dpath_as_container(ancestor_box);
    if (NULL != valuep) {
        bt_ret = dpath_read_value_internal(dpath, valuep, bst);
    }
    if (BITPUNCH_OK == bt_ret && NULL != dpathp) {
        *dpathp = dpath;
    } else {
        expr_dpath_destroy(dpath);
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_evaluate_extern_name(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    struct ast_node_hdl *extern_item;

    extern_item = bitpunch_board_get_external_item(
        bst->board, expr->ndat->u.extern_name.name);
    if (NULL == extern_item) {
        return BITPUNCH_NO_ITEM;
    }
    return expr_evaluate_internal(extern_item, NULL, flags,
                                  valuep, dpathp, bst);
}

static bitpunch_status_t
expr_evaluate_extern_func(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    struct extern_func *extern_func;

    extern_func = &expr->ndat->u.rexpr_extern_func.extern_func;
    return extern_func->extern_func_fn(extern_func->user_arg,
                                       valuep, dpathp, bst);
}

static bitpunch_status_t
expr_evaluate_filter_chain(
    struct ast_node_hdl *expr,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    struct dpath_transform transform;
    bitpunch_status_t bt_ret;

    transform.dpath.type = EXPR_DPATH_TYPE_NONE;
    transform.dpath_is_data_source = FALSE;
    if (expr->ndat->u.rexpr.dpath_type_mask != EXPR_DPATH_TYPE_UNSET &&
        expr->ndat->u.rexpr.dpath_type_mask != EXPR_DPATH_TYPE_NONE) {
        bt_ret = expr_transform_dpath_internal(expr, NULL, &transform, bst);
    } else {
        bt_ret = BITPUNCH_OK;
    }
    if (BITPUNCH_OK == bt_ret && NULL != valuep) {
        if (0 == (flags & EXPR_EVALFLAG_DPATH_XOR_VALUE)) {
            bt_ret = dpath_read_value_internal(transform.dpath, valuep, bst);
        } else {
            if (EXPR_DPATH_TYPE_NONE == transform.dpath.type) {
                return BITPUNCH_NO_DATA;
            }
            *valuep = expr_value_unset();
        }
    }
    if (BITPUNCH_OK == bt_ret && NULL != dpathp) {
        *dpathp = transform.dpath;
    } else {
        expr_dpath_destroy(transform.dpath);
    }
    return bt_ret;
}

bitpunch_status_t
expr_evaluate_internal(
    struct ast_node_hdl *expr, struct box *scope,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    struct box *scope_storage;
    bitpunch_status_t bt_ret;

    browse_state_push_scope(bst, scope, &scope_storage);

    switch (expr->ndat->type) {
    case AST_NODE_TYPE_REXPR_NATIVE:
        bt_ret = expr_evaluate_native(expr, flags, valuep, dpathp, bst);
        break ;
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
        bt_ret = expr_evaluate_binary_operator(expr, flags, valuep, dpathp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT:
        bt_ret = expr_evaluate_unary_operator(expr, flags, valuep, dpathp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_OP_SIZEOF:
        bt_ret = expr_evaluate_sizeof(expr, flags, valuep, dpathp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_OP_ADDROF:
        bt_ret = expr_evaluate_addrof(expr, flags, valuep, dpathp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_OP_FCALL:
        bt_ret = expr_evaluate_fcall(expr, flags, valuep, dpathp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        bt_ret = expr_evaluate_named_expr(expr, flags, valuep, dpathp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_POLYMORPHIC:
        bt_ret = expr_evaluate_polymorphic(expr, flags, valuep, dpathp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_SELF:
        bt_ret = expr_evaluate_self(expr, flags, valuep, dpathp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_FIELD:
        bt_ret = expr_evaluate_field(expr, flags, valuep, dpathp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
        bt_ret = expr_evaluate_subscript(expr, flags, valuep, dpathp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        bt_ret = expr_evaluate_subscript_slice(expr, flags, valuep, dpathp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_OP_ANCESTOR:
        bt_ret = expr_evaluate_ancestor(expr, flags, valuep, dpathp, bst);
        break ;
    case AST_NODE_TYPE_EXTERN_NAME:
        bt_ret = expr_evaluate_extern_name(expr, flags, valuep, dpathp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_EXTERN_FUNC:
        bt_ret = expr_evaluate_extern_func(expr, flags, valuep, dpathp, bst);
        break ;
    default:
        bt_ret = expr_evaluate_filter_chain(expr, flags, valuep, dpathp, bst);
        break ;
    }
    browse_state_pop_scope(bst, scope, &scope_storage);
    if (BITPUNCH_OK != bt_ret) {
        bitpunch_error_add_node_context(
            expr, bst,
            "when evaluating expression of type \"%s\"",
            ast_node_type_str(expr->ndat->type));
    }
    return bt_ret;
}

bitpunch_status_t
expr_evaluate_value_internal(struct ast_node_hdl *expr, struct box *scope,
                             expr_value_t *valuep,
                             struct browse_state *bst)
{
    return expr_evaluate_internal(expr, scope, 0u, valuep, NULL, bst);
}

bitpunch_status_t
expr_evaluate_dpath_internal(struct ast_node_hdl *expr, struct box *scope,
                             expr_dpath_t *dpathp,
                             struct browse_state *bst)
{
    return expr_evaluate_internal(expr, scope, 0u, NULL, dpathp, bst);
}

static bitpunch_status_t
expr_transform_dpath_generic_internal(
    struct ast_node_hdl *expr,
    struct dpath_transform *transformp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    expr_dpath_t filtered_dpath;

    if (EXPR_DPATH_TYPE_NONE != transformp->dpath.type) {
        expr_dpath_destroy(transformp->dpath);
        transformp->dpath.type = EXPR_DPATH_TYPE_NONE;
    }
    bt_ret = expr_evaluate_dpath_internal(expr, NULL,
                                          &transformp->dpath, bst);
    if (BITPUNCH_OK == bt_ret
        && EXPR_DPATH_TYPE_ITEM == transformp->dpath.type) {
        bt_ret = tracker_get_filtered_dpath_internal(
            transformp->dpath.tk, &filtered_dpath, bst);
        if (BITPUNCH_OK == bt_ret) {
            expr_dpath_destroy(transformp->dpath);
            transformp->dpath = filtered_dpath;
        }
    }
    return bt_ret;
}

static bitpunch_status_t
expr_transform_dpath_named_expr(
    struct ast_node_hdl *expr,
    struct dpath_transform *transformp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *member_scope;
    const struct named_expr *named_expr;

    bt_ret = expr_compile_named_expr_internal(expr, bst);
    if (BITPUNCH_OK == bt_ret) {
        if (AST_NODE_TYPE_REXPR_NAMED_EXPR != expr->ndat->type) {
            return expr_transform_dpath_internal(expr, NULL, transformp, bst);
        }
        bt_ret = expr_evaluate_named_expr_internal(
            expr, &named_expr, &member_scope, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_transform_dpath_internal(
        named_expr->expr, member_scope, transformp, bst);
    box_delete(member_scope);
    return bt_ret;
}

static bitpunch_status_t
expr_transform_dpath_polymorphic(
    struct ast_node_hdl *expr,
    struct dpath_transform *transformp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    enum statement_type stmt_type;
    const struct named_statement *nstmt;
    struct box *member_scope;

    bt_ret = expr_evaluate_polymorphic_internal(
        expr, &stmt_type, &nstmt, &member_scope, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    switch (stmt_type) {
    case STATEMENT_TYPE_FIELD: {
        const struct field *field;
        struct tracker *tk;

        if (EXPR_DPATH_TYPE_NONE != transformp->dpath.type) {
            box_delete(member_scope);
            return node_error(
                BITPUNCH_INVALID_PARAM, expr, bst,
                "cannot evaluate filtered dpath: multiple sources");
        }
        field = (struct field *)nstmt;
        bt_ret = track_box_contents_internal(member_scope, &tk, bst);
        box_delete(member_scope);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = tracker_goto_field_internal(tk, field, FALSE, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(tk);
            return bt_ret;
        }
        transformp->dpath.type = EXPR_DPATH_TYPE_ITEM;
        transformp->dpath.tk = tk;
        return BITPUNCH_OK;
    }
    case STATEMENT_TYPE_NAMED_EXPR: {
        const struct named_expr *named_expr;

        named_expr = (struct named_expr *)nstmt;
        bt_ret = expr_transform_dpath_internal(
            named_expr->expr, member_scope, transformp, bst);
        box_delete(member_scope);
        return bt_ret;
    }
    default:
        assert(0);
    }
}

static bitpunch_status_t
expr_transform_dpath_operator_filter(
    struct ast_node_hdl *expr,
    struct dpath_transform *transformp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct ast_node_hdl *target;
    struct ast_node_hdl *filter_expr;
    struct box *filter_scope;

    target = expr->ndat->u.rexpr_op_filter.target;
    filter_expr = expr->ndat->u.rexpr_op_filter.filter_expr;

    bt_ret = expr_transform_dpath_internal(target, NULL, transformp, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_dpath_to_box_internal(transformp->dpath, &filter_scope, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_transform_dpath_internal(filter_expr, filter_scope,
                                           transformp, bst);
    box_delete(filter_scope);
    return bt_ret;
}

static bitpunch_status_t
expr_transform_dpath_filter(
    struct ast_node_hdl *expr,
    struct dpath_transform *transformp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *parent_box;
    struct box *filtered_data_box;
    struct filter_instance *f_instance;

    switch (transformp->dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = tracker_create_item_box_internal(
            transformp->dpath.tk, &filtered_data_box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        break ;

    case EXPR_DPATH_TYPE_CONTAINER:
        parent_box = transformp->dpath.box;
        filtered_data_box = box_new_filter_box(parent_box, expr, bst);
        if (NULL == filtered_data_box) {
            return BITPUNCH_DATA_ERROR;
        }
        break ;

    case EXPR_DPATH_TYPE_NONE:
        f_instance = expr->ndat->u.rexpr_filter.f_instance;
        if (NULL == f_instance->b_item.get_data_source) {
            return BITPUNCH_NO_DATA;
        }
        filtered_data_box = box_new_filter_box(NULL, expr, bst);
        if (NULL == filtered_data_box) {
            return BITPUNCH_DATA_ERROR;
        }
        break ;

    default:
        assert(0);
    }
    expr_dpath_destroy(transformp->dpath);
    transformp->dpath.type = EXPR_DPATH_TYPE_CONTAINER;
    transformp->dpath.box = filtered_data_box;
    return BITPUNCH_OK;
}

static bitpunch_status_t
expr_transform_dpath_extern_name(
    struct ast_node_hdl *expr,
    struct dpath_transform *transformp,
    struct browse_state *bst)
{
    struct ast_node_hdl *extern_item;

    extern_item = bitpunch_board_get_external_item(
        bst->board, expr->ndat->u.extern_name.name);
    if (NULL == extern_item) {
        return node_error(BITPUNCH_NOT_IMPLEMENTED, expr, bst,
                          "extern name '%s' not bound to an external reference",
                          expr->ndat->u.extern_name.name);
    }
    return expr_transform_dpath_internal(extern_item, NULL, transformp, bst);
}

bitpunch_status_t
expr_transform_dpath_internal(
    struct ast_node_hdl *expr, struct box *scope,
    struct dpath_transform *transformp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *scope_storage;

    browse_state_push_scope(bst, scope, &scope_storage);

    switch (expr->ndat->type) {
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        bt_ret = expr_transform_dpath_named_expr(expr, transformp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_POLYMORPHIC:
        bt_ret = expr_transform_dpath_polymorphic(expr, transformp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        bt_ret = expr_transform_dpath_operator_filter(expr, transformp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_FILTER:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
        bt_ret = expr_transform_dpath_filter(expr, transformp, bst);
        break ;
    case AST_NODE_TYPE_EXTERN_NAME:
        bt_ret = expr_transform_dpath_extern_name(expr, transformp, bst);
        break ;
    case AST_NODE_TYPE_REXPR_SELF:
    case AST_NODE_TYPE_REXPR_FIELD:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
    case AST_NODE_TYPE_REXPR_OP_ANCESTOR:
    case AST_NODE_TYPE_REXPR_OP_FCALL:
        bt_ret = expr_transform_dpath_generic_internal(expr, transformp, bst);
        break ;
    default:
        bt_ret = BITPUNCH_OK;
        break ;
    }
    browse_state_pop_scope(bst, scope, &scope_storage);
    return bt_ret;
}

static bitpunch_status_t
expr_evaluate_filter_type_named_expr(
    struct ast_node_hdl *filter,
    enum filter_kind kind,
    struct ast_node_hdl **filter_typep,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *member_scope;
    const struct named_expr *named_expr;

    bt_ret = expr_compile_named_expr_internal(filter, bst);
    if (BITPUNCH_OK == bt_ret) {
        if (AST_NODE_TYPE_REXPR_NAMED_EXPR != filter->ndat->type) {
            return expr_evaluate_filter_type_internal(
                filter, NULL, kind, filter_typep, bst);
        }
        bt_ret = expr_evaluate_named_expr_internal(
            filter, &named_expr, &member_scope, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_evaluate_filter_type_internal(
        named_expr->expr, member_scope, kind, filter_typep, bst);
    box_delete(member_scope);
    return bt_ret;
}

static bitpunch_status_t
expr_evaluate_filter_type_op_filter(
    struct ast_node_hdl *filter,
    enum filter_kind kind,
    struct ast_node_hdl **filter_typep,
    struct browse_state *bst)
{
    switch (kind) {
    case FILTER_KIND_ITEM:
        return expr_evaluate_filter_type_internal(
            filter->ndat->u.rexpr_op_filter.target, NULL, kind,
            filter_typep, bst);
    case FILTER_KIND_FILTER:
        return expr_evaluate_filter_type_internal(
            filter->ndat->u.rexpr_op_filter.filter_expr, NULL, kind,
            filter_typep, bst);
    default:
        assert(0);
    }
}

bitpunch_status_t
expr_evaluate_filter_type_internal(
    struct ast_node_hdl *filter,
    struct box *scope,
    enum filter_kind kind,
    struct ast_node_hdl **filter_typep,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *scope_storage;

    browse_state_push_scope(bst, scope, &scope_storage);

    switch (filter->ndat->type) {
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        bt_ret = expr_evaluate_filter_type_named_expr(filter, kind,
                                                      filter_typep, bst);
        break ;
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        bt_ret = expr_evaluate_filter_type_op_filter(filter, kind,
                                                     filter_typep, bst);
        break ;
    case AST_NODE_TYPE_REXPR_FILTER:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
        *filter_typep = filter;
        bt_ret = BITPUNCH_OK;
        break ;
    default:
        bt_ret = node_error(
            BITPUNCH_INVALID_PARAM, filter, bst,
            "cannot evaluate filter type on expression of type '%s'",
            ast_node_type_str(filter->ndat->type));
    }
    browse_state_pop_scope(bst, scope, &scope_storage);
    return bt_ret;
}

bitpunch_status_t
evaluate_conditional_internal(struct ast_node_hdl *cond, struct box *scope,
                              int *evalp, struct browse_state *bst)
{
    int outer_cond_eval;
    expr_value_t cond_eval;
    bitpunch_status_t bt_ret;

    if (NULL == cond) {
        // no condition = always true
        *evalp = TRUE;
        return BITPUNCH_OK;
    }
    if (NULL != cond->ndat->u.conditional.outer_cond) {
        bt_ret = evaluate_conditional_internal(
            cond->ndat->u.conditional.outer_cond, scope, &outer_cond_eval, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        if (!outer_cond_eval) {
            *evalp = FALSE;
            return BITPUNCH_OK;
        }
    }
    bt_ret = expr_evaluate_value_internal(cond->ndat->u.conditional.cond_expr,
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

bitpunch_status_t
evaluate_scoped_statement_internal(
    struct box *scope,
    enum statement_type stmt_type, const struct named_statement *named_stmt,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    switch (stmt_type) {
    case STATEMENT_TYPE_NAMED_EXPR:
    case STATEMENT_TYPE_ATTRIBUTE:{
        const struct named_expr *named_expr;
        struct ast_node_hdl *expr;

        named_expr = (const struct named_expr *)named_stmt;
        expr = named_expr->expr;
        return expr_evaluate_internal(expr, scope, flags, valuep, dpathp, bst);
    }
    case STATEMENT_TYPE_FIELD: {
        struct tracker *tk;
        const struct field *field;
        struct box *scope_storage;

        field = (const struct field *)named_stmt;
        bt_ret = track_box_contents_internal(scope, &tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        browse_state_push_scope(bst, scope, &scope_storage);
        bt_ret = tracker_goto_field_internal(tk, field, FALSE, bst);
        if (BITPUNCH_OK == bt_ret && NULL != valuep) {
            if (0 == (flags & EXPR_EVALFLAG_DPATH_XOR_VALUE)) {
                bt_ret = tracker_read_item_value_internal(tk, valuep, bst);
            } else {
                *valuep = expr_value_unset();
            }
        }
        if (BITPUNCH_OK == bt_ret && NULL != dpathp) {
            *dpathp = expr_dpath_as_item(tk);
        } else {
            tracker_delete(tk);
        }
        browse_state_pop_scope(bst, scope, &scope_storage);
        return bt_ret;
    }
    default:
        assert(0);
    }
    /*NOT REACHED*/
}

bitpunch_status_t
dpath_read_value_internal(expr_dpath_t dpath,
                          expr_value_t *expr_valuep,
                          struct browse_state *bst)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return tracker_read_item_value_internal(dpath.tk,
                                                expr_valuep, bst);
    case EXPR_DPATH_TYPE_CONTAINER:
        return box_read_value_internal(dpath.box, expr_valuep, bst);
    case EXPR_DPATH_TYPE_NONE:
        return BITPUNCH_NO_DATA;
    default:
        assert(0);
    }
}

const char *
expr_value_type_str(enum expr_value_type type)
{
    switch (type) {
    case EXPR_VALUE_TYPE_UNSET:
        return "unset";
    case EXPR_VALUE_TYPE_INTEGER:
        return "integer";
    case EXPR_VALUE_TYPE_BOOLEAN:
        return "boolean";
    case EXPR_VALUE_TYPE_STRING:
        return "string";
    case EXPR_VALUE_TYPE_BYTES:
        return "bytes";
    case EXPR_VALUE_TYPE_ANY:
        return "any value-type";
    default:
        return "multiple value-type";
    }
}

const char *
expr_dpath_type_str(enum expr_dpath_type type)
{
    switch (type) {
    case EXPR_DPATH_TYPE_UNSET:
        return "unset";
    case EXPR_DPATH_TYPE_NONE:
        return "none";
    case EXPR_DPATH_TYPE_ITEM:
        return "item";
    case EXPR_DPATH_TYPE_CONTAINER:
        return "container";
    case EXPR_DPATH_TYPE_ANY:
        return "any dpath-type";
    default:
        return "multiple dpath-type";
    }
}

void
expr_value_to_hashable(expr_value_t value,
                       const char **bufp, int64_t *lenp)
{
    switch (value.type) {
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

int
expr_value_type_mask_contains_dpath(enum expr_value_type value_type_mask)
{
    return (0 != (value_type_mask & (EXPR_VALUE_TYPE_STRING |
                                     EXPR_VALUE_TYPE_BYTES |
                                     EXPR_VALUE_TYPE_DATA |
                                     EXPR_VALUE_TYPE_DATA_RANGE)));
}

/*
 * external API wrappers
 */

bitpunch_status_t
expr_evaluate(struct ast_node_hdl *expr,
              struct box *scope, struct bitpunch_board *board,
              enum expr_evaluate_flag flags,
              expr_value_t *valuep, expr_dpath_t *dpathp,
              struct bitpunch_error **errp)
{
    struct browse_state bst;
    bitpunch_status_t bt_ret;

    assert(NULL != valuep || NULL != dpathp);

    browse_state_init_scope(&bst, scope);
    bt_ret = browse_state_set_environment(&bst, board);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return transmit_error(
        expr_evaluate_internal(expr, scope, flags, valuep, dpathp, &bst),
        &bst, errp);
}

bitpunch_status_t
expr_evaluate_value(struct ast_node_hdl *expr,
                    struct box *scope, struct bitpunch_board *board,
                    expr_value_t *valuep,
                    struct bitpunch_error **errp)
{
    struct browse_state bst;
    bitpunch_status_t bt_ret;

    assert(NULL != valuep);

    browse_state_init_scope(&bst, scope);
    bt_ret = browse_state_set_environment(&bst, board);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return transmit_error(
        expr_evaluate_value_internal(expr, scope, valuep, &bst),
        &bst, errp);
}

bitpunch_status_t
expr_evaluate_dpath(struct ast_node_hdl *expr,
                    struct box *scope, struct bitpunch_board *board,
                    expr_dpath_t *dpathp,
                    struct bitpunch_error **errp)
{
    struct browse_state bst;
    bitpunch_status_t bt_ret;

    assert(NULL != dpathp);

    browse_state_init_scope(&bst, scope);
    bt_ret = browse_state_set_environment(&bst, board);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return transmit_error(
        expr_evaluate_dpath_internal(expr, scope, dpathp, &bst),
        &bst, errp);
}

bitpunch_status_t
evaluate_conditional(struct ast_node_hdl *cond,
                     struct box *scope, struct bitpunch_board *board,
                     int *evalp,
                     struct bitpunch_error **errp)
{
    struct browse_state bst;
    bitpunch_status_t bt_ret;

    browse_state_init_scope(&bst, scope);
    bt_ret = browse_state_set_environment(&bst, board);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return transmit_error(
        evaluate_conditional_internal(cond, scope, evalp, &bst),
        &bst, errp);
}
bitpunch_status_t
dpath_read_value(expr_dpath_t dpath,
                 expr_value_t *expr_valuep,
                 struct bitpunch_error **errp)
{
    struct browse_state bst;

    browse_state_init_dpath(&bst, dpath);
    return transmit_error(
        dpath_read_value_internal(dpath, expr_valuep, &bst),
        &bst, errp);
}
