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

%{

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdarg.h>
#include <stddef.h>

#include "core/parser.h"

  %}


%code requires {

#include <inttypes.h>
#include "api/bitpunch-structs.h"
#include "core/ast.h"
#include "core/expr.h"
#include "core/track-structs.h"

#define YYLTYPE struct parser_location
#define YYLLOC_DEFAULT(Cur, Rhs, N)                             \
    do {                                                        \
        if (N){                                                 \
            (Cur).parser_ctx   = YYRHSLOC(Rhs, 1).parser_ctx;   \
            (Cur).first_line   = YYRHSLOC(Rhs, 1).first_line;   \
            (Cur).first_column = YYRHSLOC(Rhs, 1).first_column; \
            (Cur).last_line    = YYRHSLOC(Rhs, N).last_line;    \
            (Cur).last_column  = YYRHSLOC(Rhs, N).last_column;  \
            (Cur).start_offset = YYRHSLOC(Rhs, 1).start_offset; \
            (Cur).end_offset   = YYRHSLOC(Rhs, N).end_offset;   \
        } else {                                                \
            (Cur).parser_ctx   = YYRHSLOC(Rhs, 0).parser_ctx;   \
            (Cur).first_line   = (Cur).last_line   =            \
                YYRHSLOC(Rhs, 0).last_line;                     \
            (Cur).first_column = (Cur).last_column =            \
                YYRHSLOC(Rhs, 0).last_column;                   \
            (Cur).start_offset = (Cur).end_offset =             \
                YYRHSLOC(Rhs, 0).end_offset;                    \
        }                                                       \
    } while (0)

#define AST_NODE_BYTE &shared_ast_node_byte
#define AST_NODE_ARRAY_SLICE &shared_ast_node_array_slice
#define AST_NODE_BYTE_SLICE &shared_ast_node_byte_slice
#define AST_NODE_AS_BYTES &shared_ast_node_as_bytes
#define AST_NODE_FILTERED &shared_ast_node_filtered

#include "utils/queue.h"

    typedef void *yyscan_t;

    struct parser_location {
        struct parser_ctx *parser_ctx;
        int parser_line_column;
        int first_line;
        int first_column;
        int last_line;
        int last_column;
        size_t start_offset;
        size_t end_offset;
    };

    struct named_type;
    struct ast_node;
    struct param;
    struct func_param;

    struct statement;

    STAILQ_HEAD(named_type_list, named_type);
    STAILQ_HEAD(ast_node_list, ast_node);
    STAILQ_HEAD(param_list, param);
    STAILQ_HEAD(func_param_list, func_param);

    TAILQ_HEAD(statement_list, statement);

    struct block_stmt_list {
        struct named_type_list *named_type_list;
        struct ast_node_list *block_def_list;

        struct statement_list *field_list;
        struct statement_list *link_list;
        struct statement_list *span_list;
        struct statement_list *key_list;
        struct statement_list *last_stmt_list;
        struct statement_list *match_list;
    };

    typedef int
        (*interpreter_read_func_t)(union expr_value *read_value,
                                   const char *data, size_t span_size,
                                   const struct ast_node *param_values);
    typedef int
        (*interpreter_write_func_t)(const union expr_value *write_value,
                                    char *data, size_t span_size,
                                    const struct ast_node *param_values);
    typedef int
        (*interpreter_get_size_func_t)(size_t *sizep,
                                       const char *data, size_t span_size,
                                       const struct ast_node *param_values);

    typedef union expr_value
        (*expr_evalop_fn_t)(union expr_value operands[]);

    struct expr_evaluator {
        enum expr_value_type res_type;
        expr_evalop_fn_t eval_fn;
    };

    const struct expr_evaluator *
        expr_lookup_evaluator(enum ast_node_type op_type,
                              enum expr_value_type opd_types[]);

    extern const int SPAN_SIZE_UNDEF;
    extern struct ast_node shared_ast_node_byte;
    extern struct ast_node shared_ast_node_array_slice;
    extern struct ast_node shared_ast_node_byte_slice;
    extern struct ast_node shared_ast_node_as_bytes;
    extern struct ast_node shared_ast_node_filtered;

    enum ast_node_flag {
        ASTFLAG_IS_SPAN_EXPR                = (1<<0),
        ASTFLAG_PROCESSING                  = (1<<1),
        ASTFLAG_IS_SPAN_SIZE_DYNAMIC        = (1<<2),
        ASTFLAG_IS_USED_SIZE_DYNAMIC        = (1<<3),
        ASTFLAG_NEED_SLACK                  = (1<<4),
        ASTFLAG_PROCESSED_TRACK_BACKEND     = (1<<5),
        ASTFLAG_IS_ROOT_BLOCK               = (1<<6),
        ASTFLAG_REVERSE_COND                = (1<<7),
        ASTFLAG_CONTAINS_LAST_STMT          = (1<<8),
        ASTFLAG_HAS_UNDETERMINED_SIZE       = (1<<9),
        ASTFLAG_HAS_FOOTER                  = (1<<10),
    };

    struct ast_node {
        /* when changing this enum, don't forget to update
         * ast_node_type_str() */
        enum ast_node_type {
            AST_NODE_TYPE_NONE = 0,
            AST_NODE_TYPE_INTEGER,
            AST_NODE_TYPE_BOOLEAN,
            AST_NODE_TYPE_STRING,
            AST_NODE_TYPE_IDENTIFIER,
            AST_NODE_TYPE_FILTER,
            AST_NODE_TYPE_TYPENAME,
            AST_NODE_TYPE_BLOCK_DEF,
            AST_NODE_TYPE_ARRAY,
            AST_NODE_TYPE_BYTE,
            AST_NODE_TYPE_BYTE_ARRAY,
            AST_NODE_TYPE_ARRAY_SLICE,
            AST_NODE_TYPE_BYTE_SLICE,
            AST_NODE_TYPE_AS_BYTES,
            AST_NODE_TYPE_FILTERED,
            AST_NODE_TYPE_CONDITIONAL,
            AST_NODE_TYPE_OP_EQ,
            AST_NODE_TYPE_OP_NE,
            AST_NODE_TYPE_OP_GT,
            AST_NODE_TYPE_OP_LT,
            AST_NODE_TYPE_OP_GE,
            AST_NODE_TYPE_OP_LE,
            AST_NODE_TYPE_OP_LOR,
            AST_NODE_TYPE_OP_LAND,
            AST_NODE_TYPE_OP_BWOR,
            AST_NODE_TYPE_OP_BWXOR,
            AST_NODE_TYPE_OP_BWAND,
            AST_NODE_TYPE_OP_LSHIFT,
            AST_NODE_TYPE_OP_RSHIFT,
            AST_NODE_TYPE_OP_ADD,
            AST_NODE_TYPE_OP_SUB,
            AST_NODE_TYPE_OP_MUL,
            AST_NODE_TYPE_OP_DIV,
            AST_NODE_TYPE_OP_MOD,
            AST_NODE_TYPE_OP_UPLUS,
            AST_NODE_TYPE_OP_UMINUS,
            AST_NODE_TYPE_OP_LNOT,
            AST_NODE_TYPE_OP_BWNOT,
            AST_NODE_TYPE_OP_SIZEOF,
            AST_NODE_TYPE_OP_ADDROF,
            AST_NODE_TYPE_OP_FILTER,
            AST_NODE_TYPE_OP_SUBSCRIPT,
            AST_NODE_TYPE_OP_SUBSCRIPT_SLICE,
            AST_NODE_TYPE_OP_MEMBER,
            AST_NODE_TYPE_OP_SET_FILTER,
            AST_NODE_TYPE_OP_FCALL,
            AST_NODE_TYPE_EXPR_FILE,
            AST_NODE_TYPE_EXPR_SELF,
            AST_NODE_TYPE_EXPR_STAR_WILDCARD,
            AST_NODE_TYPE_REXPR_NATIVE,
            AST_NODE_TYPE_REXPR_OP_EQ,
            AST_NODE_TYPE_REXPR_OP_NE,
            AST_NODE_TYPE_REXPR_OP_GT,
            AST_NODE_TYPE_REXPR_OP_LT,
            AST_NODE_TYPE_REXPR_OP_GE,
            AST_NODE_TYPE_REXPR_OP_LE,
            AST_NODE_TYPE_REXPR_OP_LOR,
            AST_NODE_TYPE_REXPR_OP_LAND,
            AST_NODE_TYPE_REXPR_OP_BWOR,
            AST_NODE_TYPE_REXPR_OP_BWXOR,
            AST_NODE_TYPE_REXPR_OP_BWAND,
            AST_NODE_TYPE_REXPR_OP_LSHIFT,
            AST_NODE_TYPE_REXPR_OP_RSHIFT,
            AST_NODE_TYPE_REXPR_OP_ADD,
            AST_NODE_TYPE_REXPR_OP_SUB,
            AST_NODE_TYPE_REXPR_OP_MUL,
            AST_NODE_TYPE_REXPR_OP_DIV,
            AST_NODE_TYPE_REXPR_OP_MOD,
            AST_NODE_TYPE_REXPR_OP_UPLUS,
            AST_NODE_TYPE_REXPR_OP_UMINUS,
            AST_NODE_TYPE_REXPR_OP_LNOT,
            AST_NODE_TYPE_REXPR_OP_BWNOT,
            AST_NODE_TYPE_REXPR_OP_SIZEOF,
            AST_NODE_TYPE_REXPR_OP_ADDROF,
            AST_NODE_TYPE_REXPR_OP_FILTER,
            AST_NODE_TYPE_REXPR_INTERPRETER,
            AST_NODE_TYPE_REXPR_AS_TYPE,
            AST_NODE_TYPE_REXPR_FIELD,
            AST_NODE_TYPE_REXPR_LINK,
            AST_NODE_TYPE_REXPR_BUILTIN,
            AST_NODE_TYPE_REXPR_OP_SUBSCRIPT,
            AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE,
            AST_NODE_TYPE_REXPR_OP_FCALL,
            AST_NODE_TYPE_REXPR_FILE,
            AST_NODE_TYPE_REXPR_SELF,
            AST_NODE_TYPE_REXPR_STAR_WILDCARD,
        } type;
        struct parser_location loc;
        enum ast_node_flag flags;
        STAILQ_ENTRY(ast_node) stmt_list;
        union {
            int64_t integer;
            int boolean;
            struct expr_value_string string;
            char *identifier;
            const char *operator;
            struct filter {
                struct ast_node *filter_type;
                struct param_list *param_list;
                struct ast_node *target;
            } filter;
            struct item_node {
                struct ast_node *filter;
                struct ast_node *filter_defining_size;
                struct item_backend b_item;
                int64_t min_span_size; /* minimum size */
            } item;
            struct container_node {
                struct item_node item; /* inherits */
                struct box_backend b_box;
                struct tracker_backend b_tk;
            } container;
            struct type_name {
                struct item_node item; /* inherits */
                const char *name;
            } type_name;
            struct block_def {
                struct container_node container; /* inherits */
                enum block_type {
                    BLOCK_TYPE_UNDEF,
                    BLOCK_TYPE_STRUCT,
                    BLOCK_TYPE_UNION,
                } type;
                struct block_stmt_list block_stmt_list;
            } block_def;
            struct array {
                struct container_node container; /* inherits */
                struct ast_node *value_type;
                struct ast_node *value_count;
            } array;
            struct byte_array {
                struct container_node container; /* inherits */
                struct ast_node *size;
            } byte_array;
            struct conditional {
                struct ast_node *cond_expr;
                struct ast_node *outer_cond;
            } conditional;
            struct op {
                struct ast_node *operands[2];
            } op;
            struct subscript_common {
                struct ast_node *anchor_expr;
            } op_subscript_common;
            struct subscript {
                struct subscript_common common; /* inherits */
                struct subscript_index {
                    struct ast_node *key;
                    struct ast_node *twin;
                } index;
            } op_subscript;
            struct subscript_slice {
                struct subscript_common common; /* inherits */
                struct subscript_index start;
                struct subscript_index end;
            } op_subscript_slice;
            struct fcall {
                struct ast_node *object;
                struct ast_node *func;
                struct func_param_list *func_params;
            } op_fcall;
            struct file_attr {
                struct ast_node *attr_name;
            } file_attr;
            struct rexpr {
                enum expr_value_type value_type;
                enum expr_dpath_type dpath_type;
                /** expression item type, or NULL if not applicable */
                struct ast_node *target_item;
            } rexpr; /* base, not instanciable */
            struct rexpr_filter {
                struct rexpr rexpr; /* inherits */
                struct ast_node *target;
                struct ast_node *filter_type;
            } rexpr_filter; /* base, not instanciable */
            struct rexpr_interpreter {
                struct rexpr_filter rexpr_filter; /* inherits */
                struct param_list *param_list;
                const struct interpreter *interpreter;
                interpreter_read_func_t read_func;
                interpreter_write_func_t write_func;
                interpreter_get_size_func_t get_size_func;
                /* cannot declare the following array but that's how
                 * it will be interpreted */
                /* struct ast_node params[]; */
            } rexpr_interpreter;
            struct rexpr_as_type {
                struct rexpr_filter rexpr_filter; /* inherits */
            } rexpr_as_type;
            struct rexpr_native {
                struct rexpr rexpr; /* inherits */
                union expr_value value;
            } rexpr_native;
            struct rexpr_member_common {
                struct rexpr rexpr; /* inherits */
                struct ast_node *anchor_expr;
                struct ast_node *anchor_block;
                struct ast_node *member;
            } rexpr_member_common;
            struct rexpr_field {
                /* inherits */
                struct rexpr_member_common rexpr_member_common;
                const struct field *field;
            } rexpr_field;
            struct rexpr_link {
                /* inherits */
                struct rexpr_member_common rexpr_member_common;
                const struct link *link;
            } rexpr_link;
            struct rexpr_builtin {
                struct rexpr rexpr; /* inherits */
                struct ast_node *anchor_expr;
                const struct expr_builtin_fn *builtin;
            } rexpr_builtin;
            struct rexpr_op {
                struct rexpr rexpr; /* inherits */
                struct op op;
                const struct expr_evaluator *evaluator;
            } rexpr_op;
            struct rexpr_op_subscript_common {
                struct rexpr rexpr; /* inherits */
                struct ast_node *anchor_expr;
            } rexpr_op_subscript_common;
            struct rexpr_op_subscript {
                struct rexpr_op_subscript_common common; /* inherits */
                struct subscript_index index;
            } rexpr_op_subscript;
            struct rexpr_op_subscript_slice {
                struct rexpr_op_subscript_common common; /* inherits */
                struct subscript_index start;
                struct subscript_index end;
            } rexpr_op_subscript_slice;
            struct rexpr_op_fcall {
                struct rexpr rexpr; /* inherits */
                const struct expr_builtin_fn *builtin;
                struct ast_node *object;
                int n_func_params;
                struct func_param_list *func_params;
            } rexpr_op_fcall;
            struct rexpr_file {
                struct rexpr rexpr; /* inherits */
            } rexpr_file;
            struct rexpr_self {
                struct rexpr rexpr; /* inherits */
            } rexpr_self;
        } u;
    };

    struct named_type {
        STAILQ_ENTRY(named_type) list;
        struct parser_location loc;
        char *name;
        struct ast_node *type;
        struct named_type *inner_named_type;
        struct ast_node *inner_block_def;
    };

    struct param {
        STAILQ_ENTRY(param) list;
        struct parser_location loc;
        char *name;
        struct ast_node *value;
    };

    struct statement {
        TAILQ_ENTRY(statement) list;
        struct parser_location loc;
        int stmt_flags; // type-specific flags
        struct ast_node *cond;
    };

    struct named_statement {
        struct statement stmt; // inherits
        struct named_statement *next_sibling;
        char *name;
    };

    enum field_flag {
        FIELD_FLAG_SLACK_TRAILER = (1<<0),
        FIELD_FLAG_HIDDEN        = (1<<1),
    };
    struct field {
        struct named_statement nstmt; // inherits
        struct ast_node *field_type;
    };

    struct match {
        struct statement stmt; // inherits
        struct ast_node *expr;
    };

    struct link {
        struct named_statement nstmt; // inherits
        struct ast_node *dst_expr;
    };

    enum span_stmt_flag {
        SPAN_FLAG_MIN = (1<<0),
        SPAN_FLAG_MAX = (1<<1),
    };
    struct span_stmt {
        struct statement stmt; // inherits
        struct ast_node *span_expr;
    };

    struct key_stmt {
        struct statement stmt; // inherits
        struct ast_node *key_expr;
    };

    struct last_stmt {
        struct statement stmt; // inherits
    };

    struct func_param {
        STAILQ_ENTRY(func_param) list;
        struct ast_node *expr;
    };

    typedef bitpunch_status_t
        (*expr_eval_value_builtin_fn_t)(struct ast_node *object,
                                        struct func_param_list *params,
                                        int n_params,
                                        struct box *scope,
                                        union expr_value *eval_valuep,
                                        struct browse_state *bst);

    typedef bitpunch_status_t
        (*expr_eval_dpath_builtin_fn_t)(struct ast_node *object,
                                        struct func_param_list *params,
                                        int n_params,
                                        struct box *scope,
                                        union expr_dpath *eval_dpathp,
                                        struct browse_state *bst);

    struct expr_builtin_fn {
        const char *builtin_name;
        enum expr_value_type res_value_type;
        expr_eval_value_builtin_fn_t eval_value_fn;
        enum expr_dpath_type res_dpath_type;
        expr_eval_dpath_builtin_fn_t eval_dpath_fn;
        enum ast_node_type member_of;
        int min_n_params;
        int max_n_params;
    };

    enum semantic_loglevel {
        SEMANTIC_LOGLEVEL_INFO,
        SEMANTIC_LOGLEVEL_WARNING,
        SEMANTIC_LOGLEVEL_ERROR,
    };

    void yyerror(YYLTYPE *loc, yyscan_t scanner,
                 struct parser_ctx *parser_ctx, void *out_param,
                 const char *str);

    size_t
        bitpunch_parser_print_location(const struct parser_location *loc,
                                       FILE *out);
    void
        parser_location_make_span(struct parser_location *dest_loc,
                                  const struct parser_location *start_loc,
                                  const struct parser_location *end_loc);
    const char *semantic_loglevel2str(enum semantic_loglevel lvl);
    void semantic_error(enum semantic_loglevel lvl,
                        const struct parser_location *loc,
                        const char *fmt, ...)
        __attribute__((format(printf,3,4)));
    const char *ast_node_type_str(enum ast_node_type type);
    const char *block_type_str(enum block_type type);
}

%define parse.error verbose
%define api.pure full
%param { yyscan_t scanner }
%param { struct parser_ctx *parser_ctx }
%param { void *out_param }

%code provides {
    int yylex(YYSTYPE *lvalp, YYLTYPE *llocp, yyscan_t yyscanner,
              struct parser_ctx *parser_ctx, void *out_param);
 }

%code {
    const int SPAN_SIZE_UNDEF = (int64_t)-1;
    struct ast_node shared_ast_node_byte = {
        .type = AST_NODE_TYPE_BYTE,
        .u = {
            .item = {
                .min_span_size = 1,
            }
        }
    };
    struct ast_node shared_ast_node_array_slice = {
        .type = AST_NODE_TYPE_ARRAY_SLICE,
    };
    struct ast_node shared_ast_node_byte_slice = {
        .type = AST_NODE_TYPE_BYTE_SLICE,
    };
    struct ast_node shared_ast_node_as_bytes = {
        .type = AST_NODE_TYPE_AS_BYTES,
    };
    struct ast_node shared_ast_node_filtered = {
        .type = AST_NODE_TYPE_FILTERED,
    };

    static struct ast_node *
    make_full_type(struct ast_node *base_type,
                   struct ast_node *array_chain,
                   struct ast_node *filter_chain)
    {
        struct ast_node *full_type;
        struct ast_node *array;

        full_type = base_type;
        if (NULL != array_chain) {
            for (array = array_chain;
                 NULL != array->u.array.value_type;
                 array = array->u.array.value_type)
                ;
            array->u.array.value_type = full_type;
            full_type = array_chain;
        }
        if (NULL != filter_chain) {
            struct ast_node *filter;

            for (filter = filter_chain; NULL != filter->u.filter.target;
                 filter = filter->u.filter.target) {
            }
            filter->u.filter.target = full_type;
            assert(ast_node_is_item(full_type));
            full_type->u.item.filter = filter_chain;
        }
        return full_type;
    }

    static struct ast_node *
    expr_gen_ast_node(enum ast_node_type op_type,
                      struct ast_node *opd1,
                      struct ast_node *opd2,
                      const struct parser_location *loc)
    {
        struct ast_node *node;

        node = new_safe(struct ast_node);
        node->type = op_type;
        node->loc = *loc;
        node->u.op.operands[0] = opd1;
        node->u.op.operands[1] = opd2;
        return node;
    }

    static void
    init_block_stmt_list(struct block_stmt_list *dst)
    {
        memset(dst, 0, sizeof (*dst));
        dst->named_type_list = new_safe(struct named_type_list);
        dst->block_def_list = new_safe(struct ast_node_list);
        dst->field_list = new_safe(struct statement_list);
        dst->link_list = new_safe(struct statement_list);
        dst->span_list = new_safe(struct statement_list);
        dst->key_list = new_safe(struct statement_list);
        dst->last_stmt_list = new_safe(struct statement_list);
        dst->match_list = new_safe(struct statement_list);
        STAILQ_INIT(dst->named_type_list);
        STAILQ_INIT(dst->block_def_list);
        TAILQ_INIT(dst->field_list);
        TAILQ_INIT(dst->link_list);
        TAILQ_INIT(dst->span_list);
        TAILQ_INIT(dst->key_list);
        TAILQ_INIT(dst->last_stmt_list);
        TAILQ_INIT(dst->match_list);
    }

    static int
    merge_block_stmt_list(struct block_stmt_list *dst,
                          struct block_stmt_list *src)
    {
        STAILQ_CONCAT(dst->named_type_list, src->named_type_list);
        STAILQ_CONCAT(dst->block_def_list, src->block_def_list);
        TAILQ_CONCAT(dst->field_list, src->field_list, list);
        TAILQ_CONCAT(dst->link_list, src->link_list, list);

        TAILQ_CONCAT(dst->span_list, src->span_list, list);
        TAILQ_CONCAT(dst->key_list, src->key_list, list);
        TAILQ_CONCAT(dst->last_stmt_list, src->last_stmt_list, list);
        return 0;
    }

    static void
    attach_outer_conditional(struct ast_node **inner_condp,
                             struct ast_node *outer_cond)
    {
        struct ast_node **condp;

        for (condp = inner_condp;
             NULL != *condp && *condp != outer_cond;
             condp = &(*condp)->u.conditional.outer_cond)
            ;
        if (NULL == *condp) {
            *condp = outer_cond;
        }
    }
 }

%union {
    int64_t integer;
    int boolean;
    char *ident;
    struct expr_value_string literal;
    struct ast_node *ast_node;
    struct named_type *named_type;
    struct param *param;
    struct param_list *param_list;
    struct field *field;
    struct block_stmt_list block_stmt_list;
    struct statement_list *statement_list;
    struct file_block file_block;
    struct match *match;
    struct link *link;
    struct span_stmt *span_stmt;
    struct key_stmt *key_stmt;
    struct func_param *func_param;
    struct func_param_list *func_param_list;
    struct subscript_index subscript_index;
    struct last_stmt *last_stmt;
}

%token TOK_ERROR
%token <ident> IDENTIFIER
%token <integer> INTEGER
%token <literal> LITERAL
%token <boolean> KW_TRUE KW_FALSE
%token KW_TYPE KW_STRUCT KW_UNION KW_FILE KW_MATCH KW_SPAN KW_MINSPAN KW_MAXSPAN KW_IF KW_ELSE KW_KEY KW_SELF KW_LAST

%token <ast_node_type> '|' '^' '&' '>' '<' '+' '-' '*' '/' '%' '!' '~' '.' ':'
%token <ast_node_type> TOK_LOR "||"
%token <ast_node_type> TOK_LAND "&&"
%token <ast_node_type> TOK_EQ "=="
%token <ast_node_type> TOK_NE "!="
%token <ast_node_type> TOK_GE ">="
%token <ast_node_type> TOK_LE "<="
%token <ast_node_type> TOK_LSHIFT "<<"
%token <ast_node_type> TOK_RSHIFT ">>"
%token <ast_node_type> TOK_LINK "=>"
%token <ast_node_type> TOK_RANGE ".."
%token <ast_node_type> OP_SIZEOF

%left  "||"
%left  "&&"
%left  '|'
%left  '^'
%left  '&'
%left  "==" "!="
%left  '>' '<' ">=" "<="
%left  "<<" ">>"
%left  '+' '-'
%left  '*' '/' '%'
%right OP_ARITH_UNARY_OP '!' '~' OP_SIZEOF
%left  ':'
%left  OP_SUBSCRIPT OP_FCALL '.'
%left  OP_BRACKETS

%type <ast_node> g_integer g_boolean g_identifier g_literal block param_value expr twin_index opt_twin_index adhoc_struct_def adhoc_union_def adhoc_block_def type type_name array_decl array_decl_list filter opt_filters
%type <file_block> file_block
%type <block_stmt_list> block_stmt_list if_block else_block opt_else_block
%type <named_type> type_decl type_def named_struct_def named_union_def named_block_def named_type_def
%type <param> param
%type <param_list> opt_param_list param_list param_nonempty_list
%type <field> field
%type <statement_list> field_list field_nonempty_list
%type <func_param> func_param
%type <func_param_list> func_params func_param_nonempty_list
%type <match> match_stmt
%type <link> link_stmt
%type <span_stmt> span_stmt
%type <key_stmt> key_stmt
%type <subscript_index> key_expr opt_key_expr
%type <last_stmt> last_stmt
%locations

%token START_DEF_FILE START_EXPR

%start start

%%

start:
    START_DEF_FILE schema
  | START_EXPR start_expr
    /* here go other parsers with shared grammar */

g_integer:
    INTEGER {
        $$ = new_safe(struct ast_node);
        $$->type = AST_NODE_TYPE_INTEGER;
        $$->loc = @$;
        $$->u.integer = $1;
    }
g_boolean:
    KW_TRUE {
        $$ = new_safe(struct ast_node);
        $$->type = AST_NODE_TYPE_BOOLEAN;
        $$->loc = @$;
        $$->u.boolean = 1;
    }
  | KW_FALSE {
        $$ = new_safe(struct ast_node);
        $$->type = AST_NODE_TYPE_BOOLEAN;
        $$->loc = @$;
        $$->u.boolean = 0;
    }
g_identifier:
    IDENTIFIER {
        $$ = new_safe(struct ast_node);
        $$->type = AST_NODE_TYPE_IDENTIFIER;
        $$->loc = @$;
        $$->u.identifier = $1;
    }
g_literal:
    LITERAL {
        $$ = new_safe(struct ast_node);
        $$->type = AST_NODE_TYPE_STRING;
        $$->loc = @$;
        $$->u.string.str = $1.str;
        $$->u.string.len = $1.len;
    }
  | g_literal LITERAL {
      int64_t new_len;

      // concatenate consecutive string literals
      $$ = $1;
      new_len = $$->u.string.len + $2.len;
      $$->u.string.str = realloc_safe((char *)$$->u.string.str, new_len);
      memcpy((char *)$$->u.string.str + $$->u.string.len, $2.str, $2.len);
      $$->u.string.len = new_len;
    }

start_expr: expr {
    memcpy(out_param, &$expr, sizeof($expr));
}

expr:
    g_integer
  | g_boolean
  | g_identifier
  | g_literal
  | KW_FILE {
        $$ = new_safe(struct ast_node);
        $$->type = AST_NODE_TYPE_EXPR_FILE;
        $$->loc = @KW_FILE;
    }
  | KW_SELF {
        $$ = new_safe(struct ast_node);
        $$->type = AST_NODE_TYPE_EXPR_SELF;
        $$->loc = @KW_SELF;
    }
  | '+' expr %prec OP_ARITH_UNARY_OP {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_UPLUS, $2, NULL, &@1);
    }
  | '-' expr %prec OP_ARITH_UNARY_OP {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_UMINUS, $2, NULL, &@1);
    }
  | '!' expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_LNOT, $2, NULL, &@1);
    }
  | '~' expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_BWNOT, $2, NULL, &@1);
    }
  | OP_SIZEOF expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_SIZEOF, $2, NULL, &@1);
    }
  | '&' expr %prec OP_ARITH_UNARY_OP {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_ADDROF, $2, NULL, &@1);
    }
  | '*' expr %prec OP_ARITH_UNARY_OP {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_FILTER, $2, NULL, &@1);
    }
  | expr "||" expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_LOR, $1, $3, &@2);
    }
  | expr "&&" expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_LAND, $1, $3, &@2);
    }
  | expr '|' expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_BWOR, $1, $3, &@2);
    }
  | expr '^' expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_BWXOR, $1, $3, &@2);
    }
  | expr '&' expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_BWAND, $1, $3, &@2);
    }
  | expr "==" expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_EQ, $1, $3, &@2);
    }
  | expr "!=" expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_NE, $1, $3, &@2);
    }
  | expr '>' expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_GT, $1, $3, &@2);
    }
  | expr '<' expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_LT, $1, $3, &@2);
    }
  | expr ">=" expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_GE, $1, $3, &@2);
    }
  | expr "<=" expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_LE, $1, $3, &@2);
    }
  | expr "<<" expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_LSHIFT, $1, $3, &@2);
    }
  | expr ">>" expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_RSHIFT, $1, $3, &@2);
    }
  | expr '+' expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_ADD, $1, $3, &@2);
    }
  | expr '-' expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_SUB, $1, $3, &@2);
    }
  | expr '*' expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_MUL, $1, $3, &@2);
    }
  | expr '/' expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_DIV, $1, $3, &@2);
    }
  | expr '%' expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_MOD, $1, $3, &@2);
    }
  | expr '.' g_identifier {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_MEMBER, $1, $3, &@2);
        parser_location_make_span(&$$->loc, &@1, &@3);
    }
  | expr ':' filter {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_SET_FILTER, $1, $3, &@2);
        $3->u.filter.target = $1;
    }
  | expr '[' key_expr ']' %prec OP_SUBSCRIPT {
        $$ = new_safe(struct ast_node);
        $$->type = AST_NODE_TYPE_OP_SUBSCRIPT;
        parser_location_make_span(&$$->loc, &@2, &@4);
        $$->u.op_subscript_common.anchor_expr = $1;
        $$->u.op_subscript.index = $3;
    }
  | expr '['
    opt_key_expr TOK_RANGE opt_key_expr ']' %prec OP_SUBSCRIPT {
        $$ = new_safe(struct ast_node);
        $$->type = AST_NODE_TYPE_OP_SUBSCRIPT_SLICE;
        parser_location_make_span(&$$->loc, &@2, &@6);
        $$->u.op_subscript_common.anchor_expr = $1;
        $$->u.op_subscript_slice.start = $3;
        $$->u.op_subscript_slice.end = $5;
    }
  | expr '(' func_params ')' %prec OP_FCALL {
        $$ = new_safe(struct ast_node);
        $$->type = AST_NODE_TYPE_OP_FCALL;
        $$->loc = $1->loc;
        $$->u.op_fcall.func = $1;
        $$->u.op_fcall.func_params = $func_params;
    }
  | '(' expr ')' {
        $$ = $2;
    }

key_expr:
    expr opt_twin_index {
      $$.key = $expr;
      $$.twin = $opt_twin_index;
    }

opt_key_expr:
    /* empty */ {
      $$.key = NULL;
      $$.twin = NULL;
    }
  | key_expr

opt_twin_index:
    /* empty */ {
        $$ = NULL;
    }
  | twin_index

twin_index:
    '{' expr '}' {
        $$ = $2;
    }
  | '{' '*' '}' {
        $$ = new_safe(struct ast_node);
        $$->type = AST_NODE_TYPE_EXPR_STAR_WILDCARD;
        $$->loc = @2;
    }

func_params:
    /* empty */ {
        $$ = new_safe(struct func_param_list);
        STAILQ_INIT($$);
    }
  | func_param_nonempty_list {
        $$ = $func_param_nonempty_list;
    }

func_param_nonempty_list:
    func_param {
        $$ = new_safe(struct func_param_list);
        STAILQ_INIT($$);
        STAILQ_INSERT_TAIL($$, $func_param, list);
    }
  | func_param_nonempty_list ',' func_param {
        $$ = $1;
        STAILQ_INSERT_TAIL($$, $func_param, list);
    }

func_param:
    expr {
        $$ = new_safe(struct func_param);
        $$->expr = $expr;
    }


schema:
    block_stmt_list file_block block_stmt_list {
        struct statement *stmt, *tstmt;
        struct field *field;

        if (-1 == merge_block_stmt_list(&$1, &$3)) {
            YYERROR;
        }
        TAILQ_FOREACH_SAFE(stmt, $1.field_list, list, tstmt) {
            field = (struct field *)stmt;
            semantic_error(SEMANTIC_LOGLEVEL_WARNING, &stmt->loc,
                           "top-level field declared outside file{} block");
            free(field->nstmt.name);
            free(field);
        }
        /* ignore fields outside file{} */
        TAILQ_INIT($1.field_list);
        if (-1 == merge_block_stmt_list(&$file_block.root
                                        ->u.block_def.block_stmt_list,
                                        &$1)) {
            YYERROR;
        }
        memcpy(out_param, &$file_block, sizeof($file_block));
    }
  | block_stmt_list {
      semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                     "missing top-level \"file {}\" block in "
                     "binary definition file");
      YYERROR;
  }

file_block: KW_FILE '{' block_stmt_list '}' {
        $$.root = new_safe(struct ast_node);
        $$.root->type = AST_NODE_TYPE_BLOCK_DEF;
        $$.root->loc = @$;
        $$.root->u.block_def.type = BLOCK_TYPE_STRUCT;
        if (TAILQ_EMPTY($block_stmt_list.field_list)) {
            semantic_error(SEMANTIC_LOGLEVEL_WARNING, &@$,
                           "file block has zero field");
        }
        if (!TAILQ_EMPTY($block_stmt_list.span_list)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR,
                &TAILQ_FIRST($block_stmt_list.span_list)->loc,
                "file block cannot have span information "
                "(it's always the whole file)");
            YYERROR;
        }
        if (!TAILQ_EMPTY($block_stmt_list.key_list)) {
            semantic_error(
                SEMANTIC_LOGLEVEL_ERROR,
                &TAILQ_FIRST($block_stmt_list.key_list)->loc,
                "file block cannot have key information");
            YYERROR;
        }
        $$.root->u.block_def.block_stmt_list = $block_stmt_list;
        $$.root->u.item.min_span_size = SPAN_SIZE_UNDEF;
        $$.root->flags = ASTFLAG_IS_ROOT_BLOCK;
    }

type_decl:
    KW_TYPE IDENTIFIER type_def ';' {
        $$ = $type_def;
        $$->loc = @IDENTIFIER;
        $$->name = $IDENTIFIER;
    }

type_def:
    type array_decl_list opt_filters {
        struct ast_node *full_type;

        full_type = make_full_type($type,
                                   $array_decl_list,
                                   $opt_filters);
        $$ = new_safe(struct named_type);
        $$->loc = @type;
        $$->name = NULL;
        $$->type = full_type;
        /* $type is a inner type so at end of chain thus
         * we can just point at it */
        if ($type->type == AST_NODE_TYPE_BLOCK_DEF) {
            $$->inner_block_def = $type;
        } else {
            $$->inner_block_def = NULL;
        }
        $$->inner_named_type = NULL;
    }
  | named_type_def array_decl_list opt_filters {
        struct ast_node *full_type;

        full_type = make_full_type($named_type_def->type,
                                   $array_decl_list,
                                   $opt_filters);
        $$ = new_safe(struct named_type);
        $$->loc = @named_type_def;
        $$->name = NULL;
        $$->type = full_type;
        /* $named_type_def is a inner type so at end of chain thus we
         * can just point at it */
        if ($named_type_def->type->type == AST_NODE_TYPE_BLOCK_DEF) {
            $$->inner_block_def = $named_type_def->type;
        } else {
            $$->inner_block_def = NULL;
        }
        $$->inner_named_type = $named_type_def;
    }

type:
    type_name
  | adhoc_block_def

type_name:
    g_identifier {
        char *identifier;

        identifier = $g_identifier->u.identifier;
        $$ = $g_identifier;
        $$->type = AST_NODE_TYPE_TYPENAME;
        memset(&$$->u.item, 0, sizeof($$->u.item));
        $$->u.type_name.name = identifier;
}

named_type_def:
    named_block_def

named_block_def:
    named_struct_def
  | named_union_def

adhoc_block_def:
    adhoc_struct_def
  | adhoc_union_def

named_struct_def:
    KW_STRUCT IDENTIFIER block {
        $$ = new_safe(struct named_type);
        $$->loc = @IDENTIFIER;
        $$->name = $IDENTIFIER;
        $$->type = dup_safe($block);
        $$->type->loc = @IDENTIFIER;
        $$->type->u.block_def.type = BLOCK_TYPE_STRUCT;
    }

adhoc_struct_def:
    KW_STRUCT block {
        $$ = dup_safe($block);
        $$->loc = @KW_STRUCT;
        $$->u.block_def.type = BLOCK_TYPE_STRUCT;
    }

named_union_def:
    KW_UNION IDENTIFIER block {
        $$ = new_safe(struct named_type);
        $$->loc = @IDENTIFIER;
        $$->name = $IDENTIFIER;
        $$->type = dup_safe($block);
        $$->type->loc = @IDENTIFIER;
        $$->type->u.block_def.type = BLOCK_TYPE_UNION;
    }

adhoc_union_def:
    KW_UNION block {
        $$ = dup_safe($block);
        $$->loc = @KW_UNION;
        $$->u.block_def.type = BLOCK_TYPE_UNION;
    }

opt_filters:
    /* empty */ {
        $$ = NULL;
    }
  | opt_filters[target] ':' filter {
        $$ = $filter;
        $$->u.filter.target = $target;
    }

filter:
    type_name array_decl_list opt_param_list {
        struct ast_node *full_type;

        full_type = make_full_type($type_name,
                                   $array_decl_list,
                                   NULL);
        $$ = new_safe(struct ast_node);
        $$->type = AST_NODE_TYPE_FILTER;
        $$->loc = @type_name;
        $$->u.filter.filter_type = full_type;
        $$->u.filter.param_list = $opt_param_list;
    }

opt_param_list:
    /* empty */ {
        $$ = new_safe(struct param_list);
        STAILQ_INIT($$);
    }
  | '(' param_list ')' {
        $$ = $param_list;
    }

param_list:
    /* empty */ {
        $$ = new_safe(struct param_list);
        STAILQ_INIT($$);
    }
  | param_nonempty_list

param_nonempty_list:
    param {
      $$ = new_safe(struct param_list);
      STAILQ_INIT($$);
      STAILQ_INSERT_TAIL($$, $param, list);
    }
  | param_nonempty_list ',' param {
        $$ = $1;
        STAILQ_INSERT_TAIL($$, $param, list);
    }

param:
    IDENTIFIER '=' param_value {
        $$ = new_safe(struct param);
        $$->loc = @IDENTIFIER;
        $$->name = $IDENTIFIER;
        $$->value = $param_value;
    }

param_value:
    g_integer
  | g_boolean
  | g_identifier
  | g_literal


block:
    '{' block_stmt_list '}' {
        $$ = new_safe(struct ast_node);
        $$->type = AST_NODE_TYPE_BLOCK_DEF;
        $$->loc = @$;
        $$->u.block_def.type = BLOCK_TYPE_UNDEF;
        $$->u.block_def.block_stmt_list = $block_stmt_list;
        $$->u.item.min_span_size = SPAN_SIZE_UNDEF;
    }

if_block:
    KW_IF '(' expr ')' '{' block_stmt_list '}' opt_else_block {
        struct ast_node *cond;
        struct statement *stmt;

        cond = new_safe(struct ast_node);
        cond->type = AST_NODE_TYPE_CONDITIONAL;
        cond->loc = @$;
        cond->u.conditional.cond_expr = $expr;

        TAILQ_FOREACH(stmt, $block_stmt_list.field_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        TAILQ_FOREACH(stmt, $block_stmt_list.link_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        TAILQ_FOREACH(stmt, $block_stmt_list.span_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        TAILQ_FOREACH(stmt, $block_stmt_list.key_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        TAILQ_FOREACH(stmt, $block_stmt_list.last_stmt_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        $$ = $block_stmt_list;

        cond = new_safe(struct ast_node);
        cond->type = AST_NODE_TYPE_CONDITIONAL;
        cond->loc = @$;
        cond->u.conditional.cond_expr = $expr;
        cond->flags |= ASTFLAG_REVERSE_COND;

        TAILQ_FOREACH(stmt, $opt_else_block.field_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        TAILQ_FOREACH(stmt, $opt_else_block.link_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        TAILQ_FOREACH(stmt, $opt_else_block.span_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        TAILQ_FOREACH(stmt, $opt_else_block.key_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        TAILQ_FOREACH(stmt, $opt_else_block.last_stmt_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        if (-1 == merge_block_stmt_list(&$$, &$opt_else_block)) {
            YYERROR;
        }
    }

opt_else_block:
    /* empty */ {
        init_block_stmt_list(&$$);
    }
  | else_block {
        $$ = $else_block;
    }

else_block:
    KW_ELSE '{' block_stmt_list '}' {
        $$ = $block_stmt_list;
    }
  | KW_ELSE if_block {
        $$ = $if_block;
    }

block_stmt_list:
    /* empty */ {
        init_block_stmt_list(&$$);
    }
  | block_stmt_list type_decl {
        $$ = $1;
        STAILQ_INSERT_TAIL($$.named_type_list, $type_decl, list);
        if (NULL != $type_decl->inner_named_type) {
            STAILQ_INSERT_TAIL($$.named_type_list,
                               $type_decl->inner_named_type, list);
        }
        if (NULL != $type_decl->inner_block_def) {
            STAILQ_INSERT_TAIL($$.block_def_list,
                               $type_decl->inner_block_def,
                               stmt_list);
        }
    }
  | block_stmt_list named_block_def array_decl_list
    field_list opt_filters ';' {
        struct ast_node *full_type;
        struct statement *stmt;
        struct field *field;

        $$ = $1;
        full_type = make_full_type($named_block_def->type,
                                   $array_decl_list,
                                   $opt_filters);
        STAILQ_INSERT_TAIL($$.block_def_list, $named_block_def->type,
                           stmt_list);
        STAILQ_INSERT_TAIL($$.named_type_list, $named_block_def, list);
        if (TAILQ_EMPTY($field_list)) {
            if (NULL != $array_decl_list) {
                semantic_error(
                    SEMANTIC_LOGLEVEL_WARNING,
                    &$array_decl_list->loc,
                    "useless array declaration: "
                    "does not belong to block type");
            }
            if (NULL != $opt_filters) {
                semantic_error(
                    SEMANTIC_LOGLEVEL_WARNING,
                    &$opt_filters->loc,
                    "useless filter declaration: "
                    "does not belong to block type");
            }
        } else {
            TAILQ_FOREACH(stmt, $field_list, list) {
                field = (struct field *)stmt;
                field->field_type = full_type;
            }
            TAILQ_CONCAT($$.field_list, $field_list, list);
        }
    }

  | block_stmt_list type array_decl_list
    field_list opt_filters ';' {
        struct ast_node *full_type;
        struct statement *stmt;
        struct field *field;

        $$ = $1;
        full_type = make_full_type($type,
                                   $array_decl_list,
                                   $opt_filters);
        if ($type->type == AST_NODE_TYPE_BLOCK_DEF) {
            STAILQ_INSERT_TAIL($$.block_def_list, $type, stmt_list);
        }
        if (!TAILQ_EMPTY($field_list)) {
            TAILQ_FOREACH(stmt, $field_list, list) {
                field = (struct field *)stmt;
                field->field_type = full_type;
            }
            TAILQ_CONCAT($$.field_list, $field_list, list);
        } else {
            // anonymous block
            if (AST_NODE_TYPE_BLOCK_DEF == $type->type
                && !TAILQ_EMPTY($type->u.block_def
                                .block_stmt_list.link_list)) {
                semantic_error(
                    SEMANTIC_LOGLEVEL_ERROR,
                    &TAILQ_FIRST($type->u.block_def
                                 .block_stmt_list.link_list)->loc,
                    "anonymous field cannot contain links");
                YYERROR;
            }
            field = new_safe(struct field);
            field->nstmt.stmt.loc = @$;
            field->field_type = full_type;
            TAILQ_INSERT_TAIL($$.field_list, (struct statement *)field,
                              list);
        }
    }

  | block_stmt_list match_stmt ';' {
        $$ = $1;
        TAILQ_INSERT_TAIL($$.match_list,
                          (struct statement *)$match_stmt, list);
    }

  | block_stmt_list span_stmt ';' {
        $$ = $1;
        TAILQ_INSERT_TAIL($$.span_list,
                          (struct statement *)$span_stmt, list);
    }
  | block_stmt_list key_stmt ';' {
        $$ = $1;
        if (!TAILQ_EMPTY($$.key_list)) {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR,
                           &$key_stmt->stmt.loc,
                           "multiple key statements not supported");
            semantic_error(SEMANTIC_LOGLEVEL_ERROR,
                           &TAILQ_FIRST($$.key_list)->loc,
                           "first key statement here");
            YYERROR;
        }
        TAILQ_INSERT_TAIL($$.key_list,
                          (struct statement *)$key_stmt, list);
    }
  | block_stmt_list link_stmt ';' {
        $$ = $1;
        TAILQ_INSERT_TAIL($$.link_list,
                          (struct statement *)$link_stmt, list);
    }

  | block_stmt_list last_stmt ';' {
        $$ = $1;
        TAILQ_INSERT_TAIL($$.last_stmt_list,
                          (struct statement *)$last_stmt, list);
    }

  | block_stmt_list if_block {
      /* join 'if' node children to block stmt lists */
      if (-1 == merge_block_stmt_list(&$$, &$if_block)) {
          YYERROR;
      }
  }

field_list:
    /* empty */ {
        $$ = new_safe(struct statement_list);
        TAILQ_INIT($$);
    }
  | field_nonempty_list

field_nonempty_list:
    field {
        $$ = new_safe(struct statement_list);
        TAILQ_INIT($$);
        TAILQ_INSERT_TAIL($$, (struct statement *)$field, list);
    }
  | field_nonempty_list ',' field {
        $$ = $1;
        TAILQ_INSERT_TAIL($$, (struct statement *)$field, list);
    }

field:
    IDENTIFIER {
        $$ = new_safe(struct field);
        $$->nstmt.name = $IDENTIFIER;
        $$->nstmt.stmt.loc = @$;
    }

array_decl_list:
    /* empty */ {
        $$ = NULL;
    }
  | array_decl array_decl_list[subarray] {
        $$ = new_safe(struct ast_node);
        $$->loc = @$;
        $$->type = AST_NODE_TYPE_ARRAY;
        $$->u.array.value_type = $subarray;
        $$->u.array.value_count = $array_decl;
        $$->u.item.min_span_size = SPAN_SIZE_UNDEF;
    }

array_decl:
    '[' expr ']' {
        $$ = $expr;
    }
  | '[' ']' {
        $$ = NULL;
    }

match_stmt:
    KW_MATCH expr {
        $$ = new_safe(struct match);
        $$->stmt.loc = @$;
        $$->expr = $expr;
    }

span_stmt:
    KW_SPAN expr {
        $$ = new_safe(struct span_stmt);
        $$->stmt.loc = @$;
        $$->span_expr = $expr;
        $$->span_expr->flags |= ASTFLAG_IS_SPAN_EXPR;
        $$->stmt.stmt_flags = SPAN_FLAG_MIN | SPAN_FLAG_MAX;
    }
  | KW_MINSPAN expr {
        $$ = new_safe(struct span_stmt);
        $$->stmt.loc = @$;
        $$->span_expr = $expr;
        $$->span_expr->flags |= ASTFLAG_IS_SPAN_EXPR;
        $$->stmt.stmt_flags = SPAN_FLAG_MIN;
    }
  | KW_MAXSPAN expr {
        $$ = new_safe(struct span_stmt);
        $$->stmt.loc = @$;
        $$->span_expr = $expr;
        $$->span_expr->flags |= ASTFLAG_IS_SPAN_EXPR;
        $$->stmt.stmt_flags = SPAN_FLAG_MAX;
    }

key_stmt:
    KW_KEY expr {
        $$ = new_safe(struct key_stmt);
        $$->stmt.loc = @$;
        $$->key_expr = $expr;
    }

link_stmt:
    IDENTIFIER TOK_LINK expr {
        if (*$IDENTIFIER != '?') {
            semantic_error(SEMANTIC_LOGLEVEL_ERROR, &@$,
                           "defining a link requires the member name to "
                           "start with '?' character.");
            YYERROR;
        }
        $$ = new_safe(struct link);
        $$->nstmt.stmt.loc = @$;
        $$->nstmt.name = $IDENTIFIER + 1;
        $$->dst_expr = $expr;
    }

last_stmt:
    KW_LAST {
        $$ = new_safe(struct last_stmt);
        $$->stmt.loc = @$;
    }

%%


void
parser_location_make_span(struct parser_location *dest_loc,
                          const struct parser_location *start_loc,
                          const struct parser_location *end_loc)
{
    dest_loc->parser_ctx   = start_loc->parser_ctx;
    dest_loc->first_line   = start_loc->first_line;
    dest_loc->first_column = start_loc->first_column;
    dest_loc->last_line    = end_loc->last_line;
    dest_loc->last_column  = end_loc->last_column;
    dest_loc->start_offset = start_loc->start_offset;
    dest_loc->end_offset   = end_loc->end_offset;
}

/**
 * @brief pretty-print parser location information @loc into buffer
 * @out_buf of size @out_buf_size
 *
 * @return the number of printed characters into @out_buf, not
 * including the final \0, or the number of characters that would have
 * been printed if @out_buf_size was big enough
 */
size_t
bitpunch_parser_print_location(const struct parser_location *loc,
                               FILE *out)
{
    const char *schema_end;
    const char *line_start;
    const char *line_end;

    if (NULL == loc->parser_ctx) {
        return 0;
    }
    line_start = (loc->parser_ctx->parser_data + loc->end_offset)
        - loc->last_column;
    schema_end =
        loc->parser_ctx->parser_data +
        loc->parser_ctx->parser_data_length;
    if (0 == loc->parser_ctx->parser_data_length) {
        line_end = loc->parser_ctx->parser_data;
    } else {
        line_end = memchr(line_start, '\n', schema_end - line_start);
        if (NULL == line_end)
            line_end = schema_end;
    }
    return fprintf(out, "%.*s\n%*s\n",
                   (int)(line_end - line_start), line_start,
                   (int)(loc->end_offset
                         - (line_start - loc->parser_ctx->parser_data)),
                   "^");
}

const char *semantic_loglevel2str(enum semantic_loglevel lvl)
{
    switch (lvl) {
#define MAP(LEVEL, STR) case SEMANTIC_LOGLEVEL_##LEVEL: return STR
        MAP(INFO, "");
        MAP(WARNING, "warning");
        MAP(ERROR, "error");
#undef MAP
    default:
        return "";
    }
}

void semantic_error(enum semantic_loglevel lvl,
                    const struct parser_location *loc,
                    const char *fmt, ...)
{
    va_list ap;

    if (NULL != loc) {
        if (NULL != loc->parser_ctx->parser_filepath) {
            fprintf(stderr, "%s in %s at line %d:\n",
                    semantic_loglevel2str(lvl),
                    loc->parser_ctx->parser_filepath, loc->last_line);
        } else {
            fprintf(stderr, "%s at line %d:\n",
                    semantic_loglevel2str(lvl), loc->last_line);
        }
        bitpunch_parser_print_location(loc, stderr);
    } else {
        fprintf(stderr, "%s: ", semantic_loglevel2str(lvl));
    }
    if (NULL != fmt) {
        va_start(ap, fmt);
        vfprintf(stderr, fmt, ap);
        fputs("\n", stderr);
        va_end(ap);
    }
}

const char *
ast_node_type_str(enum ast_node_type type)
{
    switch (type) {
    case AST_NODE_TYPE_NONE: return "none";
    case AST_NODE_TYPE_INTEGER: return "integer";
    case AST_NODE_TYPE_BOOLEAN: return "boolean";
    case AST_NODE_TYPE_STRING: return "string";
    case AST_NODE_TYPE_IDENTIFIER: return "identifier";
    case AST_NODE_TYPE_FILTER: return "filter";
    case AST_NODE_TYPE_TYPENAME: return "typename";
    case AST_NODE_TYPE_BLOCK_DEF: return "block def";
    case AST_NODE_TYPE_ARRAY: return "array";
    case AST_NODE_TYPE_BYTE: return "byte";
    case AST_NODE_TYPE_BYTE_ARRAY: return "byte array";
    case AST_NODE_TYPE_ARRAY_SLICE: return "slice";
    case AST_NODE_TYPE_BYTE_SLICE: return "byte slice";
    case AST_NODE_TYPE_AS_BYTES: return "as bytes";
    case AST_NODE_TYPE_FILTERED: return "filtered";
    case AST_NODE_TYPE_CONDITIONAL: return "conditional";
    case AST_NODE_TYPE_REXPR_NATIVE: return "native type";
    case AST_NODE_TYPE_OP_FCALL:
    case AST_NODE_TYPE_REXPR_OP_FCALL: return "function call";
    case AST_NODE_TYPE_EXPR_FILE:
    case AST_NODE_TYPE_REXPR_FILE: return "'file' expr";
    case AST_NODE_TYPE_EXPR_SELF:
    case AST_NODE_TYPE_REXPR_SELF: return "'self' expr";
    case AST_NODE_TYPE_EXPR_STAR_WILDCARD:
    case AST_NODE_TYPE_REXPR_STAR_WILDCARD: return "'*' wildcard expression";
    case AST_NODE_TYPE_OP_EQ:
    case AST_NODE_TYPE_REXPR_OP_EQ: return "operator '=='";
    case AST_NODE_TYPE_OP_NE:
    case AST_NODE_TYPE_REXPR_OP_NE: return "operator '!='";
    case AST_NODE_TYPE_OP_GT:
    case AST_NODE_TYPE_REXPR_OP_GT: return "operator '>'";
    case AST_NODE_TYPE_OP_LT:
    case AST_NODE_TYPE_REXPR_OP_LT: return "operator '<'";
    case AST_NODE_TYPE_OP_GE:
    case AST_NODE_TYPE_REXPR_OP_GE: return "operator '>='";
    case AST_NODE_TYPE_OP_LE:
    case AST_NODE_TYPE_REXPR_OP_LE: return "operator '<='";
    case AST_NODE_TYPE_OP_LOR:
    case AST_NODE_TYPE_REXPR_OP_LOR: return "logical 'or'";
    case AST_NODE_TYPE_OP_LAND:
    case AST_NODE_TYPE_REXPR_OP_LAND: return "logical 'and'";
    case AST_NODE_TYPE_OP_BWOR:
    case AST_NODE_TYPE_REXPR_OP_BWOR: return "bitwise 'or'";
    case AST_NODE_TYPE_OP_BWXOR:
    case AST_NODE_TYPE_REXPR_OP_BWXOR: return "bitwise 'xor'";
    case AST_NODE_TYPE_OP_BWAND:
    case AST_NODE_TYPE_REXPR_OP_BWAND: return "bitwise 'and'";
    case AST_NODE_TYPE_OP_LSHIFT:
    case AST_NODE_TYPE_REXPR_OP_LSHIFT: return "operator 'left-shift'";
    case AST_NODE_TYPE_OP_RSHIFT:
    case AST_NODE_TYPE_REXPR_OP_RSHIFT: return "operator 'right-shift'";
    case AST_NODE_TYPE_OP_ADD:
    case AST_NODE_TYPE_REXPR_OP_ADD: return "operator 'add'";
    case AST_NODE_TYPE_OP_SUB:
    case AST_NODE_TYPE_REXPR_OP_SUB: return "operator 'substract'";
    case AST_NODE_TYPE_OP_MUL:
    case AST_NODE_TYPE_REXPR_OP_MUL: return "operator 'multiply'";
    case AST_NODE_TYPE_OP_DIV:
    case AST_NODE_TYPE_REXPR_OP_DIV: return "operator 'divide'";
    case AST_NODE_TYPE_OP_MOD:
    case AST_NODE_TYPE_REXPR_OP_MOD: return "operator 'modulo'";
    case AST_NODE_TYPE_OP_SET_FILTER: return "operator 'set filter'";
    case AST_NODE_TYPE_REXPR_INTERPRETER: return "operator 'interpreter'";
    case AST_NODE_TYPE_REXPR_AS_TYPE: return "as type";
    case AST_NODE_TYPE_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UPLUS: return "unary 'plus'";
    case AST_NODE_TYPE_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS: return "unary 'minus'";
    case AST_NODE_TYPE_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_LNOT: return "logical 'not'";
    case AST_NODE_TYPE_OP_BWNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT: return "bitwise 'not'";
    case AST_NODE_TYPE_OP_SIZEOF:
    case AST_NODE_TYPE_REXPR_OP_SIZEOF: return "operator 'sizeof'";
    case AST_NODE_TYPE_OP_ADDROF:
    case AST_NODE_TYPE_REXPR_OP_ADDROF: return "operator 'addr of'";
    case AST_NODE_TYPE_OP_FILTER:
    case AST_NODE_TYPE_REXPR_OP_FILTER: return "operator 'filter'";
    case AST_NODE_TYPE_OP_SUBSCRIPT:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT: return "array subscript";
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        return "array subscript slice";
    case AST_NODE_TYPE_OP_MEMBER: return "operator 'member of'";
    case AST_NODE_TYPE_REXPR_FIELD: return "field expression";
    case AST_NODE_TYPE_REXPR_LINK: return "link expression";
    case AST_NODE_TYPE_REXPR_BUILTIN: return "builtin expression";
    }
    return "!!bad value type!!";
}

const char *
block_type_str(enum block_type type)
{
    switch (type) {
    case BLOCK_TYPE_UNDEF: return "undef";
    case BLOCK_TYPE_STRUCT: return "struct";
    case BLOCK_TYPE_UNION: return "union";
    }
    return "!!bad block type!!";
}

void yyerror(YYLTYPE *loc, yyscan_t scanner,
             struct parser_ctx *parser_ctx, void *out_param,
             const char *str)
{
    semantic_error(SEMANTIC_LOGLEVEL_ERROR, loc, "%s", str);
}
