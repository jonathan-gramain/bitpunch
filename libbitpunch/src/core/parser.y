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
#include "utils/dep_resolver.h"

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

    struct ast_node_data;
    struct ast_node_ref;
    struct param;

    struct statement;
    struct filter_instance;

    TAILQ_HEAD(statement_list, statement);

#define	STATEMENT_FIRST(stmt_type, head)        \
    (struct stmt_type *)TAILQ_FIRST((head))

#define STATEMENT_NEXT(stmt_type, var, field)                           \
    (struct stmt_type *)TAILQ_NEXT(((struct statement *)(var)), field)

#define	STATEMENT_FOREACH(stmt_type, var, head, field)                  \
    for ((var) = STATEMENT_FIRST(stmt_type, head);                      \
     (var); (var) = STATEMENT_NEXT(stmt_type, var, field))


    struct block_stmt_list {
        struct statement_list *field_list;
        struct statement_list *named_expr_list;
        struct statement_list *attribute_list;
    };

    typedef expr_value_t
        (*expr_evalop_fn_t)(expr_value_t operands[]);

    struct expr_evaluator {
        enum expr_value_type res_type_mask;
        expr_evalop_fn_t eval_fn;
    };

    const struct expr_evaluator *
        expr_lookup_evaluator(enum ast_node_type op_type,
                              enum expr_value_type opd_types[]);

    typedef struct filter_instance *
        (*filter_instance_build_func_t)(
            struct ast_node_hdl *filter);

    typedef int
        (*filter_instance_compile_func_t)(
            struct ast_node_hdl *filter,
            struct filter_instance *f_instance,
            dep_resolver_tagset_t tags,
            struct compile_ctx *ctx);

    typedef bitpunch_status_t
        (*extern_func_fn_t)(
            void *user_arg,
            expr_value_t *valuep,
            expr_dpath_t *dpathp,
            struct browse_state *bst);

    extern const int SPAN_SIZE_UNDEF;

    enum ast_node_flag {
        ASTFLAG_IS_ANONYMOUS_MEMBER         = (1<<0),
        ASTFLAG_HAS_POLYMORPHIC_ANCHOR      = (1<<1),
        ASTFLAG_REVERSE_COND                = (1<<2),
        ASTFLAG_CONTAINS_LAST_ATTR          = (1<<3),
        ASTFLAG_DUMPING                     = (1<<4),
        /** item defined by the user */
        ASTFLAG_EXTERNAL                    = (1<<5),
    };

    enum ast_node_data_flag {
        /** template filter */
        ASTFLAG_DATA_TEMPLATE               = (1<<0),
    };

    struct rexpr {
        enum expr_value_type value_type_mask;
        enum expr_dpath_type dpath_type_mask;
    };

    enum item_flag {
        ITEMFLAG_IS_SPAN_SIZE_VARIABLE       = (1<<0),
        ITEMFLAG_IS_USED_SIZE_VARIABLE       = (1<<1),
        ITEMFLAG_USES_SLACK                  = (1<<2),
        ITEMFLAG_SPREADS_SLACK               = (1<<3),
        ITEMFLAG_CONDITIONALLY_SPREADS_SLACK = (1<<4),
        ITEMFLAG_FILLS_SLACK                 = (1<<5),
        ITEMFLAG_CONDITIONALLY_FILLS_SLACK   = (1<<6),
        ITEMFLAG_FILTER_MAPS_LIST            = (1<<7),
        ITEMFLAG_FILTER_MAPS_OBJECT          = (1<<8),
    };

    struct item_node {
        struct rexpr rexpr; /* inherits */
        enum item_flag flags;
        int64_t min_span_size; /* minimum size */
    };

    struct dpath_node {
        struct ast_node_hdl *item;
        struct ast_node_hdl *filter;
        struct dep_resolver_node dr_node;
    };

    enum statement_type {
        STATEMENT_TYPE_FIELD = (1<<0),
        STATEMENT_TYPE_NAMED_EXPR = (1<<1),
        STATEMENT_TYPE_ATTRIBUTE = (1<<2),
    };

    struct named_statement_spec {
        enum statement_type stmt_type;
        struct named_statement *nstmt;
        const struct ast_node_hdl *anchor_filter;
        int anonymous_member;
    };

    struct ast_node_data {
        /* when changing this enum, don't forget to update
         * ast_node_type_str() */
        enum ast_node_type {
            AST_NODE_TYPE_NONE = 0,
            AST_NODE_TYPE_INTEGER,
            AST_NODE_TYPE_BOOLEAN,
            AST_NODE_TYPE_STRING,
            AST_NODE_TYPE_IDENTIFIER,
            AST_NODE_TYPE_SCOPE_DEF,
            AST_NODE_TYPE_FILTER_DEF,
            AST_NODE_TYPE_ARRAY,
            AST_NODE_TYPE_BYTE,
            AST_NODE_TYPE_BYTE_ARRAY,
            AST_NODE_TYPE_ARRAY_SLICE,
            AST_NODE_TYPE_BYTE_SLICE,
            AST_NODE_TYPE_CONDITIONAL,
            AST_NODE_TYPE_EXTERN_DECL,
            AST_NODE_TYPE_EXTERN_FUNC,
            AST_NODE_TYPE_EXTERN_FILTER,
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
            AST_NODE_TYPE_OP_ANCESTOR,
            AST_NODE_TYPE_OP_SUBSCRIPT,
            AST_NODE_TYPE_OP_SUBSCRIPT_SLICE,
            AST_NODE_TYPE_OP_MEMBER,
            AST_NODE_TYPE_OP_SCOPE,
            AST_NODE_TYPE_OP_FILTER,
            AST_NODE_TYPE_OP_FCALL,
            AST_NODE_TYPE_EXPR_SELF,
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
            AST_NODE_TYPE_REXPR_OP_ANCESTOR,
            AST_NODE_TYPE_REXPR_OP_FILTER,
            AST_NODE_TYPE_REXPR_FILTER,
            AST_NODE_TYPE_REXPR_OP_MEMBER,
            AST_NODE_TYPE_REXPR_OP_SCOPE,
            AST_NODE_TYPE_REXPR_FIELD,
            AST_NODE_TYPE_REXPR_NAMED_EXPR,
            AST_NODE_TYPE_REXPR_POLYMORPHIC,
            AST_NODE_TYPE_REXPR_BUILTIN,
            AST_NODE_TYPE_REXPR_OP_SUBSCRIPT,
            AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE,
            AST_NODE_TYPE_REXPR_OP_FCALL,
            AST_NODE_TYPE_REXPR_SELF,
            AST_NODE_TYPE_REXPR_EXTERN_DECL,
            AST_NODE_TYPE_REXPR_EXTERN_FUNC,
        } type;
        enum ast_node_data_flag flags;
        union {
            int64_t integer;
            int boolean;
            struct expr_value_string string;
            char *identifier;
            struct item_node item;
            struct scope_def {
                struct block_stmt_list block_stmt_list;
            } scope_def;
            struct filter_def {
                struct scope_def scope_def; /* inherits */
                const char *filter_type;
                struct filter_class *filter_cls; /* set after resolve */
            } filter_def;
            struct conditional {
                struct ast_node_hdl *cond_expr;
                struct ast_node_hdl *outer_cond;
            } conditional;
            struct extern_decl {
                struct ast_node_hdl *filter_spec;
                struct filter_class *filter_cls; /* set after resolve */
                /** list of instances bound to this external filter */
                SLIST_HEAD(filter_instance_list,
                           filter_instance_extern) instance_list;
            } extern_decl;
            struct extern_func {
                extern_func_fn_t extern_func_fn;
                void *user_arg;
            } extern_func;
            struct extern_filter {
                filter_instance_build_func_t build_func;
                filter_instance_compile_func_t compile_func;
                void *user_arg;
            } extern_filter;
            struct op {
                struct ast_node_hdl *operands[2];
            } op;
            struct subscript_common {
                struct ast_node_hdl *anchor_expr;
            } op_subscript_common;
            struct subscript {
                struct subscript_common common; /* inherits */
                struct subscript_index {
                    struct ast_node_hdl *key;
                    struct ast_node_hdl *twin;
                    struct dep_resolver_node dr_node;
                } index;
            } op_subscript;
            struct subscript_slice {
                struct subscript_common common; /* inherits */
                struct subscript_index start;
                struct subscript_index end;
            } op_subscript_slice;
            struct fcall {
                struct ast_node_hdl *object;
                struct ast_node_hdl *func;
                struct statement_list *func_params;
            } op_fcall;
            struct rexpr rexpr; /* base, not instanciable */
            struct rexpr_filter {
                struct item_node item; /* inherits */
                struct filter_def *filter_def;
                const struct filter_class *filter_cls;
                struct filter_instance *f_instance;
            } rexpr_filter;
            struct rexpr_op_filter {
                struct rexpr_filter rexpr_filter; /* inherits */
                struct ast_node_hdl *filter_expr;
                struct ast_node_hdl *target;
            } rexpr_op_filter;
            struct rexpr_native {
                struct rexpr rexpr; /* inherits */
                expr_value_t value;
            } rexpr_native;
            struct rexpr_member_common {
                struct rexpr rexpr; /* inherits */
                struct ast_node_hdl *anchor_expr;
                struct ast_node_hdl *anchor_filter;
            } rexpr_member_common;
            struct rexpr_self {
                struct rexpr_member_common rexpr; /* inherits */
            } rexpr_self;
            struct rexpr_field {
                /* inherits */
                struct rexpr_member_common rexpr_member_common;
                const struct field *field;
            } rexpr_field;
            struct rexpr_named_expr {
                /* inherits */
                struct rexpr_member_common rexpr_member_common;
                const struct named_expr *named_expr;
            } rexpr_named_expr;
            struct rexpr_polymorphic {
                /* inherits */
                struct rexpr_member_common rexpr_member_common;
                const char *identifier;
                struct named_statement_spec *visible_statements;
                int n_visible_statements;
            } rexpr_polymorphic;
            struct rexpr_builtin {
                struct rexpr rexpr; /* inherits */
                //struct ast_node_hdl *anchor_expr;
                const struct expr_builtin_fn *builtin;
            } rexpr_builtin;
            struct rexpr_op {
                struct rexpr rexpr; /* inherits */
                struct op op;
                const struct expr_evaluator *evaluator;
            } rexpr_op;
            struct rexpr_op_subscript_common {
                struct rexpr rexpr; /* inherits */
                struct ast_node_hdl *anchor_expr;
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
                int n_func_params;
                struct statement_list *func_params;
            } rexpr_op_fcall;
            struct rexpr_extern_func {
                struct rexpr rexpr; /* inherits */
                struct extern_func extern_func;
            } rexpr_extern_func;
        } u;
    };

    struct ast_node_hdl {
        struct ast_node_data *ndat;
        struct parser_location loc;
        enum ast_node_flag flags;
        enum resolve_identifiers_tag resolved_tags;
        struct dep_resolver_node dr_node;
    };

    struct statement {
        TAILQ_ENTRY(statement) list;
        struct parser_location loc;
        int stmt_flags; // type-specific flags
        struct ast_node_hdl *cond;
    };

    enum statement_flag {
        STATEMENT_FLAGS_END = (1<<0),
    };

    struct named_statement {
        struct statement stmt; // inherits
        char *name;
    };

    enum named_statement_flag {
        NAMED_STATEMENT_FLAGS_END          = (STATEMENT_FLAGS_END<<0),
    };

    struct named_expr {
        struct named_statement nstmt; // inherits
        struct ast_node_hdl *expr;
    };

    struct field {
        struct named_statement nstmt; // inherits
        struct ast_node_hdl *filter;
        struct dep_resolver_node dr_node;
    };

    enum field_flag {
        FIELD_FLAG_HIDDEN        = (NAMED_STATEMENT_FLAGS_END<<0),
        FIELD_FLAG_HEADER        = (NAMED_STATEMENT_FLAGS_END<<1),
        FIELD_FLAG_TRAILER       = (NAMED_STATEMENT_FLAGS_END<<2),
    };

    typedef bitpunch_status_t
        (*expr_eval_builtin_fn_t)(
            struct ast_node_hdl *object,
            struct statement_list *params,
            int n_params,
            enum expr_evaluate_flag flags,
            expr_value_t *valuep,
            expr_dpath_t *dpathp,
            struct browse_state *bst);

    struct expr_builtin_fn {
        const char *builtin_name;
        enum expr_value_type res_value_type_mask;
        enum expr_value_type res_dpath_type_mask;
        expr_eval_builtin_fn_t eval_fn;
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
    struct ast_node_hdl *ast_node_hdl_new(void);
    struct ast_node_hdl *
    ast_node_hdl_create(enum ast_node_type type,
                        const struct parser_location *loc);
    struct ast_node_hdl *
    ast_node_hdl_create_scope(const struct parser_location *loc);

    void init_block_stmt_list(struct block_stmt_list *dst);
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

    struct ast_node_hdl *
    ast_node_hdl_new(void) {
        struct ast_node_hdl *nhdl;

        nhdl = new_safe(struct ast_node_hdl);
        dep_resolver_node_init(&nhdl->dr_node);
        return nhdl;
    }

    void
    init_block_stmt_list(struct block_stmt_list *dst)
    {
        memset(dst, 0, sizeof (*dst));
        dst->field_list = new_safe(struct statement_list);
        dst->named_expr_list = new_safe(struct statement_list);
        dst->attribute_list = new_safe(struct statement_list);
        TAILQ_INIT(dst->field_list);
        TAILQ_INIT(dst->named_expr_list);
        TAILQ_INIT(dst->attribute_list);
    }

    struct ast_node_hdl *
    ast_node_hdl_create(enum ast_node_type type,
                        const struct parser_location *loc)
    {
        struct ast_node_hdl *nhdl;

        nhdl = ast_node_hdl_new();
        if (NULL != loc) {
            nhdl->loc = *loc;
        }
        nhdl->ndat = new_safe(struct ast_node_data);
        nhdl->ndat->type = type;
        return nhdl;
    }
    
    struct ast_node_hdl *
    ast_node_hdl_create_scope(const struct parser_location *loc)
    {
        struct ast_node_hdl *scope_node;

        scope_node = ast_node_hdl_create(AST_NODE_TYPE_SCOPE_DEF, loc);
        init_block_stmt_list(&scope_node->ndat->u.scope_def.block_stmt_list);

        return scope_node;
    }

    static struct ast_node_hdl *
    expr_gen_ast_node(enum ast_node_type op_type,
                      struct ast_node_hdl *opd1,
                      struct ast_node_hdl *opd2,
                      const struct parser_location *loc)
    {
        struct ast_node_hdl *nhdl;

        nhdl = ast_node_hdl_create(op_type, loc);
        nhdl->ndat->u.op.operands[0] = opd1;
        nhdl->ndat->u.op.operands[1] = opd2;
        return nhdl;
    }

    static int
    merge_block_stmt_list(struct block_stmt_list *dst,
                          struct block_stmt_list *src)
    {
        TAILQ_CONCAT(dst->field_list, src->field_list, list);
        TAILQ_CONCAT(dst->named_expr_list, src->named_expr_list, list);
        TAILQ_CONCAT(dst->attribute_list, src->attribute_list, list);
        return 0;
    }

    static void
    attach_outer_conditional(struct ast_node_hdl **inner_condp,
                             struct ast_node_hdl *outer_cond)
    {
        struct ast_node_hdl **condp;

        for (condp = inner_condp;
             NULL != *condp && *condp != outer_cond;
             condp = &(*condp)->ndat->u.conditional.outer_cond)
            ;
        if (NULL == *condp) {
            *condp = outer_cond;
        }
    }

    static void
    attribute_list_push(struct statement_list *attribute_list,
                        const char *attr_name, struct parser_location *loc,
                        struct ast_node_hdl *attr_expr)
    {
        struct named_expr *attr;

        attr = new_safe(struct named_expr);
        if (NULL != loc) {
            attr->nstmt.stmt.loc = *loc;
        }
        attr->nstmt.name = strdup_safe(attr_name);
        attr->expr = attr_expr;
        TAILQ_INSERT_TAIL(attribute_list, (struct statement *)attr, list);
    }
 }

%union {
    int64_t integer;
    int boolean;
    char *ident;
    struct expr_value_string literal;
    struct ast_node_hdl *ast_node_hdl;
    struct field *field;
    struct block_stmt_list block_stmt_list;
    struct statement_list *statement_list;
    struct named_expr *named_expr;
    struct func_param *func_param;
    struct subscript_index subscript_index;
}

%token TOK_ERROR
%token <ident> IDENTIFIER
%token <integer> INTEGER
%token <literal> LITERAL
%token <boolean> KW_TRUE KW_FALSE
%token KW_IF KW_ELSE KW_SELF KW_LET KW_EXTERN

%token <ast_node_type> '|' '^' '&' '>' '<' '+' '-' '*' '/' '%' '!' '~' '.' ':'
%token <ast_node_type> TOK_LOR "||"
%token <ast_node_type> TOK_LAND "&&"
%token <ast_node_type> TOK_EQ "=="
%token <ast_node_type> TOK_NE "!="
%token <ast_node_type> TOK_GE ">="
%token <ast_node_type> TOK_LE "<="
%token <ast_node_type> TOK_LSHIFT "<<"
%token <ast_node_type> TOK_RSHIFT ">>"
%token <ast_node_type> TOK_RANGE ".."
%token <ast_node_type> TOK_FILTER "<>"
%token <ast_node_type> TOK_SCOPE "::"
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
%left  ':' "<>"
%right OP_ARRAY_DECL
%left  OP_SUBSCRIPT OP_FCALL '.' "::"

%type <ast_node_hdl> schema g_integer g_boolean g_identifier g_self g_literal scope_block filter_block expr opt_expr twin_index opt_twin_index
%type <block_stmt_list> block_stmt_list if_block else_block opt_else_block
%type <statement_list> func_params func_param_nonempty_list
%type <named_expr> attribute_stmt let_stmt extern_stmt func_param
%type <subscript_index> key_expr opt_key_expr
%locations

%token START_SCHEMA START_EXPR

%start start

%%

start:
    START_SCHEMA schema
  | START_EXPR start_expr
    /* here go other parsers with shared grammar */

g_integer:
    INTEGER {
        $$ = ast_node_hdl_create(AST_NODE_TYPE_INTEGER, &@$);
        $$->ndat->u.integer = $1;
    }
g_boolean:
    KW_TRUE {
        $$ = ast_node_hdl_create(AST_NODE_TYPE_BOOLEAN, &@$);
        $$->ndat->u.boolean = 1;
    }
  | KW_FALSE {
        $$ = ast_node_hdl_create(AST_NODE_TYPE_BOOLEAN, &@$);
        $$->ndat->u.boolean = 0;
    }
g_identifier:
    IDENTIFIER {
        $$ = ast_node_hdl_create(AST_NODE_TYPE_IDENTIFIER, &@$);
        $$->ndat->u.identifier = $1;
    }
g_self:
    KW_SELF {
        $$ = ast_node_hdl_create(AST_NODE_TYPE_EXPR_SELF, &@$);
    }
g_literal:
    LITERAL {
        $$ = ast_node_hdl_create(AST_NODE_TYPE_STRING, &@$);
        $$->ndat->u.string.str = $1.str;
        $$->ndat->u.string.len = $1.len;
    }
  | g_literal LITERAL {
      int64_t new_len;

      // concatenate consecutive string literals
      $$ = $1;
      new_len = $$->ndat->u.string.len + $2.len;
      $$->ndat->u.string.str = realloc_safe((char *)$$->ndat->u.string.str, new_len);
      memcpy((char *)$$->ndat->u.string.str + $$->ndat->u.string.len, $2.str, $2.len);
      $$->ndat->u.string.len = new_len;
    }

start_expr: expr {
    memcpy(out_param, &$expr, sizeof($expr));
}

expr:
    g_integer
  | g_boolean
  | g_identifier
  | g_literal
  | g_self
  | scope_block
  | filter_block
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
  | '^' expr %prec OP_ARITH_UNARY_OP {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_ANCESTOR, $2, NULL, &@1);
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
  | expr TOK_SCOPE g_identifier {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_SCOPE, $1, $3, &@2);
        parser_location_make_span(&$$->loc, &@1, &@3);
    }
  | expr TOK_SCOPE g_self {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_SCOPE, $1, $3, &@2);
        parser_location_make_span(&$$->loc, &@1, &@3);
    }
  | expr TOK_FILTER expr {
        $$ = expr_gen_ast_node(AST_NODE_TYPE_OP_FILTER, $1, $3, &@2);
    }
  | expr '[' opt_key_expr ']' %prec OP_SUBSCRIPT {
        $$ = ast_node_hdl_create(AST_NODE_TYPE_OP_SUBSCRIPT, NULL);
        parser_location_make_span(&$$->loc, &@2, &@4);
        $$->ndat->u.op_subscript_common.anchor_expr = $1;
        $$->ndat->u.op_subscript.index = $3;
    }
  | expr '['
    opt_key_expr TOK_RANGE opt_key_expr ']' %prec OP_SUBSCRIPT {
        $$ = ast_node_hdl_create(AST_NODE_TYPE_OP_SUBSCRIPT_SLICE, &@$);
        parser_location_make_span(&$$->loc, &@2, &@6);
        $$->ndat->u.op_subscript_common.anchor_expr = $1;
        $$->ndat->u.op_subscript_slice.start = $3;
        $$->ndat->u.op_subscript_slice.end = $5;
    }
  | expr '(' func_params ')' %prec OP_FCALL {
        $$ = ast_node_hdl_create(AST_NODE_TYPE_OP_FCALL, &$1->loc);
        $$->ndat->u.op_fcall.func = $1;
        $$->ndat->u.op_fcall.func_params = $func_params;
    }
  | '(' expr ')' {
        $$ = $2;
    }
  | '[' opt_expr ']' expr %prec OP_ARRAY_DECL {
      /* Array declaration syntax is equivalent to regular filter
       * block syntax with filter type "array", and attributes @item
       * and (optionally) @length set. */

        $$ = ast_node_hdl_create(AST_NODE_TYPE_FILTER_DEF, NULL);
        parser_location_make_span(&$$->loc, &@1, &@4);
        $$->ndat->u.filter_def.filter_type = "array";
        init_block_stmt_list(&$$->ndat->u.scope_def.block_stmt_list);
        attribute_list_push(
            $$->ndat->u.scope_def.block_stmt_list.attribute_list,
            "@item", &@4, $4);
        if (NULL != $2) {
            attribute_list_push(
                $$->ndat->u.scope_def.block_stmt_list.attribute_list,
                "@length", &@2, $2);
        }
    }

opt_expr:
    /* empty */ {
      memset(&$$, 0, sizeof ($$));
    }
  | expr

key_expr:
    expr opt_twin_index {
      memset(&$$, 0, sizeof ($$));
      $$.key = $expr;
      $$.twin = $opt_twin_index;
    }

opt_key_expr:
    /* empty */ {
      memset(&$$, 0, sizeof ($$));
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

func_params:
    /* empty */ {
        $$ = new_safe(struct statement_list);
        TAILQ_INIT($$);
    }
  | func_param_nonempty_list {
        $$ = $func_param_nonempty_list;
    }

func_param_nonempty_list:
    func_param {
        $$ = new_safe(struct statement_list);
        TAILQ_INIT($$);
        TAILQ_INSERT_TAIL($$, (struct statement *)$func_param, list);
    }
  | func_param_nonempty_list ',' func_param {
        $$ = $1;
        TAILQ_INSERT_TAIL($$, (struct statement *)$func_param, list);
    }

func_param:
    IDENTIFIER '=' expr {
        $$ = new_safe(struct named_expr);
        $$->nstmt.stmt.loc = @IDENTIFIER;
        $$->nstmt.name = $IDENTIFIER;
        $$->expr = $expr;
    }
  | expr {
        $$ = new_safe(struct named_expr);
        $$->nstmt.stmt.loc = @expr;
        $$->expr = $expr;
    }


schema:
    block_stmt_list {
        $$ = ast_node_hdl_create(AST_NODE_TYPE_SCOPE_DEF, &@$);
        $$->loc = @1;
        $$->ndat->u.scope_def.block_stmt_list = $block_stmt_list;
        memcpy(out_param, &$$, sizeof($$));
    }

scope_block:
    '{' block_stmt_list '}' {
        $$ = ast_node_hdl_create(AST_NODE_TYPE_SCOPE_DEF, &@$);
        $$->loc = @1;
        $$->ndat->u.scope_def.block_stmt_list = $block_stmt_list;
    }

filter_block:
    IDENTIFIER scope_block {
        $$ = $scope_block;
        $$->ndat->type = AST_NODE_TYPE_FILTER_DEF;
        $$->loc = @IDENTIFIER;
        $$->ndat->u.filter_def.filter_type = $IDENTIFIER;
    }

if_block:
    KW_IF '(' expr ')' '{' block_stmt_list '}' opt_else_block {
        struct ast_node_hdl *cond;
        struct statement *stmt;

        cond = ast_node_hdl_create(AST_NODE_TYPE_CONDITIONAL, &@expr);
        cond->ndat->u.conditional.cond_expr = $expr;

        TAILQ_FOREACH(stmt, $block_stmt_list.field_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        TAILQ_FOREACH(stmt, $block_stmt_list.named_expr_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        TAILQ_FOREACH(stmt, $block_stmt_list.attribute_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        $$ = $block_stmt_list;

        cond = ast_node_hdl_create(AST_NODE_TYPE_CONDITIONAL, &@expr);
        cond->ndat->u.conditional.cond_expr = $expr;
        cond->flags |= ASTFLAG_REVERSE_COND;

        TAILQ_FOREACH(stmt, $opt_else_block.field_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        TAILQ_FOREACH(stmt, $opt_else_block.named_expr_list, list) {
            attach_outer_conditional(&stmt->cond, cond);
        }
        TAILQ_FOREACH(stmt, $opt_else_block.attribute_list, list) {
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
  | block_stmt_list attribute_stmt {
        $$ = $1;
        if (NULL != $attribute_stmt->nstmt.name
            && $attribute_stmt->nstmt.name[0] == '@') {
            TAILQ_INSERT_TAIL($$.attribute_list,
                              (struct statement *)$attribute_stmt, list);
        } else {
            struct field *field;

            field = new_safe(struct field);
            field->nstmt = $attribute_stmt->nstmt;
            field->filter = $attribute_stmt->expr;
            TAILQ_INSERT_TAIL($$.field_list, (struct statement *)field, list);
        }
    }

  | block_stmt_list let_stmt {
        $$ = $1;
        TAILQ_INSERT_TAIL($$.named_expr_list,
                          (struct statement *)$let_stmt, list);
    }

  | block_stmt_list extern_stmt {
        $$ = $1;
        TAILQ_INSERT_TAIL($$.named_expr_list,
                          (struct statement *)$extern_stmt, list);
    }

  | block_stmt_list if_block {
      /* join 'if' node children to block stmt lists */
      if (-1 == merge_block_stmt_list(&$$, &$if_block)) {
          YYERROR;
      }
  }

attribute_stmt:
    IDENTIFIER ':' expr ';' {
        $$ = new_safe(struct named_expr);
        $$->nstmt.name = $IDENTIFIER;
        $$->nstmt.stmt.loc = @$;
        $$->expr = $expr;
    }
  | expr ';' {
        $$ = new_safe(struct named_expr);
        $$->nstmt.name = NULL;
        $$->nstmt.stmt.loc = @$;
        $$->expr = $expr;
    }

let_stmt:
    KW_LET IDENTIFIER '=' expr ';' {
        $$ = new_safe(struct named_expr);
        $$->nstmt.stmt.loc = @$;
        $$->nstmt.name = $IDENTIFIER;
        $$->expr = $expr;
    }

extern_stmt:
    KW_EXTERN filter_block ';' {
        $$ = new_safe(struct named_expr);
        $$->nstmt.stmt.loc = @$;
        $$->nstmt.name = strdup($filter_block->ndat->u.filter_def.filter_type);
        $$->expr = ast_node_hdl_create(AST_NODE_TYPE_EXTERN_DECL, &@$);
        $$->expr->ndat->u.extern_decl.filter_spec = $filter_block;
        SLIST_INIT(&$$->expr->ndat->u.extern_decl.instance_list);
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
    const struct parser_ctx *parser_ctx;
    const char *schema_end;
    const char *line_start;
    const char *line_end;

    parser_ctx = loc->parser_ctx;
    if (NULL == parser_ctx) {
        return 0;
    }
    if (NULL != parser_ctx->parser_filepath) {
        fprintf(out, "%s:%d:\n",
                parser_ctx->parser_filepath, loc->last_line);
    }
    line_start = (parser_ctx->parser_data + loc->end_offset)
        - loc->last_column;
    schema_end =
        parser_ctx->parser_data +
        parser_ctx->parser_data_length;
    if (0 == parser_ctx->parser_data_length) {
        line_end = parser_ctx->parser_data;
    } else {
        line_end = memchr(line_start, '\n', schema_end - line_start);
        if (NULL == line_end)
            line_end = schema_end;
    }
    return fprintf(out, "%.*s\n%*s\n",
                   (int)(line_end - line_start), line_start,
                   (int)(loc->end_offset
                         - (line_start - parser_ctx->parser_data)),
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

    if (NULL != loc && NULL != loc->parser_ctx) {
        if (NULL != loc->parser_ctx->parser_filepath) {
            fprintf(stderr, "%s at %s:%d:\n",
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
    case AST_NODE_TYPE_SCOPE_DEF: return "scope def";
    case AST_NODE_TYPE_FILTER_DEF: return "filter def";
    case AST_NODE_TYPE_ARRAY: return "array";
    case AST_NODE_TYPE_BYTE: return "byte";
    case AST_NODE_TYPE_BYTE_ARRAY: return "byte array";
    case AST_NODE_TYPE_ARRAY_SLICE: return "slice";
    case AST_NODE_TYPE_BYTE_SLICE: return "byte slice";
    case AST_NODE_TYPE_CONDITIONAL: return "conditional";
    case AST_NODE_TYPE_EXTERN_DECL:
    case AST_NODE_TYPE_REXPR_EXTERN_DECL: return "extern declaration";
    case AST_NODE_TYPE_EXTERN_FUNC:
    case AST_NODE_TYPE_REXPR_EXTERN_FUNC: return "extern func";
    case AST_NODE_TYPE_EXTERN_FILTER: return "extern filter";
    case AST_NODE_TYPE_REXPR_NATIVE: return "native type";
    case AST_NODE_TYPE_OP_FCALL:
    case AST_NODE_TYPE_REXPR_OP_FCALL: return "function call";
    case AST_NODE_TYPE_EXPR_SELF:
    case AST_NODE_TYPE_REXPR_SELF: return "'self' expr";
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
    case AST_NODE_TYPE_OP_FILTER:
    case AST_NODE_TYPE_REXPR_OP_FILTER: return "operator 'filter'";
    case AST_NODE_TYPE_REXPR_FILTER: return "filter";
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
    case AST_NODE_TYPE_OP_ANCESTOR:
    case AST_NODE_TYPE_REXPR_OP_ANCESTOR: return "operator 'ancestor'";
    case AST_NODE_TYPE_OP_SUBSCRIPT:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT: return "array subscript";
    case AST_NODE_TYPE_OP_SUBSCRIPT_SLICE:
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        return "array subscript slice";
    case AST_NODE_TYPE_OP_MEMBER:
    case AST_NODE_TYPE_REXPR_OP_MEMBER: return "operator 'member of'";
    case AST_NODE_TYPE_OP_SCOPE:
    case AST_NODE_TYPE_REXPR_OP_SCOPE: return "operator 'scope'";
    case AST_NODE_TYPE_REXPR_FIELD: return "field value";
    case AST_NODE_TYPE_REXPR_NAMED_EXPR: return "named expression";
    case AST_NODE_TYPE_REXPR_POLYMORPHIC: return "polymorphic value";
    case AST_NODE_TYPE_REXPR_BUILTIN: return "builtin function";
    }
    return "!!bad value type!!";
}

void yyerror(YYLTYPE *loc, yyscan_t scanner,
             struct parser_ctx *parser_ctx, void *out_param,
             const char *str)
{
    semantic_error(SEMANTIC_LOGLEVEL_ERROR, loc, "%s", str);
}
