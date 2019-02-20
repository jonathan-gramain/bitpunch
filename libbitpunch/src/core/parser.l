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
#include <assert.h>
#include "core/parser.h"
#include PATH_TO_PARSER_TAB_H

#define YY_DECL \
    int yylex(YYSTYPE * yylval_param, \
              YYLTYPE * yylloc_param, \
              yyscan_t yyscanner, \
              struct parser_ctx *parser_ctx, \
              void *out_param)

#define YY_USER_INIT { \
    yylloc_param->parser_ctx = parser_ctx; \
    yylloc_param->parser_line_column = 1; \
    yylloc_param->first_line = 1; \
    yylloc_param->first_column = 1; \
    yylloc_param->last_line = 1; \
    yylloc_param->last_column = 1; \
    yylloc_param->start_offset = 0; \
    yylloc_param->end_offset = 0; \
}

#define YY_USER_ACTION { \
    yylloc_param->first_line = yyget_lineno(yyscanner); \
    yylloc_param->first_column = yylloc_param->parser_line_column; \
    yylloc_param->parser_line_column += yyget_leng(yyscanner); \
    yylloc_param->last_line = yyget_lineno(yyscanner); \
    yylloc_param->last_column = yylloc_param->parser_line_column - 1; \
    yylloc_param->start_offset = yylloc_param->end_offset; \
    yylloc_param->end_offset += yyget_leng(yyscanner); \
}

char *lit_buf;
int lit_len;
int lit_alloc;

#define LIT_PUSH_CHAR(c) do { \
    if (lit_len == lit_alloc) { \
        lit_buf = realloc(lit_buf, lit_alloc * 2); \
        assert(NULL != lit_buf); \
        lit_alloc *= 2; \
    } \
    lit_buf[lit_len++] = (c); \
} while (0)

%}

%option reentrant bison-bridge bison-locations
%option yylineno
%x lit_squot lit_dquot

%%

%{
    /*
     * output the type of parser as the first token, depending on the
     * value of start_token in the parser context
     */
    int _start_token;

    _start_token = parser_ctx->start_token;
    parser_ctx->start_token = -1;
    if (-1 != _start_token) {
        return _start_token;
    }
%}

if         { return KW_IF; }
else       { return KW_ELSE; }
self       { return KW_SELF; }
let        { return KW_LET; }

true        {
    yylval_param->boolean = 1;
    return KW_TRUE;
}
false       {
    yylval_param->boolean = 0;
    return KW_FALSE;
}

    /* ignore comments */
\/\/[^\n]*\n { yylloc_param->parser_line_column = 1; }
\/\*([^*]|\*[^/])*\*\/ {}


"||"          { return TOK_LOR; }
"&&"          { return TOK_LAND; }
"=="          { return TOK_EQ; }
"!="          { return TOK_NE; }
">="          { return TOK_GE; }
"<="          { return TOK_LE; }
"<<"          { return TOK_LSHIFT; }
">>"          { return TOK_RSHIFT; }
".."          { return TOK_RANGE; }
"<>"          { return TOK_FILTER; }
"::"          { return TOK_SCOPE; }
[|^&><+\-*/%!~()=[\].{}] { return yytext[0]; }
sizeof        { return OP_SIZEOF; }

(\?|@)?[A-Za-z_][A-Za-z0-9_]*  {
    yylval_param->ident = strdup(yytext);
    return IDENTIFIER;
}
[0-9]+|0x[0-9a-fA-F]+      {
    char *endp;
    errno = 0;
    yylval_param->integer = (int64_t)strtoll(yytext, &endp, 0);
    if (ERANGE == errno) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, yylloc_param,
                       "integer too large, does not fit into a signed "
                       "64 bit integer");
        return TOK_ERROR;
    }
    if (*endp != '\0') {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, yylloc_param,
                       "invalid integer digits for the base");
        return TOK_ERROR;
    }
    return INTEGER;
}

  /* adapted from
   * http://flex.sourceforge.net/manual/Start-Conditions.html
   */

\'      {
    lit_alloc = 16;
    lit_buf = malloc_safe(lit_alloc);
    lit_len = 0;
    BEGIN(lit_squot);
}

<lit_squot>\'        { /* saw closing single quote - all done */
    BEGIN(INITIAL);
    yylval_param->literal.str = lit_buf;
    yylval_param->literal.len = lit_len;
    return LITERAL;
}

\"      {
    lit_alloc = 16;
    lit_buf = malloc_safe(lit_alloc);
    lit_len = 0;
    BEGIN(lit_dquot);
}

<lit_dquot>\"        { /* saw closing double quote - all done */
    BEGIN(INITIAL);
    yylval_param->literal.str = lit_buf;
    yylval_param->literal.len = lit_len;
    return LITERAL;
}

<lit_squot,lit_dquot>\n        {
    semantic_error(SEMANTIC_LOGLEVEL_ERROR, yylloc_param,
                   "newline in string constant");
    return TOK_ERROR;
}

<lit_squot,lit_dquot><<EOF>>        {
    semantic_error(SEMANTIC_LOGLEVEL_ERROR, yylloc_param,
                   "EOF reached during parsing of string constant");
    BEGIN(INITIAL);
    return TOK_ERROR;
}

<lit_squot,lit_dquot>\\[0-7]{1,3} {
    /* octal escape sequence */
    int result;

    (void) sscanf(yytext + 1, "%o", &result);

    if (result > 0xff) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, yylloc_param,
                       "constant is out-of-bounds: '%s'", yytext + 1);
        return TOK_ERROR;
    }
    LIT_PUSH_CHAR(result);
}

<lit_squot,lit_dquot>\\x[0-9a-fA-F]{0,2} {
    /* hexadecimal escape sequence */
    int result;

    if (yytext[2] == '\0') {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, yylloc_param,
                       "no digit for hex constant");
        return TOK_ERROR;
    }
    (void) sscanf(yytext + 2, "%x", &result);
    assert(result <= 0xff);
    LIT_PUSH_CHAR(result);
}

<lit_squot,lit_dquot>\\[0-9]+ {
    semantic_error(SEMANTIC_LOGLEVEL_ERROR, yylloc_param,
                   "bad escape sequence '%s'", yytext);
    return TOK_ERROR;
}

<lit_squot,lit_dquot>{
    \\n  LIT_PUSH_CHAR('\n');
    \\t  LIT_PUSH_CHAR('\t');
    \\r  LIT_PUSH_CHAR('\r');
    \\b  LIT_PUSH_CHAR('\b');
    \\f  LIT_PUSH_CHAR('\f');
}

<lit_squot,lit_dquot>\\(.|\n)  LIT_PUSH_CHAR(yytext[1]);

<lit_squot>[^\\\n\']+        {
    char *yptr = yytext;

    while ('\0' != *yptr) {
        LIT_PUSH_CHAR(*yptr++);
    }
}

<lit_dquot>[^\\\n\"]+        {
    char *yptr = yytext;

    while ('\0' != *yptr) {
        LIT_PUSH_CHAR(*yptr++);
    }
}


[ \r\t]   ;
\n      {
    yylloc_param->parser_line_column = 1;
}


.           { return yytext[0]; }

%%

int
bitpunch_parse_schema(struct ast_node_hdl *schema)
{
    struct parser_ctx *parser_ctx;
    yyscan_t scanner;
    FILE *fstream;
    int ret;

    parser_ctx = &schema->ndat->u.schema_def.parser_ctx;
    memset(parser_ctx, 0, sizeof (*parser_ctx));
    parser_ctx->parser_filepath = schema->ndat->u.schema_def.file_path;
    parser_ctx->parser_data = schema->ndat->u.schema_def.data;
    parser_ctx->parser_data_length = schema->ndat->u.schema_def.data_length;
    parser_ctx->start_token = START_SCHEMA;

    fstream = fmemopen((char *)schema->ndat->u.schema_def.data,
                       schema->ndat->u.schema_def.data_length, "r");
    if (NULL == fstream) {
        return -1;
    }
    yylex_init(&scanner);
    yyset_in(fstream, scanner);
    ret = yyparse(scanner, parser_ctx, schema);
    yylex_destroy(scanner);
    fclose(fstream);
    if (0 != ret) {
        return -1;
    }
    return 0;
}

int
bitpunch_parse_expr(const char *expr,
                    struct ast_node_hdl **expr_nodep,
                    struct parser_ctx **parser_ctxp)
{
    yyscan_t scanner;
    struct parser_ctx *parser_ctx;
    size_t expr_len;
    FILE *expr_stream;
    int ret;

    expr_len = strlen(expr);
    expr_stream = fmemopen((char *)expr, expr_len, "r");
    if (NULL == expr_stream) {
        fprintf(stderr,
                "Cannot open memory stream for expression evaluation: "
                "%s\n", strerror(errno));
        return -1;
    }
    parser_ctx = new_safe(struct parser_ctx);
    parser_ctx->parser_filepath = NULL;
    parser_ctx->parser_data = expr;
    parser_ctx->parser_data_length = expr_len;
    parser_ctx->start_token = START_EXPR;

    yylex_init(&scanner);
    yyset_in(expr_stream, scanner);
    ret = yyparse(scanner, parser_ctx, expr_nodep);
    yylex_destroy(scanner);
    fclose(expr_stream);
    if (0 != ret) {
        return -1;
    }
    if (NULL != parser_ctxp) {
        *parser_ctxp = parser_ctx;
    } else {
        free(parser_ctx);
    }
    return 0;
}
