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

#ifndef __BITPUNCH_API_H__
#define __BITPUNCH_API_H__

#include "core/parser.h"
#include PATH_TO_PARSER_TAB_H

#if defined DEBUG
extern int tracker_debug_mode;
#endif

struct bitpunch_schema;

int
bitpunch_init(void);
void
bitpunch_cleanup(void);
int
bitpunch_schema_create_from_path(
    const char *path,
    struct bitpunch_schema **schema);
int
bitpunch_schema_create_from_file_descriptor(
    int fd,
    struct bitpunch_schema **schemap);
int
bitpunch_schema_create_from_buffer(
    const char *buf, size_t buf_size,
    struct bitpunch_schema **schemap);
int
bitpunch_schema_create_from_string(
    const char *str,
    struct bitpunch_schema **schemap);
void
bitpunch_schema_close(struct bitpunch_schema *schema);

void
bitpunch_schema_free(struct bitpunch_schema *schema);

int
bitpunch_data_source_create_from_file_path(
    const char *path,
    struct bitpunch_data_source **dsp);

int
bitpunch_data_source_create_from_file_descriptor(
    int fd,
    struct bitpunch_data_source **dsp);

int
bitpunch_data_source_create_from_memory(
    const char *data, size_t data_size, int managed_buffer,
    struct bitpunch_data_source **dsp);

int
bitpunch_data_source_close(struct bitpunch_data_source *ds);

int
bitpunch_data_source_free(struct bitpunch_data_source *ds);

int
bitpunch_parse_expr(const char *expr,
                    struct ast_node_hdl **expr_node_p,
                    struct parser_ctx **parser_ctxp);
int
bitpunch_resolve_expr(struct ast_node_hdl *expr, struct box *scope);

int
bitpunch_eval_expr(struct bitpunch_schema *schema,
                   struct bitpunch_data_source *ds,
                   const char *expr,
                   struct box *scope,
                   expr_value_t *valuep, expr_dpath_t *dpathp,
                   struct tracker_error **errp);

const char *
bitpunch_status_pretty(bitpunch_status_t bt_ret);

#endif
