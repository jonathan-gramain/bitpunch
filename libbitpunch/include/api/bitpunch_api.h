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

struct bitpunch_schema_hdl;

int
bitpunch_init(void);
void
bitpunch_cleanup(void);
int
bitpunch_load_schema_from_path(const char *path,
                                struct bitpunch_schema_hdl **schema);
int
bitpunch_load_schema_from_fd(int fd,
                              struct bitpunch_schema_hdl **schemap);
int
bitpunch_load_schema_from_buffer(const char *buf, size_t buf_size,
                                  struct bitpunch_schema_hdl **schemap);
int
bitpunch_load_schema_from_string(const char *str,
                                  struct bitpunch_schema_hdl **schemap);
void
bitpunch_close_schema(struct bitpunch_schema_hdl *schema);

void
bitpunch_free_schema(struct bitpunch_schema_hdl *schema);

int
bitpunch_load_binary_file_from_path(const char *path,
                                   struct bitpunch_binary_file_hdl **binary_filep);

int
bitpunch_load_binary_file_from_fd(int fd,
                                 struct bitpunch_binary_file_hdl **binary_filep);

int
bitpunch_load_binary_file_from_buffer(const char *data, size_t data_size,
                                     struct bitpunch_binary_file_hdl **binary_filep);

int
bitpunch_close_binary_file(struct bitpunch_binary_file_hdl *bf);

int
bitpunch_free_binary_file(struct bitpunch_binary_file_hdl *bf);

int
bitpunch_eval_expr(struct bitpunch_schema_hdl *schema,
                  struct bitpunch_binary_file_hdl *binary_file,
                  const char *expr,
                  enum expr_value_type *expr_value_typep,
                  union expr_value *expr_valuep,
                  enum expr_dpath_type *expr_dpath_typep,
                  union expr_dpath *expr_dpathp);

const char *
bitpunch_status_pretty(bitpunch_status_t bt_ret);

#endif
