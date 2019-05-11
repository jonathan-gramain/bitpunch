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

enum filter_attr_flag {
    FILTER_ATTR_MANDATORY = (1u<<0),
};

enum filter_class_flag {
    /** set when the filter maps to a list type */
    FILTER_CLASS_MAPS_LIST = (1u<<0),
    /** set when the filter maps to an object type */
    FILTER_CLASS_MAPS_OBJECT = (1u<<1),
    /** set when the filter allows arbitrary attribute names */
    FILTER_CLASS_UNRESTRICTED_ATTRIBUTE_NAMES = (1u<<2),
};

int
bitpunch_init(void);
void
bitpunch_cleanup(void);
int
bitpunch_schema_create_from_path(
    struct ast_node_hdl **schemap, const char *path);
int
bitpunch_schema_create_from_file_descriptor(
    struct ast_node_hdl **schemap, int fd);
int
bitpunch_schema_create_from_buffer(
    struct ast_node_hdl **schemap, const char *buf, size_t buf_size);
int
bitpunch_schema_create_from_string(
    struct ast_node_hdl **schemap, const char *str);

void
bitpunch_schema_free(struct ast_node_hdl *schema);

int
bitpunch_data_source_create_from_file_path(
    struct bitpunch_data_source **dsp, const char *path);

void
bitpunch_data_source_notify_file_change(const char *path);

int
bitpunch_data_source_create_from_file_descriptor(
    struct bitpunch_data_source **dsp, int fd);

void
bitpunch_data_source_create_from_memory(
    struct bitpunch_data_source **dsp,
    const char *data, size_t data_size, int manage_buffer);

void
bitpunch_buffer_new(
    struct bitpunch_data_source **dsp,
    size_t buffer_size);

void
bitpunch_data_source_acquire(struct bitpunch_data_source *ds);

int
bitpunch_data_source_release(struct bitpunch_data_source *ds);

struct ast_node_hdl *
bitpunch_data_source_to_filter(struct bitpunch_data_source *ds);

int
bitpunch_external_create_function(
    struct ast_node_hdl **nodep,
    extern_func_fn_t extern_func_fn,
    void *user_arg);

int
bitpunch_external_create_filter(
    struct ast_node_hdl **nodep,
    filter_instance_build_func_t build_func,
    filter_instance_compile_func_t compile_func,
    void *user_arg);

struct bitpunch_board *
bitpunch_board_new(void);

void
bitpunch_board_free(
    struct bitpunch_board *board);

void
bitpunch_board_add_let_expression(
    struct bitpunch_board *board,
    const char *name,
    struct ast_node_hdl *expr);

int
bitpunch_board_remove_by_name(
    struct bitpunch_board *board,
    const char *name);

void
bitpunch_board_add_external_def(
    struct bitpunch_board *board,
    const char *name,
    struct ast_node_hdl *external_def);

void
bitpunch_board_use_spec(
    struct bitpunch_board *board,
    struct ast_node_hdl *spec);

void
bitpunch_board_forget_spec(
    struct bitpunch_board *board);

bitpunch_status_t
bitpunch_board_add_expr(
    struct bitpunch_board *board,
    const char *name,
    const char *expr);

struct ast_node_hdl *
bitpunch_board_get_item(
    struct bitpunch_board *board,
    const char *name);

struct ast_node_hdl *
bitpunch_board_get_external_item(
    struct bitpunch_board *board,
    const char *name);

bitpunch_status_t
bitpunch_eval_expr(
    struct bitpunch_board *board,
    const char *expr,
    struct box *scope,
    enum bitpunch_eval_flag flags,
    struct ast_node_hdl **parsed_exprp,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct bitpunch_error **errp);

const char *
bitpunch_status_pretty(bitpunch_status_t bt_ret);

#endif
