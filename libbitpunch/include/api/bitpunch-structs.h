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

#ifndef __BITPUNCH_STRUCTS_H__
#define __BITPUNCH_STRUCTS_H__

#include <stdio.h>

#define BITPUNCH_SCHEMA_MAX_LENGTH   1048576

typedef enum bitpunch_status {
    BITPUNCH_OK = 0,
    BITPUNCH_ERROR = -1,
    BITPUNCH_INVALID_PARAM = -2,
    BITPUNCH_INVALID_STATE = -3,
    BITPUNCH_NO_ITEM = -4,
    BITPUNCH_NOT_CONTAINER = -5,
    BITPUNCH_DATA_ERROR = -6,
    BITPUNCH_OUT_OF_BOUNDS_ERROR = -7,
    BITPUNCH_NOT_IMPLEMENTED = -8,
    BITPUNCH_NO_DATA = -9,
    BITPUNCH_STATUS_LAST = -9,
} bitpunch_status_t;

struct bitpunch_error_context_info {
    /** error context messages pointing to offsets in @ref error_buf */
    const char *message;
    struct tracker *tk;
    struct box *box;
    const struct ast_node_hdl *node;
};


/** abstract type for error-specific info object */
typedef void bitpunch_error_info_t;

/**
 * @brief information about last error that occurred during a tracker
 * API call
 */
struct bitpunch_error {
    struct tracker *tk;      /**< copy of tracker when the error
                              * occurred (mutually exclusive with @ref
                              * box) */

    struct box *box;         /**< box when the error occurred
                              * (mutually exclusive with @ref tk) */

    bitpunch_status_t bt_ret; /**< error status code */

    enum bitpunch_error_flags {
        TRACKER_ERROR_STATIC = (1<<0), /**< error is statically allocated */
    } flags;               /**< error flags */

    const struct ast_node_hdl *node; /**< node that relates to the error */

#define BITPUNCH_ERROR_BUF_SIZE 2048
    /**< string table holding custom error message and context
     * information */
    char error_buf[BITPUNCH_ERROR_BUF_SIZE];

    char *error_buf_end;     /**< end of @ref error_buf contents */

    const char *reason;      /**< points to the reason phrase in @ref
                              * error_buf */

    int n_contexts;          /**< number of error context messages */

#define BITPUNCH_ERROR_MAX_CONTEXTS 64
    struct bitpunch_error_context_info contexts[BITPUNCH_ERROR_MAX_CONTEXTS];

    bitpunch_error_info_t *error_info; /**< error-specific additional info */

    /** argument that may be attached when user code raises a bitpunch
     * error from inside an API call. It can be retrieved from the
     * error object later. */
    void *user_arg;
};

enum parser_type {
    PARSER_TYPE_SCHEMA,
    PARSER_TYPE_EXPR,
};

struct parser_ctx {
    const char *parser_filepath;
    const char *parser_data;
    size_t      parser_data_length;
    enum parser_type parser_type;
    int         _start_token; // internal
};

enum bitpunch_schema_type {
    BITPUNCH_SCHEMA_TYPE_UNSET = 0,
    BITPUNCH_SCHEMA_TYPE_FILEPATH,
    BITPUNCH_SCHEMA_TYPE_FILE_DESCRIPTOR,
    BITPUNCH_SCHEMA_TYPE_BUFFER,
};

struct bitpunch_data_source;

enum bitpunch_data_source_flag {
    BITPUNCH_DATA_SOURCE_CACHED = (1u<<0),
    BITPUNCH_DATA_SOURCE_EXTERNAL = (1u<<1),
};

typedef int (*bitpunch_data_source_close_func_t)(
    struct bitpunch_data_source *ds);

struct bitpunch_data_source_backend {
    bitpunch_data_source_close_func_t close;
};

struct bitpunch_data_source {
    enum bitpunch_data_source_flag flags;
    struct bitpunch_data_source_backend backend;
    char              *ds_data;
    size_t            ds_data_length;
    int               use_count;
};

struct bitpunch_file_source {
    struct bitpunch_data_source ds; /* inherits */
    char      *path;
    int       fd;
    char      *map;
    size_t    map_length;
};

struct bitpunch_board {
    /** root node of the board, of type AST_NODE_TYPE_SCOPE_DEF */
    struct ast_node_hdl *ast_root;
    struct ast_node_hdl *used_spec;
    struct statement_list *extern_defs;
};

enum bitpunch_eval_flag {
    BITPUNCH_EVAL_DPATH_XOR_VALUE = (1<<0),
};

#endif
