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

struct box_cache;

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
    BITPUNCH_STATUS_LAST = -8,
} bitpunch_status_t;

struct parser_ctx {
    const char *parser_filepath;
    const char *parser_data;
    size_t      parser_data_length;
    int         start_token;
};

struct file_block {
    struct dpath_node *root;
};

enum df_open_type {
    DF_OPEN_TYPE_UNSET = 0,
    DF_OPEN_TYPE_FILEPATH,
    DF_OPEN_TYPE_FILE_DESCRIPTOR,
    DF_OPEN_TYPE_BUFFER,
};

struct bitpunch_schema_hdl {
    enum df_open_type df_open_type;
    union {
        struct {
            char      *path;
            int       fd;
            char      *map;
            size_t    map_length;
        }             filepath;
        struct {
        }             buffer;
    }                 df_open_data;
    const char        *df_data;
    size_t            df_data_length;
    FILE              *df_fstream;
    struct parser_ctx df_parser_ctx;
    struct file_block df_file_block;
};

enum ds_open_type {
    DS_OPEN_TYPE_UNSET = 0,
    DS_OPEN_TYPE_FILEPATH,
    DS_OPEN_TYPE_FILE_DESCRIPTOR,
    DS_OPEN_TYPE_USER_BUFFER,
    DS_OPEN_TYPE_OWN_BUFFER,
};

struct bitpunch_data_source {
    enum ds_open_type ds_open_type;
    union {
        struct {
            char      *path;
            int       fd;
            char      *map;
            size_t    map_length;
        }             filepath;
        struct {
        }             buffer;
    }                 ds_open_data;
    const char        *ds_data;
    size_t            ds_data_length;
    struct box_cache  *box_cache;
};

#endif
