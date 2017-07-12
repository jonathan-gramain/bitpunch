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

/**
 * @file
 * @brief main API
 */
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include "utils/queue.h"
#include "api/bitpunch-structs.h"
#include "api/bitpunch_api.h"
#include "core/parser.h"
#include "core/ast.h"
#include "core/interpreter.h"
#include "core/browse.h"

#if defined DEBUG
int tracker_debug_mode = 0;
#endif

int
bitpunch_init(void)
{
    interpreter_declare_std();
    return 0;
}

void
bitpunch_cleanup(void)
{
}

static struct bitpunch_schema_hdl *
bitpunch_schema_hdl_new(enum df_open_type open_type)
{
    struct bitpunch_schema_hdl *schema;

    schema = new_safe(struct bitpunch_schema_hdl);
    schema->df_open_type = open_type;
    switch (open_type) {
    case DF_OPEN_TYPE_FILEPATH:
    case DF_OPEN_TYPE_FILE_DESCRIPTOR:
        schema->df_open_data.filepath.fd = -1;
        break ;
    case DF_OPEN_TYPE_BUFFER:
        break ;
    default:
        assert(0);
    }
    return schema;
}

static int
load_schema_common(struct bitpunch_schema_hdl *schema)
{
    schema->df_fstream = fmemopen((char *)schema->df_data,
                                    schema->df_data_length, "r");
    if (NULL == schema->df_fstream) {
        fprintf(stderr,
                "error opening a stream on binary definition file\n");
        return -1;
    }
    if (-1 == bitpunch_parse_schema(schema)) {
        return -1;
    }
    if (-1 == resolve_schema_references(schema)) {
        return -1;
    }
    //dump_ast(schema->df_file_block.root, stdout);
    return 0;
}

static int
open_schema_from_fd(int fd,
                      struct bitpunch_schema_hdl *schema)
{
    char *map;
    size_t map_length;

    map_length = lseek(fd, 0, SEEK_END);
    map = mmap(NULL, map_length, PROT_READ, MAP_PRIVATE, fd, 0);
    if (NULL == map) {
        (void)close(fd);
        fprintf(stderr, "Unable to mmap binary definition file fd=\"%d\"\n",
                fd);
        return -1;
    }
    schema->df_open_data.filepath.fd = fd;
    schema->df_open_data.filepath.map = map;
    schema->df_open_data.filepath.map_length = map_length;
    schema->df_data = map;
    schema->df_data_length = map_length;
    return 0;
}

static int
open_schema_from_path(const char *path,
                        struct bitpunch_schema_hdl *schema)
{
    char *path_dup;
    int fd;

    path_dup = strdup_safe(path);
    fd = open(path, O_RDONLY);
    if (-1 == fd) {
        fprintf(stderr, "Unable to open binary definition file %s: "
                "open failed: %s\n",
                path, strerror(errno));
        free(path_dup);
        return -1;
    }
    if (-1 == open_schema_from_fd(fd, schema)) {
        fprintf(stderr, "Unable to open binary definition file %s\n",
                path);
        close(fd);
        free(path_dup);
        return -1;
    }
    schema->df_open_data.filepath.path = path_dup;
    return 0;
}

int
bitpunch_load_schema_from_path(const char *path,
                                struct bitpunch_schema_hdl **schemap)
{
    struct bitpunch_schema_hdl *schema;

    assert(NULL != path);
    assert(NULL != schemap);

    schema = bitpunch_schema_hdl_new(DF_OPEN_TYPE_FILEPATH);
    if (-1 == open_schema_from_path(path, schema)) {
        bitpunch_free_schema(schema);
        return -1;
    }
    if (-1 == load_schema_common(schema)) {
        bitpunch_free_schema(schema);
        return -1;
    }
    *schemap = schema;
    return 0;
}

int
bitpunch_load_schema_from_fd(int fd,
                              struct bitpunch_schema_hdl **schemap)
{
    struct bitpunch_schema_hdl *schema;

    assert(-1 != fd);
    assert(NULL != schemap);

    schema = bitpunch_schema_hdl_new(DF_OPEN_TYPE_FILE_DESCRIPTOR);
    if (-1 == open_schema_from_fd(fd, schema)) {
        bitpunch_free_schema(schema);
        return -1;
    }
    if (-1 == load_schema_common(schema)) {
        bitpunch_free_schema(schema);
        return -1;
    }
    *schemap = schema;
    return 0;
}

int
bitpunch_load_schema_from_buffer(const char *buf, size_t buf_size,
                                  struct bitpunch_schema_hdl **schemap)
{
    struct bitpunch_schema_hdl *schema;

    assert(NULL != schemap);

    schema = bitpunch_schema_hdl_new(DF_OPEN_TYPE_BUFFER);
    schema->df_data = buf;
    schema->df_data_length = buf_size;
    if (-1 == load_schema_common(schema)) {
        bitpunch_free_schema(schema);
        return -1;
    }
    *schemap = schema;
    return 0;
}

int
bitpunch_load_schema_from_string(const char *str,
                                  struct bitpunch_schema_hdl **schemap)
{
    return bitpunch_load_schema_from_buffer(str, strlen(str),
                                             schemap);
}

void
bitpunch_close_schema(struct bitpunch_schema_hdl *schema)
{
    if (NULL == schema) {
        return ;
    }
    switch (schema->df_open_type) {
    case DF_OPEN_TYPE_UNSET:
        break ;
    case DF_OPEN_TYPE_FILEPATH:
        if (NULL != schema->df_fstream) {
            fclose(schema->df_fstream);
        }
        if (NULL != schema->df_open_data.filepath.map) {
            (void)munmap(schema->df_open_data.filepath.map,
                         schema->df_open_data.filepath.map_length);
        }
        if (-1 != schema->df_open_data.filepath.fd) {
            (void)close(schema->df_open_data.filepath.fd);
        }
        free(schema->df_open_data.filepath.path);
        break ;
    case DF_OPEN_TYPE_FILE_DESCRIPTOR:
        if (NULL != schema->df_fstream) {
            fclose(schema->df_fstream);
        }
        if (NULL != schema->df_open_data.filepath.map) {
            (void)munmap(schema->df_open_data.filepath.map,
                         schema->df_open_data.filepath.map_length);
        }
        break ;
    case DF_OPEN_TYPE_BUFFER:
        if (NULL != schema->df_fstream) {
            fclose(schema->df_fstream);
        }
        break ;
    default:
        assert(0);
    }
    schema->df_open_type = DF_OPEN_TYPE_UNSET;
}

void
bitpunch_free_schema(struct bitpunch_schema_hdl *schema)
{
    bitpunch_close_schema(schema);
    free(schema);
}

static int
open_binary_file_from_fd(int fd,
                         struct bitpunch_binary_file_hdl *bf)
{
    char *map;
    size_t map_length;

    map_length = lseek(fd, 0, SEEK_END);
    map = mmap(NULL, map_length, PROT_READ, MAP_PRIVATE, fd, 0);
    if (NULL == map) {
        fprintf(stderr, "Unable to mmap binary file: %s\n",
                strerror(errno));
        return -1;
    }
    bf->bf_open_data.filepath.fd = fd;
    bf->bf_open_data.filepath.map = map;
    bf->bf_open_data.filepath.map_length = map_length;

    bf->bf_data = map;
    bf->bf_data_length = map_length;

    return 0;
}

int
bitpunch_load_binary_file_from_path(const char *path,
                                   struct bitpunch_binary_file_hdl **binary_filep)
{
    struct bitpunch_binary_file_hdl *bf;
    int fd;

    assert(NULL != path);
    assert(NULL != binary_filep);

    fd = open(path, O_RDONLY);
    if (-1 == fd) {
        fprintf(stderr, "Unable to open binary file %s: open failed: %s\n",
                path, strerror(errno));
        return -1;
    }
    bf = new_safe(struct bitpunch_binary_file_hdl);
    bf->bf_open_type = BF_OPEN_TYPE_FILEPATH;

    if (-1 == open_binary_file_from_fd(fd, bf)) {
        fprintf(stderr, "Error loading binary file %s\n", path);
        (void)close(fd);
        free(bf);
        return -1;
    }
    bf->bf_open_data.filepath.path = strdup_safe(path);

    *binary_filep = bf;
    return 0;
}

int
bitpunch_load_binary_file_from_fd(int fd,
                                 struct bitpunch_binary_file_hdl **binary_filep)
{
    struct bitpunch_binary_file_hdl *bf;

    assert(-1 != fd);
    assert(NULL != binary_filep);

    bf = new_safe(struct bitpunch_binary_file_hdl);
    bf->bf_open_type = BF_OPEN_TYPE_FILE_DESCRIPTOR;

    if (-1 == open_binary_file_from_fd(fd, bf)) {
        fprintf(stderr, "Error loading binary file from file descriptor fd=%d\n",
                fd);
        free(bf);
        return -1;
    }
    *binary_filep = bf;
    return 0;
}

int
bitpunch_load_binary_file_from_buffer(const char *data, size_t data_size,
                                     struct bitpunch_binary_file_hdl **binary_filep)
{
    struct bitpunch_binary_file_hdl *bf;

    assert(NULL != binary_filep);

    bf = new_safe(struct bitpunch_binary_file_hdl);
    bf->bf_open_type = BF_OPEN_TYPE_USER_BUFFER;

    bf->bf_data = data;
    bf->bf_data_length = data_size;

    *binary_filep = bf;
    return 0;
}

int
bitpunch_close_binary_file(struct bitpunch_binary_file_hdl *bf)
{
    if (NULL == bf) {
        return 0;
    }
    switch (bf->bf_open_type) {
    case BF_OPEN_TYPE_UNSET:
        break ;
    case BF_OPEN_TYPE_FILEPATH:
        if (-1 == munmap(bf->bf_open_data.filepath.map,
                         bf->bf_open_data.filepath.map_length)) {
            return -1;
        }
        if (-1 == close(bf->bf_open_data.filepath.fd)) {
            return -1;
        }
        free(bf->bf_open_data.filepath.path);
        break ;
    case BF_OPEN_TYPE_FILE_DESCRIPTOR:
        if (-1 == munmap(bf->bf_open_data.filepath.map,
                         bf->bf_open_data.filepath.map_length)) {
            return -1;
        }
        break ;
    case BF_OPEN_TYPE_USER_BUFFER:
        break ;
    case BF_OPEN_TYPE_OWN_BUFFER:
        free((char *)bf->bf_data);
        break ;
    default:
        assert(0);
    }
    bf->bf_open_type = BF_OPEN_TYPE_UNSET;
    return 0;
}

int
bitpunch_free_binary_file(struct bitpunch_binary_file_hdl *bf)
{
    if (-1 == bitpunch_close_binary_file(bf))
        return -1;
    if (NULL != bf->box_cache) {
        box_cache_free(bf->box_cache);
    }
    free(bf);
    return 0;
}

int
bitpunch_eval_expr(struct bitpunch_schema_hdl *schema,
                   struct bitpunch_binary_file_hdl *binary_file,
                   const char *expr,
                   struct box *scope,
                   enum expr_value_type *expr_value_typep,
                   union expr_value *expr_valuep,
                   enum expr_dpath_type *expr_dpath_typep,
                   union expr_dpath *expr_dpathp)
{
    struct ast_node *expr_node = NULL;
    struct parser_ctx *parser_ctx = NULL;
    union expr_value expr_value;
    union expr_dpath expr_dpath;
    const struct ast_node *scope_node;

    assert(NULL != expr);

    if (-1 == bitpunch_parse_expr(expr, &expr_node, &parser_ctx)) {
        return -1;
    }
    if (NULL != schema && NULL != binary_file) {
        if (NULL == scope) {
            scope = box_new_from_file(schema, binary_file);
            if (NULL == scope) {
                goto err;
            }
            scope_node = schema->df_file_block.root;
        } else {
            box_acquire(scope);
            scope_node = scope->node;
        }
    } else {
        scope = NULL; // just in case
        scope_node = NULL;
    }
    if (-1 == resolve_user_expr(&expr_node, scope_node)) {
        goto err;
    }
    assert(ast_node_is_rexpr(expr_node));
    if (NULL != expr_dpathp
        && EXPR_DPATH_TYPE_NONE != expr_node->u.rexpr.dpath_type
        && BITPUNCH_OK != expr_evaluate_dpath(expr_node, scope,
                                              &expr_dpath)) {
        goto err;
    }
    if (NULL != expr_valuep
        && EXPR_VALUE_TYPE_UNSET != expr_node->u.rexpr.value_type
        && BITPUNCH_OK != expr_evaluate_value(expr_node, scope,
                                              &expr_value)) {
        goto err;
    }
    box_delete(scope);
    if (NULL != expr_value_typep) {
        *expr_value_typep = expr_node->u.rexpr.value_type;
    }
    if (NULL != expr_valuep) {
        *expr_valuep = expr_value;
    }
    if (NULL != expr_dpath_typep) {
        *expr_dpath_typep = expr_node->u.rexpr.dpath_type;
    }
    if (NULL != expr_dpathp) {
        *expr_dpathp = expr_dpath;
    }
    /* TODO free expr_node */
    free(parser_ctx);
    return 0;

  err:
    box_delete(scope);
    /* TODO free expr_node */
    free(parser_ctx);
    return -1;
}


const char *
bitpunch_status_pretty(bitpunch_status_t bt_ret)
{
    switch (bt_ret) {
    case BITPUNCH_OK:
        return "success";
    case BITPUNCH_ERROR:
        return "error";
    case BITPUNCH_INVALID_PARAM:
        return "invalid parameter";
    case BITPUNCH_INVALID_STATE:
        return "invalid state";
    case BITPUNCH_NO_ITEM:
        return "no item";
    case BITPUNCH_NOT_CONTAINER:
        return "not a container";
    case BITPUNCH_DATA_ERROR:
        return "data error";
    case BITPUNCH_OUT_OF_BOUNDS_ERROR:
        return "out of data structure boundaries";
    case BITPUNCH_NOT_IMPLEMENTED:
        return "not implemented";
    default:
        return "unknown tracker status";
    }
    /*NOT REACHED*/
}
