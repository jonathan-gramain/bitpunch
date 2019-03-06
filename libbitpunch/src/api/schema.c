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
 * @brief bitpunch schema API
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <errno.h>

#include "api/bitpunch_api.h"
#include "core/filter.h"
#include "core/scope.h"

static int
load_schema_common(struct parser_ctx *parser_ctx, struct ast_node_hdl **astp)
{
    if (-1 == bitpunch_parse(parser_ctx, astp)) {
        return -1;
    }
    if (-1 == bitpunch_compile_schema(*astp)) {
        return -1;
    }
    return 0;
}

static struct parser_ctx *
create_parser_ctx_from_fd(int fd)
{
    char *buffer = NULL;
    ssize_t n_read;
    off_t cur_offset;
    struct parser_ctx *parser_ctx;
    char error_buf[256];

    parser_ctx = new_safe(struct parser_ctx);
    error_buf[0] = '\0';
    buffer = malloc_safe(BITPUNCH_SCHEMA_MAX_LENGTH);
    cur_offset = 0;
    while (cur_offset < BITPUNCH_SCHEMA_MAX_LENGTH) {
        n_read = pread(fd, buffer + cur_offset,
                       BITPUNCH_SCHEMA_MAX_LENGTH - cur_offset,
                       cur_offset);
        if (-1 == n_read) {
            break ;
        }
        if (0 == n_read) {
            buffer = realloc_safe(buffer, cur_offset);
            parser_ctx->parser_data = buffer;
            parser_ctx->parser_data_length = cur_offset;
            parser_ctx->start_token = START_SCHEMA;
            return parser_ctx;
        }
        cur_offset += n_read;
    }
    if (cur_offset == BITPUNCH_SCHEMA_MAX_LENGTH) {
        snprintf(error_buf, sizeof error_buf,
                 "file too large: maximum is %d bytes",
                 BITPUNCH_SCHEMA_MAX_LENGTH);
    } else {
        strerror_r(errno, error_buf, sizeof error_buf);
    }
    fprintf(stderr, "error reading bitpunch spec from file descriptor %d: %s\n",
            fd, error_buf);
    free(buffer);
    return NULL;
}

static struct parser_ctx *
create_parser_ctx_from_path(const char *path)
{
    int fd;
    struct parser_ctx *parser_ctx;

    fd = open(path, O_RDONLY);
    if (-1 == fd) {
        char error_buf[256];

        error_buf[0] = '\0';
        strerror_r(errno, error_buf, sizeof error_buf);
        fprintf(stderr, "error reading bitpunch spec from file \"%s\": %s\n",
                path, error_buf);
        return NULL;
    }
    parser_ctx = create_parser_ctx_from_fd(fd);
    if (NULL == parser_ctx) {
        close(fd);
        return NULL;
    }
    parser_ctx->parser_filepath = strdup_safe(path);
    return parser_ctx;
}

int
bitpunch_schema_create_from_path(
    struct ast_node_hdl **schemap, const char *path)
{
    struct parser_ctx *parser_ctx;

    assert(NULL != path);
    assert(NULL != schemap);

    parser_ctx = create_parser_ctx_from_path(path);
    if (NULL == parser_ctx) {
        return -1;
    }
    return load_schema_common(parser_ctx, schemap);
}

int
bitpunch_schema_create_from_file_descriptor(
    struct ast_node_hdl **schemap, int fd)
{
    struct parser_ctx *parser_ctx;

    assert(-1 != fd);
    assert(NULL != schemap);

    parser_ctx = create_parser_ctx_from_fd(fd);
    if (NULL == parser_ctx) {
        return -1;
    }
    return load_schema_common(parser_ctx, schemap);
}

int
bitpunch_schema_create_from_buffer(
    struct ast_node_hdl **schemap, const char *buf, size_t buf_size)
{
    struct parser_ctx *parser_ctx;

    assert(NULL != schemap);
    assert(NULL != buf);

    parser_ctx = new_safe(struct parser_ctx);
    parser_ctx->parser_data = memcpy(malloc_safe(buf_size), buf, buf_size);
    parser_ctx->parser_data_length = buf_size;
    parser_ctx->start_token = START_SCHEMA;
    return load_schema_common(parser_ctx, schemap);
}

int
bitpunch_schema_create_from_string(
    struct ast_node_hdl **schemap, const char *str)
{
    return bitpunch_schema_create_from_buffer(schemap, str, strlen(str));
}

void
bitpunch_schema_free(
    struct ast_node_hdl *schema)
{
    free(schema);
}
