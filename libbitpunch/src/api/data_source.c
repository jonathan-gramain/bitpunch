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
 * @brief bitpunch data source API
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <errno.h>
#include <string.h>

#include "utils/queue.h"
#include "core/browse.h"
#include "core/filter.h"
#include "api/bitpunch_api.h"
#include "filters/data_source.h"

static int
data_source_free(struct bitpunch_data_source *ds);

static int
file_source_refresh_internal(struct bitpunch_file_source *fs);

struct cached_file_source {
    LIST_ENTRY(cached_file_source) list;
    struct bitpunch_file_source *fs;
};

LIST_HEAD(cached_file_source_list, cached_file_source) cached_file_sources;

void
data_source_global_init(void)
{
    LIST_INIT(&cached_file_sources);
}

void
data_source_global_destroy(void)
{
    struct cached_file_source *cfs, *tcfs;

    LIST_FOREACH_SAFE(cfs, &cached_file_sources, list, tcfs) {
        data_source_free((struct bitpunch_data_source *)cfs->fs);
        free(cfs);
    }
}

static struct bitpunch_file_source *
lookup_cached_file_source(const char *path)
{
    struct cached_file_source *cfs, *tcfs;

    LIST_FOREACH_SAFE(cfs, &cached_file_sources, list, tcfs) {
        if (NULL != cfs->fs->path && 0 == strcmp(cfs->fs->path, path)) {
            LIST_REMOVE(cfs, list);
            LIST_INSERT_HEAD(&cached_file_sources, cfs, list);
            return cfs->fs;
        }
    }
    return NULL;
}

static int
refresh_cached_file_source(const char *path)
{
    struct cached_file_source *cfs, *tcfs;

    LIST_FOREACH_SAFE(cfs, &cached_file_sources, list, tcfs) {
        if (NULL != cfs->fs->path && 0 == strcmp(cfs->fs->path, path)) {
            file_source_refresh_internal(cfs->fs);
            return 0;
        }
    }
    return -1;
}

static void
add_file_source_to_cache(struct bitpunch_file_source *fs)
{
    struct cached_file_source *cfs;

    cfs = new_safe(struct cached_file_source);
    cfs->fs = fs;
    cfs->fs->ds.flags = BITPUNCH_DATA_SOURCE_CACHED;
    LIST_INSERT_HEAD(&cached_file_sources, cfs, list);
}

static int
open_file_source_from_fd(struct bitpunch_file_source *fs, int fd)
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
    fs->fd = fd;
    fs->map = map;
    fs->map_length = map_length;

    fs->ds.ds_data = map;
    fs->ds.ds_data_length = map_length;

    return 0;
}

static int
open_file_source_from_file_path(
    struct bitpunch_file_source *fs, const char *path)
{
    int fd;

    assert(NULL != path);

    fd = open(path, O_RDONLY);
    if (-1 == fd) {
        fprintf(stderr, "Unable to open binary file %s: open failed: %s\n",
                path, strerror(errno));
        return -1;
    }
    if (-1 == open_file_source_from_fd(fs, fd)) {
        (void)close(fd);
        return -1;
    }
    return 0;
}

static int
data_source_close_file_path(struct bitpunch_data_source *ds)
{
    struct bitpunch_file_source *fs;

    fs = (struct bitpunch_file_source *)ds;
    if (-1 == munmap(fs->map, fs->map_length)) {
        return -1;
    }
    if (-1 == close(fs->fd)) {
        return -1;
    }
    free(fs->path);
    return 0;
}

int
data_source_create_from_file_path_internal(
    struct bitpunch_data_source **dsp, const char *path, int external)
{
    struct bitpunch_file_source *fs;
    int fd;

    assert(NULL != path);
    assert(NULL != dsp);

    fs = lookup_cached_file_source(path);
    if (NULL == fs) {
        fd = open(path, O_RDONLY);
        if (-1 == fd) {
            fprintf(stderr, "Unable to open binary file %s: open failed: %s\n",
                    path, strerror(errno));
            return -1;
        }
        fs = new_safe(struct bitpunch_file_source);
        fs->ds.backend.close = data_source_close_file_path;
        if (-1 == open_file_source_from_file_path(fs, path)) {
            fprintf(stderr, "Error loading binary file %s\n", path);
            free(fs);
            return -1;
        }
        fs->path = strdup_safe(path);
        add_file_source_to_cache(fs);
        if (external) {
            fs->ds.flags |= BITPUNCH_DATA_SOURCE_EXTERNAL;
        }
    }
    *dsp = &fs->ds;
    return 0;
}

int
bitpunch_data_source_create_from_file_path(
    struct ast_node_hdl **dsp, const char *path)
{
    int ret;
    struct bitpunch_data_source *ds;

    ret = data_source_create_from_file_path_internal(&ds, path, TRUE);
    if (0 == ret) {
        *dsp = ast_node_hdl_create_data_source(ds);
    }
    return ret;
}

void
bitpunch_data_source_notify_file_change(const char *path)
{
    (void) refresh_cached_file_source(path);
}

static int
data_source_close_file_descriptor(struct bitpunch_data_source *ds)
{
    struct bitpunch_file_source *fs;

    fs = (struct bitpunch_file_source *)ds;
    return munmap(fs->map, fs->map_length);
}

int
bitpunch_data_source_create_from_file_descriptor(
    struct ast_node_hdl **dsp, int fd)
{
    struct bitpunch_file_source *fs;

    assert(-1 != fd);
    assert(NULL != dsp);

    fs = new_safe(struct bitpunch_file_source);
    fs->ds.backend.close = data_source_close_file_descriptor;
    fs->ds.flags = BITPUNCH_DATA_SOURCE_EXTERNAL;

    if (-1 == open_file_source_from_fd(fs, fd)) {
        fprintf(stderr,
                "Error loading binary file from file descriptor fd=%d\n",
                fd);
        free(fs);
        return -1;
    }
    *dsp = ast_node_hdl_create_data_source(&fs->ds);
    return 0;
}

static int
data_source_close_managed_memory(struct bitpunch_data_source *ds)
{
    free((char *)ds->ds_data);
    return 0;
}

int
data_source_create_from_memory_internal(
    struct bitpunch_data_source **dsp,
    const char *data, size_t data_size, int manage_buffer)
{
    struct bitpunch_data_source *ds;

    ds = new_safe(struct bitpunch_data_source);
    if (manage_buffer) {
        ds->backend.close = data_source_close_managed_memory;
    } else {
        ds->flags = BITPUNCH_DATA_SOURCE_EXTERNAL;
    }
    ds->ds_data = data;
    ds->ds_data_length = data_size;
    *dsp = ds;
    return 0;
}

int
bitpunch_data_source_create_from_memory(
    struct ast_node_hdl **dsp,
    const char *data, size_t data_size, int manage_buffer)
{
    struct bitpunch_data_source *ds;
    int ret;

    ret = data_source_create_from_memory_internal(
        &ds, data, data_size, manage_buffer);
    if (0 == ret) {
        *dsp = ast_node_hdl_create_data_source(ds);
    }
    return ret;
}

static int
data_source_close(struct bitpunch_data_source *ds)
{
    int ret;

    if (NULL == ds || NULL == ds->backend.close) {
        return 0;
    }
    ret = ds->backend.close(ds);
    ds->ds_data = NULL;
    ds->ds_data_length = 0;
    return ret;
}

static int
data_source_free(struct bitpunch_data_source *ds)
{
    int ret;

    ret = data_source_close(ds);
    free(ds);
    return ret;
}

int
data_source_release_internal(struct bitpunch_data_source *ds)
{
    if (0 == (ds->flags & (BITPUNCH_DATA_SOURCE_CACHED |
                           BITPUNCH_DATA_SOURCE_EXTERNAL))) {
        return data_source_free(ds);
    }
    // we could implement cleanup for less-used cache entries here
    return 0;
}

int
bitpunch_data_source_release(struct ast_node_hdl *ds_node)
{
    bitpunch_status_t bt_ret;
    struct bitpunch_data_source *ds;

    if (NULL == ds_node) {
        return 0;
    }
    bt_ret = filter_instance_get_data_source(ds_node, NULL, &ds, NULL);
    if (BITPUNCH_OK != bt_ret) {
        return -1;
    }
    return data_source_release_internal(ds);
}

static int
file_source_refresh_internal(struct bitpunch_file_source *fs)
{
    int ret;
    char *path;

    path = strdup(fs->path);
    ret = data_source_close(&fs->ds);
    if (-1 == ret) {
        return -1;
    }
    return open_file_source_from_file_path(fs, path);
    free(path);
}
