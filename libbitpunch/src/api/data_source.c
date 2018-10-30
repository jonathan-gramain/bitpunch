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
#include "api/bitpunch_api.h"

static int
data_source_free(struct bitpunch_data_source *ds);

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
remove_cached_file_source(const char *path)
{
    struct cached_file_source *cfs, *tcfs;

    LIST_FOREACH_SAFE(cfs, &cached_file_sources, list, tcfs) {
        if (NULL != cfs->fs->path && 0 == strcmp(cfs->fs->path, path)) {
            LIST_REMOVE(cfs, list);
            data_source_free((struct bitpunch_data_source *)cfs->fs);
            free(cfs);
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
open_data_source_from_fd(struct bitpunch_file_source *fs, int fd)
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
bitpunch_data_source_create_from_file_path(
    struct bitpunch_data_source **dsp, const char *path)
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

        if (-1 == open_data_source_from_fd(fs, fd)) {
            fprintf(stderr, "Error loading binary file %s\n", path);
            (void)close(fd);
            free(fs);
            return -1;
        }
        fs->path = strdup_safe(path);
        add_file_source_to_cache(fs);
    }
    *dsp = &fs->ds;
    return 0;
}

void
bitpunch_data_source_notify_file_change(const char *path)
{
    (void) remove_cached_file_source(path);
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
    struct bitpunch_data_source **dsp, int fd)
{
    struct bitpunch_file_source *fs;

    assert(-1 != fd);
    assert(NULL != dsp);

    fs = new_safe(struct bitpunch_file_source);
    fs->ds.backend.close = data_source_close_file_descriptor;

    if (-1 == open_data_source_from_fd(fs, fd)) {
        fprintf(stderr,
                "Error loading binary file from file descriptor fd=%d\n",
                fd);
        free(fs);
        return -1;
    }
    *dsp = &fs->ds;
    return 0;
}

static int
data_source_close_managed_memory(struct bitpunch_data_source *ds)
{
    free((char *)ds->ds_data);
    return 0;
}

int
bitpunch_data_source_create_from_memory(
    struct bitpunch_data_source **dsp,
    const char *data, size_t data_size, int manage_buffer)
{
    struct bitpunch_data_source *ds;

    ds = new_safe(struct bitpunch_data_source);
    if (manage_buffer) {
        ds->backend.close = data_source_close_managed_memory;
    }
    ds->ds_data = data;
    ds->ds_data_length = data_size;
    *dsp = ds;
    return 0;
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
    if (NULL != ds->box_cache) {
        box_cache_free(ds->box_cache);
    }
    free(ds);
    return ret;
}

int
bitpunch_data_source_release(struct bitpunch_data_source *ds)
{
    if (NULL == ds) {
        return 0;
    }
    if (0 == (ds->flags & BITPUNCH_DATA_SOURCE_CACHED)) {
        return data_source_free(ds);
    }
    // we could implement cleanup for less-used cache entries here
    return 0;
}
