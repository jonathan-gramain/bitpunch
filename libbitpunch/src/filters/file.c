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

#define _DEFAULT_SOURCE
#define _GNU_SOURCE
#include <sys/types.h>
#include <assert.h>
#include <string.h>

#include "api/bitpunch_api.h"
#include "core/filter.h"

static bitpunch_status_t
file_get_data_source(
    struct ast_node_hdl *filter,
    struct box *scope,
    struct bitpunch_data_source **ds_outp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    expr_value_t path_value;
    int ret;
    char file_path[1024];

    bt_ret = filter_evaluate_attribute_internal(
        filter, scope, "@path", NULL, &path_value, NULL, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (path_value.string.len >= sizeof (file_path)) {
        return box_error(BITPUNCH_DATA_ERROR, scope, filter, bst,
                         "file path \"%.20s\" is too long (%"PRIi64" "
                         "characters, max is %zu)",
                         path_value.string.str,
                         path_value.string.len, sizeof (file_path) - 1);
    }
    memcpy(file_path, path_value.string.str, path_value.string.len);
    file_path[path_value.string.len] = '\0';
    ret = bitpunch_data_source_create_from_file_path(ds_outp, file_path);
    expr_value_destroy(path_value);
    if (-1 == ret) {
        return BITPUNCH_DATA_ERROR;
    }
    return BITPUNCH_OK;
}

static struct filter_instance *
file_filter_instance_build(struct ast_node_hdl *filter)
{
    struct filter_instance *f_instance;

    f_instance = new_safe(struct filter_instance);
    f_instance->get_data_source_func = file_get_data_source;
    return f_instance;
}

void
filter_class_declare_file(void)
{
    int ret;

    ret = filter_class_declare("_file_",
                               EXPR_VALUE_TYPE_BYTES,
                               file_filter_instance_build, NULL,
                               1,
                               "@path", EXPR_VALUE_TYPE_STRING,
                               FILTER_ATTR_FLAG_MANDATORY);
    assert(0 == ret);
}
