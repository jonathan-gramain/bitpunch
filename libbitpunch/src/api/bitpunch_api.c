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
#include <assert.h>

#include "core/filter.h"
#include "api/bitpunch_api.h"
#include "api/data_source_internal.h"

#if defined DEBUG
int tracker_debug_mode = 0;
#endif

int
bitpunch_init(void)
{
    filter_class_declare_std();
    data_source_global_init();
    compile_global_nodes();
    return 0;
}

void
bitpunch_cleanup(void)
{
    data_source_global_destroy();
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
    case BITPUNCH_NO_DATA:
        return "no data input";
    default:
        return "unknown tracker status";
    }
    /*NOT REACHED*/
}
