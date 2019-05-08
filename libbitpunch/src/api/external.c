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
 * @brief bitpunch external plugin API
 */

#include <stdarg.h>

#include "core/filter.h"
#include "api/bitpunch_api.h"

int
bitpunch_external_create_function(
    struct ast_node_hdl **nodep,
    extern_func_fn_t extern_func_fn,
    void *user_arg)
{
    struct ast_node_hdl *func_hdl;

    func_hdl = ast_node_hdl_create(AST_NODE_TYPE_EXTERN_FUNC, NULL);
    func_hdl->ndat->u.extern_func.extern_func_fn = extern_func_fn;
    func_hdl->ndat->u.extern_func.user_arg = user_arg;
    func_hdl->flags |= ASTFLAG_EXTERNAL;
    *nodep = func_hdl;
    return 0;
}

int
bitpunch_external_create_filter(
    struct ast_node_hdl **nodep,
    filter_instance_build_func_t build_func,
    filter_instance_compile_func_t compile_func,
    void *user_arg)
{
    struct ast_node_hdl *filter_hdl;

    filter_hdl = ast_node_hdl_create(AST_NODE_TYPE_EXTERN_FILTER, NULL);
    filter_hdl->flags |= ASTFLAG_EXTERNAL;
    filter_hdl->ndat->u.extern_filter.build_func = build_func;
    filter_hdl->ndat->u.extern_filter.compile_func = compile_func;
    filter_hdl->ndat->u.extern_filter.user_arg = user_arg;
    *nodep = filter_hdl;
    return 0;
}
