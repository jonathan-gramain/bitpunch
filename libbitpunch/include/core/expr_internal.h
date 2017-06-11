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

#ifndef __EXPR_INTERNAL_H__
#define __EXPR_INTERNAL_H__

#include "core/expr.h"

bitpunch_status_t
expr_evaluate_value_internal(struct ast_node *expr, struct box *scope,
                             union expr_value *eval_valuep,
                             struct browse_state *bst);
bitpunch_status_t
expr_evaluate_dpath_internal(struct ast_node *expr, struct box *scope,
                             union expr_dpath *eval_dpathp,
                             struct browse_state *bst);
bitpunch_status_t
link_evaluate_dpath_internal(const struct link *link, struct box *scope,
                             enum expr_dpath_type *dpath_typep,
                             union expr_dpath *eval_dpathp,
                             struct browse_state *bst);
bitpunch_status_t
link_evaluate_value_internal(const struct link *link, struct box *scope,
                             enum expr_value_type *value_typep,
                             union expr_value *eval_valuep,
                             struct browse_state *bst);
bitpunch_status_t
evaluate_conditional_internal(struct ast_node *cond, struct box *scope,
                              int *evalp, struct browse_state *bst);

#endif /* __EXPR_INTERNAL_H__ */
