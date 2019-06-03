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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stddef.h>
#include <assert.h>

#include "core/debug.h"
#include "core/filter.h"
#include "core/browse_internal.h"
#include "core/expr_internal.h"
#include "filters/composite.h"
#include "filters/byte.h"
#include "filters/array.h"

#define MAX_BUILTIN_FILTER_COUNT           256

struct filter_class builtin_filter_class_table[MAX_BUILTIN_FILTER_COUNT];
int                 builtin_filter_class_count = 0;


struct filter_instance_extern {
    struct filter_instance f_instance; /* inherits */
    /** inclusive list of all instances bound to the same external filter */
    SLIST_ENTRY(filter_instance_extern) instance_list_entry;
};

static struct filter_class *
builtin_filter_class_new(void)
{
    if (builtin_filter_class_count == MAX_BUILTIN_FILTER_COUNT) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, NULL,
                       "Too many builtin filters (max %d)\n",
                         MAX_BUILTIN_FILTER_COUNT);
        return NULL;
    }
    return &builtin_filter_class_table[builtin_filter_class_count++];
}

struct filter_class *
filter_class_new(void *user_arg)
{
    struct filter_class *filter_cls;

    filter_cls = new_safe(struct filter_class);
    filter_cls->user_arg = user_arg;
    return filter_cls;
}

void
filter_class_free(struct filter_class *filter_cls)
{
    free(filter_cls);
}

struct filter_attr_def *
filter_attr_def_new(void)
{
    return new_safe(struct filter_attr_def);
}


int
filter_class_construct_internal(
    struct filter_class *filter_cls,
    const char *name,
    enum expr_value_type value_type_mask,
    filter_instance_build_func_t filter_instance_build_func,
    filter_instance_compile_func_t filter_instance_compile_func,
    filter_class_generate_func_t filter_class_generate_func,
    enum filter_class_flag flags,
    int n_attrs,
    va_list ap)
{
    int i;
    struct filter_attr_def *attr_def;

    filter_cls->name = name;
    filter_cls->value_type_mask = value_type_mask;
    filter_cls->filter_instance_build_func = filter_instance_build_func;
    filter_cls->filter_instance_compile_func = filter_instance_compile_func;
    filter_cls->filter_class_generate_func = filter_class_generate_func;
    filter_cls->flags = flags;
    filter_cls->n_attrs = n_attrs;
    STAILQ_INIT(&filter_cls->attr_list);
    for (i = 0; i < n_attrs; ++i) {
        attr_def = filter_attr_def_new();
        attr_def->name = va_arg(ap, const char *);
        attr_def->value_type_mask = va_arg(ap, enum ast_node_type);
        attr_def->flags = va_arg(ap, enum filter_attr_flag);
        STAILQ_INSERT_TAIL(&filter_cls->attr_list, attr_def, list);
    }
    return 0;
}

int
builtin_filter_declare(
    const char *name,
    enum expr_value_type value_type_mask,
    filter_instance_build_func_t filter_instance_build_func,
    filter_instance_compile_func_t filter_instance_compile_func,
    enum filter_class_flag flags,
    int n_attrs,
    ... /* attrs: (name, type, flags) tuples */)
{
    struct filter_class *filter_cls;
    va_list ap;
    int ret;

    filter_cls = builtin_filter_class_new();
    if (NULL == filter_cls) {
        return -1;
    }
    va_start(ap, n_attrs);
    ret = filter_class_construct_internal(
        filter_cls,
        name, value_type_mask,
        filter_instance_build_func,
        filter_instance_compile_func,
        NULL,
        flags, n_attrs, ap);
    va_end(ap);
    return ret;
}

int
builtin_filter_declare_generator(
    const char *name,
    filter_class_generate_func_t filter_class_generate_func,
    int n_attrs,
    ... /* attrs: (name, type, flags) tuples */)
{
    struct filter_class *filter_cls;
    va_list ap;
    int ret;

    filter_cls = builtin_filter_class_new();
    if (NULL == filter_cls) {
        return -1;
    }
    va_start(ap, n_attrs);
    ret = filter_class_construct_internal(
        filter_cls,
        name, EXPR_VALUE_TYPE_UNSET,
        NULL, NULL, filter_class_generate_func,
        0u, n_attrs, ap);
    va_end(ap);
    return ret;
}

struct filter_class *
builtin_filter_lookup(const char *name)
{
    int i;

    for (i = 0; i < builtin_filter_class_count; ++i) {
        if (0 == strcmp(builtin_filter_class_table[i].name, name)) {
            return &builtin_filter_class_table[i];
        }
    }
    return NULL;
}

void builtin_filter_declare_data_source(void);
void builtin_filter_declare_file(void);
void builtin_filter_declare_extern(void);

void builtin_filter_declare_byte(void);
void builtin_filter_declare_struct(void);
void builtin_filter_declare_union(void);
void builtin_filter_declare_array(void);
void builtin_filter_declare_binary_integer(void);
void builtin_filter_declare_bytes(void);
void builtin_filter_declare_string(void);
void builtin_filter_declare_varint(void);
void builtin_filter_declare_base64(void);
void builtin_filter_declare_deflate(void);
void builtin_filter_declare_snappy(void);
void builtin_filter_declare_formatted_integer(void);

void
builtin_filter_declare_std(void)
{
    builtin_filter_declare_data_source();
    builtin_filter_declare_file();
    builtin_filter_declare_extern();

    builtin_filter_declare_byte();
    builtin_filter_declare_struct();
    builtin_filter_declare_union();
    builtin_filter_declare_array();
    builtin_filter_declare_binary_integer();
    builtin_filter_declare_bytes();
    builtin_filter_declare_string();
    builtin_filter_declare_varint();
    builtin_filter_declare_base64();
    builtin_filter_declare_deflate();
    builtin_filter_declare_snappy();
    builtin_filter_declare_formatted_integer();
}

struct filter_def *
filter_def_create_empty(const char *filter_type)
{
    struct filter_def *filter_def;

    filter_def = new_safe(struct filter_def);
    filter_def->filter_type = filter_type;
    init_block_stmt_list(&filter_def->scope_def.block_stmt_list);
    return filter_def;
}

int
filter_instance_build(struct ast_node_hdl *node,
                      const struct filter_class *filter_cls,
                      struct filter_def *filter_def)
{
    struct ast_node_data *ndat;
    struct filter_instance *f_instance;

    assert(NULL != filter_cls->filter_instance_build_func);

    ndat = new_safe(struct ast_node_data);
    // template flag may be removed when compiling OP_FILTER
    ndat->flags |= ASTFLAG_DATA_TEMPLATE;
    ndat->u.item.min_span_size = SPAN_SIZE_UNDEF;
    if (0 == strcmp(filter_def->filter_type, "array")) {
        ndat->type = AST_NODE_TYPE_ARRAY;
    } else if (0 == strcmp(filter_def->filter_type, "byte")) {
        ndat->type = AST_NODE_TYPE_BYTE;
    } else {
        ndat->type = AST_NODE_TYPE_REXPR_FILTER;
    }
    ndat->u.rexpr.value_type_mask = filter_cls->value_type_mask;
    ndat->u.rexpr.dpath_type_mask = EXPR_DPATH_TYPE_UNSET;
    if (0 != (filter_cls->value_type_mask & (EXPR_VALUE_TYPE_INTEGER |
                                             EXPR_VALUE_TYPE_BOOLEAN))) {
        ndat->u.rexpr.dpath_type_mask |= EXPR_DPATH_TYPE_NONE;
    }
    if (0 != (filter_cls->value_type_mask & (EXPR_VALUE_TYPE_BYTES |
                                             EXPR_VALUE_TYPE_STRING))) {
        ndat->u.rexpr.dpath_type_mask |= EXPR_DPATH_TYPE_CONTAINER;
    }
    ndat->u.rexpr_filter.filter_cls = filter_cls;
    ndat->u.rexpr_filter.filter_def = filter_def;

    node->ndat = ndat;

    f_instance = filter_cls->filter_instance_build_func(node);
    if (NULL == f_instance) {
        return -1;
    }
    ndat->u.rexpr_filter.f_instance = f_instance;
    // FIXME merge filter item with node item
    //ndat->u.item = f_instance->item;
    return 0;
}

int
filter_instance_build_shared(struct ast_node_hdl *node,
                             const char *filter_name)
{
    const struct filter_class *filter_cls;

    filter_cls = builtin_filter_lookup(filter_name);
    assert(NULL != filter_cls);
    if (-1 == filter_instance_build(node, filter_cls,
                                    filter_def_create_empty(filter_name))) {
        return -1;
    }
    return 0;
}

const struct filter_attr_def *
filter_class_get_attr(const struct filter_class *filter_cls,
                         const char *attr_name)
{
    struct filter_attr_def *attr_def;

    STAILQ_FOREACH(attr_def, &filter_cls->attr_list, list) {
        if (0 == strcmp(attr_def->name, attr_name) ||
            0 == strcmp(attr_def->name, "@*")) {
            return attr_def;
        }
    }
    return NULL; /* not found */
}

bitpunch_status_t
filter_instance_read_value(struct ast_node_hdl *filter,
                           struct box *scope,
                           int64_t item_start_offset,
                           int64_t item_end_offset,
                           expr_value_t *valuep,
                           struct browse_state *bst)
{
    struct filter_instance *f_instance;
    bitpunch_status_t bt_ret;
    int64_t span_size;
    const char *item_data;
    expr_value_t value;

    assert(-1 != item_start_offset);
    assert(NULL != scope->ds_in);

    value.type = EXPR_VALUE_TYPE_UNSET;
    f_instance = filter->ndat->u.rexpr_filter.f_instance;
    if (NULL != f_instance->b_item.compute_item_size_from_buffer) {
        item_data = scope->ds_in->ds_data + item_start_offset;
        bt_ret = f_instance->b_item.compute_item_size_from_buffer(
            filter, scope,
            item_data, item_end_offset - item_start_offset,
            &span_size, bst);
    } else if (NULL != f_instance->b_item.compute_item_size) {
        bt_ret = f_instance->b_item.compute_item_size(
            filter, scope, item_start_offset, item_end_offset,
            &span_size, bst);
    } else {
        span_size = item_end_offset - item_start_offset;
        bt_ret = BITPUNCH_OK;
    }
    if (BITPUNCH_OK == bt_ret) {
        memset(&value, 0, sizeof(value));
        item_data = scope->ds_in->ds_data + item_start_offset;
        if (NULL != f_instance->b_item.read_value_from_buffer) {
            bt_ret = f_instance->b_item.read_value_from_buffer(
                filter, scope, item_data, span_size, &value, bst);
        } else {
            value = expr_value_as_data_range(
                scope->ds_in, item_start_offset, item_end_offset);
            bitpunch_data_source_acquire(scope->ds_in);
        }
    }
    if (BITPUNCH_OK == bt_ret) {
        if (NULL != valuep) {
            *valuep = value;
        } else {
            expr_value_destroy(value);
        }
    } else {
        bitpunch_error_add_node_context(
            filter, bst, "when reading filtered value");
    }
    return bt_ret;
}

bitpunch_status_t
filter_instance_get_data_source(
    struct ast_node_hdl *filter, struct box *scope,
    struct bitpunch_data_source **ds_outp, struct browse_state *bst)
{
    struct filter_instance *f_instance;

    f_instance = filter->ndat->u.rexpr_filter.f_instance;
    assert(NULL != f_instance->b_item.get_data_source);

    return f_instance->b_item.get_data_source(filter, scope, ds_outp, bst);
}


struct filter_iterator
filter_iter_statements(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier)
{
    struct ast_node_hdl *base_filter;
    struct filter_iterator fit;

    assert(ast_node_is_filter(filter));
    fit.filter = filter;
    // start with the top-level base filter
    fit.cur_base_filter = filter;
    while (TRUE) {
        base_filter =
            fit.cur_base_filter->ndat->u.rexpr_filter.filter_def->base_filter;
        if (NULL == base_filter) {
            break ;
        }
        assert(ast_node_is_filter(base_filter));
        fit.cur_base_filter = base_filter;
    }
    fit.scit = scope_iter_statements(
        ast_node_get_scope_def(fit.cur_base_filter),
        scope, stmt_mask, identifier);
    return fit;
}

struct filter_iterator
filter_iter_statements_from(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const struct statement *stmt,
    const char *identifier)
{
    struct ast_node_hdl *base_filter;
    struct filter_iterator fit;

    assert(ast_node_is_filter(filter));
    fit.filter = filter;
    // find the base filter containing stmt
    fit.cur_base_filter = filter;
    while (TRUE) {
        if (scope_contains_statement(
                ast_node_get_const_scope_def(fit.cur_base_filter),
                stmt_mask, stmt)) {
            break ;
        }
        base_filter =
            fit.cur_base_filter->ndat->u.rexpr_filter.filter_def->base_filter;
        assert(NULL != base_filter);
        assert(ast_node_is_filter(base_filter));
        fit.cur_base_filter = base_filter;
    }
    fit.scit = scope_iter_statements_from(
        ast_node_get_scope_def(fit.cur_base_filter), scope, stmt, identifier);
    return fit;
}

struct filter_iterator
filter_riter_statements(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier)
{
    struct filter_iterator fit;

    assert(ast_node_is_filter(filter));
    fit.filter = filter;
    // start with the leaf filter
    fit.cur_base_filter = filter;
    fit.scit = scope_riter_statements(
        ast_node_get_scope_def(filter), scope, stmt_mask, identifier);
    return fit;
}

struct filter_iterator
filter_riter_statements_from(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const struct statement *stmt,
    const char *identifier)
{
    struct ast_node_hdl *base_filter;
    struct filter_iterator fit;

    assert(ast_node_is_filter(filter));
    fit.filter = filter;
    // find the base filter containing stmt
    fit.cur_base_filter = filter;
    while (TRUE) {
        if (scope_contains_statement(
                ast_node_get_const_scope_def(fit.cur_base_filter),
                stmt_mask, stmt)) {
            break ;
        }
        base_filter =
            fit.cur_base_filter->ndat->u.rexpr_filter.filter_def->base_filter;
        assert(NULL != base_filter);
        assert(ast_node_is_filter(base_filter));
        fit.cur_base_filter = base_filter;
    }
    fit.scit = scope_riter_statements_from(
        ast_node_get_scope_def(fit.cur_base_filter), scope, stmt, identifier);
    return fit;
}

bitpunch_status_t
filter_iter_statements_next_internal(
    struct filter_iterator *fit,
    enum statement_type *stmt_typep, const struct statement **stmtp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct ast_node_hdl *cur_filter;
    struct ast_node_hdl *base_filter;

    while (TRUE) {
        bt_ret = scope_iter_statements_next_internal(
            &fit->scit, stmt_typep, stmtp, bst);
        if (BITPUNCH_NO_ITEM != bt_ret) {
            return bt_ret;
        }
        if (0 != (fit->scit.it_flags & SCOPE_ITERATOR_FLAG_REVERSE)) {
            fit->cur_base_filter =
                fit->cur_base_filter->ndat->u.rexpr_filter.filter_def->base_filter;
            if (NULL == fit->cur_base_filter) {
                return BITPUNCH_NO_ITEM;
            }
        } else {
            if (fit->filter == fit->cur_base_filter) {
                return BITPUNCH_NO_ITEM;
            }
            cur_filter = fit->filter;
            while (TRUE) {
                base_filter =
                    cur_filter->ndat->u.rexpr_filter.filter_def->base_filter;
                if (base_filter == fit->cur_base_filter) {
                    fit->cur_base_filter = cur_filter;
                    break ;
                }
                cur_filter = base_filter;
            }
        }
        fit->scit = scope_iter_statements(
            ast_node_get_scope_def(fit->cur_base_filter),
            fit->scit.scope, fit->scit.stmt_mask, fit->scit.identifier);
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
filter_iter_statements_next(
    struct filter_iterator *fit,
    enum statement_type *stmt_typep, const struct statement **stmtp,
    struct bitpunch_error **errp)
{
    struct browse_state bst;

    browse_state_init_scope(&bst, fit->scit.scope);
    return transmit_error(
        filter_iter_statements_next_internal(fit, stmt_typep, stmtp, &bst),
        &bst, errp);
}

bitpunch_status_t
filter_lookup_statement_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct ast_node_hdl *base_filter;

    assert(ast_node_is_filter(filter));
    base_filter = filter->ndat->u.rexpr_filter.filter_def->base_filter;
    bt_ret = scope_lookup_statement_internal(
        ast_node_get_scope_def(filter), scope, stmt_mask, identifier,
        stmt_typep, stmtp, scopep, bst);
    if (BITPUNCH_NO_ITEM != bt_ret || NULL == base_filter) {
        return bt_ret;
    }
    return filter_lookup_statement_internal(
        base_filter, scope, stmt_mask, identifier,
        stmt_typep, stmtp, scopep, bst);
}

bitpunch_status_t
filter_get_n_statements_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    int64_t *stmt_countp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct ast_node_hdl *base_filter;
    int64_t scope_count;
    int64_t base_filter_count;

    assert(ast_node_is_filter(filter));
    base_filter = filter->ndat->u.rexpr_filter.filter_def->base_filter;
    bt_ret = scope_get_n_statements_internal(
        ast_node_get_scope_def(filter), scope, stmt_mask, identifier,
        &scope_count, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (NULL != base_filter) {
        bt_ret = filter_get_n_statements_internal(
            base_filter, scope, stmt_mask, identifier, &base_filter_count, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        *stmt_countp = scope_count + base_filter_count;
    } else {
        *stmt_countp = scope_count;
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
filter_evaluate_identifier_internal(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum expr_evaluate_flag flags,
    enum statement_type *stmt_typep, const struct named_statement **stmtp,
    struct box **scopep,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct ast_node_hdl *base_filter;

    assert(ast_node_is_filter(filter));
    base_filter = filter->ndat->u.rexpr_filter.filter_def->base_filter;
    bt_ret = scope_evaluate_identifier_internal(
        ast_node_get_scope_def(filter), scope, stmt_mask, identifier, flags,
        stmt_typep, stmtp, scopep, valuep, dpathp, bst);
    if (BITPUNCH_NO_ITEM != bt_ret || NULL == base_filter) {
        return bt_ret;
    }
    return filter_evaluate_identifier_internal(
        base_filter, scope, stmt_mask, identifier, flags,
        stmt_typep, stmtp, scopep, valuep, dpathp, bst);
}

bitpunch_status_t
filter_evaluate_attribute_internal(
    struct ast_node_hdl *filter, struct box *scope,
    const char *attr_name,
    enum expr_evaluate_flag flags,
    const struct named_expr **attrp,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct browse_state *bst)
{
    return filter_evaluate_identifier_internal(
        filter, scope, STATEMENT_TYPE_ATTRIBUTE, attr_name, flags,
        NULL, (const struct named_statement **)attrp, NULL,
        valuep, dpathp, bst);
}

bitpunch_status_t
filter_evaluate_identifier(
    struct ast_node_hdl *filter, struct box *scope,
    enum statement_type stmt_mask, const char *identifier,
    enum expr_evaluate_flag flags,
    expr_value_t *valuep, expr_dpath_t *dpathp,
    struct bitpunch_error **errp)
{
    struct browse_state bst;

    browse_state_init_scope(&bst, scope);
    return transmit_error(
        filter_evaluate_identifier_internal(
            filter, scope, stmt_mask, identifier, flags,
            NULL, NULL, NULL, valuep, dpathp, &bst),
        &bst, errp);
}

void
filter_attach_native_attribute(
    struct ast_node_hdl *filter,
    const char *attr_name, expr_value_t value)
{
  return scope_attach_native_attribute(
      ast_node_get_scope_def(filter), attr_name, value);
}

struct ast_node_hdl *
filter_get_first_declared_named_expr(
    const struct ast_node_hdl *filter,
    const char *name)
{
    struct ast_node_hdl *base_filter;
    struct ast_node_hdl *named_expr;

    assert(ast_node_is_filter(filter));
    base_filter = filter->ndat->u.rexpr_filter.filter_def->base_filter;
    named_expr = scope_get_first_declared_named_expr(
        ast_node_get_const_scope_def(filter), name);
    if (NULL != named_expr || NULL == base_filter) {
        return named_expr;
    }
    return filter_get_first_declared_named_expr(base_filter, name);
}

struct ast_node_hdl *
filter_get_first_declared_attribute(
    const struct ast_node_hdl *filter,
    const char *attr_name)
{
    struct ast_node_hdl *base_filter;
    struct ast_node_hdl *named_expr;

    assert(ast_node_is_filter(filter));
    base_filter = filter->ndat->u.rexpr_filter.filter_def->base_filter;
    named_expr = scope_get_first_declared_attribute(
        ast_node_get_const_scope_def(filter), attr_name);
    if (NULL != named_expr || NULL == base_filter) {
        return named_expr;
    }
    return filter_get_first_declared_attribute(base_filter, attr_name);
}



/*
 * tracking backends
 */

bitpunch_status_t
box_compute_used_size__from_apply_filter(struct box *box,
                                         struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    bt_ret = box_apply_filter_internal(box, bst);
    // check if applied filter did not yield data as a result
    if (BITPUNCH_OK == bt_ret &&
        (-1 == box->start_offset_used || -1 == box->end_offset_used)) {
        return box_compute_used_size__as_span(box, bst);
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_first_item__data_filter(struct tracker *tk,
                                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t filtered_size;

    DBG_TRACKER_DUMP(tk);
    if (0 != (tk->flags & TRACKER_REVERSED) ||
        0 != (tk->box->flags & BOX_RALIGN)) {
        return bitpunch_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst,
                             "tracker_goto_first_item() not implemented "
                             "in reverse mode on data filters");
    }
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    bt_ret = box_get_used_size(tk->box, &filtered_size, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (0 == filtered_size) {
        return BITPUNCH_NO_ITEM;
    }
    tk->item_offset = tk->box->start_offset_used;
    tk->cur.u.array.index = 0;
    tk->dpath.filter = filter_get_global_instance__byte();
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_next_item__data_filter(struct tracker *tk,
                                    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t filtered_size;

    DBG_TRACKER_DUMP(tk);
    if (0 != (tk->flags & TRACKER_REVERSED) ||
        0 != (tk->box->flags & BOX_RALIGN)) {
        return bitpunch_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst,
                             "tracker_goto_next_item() not implemented "
                             "in reverse mode on data filters");
    }
    bt_ret = box_get_used_size(tk->box, &filtered_size, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    ++tk->cur.u.array.index;
    tk->item_offset += 1;
    if (tk->cur.u.array.index == filtered_size) {
        (void) tracker_set_end(tk, bst);
        return BITPUNCH_NO_ITEM;
    }
    /* check new item */
    bt_ret = tracker_check_item(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        /* rollback */
        tk->item_offset -= 1;
        --tk->cur.u.array.index;
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item__data_filter(
    struct tracker *tk, int64_t index,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t filtered_size;

    DBG_TRACKER_DUMP(tk);
    if (0 != (tk->flags & TRACKER_REVERSED) ||
        0 != (tk->box->flags & BOX_RALIGN)) {
        return bitpunch_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst,
                             "tracker_goto_nth_item() not implemented "
                             "in reverse mode on data filters");
    }
    bt_ret = box_get_used_size(tk->box, &filtered_size, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (index >= filtered_size) {
        return BITPUNCH_NO_ITEM;
    }
    tk->flags &= ~TRACKER_AT_END;
    tk->dpath.filter = filter_get_global_instance__byte();
    tk->item_offset = tk->box->start_offset_used + index;
    tk->item_size = 1;
    tk->cur = track_path_from_array_index(index);
    return tracker_check_item(tk, bst);
}

void
compile_node_backends__filter__filter(struct ast_node_hdl *filter)
{
    //struct item_backend *b_item = NULL;

    //b_item = &filter->ndat->u.rexpr_filter.f_instance->b_item;
    //memset(b_item, 0, sizeof (*b_item));
}

static void
compile_node_backends__box__filter(struct ast_node_hdl *item)
{
    struct filter_instance *f_instance;
    struct item_backend *b_item;
    struct box_backend *b_box;

    f_instance = item->ndat->u.rexpr_filter.f_instance;
    b_item = &f_instance->b_item;
    b_box = &f_instance->b_box;
    // FIXME avoid memset because filter may have set functions
    // already, find a cleaner way to deal with this
    //memset(b_box, 0, sizeof (*b_box));

    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->compute_max_span_size = box_compute_max_span_size__as_slack;
    if (NULL != b_item->compute_item_size_from_buffer) {
        b_box->compute_span_size =
            box_compute_span_size__from_compute_item_size_from_buffer;
    } else if (NULL != b_item->compute_item_size) {
        b_box->compute_span_size =
            box_compute_span_size__from_compute_item_size;
    } else {
        b_box->compute_span_size = box_compute_span_size__as_slack;
    }
    b_box->get_n_items = box_get_n_items__as_used;
    // FIXME should check for non-dpath filter only instead of
    // specific value-type (i.e. output dpath == input dpath)
    if (EXPR_VALUE_TYPE_INTEGER ==
        item->ndat->u.rexpr_filter.filter_cls->value_type_mask) {
        b_box->compute_used_size = box_compute_used_size__as_span;
    } else {
        b_box->compute_used_size = box_compute_used_size__from_apply_filter;
    }
}

void
compile_node_backends__tracker__filter(struct ast_node_hdl *item)
{
    struct filter_instance *as_bytes;
    struct tracker_backend *b_tk;

    as_bytes = item->ndat->u.rexpr_filter.f_instance;
    b_tk = &as_bytes->b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

    b_tk->get_item_key = tracker_get_item_key__array_generic;
    b_tk->goto_first_item = tracker_goto_first_item__data_filter;
    b_tk->goto_next_item = tracker_goto_next_item__data_filter;
    b_tk->goto_nth_item = tracker_goto_nth_item__data_filter;
    b_tk->goto_next_item_with_key = tracker_goto_next_item_with_key__default;
    b_tk->goto_nth_item_with_key = tracker_goto_nth_item_with_key__default;
    b_tk->goto_end_path = tracker_goto_end_path__array_generic;
    b_tk->goto_nil = tracker_goto_nil__array_generic;
}

void
compile_node_backends__filter_generic(struct ast_node_hdl *filter)
{
    compile_node_backends__filter__filter(filter);
    compile_node_backends__box__filter(filter);
    compile_node_backends__tracker__filter(filter);
}
