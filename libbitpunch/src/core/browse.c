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
#include <string.h>
#include <assert.h>
#include <err.h>
#include <stdarg.h>

#include "utils/bloom.h"

#include "api/bitpunch_api.h"

#include "core/ast.h"
#include "core/parser.h"
#include PATH_TO_PARSER_TAB_H
#include "core/filter.h"
#include "core/print.h"
#include "core/browse_internal.h"
#include "core/expr_internal.h"
#include "core/debug.h"

//FIXME remove once filters become isolated
#include "filters/composite.h"
#include "filters/array.h"

#define DISABLE_BOX_CACHING

struct tracker_error_slist {
    struct tracker_error tk_err;
    struct tracker_error_slist *next;
};


#define WITH_EXPECTED_ERROR(BITPUNCH_ERROR_CODE, BODY) do {      \
        struct tracker_error_slist __expected_error;            \
                                                                \
        tracker_error_init(&__expected_error.tk_err,            \
                           BITPUNCH_ERROR_CODE);                 \
        __expected_error.tk_err.flags = TRACKER_ERROR_STATIC;   \
        __expected_error.next = bst->expected_errors;           \
        bst->expected_errors = &__expected_error;               \
        BODY                                                    \
        bst->expected_errors = __expected_error.next;           \
} while (0)

static struct tracker_error *
error_get_expected(bitpunch_status_t bt_err,
                   struct browse_state *bst)
{
    struct tracker_error_slist *next_expected_error;

    for (next_expected_error = bst->expected_errors;
         NULL != next_expected_error;
         next_expected_error = next_expected_error->next) {
        if (bt_err == next_expected_error->tk_err.bt_ret) {
            return &next_expected_error->tk_err;
        }
    }
    return NULL;
}

// static ast nodes

struct ast_node_data shared_ast_node_data_byte = {
    .type = AST_NODE_TYPE_BYTE,
    .u = {
        .item = {
            .min_span_size = 1,
        }
    }
};
struct ast_node_hdl shared_ast_node_byte = {
    .ndat = &shared_ast_node_data_byte,
};

struct ast_node_data shared_ast_node_data_array_slice = {
    .type = AST_NODE_TYPE_ARRAY_SLICE,
};
struct ast_node_hdl shared_ast_node_array_slice = {
    .ndat = &shared_ast_node_data_array_slice,
};

struct ast_node_data shared_ast_node_data_byte_slice = {
    .type = AST_NODE_TYPE_BYTE_SLICE,
};
struct ast_node_hdl shared_ast_node_byte_slice = {
    .ndat = &shared_ast_node_data_byte_slice,
};

struct ast_node_data shared_ast_node_data_as_bytes = {
    .type = AST_NODE_TYPE_AS_BYTES,
};
struct ast_node_hdl shared_ast_node_as_bytes = {
    .ndat = &shared_ast_node_data_as_bytes,
};

struct dpath_node shared_dpath_node_array_slice = {
    .filter = &shared_ast_node_array_slice,
    .item = &shared_ast_node_array_slice,
};
struct dpath_node shared_dpath_node_byte_slice = {
    .filter = &shared_ast_node_byte_slice,
    .item = &shared_ast_node_byte_slice,
};


static struct track_path
track_path_from_composite_field(const struct field *field)
{
    struct track_path ret;

    memset(&ret, 0, sizeof (ret));
    ret.type = TRACK_PATH_COMPOSITE;
    ret.u.block.field = field;
    if (NULL != field) {
        if (0 != (field->nstmt.stmt.stmt_flags & FIELD_FLAG_HEADER)) {
            ret.flags |= TRACK_PATH_HEADER;
        }
        if (0 != (field->nstmt.stmt.stmt_flags & FIELD_FLAG_TRAILER)) {
            ret.flags |= TRACK_PATH_TRAILER;
        }
    }
    return ret;
}

static struct track_path
track_path_from_array_index(int64_t index)
{
    struct track_path ret;

    memset(&ret, 0, sizeof (ret));
    ret.type = TRACK_PATH_ARRAY;
    ret.u.array.index = index;
    return ret;
}

static struct track_path
track_path_from_array_slice(int64_t index_start, int64_t index_end)
{
    struct track_path ret;

    memset(&ret, 0, sizeof (ret));
    ret.type = TRACK_PATH_ARRAY_SLICE;
    ret.u.array.index = index_start;
    ret.u.array_slice.index_end = index_end;
    return ret;
}


static const char *
box_offset_type_str(enum box_offset_type type);

static void
box_delete_non_null(struct box *box);

static bitpunch_status_t
box_compute_min_span_size(struct box *box,
                          struct browse_state *bst);
static bitpunch_status_t
box_compute_used_size(struct box *box,
                      struct browse_state *bst);
static bitpunch_status_t
box_compute_max_span_size(struct box *box,
                          struct browse_state *bst);
static bitpunch_status_t
box_compute_slack_size(struct box *box,
                        struct browse_state *bst);
static bitpunch_status_t
box_get_children_slack(struct box *box, int get_left_offset,
                       int64_t *max_slack_offsetp,
                       struct browse_state *bst);

static void
tracker_set_dangling_internal(struct tracker *tk);
static void
tracker_set_dangling(struct tracker *tk);
static struct tracker *
tracker_new(struct box *box);
static bitpunch_status_t
tracker_compute_item_offset(struct tracker *tk,
                            struct browse_state *bst);
static bitpunch_status_t
tracker_compute_item_size_internal(struct tracker *tk,
                                   int64_t *item_sizep,
                                   struct browse_state *bst);
static bitpunch_status_t
tracker_compute_item_size(struct tracker *tk,
                          struct browse_state *bst);
static bitpunch_status_t
tracker_compute_item_location(struct tracker *tk,
                              struct browse_state *bst);

static void
box_init_index_cache_by_key(struct box *box);
static void
box_init_mark_offsets_repo(struct box *box);
static int
box_index_cache_exists(const struct box *box);
static int
box_mark_offsets_repo_exists(const struct box *box);
static void
box_destroy_index_cache_by_key(struct box *box);
static void
box_destroy_mark_offsets_repo(struct box *box);
static void
tracker_jump_to_item_internal(struct tracker *tk,
                              struct dpath_node *item_dpath,
                              struct track_path item_path,
                              int64_t item_offset,
                              struct browse_state *bst);
static void
tracker_goto_mark_internal(struct tracker *tk,
                           struct dpath_node *item_dpath,
                           int64_t mark,
                           struct browse_state *bst);
static void
tracker_index_cache_add_item(struct tracker *tk, expr_value_t item_key);
static void
tracker_goto_last_cached_item_internal(struct tracker *tk,
                                       struct browse_state *bst);
static bitpunch_status_t
tracker_goto_next_key_match_in_mark(struct tracker *tk,
                                    expr_value_t key,
                                    int64_t mark,
                                    struct browse_state *bst);

static bitpunch_status_t
tracker_goto_end_path(struct tracker *tk,
                      struct browse_state *bst);
static bitpunch_status_t
tracker_goto_end_offset(struct tracker *tk,
                        struct browse_state *bst);
static bitpunch_status_t
tracker_goto_next_item_internal(struct tracker *tk,
                                struct browse_state *bst);
static bitpunch_status_t
tracker_goto_nth_item_internal(struct tracker *tk, int64_t index,
                               struct browse_state *bst);
static bitpunch_status_t
tracker_goto_end_internal(struct tracker *tk,
                          struct browse_state *bst);
static bitpunch_status_t
tracker_goto_first_item_with_key_internal(struct tracker *tk,
                                          expr_value_t item_key,
                                          struct browse_state *bst);
static bitpunch_status_t
tracker_goto_next_item_with_key_internal(struct tracker *tk,
                                         expr_value_t item_key,
                                         struct browse_state *bst);
static bitpunch_status_t
tracker_goto_nth_item_with_key_internal(struct tracker *tk,
                                        expr_value_t item_key,
                                        int nth_twin,
                                        struct browse_state *bst);

static bitpunch_status_t
tracker_goto_first_item_int__composite(struct tracker *tk, int flat,
                                       struct browse_state *bst);
static bitpunch_status_t
tracker_goto_next_item_int__composite(struct tracker *tk, int flat,
                                      struct browse_state *bst);


void
browse_state_init(struct browse_state *bst)
{
    memset(bst, 0, sizeof (*bst));
}

void
browse_state_cleanup(struct browse_state *bst)
{
    if (NULL != bst) {
        tracker_error_destroy(bst->last_error);
    }
}

static void
browse_state_clear_error(struct browse_state *bst)
{
    tracker_error_destroy(bst->last_error);
    bst->last_error = NULL;
}

bitpunch_status_t
browse_state_get_last_error_status(struct browse_state *bst)
{
    if (NULL == bst->last_error) {
        return BITPUNCH_OK;
    } else {
        return bst->last_error->bt_ret;
    }
}

/*
 * dpath
 */

bitpunch_status_t
expr_dpath_to_tracker_internal(expr_dpath_t dpath,
                               struct tracker **tkp, struct browse_state *bst)
{
    struct tracker *tk;

    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        tk = tracker_dup(dpath.item.tk);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        tk = track_box_contents_internal(dpath.container.box, bst);
        break ;
    default:
        assert(0);
    }
    *tkp = tk;
    return BITPUNCH_OK;
}

bitpunch_status_t
expr_dpath_to_box_internal(expr_dpath_t dpath,
                           struct box **boxp,
                           struct browse_state *bst)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return tracker_get_filtered_item_box_internal(dpath.item.tk, boxp,
                                                      bst);
    case EXPR_DPATH_TYPE_CONTAINER:
        box_acquire(dpath.container.box);
        *boxp = dpath.container.box;
        return BITPUNCH_OK;
    default:
        assert(0);
    }
}

bitpunch_status_t
expr_dpath_to_box_direct(expr_dpath_t dpath,
                         struct box **boxp,
                         struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *box;

    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = tracker_create_item_box_internal(dpath.item.tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        box = dpath.item.tk->item_box;
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        box = dpath.container.box;
        break ;
    default:
        assert(0);
    }
    box_acquire(box);
    *boxp = box;
    return BITPUNCH_OK;
}

bitpunch_status_t
expr_dpath_to_container_internal(expr_dpath_t dpath,
                                 expr_dpath_t *dpathp,
                                 struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *box;

    bt_ret = expr_dpath_to_box_internal(dpath, &box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    dpathp->type = EXPR_DPATH_TYPE_CONTAINER;
    dpathp->container.box = box;
    return BITPUNCH_OK;
}

bitpunch_status_t
expr_dpath_to_item_internal(expr_dpath_t dpath,
                            expr_dpath_t *dpathp,
                            struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct tracker *tk;

    bt_ret = expr_dpath_to_tracker_internal(dpath, &tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    dpathp->type = EXPR_DPATH_TYPE_ITEM;
    dpathp->item.tk = tk;
    return BITPUNCH_OK;
}

bitpunch_status_t
expr_dpath_to_dpath_internal(expr_dpath_t src_dpath,
                             enum expr_dpath_type dst_type,
                             expr_dpath_t *dst_dpathp,
                             struct browse_state *bst)
{
    switch (dst_type) {
    case EXPR_DPATH_TYPE_ITEM:
        return expr_dpath_to_item_internal(src_dpath, dst_dpathp, bst);
    case EXPR_DPATH_TYPE_CONTAINER:
        return expr_dpath_to_container_internal(src_dpath, dst_dpathp, bst);
    default:
        assert(0);
    }
}

struct box *
expr_dpath_get_parent_box(expr_dpath_t dpath)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return dpath.item.tk->box;
    case EXPR_DPATH_TYPE_CONTAINER:
        return dpath.container.box->parent_box;
    default:
        assert(0);
    }
}

bitpunch_status_t
expr_dpath_get_size_internal(expr_dpath_t dpath,
                             int64_t *dpath_sizep,
                             struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = tracker_get_item_size_internal(dpath.item.tk,
                                                dpath_sizep, bst);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        bt_ret = box_get_used_size(dpath.container.box, dpath_sizep, bst);
        break ;
    default:
        assert(0);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
expr_dpath_get_location_internal(expr_dpath_t dpath,
                                 int64_t *offsetp, int64_t *sizep,
                                 struct browse_state *bst)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return tracker_get_item_location_internal(dpath.item.tk,
                                                  offsetp, sizep, bst);
    case EXPR_DPATH_TYPE_CONTAINER:
        return box_get_location_internal(dpath.container.box,
                                         offsetp, sizep, bst);
    default:
        assert(0);
    }
}

bitpunch_status_t
expr_dpath_evaluate_filter_type_internal(
    expr_dpath_t dpath,
    struct box *scope,
    enum filter_kind kind,
    struct ast_node_hdl **filter_typep,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        switch (kind) {
        case FILTER_KIND_ITEM:
            if (NULL == dpath.item.tk->dpath.item) {
                bt_ret = expr_evaluate_filter_type_internal(
                    dpath.item.tk->dpath.filter, dpath.item.tk->box,
                    FILTER_KIND_ITEM, &dpath.item.tk->dpath.item, bst);
                if (BITPUNCH_OK != bt_ret) {
                    return bt_ret;
                }
            }
            *filter_typep = dpath.item.tk->dpath.item;
            return BITPUNCH_OK;
        default:
            return expr_evaluate_filter_type_internal(
                dpath.item.tk->dpath.filter, dpath.item.tk->box,
                kind, filter_typep, bst);
        }

    case EXPR_DPATH_TYPE_CONTAINER:
        switch (kind) {
        case FILTER_KIND_ITEM:
            *filter_typep = dpath.container.box->dpath.item;
            break ;
        case FILTER_KIND_FILTER:
            *filter_typep = dpath.container.box->dpath.filter;
            break ;
        case FILTER_KIND_DEFINING_SPAN_SIZE:
            *filter_typep =
                dpath.container.box->dpath.filter_defining_span_size;
            break ;
        case FILTER_KIND_DEFINING_USED_SIZE:
            *filter_typep =
                dpath.container.box->dpath.filter_defining_used_size;
            break ;
        case FILTER_KIND_ANCESTOR:
        default:
            assert(0);
        }
        return BITPUNCH_OK;

    default:
        assert(0);
    }
}

int
expr_dpath_contains_indexed_items(expr_dpath_t dpath)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return ast_node_is_indexed(dpath.item.tk->dpath.item);
    case EXPR_DPATH_TYPE_CONTAINER:
        return box_contains_indexed_items(dpath.container.box);
    default:
        assert(0);
    }
}

const struct ast_node_hdl *
expr_dpath_get_as_type(expr_dpath_t dpath)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return dpath_node_get_as_type(&dpath.item.tk->dpath);
    case EXPR_DPATH_TYPE_CONTAINER:
        return dpath_node_get_as_type(&dpath.container.box->dpath);
    default:
        assert(0);
    }
}

const struct ast_node_hdl *
expr_dpath_get_target_filter(expr_dpath_t dpath)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return dpath_node_get_target_filter(&dpath.item.tk->dpath);
    case EXPR_DPATH_TYPE_CONTAINER:
        return dpath_node_get_target_filter(&dpath.container.box->dpath);
    default:
        assert(0);
    }
}

struct track_path
expr_dpath_get_track_path(expr_dpath_t dpath)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return dpath.item.tk->cur;
    case EXPR_DPATH_TYPE_CONTAINER:
        return dpath.container.box->track_path;
    default:
        assert(0);
    }
}

int
expr_dpath_is(expr_dpath_t dpath1, expr_dpath_t dpath2)
{
    if (dpath1.type != dpath2.type) {
        return FALSE;
    }
    switch (dpath1.type) {
    case EXPR_DPATH_TYPE_CONTAINER:
        return dpath1.container.box == dpath2.container.box;
    case EXPR_DPATH_TYPE_ITEM:
        return dpath1.item.tk == dpath2.item.tk;
    default:
        return FALSE;
    }
}

/**
 * @brief find the common ancestor box of two dpath expressions
 *
 * @param[out] ancestor1_typep dpath type of closest common ancestor
 * (from dpath1's ancestors)
 * @param[out] ancestor1_dpathp dpath of closest common ancestor (from
 * dpath1's ancestors)
 * @param[out] ancestor2_typep dpath type of closest common ancestor
 * (from dpath2's ancestors)
 * @param[out] ancestor2_dpathp dpath of closest common ancestor (from
 * dpath2's ancestors)
 */
void
expr_dpath_find_common_ancestor(expr_dpath_t dpath1,
                                expr_dpath_t dpath2,
                                expr_dpath_t *ancestor1_dpathp,
                                expr_dpath_t *ancestor2_dpathp)
{
    struct track_path path1, path2;
    struct box *pbox1, *pbox2;
    struct box *box1, *box2;
    struct box **ancestors1, **ancestors2;
    int path_eq;
    expr_dpath_t ancestor1_dpath, ancestor2_dpath;

    pbox1 = expr_dpath_get_parent_box(dpath1);
    pbox2 = expr_dpath_get_parent_box(dpath2);

    if (NULL != pbox1) {
        ancestors1 = alloca((pbox1->depth_level + 1) * sizeof(*ancestors1));
        box1 = pbox1;
        while (NULL != box1) {
            *ancestors1 = box1;
            ++ancestors1;
            box1 = box1->parent_box;
        }
    }
    if (NULL != pbox2) {
        ancestors2 = alloca((pbox2->depth_level + 1) * sizeof(*ancestors2));
        box2 = pbox2;
        while (NULL != box2) {
            *ancestors2 = box2;
            ++ancestors2;
            box2 = box2->parent_box;
        }
    }
    if (NULL == pbox1) {
        ancestor1_dpath = dpath1;
        if (NULL == pbox2) {
            ancestor2_dpath = dpath2;
        } else {
            ancestor2_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
            ancestor2_dpath.container.box = ancestors2[-1];
        }
        goto end;
    }
    if (NULL == pbox2) {
        ancestor1_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor1_dpath.container.box = ancestors1[-1];
        ancestor2_dpath = dpath2;
        goto end;
    }
    do {
        --ancestors1;
        --ancestors2;
        path_eq = track_path_eq((*ancestors1)->track_path,
                                (*ancestors2)->track_path);
    }
    while (path_eq && *ancestors1 != pbox1 && *ancestors2 != pbox2);

    if (!path_eq) {
        ancestor1_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor1_dpath.container.box = ancestors1[1];
        ancestor2_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor2_dpath.container.box = ancestors2[1];
        goto end;
    }
    if (*ancestors1 == pbox1) {
        path1 = expr_dpath_get_track_path(dpath1);
    } else {
        path1 = ancestors1[-1]->track_path;
    }
    if (*ancestors2 == pbox2) {
        path2 = expr_dpath_get_track_path(dpath2);
    } else {
        path2 = ancestors2[-1]->track_path;
    }
    path_eq = track_path_eq(path1, path2);
    if (!path_eq) {
        ancestor1_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor1_dpath.container.box = *ancestors1;
        ancestor2_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor2_dpath.container.box = *ancestors2;
        goto end;
    }
    if (*ancestors1 == pbox1) {
        ancestor1_dpath = dpath1;
    } else {
        ancestor1_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor1_dpath.container.box = ancestors1[-1];
    }
    if (*ancestors2 == pbox2) {
        ancestor2_dpath = dpath2;
    } else {
        ancestor2_dpath.type = EXPR_DPATH_TYPE_CONTAINER;
        ancestor2_dpath.container.box = ancestors2[-1];
    }

  end:
    if (NULL != ancestor1_dpathp) {
        *ancestor1_dpathp = ancestor1_dpath;
    }
    if (NULL != ancestor2_dpathp) {
        *ancestor2_dpathp = ancestor2_dpath;
    }
}


/*
 * box
 */

static int64_t
box_get_offset(struct box *box, enum box_offset_type type)
{
    switch (type) {
    case BOX_START_OFFSET_HARD_MIN:
        return box->end_offset_used -
            ast_node_get_min_span_size(box->dpath.filter);
    case BOX_START_OFFSET_MIN_SPAN:
        return box->start_offset_min_span;
    case BOX_START_OFFSET_USED:
        return box->start_offset_used;
    case BOX_START_OFFSET_MAX_SPAN:
        return box->start_offset_max_span;
    case BOX_START_OFFSET_SLACK:
        return box->start_offset_slack;
    case BOX_START_OFFSET_PARENT:
        return box->start_offset_parent;
    case BOX_END_OFFSET_HARD_MIN:
        return box->start_offset_used +
            ast_node_get_min_span_size(box->dpath.filter);
    case BOX_END_OFFSET_MIN_SPAN:
        return box->end_offset_min_span;
    case BOX_END_OFFSET_USED:
        return box->end_offset_used;
    case BOX_END_OFFSET_MAX_SPAN:
        return box->end_offset_max_span;
    case BOX_END_OFFSET_SLACK:
        return box->end_offset_slack;
    case BOX_END_OFFSET_PARENT:
        return box->end_offset_parent;
    default:
        assert(0);
    }
}

static int64_t
box_get_known_start_offset_mask(const struct box *box,
                                enum box_offset_type mask)
{
    if ((mask & BOX_START_OFFSET_USED) && box->start_offset_used >= 0) {
        return box->start_offset_used;
    }
    if ((mask & BOX_START_OFFSET_MAX_SPAN)
        && box->start_offset_max_span >= 0) {
        return box->start_offset_max_span;
    }
    if ((mask & BOX_START_OFFSET_SLACK) && box->start_offset_slack >= 0) {
        return box->start_offset_slack;
    }
    if ((mask & BOX_START_OFFSET_PARENT)) {
        return box->start_offset_parent;
    }
    return -1;
}

static int64_t
box_get_known_start_offset(const struct box *box)
{
    return box_get_known_start_offset_mask(box,
                                           BOX_START_OFFSET_USED |
                                           BOX_START_OFFSET_MAX_SPAN |
                                           BOX_START_OFFSET_SLACK |
                                           BOX_START_OFFSET_PARENT);
}

static int64_t
box_get_known_end_offset_mask(const struct box *box,
                              enum box_offset_type mask)
{
    if ((mask & BOX_END_OFFSET_USED) && box->end_offset_used >= 0) {
        return box->end_offset_used;
    }
    if ((mask & BOX_END_OFFSET_MAX_SPAN) && box->end_offset_max_span >= 0) {
        return box->end_offset_max_span;
    }
    if ((mask & BOX_END_OFFSET_SLACK) && box->end_offset_slack >= 0) {
        return box->end_offset_slack;
    }
    if ((mask & BOX_END_OFFSET_PARENT)) {
        return box->end_offset_parent;
    }
    return -1;
}

static int64_t
box_get_known_end_offset(const struct box *box)
{
    return box_get_known_end_offset_mask(box,
                                         BOX_END_OFFSET_USED |
                                         BOX_END_OFFSET_MAX_SPAN |
                                         BOX_END_OFFSET_SLACK |
                                         BOX_END_OFFSET_PARENT);
}

static enum box_offset_type
box_get_known_end_offset_type(const struct box *box)
{
    if (box->end_offset_used >= 0) {
        return BOX_END_OFFSET_USED;
    }
    if (box->end_offset_max_span >= 0) {
        return BOX_END_OFFSET_MAX_SPAN;
    }
    if (box->end_offset_slack >= 0) {
        return BOX_END_OFFSET_SLACK;
    }
    return BOX_END_OFFSET_PARENT;
}

static int64_t
box_get_known_size_mask(const struct box *box,
                        enum box_offset_type size_mask)
{
    if ((size_mask & BOX_SIZE_USED) == BOX_SIZE_USED
        && box->start_offset_used >= 0 && box->end_offset_used >= 0) {
        return box->end_offset_used - box->start_offset_used;
    }
    if ((size_mask & BOX_SIZE_MAX_SPAN) == BOX_SIZE_MAX_SPAN
        && box->start_offset_max_span >= 0 && box->end_offset_max_span >= 0) {
        return box->end_offset_max_span - box->start_offset_max_span;
    }
    if ((size_mask & BOX_SIZE_SLACK) == BOX_SIZE_SLACK
        && box->start_offset_slack >= 0 && box->end_offset_slack >= 0) {
        return box->end_offset_slack - box->start_offset_slack;
    }
    if ((size_mask & BOX_SIZE_PARENT) == BOX_SIZE_PARENT) {
        return box->end_offset_parent - box->start_offset_parent;
    }
    return -1;
}

static int64_t
box_get_known_size(const struct box *box)
{
    return box_get_known_size_mask(box,
                                   BOX_SIZE_USED |
                                   BOX_SIZE_MAX_SPAN |
                                   BOX_SIZE_SLACK |
                                   BOX_SIZE_PARENT);
}

static bitpunch_status_t
box_check_start_offset(struct box *box, int64_t start_offset,
                       enum box_offset_type type,
                       struct browse_state *bst)
{
    switch (type) {
    case BOX_START_OFFSET_HARD_MIN:
        if (box->start_offset_min_span >= 0) {
            if (start_offset < box->start_offset_min_span) {
                return box_error_out_of_bounds(box, NULL, type,
                                               start_offset,
                                               BOX_START_OFFSET_MIN_SPAN,
                                               bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_START_OFFSET_MIN_SPAN:
        if (box->start_offset_used >= 0) {
            if (start_offset < box->start_offset_used) {
                return box_error_out_of_bounds(box, NULL, type,
                                               start_offset,
                                               BOX_START_OFFSET_USED, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_START_OFFSET_USED:
        if (box->start_offset_max_span >= 0) {
            if (start_offset < box->start_offset_max_span) {
                return box_error_out_of_bounds(box, NULL, type,
                                               start_offset,
                                               BOX_START_OFFSET_MAX_SPAN,
                                               bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_START_OFFSET_SLACK:
        if (box->start_offset_parent >= 0) {
            if (start_offset < box->start_offset_parent) {
                return box_error_out_of_bounds(box, NULL, type,
                                               start_offset,
                                               BOX_START_OFFSET_PARENT, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_START_OFFSET_PARENT:
    case BOX_START_OFFSET_MAX_SPAN:
        break ;
    default:
        assert(0);
    }

    switch (type) {
    case BOX_START_OFFSET_PARENT:
        if (box->start_offset_slack >= 0) {
            if (start_offset > box->start_offset_slack) {
                return box_error_out_of_bounds(box, NULL, type,
                                               start_offset,
                                               BOX_START_OFFSET_SLACK, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_START_OFFSET_SLACK:
    case BOX_START_OFFSET_MAX_SPAN:
        if (box->start_offset_used >= 0) {
            if (start_offset > box->start_offset_used) {
                return box_error_out_of_bounds(box, NULL, type,
                                               start_offset,
                                               BOX_START_OFFSET_USED, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_START_OFFSET_USED:
        if (box->start_offset_min_span >= 0) {
            if (start_offset > box->start_offset_min_span) {
                return box_error_out_of_bounds(box, NULL, type,
                                               start_offset,
                                               BOX_START_OFFSET_MIN_SPAN, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_START_OFFSET_MIN_SPAN:
        if (start_offset > box_get_offset(box, BOX_START_OFFSET_HARD_MIN)) {
            return box_error_out_of_bounds(box, NULL, type,
                                           start_offset,
                                           BOX_START_OFFSET_HARD_MIN, bst);
        }
        /*FALLTHROUGH*/
    case BOX_START_OFFSET_HARD_MIN:
        break ;
    default:
        assert(0);
    }
    return BITPUNCH_OK;
}


static bitpunch_status_t
box_check_end_offset(struct box *box, int64_t end_offset,
                     enum box_offset_type type,
                     struct browse_state *bst)
{
    switch (type) {
    case BOX_END_OFFSET_HARD_MIN:
        if (box->end_offset_min_span >= 0) {
            if (end_offset > box->end_offset_min_span) {
                return box_error_out_of_bounds(box, NULL, type,
                                               end_offset,
                                               BOX_END_OFFSET_MIN_SPAN, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_MIN_SPAN:
        if (box->end_offset_used >= 0) {
            if (end_offset > box->end_offset_used) {
                return box_error_out_of_bounds(box, NULL, type,
                                               end_offset,
                                               BOX_END_OFFSET_USED, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_USED:
        if (box->end_offset_max_span >= 0) {
            if (end_offset > box->end_offset_max_span) {
                return box_error_out_of_bounds(box, NULL, type,
                                               end_offset,
                                               BOX_END_OFFSET_MAX_SPAN,
                                               bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_SLACK:
        if (box->end_offset_parent >= 0) {
            if (end_offset > box->end_offset_parent) {
                return box_error_out_of_bounds(box, NULL, type,
                                               end_offset,
                                               BOX_END_OFFSET_PARENT, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_PARENT:
    case BOX_END_OFFSET_MAX_SPAN:
        break ;
    default:
        assert(0);
    }

    switch (type) {
    case BOX_END_OFFSET_PARENT:
        if (box->end_offset_slack >= 0) {
            if (end_offset < box->end_offset_slack) {
                return box_error_out_of_bounds(box, NULL, type,
                                               end_offset,
                                               BOX_END_OFFSET_SLACK, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_SLACK:
    case BOX_END_OFFSET_MAX_SPAN:
        if (box->end_offset_used >= 0) {
            if (end_offset < box->end_offset_used) {
                return box_error_out_of_bounds(box, NULL, type,
                                               end_offset,
                                               BOX_END_OFFSET_USED, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_USED:
        if (box->end_offset_min_span >= 0) {
            if (end_offset < box->end_offset_min_span) {
                return box_error_out_of_bounds(box, NULL, type,
                                               end_offset,
                                               BOX_END_OFFSET_MIN_SPAN, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_MIN_SPAN:
        if (end_offset < box_get_offset(box, BOX_END_OFFSET_HARD_MIN)) {
            return box_error_out_of_bounds(box, NULL, type,
                                           end_offset,
                                           BOX_END_OFFSET_HARD_MIN, bst);
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_HARD_MIN:
        break ;
    default:
        assert(0);
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_set_start_offset(struct box *box, int64_t start_offset,
                     enum box_offset_type type,
                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = box_check_start_offset(box, start_offset, type, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    switch (type) {
    case BOX_START_OFFSET_PARENT:
        box->start_offset_parent = start_offset;
        break ;
    case BOX_START_OFFSET_SLACK:
        box->start_offset_slack = start_offset;
        break ;
    case BOX_START_OFFSET_MAX_SPAN:
        bt_ret = box_compute_slack_size(box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        box->start_offset_max_span = MAX(start_offset,
                                         box->start_offset_slack);
        break ;
    case BOX_START_OFFSET_USED:
        box->start_offset_used = start_offset;
        break ;
    case BOX_START_OFFSET_MIN_SPAN: {
        int64_t start_offset_hard_min;

        start_offset_hard_min = box->end_offset_used -
            ast_node_get_min_span_size(box->dpath.filter);
        box->start_offset_min_span = MIN(start_offset, start_offset_hard_min);
        break ;
    }
    case BOX_START_OFFSET_HARD_MIN:
        break ;
    default:
        assert(0);
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_set_end_offset(struct box *box, int64_t end_offset,
                   enum box_offset_type type,
                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = box_check_end_offset(box, end_offset, type, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    switch (type) {
    case BOX_END_OFFSET_PARENT:
        box->end_offset_parent = end_offset;
        break ;
    case BOX_END_OFFSET_SLACK:
        box->end_offset_slack = end_offset;
        break ;
    case BOX_END_OFFSET_MAX_SPAN:
        bt_ret = box_compute_slack_size(box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        box->end_offset_max_span = MIN(end_offset, box->end_offset_slack);
        break ;
    case BOX_END_OFFSET_USED:
        box->end_offset_used = end_offset;
        break ;
    case BOX_END_OFFSET_MIN_SPAN: {
        int64_t end_offset_hard_min;

        end_offset_hard_min = box->start_offset_used +
            ast_node_get_min_span_size(box->dpath.filter);
        box->end_offset_min_span = MAX(end_offset, end_offset_hard_min);
        break ;
    }
    case BOX_END_OFFSET_HARD_MIN:
        break ;
    default:
        assert(0);
    }
    return BITPUNCH_OK;
}

static const char *
box_offset_type_str(enum box_offset_type type)
{
    switch (type) {
    case BOX_START_OFFSET_PARENT:
        return "parent start";
    case BOX_START_OFFSET_SLACK:
        return "slack start";
    case BOX_START_OFFSET_MAX_SPAN:
        return "max span start";
    case BOX_START_OFFSET_USED:
        return "used start";
    case BOX_START_OFFSET_MIN_SPAN:
        return "min span start";
    case BOX_START_OFFSET_HARD_MIN:
        return "hard min start";
    case BOX_END_OFFSET_PARENT:
        return "parent end";
    case BOX_END_OFFSET_SLACK:
        return "slack end";
    case BOX_END_OFFSET_MAX_SPAN:
        return "max span end";
    case BOX_END_OFFSET_USED:
        return "used end";
    case BOX_END_OFFSET_MIN_SPAN:
        return "min span end";
    case BOX_END_OFFSET_HARD_MIN:
        return "hard min end";
    default:
        return "(bad offset type)";
    }
}

static bitpunch_status_t
box_set_size(struct box *box, int64_t box_size,
             enum box_offset_type size_type,
             struct browse_state *bst)
{
    if (0 != (box->flags & BOX_RALIGN)) {
        assert(-1 != box->end_offset_used);
        return box_set_start_offset(box, box->end_offset_used - box_size,
                                    (size_type & BOX_START_OFFSETS), bst);
    } else {
        assert(-1 != box->start_offset_used);
        return box_set_end_offset(box, box->start_offset_used + box_size,
                                  (size_type & BOX_END_OFFSETS), bst);
    }
}

static bitpunch_status_t
box_set_min_span_size(struct box *box, int64_t min_span_size,
                      struct browse_state *bst)
{
    return box_set_size(box, min_span_size, BOX_SIZE_MIN_SPAN, bst);
}

static bitpunch_status_t
box_set_used_size(struct box *box, int64_t used_size,
                  struct browse_state *bst)
{
    return box_set_size(box, used_size, BOX_SIZE_USED, bst);
}

static bitpunch_status_t
box_set_max_span_size(struct box *box, int64_t max_span_size,
                      struct browse_state *bst)
{
    return box_set_size(box, max_span_size, BOX_SIZE_MAX_SPAN, bst);
}


static void
box_set_boundary_offset(struct box *box, int64_t boundary_offset)
{
    if (0 != (box->flags & BOX_RALIGN)) {
        box->end_offset_slack = boundary_offset;
        box->end_offset_max_span = boundary_offset;
        box->end_offset_used = boundary_offset;
        box->end_offset_min_span = boundary_offset;
    } else {
        box->start_offset_slack = boundary_offset;
        box->start_offset_max_span = boundary_offset;
        box->start_offset_used = boundary_offset;
        box->start_offset_min_span = boundary_offset;
    }
}

static void
box_setup_overlay_offsets(struct box *box, struct box *parent_box,
                          int64_t boundary_offset)
{
    int box_is_right_aligned;

    if (0 != (box->flags & BOX_RALIGN)) {
        box_is_right_aligned =
            0 == (box->track_path.flags & TRACK_PATH_HEADER);
    } else {
        box_is_right_aligned =
            0 != (box->track_path.flags & TRACK_PATH_TRAILER);
    }
    if (box_is_right_aligned) {
        box->flags |= BOX_RALIGN;
    }
    if (NULL != parent_box) {
        box->start_offset_parent = box_get_known_start_offset(parent_box);
        box->end_offset_parent = box_get_known_end_offset(parent_box);
    }
    if (-1 == boundary_offset) {
        assert(NULL != parent_box);
        boundary_offset = 0 != (box->flags & BOX_RALIGN) ?
            box_get_known_end_offset(parent_box) :
            box_get_known_start_offset(parent_box);
    }
    box_set_boundary_offset(box, boundary_offset);
}

static bitpunch_status_t
box_construct(struct box *o_box,
              struct box *parent_box,
              struct dpath_node *dpath,
              int64_t boundary_offset,
              enum box_flag box_flags,
              struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    // first initialize the structural fields, so we get a valid box
    // in case of errors (as bst->last_error will then have a
    // reference to the box)
    TAILQ_INIT(&o_box->cached_children);
    o_box->use_count = 1;

    if (NULL != parent_box
        && parent_box->depth_level == BOX_MAX_DEPTH_LEVEL) {
        return box_error(BITPUNCH_DATA_ERROR, parent_box, dpath->filter, bst,
                         "reached maximum box nesting level %d",
                         BOX_MAX_DEPTH_LEVEL);
    }
    o_box->dpath = *dpath;
    o_box->flags = box_flags;
    o_box->start_offset_parent = -1;
    o_box->start_offset_slack = -1;
    o_box->start_offset_max_span = -1;
    o_box->start_offset_used = -1;
    o_box->start_offset_min_span = -1;
    o_box->end_offset_parent = -1;
    o_box->end_offset_slack = -1;
    o_box->end_offset_max_span = -1;
    o_box->end_offset_used = -1;
    o_box->end_offset_min_span = -1;
    if (NULL != parent_box) {
        assert(parent_box != o_box);
        o_box->parent_box = parent_box;
        o_box->file_hdl = NULL;
        o_box->depth_level = parent_box->depth_level + 1;
        box_acquire(parent_box);
    }
    if (-1 != boundary_offset) {
        box_setup_overlay_offsets(o_box, parent_box, boundary_offset);
        bt_ret = box_set_size(o_box, ast_node_get_min_span_size(dpath->filter),
                              BOX_SIZE_HARD_MIN, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    /* initialize internal state */
    switch (dpath->filter->ndat->type) {
    case AST_NODE_TYPE_ARRAY: {
        struct filter_instance_array *array;

        array = (struct filter_instance_array *)
            dpath->filter->ndat->u.rexpr_filter.f_instance;
        o_box->u.array_generic.n_items = -1;
        o_box->u.array.last_cached_index = -1;
        o_box->u.array.last_cached_item_offset = -1;
        if (0 != ((ast_node_get_target_item(
                       array->item_type.item)->ndat->u.item.flags)
                  & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            box_init_mark_offsets_repo(o_box);
            o_box->u.array.cache_log2_n_keys_per_mark =
                BOX_INDEX_CACHE_DEFAULT_LOG2_N_KEYS_PER_MARK;
        }
        if (ast_node_is_indexed(dpath->filter)) {
            box_init_index_cache_by_key(o_box);
            o_box->u.array.cache_log2_n_keys_per_mark =
                log2_i(bloom_book_suggested_n_words_per_mark(
                           o_box->u.array.cache_by_key));
        }
        break ;
    }
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
        o_box->u.array_generic.n_items = -1;
        break ;
    case AST_NODE_TYPE_REXPR_FILTER: {
        if (NULL != dpath->filter->ndat->u.rexpr_filter.f_instance->b_box.init) {
            bt_ret = dpath->filter->ndat->u.rexpr_filter.f_instance->b_box.init(o_box);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
        }
        break ;
    }
    default:
        break ;
    }
    return BITPUNCH_OK;
}

static void
box_dump_flags(const struct box *box, FILE *out)
{
    static const char *flag_desc[] = {
        "COMPUTING_SPAN_SIZE",
        "COMPUTING_MAX_SLACK_OFFSET",
        "BOX_CACHED",
        "BOX_RALIGN",
        "BOX_FILTER",
        "BOX_DATA_FILTER",
        "BOX_FILTER_APPLIED",
    };
    int flag;
    int i;
    int first;

    first = TRUE;
    for (i = 0, flag = 1; flag <= box->flags; ++i, flag <<= 1) {
        if (0 != (box->flags & flag)) {
            fprintf(out, "%s%s",
                    (first ? "" : ","), flag_desc[i]);
            first = FALSE;
        }
    }
}

static void
box_dump_internal(const struct box *box, FILE *out, int indent)
{
    if (NULL != box->parent_box) {
        box_dump_internal(box->parent_box, out, indent);
    }
    fprintf(out,
            "%*sBOX @",
            (indent + box->depth_level) * 4, "");
    box_dump_abs_dpath(box, out);
    fprintf(out,
            ": [%"PRIi64"p..[%"PRIi64"s..[%"PRIi64"M.."
            "[%"PRIi64"u..[%"PRIi64"m..%"PRIi64"m].."
            "%"PRIi64"u]..%"PRIi64"M]..%"PRIi64"s]"
            "..%"PRIi64"p]\n",
            box->start_offset_parent, box->start_offset_slack,
            box->start_offset_max_span, box->start_offset_used,
            box->start_offset_min_span,
            box->end_offset_min_span, box->end_offset_used,
            box->end_offset_max_span, box->end_offset_slack,
            box->end_offset_parent);
    fprintf(out,
            "%*stype: %s ftype: %s flags: ",
            (indent + box->depth_level) * 4, "",
            ast_node_type_str(box->dpath.item->ndat->type),
            (NULL != box->dpath.filter ?
             ast_node_type_str(box->dpath.filter->ndat->type) : "N/A"));
    box_dump_flags(box, out);
    fprintf(out,
            "\n%*sinternals: use_count=%d n_cached_children=%d\n",
            (indent + box->depth_level) * 4, "",
            box->use_count, box->n_cached_children);
    fprintf(out, "\n");
}

void
box_dump(const struct box *box)
{
    box_fdump(box, stdout);
}

void
box_fdump(const struct box *box, FILE *out)
{
    box_dump_internal(box, out, 0);
}

static void
box_cache_init(struct box_cache *cache,
               int max_n_boxes, int max_n_cached_children)
{
    assert(max_n_boxes > 0);
    assert(max_n_cached_children > 0);
    assert(max_n_cached_children <= max_n_boxes);
    TAILQ_INIT(&cache->mru_boxes);
    cache->n_boxes = 0;
    cache->max_n_boxes = max_n_boxes;
    cache->max_n_cached_children = max_n_cached_children;
}

static struct box_cache *
box_cache_new(int max_n_boxes, int max_n_cached_children)
{
    struct box_cache *cache;

    cache = new_safe(struct box_cache);
    box_cache_init(cache, max_n_boxes, max_n_cached_children);
    return cache;
}

void
box_cache_clear(struct box_cache *cache)
{
    struct box *box;
    struct box *tbox;

    TAILQ_FOREACH_SAFE(box, &cache->mru_boxes, cached_boxes_list, tbox) {
        box_delete_non_null(box);
    }
    TAILQ_INIT(&cache->mru_boxes);
    cache->n_boxes = 0;
}

void
box_cache_free(struct box_cache *cache)
{
    box_cache_clear(cache);
    free(cache);
}

void
box_cache_fdump(struct box_cache *cache, FILE *out)
{
    struct box *box;

    fprintf(out, "=== BOX CACHE CONTENTS ===:\n");
    TAILQ_FOREACH(box, &cache->mru_boxes, cached_boxes_list) {
        box_fdump(box, out);
    }
}

#ifndef DISABLE_BOX_CACHING
static int
box_is_cached(struct box *box)
{
    return 0 != (box->flags & BOX_CACHED);
}

static void
box_cache_remove_box(struct box_cache *cache, struct box *box)
{
    DBG_BOX_DUMP(box);
    TAILQ_REMOVE(&cache->mru_boxes, box, cached_boxes_list);
    if (NULL != box->parent_box) {
        TAILQ_REMOVE(&box->parent_box->cached_children, box,
                     cached_children_list);
        assert(box->parent_box->n_cached_children > 0);
        --box->parent_box->n_cached_children;
    }
    box->cached_boxes_list.tqe_prev = NULL;
    box->flags &= ~BOX_CACHED;
    box_delete_non_null(box);
}
#endif

static void
box_update_cache(struct box *box, struct browse_state *bst)
{
#ifndef DISABLE_BOX_CACHING
    struct box_cache *cache;

    DBG_BOX_DUMP(box);
    cache = box->file_hdl->box_cache;
    if (box_is_cached(box)) {
        TAILQ_REMOVE(&cache->mru_boxes, box, cached_boxes_list);
        /* also reposition the box in children list in front to
         * optimize performance (MRU in front heuristic) */
        if (NULL != box->parent_box) {
            TAILQ_REMOVE(&box->parent_box->cached_children, box,
                         cached_children_list);
        }
    } else {
        box_acquire(box);
        if (NULL != box->parent_box
            && box->parent_box->n_cached_children
            == cache->max_n_cached_children) {
            box_cache_remove_box(cache, TAILQ_LAST(
                                     &box->parent_box->cached_children,
                                     box_tailq));
        } else if (cache->n_boxes == cache->max_n_boxes) {
            box_cache_remove_box(cache, TAILQ_LAST(&cache->mru_boxes,
                                                   box_tailq));
        } else {
            assert(cache->n_boxes < cache->max_n_boxes);
            ++cache->n_boxes;
        }
        if (NULL != box->parent_box) {
            assert(box->parent_box->n_cached_children <
                   cache->max_n_cached_children);
            ++box->parent_box->n_cached_children;
        }
        box->flags |= BOX_CACHED;
    }
    TAILQ_INSERT_HEAD(&cache->mru_boxes, box, cached_boxes_list);
    if (NULL != box->parent_box) {
        TAILQ_INSERT_HEAD(&box->parent_box->cached_children, box,
                          cached_children_list);
    }
#endif
}

static struct box *
box_lookup_cached_child(struct box *box, struct track_path path,
                        enum box_flag flags)
{
#ifndef DISABLE_BOX_CACHING
    struct box *child_box;

    DBG_BOX_DUMP(box);
    TAILQ_FOREACH(child_box, &box->cached_children, cached_children_list) {
        if (track_path_eq(child_box->track_path, path)
            && (child_box->flags & BOX_RALIGN) == (flags & BOX_RALIGN)) {
            return child_box;
        }
    }
#endif
    return NULL;
}

static struct box *
box_new_from_file_internal(const struct bitpunch_schema_hdl *def_hdl,
                           struct bitpunch_binary_file_hdl *file_hdl,
                           struct browse_state *bst)
{
    struct box *box;
    struct dpath_node *root;
    bitpunch_status_t bt_ret;

    root = def_hdl->df_file_block.root;
    assert(NULL != root);
    box = new_safe(struct box);
    bt_ret = box_construct(box, NULL, root, 0, 0u, bst);
    if (BITPUNCH_OK != bt_ret) {
        /* TODO error reporting */
        box_delete_non_null(box);
        return NULL;
    }
    box->file_hdl = file_hdl;
    box->flags |= BOX_FILTER | BOX_FILTER_APPLIED;
    bt_ret = box_set_end_offset(box, file_hdl->bf_data_length,
                                BOX_END_OFFSET_SLACK, bst);
    if (BITPUNCH_OK != bt_ret) {
        /* TODO error reporting */
        box_delete(box);
        return NULL;
    }
    if (NULL == file_hdl->box_cache) {
        file_hdl->box_cache = box_cache_new(BOX_CACHE_MAX_N_BOXES,
                                            BOX_CACHE_MAX_N_CACHED_CHILDREN);
    }
    return box;
}

struct box *
box_new_from_file(const struct bitpunch_schema_hdl *def_hdl,
                  struct bitpunch_binary_file_hdl *file_hdl)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return box_new_from_file_internal(def_hdl, file_hdl, &bst);
}

struct box *
box_new_slice_box(struct tracker *slice_start,
                  struct tracker *slice_end,
                  struct browse_state *bst)
{
    struct box *slice_box;
    bitpunch_status_t bt_ret;
    struct dpath_node slice_node;
    int64_t index_start;
    int64_t index_end;
    struct track_path slice_path;
    int64_t slice_start_offset_used;

    if (NULL != slice_end && slice_start->box != slice_end->box) {
        (void) tracker_error(BITPUNCH_INVALID_PARAM, slice_start, NULL, bst,
                             "can't set tracker slice: end of slice "
                             "tracker must track the same box");
        return NULL;
    }

    switch (slice_start->box->dpath.filter->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
        slice_node = *DPATH_NODE_ARRAY_SLICE;
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_REXPR_FILTER:
        slice_node = *DPATH_NODE_BYTE_SLICE;
        break ;
    default:
        (void) tracker_error(BITPUNCH_INVALID_PARAM, slice_start, NULL, bst,
                             "can't set tracker slice: does not track "
                             "an array");
        return NULL;
    }
    index_start = slice_start->cur.u.array.index;
    if (-1 == index_start) {
        index_start = 0;
    }
    index_end = (NULL != slice_end ? slice_end->cur.u.array.index : -1);
    if (index_end != -1 && index_end < index_start) {
        (void) tracker_error(BITPUNCH_DATA_ERROR, slice_start, NULL,
                             bst,
                             "slice end index %"PRIi64" before "
                             "start index %"PRIi64"",
                             index_end, index_start);
        return NULL;
    }

    // FIXME this may not work for RALIGN track path
    slice_path = track_path_from_array_slice(index_start, index_end);
    if (-1 != slice_start->cur.u.array.index) {
        if (-1 == slice_start->item_offset) {
            bt_ret = tracker_compute_item_offset(slice_start, bst);
            if (BITPUNCH_OK != bt_ret) {
                return NULL;
            }
            assert(slice_start->item_offset >= 0);
        }
        slice_start_offset_used = slice_start->item_offset;
    } else {
        slice_start_offset_used = slice_start->box->start_offset_used;
    }
    slice_box = new_safe(struct box);
    bt_ret = box_construct(slice_box,
                           slice_start->box, &slice_node,
                           slice_start_offset_used, 0u, bst);
    if (BITPUNCH_OK != bt_ret) {
        box_delete_non_null(slice_box);
        DBG_TRACKER_CHECK_STATE(slice_start);
        return NULL;
    }
    slice_box->track_path = slice_path;
    if (0 != (TRACKER_AT_END & slice_start->flags)) {
        slice_box->u.array_generic.n_items = 0;
    }
    return slice_box;
}

struct box *
box_new_as_box(struct box *parent_box,
               struct dpath_node *as_dpath,
               struct browse_state *bst)
{
    struct box *as_box;
    bitpunch_status_t bt_ret;
    int64_t parent_size;

    if (BITPUNCH_OK != box_compute_max_span_size(parent_box, bst)) {
        return NULL;
    }
    as_box = new_safe(struct box);
    bt_ret = box_construct(as_box, parent_box,
                           as_dpath, parent_box->start_offset_used,
                           BOX_FILTER, bst);
    if (BITPUNCH_OK != bt_ret) {
        box_delete_non_null(as_box);
        return NULL;
    }
    as_box->track_path = TRACK_PATH_NONE;

    // limit the available space to the slack size of the parent
    parent_size = box_get_known_size(parent_box);
    bt_ret = box_set_size(as_box, parent_size, BOX_SIZE_SLACK, bst);
    if (BITPUNCH_OK != bt_ret) {
        box_delete_non_null(as_box);
        return NULL;
    }
    return as_box;
}

static struct bitpunch_binary_file_hdl *
create_data_hdl_from_buffer(
    const char *data, size_t data_size, int own_buffer,
    const struct bitpunch_binary_file_hdl *parent_hdl)
{
    struct bitpunch_binary_file_hdl *data_hdl;

    data_hdl = new_safe(struct bitpunch_binary_file_hdl);
    data_hdl->bf_open_type = (own_buffer ?
                              BF_OPEN_TYPE_OWN_BUFFER :
                              BF_OPEN_TYPE_USER_BUFFER);
    data_hdl->bf_data = data;
    data_hdl->bf_data_length = data_size;
    data_hdl->box_cache = parent_hdl->box_cache;
    return data_hdl;
}

struct box *
box_new_filter_box(struct box *parent_box,
                   struct ast_node_hdl *filter,
                   struct ast_node_hdl *item,
                   struct ast_node_hdl *filter_defining_used_size,
                   struct browse_state *bst)
{
    struct box *box;
    bitpunch_status_t bt_ret;
    struct dpath_node dpath;

    dpath_node_reset(&dpath);
    dpath.item = item;
    dpath.filter = filter;
    dpath.filter_defining_used_size = filter_defining_used_size;
    box = new_safe(struct box);
    bt_ret = box_construct(box, parent_box, &dpath, -1, BOX_FILTER, bst);
    if (BITPUNCH_OK != bt_ret) {
        box_delete_non_null(box);
        return NULL;
    }
    box->file_hdl = NULL; // filter not applied yet
    return box;
}

static bitpunch_status_t
box_apply_local_filter(struct box *box, struct browse_state *bst)
{
    const struct filter_class *filter_cls;
    const struct bitpunch_binary_file_hdl *parent_file_hdl;
    struct bitpunch_binary_file_hdl *filtered_data_hdl;
    const char *unfiltered_data;
    int64_t unfiltered_size;
    expr_value_t filtered_value;
    const char *filtered_data;
    int64_t filtered_size;
    bitpunch_status_t bt_ret;

    assert(NULL == box->file_hdl);
    assert(NULL != box->parent_box);
    filter_cls = box->dpath.filter->ndat->u.rexpr_filter.filter_cls;
    parent_file_hdl = box->parent_box->file_hdl;
    if (0 == (box->flags & BOX_FILTER)) {
        box->file_hdl = parent_file_hdl;
        return BITPUNCH_OK;
    }
    if (!expr_value_type_mask_contains_dpath(
            filter_cls->value_type_mask)) {
        box->file_hdl = parent_file_hdl;
        box_setup_overlay_offsets(box, box->parent_box, -1);
        return BITPUNCH_OK;
    }
    bt_ret = box_get_used_size(box->parent_box, &unfiltered_size, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    unfiltered_data = parent_file_hdl->bf_data
        + box->parent_box->start_offset_used;
    // FIXME check "box" is the right scope
    bt_ret = filter_instance_read_value(
        box->dpath.filter, box, unfiltered_data, unfiltered_size,
        &filtered_value, bst);
    if (BITPUNCH_OK != bt_ret) {
        return box_error(bt_ret, box, box->dpath.filter, bst,
                         "error reading value through filter");
    }
    if (!expr_value_type_mask_contains_dpath(filtered_value.type)) {
        // dynamic evaluation did not yield a dpath type
        expr_value_destroy(filtered_value);
        box->file_hdl = parent_file_hdl;
        box_setup_overlay_offsets(box, box->parent_box, -1);
        return BITPUNCH_OK;
    }
    switch (filtered_value.type) {
    case EXPR_VALUE_TYPE_STRING:
        filtered_data = filtered_value.string.str;
        filtered_size = filtered_value.string.len;
        break ;
    case EXPR_VALUE_TYPE_BYTES:
        filtered_data = filtered_value.bytes.buf;
        filtered_size = filtered_value.bytes.len;
        break ;
    default:
        assert(0);
    }
    if (0 == filtered_size
        || (filtered_data >= parent_file_hdl->bf_data &&
            filtered_data < parent_file_hdl->bf_data
            + parent_file_hdl->bf_data_length)) {
        box->file_hdl = parent_file_hdl;
        box_setup_overlay_offsets(box, box->parent_box, -1);
    } else {
        filtered_data_hdl = create_data_hdl_from_buffer(
            filtered_data, filtered_size, TRUE, parent_file_hdl);
        box->file_hdl = filtered_data_hdl;
        box->flags |= BOX_DATA_SOURCE;
        box_set_boundary_offset(box, 0);
    }
    expr_value_destroy(filtered_value);
    return box_set_used_size(box, filtered_size, bst);
}

bitpunch_status_t
box_apply_filter_internal(struct box *box,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    if (0 != (box->flags & BOX_FILTER_APPLIED)) {
        return BITPUNCH_OK;
    }
    if (NULL != box->parent_box) {
        bt_ret = box_apply_filter_internal(box->parent_box, bst);
    } else {
        // root box
        bt_ret = BITPUNCH_OK;
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = box_apply_local_filter(box, bst);
    if (BITPUNCH_OK == bt_ret) {
        assert(NULL != box->file_hdl);
        box->flags |= BOX_FILTER_APPLIED;
    }
    return bt_ret;
}

static struct box *
box_get_container_parent_box(struct box *box)
{
    while (TRUE) {
        switch (box->dpath.filter->ndat->type) {
        case AST_NODE_TYPE_COMPOSITE:
        case AST_NODE_TYPE_ARRAY:
            return box;
        default:
            box = box->parent_box;
            break ;
        }
    }
    return box;
}

static struct box *
box_get_scope_box(struct box *box)
{
    struct box *scope;

    scope = box;
    while (NULL != scope
           && AST_NODE_TYPE_COMPOSITE != scope->dpath.filter->ndat->type) {
        scope = scope->parent_box;
    }
    return scope;
}

static void
box_free(struct box *box)
{
    struct ast_node_hdl *item;

    item = box->dpath.filter;
    /* destroy internal state */
    if (NULL != item) {
        switch (item->ndat->type) {
        case AST_NODE_TYPE_ARRAY:
            if (box_index_cache_exists(box)) {
                box_destroy_index_cache_by_key(box);
            }
            box_destroy_mark_offsets_repo(box);
            break ;
        case AST_NODE_TYPE_REXPR_FILTER:
            if (NULL != item->ndat->u.rexpr_filter.f_instance->b_box.destroy) {
                item->ndat->u.rexpr_filter.f_instance->b_box.destroy(box);
            }
            break ;
        default:
            break ;
        }
    }
    if (0 != (box->flags & BOX_DATA_SOURCE)) {
        (void)bitpunch_close_binary_file(
            (struct bitpunch_binary_file_hdl *)box->file_hdl);
        free((struct bitpunch_binary_file_hdl *)box->file_hdl);
    }
    free(box);
}

void
box_acquire(struct box *box)
{
    if (NULL != box) {
        ++box->use_count;
    }
}

static void
box_delete_non_null(struct box *box)
{
    assert(box->use_count > 0);
    --box->use_count;
    if (0 == box->use_count) {
        box_delete(box->parent_box);
        box_free(box);
    }
}

void
box_delete(struct box *box)
{
    if (NULL != box) {
        box_delete_non_null(box);
    }
}

/*
 * index management
 */

static void
box_init_index_cache_by_key(struct box *box)
{
    box->u.array.cache_by_key = bloom_book_create();
}

static int
box_index_cache_exists(const struct box *box)
{
    switch (box->dpath.filter->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
        return (NULL != box->u.array.cache_by_key);
    default:
        return FALSE;
    }
}

int
box_contains_indexed_items(const struct box *box)
{
    switch (box->dpath.filter->ndat->type) {
    case AST_NODE_TYPE_ARRAY_SLICE:
        return box_contains_indexed_items(box->parent_box);
    default:
        return ast_node_is_indexed(box->dpath.filter);
    }
}

enum expr_value_type
box_get_index_type(const struct box *box)
{
    switch (box->dpath.filter->ndat->type) {
    case AST_NODE_TYPE_ARRAY_SLICE:
        return box_get_index_type(box->parent_box);
    default:
        return ast_node_get_key_type(box->dpath.filter);
    }
}

struct ast_node_hdl *
box_get_key_expr(const struct box *box)
{
    switch (box->dpath.filter->ndat->type) {
    case AST_NODE_TYPE_ARRAY_SLICE:
        return box_get_key_expr(box->parent_box);
    default:
        return ast_node_get_key_expr(box->dpath.filter);
    }
}

static void
box_destroy_index_cache_by_key(struct box *box)
{
    bloom_book_destroy(box->u.array.cache_by_key);
    box->u.array.cache_by_key = NULL;
}

static void
box_init_mark_offsets_repo(struct box *box)
{
    ARRAY_INIT(&box->u.array.mark_offsets, 0,
               struct index_cache_mark_offset);
}

static int
box_mark_offsets_repo_exists(const struct box *box)
{
    struct filter_instance_array *array;

    array = (struct filter_instance_array *)
        box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    return 0 != (array->item_type.item->ndat->u.item.flags
                 & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC);
}

static void
box_destroy_mark_offsets_repo(struct box *box)
{
    ARRAY_DESTROY(&box->u.array.mark_offsets);
}

static int
box_array_index_is_marked(struct box *box, int64_t index)
{
    return 0 == (index & ((1 << box->u.array.cache_log2_n_keys_per_mark)
                          - 1));
}

static int64_t
box_array_get_index_mark(struct box *box, int64_t index)
{
    return index >> box->u.array.cache_log2_n_keys_per_mark;
}

static int64_t
box_array_get_mark_start_index(struct box *box, int64_t mark)
{
    return mark << box->u.array.cache_log2_n_keys_per_mark;
}

static int64_t
box_array_get_mark_offset_at_index(struct box *box, int64_t index)
{
    int64_t marks_index;

    marks_index = box_array_get_index_mark(box, index);
    assert(ARRAY_SIZE(&box->u.array.mark_offsets) > marks_index);

    return ARRAY_ITEM(&box->u.array.mark_offsets, marks_index).item_offset;
}

static void
box_array_add_mark_offset(struct box *box,
                          int64_t mark, int64_t item_offset)
{
    struct index_cache_mark_offset mark_offset;

    assert(ARRAY_SIZE(&box->u.array.mark_offsets) == mark);

    mark_offset.item_offset = item_offset;
    ARRAY_APPEND(&box->u.array.mark_offsets, mark_offset,
                 struct index_cache_mark_offset);
}

static void
tracker_index_cache_add_item(struct tracker *tk, expr_value_t item_key)
{
    const char *key_buf;
    int64_t key_len;
    int64_t mark;

    DBG_TRACKER_DUMP(tk);
    assert(AST_NODE_TYPE_ARRAY == tk->box->dpath.filter->ndat->type);
    assert(tk->cur.u.array.index ==
           tk->box->u.array.last_cached_index + 1);

    if (box_index_cache_exists(tk->box)) {
        if (box_array_index_is_marked(tk->box, tk->cur.u.array.index)) {
            mark = (int64_t)bloom_book_add_mark(
                tk->box->u.array.cache_by_key);
            if (box_mark_offsets_repo_exists(tk->box)) {
                box_array_add_mark_offset(tk->box, mark, tk->item_offset);
            }
        }
        expr_value_to_hashable(item_key, &key_buf, &key_len);
        bloom_book_insert_word(tk->box->u.array.cache_by_key,
                               key_buf, key_len);
    } else {
        if (box_mark_offsets_repo_exists(tk->box)
            && box_array_index_is_marked(tk->box, tk->cur.u.array.index)) {
            mark = box_array_get_index_mark(tk->box, tk->cur.u.array.index);
            box_array_add_mark_offset(tk->box, mark, tk->item_offset);
        }
    }
    tk->box->u.array.last_cached_index = tk->cur.u.array.index;
    tk->box->u.array.last_cached_item_offset = tk->item_offset;
}


struct index_cache_iterator {
    struct bloom_book_cookie bloom_cookie;
    struct tracker *xtk;
    expr_value_t key;
    bloom_book_mark_t mark;
    struct track_path in_slice_path;
    bloom_book_mark_t from_mark;
    int first;
};

/**
 * @brief Lookup items twins which key match @ref item_key, return an
 * iterator in @ref iterp.
 *
 * @param tk
 * @param item_key
 * @param from_path if not TRACK_PATH_NONE, lookup starting from this
 * path.
 * @param[out] iterp returned iterator object, to be cleaned with
 * index_cache_iterator_done() when done iterating, whether an error
 * occurs or not during iteration.
 */
static void
box_index_cache_lookup_key_twins(struct box *box,
                                 expr_value_t item_key,
                                 struct track_path in_slice_path,
                                 struct index_cache_iterator *iterp,
                                 struct browse_state *bst)
{
    struct filter_instance_array *array;
    struct dpath_node *item_dpath;
    const char *key_buf;
    int64_t key_len;
    bloom_book_mark_t from_mark;

    DBG_BOX_DUMP(box);
    assert(AST_NODE_TYPE_ARRAY == box->dpath.filter->ndat->type);
    assert(box_index_cache_exists(box));
    array = (struct filter_instance_array *)
        box->dpath.filter->ndat->u.rexpr_filter.f_instance;

    expr_value_to_hashable(item_key, &key_buf, &key_len);

    if (! track_path_eq(in_slice_path, TRACK_PATH_NONE)) {
        assert(TRACK_PATH_ARRAY_SLICE == in_slice_path.type);
        from_mark = box_array_get_index_mark(box,
                                             in_slice_path.u.array.index);
    } else {
        from_mark = BLOOM_BOOK_MARK_NONE;
    }
    bloom_book_lookup_word_from_mark(box->u.array.cache_by_key,
                                     key_buf, key_len, from_mark,
                                     &iterp->bloom_cookie);
    iterp->key = item_key;
    iterp->xtk = tracker_new(box);
    iterp->in_slice_path = in_slice_path;
    iterp->from_mark = from_mark;
    iterp->mark = bloom_book_lookup_word_get_next_candidate(
        box->u.array.cache_by_key, &iterp->bloom_cookie);
    if (BLOOM_BOOK_MARK_NONE != iterp->mark) {
        item_dpath = &array->item_type;
        tracker_goto_mark_internal(iterp->xtk, item_dpath, iterp->mark, bst);
    }
    iterp->first = TRUE;
}

static bitpunch_status_t
index_cache_iterator_next_twin(struct index_cache_iterator *iter,
                               struct track_path *item_pathp,
                               struct browse_state *bst)
{
    struct filter_instance_array *array;
    struct tracker *xtk;
    bitpunch_status_t bt_ret;
    struct dpath_node *item_dpath;
    int64_t index_end;

    xtk = iter->xtk;
    if (BLOOM_BOOK_MARK_NONE == iter->mark) {
        return BITPUNCH_NO_ITEM;
    }
    if (!iter->first) {
        bt_ret = tracker_goto_next_item_internal(xtk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    array = (struct filter_instance_array *)
        xtk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    item_dpath = &array->item_type;
    index_end = iter->in_slice_path.u.array_slice.index_end;
    while (TRUE) {
        bt_ret = tracker_goto_next_key_match_in_mark(
            xtk, iter->key, iter->mark, bst);
        if (iter->mark == iter->from_mark) {
            while (BITPUNCH_OK == bt_ret
                   && (xtk->cur.u.array.index
                       < iter->in_slice_path.u.array.index)) {
                /* skip initial keys lying before from_path in the
                 * same mark */
                bt_ret = tracker_goto_next_item_internal(xtk, bst);
                if (BITPUNCH_OK == bt_ret) {
                    bt_ret = tracker_goto_next_key_match_in_mark(
                        xtk, iter->key, iter->mark, bst);
                }
            }
        }
        if (BITPUNCH_OK == bt_ret
            && -1 != index_end && xtk->cur.u.array.index >= index_end) {
            return BITPUNCH_NO_ITEM;
        }
        if (BITPUNCH_NO_ITEM != bt_ret) {
            break ;
        }
        iter->mark = bloom_book_lookup_word_get_next_candidate(
            xtk->box->u.array.cache_by_key, &iter->bloom_cookie);
        if (BLOOM_BOOK_MARK_NONE == iter->mark) {
            return BITPUNCH_NO_ITEM;
        }
        tracker_goto_mark_internal(xtk, item_dpath, iter->mark, bst);
    }
    if (BITPUNCH_OK == bt_ret) {
        *item_pathp = xtk->cur;
    }
    /* skip the previous check for next iterations */
    iter->from_mark = BLOOM_BOOK_MARK_NONE;
    iter->first = FALSE;
    return bt_ret;
}

static void
index_cache_iterator_done(struct index_cache_iterator *iter)
{
    tracker_delete(iter->xtk);
}

/**
 * @brief Lookup the twin index of @ref item_key at tracking position
 * of @ref tk.
 *
 * @param tk
 * @param item_key
 * @param[out] nth_twinp If the return value is BITPUNCH_OK, contains
 * the index of the current tracker item's key amongst all items
 * sharing @ref item_key. If the return value is BITPUNCH_NO_ITEM,
 * contains the highest cached item's twin index with key @ref
 * item_key, or -1 if no such item is cached.
 */
static bitpunch_status_t
tracker_index_cache_lookup_current_twin_index(
    struct tracker *tk,
    expr_value_t item_key,
    struct track_path in_slice_path,
    int *nth_twinp,
    struct browse_state *bst)
{
    struct index_cache_iterator twin_iter;
    bitpunch_status_t bt_ret;
    struct track_path item_path;
    int cur_twin;
 
    DBG_TRACKER_DUMP(tk);
    box_index_cache_lookup_key_twins(tk->box, item_key, in_slice_path,
                                     &twin_iter, bst);
    cur_twin = -1;
    do {
        bt_ret = index_cache_iterator_next_twin(&twin_iter, &item_path, bst);
        if (BITPUNCH_OK != bt_ret) {
            break ;
        }
        ++cur_twin;
    } while (! track_path_eq(item_path, tk->cur));

    if (BITPUNCH_OK == bt_ret || BITPUNCH_NO_ITEM == bt_ret) {
        *nth_twinp = cur_twin;
    }
    index_cache_iterator_done(&twin_iter);
    return bt_ret;
}

/**
 * @brief Go to the nth twin item whose key is @ref item_key.
 *
 * @param tk
 * @param item_key
 * @param nth_twin Rank of twin to go to
 * @param[out] last_twinp If the return value is BITPUNCH_NO_ITEM,
 * contains the highest cached item's twin index with key @ref
 * item_key, or -1 if no such item is cached.
 */
static bitpunch_status_t
tracker_index_cache_goto_twin(struct tracker *tk,
                              expr_value_t item_key,
                              int nth_twin,
                              struct track_path in_slice_path,
                              int *last_twinp,
                              struct browse_state *bst)
{
    struct index_cache_iterator twin_iter;
    bitpunch_status_t bt_ret;
    struct track_path item_path;
    int cur_twin;
 
    DBG_TRACKER_DUMP(tk);
    box_index_cache_lookup_key_twins(tk->box, item_key, in_slice_path,
                                     &twin_iter, bst);
    cur_twin = -1;
    do {
        bt_ret = index_cache_iterator_next_twin(&twin_iter, &item_path, bst);
        if (BITPUNCH_OK != bt_ret) {
            break ;
        }
        ++cur_twin;
    } while (nth_twin != cur_twin);

    if (BITPUNCH_OK == bt_ret) {
        tracker_set(tk, twin_iter.xtk);
    } else if (BITPUNCH_NO_ITEM == bt_ret) {
        *last_twinp = cur_twin;
    }
    index_cache_iterator_done(&twin_iter);
    return bt_ret;
}

static bitpunch_status_t
tracker_lookup_current_twin_index(struct tracker *tk,
                                  expr_value_t item_key,
                                  struct track_path in_slice_path,
                                  int *nth_twinp,
                                  struct browse_state *bst)

{
    bitpunch_status_t bt_ret;
    const struct ast_node_hdl *node;
    int cur_twin;
    struct tracker *xtk;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_index_cache_lookup_current_twin_index(tk, item_key,
                                                           in_slice_path,
                                                           nth_twinp, bst);
    if (BITPUNCH_NO_ITEM != bt_ret) {
        return bt_ret; /* BITPUNCH_OK included */
    }
    /* Browse the yet-uncached tail */
    node = tk->box->dpath.filter;
    xtk = tracker_dup(tk);
    tracker_goto_last_cached_item_internal(xtk, bst);
    cur_twin = *nth_twinp;
    do {
        bt_ret = tracker_goto_next_item_internal(xtk, bst);
        if (BITPUNCH_OK == bt_ret) {
            bt_ret = node->ndat->u.rexpr_filter.f_instance->b_tk.goto_next_key_match(
                xtk, item_key, TRACK_PATH_NONE, bst);
            if (BITPUNCH_OK == bt_ret
                && (-1 == in_slice_path.u.array.index
                    || (xtk->cur.u.array.index
                        >= in_slice_path.u.array.index))) {
                ++cur_twin;
            }
        }
    } while (BITPUNCH_OK == bt_ret && ! track_path_eq(xtk->cur, tk->cur));

    if (BITPUNCH_OK == bt_ret) {
        assert(track_path_eq(xtk->cur, tk->cur));
        *nth_twinp = cur_twin;
    }
    tracker_delete(xtk);
    return bt_ret;
}

static void
tracker_jump_to_item_internal(struct tracker *tk,
                              struct dpath_node *item_dpath,
                              struct track_path item_path,
                              int64_t item_offset,
                              struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    assert(NULL != item_dpath);
    assert(-1 != item_offset);
    tracker_set_dangling(tk);
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    tk->dpath.filter = item_dpath->filter;
    tk->cur = item_path;
    tk->item_offset = item_offset;
    DBG_TRACKER_CHECK_STATE(tk);
}

static void
tracker_goto_mark_internal(struct tracker *tk,
                           struct dpath_node *item_dpath,
                           int64_t mark,
                           struct browse_state *bst)
{
    struct track_path item_path;
    int64_t item_offset;

    DBG_TRACKER_DUMP(tk);
    memset(&item_path, 0, sizeof (item_path));
    item_path = track_path_from_array_index(
        box_array_get_mark_start_index(tk->box, mark));
    if (box_mark_offsets_repo_exists(tk->box)) {
        item_offset = box_array_get_mark_offset_at_index(
            tk->box, item_path.u.array.index);
    } else {
        assert(0 == (item_dpath->item->ndat->u.item.flags
                     & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC));
        item_offset = tk->box->start_offset_used
            + (item_path.u.array.index
               * ast_node_get_min_span_size(item_dpath->item));
    }
    tracker_jump_to_item_internal(tk, item_dpath, item_path, item_offset,
                                  bst);
}

static void
tracker_goto_last_cached_item_internal(struct tracker *tk,
                                       struct browse_state *bst)
{
    struct filter_instance_array *array;

    DBG_TRACKER_DUMP(tk);
    array = (struct filter_instance_array *)
        tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    assert(AST_NODE_TYPE_ARRAY == tk->box->dpath.filter->ndat->type);
    if (-1 != tk->box->u.array.last_cached_index) {
        tk->dpath.filter = array->item_type.filter;
        // FIXME should reset item to NULL if filter is dynamic
        tk->dpath.item = array->item_type.item;
        if (0 != (tk->dpath.item->ndat->u.item.flags
                  & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            tk->item_size = -1;
        }
        tk->item_offset = tk->box->u.array.last_cached_item_offset;
        box_delete(tk->item_box);
        tk->item_box = NULL;
        tk->flags &= ~TRACKER_AT_END;
        tk->cur.u.array.index = tk->box->u.array.last_cached_index;
    } else {
        tracker_rewind(tk);
    }
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    DBG_TRACKER_CHECK_STATE(tk);
}

static bitpunch_status_t
tracker_goto_next_key_match_in_mark(struct tracker *tk,
                                    expr_value_t key,
                                    int64_t mark,
                                    struct browse_state *bst)
{
    struct track_path search_boundary;

    DBG_TRACKER_DUMP(tk);
    /* limit search to keys belonging to the current mark's scope */
    search_boundary = track_path_from_array_index(
        box_array_get_mark_start_index(tk->box, mark + 1));

    return tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_tk.goto_next_key_match(
        tk, key, search_boundary, bst);
}


/*
 * tracker
 */

enum tracker_state
tracker_get_state(const struct tracker *tk)
{
    if (tracker_is_dangling(tk)) {
        if (0 != (tk->flags & TRACKER_AT_END)) {
            return TRACKER_STATE_AT_END;
        } else {
            return TRACKER_STATE_DANGLING;
        }
    } else if (-1 == tk->item_size) {
        if (0 != (tk->flags & TRACKER_AT_END)) {
            return TRACKER_STATE_AT_END;
        } else if (NULL != tk->item_box) {
            return TRACKER_STATE_ITEM_BOX;
        } else if (-1 != tk->item_offset) {
            return TRACKER_STATE_ITEM_OFFSET;
        } else {
            return TRACKER_STATE_ITEM;
        }
    } else {
        if (NULL != tk->item_box) {
            return TRACKER_STATE_ITEM_BOX_SIZE;
        } else {
            return TRACKER_STATE_ITEM_SIZE;
        }
    }
    /*NOT REACHED*/
}

static void
tracker_reset_dpath_internal(struct tracker *tk)
{
    tk->dpath.filter = NULL;
    tk->dpath.item = NULL;
}

static void
tracker_reset_item_cache_internal(struct tracker *tk)
{
    tk->item_size = -1;
    box_delete(tk->item_box);
    tk->item_box = NULL;
    tk->dpath.item = NULL;
}

static void
tracker_reset_item_cache(struct tracker *tk)
{
    DBG_TRACKER_DUMP(tk);
    tracker_reset_item_cache_internal(tk);
}

static void
tracker_reset_track_path(struct tracker *tk)
{
    struct ast_node_hdl *item;

    item = tk->box->dpath.filter;
    switch (item->ndat->type) {
    case AST_NODE_TYPE_COMPOSITE:
        tk->cur = track_path_from_composite_field(NULL);
        break ;
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
        tk->cur = track_path_from_array_index(-1);
        break ;
    case AST_NODE_TYPE_REXPR_FILTER:
        if (NULL != item->ndat->u.rexpr_filter.f_instance->b_tk.reset_track_path) {
            item->ndat->u.rexpr_filter.f_instance->b_tk.reset_track_path(tk);
        }
        break ;
    default:
        assert(0);
    }
}

static void
tracker_set_dangling_internal(struct tracker *tk)
{
    tracker_reset_item_cache_internal(tk);
    tracker_reset_dpath_internal(tk);
    tk->flags &= ~TRACKER_AT_END;
    tracker_reset_track_path(tk);
}

static void
tracker_set_dangling(struct tracker *tk)
{
    DBG_TRACKER_DUMP(tk);
    tracker_set_dangling_internal(tk);
}

static void
tracker_construct(struct tracker *o_tk,
                  struct box *box)
{
    assert(NULL != box);
    o_tk->box = box;
    box_acquire(box);
    o_tk->item_size = -1;
    o_tk->item_offset = -1;
    tracker_reset_track_path(o_tk);
}

static void
tracker_destroy(struct tracker *tk)
{
    box_delete_non_null(tk->box);
    box_delete(tk->item_box);
}

static struct tracker *
tracker_new(struct box *box)
{
    struct tracker *tk;

    tk = new_safe(struct tracker);
    tracker_construct(tk, box);
    return tk;
}

void
tracker_set(struct tracker *tk, const struct tracker *src_tk)
{
    box_acquire(src_tk->box);
    if (NULL != src_tk->item_box) {
        box_acquire(src_tk->item_box);
    }
    tracker_destroy(tk);
    memcpy(tk, src_tk, sizeof (*tk));
}

static struct tracker *
tracker_dup_raw(struct tracker *tk)
{
    struct tracker *tk_dup;

    tk_dup = dup_safe(tk);
    box_acquire(tk_dup->box);
    if (NULL != tk_dup->item_box) {
        box_acquire(tk_dup->item_box);
    }
    return tk_dup;
}

struct tracker *
tracker_dup(struct tracker *tk)
{
    struct tracker *tk_dup;

    tk_dup = tracker_dup_raw(tk);
    return tk_dup;
}

void
tracker_delete(struct tracker *tk)
{
    if (NULL != tk) {
        tracker_destroy(tk);
        free(tk);
    }
}

static void
tracker_dump_flags(const struct tracker *tk, FILE *out)
{
    static const char *flag_desc[] = {
        "AT_END",
        "NEED_ITEM_OFFSET",
        "REVERSED",
    };
    int flag;
    int i;
    int first;

    first = TRUE;
    for (i = 0, flag = 1; flag <= tk->flags; ++i, flag <<= 1) {
        if (0 != (tk->flags & flag)) {
            fprintf(out, "%s%s",
                    (first ? "" : ","), flag_desc[i]);
            first = FALSE;
        }
    }
}

void
tracker_dump(const struct tracker *tk)
{
    tracker_fdump(tk, stdout);
}

void
tracker_fdump(const struct tracker *tk, FILE *out)
{
    fprintf(out,
            "TRACKER @");
    tracker_dump_abs_dpath(tk, out);
    fprintf(out, ": itype='%s' ftype='%s' iloc=[%"PRIi64"..%"PRIi64"[ flags: ",
            (NULL != tk->dpath.item ?
             ast_node_type_str(tk->dpath.item->ndat->type) : "N/A"),
            (NULL != tk->dpath.filter ?
             ast_node_type_str(tk->dpath.filter->ndat->type) : "N/A"),
            tk->item_offset,
            (-1 == tk->item_size ? -1 :
             (0 != (tk->flags & TRACKER_REVERSED)) ?
             tk->item_offset - tk->item_size :
             tk->item_offset + tk->item_size));
    tracker_dump_flags(tk, out);
    fprintf(out, " BOX:\n");
    box_dump_internal(tk->box, out, 1);
    fprintf(out, "\n");
}

static bitpunch_status_t
tracker_set_item_size(struct tracker *tk, int64_t item_size,
                      struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    assert(-1 != tk->item_offset);
    assert(item_size >= 0);
    if (NULL != tk->item_box) {
        bt_ret = box_set_used_size(tk->item_box, item_size, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    tk->item_size = item_size;
    bt_ret = tracker_check_item(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        tracker_reset_item_cache(tk);
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return bt_ret;
}

static void
tracker_set_end_nocheck(struct tracker *tk)
{
    tracker_reset_item_cache_internal(tk);
    tracker_reset_dpath_internal(tk);
    tk->flags |= TRACKER_AT_END;
}

static bitpunch_status_t
tracker_set_end(struct tracker *tk, struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    tracker_set_end_nocheck(tk);
    return BITPUNCH_OK;
}

bitpunch_status_t
tracker_create_item_box_internal(struct tracker *tk,
                                 struct browse_state *bst)
{
    struct tracker *xtk;
    struct box *item_box;
    int item_box_is_right_aligned;
    enum box_flag box_flags;
    int reverse_tracker;
    bitpunch_status_t bt_ret;

    if (0 != (tk->box->flags & BOX_RALIGN)) {
        item_box_is_right_aligned =
            0 == (tk->cur.flags & TRACK_PATH_HEADER);
    } else {
        item_box_is_right_aligned =
            0 != (tk->cur.flags & TRACK_PATH_TRAILER);
    }
    reverse_tracker = 0 != (tk->flags & TRACKER_REVERSED) ?
        !item_box_is_right_aligned :
        item_box_is_right_aligned;
    if (reverse_tracker) {
        xtk = tracker_dup(tk);
        bt_ret = tracker_reverse_direction_internal(xtk, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(xtk);
            return bt_ret;
        }
    } else {
        xtk = tk;
        bt_ret = BITPUNCH_OK;
    }
    box_flags = 0 != (xtk->flags & TRACKER_REVERSED) ? BOX_RALIGN : 0u;
    if (NULL == xtk->item_box) {
        if (tracker_is_dangling(xtk)) {
            bt_ret = BITPUNCH_NO_ITEM;
            goto end;
        }
        if (-1 == xtk->item_offset) {
            bt_ret = tracker_compute_item_offset(xtk, bst);
            if (BITPUNCH_OK != bt_ret) {
                goto end;
            }
            assert(xtk->item_offset >= 0);
        }
    }
    // need to re-check because tracker_compute_item_offset() may have
    // set the item box
    if (NULL == xtk->item_box) {
        item_box = box_lookup_cached_child(xtk->box, xtk->cur, box_flags);
        if (NULL != item_box) {
#ifdef DEBUG
            if (tracker_debug_mode) {
                printf("found box in cache: ");
                DBG_BOX_DUMP(item_box);
            }
#endif
            box_acquire(item_box);
        } else {
            struct dpath_node box_dpath;

            dpath_node_reset(&box_dpath);
            bt_ret = tracker_compute_item_filter_internal(xtk, bst);
            if (BITPUNCH_OK != bt_ret) {
                goto end;
            }
            box_dpath.item = xtk->dpath.item;
            // it's an item box, so the filter is also the item here
            box_dpath.filter = xtk->dpath.item;
            bt_ret = expr_evaluate_filter_type_internal(
                xtk->dpath.filter, xtk->box, FILTER_KIND_DEFINING_SPAN_SIZE,
                &box_dpath.filter_defining_span_size, bst);
            if (BITPUNCH_OK != bt_ret) {
                goto end;
            }
            item_box = new_safe(struct box);
            bt_ret = box_construct(item_box, xtk->box, &box_dpath,
                                   xtk->item_offset, box_flags, bst);
            if (BITPUNCH_OK != bt_ret) {
                box_delete_non_null(item_box);
                DBG_TRACKER_CHECK_STATE(xtk);
                goto end;
            }
        }
        item_box->track_path = xtk->cur;
        if (-1 != item_box->start_offset_used
            && -1 != item_box->end_offset_used) {
            xtk->item_size =
                item_box->end_offset_used - item_box->start_offset_used;
        } else if (-1 != xtk->item_size) {
            bt_ret = box_set_used_size(item_box, xtk->item_size, bst);
            if (BITPUNCH_OK != bt_ret) {
                box_delete_non_null(item_box);
                DBG_TRACKER_CHECK_STATE(xtk);
                goto end;
            }
        }
        xtk->item_box = item_box;
    } else {
        item_box = xtk->item_box;
    }
    box_update_cache(item_box, bst);
    if (-1 != xtk->item_size) {
        box_set_used_size(xtk->item_box, xtk->item_size, bst);
    }
    if (BITPUNCH_OK == bt_ret && reverse_tracker) {
        bt_ret = tracker_reverse_direction_internal(xtk, bst);
        if (BITPUNCH_OK == bt_ret) {
            tracker_set(tk, xtk);
        }
    }
  end:
    DBG_TRACKER_CHECK_STATE(tk);
    if (reverse_tracker) {
        tracker_delete(xtk);
    }
    return bt_ret;
}

bitpunch_status_t
tracker_get_filtered_dpath_internal(struct tracker *tk,
                                    expr_dpath_t *filtered_dpathp,
                                    struct browse_state *bst)
{
    struct dpath_transform transform;
    bitpunch_status_t bt_ret;

    if (tracker_is_dangling(tk)) {
        return BITPUNCH_NO_ITEM;
    }
    // initialize work dpath with unfiltered tracked item
    transform.dpath.type = EXPR_DPATH_TYPE_ITEM;
    transform.dpath.item.tk = tracker_dup(tk);
    transform.dpath_is_data_source = TRUE;
    bt_ret = expr_transform_dpath_internal(
        tk->dpath.filter, tk->box, &transform, bst);
    if (BITPUNCH_OK == bt_ret) {
        *filtered_dpathp = transform.dpath;
    } else {
        expr_dpath_destroy(transform.dpath);
    }
    return bt_ret;
}

bitpunch_status_t
tracker_get_filtered_item_box_internal(struct tracker *tk,
                                       struct box **filtered_boxp,
                                       struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    expr_dpath_t filtered_dpath;
    struct box *filtered_box;

    if (tracker_is_dangling(tk)) {
        return BITPUNCH_NO_ITEM;
    }
    bt_ret = tracker_get_filtered_dpath_internal(tk, &filtered_dpath, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_dpath_to_box_direct(filtered_dpath, &filtered_box, bst);
    expr_dpath_destroy(filtered_dpath);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    *filtered_boxp = filtered_box;
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_min_span_size(struct box *box,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    if (-1 != box->start_offset_min_span && -1 != box->end_offset_min_span) {
        /* nothing to do */
        return BITPUNCH_OK;
    }
    bt_ret = box_apply_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_box.compute_min_span_size(
        box, bst);
}

bitpunch_status_t
box_get_min_span_size(struct box *box, int64_t *min_span_sizep,
                      struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = box_compute_min_span_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (NULL != min_span_sizep) {
        *min_span_sizep =
            box->end_offset_min_span - box->start_offset_min_span;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_used_size(struct box *box,
                      struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int call_backend;

    if (-1 != box->start_offset_used && -1 != box->end_offset_used) {
        /* nothing to do */
        return BITPUNCH_OK;
    }
    bt_ret = box_apply_filter_internal(box, bst);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = box_compute_min_span_size(box, bst);
    }
    if (BITPUNCH_OK == bt_ret
        && 0 == (box->flags & COMPUTING_SPAN_SIZE)) {
        bt_ret = box_compute_max_span_size(box, bst);
    }
    if (BITPUNCH_OK == bt_ret) {
        call_backend = 0 != (box->flags & COMPUTING_SPAN_SIZE);
        if (0 == (box->flags & COMPUTING_SPAN_SIZE)) {
            if ((0 == (box->flags & BOX_RALIGN)
                 && box->end_offset_min_span
                 == box->end_offset_max_span)
                || (0 != (box->flags & BOX_RALIGN)
                    && box->start_offset_min_span
                    == box->start_offset_max_span)) {
                bt_ret = box_set_used_size(box, box->end_offset_max_span
                                           - box->start_offset_max_span,
                                           bst);
            } else {
                call_backend = TRUE;
            }
        }
        if (call_backend) {
            bt_ret =
                box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_box.compute_used_size(
                    box, bst);
        }
    }
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(
            box, bst, "when computing used size");
    }
    return bt_ret;
}

bitpunch_status_t
box_get_used_size(struct box *box, int64_t *used_sizep,
                  struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = box_compute_used_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (NULL != used_sizep) {
        *used_sizep = box->end_offset_used - box->start_offset_used;
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
box_get_slack_size(struct box *box, int64_t *slack_sizep,
                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = box_compute_slack_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (NULL != slack_sizep) {
        *slack_sizep = box->end_offset_slack - box->start_offset_slack;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_max_span_size(struct box *box,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    if ((-1 != box->start_offset_max_span && -1 != box->end_offset_max_span)
        || 0 != (box->flags & COMPUTING_SPAN_SIZE)) {
        /* nothing to do */
        return BITPUNCH_OK;
    }
    box->flags |= COMPUTING_SPAN_SIZE;
    bt_ret = box_apply_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_box.compute_max_span_size(box, bst);
    box->flags &= ~COMPUTING_SPAN_SIZE;
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(
            box, bst, "when computing slack size");
    }
    return bt_ret;
}

/**
 * @brief compute available space for @ref box
 */
static bitpunch_status_t
box_compute_slack_size(struct box *box,
                       struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    if (-1 != box->start_offset_slack && -1 != box->end_offset_slack) {
        /* nothing to do */
        return BITPUNCH_OK;
    }
    bt_ret = box_apply_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_box.compute_slack_size(
        box, bst);
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(
            box, bst, "when computing slack size");
    }
    return bt_ret;
}

static bitpunch_status_t
box_get_children_slack(struct box *box,
                       int get_left_offset,
                       int64_t *max_slack_offsetp,
                       struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int flag_set = FALSE;

    // check for circular dependency
    if (!(box->flags & COMPUTING_MAX_SLACK_OFFSET)) {
        box->flags |= COMPUTING_MAX_SLACK_OFFSET;
        flag_set = TRUE;
    }
    bt_ret = box_apply_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (NULL != box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_box.get_max_slack_offset) {
        bt_ret = box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_box.get_max_slack_offset(
            box, get_left_offset, max_slack_offsetp, bst);
    } else {
        bt_ret = box_compute_max_span_size(box, bst);
        if (BITPUNCH_OK == bt_ret) {
            if (0 != (box->flags & BOX_RALIGN)) {
                *max_slack_offsetp = box_get_known_start_offset(box);
            } else {
                *max_slack_offsetp = box_get_known_end_offset(box);
            }
        }
    }
    if (flag_set) {
        box->flags &= ~COMPUTING_MAX_SLACK_OFFSET;
    }
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(
            box, bst, "when computing max slack offset");
    }
    return bt_ret;
}

bitpunch_status_t
box_get_n_items_internal(struct box *box, int64_t *n_itemsp,
                         struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = box_apply_filter_internal(box, bst);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_box.get_n_items(
            box, n_itemsp, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(
            box, bst, "when computing number of array items");
    }
    return bt_ret;
}

bitpunch_status_t
box_get_location_internal(struct box *box,
                          int64_t *offsetp, int64_t *sizep,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = box_apply_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = box_get_used_size(box, sizep, bst);
    if (BITPUNCH_OK == bt_ret) {
        *offsetp = box_get_start_offset(box);
    }
    return bt_ret;
}

bitpunch_status_t
box_read_value_internal(struct box *box,
                        expr_value_t *valuep,
                        struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct item_backend *b_item = NULL;

    bt_ret = box_apply_filter_internal(box, bst);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = box_compute_used_size(box, bst);
    }
    if (BITPUNCH_OK == bt_ret) {
        b_item = &box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_item;
        bt_ret = b_item->read_value(
            box->dpath.filter, box,
            box->start_offset_used,
            box->end_offset_used - box->start_offset_used,
            valuep, bst);
    }
    if (BITPUNCH_OK == bt_ret && NULL != valuep) {
        expr_value_attach_box(valuep, box);
    }
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(box, bst, "when reading box value");
    }
    return bt_ret;
}

static bitpunch_status_t
box_compute_end_offset_internal(struct box *box,
                                enum box_offset_type off_type,
                                int64_t *end_offsetp,
                                struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t end_offset;

    bt_ret = box_apply_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    switch (off_type) {
    case BOX_END_OFFSET_PARENT:
        bt_ret = BITPUNCH_OK;
        end_offset = box->end_offset_parent;
        break ;
    case BOX_END_OFFSET_SLACK:
        bt_ret = box_compute_slack_size(box, bst);
        end_offset = box->end_offset_slack;
        break ;
    case BOX_END_OFFSET_MAX_SPAN:
        bt_ret = box_compute_max_span_size(box, bst);
        end_offset = box->end_offset_max_span;
        break ;
    case BOX_END_OFFSET_USED:
        bt_ret = box_compute_used_size(box, bst);
        end_offset = box->end_offset_used;
        break ;
    case BOX_END_OFFSET_MIN_SPAN:
        bt_ret = box_compute_min_span_size(box, bst);
        end_offset = box->end_offset_min_span;
        break ;
    default:
        return BITPUNCH_INVALID_PARAM;
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (NULL != end_offsetp) {
        *end_offsetp = end_offset;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_get_end_path(struct box *box, struct track_path *end_pathp,
                 struct browse_state *bst)
{
    const struct ast_node_hdl *node;
    bitpunch_status_t bt_ret;
    int64_t n_items;
    int64_t index_start;

    DBG_BOX_DUMP(box);
    node = box->dpath.filter;
    switch (node->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_REXPR_FILTER:
        bt_ret = box_get_n_items_internal(box, &n_items, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        *end_pathp = track_path_from_array_index(n_items);
        break ;
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
        bt_ret = box_get_n_items_internal(box, &n_items, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        index_start = box->track_path.u.array.index;
        if (-1 == index_start) {
            index_start = 0;
        }
        *end_pathp = track_path_from_array_index(index_start + n_items);
        break ;
    case AST_NODE_TYPE_COMPOSITE:
        *end_pathp = track_path_from_composite_field(NULL);
        break ;
    default:
        *end_pathp = TRACK_PATH_NONE;
        break ;
    }
    return BITPUNCH_OK;
}

struct tracker *
track_box_contents_internal(struct box *box, struct browse_state *bst)
{
    return tracker_new(box);
}

struct tracker *
track_file(const struct bitpunch_schema_hdl *def_hdl,
           struct bitpunch_binary_file_hdl *file_hdl,
           struct tracker_error **errp)
{
    struct box *box;
    struct tracker *tk;

    /* TODO issue warning if root node min span size > file length */
    box = box_new_from_file(def_hdl, file_hdl);
    if (NULL == box) {
        return NULL;
    }
    tk = track_box_contents(box, errp);
    box_delete_non_null(box);
    return tk;
}

bitpunch_status_t
track_item_contents_internal(struct tracker *tk,
                             struct tracker **tkp,
                             struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *filtered_box;
    struct tracker *item_tk;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_get_filtered_item_box_internal(tk, &filtered_box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    item_tk = track_box_contents_internal(filtered_box, bst);
    box_delete_non_null(filtered_box);
    *tkp = item_tk;
    return BITPUNCH_OK;
}

bitpunch_status_t
tracker_compute_item_filter_internal(struct tracker *tk,
                                     struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    if (tracker_is_dangling(tk)) {
        return BITPUNCH_NO_ITEM;
    }
    if (NULL != tk->dpath.item) {
        return BITPUNCH_OK;
    }
    return expr_evaluate_filter_type_internal(
        tk->dpath.filter, tk->box, FILTER_KIND_ITEM, &tk->dpath.item, bst);
}

static bitpunch_status_t
tracker_compute_item_offset(struct tracker *tk,
                            struct browse_state *bst)
{
    const struct ast_node_hdl *node;

    DBG_TRACKER_DUMP(tk);
    if (tracker_is_dangling(tk)) {
        if (0 != (tk->flags & TRACKER_AT_END)) {
            tk->flags |= TRACKER_NEED_ITEM_OFFSET;
            return tracker_goto_end_offset(tk, bst);
        } else {
            return BITPUNCH_NO_ITEM;
        }
    }
    if (-1 != tk->item_offset) {
        return BITPUNCH_OK;
    }
    /* carry out deferred evaluation of item offset */
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    node = tk->box->dpath.filter;
    switch (node->ndat->type) {
    case AST_NODE_TYPE_COMPOSITE:
        return tracker_goto_field_internal(tk, tk->cur.u.block.field,
                                           TRUE, bst);
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_REXPR_FILTER:
        return tracker_goto_nth_item_internal(tk, tk->cur.u.array.index,
                                              bst);
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE: {
        int64_t index_start;

        index_start = tk->box->track_path.u.array.index;
        if (-1 == index_start) {
            index_start = 0;
        }
        return tracker_goto_nth_item_internal(
            tk, tk->cur.u.array.index - index_start, bst);
    }
    default:
        assert(0);
    }
    /*NOT REACHED*/
}

bitpunch_status_t
tracker_get_item_filter_internal(struct tracker *tk,
                                 struct ast_node_hdl **item_filterp,
                                 struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_compute_item_filter_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    *item_filterp = tk->dpath.item;
    return BITPUNCH_OK;
}

bitpunch_status_t
tracker_get_item_offset_internal(struct tracker *tk, int64_t *item_offsetp,
                                 struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    if (-1 == tk->item_offset) {
        bt_ret = tracker_compute_item_offset(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        assert(tk->item_offset >= 0);
    } else if (tracker_is_dangling(tk)) {
        return BITPUNCH_NO_ITEM;
    }
    if (NULL != item_offsetp) {
        *item_offsetp = tk->item_offset;
    }
    DBG_TRACKER_DUMP(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_get_n_items_internal(struct tracker *tk, int64_t *item_countp,
                             struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return box_get_n_items_internal(tk->box, item_countp, bst);
}

bitpunch_status_t
tracker_check_item(struct tracker *tk,
                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int reversed_iter;
    int64_t max_offset;
    int64_t item_size;

    DBG_TRACKER_DUMP(tk);
    reversed_iter = (0 != (tk->flags & TRACKER_REVERSED));
    if (!tracker_is_dangling(tk)) {
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)
            && -1 == tk->item_offset) {
            bt_ret = tracker_compute_item_offset(tk, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
            assert(tk->item_offset >= 0);
        } else {
            bt_ret = tracker_compute_item_filter_internal(tk, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
        }
    }
    if (reversed_iter) {
        if ((tk->box->flags & COMPUTING_MAX_SLACK_OFFSET)) {
            max_offset = box_get_known_start_offset_mask(
                tk->box, (BOX_START_OFFSET_MAX_SPAN |
                          BOX_START_OFFSET_SLACK |
                          BOX_START_OFFSET_PARENT));
        } else {
            max_offset = box_get_known_start_offset(tk->box);
        }
    } else {
        if ((tk->box->flags & COMPUTING_MAX_SLACK_OFFSET)) {
            max_offset = box_get_known_end_offset_mask(
                tk->box, (BOX_END_OFFSET_MAX_SPAN |
                          BOX_END_OFFSET_SLACK |
                          BOX_END_OFFSET_PARENT));
        } else {
            max_offset = box_get_known_end_offset(tk->box);
        }
    }
    assert(-1 != max_offset);

    if (-1 != tk->item_size) {
        item_size = tk->item_size;
    } else if (!tracker_is_dangling(tk)) {
        item_size = ast_node_get_min_span_size(tk->dpath.item);
    } else {
        item_size = 0;
    }
    if (-1 != tk->item_offset) {
        if (reversed_iter) {
            if (tk->item_offset - item_size < max_offset) {
                return tracker_error_item_out_of_bounds(tk, bst);
            }
        } else {
            if (tk->item_offset + item_size > max_offset) {
                return tracker_error_item_out_of_bounds(tk, bst);
            }
        }
    }
    return BITPUNCH_OK;
}

static void
tracker_rewind_internal(struct tracker *tk)
{
    tracker_set_dangling_internal(tk);
    tk->flags &= ~TRACKER_NEED_ITEM_OFFSET;
    tk->item_offset = -1;
}

void
tracker_rewind(struct tracker *tk)
{
    DBG_TRACKER_DUMP(tk);
    tracker_rewind_internal(tk);
}

static void
tracker_set_dpath_from_cur_internal(struct tracker *tk)
{
    struct filter_instance_array *array;

    switch (tk->cur.type) {
    case TRACK_PATH_COMPOSITE:
        if (NULL != tk->cur.u.block.field) {
            tk->dpath.filter = tk->cur.u.block.field->dpath.filter;
            tk->dpath.item = NULL;
        } else {
            tracker_reset_dpath_internal(tk);
        }
        break ;
    case TRACK_PATH_ARRAY:
        assert(AST_NODE_TYPE_ARRAY == tk->box->dpath.filter->ndat->type);
        array = (struct filter_instance_array *)
            tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
        tk->dpath.filter = array->item_type.filter;
        tk->dpath.item = NULL;
        break ;
    default:
        tracker_reset_dpath_internal(tk);
        break ;
    }
}

static bitpunch_status_t
tracker_goto_first_item_internal(struct tracker *tk,
                                 struct browse_state *bst)
{
    struct filter_instance *f_instance;
    bitpunch_status_t bt_ret;
    bitpunch_status_t bt_ret2;

    DBG_TRACKER_DUMP(tk);
    tracker_set_dangling(tk);
    tk->item_offset = -1;
    f_instance = tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_first_item) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->dpath.filter, bst,
            "filter does not implement goto_first_item() tracker backend function");
    }
    bt_ret = f_instance->b_tk.goto_first_item(tk, bst);
    switch (bt_ret) {
    case BITPUNCH_NO_ITEM:
        tk->flags |= TRACKER_AT_END;
        /*FALLTHROUGH*/
    case BITPUNCH_OK:
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
            tk->item_offset = 0 != (tk->flags & TRACKER_REVERSED) ?
                tk->box->end_offset_used : tk->box->start_offset_used;
            bt_ret2 = tracker_check_item(tk, bst);
            if (BITPUNCH_OK != bt_ret2) {
                return bt_ret2;
            }
        }
        break ;
    default:
        break ;
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_next_item_internal(struct tracker *tk,
                                struct browse_state *bst)
{
    struct filter_instance *f_instance;

    DBG_TRACKER_DUMP(tk);
    if (0 != (tk->flags & TRACKER_AT_END)) {
        return BITPUNCH_NO_ITEM;
    }
    if (tracker_is_dangling(tk)) {
        return tracker_goto_first_item_internal(tk, bst);
    }
    f_instance = tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_next_item) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->dpath.filter, bst,
            "filter does not implement goto_next_item() tracker backend function");
    }
    return f_instance->b_tk.goto_next_item(tk, bst);
}


static bitpunch_status_t
tracker_goto_nth_item_internal(struct tracker *tk, int64_t index,
                               struct browse_state *bst)
{
    struct filter_instance *f_instance;

    DBG_TRACKER_DUMP(tk);
    if (index < 0) {
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, NULL, bst,
                             "array index cannot be negative (got %ld)",
                             index);
    }
    f_instance = tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_nth_item) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->dpath.filter, bst,
            "filter does not implement goto_nth_item() tracker backend function");
    }
    return f_instance->b_tk.goto_nth_item(tk, index, bst);
}

static bitpunch_status_t
tracker_goto_nth_position_internal(struct tracker *tk, int64_t index,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct filter_instance *f_instance;

    DBG_TRACKER_DUMP(tk);
    if (index < 0) {
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, NULL, bst,
                             "array index cannot be negative (got %ld)",
                             index);
    }
    f_instance = tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_nth_item) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->dpath.filter, bst,
            "filter does not implement goto_nth_item() tracker backend function");
    }
    bt_ret = f_instance->b_tk.goto_nth_item(tk, index, bst);
    if (BITPUNCH_NO_ITEM == bt_ret) {
        int64_t n_items;

        bt_ret = box_get_n_items_internal(tk->box, &n_items, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        if (index == n_items) {
            bt_ret = tracker_goto_end_internal(tk, bst);
        } else {
            bt_ret = BITPUNCH_NO_ITEM;
        }
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_named_item_internal(struct tracker *tk, const char *name,
                                 struct browse_state *bst)
{
    struct filter_instance *f_instance;

    DBG_TRACKER_DUMP(tk);
    f_instance = tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_named_item) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->dpath.filter, bst,
            "filter does not implement goto_named_item() tracker backend function");
    }
    return f_instance->b_tk.goto_named_item(tk, name, bst);
}


static bitpunch_status_t
tracker_goto_first_item_with_key_internal(struct tracker *tk,
                                          expr_value_t item_key,
                                          struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_goto_nth_item_with_key_internal(tk, item_key, 0, bst);
}

static bitpunch_status_t
tracker_goto_next_item_with_key_internal(struct tracker *tk,
                                         expr_value_t item_key,
                                         struct browse_state *bst)
{
    struct filter_instance *f_instance;

    DBG_TRACKER_DUMP(tk);
    if (tracker_is_dangling(tk)) {
        return tracker_goto_nth_item_with_key_internal(tk, item_key, 0, bst);
    }
    f_instance = tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_next_item_with_key) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->dpath.filter, bst,
            "filter does not implement goto_next_item_with_key() tracker backend function");
    }
    return f_instance->b_tk.goto_next_item_with_key(
        tk, item_key, bst);
}

static bitpunch_status_t
tracker_goto_nth_item_with_key_internal(struct tracker *tk,
                                        expr_value_t item_key,
                                        int nth_twin,
                                        struct browse_state *bst)
{
    struct filter_instance *f_instance;

    DBG_TRACKER_DUMP(tk);
    if (nth_twin < 0) {
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, NULL, bst,
                             "parameter \"nth_twin\" must be >= 0 (is %d)",
                             nth_twin);
    }
    f_instance = tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_nth_item_with_key) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->dpath.filter, bst,
            "filter does not implement goto_nth_item_with_key() tracker backend function");
    }
    return f_instance->b_tk.goto_nth_item_with_key(
        tk, item_key, nth_twin, bst);
}


static void
tracker_set_field_internal__composite(struct tracker *tk,
                                      const struct field *field,
                                      struct browse_state *bst)
{
    DPRINT("TK set field "ANSI_COLOR_GREEN"%s"ANSI_COLOR_RESET" on:\n",
           field->nstmt.name);
    DBG_TRACKER_DUMP(tk);
    tracker_set_dangling_internal(tk);
    tk->cur = track_path_from_composite_field(field);
    tk->dpath.filter = field->dpath.filter;
    DBG_TRACKER_CHECK_STATE(tk);
}

bitpunch_status_t
tracker_goto_field_internal(struct tracker *tk,
                            const struct field *to_field, int flat,
                            struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int tracker_set_reversed;
    int reverse_direction;

    DBG_TRACKER_DUMP(tk);
    assert(flat || NULL != to_field->nstmt.name);

    if (flat && 0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        tracker_set_field_internal__composite(tk, to_field, bst);
        return BITPUNCH_OK;
    }
    if (0 != (tk->box->flags & BOX_RALIGN)) {
        tracker_set_reversed =
            0 == (to_field->nstmt.stmt.stmt_flags & FIELD_FLAG_HEADER);
    } else {
        tracker_set_reversed =
            0 != (to_field->nstmt.stmt.stmt_flags & FIELD_FLAG_TRAILER);
    }
    // union may be nesting anonymous structures, so setting offset to
    // box offset as an easy optimization for union types is incorrect
    // (keep a more complex optim for later)
    reverse_direction =
        (tracker_set_reversed && 0 == (tk->flags & TRACKER_REVERSED))
        || (!tracker_set_reversed && 0 != (tk->flags & TRACKER_REVERSED));
    if (reverse_direction) {
        tk->flags ^= TRACKER_REVERSED;
    }
    bt_ret = tracker_goto_first_item_int__composite(tk, flat, bst);
    while (BITPUNCH_OK == bt_ret && tk->cur.u.block.field != to_field) {
        bt_ret = tracker_goto_next_item_int__composite(tk, flat, bst);
    }
    if (reverse_direction && BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_reverse_direction_internal(tk, bst);
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_abs_dpath_internal(struct tracker *tk, const char *dpath_expr,
                                struct browse_state *bst)
{
    struct ast_node_hdl *expr_node;
    struct parser_ctx *parser_ctx = NULL;
    struct box *root_box;
    bitpunch_status_t bt_ret;
    expr_dpath_t eval_dpath;
    struct tracker *tk_tmp;

    DBG_TRACKER_DUMP(tk);
    /*TODO clarify lifetime of parser_ctx */
    if (-1 == bitpunch_parse_expr(dpath_expr, &expr_node, &parser_ctx)) {
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, NULL, bst, NULL);
    }
    root_box = tk->box;
    while (NULL != root_box->parent_box) {
        root_box = root_box->parent_box;
    }
    if (-1 == bitpunch_resolve_expr(expr_node, root_box)) {
        free(parser_ctx);
        /* TODO free expr_node */
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, NULL, bst, NULL);
    }
    if (expr_node->ndat->u.rexpr.dpath_type_mask == EXPR_DPATH_TYPE_NONE) {
        free(parser_ctx);
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, NULL, bst, NULL);
    }
    bt_ret = expr_evaluate_dpath_internal(expr_node, root_box,
                                          &eval_dpath, bst);
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_tracker_context(
            tk, bst, "when evaluating dpath expression");
        return bt_ret;
    }
    switch (eval_dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        tracker_set(tk, eval_dpath.item.tk);
        tracker_delete(eval_dpath.item.tk);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        tk_tmp = track_box_contents_internal(eval_dpath.container.box, bst);
        box_delete(eval_dpath.container.box);
        tracker_set(tk, tk_tmp);
        tracker_delete(tk_tmp);
        break ;
    default:
        assert(0);
    }
    // TODO free expr_node
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_end_path(struct tracker *tk,
                      struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return box_get_end_path(tk->box, &tk->cur, bst);
}

static bitpunch_status_t
tracker_goto_end_offset(struct tracker *tk,
                        struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        bitpunch_status_t bt_ret;

        bt_ret = box_compute_used_size(tk->box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        tk->item_offset = 0 != (tk->flags & TRACKER_REVERSED) ?
            tk->box->start_offset_used : tk->box->end_offset_used;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_end_internal(struct tracker *tk,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct track_path old_path;

    DBG_TRACKER_DUMP(tk);
    old_path = tk->cur;
    bt_ret = tracker_goto_end_path(tk, bst);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_goto_end_offset(tk, bst);
    }
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_set_end(tk, bst);
    } else {
        tk->cur = old_path;
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return bt_ret;
}

bitpunch_status_t
tracker_goto_index_internal(struct tracker *tk,
                            struct subscript_index index,
                            const char *index_desc,
                            struct box *scope,
                            int allow_end_boundary,
                            int is_end_of_slice,
                            struct browse_state *bst)
{
    expr_value_t item_index;
    bitpunch_status_t bt_ret;

    if (NULL != index.key) {
        bt_ret = expr_evaluate_value_internal(index.key, scope,
                                              &item_index, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_error_add_tracker_context(
                tk, bst, "when evaluating item index expression");
            return bt_ret;
        }
        // FIXME this may need rework regarding multiple value-types
        // in mask
        if (EXPR_VALUE_TYPE_INTEGER
            == index.key->ndat->u.rexpr.value_type_mask) {
            if (item_index.integer < 0) {
                int64_t n_items;

                bt_ret = tracker_get_n_items_internal(tk, &n_items, bst);
                if (BITPUNCH_OK != bt_ret) {
                    return bt_ret;
                }
                if (item_index.integer + n_items < 0) {
                    semantic_error(
                        SEMANTIC_LOGLEVEL_ERROR, &index.key->loc,
                        "index %"PRIi64" points outside %s of size %"PRIu64,
                        item_index.integer,
                        (ast_node_is_slice_container(tk->box->dpath.filter) ?
                         "slice" : "array"), n_items);
                    return BITPUNCH_OUT_OF_BOUNDS_ERROR;
                }
                item_index.integer += n_items;
            }
            if (allow_end_boundary) {
                bt_ret = tracker_goto_nth_position_internal(
                    tk, item_index.integer, bst);
            } else {
                bt_ret = tracker_goto_nth_item_internal(
                    tk, item_index.integer, bst);
            }
            if (BITPUNCH_NO_ITEM == bt_ret) {
                (void)box_get_n_items_internal(tk->box, NULL, bst);
                semantic_error(
                    SEMANTIC_LOGLEVEL_ERROR, &index.key->loc,
                    "%s %"PRIi64" is past array size (%"PRIi64")",
                    index_desc, item_index.integer,
                    tk->box->u.array_generic.n_items);
            }
        } else {
            expr_value_t twin_index;

            if (NULL != index.twin) {
                bt_ret = expr_evaluate_value_internal(index.twin, tk->box,
                                                      &twin_index, bst);
                if (BITPUNCH_OK != bt_ret) {
                    tracker_error_add_tracker_context(
                        tk, bst, "when evaluating twin index expression");
                    return bt_ret;
                }
            } else {
                twin_index.integer = 0;
            }
            bt_ret = tracker_goto_nth_item_with_key_internal(
                tk, item_index, twin_index.integer, bst);
            if (BITPUNCH_NO_ITEM == bt_ret
                && (EXPR_VALUE_TYPE_STRING
                    == index.key->ndat->u.rexpr.value_type_mask)) {
                semantic_error(
                    SEMANTIC_LOGLEVEL_ERROR, &index.key->loc,
                    "key '%.*s'{%"PRIi64"} does not exist",
                    (int)item_index.string.len, item_index.string.str,
                    twin_index.integer);
            }
            expr_value_destroy(item_index);
        }
    } else {
        assert(allow_end_boundary);
        if (is_end_of_slice) {
            bt_ret = tracker_goto_end_internal(tk, bst);
        } else {
            bt_ret = tracker_goto_nth_position_internal(tk, 0, bst);
        }
    }
    return bt_ret;
}

bitpunch_status_t
tracker_enter_item_internal(struct tracker *tk,
                            struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *filtered_box;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_get_filtered_item_box_internal(tk, &filtered_box, bst);
    if (BITPUNCH_OK == bt_ret) {
        box_delete(tk->item_box);
        box_delete_non_null(tk->box);
        tk->box = filtered_box;
        tk->item_box = NULL;
        tracker_rewind_internal(tk);
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return bt_ret;
}

static bitpunch_status_t
tracker_enter_slice_internal(struct tracker *tk, struct tracker *slice_end,
                             struct browse_state *bst)
{
    struct box *slice_box;

    DBG_TRACKER_DUMP(tk);
    slice_box = box_new_slice_box(tk, slice_end, bst);
    if (NULL == slice_box) {
        return browse_state_get_last_error_status(bst);
    }
    box_delete_non_null(tk->box);
    tk->box = slice_box;
    tracker_set_dangling(tk);
    tk->item_offset = -1;
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_set_item_offset_at_box(struct tracker *tk,
                               struct box *box,
                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t start_offset;
    int64_t end_offset;

    if (0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        return BITPUNCH_OK;
    }
    if (0 != (tk->flags & TRACKER_REVERSED)) {
        end_offset = box_get_known_end_offset_mask(
            box, (BOX_END_OFFSET_USED |
                  BOX_END_OFFSET_MAX_SPAN));
        if (-1 == end_offset) {
            bt_ret = box_compute_max_span_size(box, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
            end_offset = box->end_offset_max_span;
        }
        tk->item_offset = end_offset;
    } else {
        start_offset = box_get_known_start_offset_mask(
            box, (BOX_START_OFFSET_USED |
                  BOX_START_OFFSET_MAX_SPAN));
        if (-1 == start_offset) {
            bt_ret = box_compute_max_span_size(box, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
            start_offset = box->start_offset_max_span;
        }
        tk->item_offset = start_offset;
    }
    return BITPUNCH_OK;
}

static void
tracker_return_from_slice(struct tracker *tk,
                          struct browse_state *bst)
{
    struct box *slice_box;

    slice_box = tk->box;
    tk->box = tk->box->parent_box;
    if (!tracker_is_dangling(tk)) {
        if (-1 != slice_box->track_path.u.array.index) {
            tk->cur.u.array.index +=
                slice_box->track_path.u.array.index;
        }
    } else {
        tracker_set_dangling(tk);
    }
    box_acquire(tk->box);
    box_delete_non_null(slice_box);
}

bitpunch_status_t
tracker_return_internal(struct tracker *tk,
                        struct browse_state *bst)
{
    struct box *orig_box;

    DBG_TRACKER_DUMP(tk);
    if (NULL == tk->box->parent_box) {
        return BITPUNCH_NO_ITEM;
    }
    if (TRACK_PATH_ARRAY_SLICE == tk->box->track_path.type) {
        tracker_return_from_slice(tk, bst);
        DBG_TRACKER_CHECK_STATE(tk);
        return BITPUNCH_OK;
    }
    tracker_set_dangling(tk);
    orig_box = tk->box;
    tk->item_box = tk->box;
    while (0 != (tk->item_box->flags & BOX_FILTER)) {
        tk->item_box = tk->item_box->parent_box;
    }
    if (tk->item_box != orig_box) {
        box_acquire(tk->item_box);
        box_delete(orig_box);
    }
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    tracker_set_item_offset_at_box(tk, tk->item_box, bst);
    if (-1 != tk->item_box->end_offset_used) {
        tk->item_size =
            tk->item_box->end_offset_used - tk->item_box->start_offset_used;
    } else {
        tk->item_size = -1;
    }
    tk->box = tk->item_box->parent_box;
    box_acquire(tk->box);
    tk->cur = tk->item_box->track_path;
    tracker_set_dpath_from_cur_internal(tk);
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}


static int
track_path_elem_dump_to_buf(struct track_path tp, int dump_separator,
                            char *dpath_expr_buf, int buf_size)
{
    if (0 != (tp.flags & TRACK_PATH_IS_ANCESTOR)) {
        return snprintf(dpath_expr_buf, buf_size, "^");
    }
    switch (tp.type) {
    case TRACK_PATH_NOTYPE:
        //return snprintf(dpath_expr_buf, buf_size, "(as)");
        return 0;
    case TRACK_PATH_COMPOSITE:
        if (NULL == tp.u.block.field) {
            return snprintf(dpath_expr_buf, buf_size, ".<NOFIELD>");
        }
        if (dump_separator) {
            return snprintf(dpath_expr_buf, buf_size,
                            ".%s", tp.u.block.field->nstmt.name);
        } else {
            return snprintf(dpath_expr_buf, buf_size,
                            "%s", tp.u.block.field->nstmt.name);
        }
        break ;
    case TRACK_PATH_ARRAY:
        return snprintf(dpath_expr_buf, buf_size,
                        "[%"PRIi64"]", tp.u.array.index);
    case TRACK_PATH_ARRAY_SLICE:
        return snprintf(dpath_expr_buf, buf_size, "[%"PRIi64":%"PRIi64"]",
                        tp.u.array.index, tp.u.array_slice.index_end);
    default:
        assert(0);
    }
    /*NOT REACHED*/
}

static int
track_path_elem_dump(struct track_path tp, int dump_separator,
                     FILE *stream)
{
    const char *name;

    if (0 != (tp.flags & TRACK_PATH_IS_ANCESTOR)) {
        return fprintf(stream, "^");
    }
    switch (tp.type) {
    case TRACK_PATH_NOTYPE:
        //return fprintf(stream, "(as)");
        return 0;
    case TRACK_PATH_COMPOSITE:
        if (NULL == tp.u.block.field) {
            return fprintf(stream, ".<NOFIELD>");
        }
        name = tp.u.block.field->nstmt.name;
        if (NULL != name) {
            return fprintf(stream, "%s%s",
                           (dump_separator ? "." : ""), name);
        } else {
            return fprintf(stream, "%s<ANON:%s>",
                           (dump_separator ? "." : ""),
                           ast_node_type_str(
                               tp.u.block.field->dpath.item->ndat->type));
        }
        break ;
    case TRACK_PATH_ARRAY:
        return fprintf(stream, "[%"PRIi64"]", tp.u.array.index);
    case TRACK_PATH_ARRAY_SLICE:
        return fprintf(stream, "[%"PRIi64"..%"PRIi64"]",
                       tp.u.array.index, tp.u.array_slice.index_end);
    default:
        assert(0);
    }
    /*NOT REACHED*/
}

int
box_get_abs_dpath(const struct box *box,
                  char *dpath_expr_buf, int buf_size)
{
    int n_out;

    if (NULL == box->parent_box) {
        if (buf_size > 0) {
            dpath_expr_buf[0] = '\0';
        }
        return 0;
    }
    n_out = box_get_abs_dpath(box->parent_box, dpath_expr_buf, buf_size);
    if (n_out < buf_size) {
        dpath_expr_buf += n_out;
        buf_size -= n_out;
    } else {
        dpath_expr_buf = NULL;
        buf_size = 0;
    }
    n_out += track_path_elem_dump_to_buf(
        box->track_path,
        /* dump separator? */
        NULL != box->parent_box->parent_box,
        dpath_expr_buf, buf_size);
    return n_out;
}

char *
box_get_abs_dpath_alloc(const struct box *box)
{
    int path_len;
    char *path;

    path_len = box_get_abs_dpath(box, NULL, 0);
    path = malloc(path_len + 1);
    if (NULL == path) {
        return NULL;
    }
    box_get_abs_dpath(box, path, path_len + 1);
    return path;

}

int
box_dump_abs_dpath(const struct box *box, FILE *stream)
{
    int n_out;

    if (NULL == box->parent_box) {
        return 0;
    }
    n_out = box_dump_abs_dpath(box->parent_box, stream);
    n_out += track_path_elem_dump(box->track_path,
                                  /* dump separator? */
                                  NULL != box->parent_box->parent_box,
                                  stream);
    return n_out;
}

/**
 * @brief retrieve the current absolute data path of @ref tk, as a
 * string expression
 *
 * @return -1 on error, or number of characters printed in @ref
 * dpath_expr_buf, or number of characters that would have been
 * printed if @ref dpath_expr_buf had been big enough (see snprintf()
 * convention). The output path string is always null-terminated.
 */
int
tracker_get_abs_dpath(const struct tracker *tk,
                      char *dpath_expr_buf, int buf_size)
{
    int n_out;

    n_out = box_get_abs_dpath(tk->box, dpath_expr_buf, buf_size);
    if (tracker_is_dangling(tk)) {
        return n_out;
    }
    if (n_out < buf_size) {
        dpath_expr_buf += n_out;
        buf_size -= n_out;
    } else {
        dpath_expr_buf = NULL;
        buf_size = 0;
    }
    n_out += track_path_elem_dump_to_buf(tk->cur,
                                         /* dump separator? */
                                         NULL != tk->box->parent_box,
                                         dpath_expr_buf, buf_size);
    return n_out;
}

/**
 * @brief similar than tracker_get_abs_dpath() but returns a
 * null-terminated string allocated with malloc().
 */
char *
tracker_get_abs_dpath_alloc(const struct tracker *tk)
{
    int path_len;
    char *path;

    path_len = tracker_get_abs_dpath(tk, NULL, 0);
    path = malloc(path_len + 1);
    if (NULL == path) {
        return NULL;
    }
    tracker_get_abs_dpath(tk, path, path_len + 1);
    return path;
}

int
tracker_dump_abs_dpath(const struct tracker *tk, FILE *stream)
{
    int n_out;

    n_out = box_dump_abs_dpath(tk->box, stream);
    n_out += track_path_elem_dump(tk->cur,
                                  /* dump separator? */
                                  NULL != tk->box->parent_box,
                                  stream);
    return n_out;
}

static bitpunch_status_t
tracker_compute_item_size_internal(struct tracker *tk,
                                   int64_t *item_sizep,
                                   struct browse_state *bst)
{
    struct filter_instance *f_instance;
    bitpunch_status_t bt_ret;

    /* If span size is being computed, skip this, it normally means an
     * item is being read to know its container box size. */
    if (0 == (tk->box->flags & COMPUTING_SPAN_SIZE)) {
        bt_ret = box_compute_max_span_size(tk->box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    bt_ret = tracker_compute_item_filter_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    f_instance = tk->dpath.item->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.compute_item_size) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->dpath.item, bst,
            "filter does not implement compute_item_size() tracker backend function");
    }
    return f_instance->b_tk.compute_item_size(tk, item_sizep, bst);
}

static bitpunch_status_t
tracker_compute_item_size(struct tracker *tk,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t item_size;

    DBG_TRACKER_DUMP(tk);
    assert(-1 != tk->item_offset);
    if (NULL != tk->item_box
        && -1 != tk->item_box->start_offset_used
        && -1 != tk->item_box->end_offset_used) {
        tk->item_size =
            tk->item_box->end_offset_used - tk->item_box->start_offset_used;
        DBG_TRACKER_CHECK_STATE(tk);
        return BITPUNCH_OK;
    }
    if (tracker_is_dangling(tk)) {
        return BITPUNCH_NO_ITEM;
    }
    bt_ret = tracker_compute_item_filter_internal(tk, bst);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_compute_item_size_internal(tk, &item_size, bst);
    }
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_set_item_size(tk, item_size, bst);
    } else {
        tracker_error_add_tracker_context(tk, bst,
                                          "when computing item size");
    }
    return bt_ret;
}

bitpunch_status_t
tracker_get_item_size_internal(struct tracker *tk, int64_t *item_sizep,
                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_compute_item_filter_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (0 == (tk->dpath.item->ndat->u.item.flags
              & ITEMFLAG_IS_USED_SIZE_DYNAMIC)) {
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
            bt_ret = tracker_set_item_size(
                tk, tk->dpath.item->ndat->u.item.min_span_size, bst);
            if (BITPUNCH_OK == bt_ret && NULL != item_sizep) {
                *item_sizep = tk->item_size;
            }
            return bt_ret;
        } else {
            if (NULL != item_sizep) {
                *item_sizep = tk->dpath.item->ndat->u.item.min_span_size;
            }
            return BITPUNCH_OK;
        }
    }
    if (-1 == tk->item_size) {
        bt_ret = tracker_compute_item_location(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        assert(-1 != tk->item_size);
        bt_ret = tracker_check_item(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    if (NULL != item_sizep) {
        *item_sizep = tk->item_size;
    }
    DBG_TRACKER_DUMP(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_compute_item_location(struct tracker *tk,
                              struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = tracker_compute_item_offset(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return tracker_compute_item_size(tk, bst);
}

static bitpunch_status_t
tracker_get_item_key_internal(struct tracker *tk,
                              expr_value_t *keyp,
                              struct browse_state *bst)
{
    struct filter_instance *f_instance;

    DBG_TRACKER_DUMP(tk);
    if (tracker_is_dangling(tk)) {
        return BITPUNCH_NO_ITEM;
    }
    f_instance = tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.get_item_key) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->dpath.filter, bst,
            "filter does not implement get_item_key() tracker backend function");
    }
    return f_instance->b_tk.get_item_key(
        tk, keyp, NULL, bst);
}

static bitpunch_status_t
tracker_get_item_key_multi_internal(struct tracker *tk,
                                    expr_value_t *keyp,
                                    int *nth_twinp,
                                    struct browse_state *bst)
{
    struct filter_instance *f_instance;

    DBG_TRACKER_DUMP(tk);
    if (tracker_is_dangling(tk)) {
        return BITPUNCH_NO_ITEM;
    }
    f_instance = tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.get_item_key) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->dpath.filter, bst,
            "filter does not implement get_item_key() tracker backend function");
    }
    return f_instance->b_tk.get_item_key(
        tk, keyp, nth_twinp, bst);
}

bitpunch_status_t
tracker_get_item_location_internal(struct tracker *tk,
                                   int64_t *item_offsetp,
                                   int64_t *item_sizep,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = tracker_get_item_offset_internal(tk, item_offsetp, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = tracker_get_item_size_internal(tk, item_sizep, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
tracker_read_item_raw_internal(struct tracker *tk,
                               const char **item_contentsp,
                               int64_t *item_sizep,
                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = tracker_compute_item_location(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != tk->item_offset);
    assert(-1 != tk->item_size);
    if (NULL != item_contentsp) {
        bt_ret = box_apply_filter_internal(tk->box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        *item_contentsp = tk->box->file_hdl->bf_data + tk->item_offset;
    }
    if (NULL != item_sizep) {
        *item_sizep = tk->item_size;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
filtered_dpath_read_value_internal(expr_dpath_t dpath,
                                   expr_value_t *expr_valuep,
                                   struct browse_state *bst)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return tracker_read_item_value_direct_internal(dpath.item.tk,
                                                       expr_valuep, bst);
    case EXPR_DPATH_TYPE_CONTAINER:
        return box_read_value_internal(dpath.container.box,
                                       expr_valuep, bst);
    default:
        assert(0);
    }
}

bitpunch_status_t
tracker_read_item_value_internal(struct tracker *tk,
                                 expr_value_t *valuep,
                                 struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    expr_dpath_t dpath;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_get_filtered_dpath_internal(tk, &dpath, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = filtered_dpath_read_value_internal(dpath, valuep, bst);
    expr_dpath_destroy(dpath);
    return bt_ret;
}

bitpunch_status_t
tracker_read_item_value_direct_internal(struct tracker *tk,
                                        expr_value_t *valuep,
                                        struct browse_state *bst)
{
    struct filter_instance *f_instance;
    bitpunch_status_t bt_ret;
    int64_t item_offset;
    int64_t item_size;
    struct ast_node_hdl *filter_type;

    bt_ret = tracker_get_item_location_internal(
        tk, &item_offset, &item_size, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = box_apply_filter_internal(tk->box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = expr_evaluate_filter_type_internal(
        tk->dpath.filter, tk->box, FILTER_KIND_FILTER,
        &filter_type, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    f_instance = filter_type->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_item.read_value) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, filter_type, bst,
            "filter does not implement read_value() item backend function");
    }
    bt_ret = f_instance->b_item.read_value(
        filter_type, tk->box, item_offset, item_size, valuep, bst);
    if (BITPUNCH_OK == bt_ret && NULL != valuep) {
        expr_value_attach_box(valuep, tk->box);
    }
    return bt_ret;
}

bitpunch_status_t
tracker_reverse_direction_internal(struct tracker *tk,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    if (!tracker_is_dangling(tk)
        && 0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        if (-1 == tk->item_size) {
            bt_ret = tracker_compute_item_location(tk, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
        }
        if (0 != (tk->flags & TRACKER_REVERSED)) {
            assert(-1 == tk->box->start_offset_used
                   || tk->item_offset - tk->item_size
                   >= tk->box->start_offset_used);
            tk->item_offset -= tk->item_size;
        } else {
            assert(-1 == tk->box->end_offset_used
                   || tk->item_offset + tk->item_size
                   <= tk->box->end_offset_used);
            tk->item_offset += tk->item_size;
        }
    }
    tk->flags ^= TRACKER_REVERSED;
    return BITPUNCH_OK;
}

/*
 * tracker error management
 */

static void
tracker_error_init(struct tracker_error *tk_err,
                   bitpunch_status_t bt_ret)
{
    memset(tk_err, 0, sizeof (*tk_err));
    tk_err->bt_ret = bt_ret;
    tk_err->error_buf_end = tk_err->error_buf;
    tk_err->error_buf[0] = '\0';
}

static struct tracker_error *
tracker_error_new(bitpunch_status_t bt_ret,
                  struct tracker *tk, struct box *box,
                  const struct ast_node_hdl *node,
                  const char *message_fmt, va_list message_args)
{
    struct tracker_error *tk_err;

    tk_err = new_safe(struct tracker_error);
    if (NULL != tk) {
        assert(NULL == box);
        tk_err->tk = tracker_dup_raw(tk);
    } else {
        tk_err->box = box;
        box_acquire(box);
    }
    tk_err->bt_ret = bt_ret;
    tk_err->node = node;
    if (NULL != message_fmt) {
        tk_err->error_buf_end =
            tk_err->error_buf +
            vsnprintf(tk_err->error_buf, sizeof (tk_err->error_buf),
                      message_fmt, message_args) + 1;
        if (tk_err->error_buf_end >=
            tk_err->error_buf + sizeof (tk_err->error_buf))
            tk_err->error_buf_end =
                tk_err->error_buf + sizeof (tk_err->error_buf) - 1;
    } else {
        strcpy(tk_err->error_buf, bitpunch_status_pretty(bt_ret));
        tk_err->error_buf_end =
            tk_err->error_buf + strlen(tk_err->error_buf) + 1;
    }
    tk_err->reason = tk_err->error_buf;
    return tk_err;
}

void
tracker_error_destroy(struct tracker_error *tk_err)
{
    int ctx_i;

    if (NULL != tk_err) {
        tracker_delete(tk_err->tk);
        box_delete(tk_err->box);
        for (ctx_i = 0; ctx_i < tk_err->n_contexts; ++ctx_i) {
            struct tracker_error_context_info *ctx_info;

            ctx_info = &tk_err->contexts[ctx_i];
            tracker_delete(ctx_info->tk);
            box_delete(ctx_info->box);
        }
        if (!(tk_err->flags & TRACKER_ERROR_STATIC)) {
            free(tk_err);
        }
    }
}

void
tracker_error_dump(struct tracker_error *tk_err, FILE *out)
{
    fprintf(out, "%s - %s",
            bitpunch_status_pretty(tk_err->bt_ret),
            tk_err->reason);
}

bitpunch_status_t
tracker_error(bitpunch_status_t bt_ret, struct tracker *tk,
              const struct ast_node_hdl *node,
              struct browse_state *bst,
              const char *message_fmt, ...)
{
    va_list ap;

    browse_state_clear_error(bst);

    va_start(ap, message_fmt);
    bst->last_error = tracker_error_new(bt_ret, tk, NULL, node,
                                        message_fmt, ap);
    va_end(ap);
    DBG_TRACKER_DUMP(tk);
    return bt_ret;
}

bitpunch_status_t
box_error(bitpunch_status_t bt_ret, struct box *box,
          const struct ast_node_hdl *node,
          struct browse_state *bst,
          const char *message_fmt, ...)
{
    va_list ap;

    browse_state_clear_error(bst);

    va_start(ap, message_fmt);
    bst->last_error = tracker_error_new(bt_ret, NULL, box, node,
                                        message_fmt, ap);
    va_end(ap);
    DBG_BOX_DUMP(box);
    return bt_ret;
}

// FIXME make a common error handling layer outside of browse.c
bitpunch_status_t
node_error(bitpunch_status_t bt_ret,
           const struct ast_node_hdl *node,
           struct browse_state *bst,
           const char *message_fmt, ...)
{
    va_list ap;

    browse_state_clear_error(bst);

    va_start(ap, message_fmt);
    bst->last_error = tracker_error_new(bt_ret, NULL, NULL, node,
                                        message_fmt, ap);
    va_end(ap);
    return bt_ret;
}

bitpunch_status_t
box_error_out_of_bounds(struct box *box,
                        const struct ast_node_hdl *node,
                        enum box_offset_type requested_offset_type,
                        int64_t requested_offset,
                        enum box_offset_type registered_offset_type,
                        struct browse_state *bst)
{
    struct tracker_error *tk_err;

    DBG_BOX_DUMP(box);
    if (NULL != error_get_expected(BITPUNCH_OUT_OF_BOUNDS_ERROR, bst)) {
        return BITPUNCH_OUT_OF_BOUNDS_ERROR;
    }
    // FIXME make this message correct for RALIGN boxes
    (void) box_error(BITPUNCH_OUT_OF_BOUNDS_ERROR, box, node, bst,
                     "request offset out of box bounds: "
                     "box %s space is [%"PRIi64"..%"PRIi64"[, "
                     "requested %s offset at %"PRIi64"",
                     box_offset_type_str(registered_offset_type),
                     box->start_offset_used,
                     box_get_offset(box, registered_offset_type),
                     box_offset_type_str(requested_offset_type),
                     requested_offset);
    tk_err = bst->last_error;
    assert(NULL != tk_err);
    tk_err->u.out_of_bounds.registered_offset_type = registered_offset_type;
    tk_err->u.out_of_bounds.registered_offset =
        box_get_offset(box, registered_offset_type);
    tk_err->u.out_of_bounds.requested_offset_type = requested_offset_type;
    tk_err->u.out_of_bounds.requested_offset = requested_offset;

    return BITPUNCH_OUT_OF_BOUNDS_ERROR;
}

bitpunch_status_t
tracker_error_item_out_of_bounds(struct tracker *tk,
                                 struct browse_state *bst)
{
    struct tracker_error *tk_err;
    char item_span_msg[128];
    int64_t out_of_bounds_offset;

    DBG_TRACKER_DUMP(tk);
    assert(NULL != tk->dpath.item);
    assert(tk->item_offset >= 0);
    if (NULL != error_get_expected(BITPUNCH_OUT_OF_BOUNDS_ERROR, bst)) {
        return BITPUNCH_OUT_OF_BOUNDS_ERROR;
    }
    if (-1 != tk->item_size) {
        snprintf(item_span_msg, sizeof (item_span_msg),
                 "item spans [%"PRIi64"..%"PRIi64"[",
                 tk->item_offset, tk->item_offset + tk->item_size);
        out_of_bounds_offset = tk->item_offset + tk->item_size;
    } else if (!tracker_is_dangling(tk)) {
        snprintf(item_span_msg, sizeof (item_span_msg),
                 "item spans [%"PRIi64"..[",
                 tk->item_offset);
        out_of_bounds_offset = tk->item_offset;
    } else {
        snprintf(item_span_msg, sizeof (item_span_msg),
                 "last item spans [..%"PRIi64"[",
                 tk->item_offset);
        out_of_bounds_offset = tk->item_offset;
    }
    (void) tracker_error(
        BITPUNCH_OUT_OF_BOUNDS_ERROR, tk, tk->dpath.item, bst,
        "item location out of container box bounds: "
        "box %s space is [%"PRIi64"..%"PRIi64"[, %s",
        box_offset_type_str(box_get_known_end_offset_type(tk->box)),
        tk->box->start_offset_used, box_get_known_end_offset(tk->box),
        item_span_msg);
    tk_err = bst->last_error;
    assert(NULL != tk_err);
    tk_err->u.out_of_bounds.registered_offset_type =
        box_get_known_end_offset_type(tk->box);
    tk_err->u.out_of_bounds.registered_offset =
        box_get_known_end_offset(tk->box);
    tk_err->u.out_of_bounds.requested_offset_type =
        BOX_END_OFFSET_USED;
    tk_err->u.out_of_bounds.requested_offset =
        out_of_bounds_offset;

    return BITPUNCH_OUT_OF_BOUNDS_ERROR;
}

static void
tracker_error_add_context_internal(struct tracker *tk,
                                   struct box *box,
                                   const struct ast_node_hdl *node,
                                   const char *context_fmt,
                                   va_list context_args,
                                   struct browse_state *bst)
{
    struct tracker_error *tk_err;
    struct tracker_error_context_info *ctx;

    tk_err = bst->last_error;
    if (NULL == tk_err || NULL != error_get_expected(tk_err->bt_ret,
                                                     bst)) {
        return ;
    }
    if (tk_err->n_contexts == N_ELEM(tk_err->contexts)) {
        return ;
    }
    ctx = &tk_err->contexts[tk_err->n_contexts];
    ++tk_err->n_contexts;

    if (NULL != box) {
        ctx->box = box;
        box_acquire(box);
    }
    if (NULL != tk) {
        ctx->tk = tracker_dup_raw(tk);
    }
    ctx->node = node;
    if (NULL != context_fmt) {
        ctx->message = tk_err->error_buf_end;
        tk_err->error_buf_end += vsnprintf(
            tk_err->error_buf_end,
            tk_err->error_buf + sizeof (tk_err->error_buf)
            - tk_err->error_buf_end,
            context_fmt, context_args) + 1;

        if (tk_err->error_buf_end >=
            tk_err->error_buf + sizeof (tk_err->error_buf)) {
            tk_err->error_buf_end =
                tk_err->error_buf + sizeof (tk_err->error_buf) - 1;
        }
    }
}

void
tracker_error_add_context_message(struct browse_state *bst,
                                  const char *context_fmt, ...)
{
    va_list ap;

    va_start(ap, context_fmt);
    tracker_error_add_context_internal(NULL, NULL, NULL,
                                       context_fmt, ap, bst);
    va_end(ap);
}

void
tracker_error_add_tracker_context(struct tracker *tk,
                                  struct browse_state *bst,
                                  const char *context_fmt, ...)
{
    va_list ap;

    DBG_TRACKER_DUMP(tk);
    va_start(ap, context_fmt);
    tracker_error_add_context_internal(tk, NULL, NULL,
                                       context_fmt, ap, bst);
    va_end(ap);
}

void
tracker_error_add_box_context(struct box *box,
                              struct browse_state *bst,
                              const char *context_fmt, ...)
{
    va_list ap;

    DBG_BOX_DUMP(box);
    va_start(ap, context_fmt);
    tracker_error_add_context_internal(NULL, box, NULL,
                                       context_fmt, ap, bst);
    va_end(ap);
}

void
tracker_error_add_node_context(const struct ast_node_hdl *node,
                               struct browse_state *bst,
                               const char *context_fmt, ...)
{
    va_list ap;

    va_start(ap, context_fmt);
    tracker_error_add_context_internal(NULL, NULL, node,
                                       context_fmt, ap, bst);
    va_end(ap);
}

/*
 * tracking backends
 */

static bitpunch_status_t
tracker_compute_item_size__item_box(struct tracker *tk,
                                    int64_t *item_sizep,
                                    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_create_item_box_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = box_compute_used_size(tk->item_box, bst);
    if (BITPUNCH_OK == bt_ret) {
        assert(tk->item_box->end_offset_used >= 0);
        *item_sizep =
            tk->item_box->end_offset_used - tk->item_box->start_offset_used;
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return bt_ret;
}


static bitpunch_status_t
tracker_compute_item_size__byte(struct tracker *tk,
                                int64_t *item_sizep,
                                struct browse_state *bst)
{
    *item_sizep = 1;
    return BITPUNCH_OK;
}


static bitpunch_status_t
tracker_compute_item_size__static_size(struct tracker *tk,
                                       int64_t *item_sizep,
                                       struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = tracker_compute_item_filter_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    *item_sizep = tk->dpath.item->ndat->u.item.min_span_size;
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_compute_item_size__filter_span_size(
    struct tracker *tk,
    struct ast_node_hdl *span_filter,
    int64_t *span_sizep,
    struct browse_state *bst)
{
    const char *item_data;
    int64_t span_size;
    int64_t used_size;
    bitpunch_status_t bt_ret;
    int64_t max_slack_offset;

    DBG_TRACKER_DUMP(tk);
    assert(NULL != span_filter);
    assert(NULL != span_filter->ndat->u.rexpr_filter.f_instance->get_size_func);

    bt_ret = box_apply_filter_internal(tk->box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    item_data = tk->box->file_hdl->bf_data + tk->item_offset;

    if ((tk->box->flags & (COMPUTING_SPAN_SIZE |
                       COMPUTING_MAX_SLACK_OFFSET))) {
        if (0 != (tk->box->flags & BOX_RALIGN)) {
            max_slack_offset = box_get_known_start_offset_mask(
                tk->box, (BOX_START_OFFSET_MAX_SPAN |
                      BOX_START_OFFSET_SLACK |
                      BOX_START_OFFSET_PARENT));
        } else {
            max_slack_offset = box_get_known_end_offset_mask(
                tk->box, (BOX_END_OFFSET_MAX_SPAN |
                      BOX_END_OFFSET_SLACK |
                      BOX_END_OFFSET_PARENT));
        }
    } else {
        bt_ret = box_get_children_slack(tk->box, FALSE, &max_slack_offset, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    bt_ret = span_filter->ndat->u.rexpr_filter.f_instance->get_size_func(
        span_filter, tk->box, &span_size, &used_size, item_data,
        max_slack_offset - tk->item_offset, bst);
    if (BITPUNCH_OK != bt_ret) {
        return tracker_error(bt_ret, tk, span_filter, bst,
                             "filter couldn't return field's sizes");
    }
    *span_sizep = span_size;
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_item_used_size_internal__byte_array_filter_size(
    struct box *box,
    int64_t item_offset,
    struct ast_node_hdl *used_filter,
    int64_t *used_sizep,
    struct browse_state *bst)
{
    const char *item_data;
    int64_t used_size;
    int64_t dummy_size;
    bitpunch_status_t bt_ret;
    int64_t max_slack_offset;

    DBG_BOX_DUMP(box);
    assert(NULL != used_filter);
    assert(NULL != used_filter->ndat->u.rexpr_filter.f_instance->get_size_func);

    bt_ret = box_apply_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    item_data = box->file_hdl->bf_data + item_offset;

    if ((box->flags & (COMPUTING_SPAN_SIZE |
                       COMPUTING_MAX_SLACK_OFFSET))) {
        if (0 != (box->flags & BOX_RALIGN)) {
            max_slack_offset = box_get_known_start_offset_mask(
                box, (BOX_START_OFFSET_MAX_SPAN |
                      BOX_START_OFFSET_SLACK |
                      BOX_START_OFFSET_PARENT));
        } else {
            max_slack_offset = box_get_known_end_offset_mask(
                box, (BOX_END_OFFSET_MAX_SPAN |
                      BOX_END_OFFSET_SLACK |
                      BOX_END_OFFSET_PARENT));
        }
    } else {
        bt_ret = box_get_children_slack(box, FALSE, &max_slack_offset, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    bt_ret = used_filter->ndat->u.rexpr_filter.f_instance->get_size_func(
        used_filter, box, &dummy_size, &used_size, item_data,
        max_slack_offset - item_offset, bst);
    if (BITPUNCH_OK != bt_ret) {
        return box_error(bt_ret, box, used_filter, bst,
                         "filter couldn't return field's used size");
    }
    *used_sizep = used_size;
    return BITPUNCH_OK;
}


static bitpunch_status_t
box_compute_used_size__static_size(struct box *box,
                                   struct browse_state *bst)
{
    DBG_BOX_DUMP(box);
    return box_set_used_size(box, box->dpath.filter->ndat->u.item.min_span_size, bst);
}


static bitpunch_status_t
box_compute_slack_size__from_parent(struct box *box,
                                    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    assert(NULL != box->parent_box);
    bt_ret = box_compute_max_span_size(box->parent_box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (0 != (box->flags & BOX_RALIGN)) {
        return box_set_start_offset(
            box, box_get_known_start_offset(box->parent_box),
            BOX_START_OFFSET_SLACK, bst);
    } else {
        return box_set_end_offset(
            box, box_get_known_end_offset(box->parent_box),
            BOX_END_OFFSET_SLACK, bst);
    }
}

static bitpunch_status_t
box_compute_max_span_size__as_used(struct box *box,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    bt_ret = box_compute_used_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != box->start_offset_used && -1 != box->end_offset_used);
    return box_set_size(
        box, box->end_offset_used - box->start_offset_used,
        BOX_SIZE_MAX_SPAN, bst);
}

static bitpunch_status_t
box_compute_max_span_size__as_slack(struct box *box,
                                    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    bt_ret = box_compute_slack_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != box->start_offset_slack && -1 != box->end_offset_slack);
    return box_set_size(
        box, box->end_offset_slack - box->start_offset_slack,
        BOX_SIZE_MAX_SPAN, bst);
}

static bitpunch_status_t
box_compute_used_size__from_parent(struct box *box,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    assert(NULL != box->parent_box);

    bt_ret = box_compute_used_size(box->parent_box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_size(box, box->parent_box->end_offset_used
                        - box->parent_box->start_offset_used,
                        BOX_SIZE_USED, bst);
}

static bitpunch_status_t
box_compute_used_size__as_max_span(struct box *box,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    bt_ret = box_compute_max_span_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_size(box, box->end_offset_max_span
                        - box->start_offset_max_span,
                        BOX_SIZE_USED, bst);
}

static bitpunch_status_t
box_compute_used_size__as_bytes(struct box *box,
                                struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    assert(NULL != box->parent_box);

    bt_ret = box_compute_used_size(box->parent_box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (NULL != box->dpath.filter_defining_used_size) {
        int64_t used_size;

        bt_ret = box_compute_item_used_size_internal__byte_array_filter_size(
            box->parent_box, box_get_start_offset(box),
            box->dpath.filter_defining_used_size, &used_size, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        return box_set_used_size(box, used_size, bst);
    } else {
        return box_compute_used_size__from_parent(box, bst);
    }
}

static bitpunch_status_t
box_compute_min_span_size__as_hard_min(struct box *box,
                                       struct browse_state *bst)
{
    int64_t hard_min;

    DBG_BOX_DUMP(box);
    hard_min = ast_node_get_min_span_size(box->dpath.filter);
    return box_set_size(box, hard_min, BOX_SIZE_MIN_SPAN, bst);
}

static bitpunch_status_t
box_compute_slack_size__from_file_hdl(struct box *box,
                                      struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t file_size;

    DBG_BOX_DUMP(box);
    if (NULL == box->file_hdl) {
        assert(0 == (box->flags & BOX_FILTER_APPLIED));
        bt_ret = box_apply_filter_internal(box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    file_size = (int64_t)box->file_hdl->bf_data_length;
    return box_set_end_offset(box, file_size, BOX_END_OFFSET_SLACK, bst);
}

static bitpunch_status_t
box_compute_slack_size__as_bytes(struct box *box,
                                 struct browse_state *bst)
{
    DBG_BOX_DUMP(box);
    if (0 != (box->flags & BOX_DATA_SOURCE)) {
        return box_compute_slack_size__from_file_hdl(box, bst);
    } else {
        return box_compute_slack_size__from_parent(box, bst);
    }
}

static bitpunch_status_t
box_compute_min_span_size__span_expr(struct box *box,
                                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    const struct named_expr *span_stmt;
    int span_expr_defines_max;
    expr_value_t span_size;

    DBG_BOX_DUMP(box);
    span_expr_defines_max = FALSE;
    bt_ret = filter_evaluate_attribute_internal(
        box->dpath.filter, box, "@minspan", &span_stmt, &span_size, NULL, bst);
    if (BITPUNCH_NO_ITEM == bt_ret) {
        span_expr_defines_max = TRUE;
        bt_ret = filter_evaluate_attribute_internal(
            box->dpath.filter, box, "@span", &span_stmt, &span_size, NULL, bst);
    }
    switch (bt_ret) {
    case BITPUNCH_OK:
        break ;
    case BITPUNCH_NO_ITEM:
        // no dynamic span enabled by conditional: min span is the
        // hard minimum
        return box_compute_min_span_size__as_hard_min(box, bst);
    default:
        return bt_ret;
    }
    /* TODO: show data path info in errors */
    if (span_size.integer < 0) {
        return box_error(BITPUNCH_DATA_ERROR, box, span_stmt->expr, bst,
                         "evaluation of span size expression gives "
                         "negative value (%"PRIi64")", span_size.integer);
    }
    bt_ret = box_set_min_span_size(box, span_size.integer, bst);
    if (span_expr_defines_max && BITPUNCH_OK == bt_ret) {
        bt_ret = box_set_max_span_size(box, span_size.integer, bst);
    }
    return bt_ret;
}

static bitpunch_status_t
box_compute_max_span_size__span_expr(struct box *box,
                                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    const struct named_expr *span_stmt;
    int span_expr_defines_min;
    expr_value_t span_size;

    DBG_BOX_DUMP(box);
    span_expr_defines_min = FALSE;
    bt_ret = filter_evaluate_attribute_internal(
        box->dpath.filter, box, "@maxspan", &span_stmt, &span_size, NULL, bst);
    if (BITPUNCH_NO_ITEM == bt_ret) {
        span_expr_defines_min = TRUE;
        bt_ret = filter_evaluate_attribute_internal(
            box->dpath.filter, box, "@span", &span_stmt, &span_size, NULL, bst);
    }
    switch (bt_ret) {
    case BITPUNCH_OK:
        break ;
    case BITPUNCH_NO_ITEM:
        // no dynamic span enabled by conditional
        return box_compute_max_span_size__as_slack(box, bst);
    default:
        return bt_ret;
    }
    /* TODO: show data path info in errors */
    if (span_size.integer < 0) {
        return box_error(BITPUNCH_DATA_ERROR, box, span_stmt->expr, bst,
                         "evaluation of span size expression gives "
                         "negative value (%"PRIi64")", span_size.integer);
    }
    bt_ret = box_compute_slack_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (span_size.integer <= box->end_offset_slack) {
        bt_ret = box_set_max_span_size(box, span_size.integer, bst);
    } else {
        bt_ret = box_compute_max_span_size__as_slack(box, bst);
    }
    if (span_expr_defines_min && BITPUNCH_OK == bt_ret) {
        bt_ret = box_set_min_span_size(box, span_size.integer, bst);
    }
    return bt_ret;
}


static bitpunch_status_t
box_compute_used_size__packed_dynamic_size(struct box *box,
                                           struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct tracker *tk;

    DBG_BOX_DUMP(box);
    tk = tracker_new(box);
    tk->flags |= (TRACKER_NEED_ITEM_OFFSET |
                  ((box->flags & BOX_RALIGN) ? TRACKER_REVERSED : 0u));
    // TODO remove special case?
    if (AST_NODE_TYPE_COMPOSITE == box->dpath.filter->ndat->type) {
        bt_ret = tracker_goto_first_item_int__composite(tk, TRUE, bst);
        while (BITPUNCH_OK == bt_ret) {
            bt_ret = tracker_goto_next_item_int__composite(tk, TRUE, bst);
        }
    } else {
        bt_ret = tracker_goto_first_item_internal(tk, bst);
        while (BITPUNCH_OK == bt_ret) {
            bt_ret = tracker_goto_next_item_internal(tk, bst);
        }
    }
    if (BITPUNCH_NO_ITEM == bt_ret) {
        assert(-1 != tk->item_offset);
        if (0 != (box->flags & BOX_RALIGN)) {
            bt_ret = box_set_start_offset(tk->box, tk->item_offset,
                                          BOX_START_OFFSET_USED, bst);
        } else {
            bt_ret = box_set_end_offset(tk->box, tk->item_offset,
                                        BOX_END_OFFSET_USED, bst);
        }
    }
    tracker_delete(tk);
    return bt_ret;
}

static bitpunch_status_t
box_compute_used_size__union_dynamic_size(struct box *box,
                                          struct browse_state *bst)
{
    struct tracker *tk;
    bitpunch_status_t bt_ret;
    int64_t subitem_size;
    int64_t max_subitem_size;

    DBG_BOX_DUMP(box);
    tk = tracker_new(box);
    max_subitem_size = 0;
    bt_ret = tracker_goto_first_item_int__composite(tk, TRUE, bst);
    while (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_get_item_size_internal(tk, &subitem_size, bst);
        if (BITPUNCH_OK != bt_ret) {
            break ;
        }
        max_subitem_size = MAX(max_subitem_size, subitem_size);
        bt_ret = tracker_goto_next_item_int__composite(tk, TRUE, bst);
    }
    if (BITPUNCH_NO_ITEM == bt_ret) {
        bt_ret = box_set_used_size(box, max_subitem_size, bst);
    }
    tracker_delete(tk);
    return bt_ret;
}

static bitpunch_status_t
box_get_n_items__composite(struct box *box, int64_t *item_countp,
                       struct browse_state *bst)
{
    DBG_BOX_DUMP(box);
    return filter_get_n_statements_internal(
        box->dpath.filter, box, STATEMENT_TYPE_FIELD, NULL,
        item_countp, bst);
}

static bitpunch_status_t
tracker_get_item_key__composite(struct tracker *tk,
                            expr_value_t *keyp,
                            int *nth_twinp,
                            struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    assert(NULL != tk->cur.u.block.field);
    if (NULL != keyp) {
        *keyp = expr_value_as_string(tk->cur.u.block.field->nstmt.name);
    }
    if (NULL != nth_twinp) {
        /* field names are unique */
        *nth_twinp = 0;
    }
    return BITPUNCH_OK;
}


static bitpunch_status_t
tracker_goto_item_int__composite(struct tracker *tk,
                             const struct field *field, int flat,
                             struct browse_state *bst)
{
    struct tracker *xtk;
    bitpunch_status_t bt_ret;
    struct statement_iterator stit;
    const struct statement *stmt;

    DBG_TRACKER_DUMP(tk);
    if (flat
        || NULL != field->nstmt.name
        || 0 != (field->nstmt.stmt.stmt_flags & FIELD_FLAG_HIDDEN)) {
        tracker_set_field_internal__composite(tk, field, bst);
        return BITPUNCH_OK;
    }
    // recurse into anonymous struct's fields
    xtk = tracker_dup(tk);
    do {
        tracker_set_field_internal__composite(xtk, field, bst);
        bt_ret = tracker_enter_item_internal(xtk, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(xtk);
            return bt_ret;
        }
        if (0 != (tk->flags & TRACKER_REVERSED)) {
            stit = filter_riter_statements(
                xtk->box->dpath.filter, xtk->box, STATEMENT_TYPE_FIELD, NULL);
        } else {
            stit = filter_iter_statements(
                xtk->box->dpath.filter, xtk->box, STATEMENT_TYPE_FIELD, NULL);
        }
        bt_ret = filter_iter_statements_next_internal(&stit, NULL, &stmt, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(xtk);
            return bt_ret;
        }
        field = (const struct field *)stmt;
    } while (NULL == field->nstmt.name
             && 0 == (field->nstmt.stmt.stmt_flags & FIELD_FLAG_HIDDEN));
    tracker_set(tk, xtk);
    tracker_delete(xtk);
    tracker_set_field_internal__composite(tk, field, bst);
    return BITPUNCH_OK;
}

static int
tracker_in_anonymous_composite(struct tracker *tk)
{
    struct box *composite_box;
    struct box *parent_box;

    composite_box = box_get_container_parent_box(tk->box);
    parent_box = composite_box->parent_box;
    return (NULL != parent_box
            && AST_NODE_TYPE_COMPOSITE == parent_box->dpath.filter->ndat->type
            && NULL != composite_box->track_path.u.block.field
            && NULL == composite_box->track_path.u.block.field->nstmt.name
            && 0 == (composite_box->track_path.u.block.field
                     ->nstmt.stmt.stmt_flags & FIELD_FLAG_HIDDEN));
}

static bitpunch_status_t
tracker_goto_first_item_int__composite(struct tracker *tk, int flat,
                                   struct browse_state *bst)
{
    struct statement_iterator stit;
    const struct statement *stmt;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    if (!flat && NULL != tk->cur.u.block.field) {
        // return to base, non-anonymous level
        while (tracker_in_anonymous_composite(tk)) {
            bt_ret = tracker_return_internal(tk, bst);
            assert(BITPUNCH_OK == bt_ret);
        }
    }
    bt_ret = tracker_set_item_offset_at_box(tk, tk->box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (0 != (tk->flags & TRACKER_REVERSED)) {
        stit = filter_riter_statements(
            tk->box->dpath.filter, tk->box, STATEMENT_TYPE_FIELD, NULL);
    } else {
        stit = filter_iter_statements(
            tk->box->dpath.filter, tk->box, STATEMENT_TYPE_FIELD, NULL);
    }
    bt_ret = filter_iter_statements_next_internal(&stit, NULL, &stmt, bst);
    if (BITPUNCH_OK != bt_ret) {
        if (BITPUNCH_NO_ITEM == bt_ret) {
            bt_ret = tracker_set_end(tk, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
            return BITPUNCH_NO_ITEM;
        } else {
            return bt_ret;
        }
    }
    return tracker_goto_item_int__composite(tk, (const struct field *)stmt,
                                        flat, bst);
}


static bitpunch_status_t
tracker_goto_next_item_int__composite(struct tracker *tk, int flat,
                                  struct browse_state *bst)
{
    struct statement_iterator stit;
    const struct statement *stmt;
    bitpunch_status_t bt_ret;
    int reversed;

    DBG_TRACKER_DUMP(tk);
    reversed = (0 != (tk->flags & TRACKER_REVERSED));
    DBG_TRACKER_CHECK_STATE(tk);
    while (TRUE) {
        /* union: no offset change */
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
            struct filter_instance_composite *composite;

            composite = (struct filter_instance_composite *)
                tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
            if (COMPOSITE_TYPE_STRUCT == composite->type) {
                int64_t item_size;

                bt_ret = tracker_get_item_size_internal(tk, &item_size, bst);
                if (BITPUNCH_OK != bt_ret) {
                    DBG_TRACKER_CHECK_STATE(tk);
                    return bt_ret;
                }
                DBG_TRACKER_CHECK_STATE(tk);
                if (reversed) {
                    assert(tk->item_offset >= item_size);
                    tk->item_offset -= item_size;
                } else {
                    tk->item_offset += item_size;
                }
            }
        }
        tracker_reset_item_cache(tk);
        if (reversed) {
            stit = filter_riter_statements_from(
                tk->box->dpath.filter, tk->box,
                (const struct statement *)tk->cur.u.block.field, NULL);
        } else {
            stit = filter_iter_statements_from(
                tk->box->dpath.filter, tk->box,
                (const struct statement *)tk->cur.u.block.field, NULL);
        }
        bt_ret = filter_iter_statements_next_internal(&stit, NULL, &stmt, bst);
        if (BITPUNCH_NO_ITEM != bt_ret) {
            break ;
        }
        if (!flat && tracker_in_anonymous_composite(tk)) {
            // return from anonymous struct's field list
            bt_ret = tracker_return_internal(tk, bst);
            assert(BITPUNCH_OK == bt_ret);
        } else {
            bt_ret = tracker_set_end(tk, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
            return BITPUNCH_NO_ITEM;
        }
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return tracker_goto_item_int__composite(tk, (const struct field *)stmt,
                                        flat, bst);
}

static bitpunch_status_t
tracker_goto_first_item__composite(struct tracker *tk,
                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_goto_first_item_int__composite(tk, FALSE, bst);
    // skip hidden fields
    while (BITPUNCH_OK == bt_ret
           && 0 != (tk->cur.u.block.field->nstmt.stmt.stmt_flags
                    & FIELD_FLAG_HIDDEN)) {
        bt_ret = tracker_goto_next_item_int__composite(tk, FALSE, bst);
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_next_item__composite(struct tracker *tk,
                              struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    // skip hidden fields
    do {
        bt_ret = tracker_goto_next_item_int__composite(tk, FALSE, bst);
    } while (BITPUNCH_OK == bt_ret
             && 0 != (tk->cur.u.block.field->nstmt.stmt.stmt_flags
                      & FIELD_FLAG_HIDDEN));
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item__composite(struct tracker *tk, int64_t index,
                             struct browse_state *bst)
{
    struct tracker *xtk;
    bitpunch_status_t bt_ret;
    int64_t cur_idx;

    DBG_TRACKER_DUMP(tk);
    xtk = tracker_dup(tk);
    bt_ret = tracker_goto_first_item__composite(xtk, bst);
    cur_idx = 0;
    while (TRUE) {
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(xtk);
            return bt_ret;
        }
        if (cur_idx == index) {
            break ;
        }
        ++cur_idx;
        bt_ret = tracker_goto_next_item__composite(xtk, bst);
    }
    tracker_set(tk, xtk);
    tracker_delete(xtk);
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_nth_item_with_key__composite(
    struct tracker *tk, expr_value_t item_key, int nth_twin,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    if (0 != nth_twin) {
        // all composite attributes are unique
        return BITPUNCH_NO_ITEM;
    }
    if (EXPR_VALUE_TYPE_STRING != item_key.type) {
        // all composite attributes are string-typed
        return BITPUNCH_NO_ITEM;
    }
    bt_ret = tracker_goto_first_item_int__composite(tk, FALSE, bst);
    while (BITPUNCH_OK == bt_ret) {
        if (NULL != tk->cur.u.block.field->nstmt.name
            && strlen(tk->cur.u.block.field->nstmt.name) == item_key.string.len
            && 0 == memcmp(tk->cur.u.block.field->nstmt.name,
                           item_key.string.str, item_key.string.len)) {
            return BITPUNCH_OK;
        }
        bt_ret = tracker_goto_next_item_int__composite(tk, FALSE, bst);
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_named_item__composite(struct tracker *tk, const char *name,
                                   struct browse_state *bst)
{
    expr_value_t key;

    key = expr_value_as_string(name);
    return tracker_goto_nth_item_with_key__composite(tk, key, 0, bst);
}

static bitpunch_status_t
tracker_goto_next_key_match__composite(struct tracker *tk,
                                   expr_value_t index,
                                   struct track_path search_boundary,
                                   struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst, NULL);
}


static bitpunch_status_t
tracker_compute_item_size__array_static_item_size(struct tracker *tk,
                                                  int64_t *item_sizep,
                                                  struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    int64_t child_item_size;
    expr_value_t item_count;

    bt_ret = tracker_compute_item_filter_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    array = (struct filter_instance_array *)
        tk->dpath.item->ndat->u.rexpr_filter.f_instance;
    bt_ret = expr_evaluate_value_internal(array->item_count, tk->box,
                                          &item_count, bst);
    if (BITPUNCH_OK != bt_ret) {
        // FIXME more appropriate context
        tracker_error_add_box_context(
            tk->box, bst, "when evaluating array item count expression");
        return bt_ret;
    }
    child_item_size = ast_node_get_min_span_size(array->item_type.item);
    *item_sizep = item_count.integer * child_item_size;
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_used_size__array_static_item_size(struct box *box,
                                              struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    int64_t n_elems;
    int64_t item_size;

    DBG_BOX_DUMP(box);
    /* array with dynamic item count but static item size:
     * evaluate item count and multiply by item unitary size to
     * get span size */
    bt_ret = box_get_n_items_internal(box, &n_elems, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    array = (struct filter_instance_array *)
        box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    item_size = ast_node_get_min_span_size(array->item_type.item);
    return box_set_used_size(box, n_elems * item_size, bst);
}

static bitpunch_status_t
box_compute_slack_size__container_slack(struct box *box,
                                        struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int get_left_offset;
    int64_t max_slack_offset;

    DBG_BOX_DUMP(box);
    assert(NULL != box->parent_box);
    /* slack child is limited by parent's maximum slack offset */
    get_left_offset = 0 != (box->flags & BOX_RALIGN);
    bt_ret = box_get_children_slack(box->parent_box, get_left_offset,
                                    &max_slack_offset, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != max_slack_offset);
    if (0 != (box->flags & BOX_RALIGN)) {
        return box_set_start_offset(box, max_slack_offset,
                                    BOX_START_OFFSET_SLACK, bst);
    } else {
        return box_set_end_offset(box, max_slack_offset,
                                  BOX_END_OFFSET_SLACK, bst);
    }
}

static bitpunch_status_t
box_get_children_slack__struct(struct box *box,
                               int get_left_offset,
                               int64_t *max_slack_offsetp,
                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct tracker *tk;
    const struct field *field;

    // FIXME this may need rework to get correctly the left-side max
    // slack offset
    DBG_BOX_DUMP(box);
    tk = tracker_new(box);
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    if (!get_left_offset) {
        // only reverse tracker when getting the right slack offset
        // (tracking fields backwards until a non-raligned field is
        // encountered)
        tk->flags |= TRACKER_REVERSED;
    }
    bt_ret = tracker_goto_first_item_int__composite(tk, TRUE, bst);
    while (BITPUNCH_OK == bt_ret) {
        field = tk->cur.u.block.field;
        if (get_left_offset) {
            if (0 == (field->nstmt.stmt.stmt_flags & FIELD_FLAG_HEADER)) {
                break ;
            }
        } else {
            if (0 == (field->nstmt.stmt.stmt_flags & FIELD_FLAG_TRAILER)) {
                break ;
            }
        }
        bt_ret = tracker_goto_next_item_int__composite(tk, TRUE, bst);
    }
    if (BITPUNCH_OK == bt_ret) {
        *max_slack_offsetp = tk->item_offset;
    }
    tracker_delete(tk);
    return bt_ret;
}

static bitpunch_status_t
tracker_get_item_key__array_generic(struct tracker *tk,
                                    expr_value_t *keyp,
                                    int *nth_twinp,
                                    struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    assert(-1 != tk->cur.u.array.index);
    if (NULL != keyp) {
        memset(keyp, 0, sizeof(*keyp));
        keyp->type = EXPR_VALUE_TYPE_INTEGER;
        keyp->integer = tk->cur.u.array.index;
    }
    if (NULL != nth_twinp) {
        /* only relevant for indexed arrays */
        *nth_twinp = 0;
    }
    return BITPUNCH_OK;
}


static bitpunch_status_t
tracker_get_item_key__indexed_array_internal(
    struct tracker *tk,
    int64_t from_index,
    expr_value_t *keyp,
    int *nth_twinp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *filtered_box;
    struct ast_node_hdl *key_expr;
    expr_value_t item_key;

    DBG_TRACKER_DUMP(tk);
    assert(-1 != tk->cur.u.array.index);
    bt_ret = tracker_get_filtered_item_box_internal(tk, &filtered_box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = box_apply_filter_internal(filtered_box, bst);
    if (BITPUNCH_OK == bt_ret) {
        key_expr = ast_node_get_key_expr(tk->box->dpath.filter);
        bt_ret = expr_evaluate_value_internal(key_expr, filtered_box,
                                              &item_key, bst);
    }
    box_delete_non_null(filtered_box);
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_tracker_context(
            tk, bst, "when evaluating item key expression");
        return bt_ret;
    }
    if (NULL != nth_twinp) {
        //TODO use hint flag if index is unique to avoid this
        //computation
        bt_ret = tracker_lookup_current_twin_index(
            tk, item_key,
            track_path_from_array_slice(from_index, -1),
            nth_twinp, bst);
        if (BITPUNCH_OK != bt_ret) {
            expr_value_destroy(item_key);
            return bt_ret;
        }
    }
    if (NULL != keyp) {
        *keyp = item_key;
    } else {
        expr_value_destroy(item_key);
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_get_item_key__indexed_array(struct tracker *tk,
                                    expr_value_t *keyp,
                                    int *nth_twinp,
                                    struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_get_item_key__indexed_array_internal(
        tk, 0 /* count twins from first item */,
        keyp, nth_twinp, bst);
}

static bitpunch_status_t
box_get_n_items__array_non_slack(struct box *box, int64_t *item_countp,
                                 struct browse_state *bst)
{
    struct filter_instance_array *array;
    expr_value_t item_count;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        bitpunch_status_t bt_ret;
        struct box *scope;

        array = (struct filter_instance_array *)
            box->dpath.filter->ndat->u.rexpr_filter.f_instance;
        assert(0 != (array->item_count->ndat->u.rexpr.value_type_mask
                     & EXPR_VALUE_TYPE_INTEGER));
        scope = box_get_scope_box(box->parent_box);
        bt_ret = expr_evaluate_value_internal(array->item_count, scope,
                                              &item_count, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        if (EXPR_VALUE_TYPE_INTEGER != item_count.type) {
            return box_error(
                BITPUNCH_DATA_ERROR, box, array->item_count, bst,
                "evaluation of array items count returned a value-type "
                "'%s', expect an integer",
                expr_value_type_str(item_count.type));
        }
        if (item_count.integer < 0) {
            return box_error(
                BITPUNCH_DATA_ERROR, box, array->item_count, bst,
                "evaluation of array items count gives "
                "negative value (%"PRIi64")", item_count.integer);
        }
        box->u.array_generic.n_items = item_count.integer;
    }
    if (NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_get_n_items__array_slack_static_item_size(struct box *box,
                                              int64_t *item_countp,
                                              struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    const struct ast_node_hdl *node;
    int64_t item_size;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        bt_ret = box_compute_slack_size(box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        assert(-1 != box->end_offset_slack);
        node = box->dpath.filter;
        array = (struct filter_instance_array *)
            node->ndat->u.rexpr_filter.f_instance;
        item_size = ast_node_get_min_span_size(array->item_type.item);
        if (0 == item_size) {
            return box_error(
                BITPUNCH_DATA_ERROR, box, array->item_type.item, bst,
                "slack array only contains items spanning 0 bytes: "
                "the item count cannot be computed");
        }
        /* deduce number of items from the available slack space and
         * the unit elem static size */
        box->u.array_generic.n_items =
            (box->end_offset_slack - box->start_offset_used) / item_size;
        /* now is a good time to set the used size as well */
        bt_ret = box_set_used_size(
            box, box->u.array_generic.n_items * item_size, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    if (NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_n_items_by_iteration(struct box *box,
                                 struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    struct tracker *tk;
    int64_t n_items;

    DBG_BOX_DUMP(box);
    /* first compute available slack space to notify iteration where
     * to stop */
    bt_ret = box_compute_slack_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    /* slow path: iterate over all elements */
    tk = tracker_new(box);
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    bt_ret = tracker_goto_first_item_internal(tk, bst);
    n_items = 0;
    while (BITPUNCH_OK == bt_ret) {
        ++n_items;
        bt_ret = tracker_goto_next_item_internal(tk, bst);
    }
    if (BITPUNCH_NO_ITEM == bt_ret) {
        if (-1 != box->u.array_generic.n_items) {
            // if any element contains a 'last' statement it must be
            // the last element of the array, otherwise trigger an
            // inconsistency error
            array = (struct filter_instance_array *)
                box->dpath.filter->ndat->u.rexpr_filter.f_instance;
            assert(0 != (array->item_type.item->flags
                         & ASTFLAG_CONTAINS_LAST_ATTR));
            if (box->u.array_generic.n_items != n_items) {
                tracker_delete(tk);
                return box_error(
                    BITPUNCH_DATA_ERROR, box, box->dpath.filter, bst,
                    "'last' keyword triggered in array item %"PRIi64" that "
                    "was not the last item of the array of size %"PRIi64"",
                    (n_items - 1), box->u.array_generic.n_items);
            }
        } else {
            box->u.array_generic.n_items = n_items;
        }
        if (0 != (box->flags & BOX_RALIGN)) {
            bt_ret = box_set_start_offset(box, tk->item_offset,
                                          BOX_START_OFFSET_USED, bst);
        } else {
            bt_ret = box_set_end_offset(box, tk->item_offset,
                                        BOX_END_OFFSET_USED, bst);
        }
    }
    tracker_delete(tk);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_get_n_items__by_iteration(struct box *box, int64_t *item_countp,
                              struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        bt_ret = box_compute_n_items_by_iteration(box, bst);
    } else {
        bt_ret = BITPUNCH_OK;
    }
    if (BITPUNCH_OK == bt_ret && NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return bt_ret;
}

static bitpunch_status_t
box_get_n_items__array_non_slack_with_last(struct box *box,
                                           int64_t *item_countp,
                                           struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t n_items;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        // start by evaluating array size
        bt_ret = box_get_n_items__array_non_slack(box, &n_items, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = box_compute_n_items_by_iteration(box, bst);
    } else {
        bt_ret = BITPUNCH_OK;
    }
    if (BITPUNCH_OK == bt_ret && NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return bt_ret;
}


static bitpunch_status_t
filter_read_value__bytes(struct ast_node_hdl *item_filter,
                         struct box *scope,
                         int64_t item_offset,
                         int64_t item_size,
                         expr_value_t *valuep,
                         struct browse_state *bst)
{
    if (NULL != valuep) {
        memset(valuep, 0, sizeof(*valuep));
        valuep->type = EXPR_VALUE_TYPE_BYTES;
        valuep->bytes.buf = scope->file_hdl->bf_data + item_offset;
        valuep->bytes.len = item_size;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
filter_read_value__filter(struct ast_node_hdl *filter,
                          struct box *scope,
                          int64_t item_offset,
                          int64_t item_size,
                          expr_value_t *valuep,
                          struct browse_state *bst)
{
    const char *item_data;

    item_data = scope->file_hdl->bf_data + item_offset;
    // if box filter is a data filter, getting the value is simply
    // returning the box data bytes as-is since they come from the
    // filter output
    if (0 != (scope->flags & BOX_DATA_SOURCE)) {
        return filter_read_value__bytes(filter, scope,
                                        item_offset, item_size,
                                        valuep, bst);
    }
    return filter_instance_read_value(filter, scope,
                                      item_data, item_size,
                                      valuep, bst);
}

static bitpunch_status_t
filter_read_value__operator_filter(struct ast_node_hdl *filter,
                                   struct box *scope,
                                   int64_t item_offset,
                                   int64_t item_size,
                                   expr_value_t *valuep,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct ast_node_hdl *filter_expr;
    struct ast_node_hdl *filter_type;

    filter_expr = filter->ndat->u.rexpr_op_filter.filter_expr;
    bt_ret = expr_evaluate_filter_type_internal(
        filter_expr, scope, FILTER_KIND_FILTER,
        &filter_type, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return filter_type->ndat->u.rexpr_filter.f_instance->b_item.read_value(
        filter_type, scope, item_offset, item_size, valuep, bst);
}



static bitpunch_status_t
tracker_goto_first_item__array_generic(struct tracker *tk,
                                       struct dpath_node *item_dpath,
                                       struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t n_items;

    DBG_TRACKER_DUMP(tk);
    bt_ret = box_get_n_items_internal(tk->box, &n_items, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != n_items);
    if (0 == n_items) {
        return BITPUNCH_NO_ITEM;
    }
    tk->cur.u.array.index = 0;
    // FIXME reset item to NULL if filter is dynamic
    tk->dpath.filter = item_dpath->filter;
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_first_item__array_non_slack(struct tracker *tk,
                                         struct browse_state *bst)
{
    struct filter_instance_array *array;

    DBG_TRACKER_DUMP(tk);
    array = (struct filter_instance_array *)
        tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    return tracker_goto_first_item__array_generic(tk, &array->item_type, bst);
}

static bitpunch_status_t
tracker_goto_first_item__array_slack(struct tracker *tk,
                                     struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    if (0 != (tk->flags & TRACKER_REVERSED) ||
        0 != (tk->box->flags & BOX_RALIGN)) {
        return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst,
                             "tracker_goto_first_item() not implemented "
                             "in reverse mode on slack arrays");
    }
    bt_ret = box_compute_slack_size(tk->box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    array = (struct filter_instance_array *)
        tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    /* slack arrays require a maintained item offset to browse their
     * elements */
    assert(-1 != tk->box->end_offset_slack);
    tk->item_offset = tk->box->start_offset_used;
    tk->cur.u.array.index = 0;
    // FIXME reset item to NULL if filter is dynamic
    tk->dpath.filter = array->item_type.filter;
    DBG_TRACKER_CHECK_STATE(tk);

    /* check if there's size for at least one element */
    WITH_EXPECTED_ERROR(BITPUNCH_OUT_OF_BOUNDS_ERROR, {
        bt_ret = tracker_compute_item_location(tk, bst);
    });
    if (BITPUNCH_OK != bt_ret) {
        if (BITPUNCH_OUT_OF_BOUNDS_ERROR == bt_ret) {
            browse_state_clear_error(bst);
            tracker_set_dangling(tk);
            return BITPUNCH_NO_ITEM;
        } else {
            return bt_ret;
        }
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_next_item__array(struct tracker *tk,
                              struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *filtered_box;
    int64_t item_size;
    expr_value_t last_eval;
    int is_last;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_compute_item_filter_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    is_last = FALSE;
    if (0 != (tk->dpath.item->flags & ASTFLAG_CONTAINS_LAST_ATTR)) {
        bt_ret = tracker_get_filtered_item_box_internal(tk, &filtered_box,
                                                        bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = filter_evaluate_attribute_internal(
            filtered_box->dpath.filter, filtered_box, "@last",
            NULL, &last_eval, NULL, bst);
        box_delete_non_null(filtered_box);
        switch (bt_ret) {
        case BITPUNCH_OK:
            assert(EXPR_VALUE_TYPE_BOOLEAN == last_eval.type);
            is_last = last_eval.boolean;
            break ;
        case BITPUNCH_NO_ITEM:
            break ;
        default:
            return bt_ret;
        }
    }
    if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        bt_ret = tracker_compute_item_location(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        item_size = tk->item_size;
        box_delete(tk->item_box);
        tk->item_box = NULL;
        tk->item_offset += (0 != (tk->flags & TRACKER_REVERSED) ?
                            -tk->item_size : tk->item_size);
        if (0 != (tk->dpath.item->ndat->u.item.flags
                  & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            tk->item_size = -1;
        }
    }
    ++tk->cur.u.array.index;
    if ((-1 != tk->box->u.array_generic.n_items
         && tk->cur.u.array.index == tk->box->u.array_generic.n_items)
        || (-1 == tk->box->u.array_generic.n_items
            && (0 != (tk->flags & TRACKER_REVERSED) ?
                tk->item_offset == tk->box->start_offset_slack :
                tk->item_offset == tk->box->end_offset_slack))
        || is_last) {
        bt_ret = tracker_set_end(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        return BITPUNCH_NO_ITEM;
    }
    /* check new item */
    if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)
        && -1 == tk->box->u.array_generic.n_items) {
        WITH_EXPECTED_ERROR(BITPUNCH_OUT_OF_BOUNDS_ERROR, {
            bt_ret = tracker_compute_item_size(tk, bst);
            if (BITPUNCH_OK == bt_ret) {
                bt_ret = tracker_check_item(tk, bst);
            }
        });
    } else {
        bt_ret = tracker_check_item(tk, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        if (BITPUNCH_OUT_OF_BOUNDS_ERROR == bt_ret
            && -1 == tk->box->u.array_generic.n_items) {
            // slack array can't host the additional item, stop here
            bt_ret = tracker_set_end(tk, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
            return BITPUNCH_NO_ITEM;
        }
        /* rollback */
        tracker_reset_item_cache(tk);
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
            tk->item_offset += (0 != (tk->flags & TRACKER_REVERSED) ?
                                item_size : -item_size);
        }
        --tk->cur.u.array.index;
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item__array_static_item_size(struct tracker *tk,
                                              int64_t index,
                                              struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    int64_t n_items;
    int64_t item_size;
    struct tracker *xtk;

    DBG_TRACKER_DUMP(tk);
    // size computation shall set n_items
    bt_ret = box_compute_used_size__array_static_item_size(tk->box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    n_items = tk->box->u.array_generic.n_items;
    assert(-1 != n_items);
    if (index >= n_items) {
        return BITPUNCH_NO_ITEM;
    }
    array = (struct filter_instance_array *)
        tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    if (0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        tk->cur.u.array.index = index;
        // FIXME reset item to NULL if filter is dynamic
        tk->dpath.filter = array->item_type.filter;
        tk->flags &= ~TRACKER_AT_END;
        DBG_TRACKER_CHECK_STATE(tk);
        return BITPUNCH_OK;
    }
    xtk = tracker_dup(tk);
    // FIXME reset item to NULL if filter is dynamic
    xtk->dpath.filter = array->item_type.filter;
    xtk->flags &= ~TRACKER_AT_END;
    tracker_reset_item_cache_internal(xtk);
    bt_ret = tracker_compute_item_filter_internal(xtk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    item_size = ast_node_get_min_span_size(xtk->dpath.item);
    /* no item cleanup, transform item info instead */
    xtk->item_offset = xtk->box->start_offset_used
        + (0 != (tk->flags & TRACKER_REVERSED) ? index + 1 : index)
        * item_size;
    xtk->item_size = item_size;
    xtk->cur.u.array.index = index;
    bt_ret = tracker_check_item(xtk, bst);
    if (BITPUNCH_OK == bt_ret) {
        tracker_set(tk, xtk);
    }
    tracker_delete(xtk);
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item__array_slack_dynamic_item_size(
    struct tracker *tk, int64_t index,
    struct browse_state *bst)
{
    struct filter_instance_array *array;
    struct tracker *xtk;
    struct dpath_node *item_dpath;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    array = (struct filter_instance_array *)
        tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    item_dpath = &array->item_type;
    xtk = tracker_dup(tk);
    if (index < tk->box->u.array.last_cached_index) {
        int64_t mark;

        mark = box_array_get_index_mark(xtk->box, index);
        tracker_goto_mark_internal(xtk, item_dpath, mark, bst);
    } else {
        tracker_goto_last_cached_item_internal(xtk, bst);
        if (tracker_is_dangling(xtk)) {
            bt_ret = tracker_goto_next_item_internal(xtk, bst);
            if (BITPUNCH_OK != bt_ret) {
                tracker_delete(xtk);
                return bt_ret;
            }
        }
    }
    bt_ret = BITPUNCH_OK;
    if (box_index_cache_exists(xtk->box)) {
        while (TRUE) {
            if (xtk->cur.u.array.index
                == xtk->box->u.array.last_cached_index + 1) {
                expr_value_t index_key;

                bt_ret = tracker_get_item_key_internal(xtk, &index_key, bst);
                if (BITPUNCH_OK != bt_ret) {
                    break ;
                }
                tracker_index_cache_add_item(xtk, index_key);
                expr_value_destroy(index_key);
            }
            if (xtk->cur.u.array.index == index) {
                break ;
            }
            bt_ret = tracker_goto_next_item_internal(xtk, bst);
            if (BITPUNCH_OK != bt_ret) {
                break ;
            }
        }
    } else {
        while (TRUE) {
            expr_value_t dummy;

            if (xtk->cur.u.array.index
                == xtk->box->u.array.last_cached_index + 1) {
                tracker_index_cache_add_item(xtk, dummy);
            }
            if (xtk->cur.u.array.index == index) {
                break ;
            }
            bt_ret = tracker_goto_next_item_internal(xtk, bst);
            if (BITPUNCH_OK != bt_ret) {
                break ;
            }
        }
    }
    if (BITPUNCH_OK == bt_ret) {
        tracker_set(tk, xtk);
        tracker_delete(xtk);
        DBG_TRACKER_CHECK_STATE(tk);
    } else {
        tracker_delete(xtk);
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item__array_non_slack_dynamic_item_size(
    struct tracker *tk, int64_t index,
    struct browse_state *bst)
{
    struct filter_instance_array *array;
    struct dpath_node *item_dpath;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    bt_ret = box_get_n_items_internal(tk->box, NULL, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != tk->box->u.array_generic.n_items);
    if (index >= tk->box->u.array_generic.n_items) {
        return BITPUNCH_NO_ITEM;
    }
    array = (struct filter_instance_array *)
        tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    item_dpath = &array->item_type;
    if (0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        // FIXME reset item to NULL if filter is dynamic
        tk->dpath.filter = item_dpath->filter;
        tk->flags &= ~TRACKER_AT_END;
        tk->cur.u.array.index = index;
        DBG_TRACKER_CHECK_STATE(tk);
        return BITPUNCH_OK;
    }

    return tracker_goto_nth_item__array_slack_dynamic_item_size(
        tk, index, bst);
}


static bitpunch_status_t
tracker_goto_named_item__array(struct tracker *tk, const char *name,
                               struct browse_state *bst)
{
    expr_value_t item_key;

    DBG_TRACKER_DUMP(tk);
    memset(&item_key, 0, sizeof(item_key));
    item_key.string.str = name;
    item_key.string.len = strlen(name);
    return tracker_goto_first_item_with_key_internal(tk, item_key, bst);
}


static bitpunch_status_t
tracker_goto_next_key_match__generic(struct tracker *tk,
                                     struct ast_node_hdl *key_expr,
                                     expr_value_t key,
                                     struct track_path search_boundary,
                                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    expr_value_t item_key;
    struct box *filtered_box;

    DBG_TRACKER_DUMP(tk);
    while (search_boundary.type == TRACK_PATH_NOTYPE
           || ! track_path_eq(tk->cur, search_boundary)) {
        bt_ret = tracker_get_filtered_item_box_internal(tk, &filtered_box,
                                                        bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = box_apply_filter_internal(filtered_box, bst);
        if (BITPUNCH_OK == bt_ret) {
            bt_ret = expr_evaluate_value_internal(key_expr, filtered_box,
                                                  &item_key, bst);
        }
        box_delete_non_null(filtered_box);
        if (BITPUNCH_OK != bt_ret) {
            tracker_error_add_tracker_context(
                tk, bst, "when evaluating item key expression");
            expr_value_destroy(item_key);
            return bt_ret;
        }
        if (tk->cur.u.array.index ==
            tk->box->u.array.last_cached_index + 1) {
            tracker_index_cache_add_item(tk, item_key);
        }
        if (0 == expr_value_cmp(item_key, key)) {
            expr_value_destroy(item_key);
            return BITPUNCH_OK;
        }
        expr_value_destroy(item_key);
        bt_ret = tracker_goto_next_item_internal(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    return BITPUNCH_NO_ITEM;
}

static bitpunch_status_t
tracker_goto_next_key_match__array(struct tracker *tk,
                                   expr_value_t index,
                                   struct track_path search_boundary,
                                   struct browse_state *bst)
{
    struct filter_instance_array *array;
    const struct ast_node_hdl *item_type;
    struct ast_node_hdl *key_expr;

    DBG_TRACKER_DUMP(tk);
    assert(AST_NODE_TYPE_ARRAY == tk->box->dpath.filter->ndat->type);
    array = (struct filter_instance_array *)
        tk->box->dpath.filter->ndat->u.rexpr_filter.f_instance;
    item_type = dpath_node_get_as_type(&array->item_type);
    if (AST_NODE_TYPE_COMPOSITE != item_type->ndat->type) {
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, item_type, bst,
                             "only arrays which items are structures can "
                             "be accessed through named index");
    }
    key_expr = ast_node_get_key_expr(tk->box->dpath.filter);
    if (NULL == key_expr) {
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, key_expr, bst,
                             "array is not indexed");
    }
    return tracker_goto_next_key_match__generic(tk, key_expr, index,
                                                search_boundary, bst);
}

static bitpunch_status_t
tracker_goto_next_item_with_key__default(struct tracker *tk,
                                         expr_value_t item_key,
                                         struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return BITPUNCH_NO_ITEM;
}

static bitpunch_status_t
tracker_goto_next_item_with_key__composite(struct tracker *tk,
                                           expr_value_t item_key,
                                           struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return BITPUNCH_NOT_IMPLEMENTED;
}

static bitpunch_status_t
tracker_goto_next_item_with_key__indexed_array_internal(
    struct tracker *tk,
    expr_value_t item_key,
    int64_t end_index,
    struct browse_state *bst)
{
    struct index_cache_iterator twin_iter;
    bitpunch_status_t bt_ret;
    struct track_path in_slice_path;
    struct track_path item_path;
    struct tracker *xtk;
    const struct ast_node_hdl *node;
 
    DBG_TRACKER_DUMP(tk);
    assert(box_index_cache_exists(tk->box));

    if (tk->cur.u.array.index < tk->box->u.array.last_cached_index) {
        in_slice_path = track_path_from_array_slice(
            tk->cur.u.array.index + 1, end_index);
        box_index_cache_lookup_key_twins(tk->box, item_key, in_slice_path,
                                         &twin_iter, bst);
        bt_ret = index_cache_iterator_next_twin(&twin_iter, &item_path, bst);
        if (BITPUNCH_OK == bt_ret) {
            tracker_set(tk, twin_iter.xtk);
        }
        if (BITPUNCH_NO_ITEM != bt_ret) {
            index_cache_iterator_done(&twin_iter);
            return bt_ret;
        }
        index_cache_iterator_done(&twin_iter);
    }
    /* Browse the yet-uncached tail */
    node = tk->box->dpath.filter;
    xtk = tracker_dup(tk);
    tracker_goto_last_cached_item_internal(xtk, bst);
    if (-1 != end_index && xtk->cur.u.array.index >= end_index) {
        bt_ret = BITPUNCH_NO_ITEM;
    } else {
        bt_ret = tracker_goto_next_item_internal(xtk, bst);
        if (BITPUNCH_OK == bt_ret) {
            struct track_path end_path;

            if (-1 != end_index) {
                end_path = track_path_from_array_index(end_index);
            } else {
                end_path = TRACK_PATH_NONE;
            }
            bt_ret = node->ndat->u.rexpr_filter.f_instance->b_tk.goto_next_key_match(
                xtk, item_key, end_path, bst);
        }
        if (BITPUNCH_OK == bt_ret) {
            tracker_set(tk, xtk);
        }
    }
    tracker_delete(xtk);
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_next_item_with_key__indexed_array(struct tracker *tk,
                                               expr_value_t item_key,
                                               struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_goto_next_item_with_key__indexed_array_internal(
        tk, item_key, -1 /* no end boundary */, bst);
}

static bitpunch_status_t
tracker_goto_nth_item_with_key__default(struct tracker *tk,
                                        expr_value_t item_key,
                                        int nth_twin,
                                        struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return BITPUNCH_NO_ITEM;
}

static bitpunch_status_t
tracker_goto_nth_item_with_key__indexed_array_internal(
    struct tracker *tk,
    expr_value_t item_key,
    int nth_twin,
    int64_t from_index,
    int64_t end_index,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int cur_twin;
    struct tracker *xtk;
    const struct ast_node_hdl *node;

    DBG_TRACKER_DUMP(tk);
    //TODO use from_index and end_index
    assert(box_index_cache_exists(tk->box));

    bt_ret = tracker_index_cache_goto_twin(
        tk, item_key, nth_twin,
        track_path_from_array_slice(from_index, end_index),
        &cur_twin, bst);
    if (BITPUNCH_NO_ITEM != bt_ret) {
        return bt_ret; /* BITPUNCH_OK included */
    }
    /* Browse the yet-uncached tail */
    node = tk->box->dpath.filter;
    xtk = tracker_dup(tk);
    tracker_goto_last_cached_item_internal(xtk, bst);
    if (-1 != end_index && xtk->cur.u.array.index >= end_index) {
        bt_ret = BITPUNCH_NO_ITEM;
    } else {
        struct track_path end_path;

        if (-1 != end_index) {
            end_path = track_path_from_array_index(end_index);
        } else {
            end_path = TRACK_PATH_NONE;
        }
        do {
            bt_ret = tracker_goto_next_item_internal(xtk, bst);
            if (BITPUNCH_OK == bt_ret) {
                bt_ret = node->ndat->u.rexpr_filter.f_instance->b_tk.goto_next_key_match(
                    xtk, item_key, end_path, bst);
            }
            if (xtk->cur.u.array.index >= from_index) {
                ++cur_twin;
            }
        } while (BITPUNCH_OK == bt_ret && cur_twin != nth_twin);
    }

    if (BITPUNCH_OK == bt_ret) {
        tracker_set(tk, xtk);
    }
    tracker_delete(xtk);
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item_with_key__indexed_array(
    struct tracker *tk, expr_value_t item_key, int nth_twin,
    struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_goto_nth_item_with_key__indexed_array_internal(
        tk, item_key, nth_twin,
        0 /* twin count starts at first item */,
        -1 /* no end boundary */,
        bst);
}

static bitpunch_status_t
tracker_compute_item_size__byte_array_dynamic_size(
    struct tracker *tk,
    int64_t *item_sizep,
    struct browse_state *bst)
{
    struct ast_node_hdl *item;
    struct filter_instance_array *array;
    expr_value_t byte_count;
    bitpunch_status_t bt_ret;

    bt_ret = tracker_compute_item_filter_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    item = tk->dpath.item;
    array = (struct filter_instance_array *)
        item->ndat->u.rexpr_filter.f_instance;
    bt_ret = expr_evaluate_value_internal(
        array->item_count, tk->box, &byte_count, bst);
    if (BITPUNCH_OK != bt_ret) {
        // FIXME more appropriate context
        tracker_error_add_box_context(
            tk->box, bst, "when evaluating byte array size expression");
        return bt_ret;
    }
    if (EXPR_VALUE_TYPE_INTEGER != byte_count.type) {
        return box_error(
            BITPUNCH_DATA_ERROR, tk->box, array->item_count, bst,
            "evaluation of byte array size returned a value-type "
            "'%s', expect an integer",
            expr_value_type_str(byte_count.type));
    }
    if (byte_count.integer < 0) {
        return box_error(
            BITPUNCH_DATA_ERROR, tk->box, array->item_count, bst,
            "evaluation of byte array size gives negative value (%"PRIi64")",
            byte_count.integer);
    }
    *item_sizep = byte_count.integer;
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_compute_item_size__byte_array_slack(
    struct tracker *tk,
    int64_t *item_sizep,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct ast_node_hdl *filter_defining_span_size;
    int64_t max_slack_offset;

    bt_ret = expr_evaluate_filter_type_internal(
        tk->dpath.filter, tk->box, FILTER_KIND_DEFINING_SPAN_SIZE,
        &filter_defining_span_size, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (NULL != filter_defining_span_size) {
        return tracker_compute_item_size__filter_span_size(
            tk, filter_defining_span_size, item_sizep, bst);
    }
    DBG_TRACKER_DUMP(tk);
    if ((tk->box->flags & COMPUTING_MAX_SLACK_OFFSET)) {
        bt_ret = box_compute_max_span_size(tk->box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        max_slack_offset = 0 != (tk->box->flags & BOX_RALIGN) ?
            tk->box->start_offset_max_span : tk->box->end_offset_max_span;
    } else {
        bt_ret = box_get_children_slack(
            tk->box, 0 != (tk->flags & TRACKER_REVERSED),
            &max_slack_offset, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        /* slack byte arrays use the whole available slack space */
        *item_sizep = 0 != (tk->flags & TRACKER_REVERSED) ?
            tk->item_offset - max_slack_offset :
            max_slack_offset - tk->item_offset;
        assert(*item_sizep >= 0);
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_used_size__byte_array_dynamic_size(struct box *box,
                                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t used_size;

    DBG_BOX_DUMP(box);
    if (NULL != box->dpath.filter_defining_used_size) {
        bt_ret = box_compute_item_used_size_internal__byte_array_filter_size(
            box->parent_box, box_get_start_offset(box),
            box->dpath.filter_defining_used_size, &used_size, bst);
    } else {
        bt_ret = box_get_n_items_internal(box, &used_size, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_used_size(box, used_size, bst);
}

static bitpunch_status_t
box_get_n_items__byte_array_non_slack(
    struct box *box, int64_t *item_countp,
    struct browse_state *bst)
{
    struct filter_instance_array *array;
    expr_value_t size;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        bitpunch_status_t bt_ret;
        struct box *scope;

        array = (struct filter_instance_array *)
            box->dpath.filter->ndat->u.rexpr_filter.f_instance;
        scope = box_get_scope_box(box->parent_box);
        assert(NULL != scope);
        bt_ret = expr_evaluate_value_internal(array->item_count,
                                              scope, &size, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        if (EXPR_VALUE_TYPE_INTEGER != size.type) {
            return box_error(
                BITPUNCH_DATA_ERROR, box, array->item_count, bst,
                "evaluation of byte array size returned a value-type "
                "'%s', expect an integer",
                expr_value_type_str(size.type));
        }
        if (size.integer < 0) {
            return box_error(BITPUNCH_DATA_ERROR, box, array->item_count, bst,
                             "evaluation of byte array size gives "
                             "negative value (%"PRIi64")", size.integer);
        }
        bt_ret = box_set_max_span_size(box, size.integer, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        box->u.array_generic.n_items = size.integer;
    }
    if (NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_get_n_items__byte_array_slack(struct box *box, int64_t *item_countp,
                                  struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        bt_ret = box_compute_slack_size(box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        assert(-1 != box->end_offset_slack);
        /* deduce size from the available slack space */
        box->u.array_generic.n_items =
            box->end_offset_slack - box->start_offset_used;
        /* now is a good time to set the used size as well */
        bt_ret = box_set_used_size(box, box->u.array_generic.n_items,
                                   bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    if (NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_get_n_items__byte(
    struct box *box, int64_t *item_countp,
    struct browse_state *bst)
{
    *item_countp = 1;
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_first_item__byte_array_generic(struct tracker *tk,
                                            struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_goto_first_item__array_generic(tk, &tk->box->dpath, bst);
}

static bitpunch_status_t
tracker_goto_first_item__byte_array_slack(struct tracker *tk,
                                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    if (0 != (tk->flags & TRACKER_REVERSED) ||
        0 != (tk->box->flags & BOX_RALIGN)) {
        return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst,
                             "tracker_goto_first_item() not implemented "
                             "in reverse mode on slack byte arrays");
    }
    bt_ret = box_compute_slack_size(tk->box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != tk->box->end_offset_slack);
    /* check if there's size for at least one element */
    if (tk->box->end_offset_slack == tk->box->start_offset_used) {
        return BITPUNCH_NO_ITEM;
    }
    /* slack arrays require a maintained item offset to browse their
     * elements */
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    tk->item_offset = tk->box->start_offset_used;
    tk->item_size = 1;
    tk->cur = track_path_from_array_index(0);
    tk->dpath.filter = AST_NODE_BYTE;
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_next_item__byte_array_generic(struct tracker *tk,
                                           struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    ++tk->cur.u.array.index;
    if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        tk->item_offset += 1;
    }
    if ((-1 != tk->box->u.array_generic.n_items
         && tk->cur.u.array.index == tk->box->u.array_generic.n_items)
        || (-1 == tk->box->u.array_generic.n_items
            && tk->item_offset == tk->box->end_offset_slack)) {
        bt_ret = tracker_set_end(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        return BITPUNCH_NO_ITEM;
    }
    /* check new item */
    tk->dpath.filter = AST_NODE_BYTE;
    bt_ret = tracker_check_item(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        /* rollback */
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
            tk->item_offset -= 1;
        }
        --tk->cur.u.array.index;
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item__byte_array_generic(
    struct tracker *tk, int64_t index,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t n_items;

    DBG_TRACKER_DUMP(tk);
    if (0 != (tk->flags & TRACKER_REVERSED) ||
        0 != (tk->box->flags & BOX_RALIGN)) {
        return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst,
                             "tracker_goto_nth_item() not implemented "
                             "in reverse mode on byte arrays");
    }
    bt_ret = box_get_n_items_internal(tk->box, &n_items, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != n_items);
    if (index >= n_items) {
        return BITPUNCH_NO_ITEM;
    }
    tk->flags &= ~TRACKER_AT_END;
    tk->dpath.filter = AST_NODE_BYTE;
    if (0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        tk->cur = track_path_from_array_index(index);
        DBG_TRACKER_CHECK_STATE(tk);
        return BITPUNCH_OK;
    }
    tk->item_offset = tk->box->start_offset_used + index;
    tk->item_size = 1;
    tk->cur = track_path_from_array_index(index);
    return tracker_check_item(tk, bst);
}


static struct box *
box_array_slice_get_ancestor_array(struct box *box)
{
    struct box *array_box;

    DBG_BOX_DUMP(box);
    array_box = box;
    while (AST_NODE_TYPE_ARRAY != array_box->dpath.filter->ndat->type &&
           AST_NODE_TYPE_BYTE_ARRAY != array_box->dpath.filter->ndat->type &&
           AST_NODE_TYPE_AS_BYTES != array_box->dpath.filter->ndat->type) {
        array_box = array_box->parent_box;
        /* An array slice box shall always have a real array ancestor. */
        assert(NULL != array_box);
    }
    return array_box;
}

static bitpunch_status_t
box_get_n_items__slice_generic(struct box *box, int64_t *item_countp,
                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct track_path end_path;
    int64_t index_start;
    int64_t index_end;
    int64_t item_count;

    DBG_BOX_DUMP(box);
    assert(box->track_path.type == TRACK_PATH_ARRAY_SLICE);
    if (-1 == box->u.array_generic.n_items) {
        bt_ret = box_get_end_path(box->parent_box, &end_path, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        index_start = box->track_path.u.array.index;
        index_end = box->track_path.u.array_slice.index_end;
        if (-1 == index_end) {
            index_end = end_path.u.array.index;
        } else if (index_end > end_path.u.array.index) {
            index_end = end_path.u.array.index;
        }
        if (-1 == index_start) {
            index_start = 0;
        } else if (index_start > index_end) {
            index_start = index_end;
        }
        item_count = index_end - index_start;
        box->u.array_generic.n_items = item_count;
    }
    if (NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_used_size__array_slice(struct box *box,
                                   struct browse_state *bst)
{
    struct filter_instance_array *array;
    struct box *array_box;
    const struct ast_node_hdl *array_node;
    struct ast_node_hdl *item_type;
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    array_box = box_array_slice_get_ancestor_array(box);
    array_node = array_box->dpath.filter;
    array = (struct filter_instance_array *)
        array_node->ndat->u.rexpr_filter.f_instance;
    item_type = ast_node_get_target_item(array->item_type.item);
    if (0 == (array->item_type.item->ndat->u.item.flags
              & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        int64_t n_elems;
        int64_t item_size;

        bt_ret = box_get_n_items__slice_generic(box, &n_elems, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        item_size = ast_node_get_min_span_size(item_type);
        bt_ret = box_set_used_size(box, n_elems * item_size, bst);
    } else {
        /* Default implementation of dynamic sized arrays works by
         * tracking contents, which has a custom implementation for
         * array slices. */
        bt_ret = array_node->ndat->u.rexpr_filter.f_instance->b_box.compute_used_size(box, bst);
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_get_item_key__array_slice(struct tracker *tk,
                                  expr_value_t *keyp,
                                  int *nth_twinp,
                                  struct browse_state *bst)
{
    struct box *slice_box;
    struct box *array_box;
    bitpunch_status_t bt_ret;
    int64_t from_index;

    DBG_TRACKER_DUMP(tk);
    slice_box = tk->box;
    array_box = box_array_slice_get_ancestor_array(slice_box);
    tk->box = array_box;
    from_index = slice_box->track_path.u.array.index;
    if (-1 == from_index) {
        from_index = 0;
    }
    if (ast_node_is_indexed(array_box->dpath.filter)) {
        bt_ret = tracker_get_item_key__indexed_array_internal(
            tk, from_index, keyp, nth_twinp, bst);
    } else {
        if (NULL != keyp) {
            assert(tk->cur.u.array.index >= from_index);
            *keyp = expr_value_as_integer(tk->cur.u.array.index - from_index);
        }
        if (NULL != nth_twinp) {
            /* only relevant for indexed arrays */
            *nth_twinp = 0;
        }
        bt_ret = BITPUNCH_OK;
    }
    tk->box = slice_box;
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_first_item__array_slice(struct tracker *tk,
                                     struct browse_state *bst)
{
    struct box *slice_box;
    struct box *array_box;
    bitpunch_status_t bt_ret;
    int index;

    DBG_TRACKER_DUMP(tk);
    slice_box = tk->box;
    if (-1 != slice_box->track_path.u.array.index
        && (slice_box->track_path.u.array.index
            == slice_box->track_path.u.array_slice.index_end)) {
        tk->cur = slice_box->track_path;
        tk->item_offset = slice_box->start_offset_used;
        bt_ret = tracker_set_end(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        return BITPUNCH_NO_ITEM;
    }
    array_box = box_array_slice_get_ancestor_array(slice_box);
    tk->box = array_box;
    index = slice_box->track_path.u.array.index;
    if (-1 == index) {
        index = 0;
    }
    bt_ret = array_box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_tk.goto_nth_item(tk, index, bst);
    tk->box = slice_box;
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_next_item__array_slice(struct tracker *tk,
                                    struct browse_state *bst)
{
    struct box *slice_box;
    struct box *array_box;
    bitpunch_status_t bt_ret;
    int64_t index_end;

    DBG_TRACKER_DUMP(tk);
    if (0 != (tk->flags & TRACKER_REVERSED) ||
        0 != (tk->box->flags & BOX_RALIGN)) {
        return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst,
                             "tracker_goto_next_item() not implemented "
                             "in reverse mode on array slices");
    }
    slice_box = tk->box;
    index_end = slice_box->track_path.u.array_slice.index_end;
    if (tk->cur.u.array.index + 1 == index_end) {
        bt_ret = tracker_compute_item_location(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        ++tk->cur.u.array.index;
        tk->item_offset += tk->item_size;
        bt_ret = tracker_set_end(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        return BITPUNCH_NO_ITEM;
    }
    array_box = box_array_slice_get_ancestor_array(slice_box);
    tk->box = array_box;
    bt_ret = array_box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_tk.goto_next_item(tk, bst);
    tk->box = slice_box;
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item__array_slice(struct tracker *tk, int64_t index,
                                   struct browse_state *bst)
{
    struct box *slice_box;
    struct box *array_box;
    bitpunch_status_t bt_ret;
    int64_t index_start;
    int64_t index_end;

    DBG_TRACKER_DUMP(tk);
    slice_box = tk->box;
    index_start = slice_box->track_path.u.array.index;
    if (-1 == index_start) {
        index_start = 0;
    }
    index_end = slice_box->track_path.u.array_slice.index_end;
    if ((-1 != index_end && index >= index_end - index_start)) {
        return BITPUNCH_NO_ITEM;
    }
    array_box = box_array_slice_get_ancestor_array(slice_box);
    tk->box = array_box;
    bt_ret = array_box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_tk.goto_nth_item(
        tk, index_start + index, bst);
    tk->box = slice_box;
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_named_item__array_slice(struct tracker *tk, const char *name,
                                     struct browse_state *bst)
{
    expr_value_t item_key;

    DBG_TRACKER_DUMP(tk);
    memset(&item_key, 0, sizeof(item_key));
    item_key.string.str = name;
    item_key.string.len = strlen(name);
    return tracker_goto_first_item_with_key_internal(tk, item_key, bst);
}

static bitpunch_status_t
tracker_goto_next_key_match__array_slice(struct tracker *tk,
                                         expr_value_t index,
                                         struct track_path search_boundary,
                                         struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    // should not be called, only used internally by indexed arrays
    return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst, NULL);
}

static bitpunch_status_t
tracker_goto_next_item_with_key__array_slice(struct tracker *tk,
                                             expr_value_t item_key,
                                             struct browse_state *bst)
{
    struct box *slice_box;
    struct box *array_box;
    bitpunch_status_t bt_ret;
    int64_t end_index;

    DBG_TRACKER_DUMP(tk);
    slice_box = tk->box;
    array_box = box_array_slice_get_ancestor_array(slice_box);
    end_index = slice_box->track_path.u.array_slice.index_end;

    tk->box = array_box;
    if (ast_node_is_indexed(array_box->dpath.filter)) {
        bt_ret = tracker_goto_next_item_with_key__indexed_array_internal(
            tk, item_key, end_index, bst);
    } else {
        bt_ret = array_box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_tk.goto_next_item_with_key(
            tk, item_key, bst);
        if (BITPUNCH_OK == bt_ret && tk->cur.u.array.index == end_index) {
            bt_ret = tracker_set_end(tk, bst);
            if (BITPUNCH_OK == bt_ret) {
                bt_ret = BITPUNCH_NO_ITEM;
            }
        }
    }
    tk->box = slice_box;
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item_with_key__array_slice(
    struct tracker *tk, expr_value_t item_key, int nth_twin,
    struct browse_state *bst)
{
    struct box *slice_box;
    struct box *array_box;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    slice_box = tk->box;
    array_box = box_array_slice_get_ancestor_array(slice_box);

    tk->box = array_box;
    if (ast_node_is_indexed(array_box->dpath.filter)) {
        int64_t from_index;
        int64_t end_index;

        from_index = slice_box->track_path.u.array.index;
        if (-1 == from_index) {
            from_index = 0;
        }
        end_index = slice_box->track_path.u.array_slice.index_end;
        bt_ret = tracker_goto_nth_item_with_key__indexed_array_internal(
            tk, item_key, nth_twin, from_index, end_index, bst);
    } else {
        /* this call shall return an error since the function we're in
         * expects the underlying array to be indexed. */
        bt_ret = array_box->dpath.filter->ndat->u.rexpr_filter.f_instance->b_tk.goto_nth_item_with_key(
            tk, item_key, nth_twin, bst);
    }
    tk->box = slice_box;
    return bt_ret;
}



static bitpunch_status_t
box_compute_used_size__byte_slice(struct box *box,
                                  struct browse_state *bst)
{
    int64_t n_bytes;
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    bt_ret = box_get_n_items__slice_generic(box, &n_bytes, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_used_size(box, n_bytes, bst);
}

static bitpunch_status_t
tracker_get_item_key__byte_slice(struct tracker *tk,
                                 expr_value_t *keyp,
                                 int *nth_twinp,
                                 struct browse_state *bst)
{
    struct box *slice_box;
    struct box *array_box;
    int64_t from_index;

    DBG_TRACKER_DUMP(tk);
    slice_box = tk->box;
    array_box = box_array_slice_get_ancestor_array(slice_box);
    tk->box = array_box;
    from_index = slice_box->track_path.u.array.index;
    if (-1 == from_index) {
        from_index = 0;
    }
    if (NULL != keyp) {
        assert(tk->cur.u.array.index >= from_index);
        keyp->type = EXPR_VALUE_TYPE_INTEGER;
        keyp->integer = tk->cur.u.array.index - from_index;
    }
    if (NULL != nth_twinp) {
        /* only relevant for indexed arrays */
        *nth_twinp = 0;
    }
    tk->box = slice_box;
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_next_item_with_key__not_impl(struct tracker *tk,
                                          expr_value_t item_key,
                                          struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst, NULL);
}

static bitpunch_status_t
tracker_goto_nth_item_with_key__not_impl(
    struct tracker *tk, expr_value_t item_key, int nth_twin,
    struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst, NULL);
}


static bitpunch_status_t
box_get_n_items__as_bytes(struct box *box, int64_t *item_countp,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        bt_ret = box_get_used_size(box, &box->u.array_generic.n_items, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    if (NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return BITPUNCH_OK;
}


/*
 * setup backends
 */

static int
browse_setup_backends_node_recur(struct ast_node_hdl *node);
static void
browse_setup_backends__box__as_bytes(struct ast_node_hdl *item);
static void
browse_setup_backends__tracker__as_bytes(struct ast_node_hdl *item);

static void
browse_setup_backends__filter__filter(struct ast_node_hdl *filter)
{
    struct item_backend *b_item = NULL;

    b_item = &filter->ndat->u.rexpr_filter.f_instance->b_item;
    memset(b_item, 0, sizeof (*b_item));

    b_item->read_value = filter_read_value__filter;
}

static void
browse_setup_backends__filter(struct ast_node_hdl *item)
{
    browse_setup_backends__filter__filter(item);
    browse_setup_backends__box__as_bytes(item);
    browse_setup_backends__tracker__as_bytes(item);
}

static struct filter_instance *
operator_filter_filter_instance_build(struct ast_node_hdl *item)
{
    struct filter_instance *filter;
    struct item_backend *b_item;

    filter = new_safe(struct filter_instance);
    b_item = &filter->b_item;
    memset(b_item, 0, sizeof (*b_item));

    b_item->read_value = filter_read_value__operator_filter;
    return filter;
}

static void
browse_setup_backends__operator_filter(struct ast_node_hdl *item)
{
    item->ndat->u.rexpr_filter.f_instance =
        operator_filter_filter_instance_build(item);
}

static struct filter_instance *
item_filter_instance_build(struct ast_node_hdl *item)
{
    struct filter_instance *filter;
    struct item_backend *b_item;

    filter = new_safe(struct filter_instance);
    b_item = &filter->b_item;
    memset(b_item, 0, sizeof (*b_item));

    b_item->read_value = filter_read_value__bytes;
    return filter;
}

static void
browse_setup_backends__item(struct ast_node_hdl *item)
{
    item->ndat->u.rexpr_filter.f_instance = item_filter_instance_build(item);
}


static void
browse_setup_backends__box__composite(struct ast_node_hdl *item)
{
    struct box_backend *b_box = NULL;
    struct filter_instance_composite *composite;

    b_box = &item->ndat->u.rexpr_filter.f_instance->b_box;
    memset(b_box, 0, sizeof (*b_box));
    composite = (struct filter_instance_composite *)
        item->ndat->u.rexpr_filter.f_instance;

    if (0 != (item->flags & ASTFLAG_IS_ROOT_BLOCK)) {
        b_box->compute_slack_size = box_compute_slack_size__from_file_hdl;
    } else if (0 != (item->ndat->u.item.flags & ITEMFLAG_USES_SLACK)) {
        b_box->compute_slack_size = box_compute_slack_size__container_slack;
    } else {
        b_box->compute_slack_size = box_compute_slack_size__from_parent;
    }
    if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_box->compute_used_size = box_compute_used_size__static_size;
    } else if (0 != (item->ndat->u.item.flags & ITEMFLAG_FILLS_SLACK)) {
        b_box->compute_used_size = box_compute_used_size__as_max_span;
    } else if (COMPOSITE_TYPE_STRUCT == composite->type) {
        b_box->compute_used_size = box_compute_used_size__packed_dynamic_size;
    } else /* union */ {
        b_box->compute_used_size = box_compute_used_size__union_dynamic_size;
    }
    if (NULL != block_get_first_attribute(item, "@span") ||
        NULL != block_get_first_attribute(item, "@minspan") ||
        NULL != block_get_first_attribute(item, "@maxspan")) {
        b_box->compute_min_span_size = box_compute_min_span_size__span_expr;
        b_box->compute_max_span_size = box_compute_max_span_size__span_expr;
    } else {
        b_box->compute_min_span_size =
            box_compute_min_span_size__as_hard_min;
        if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            b_box->compute_max_span_size =
                box_compute_max_span_size__as_used;
        } else {
            b_box->compute_max_span_size =
                box_compute_max_span_size__as_slack;
        }
    }
    if (COMPOSITE_TYPE_STRUCT == composite->type) {
        b_box->get_max_slack_offset = box_get_children_slack__struct;
    }
    b_box->get_n_items = box_get_n_items__composite;
}

static void
browse_setup_backends__tracker__composite(struct ast_node_hdl *item)
{
    struct tracker_backend *b_tk = NULL;

    b_tk = &item->ndat->u.rexpr_filter.f_instance->b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

    b_tk->get_item_key = tracker_get_item_key__composite;
    if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_tk->compute_item_size = tracker_compute_item_size__static_size;
    } else {
        b_tk->compute_item_size = tracker_compute_item_size__item_box;
    }
    b_tk->goto_first_item = tracker_goto_first_item__composite;
    b_tk->goto_next_item = tracker_goto_next_item__composite;
    b_tk->goto_nth_item = tracker_goto_nth_item__composite;
    b_tk->goto_named_item = tracker_goto_named_item__composite;
    b_tk->goto_next_key_match = tracker_goto_next_key_match__composite;
    b_tk->goto_next_item_with_key =
        tracker_goto_next_item_with_key__composite;
    b_tk->goto_nth_item_with_key =
        tracker_goto_nth_item_with_key__composite;
}

static void
browse_setup_backends__box__array(struct ast_node_hdl *item)
{
    struct filter_instance_array *array;
    struct box_backend *b_box = NULL;
    const struct ast_node_hdl *item_type;

    array = (struct filter_instance_array *)
        item->ndat->u.rexpr_filter.f_instance;
    b_box = &array->filter.b_box;
    memset(b_box, 0, sizeof (*b_box));
    item_type = array->item_type.item;

    if (0 != (item->ndat->u.item.flags & ITEMFLAG_USES_SLACK)) {
        b_box->compute_slack_size =
            box_compute_slack_size__container_slack;
    } else {
        b_box->compute_slack_size = box_compute_slack_size__from_parent;
    }
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->compute_max_span_size = box_compute_max_span_size__as_slack;
    if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_box->compute_used_size = box_compute_used_size__static_size;
    } else if (0 == (item_type->ndat->u.item.flags
                     & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_box->compute_used_size =
            box_compute_used_size__array_static_item_size;
    } else {
        b_box->compute_used_size =
            box_compute_used_size__packed_dynamic_size;
    }
    if (0 != (item_type->flags & ASTFLAG_CONTAINS_LAST_ATTR)) {
        if (NULL != array->item_count) {
            b_box->get_n_items = box_get_n_items__array_non_slack_with_last;
        } else {
            b_box->get_n_items = box_get_n_items__by_iteration;
        }
    } else if (NULL != array->item_count) {
        b_box->get_n_items = box_get_n_items__array_non_slack;
    } else if (0 == (item_type->ndat->u.item.flags
                     & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_box->get_n_items =
            box_get_n_items__array_slack_static_item_size;
    } else {
        b_box->get_n_items = box_get_n_items__by_iteration;
    }
}

static void
browse_setup_backends__tracker__array(struct ast_node_hdl *item)
{
    struct filter_instance_array *array;
    struct tracker_backend *b_tk = NULL;
    const struct ast_node_hdl *item_type;

    array = (struct filter_instance_array *)
        item->ndat->u.rexpr_filter.f_instance;
    b_tk = &array->filter.b_tk;
    memset(b_tk, 0, sizeof (*b_tk));
    item_type = array->item_type.item;

    if (NULL != ast_node_get_key_expr(item)) {
        b_tk->get_item_key = tracker_get_item_key__indexed_array;
    } else {
        b_tk->get_item_key = tracker_get_item_key__array_generic;
    }
    if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_tk->compute_item_size = tracker_compute_item_size__static_size;
    } else if (NULL != array->item_count
               && 0 == (item_type->ndat->u.item.flags
                        & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_tk->compute_item_size =
            tracker_compute_item_size__array_static_item_size;
    } else {
        b_tk->compute_item_size = tracker_compute_item_size__item_box;
    }
    if (NULL != array->item_count) {
        b_tk->goto_first_item = tracker_goto_first_item__array_non_slack;
    } else {
        b_tk->goto_first_item = tracker_goto_first_item__array_slack;
    }
    b_tk->goto_next_item = tracker_goto_next_item__array;
    if (0 == (item_type->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_tk->goto_nth_item = tracker_goto_nth_item__array_static_item_size;
    } else if (NULL != array->item_count) {
        b_tk->goto_nth_item =
            tracker_goto_nth_item__array_non_slack_dynamic_item_size;
    } else {
        b_tk->goto_nth_item =
            tracker_goto_nth_item__array_slack_dynamic_item_size;
    }
    b_tk->goto_named_item = tracker_goto_named_item__array;
    b_tk->goto_next_key_match = tracker_goto_next_key_match__array;
    if (ast_node_is_indexed(item)) {
        b_tk->goto_next_item_with_key =
            tracker_goto_next_item_with_key__indexed_array;
        b_tk->goto_nth_item_with_key =
            tracker_goto_nth_item_with_key__indexed_array;
    } else {
        b_tk->goto_next_item_with_key =
            tracker_goto_next_item_with_key__default;
        b_tk->goto_nth_item_with_key =
            tracker_goto_nth_item_with_key__default;
    }
}

static void
browse_setup_backends__tracker__byte(struct ast_node_hdl *item)
{
    struct tracker_backend *b_tk = NULL;

    b_tk = &item->ndat->u.rexpr_filter.f_instance->b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

    b_tk->compute_item_size = tracker_compute_item_size__byte;
}


static void
browse_setup_backends__box__byte_array(struct ast_node_hdl *item)
{
    struct filter_instance_array *array;
    struct box_backend *b_box = NULL;

    array = (struct filter_instance_array *)
        item->ndat->u.rexpr_filter.f_instance;
    b_box = &array->filter.b_box;
    memset(b_box, 0, sizeof (*b_box));

    if (0 != (item->ndat->u.item.flags & ITEMFLAG_USES_SLACK)) {
        b_box->compute_slack_size =
            box_compute_slack_size__container_slack;
    } else {
        b_box->compute_slack_size = box_compute_slack_size__from_parent;
    }
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    if (0 == (item->ndat->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_box->compute_used_size = box_compute_used_size__static_size;
        b_box->compute_max_span_size = box_compute_max_span_size__as_used;
    } else {
        if (0 != (item->ndat->u.item.flags & ITEMFLAG_FILLS_SLACK)) {
            b_box->compute_used_size = box_compute_used_size__as_max_span;
        } else {
            b_box->compute_used_size =
                box_compute_used_size__byte_array_dynamic_size;
        }
        b_box->compute_max_span_size = box_compute_max_span_size__as_slack;
    }
    if (NULL != array->item_count) {
        b_box->get_n_items = box_get_n_items__byte_array_non_slack;
    } else {
        b_box->get_n_items = box_get_n_items__byte_array_slack;
    }
}

static void
browse_setup_backends__box__byte(struct ast_node_hdl *item)
{
    struct box_backend *b_box = NULL;

    b_box = &item->ndat->u.rexpr_filter.f_instance->b_box;
    memset(b_box, 0, sizeof (*b_box));

    b_box->compute_slack_size = box_compute_slack_size__from_parent;
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->compute_used_size = box_compute_used_size__static_size;
    b_box->compute_max_span_size = box_compute_max_span_size__as_used;
    b_box->get_n_items = box_get_n_items__byte;
}


static void
browse_setup_backends__byte(struct ast_node_hdl *item)
{
    item->ndat->u.rexpr_filter.f_instance = new_safe(struct filter_instance);

    browse_setup_backends__filter__filter(item);
    browse_setup_backends__box__byte(item);
    browse_setup_backends__tracker__byte(item);
}

static void
browse_setup_backends__tracker__byte_array(struct ast_node_hdl *item)
{
    struct filter_instance_array *array;
    struct tracker_backend *b_tk = NULL;

    array = (struct filter_instance_array *)
        item->ndat->u.rexpr_filter.f_instance;
    b_tk = &array->filter.b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

    b_tk->get_item_key = tracker_get_item_key__array_generic;
    if (NULL != array->item_count) {
        b_tk->compute_item_size =
            tracker_compute_item_size__byte_array_dynamic_size;
    } else {
        b_tk->compute_item_size =
            tracker_compute_item_size__byte_array_slack;
    }
    if (NULL != array->item_count) {
        b_tk->goto_first_item =
            tracker_goto_first_item__array_non_slack;
    } else {
        b_tk->goto_first_item =
            tracker_goto_first_item__byte_array_slack;
    }
    b_tk->goto_next_item = tracker_goto_next_item__byte_array_generic;
    b_tk->goto_nth_item = tracker_goto_nth_item__byte_array_generic;
    b_tk->goto_next_item_with_key =
        tracker_goto_next_item_with_key__default;
    b_tk->goto_nth_item_with_key =
        tracker_goto_nth_item_with_key__default;
}

static struct filter_instance *
array_slice_filter_instance_build(struct ast_node_hdl *item)
{
    struct filter_instance *slice;
    struct box_backend *b_box;
    struct tracker_backend *b_tk;

    slice = new_safe(struct filter_instance);
    b_box = &slice->b_box;
    b_tk = &slice->b_tk;
    memset(b_box, 0, sizeof (*b_box));
    memset(b_tk, 0, sizeof (*b_tk));

    b_box->compute_slack_size = box_compute_slack_size__from_parent;
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->compute_max_span_size = box_compute_max_span_size__as_used;
    b_box->compute_used_size = box_compute_used_size__array_slice;
    b_box->get_n_items = box_get_n_items__slice_generic;

    b_tk->get_item_key = tracker_get_item_key__array_slice;
    b_tk->compute_item_size = tracker_compute_item_size__item_box;
    b_tk->goto_first_item = tracker_goto_first_item__array_slice;
    b_tk->goto_next_item = tracker_goto_next_item__array_slice;
    b_tk->goto_nth_item = tracker_goto_nth_item__array_slice;
    b_tk->goto_named_item = tracker_goto_named_item__array_slice;
    b_tk->goto_next_key_match = tracker_goto_next_key_match__array_slice;
    b_tk->goto_next_item_with_key =
        tracker_goto_next_item_with_key__array_slice;
    b_tk->goto_nth_item_with_key =
        tracker_goto_nth_item_with_key__array_slice;

    return slice;
}

static void
browse_setup_backends__array_slice(struct ast_node_hdl *item)
{
    item->ndat->u.rexpr_filter.f_instance =
        array_slice_filter_instance_build(item);

    browse_setup_backends__filter__filter(item);
}

static struct filter_instance *
byte_slice_filter_instance_build(struct ast_node_hdl *item)
{
    struct filter_instance *slice;
    struct box_backend *b_box;
    struct tracker_backend *b_tk;

    slice = new_safe(struct filter_instance);
    b_box = &slice->b_box;
    b_tk = &slice->b_tk;
    memset(b_box, 0, sizeof (*b_box));
    memset(b_tk, 0, sizeof (*b_tk));

    b_box->compute_slack_size = box_compute_slack_size__from_parent;
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->compute_max_span_size = box_compute_max_span_size__as_used;
    b_box->compute_used_size = box_compute_used_size__byte_slice;
    b_box->get_n_items = box_get_n_items__slice_generic;

    b_tk->get_item_key = tracker_get_item_key__byte_slice;
    b_tk->compute_item_size = tracker_compute_item_size__item_box;
    b_tk->goto_first_item = tracker_goto_first_item__array_slice;
    b_tk->goto_next_item = tracker_goto_next_item__array_slice;
    b_tk->goto_nth_item = tracker_goto_nth_item__array_slice;
    b_tk->goto_named_item = tracker_goto_named_item__array_slice;
    b_tk->goto_next_key_match = tracker_goto_next_key_match__array_slice;
    b_tk->goto_next_item_with_key =
        tracker_goto_next_item_with_key__not_impl;
    b_tk->goto_nth_item_with_key =
        tracker_goto_nth_item_with_key__not_impl;

    return slice;
}

static void
browse_setup_backends__byte_slice(struct ast_node_hdl *item)
{
    item->ndat->u.rexpr_filter.f_instance =
        byte_slice_filter_instance_build(item);

    browse_setup_backends__filter__filter(item);
}

static void
browse_setup_backends__box__as_bytes(struct ast_node_hdl *item)
{
    struct filter_instance *as_bytes;
    struct box_backend *b_box;

    as_bytes = item->ndat->u.rexpr_filter.f_instance;
    b_box = &as_bytes->b_box;
    // FIXME avoid memset because filter may have set functions
    // already, find a cleaner way to deal with this
    //memset(b_box, 0, sizeof (*b_box));

    b_box->compute_slack_size = box_compute_slack_size__as_bytes;
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->compute_max_span_size = box_compute_max_span_size__as_slack;
    b_box->compute_used_size = box_compute_used_size__as_bytes;
    b_box->get_n_items = box_get_n_items__as_bytes;
}

static void
browse_setup_backends__tracker__as_bytes(struct ast_node_hdl *item)
{
    struct filter_instance *as_bytes;
    struct tracker_backend *b_tk;

    as_bytes = item->ndat->u.rexpr_filter.f_instance;
    b_tk = &as_bytes->b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

    b_tk->get_item_key = tracker_get_item_key__array_generic;
    b_tk->compute_item_size = tracker_compute_item_size__item_box;
    b_tk->goto_first_item = tracker_goto_first_item__byte_array_generic;
    b_tk->goto_next_item = tracker_goto_next_item__byte_array_generic;
    b_tk->goto_nth_item = tracker_goto_nth_item__byte_array_generic;
    b_tk->goto_next_item_with_key =
        tracker_goto_next_item_with_key__default;
    b_tk->goto_nth_item_with_key =
        tracker_goto_nth_item_with_key__default;
}

static void
browse_setup_backends__as_bytes(struct ast_node_hdl *item)
{
    item->ndat->u.rexpr_filter.f_instance = new_safe(struct filter_instance);

    browse_setup_backends__filter__filter(item);
    browse_setup_backends__box__as_bytes(item);
    browse_setup_backends__tracker__as_bytes(item);
}

static int
browse_setup_backends_node(struct ast_node_hdl *node)
{
    switch (node->ndat->type) {
    case AST_NODE_TYPE_COMPOSITE:
        browse_setup_backends__filter__filter(node);
        browse_setup_backends__box__composite(node);
        browse_setup_backends__tracker__composite(node);
        break ;
    case AST_NODE_TYPE_ARRAY:
        browse_setup_backends__filter__filter(node);
        browse_setup_backends__box__array(node);
        browse_setup_backends__tracker__array(node);
        break ;
    case AST_NODE_TYPE_BYTE:
        browse_setup_backends__byte(node);
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
        browse_setup_backends__filter__filter(node);
        browse_setup_backends__box__byte_array(node);
        browse_setup_backends__tracker__byte_array(node);
        break ;
    case AST_NODE_TYPE_ARRAY_SLICE:
        browse_setup_backends__array_slice(node);
        break ;
    case AST_NODE_TYPE_BYTE_SLICE:
        browse_setup_backends__byte_slice(node);
        break ;
    case AST_NODE_TYPE_AS_BYTES:
        browse_setup_backends__as_bytes(node);
        break ;
    case AST_NODE_TYPE_REXPR_FILTER:
        browse_setup_backends__filter(node);
        break ;
    case AST_NODE_TYPE_REXPR_FILE:
    case AST_NODE_TYPE_REXPR_SELF:
        browse_setup_backends__item(node);
        break ;
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        browse_setup_backends__operator_filter(node);
        break ;
    default:
        break ;
    }
    return 0;
}

static int
browse_setup_backends_stmt_list_generic(struct statement_list *stmt_list)
{
    struct statement *stmt;

    TAILQ_FOREACH(stmt, stmt_list, list) {
        if (NULL != stmt->cond
            && -1 == browse_setup_backends_node_recur(stmt->cond)) {
            return -1;
        }
    }
    return 0;
}

static int
browse_setup_backends_recur_composite(struct ast_node_hdl *filter)
{
    struct block_stmt_list *stmt_lists;
    struct field *field;
    struct named_expr *named_expr;

    stmt_lists = &filter->ndat->u.rexpr_filter.filter_def->block_stmt_list;
    if (-1 == browse_setup_backends_stmt_list_generic(
            stmt_lists->named_expr_list)) {
        return -1;
    }
    if (-1 == browse_setup_backends_stmt_list_generic(
            stmt_lists->field_list)) {
        return -1;
    }
    if (-1 == browse_setup_backends_stmt_list_generic(
            stmt_lists->attribute_list)) {
        return -1;
    }
    STATEMENT_FOREACH(named_expr, named_expr,
                      stmt_lists->named_expr_list, list) {
        if (-1 == browse_setup_backends_node_recur(named_expr->expr)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(field, field, stmt_lists->field_list, list) {
        if (-1 == browse_setup_backends_dpath(
                (struct dpath_node *)&field->dpath)) {
            return -1;
        }
    }
    STATEMENT_FOREACH(named_expr, named_expr,
                      stmt_lists->attribute_list, list) {
        if (-1 == browse_setup_backends_node_recur(named_expr->expr)) {
            return -1;
        }
    }
    return 0;
}

static int
browse_setup_backends_subscript_index(struct ast_node_hdl *expr,
                                     struct subscript_index *subscript)
{
    int ret;

    if (NULL == subscript->key) {
        return 0;
    }
    ret = browse_setup_backends_node_recur(subscript->key);
    if (-1 == ret) {
        return -1;
    }
    if (NULL != subscript->twin) {
        ret = browse_setup_backends_node_recur(subscript->twin);
        if (-1 == ret) {
            return -1;
        }
    }
    return 0;
}

static int
browse_setup_backends_subscript(struct ast_node_hdl *expr)
{
    int ret;
    struct subscript_index *index;
    struct ast_node_hdl *anchor_expr;

    anchor_expr = expr->ndat->u.rexpr_op_subscript_common.anchor_expr;
    assert(NULL != anchor_expr);
    ret = browse_setup_backends_node_recur(anchor_expr);
    if (-1 == ret) {
        return -1;
    }
    index = &expr->ndat->u.rexpr_op_subscript.index;
    ret = browse_setup_backends_subscript_index(expr, index);
    if (-1 == ret) {
        return -1;
    }
    return 0;
}

static int
browse_setup_backends_subscript_slice(struct ast_node_hdl *expr)
{
    int ret;
    struct ast_node_hdl *anchor_expr;
    struct subscript_index *slice_start;
    struct subscript_index *slice_end;

    anchor_expr = expr->ndat->u.rexpr_op_subscript_common.anchor_expr;
    assert(NULL != anchor_expr);
    ret = browse_setup_backends_node_recur(anchor_expr);
    if (-1 == ret) {
        return -1;
    }
    slice_start = &expr->ndat->u.rexpr_op_subscript_slice.start;
    slice_end = &expr->ndat->u.rexpr_op_subscript_slice.end;
    ret = browse_setup_backends_subscript_index(expr, slice_start);
    if (-1 == ret) {
        return -1;
    }
    ret = browse_setup_backends_subscript_index(expr, slice_end);
    if (-1 == ret) {
        return -1;
    }
    return 0;
}

static int
browse_setup_backends_fcall(struct ast_node_hdl *expr)
{
    struct statement *stmt;
    struct named_expr *param;

    /* resolve expressions in parameter list */
    TAILQ_FOREACH(stmt, expr->ndat->u.rexpr_op_fcall.func_params, list) {
        param = (struct named_expr *)stmt;
        if (-1 == browse_setup_backends_node_recur(param->expr)) {
            return -1;
        }
    }
    return 0;
}

int
browse_setup_backends_expr(struct ast_node_hdl *expr)
{
    return browse_setup_backends_node_recur(expr);
}

static int
browse_setup_backends_node_recur(struct ast_node_hdl *node)
{
    struct filter_instance_array *array;
    int ret = 0;

    if (0 != (node->flags & (ASTFLAG_BROWSE_SETUP_BACKENDS_IN_PROGRESS |
                             ASTFLAG_BROWSE_SETUP_BACKENDS_COMPLETED))) {
        return 0;
    }
    node->flags |= ASTFLAG_BROWSE_SETUP_BACKENDS_IN_PROGRESS;
    switch (node->ndat->type) {
    case AST_NODE_TYPE_COMPOSITE:
        ret = browse_setup_backends_recur_composite(node);
        break ;
    case AST_NODE_TYPE_ARRAY:
        array = (struct filter_instance_array *)
            node->ndat->u.rexpr_filter.f_instance;
        ret = browse_setup_backends_dpath(&array->item_type);
        break ;
    case AST_NODE_TYPE_CONDITIONAL:
        ret = browse_setup_backends_node_recur(
            node->ndat->u.conditional.cond_expr);
        break ;
    case AST_NODE_TYPE_REXPR_OP_FILTER:
        if (NULL != node->ndat->u.rexpr_op_filter.target) {
            ret = browse_setup_backends_node_recur(
                node->ndat->u.rexpr_op_filter.target);
        }
        if (0 == ret) {
            ret = browse_setup_backends_node_recur(
                node->ndat->u.rexpr_op_filter.filter_expr);
        }
        break ;
    case AST_NODE_TYPE_REXPR_FIELD:
        return browse_setup_backends_dpath(
            (struct dpath_node *)&node->ndat->u.rexpr_field.field->dpath);
    case AST_NODE_TYPE_REXPR_NAMED_EXPR:
        // XXX is it useful?
        return browse_setup_backends_node_recur(
            node->ndat->u.rexpr_named_expr.named_expr->expr);
    case AST_NODE_TYPE_REXPR_OP_UPLUS:
    case AST_NODE_TYPE_REXPR_OP_UMINUS:
    case AST_NODE_TYPE_REXPR_OP_LNOT:
    case AST_NODE_TYPE_REXPR_OP_BWNOT:
    case AST_NODE_TYPE_REXPR_OP_ANCESTOR:
    case AST_NODE_TYPE_REXPR_OP_EQ:
    case AST_NODE_TYPE_REXPR_OP_NE:
    case AST_NODE_TYPE_REXPR_OP_GT:
    case AST_NODE_TYPE_REXPR_OP_LT:
    case AST_NODE_TYPE_REXPR_OP_GE:
    case AST_NODE_TYPE_REXPR_OP_LE:
    case AST_NODE_TYPE_REXPR_OP_LOR:
    case AST_NODE_TYPE_REXPR_OP_LAND:
    case AST_NODE_TYPE_REXPR_OP_BWOR:
    case AST_NODE_TYPE_REXPR_OP_BWXOR:
    case AST_NODE_TYPE_REXPR_OP_BWAND:
    case AST_NODE_TYPE_REXPR_OP_LSHIFT:
    case AST_NODE_TYPE_REXPR_OP_RSHIFT:
    case AST_NODE_TYPE_REXPR_OP_ADD:
    case AST_NODE_TYPE_REXPR_OP_SUB:
    case AST_NODE_TYPE_REXPR_OP_MUL:
    case AST_NODE_TYPE_REXPR_OP_DIV:
    case AST_NODE_TYPE_REXPR_OP_MOD:
    case AST_NODE_TYPE_REXPR_OP_SIZEOF:
    case AST_NODE_TYPE_REXPR_OP_ADDROF:
        ret = browse_setup_backends_node_recur(
            node->ndat->u.rexpr_op.op.operands[0]);
        if (0 == ret && NULL != node->ndat->u.rexpr_op.op.operands[1]) {
            ret = browse_setup_backends_node_recur(
                node->ndat->u.rexpr_op.op.operands[1]);
        }
        break ;
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT:
        ret = browse_setup_backends_subscript(node);
        break ;
    case AST_NODE_TYPE_REXPR_OP_SUBSCRIPT_SLICE:
        ret = browse_setup_backends_subscript_slice(node);
        break ;
    case AST_NODE_TYPE_REXPR_OP_FCALL:
        ret = browse_setup_backends_fcall(node);
        break ;
    default:
        break ;
    }
    if (-1 == ret) {
        return -1;
    }
    if (-1 == browse_setup_backends_node(node)) {
        return -1;
    }
    node->flags |= ASTFLAG_BROWSE_SETUP_BACKENDS_COMPLETED;
    return 0;
}

int
browse_setup_backends_dpath(struct dpath_node *dpath)
{
    int ret = 0;

    if (NULL != dpath->item) {
        ret = browse_setup_backends_node_recur(dpath->item);
        if (0 != ret) {
            return ret;
        }
    }
    if (NULL != dpath->filter) {
        ret = browse_setup_backends_node_recur(dpath->filter);
        if (0 != ret) {
            return ret;
        }
    }
    return 0;
}

int
browse_setup_global_backends(void)
{
    if (-1 == browse_setup_backends_dpath(DPATH_NODE_ARRAY_SLICE)) {
        return -1;
    }
    if (-1 == browse_setup_backends_dpath(DPATH_NODE_BYTE_SLICE)) {
        return -1;
    }
    if (-1 == browse_setup_backends_node_recur(AST_NODE_BYTE)) {
        return -1;
    }
    if (-1 == browse_setup_backends_node_recur(AST_NODE_AS_BYTES)) {
        return -1;
    }
    return 0;
}

/*
 *
 */


/*
 * external API wrappers
 */

bitpunch_status_t
transmit_error(bitpunch_status_t bt_ret, struct browse_state *bst,
               struct tracker_error **errp)
{
    if (NULL != errp) {
        *errp = bst->last_error;
        bst->last_error = NULL;
    }
    browse_state_cleanup(bst);
    return bt_ret;
}

bitpunch_status_t
expr_dpath_to_dpath(expr_dpath_t src_dpath,
                    enum expr_dpath_type dst_type,
                    expr_dpath_t *dst_dpathp,
                    struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        expr_dpath_to_dpath_internal(src_dpath, dst_type, dst_dpathp, &bst),
        &bst, errp);
}

bitpunch_status_t
expr_dpath_get_size(expr_dpath_t dpath,
                    int64_t *dpath_sizep,
                    struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        expr_dpath_get_size_internal(dpath, dpath_sizep, &bst),
        &bst, errp);
}

bitpunch_status_t
expr_dpath_get_location(expr_dpath_t dpath,
                        int64_t *offsetp, int64_t *sizep,
                        struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        expr_dpath_get_location_internal(dpath, offsetp, sizep, &bst),
        &bst, errp);
}

bitpunch_status_t
box_get_n_items(struct box *box, int64_t *n_itemsp,
                struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_get_n_items_internal(box, n_itemsp, &bst),
        &bst, errp);
}

bitpunch_status_t
box_get_location(struct box *box,
                 int64_t *offsetp, int64_t *sizep,
                 struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_get_location_internal(box, offsetp, sizep, &bst),
        &bst, errp);
}

bitpunch_status_t
box_read_value(struct box *box,
               expr_value_t *valuep,
               struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_read_value_internal(box, valuep, &bst),
        &bst, errp);
}

bitpunch_status_t
box_compute_end_offset(struct box *box,
                       enum box_offset_type off_type,
                       int64_t *end_offsetp,
                       struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_compute_end_offset_internal(box, off_type, end_offsetp, &bst),
        &bst, errp);
}

bitpunch_status_t
box_apply_filter(struct box *box,
                 struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_apply_filter_internal(box, &bst), &bst, errp);
}

bitpunch_status_t
track_item_contents(struct tracker *tk,
                    struct tracker **tkp,
                    struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        track_item_contents_internal(tk, tkp, &bst),
        &bst, errp);
}

bitpunch_status_t
track_dpath_contents_internal(expr_dpath_t dpath,
                              struct tracker **tkp,
                              struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct tracker *tk;

    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = track_item_contents_internal(dpath.item.tk, &tk, bst);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        tk = track_box_contents_internal(dpath.container.box, bst);
        bt_ret = BITPUNCH_OK;
        break ;
    default:
        assert(0);
    }
    if (BITPUNCH_OK == bt_ret) {
        *tkp = tk;
    }
    return bt_ret;
}

bitpunch_status_t
track_dpath_contents(expr_dpath_t dpath,
                     struct tracker **tkp,
                     struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        track_dpath_contents_internal(dpath, tkp, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_get_n_items(struct tracker *tk, int64_t *item_countp,
                    struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_get_n_items_internal(tk, item_countp, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_first_item(struct tracker *tk,
                        struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_goto_first_item_internal(tk, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_next_item(struct tracker *tk,
                       struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_goto_next_item_internal(tk, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_nth_item(struct tracker *tk, int64_t index,
                      struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_goto_nth_item_internal(tk, index, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_nth_position(struct tracker *tk, int64_t index,
                          struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_goto_nth_position_internal(tk, index, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_named_item(struct tracker *tk, const char *name,
                        struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_goto_named_item_internal(tk, name, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_first_item_with_key(struct tracker *tk,
                                 expr_value_t item_key,
                                 struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_goto_first_item_with_key_internal(tk, item_key, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_next_item_with_key(struct tracker *tk,
                                expr_value_t item_key,
                                struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_goto_next_item_with_key_internal(tk, item_key, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_nth_item_with_key(struct tracker *tk,
                               expr_value_t item_key,
                               int nth_twin,
                               struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_goto_nth_item_with_key_internal(tk, item_key, nth_twin,
                                                &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_abs_dpath(struct tracker *tk, const char *dpath_expr,
                       struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_goto_abs_dpath_internal(tk, dpath_expr, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_end(struct tracker *tk,
                 struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_goto_end_internal(tk, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_enter_item(struct tracker *tk,
                   struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_enter_item_internal(tk, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_enter_slice(struct tracker *tk, struct tracker *slice_end,
                    struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_enter_slice_internal(tk, slice_end, &bst),
        &bst, errp);
}


bitpunch_status_t
tracker_return(struct tracker *tk,
               struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_return_internal(tk, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_get_item_filter(struct tracker *tk,
                        struct ast_node_hdl **item_filterp,
                        struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_get_item_filter_internal(tk, item_filterp, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_get_item_offset(struct tracker *tk, int64_t *item_offsetp,
                        struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_get_item_offset_internal(tk, item_offsetp, &bst),
        &bst, errp);
}


bitpunch_status_t
tracker_get_item_size(struct tracker *tk, int64_t *item_sizep,
                      struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_get_item_size_internal(tk, item_sizep, &bst),
        &bst, errp);
}


bitpunch_status_t
tracker_get_item_key(struct tracker *tk,
                     expr_value_t *keyp,
                     struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_get_item_key_internal(tk, keyp, &bst),
        &bst, errp);
}


bitpunch_status_t
tracker_get_item_key_multi(struct tracker *tk,
                           expr_value_t *keyp,
                           int *nth_twinp,
                           struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_get_item_key_multi_internal(tk, keyp, nth_twinp, &bst),
        &bst, errp);
}


bitpunch_status_t
tracker_get_item_location(struct tracker *tk,
                          int64_t *item_offsetp,
                          int64_t *item_sizep,
                          struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_get_item_location_internal(tk, item_offsetp, item_sizep,
                                           &bst),
        &bst, errp);
}


bitpunch_status_t
tracker_read_item_raw(struct tracker *tk,
                      const char **item_contentsp,
                      int64_t *item_sizep,
                      struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_read_item_raw_internal(tk, item_contentsp, item_sizep,
                                       &bst),
        &bst, errp);
}


bitpunch_status_t
tracker_read_item_value(struct tracker *tk,
                        expr_value_t *valuep,
                        struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_read_item_value_internal(tk, valuep, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_get_filtered_dpath(struct tracker *tk,
                           expr_dpath_t *filtered_dpathp,
                           struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_get_filtered_dpath_internal(tk, filtered_dpathp, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_get_filtered_item_box(struct tracker *tk,
                              struct box **filtered_boxp,
                              struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_get_filtered_item_box_internal(tk, filtered_boxp, &bst),
        &bst, errp);
}

struct tracker *
track_box_contents(struct box *box,
                   struct tracker_error **errp)
{
    struct browse_state bst;
    struct tracker *box_tk;

    browse_state_init(&bst);
    box_tk = track_box_contents_internal(box, &bst);
    (void) transmit_error(BITPUNCH_OK, &bst, errp);
    return box_tk;
}


/*
 *
 */
