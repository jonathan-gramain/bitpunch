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
#include "filters/byte.h"
#include "filters/byte_array.h"
#include "filters/array_slice.h"
#include "filters/byte_slice.h"

#define DISABLE_BOX_CACHING

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


struct track_path
track_path_from_field(const struct field *field)
{
    struct track_path ret;

    memset(&ret, 0, sizeof (ret));
    ret.type = TRACK_PATH_FIELD;
    ret.u.field = field;
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

struct track_path
track_path_from_array_index(int64_t index)
{
    struct track_path ret;

    memset(&ret, 0, sizeof (ret));
    ret.type = TRACK_PATH_ARRAY;
    ret.u.array.index = index;
    return ret;
}

struct track_path
track_path_from_array_slice(int64_t index_start, int64_t index_end)
{
    struct track_path ret;

    memset(&ret, 0, sizeof (ret));
    ret.type = TRACK_PATH_ARRAY_SLICE;
    ret.u.array.index = index_start;
    ret.u.array_slice.index_end = index_end;
    return ret;
}

int
track_path_eq(struct track_path p1, struct track_path p2)
{
    if (p1.type != p2.type) {
        return FALSE;
    }
    switch (p1.type) {
    case TRACK_PATH_NOTYPE:
        return TRUE;
    case TRACK_PATH_FIELD:
        return p1.u.field == p2.u.field;
    case TRACK_PATH_ARRAY:
        return p1.u.array.index == p2.u.array.index;
    case TRACK_PATH_ARRAY_SLICE:
        return (p1.u.array.index == p2.u.array.index &&
                p1.u.array_slice.index_end == p2.u.array_slice.index_end);
    default:
        return FALSE;
    }
}


static const char *
box_offset_type_str(enum box_offset_type type);

static void
tracker_set_dangling_internal(struct tracker *tk);
static struct tracker *
tracker_new(struct box *box);
static bitpunch_status_t
tracker_compute_item_size_internal(struct tracker *tk,
                                   int64_t *item_sizep,
                                   struct browse_state *bst);

static bitpunch_status_t
tracker_goto_end_path(struct tracker *tk,
                      struct browse_state *bst);
static bitpunch_status_t
tracker_goto_end_offset(struct tracker *tk,
                        struct browse_state *bst);
static bitpunch_status_t
tracker_goto_end_internal(struct tracker *tk,
                          struct browse_state *bst);

void
browse_state_init(struct browse_state *bst)
{
    memset(bst, 0, sizeof (*bst));
}

void
browse_state_init_scope(struct browse_state *bst, struct box *scope)
{
    browse_state_init(bst);
    bst->scope = scope;
}

void
browse_state_init_box(struct browse_state *bst, struct box *box)
{
    browse_state_init_scope(bst, box);
}

void
browse_state_init_tracker(struct browse_state *bst, struct tracker *tk)
{
    browse_state_init_box(bst, tk->box);
}

void
browse_state_init_dpath(struct browse_state *bst, expr_dpath_t dpath)
{
    browse_state_init(bst);
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        browse_state_init_tracker(bst, dpath.tk);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        browse_state_init_box(bst, dpath.box);
        break ;
    default:
        assert(0);
    }
}

void
browse_state_cleanup(struct browse_state *bst)
{
    if (NULL != bst) {
        tracker_error_destroy(bst->last_error);
    }
}

void
browse_state_push_scope(struct browse_state *bst, struct box *scope,
                        struct box **storagep)
{
    if (NULL != scope) {
        *storagep = bst->scope;
        assert(NULL != *storagep);
        bst->scope = scope;
    }
}

void
browse_state_pop_scope(struct browse_state *bst, struct box *scope,
                       struct box **storagep)
{
    if (NULL != scope) {
        assert(NULL != *storagep);
        bst->scope = *storagep;
    }
}

void
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
 * box
 */

int64_t
box_get_offset(struct box *box, enum box_offset_type type)
{
    switch (type) {
    case BOX_START_OFFSET_HARD_MIN:
        return box->end_offset_span -
            ast_node_get_min_span_size(box->filter);
    case BOX_START_OFFSET_MIN_SPAN:
        return box->start_offset_min_span;
    case BOX_START_OFFSET_SPAN:
        return box->start_offset_span;
    case BOX_START_OFFSET_MAX_SPAN:
        return box->start_offset_max_span;
    case BOX_START_OFFSET_SLACK:
        return box->start_offset_slack;
    case BOX_START_OFFSET_PARENT:
        return box->start_offset_parent;
    case BOX_START_OFFSET_USED:
        return box->start_offset_used;
    case BOX_END_OFFSET_HARD_MIN:
        return box->start_offset_span +
            ast_node_get_min_span_size(box->filter);
    case BOX_END_OFFSET_MIN_SPAN:
        return box->end_offset_min_span;
    case BOX_END_OFFSET_SPAN:
        return box->end_offset_span;
    case BOX_END_OFFSET_MAX_SPAN:
        return box->end_offset_max_span;
    case BOX_END_OFFSET_SLACK:
        return box->end_offset_slack;
    case BOX_END_OFFSET_PARENT:
        return box->end_offset_parent;
    case BOX_END_OFFSET_USED:
        return box->end_offset_used;
    default:
        assert(0);
    }
}

int64_t
box_get_known_start_offset_mask(const struct box *box,
                                enum box_offset_type mask)
{
    if ((mask & BOX_START_OFFSET_USED) && box->start_offset_used >= 0) {
        return box->start_offset_used;
    }
    if ((mask & BOX_START_OFFSET_SPAN) && box->start_offset_span >= 0) {
        return box->start_offset_span;
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

int64_t
box_get_known_start_offset(const struct box *box)
{
    return box_get_known_start_offset_mask(box,
                                           BOX_START_OFFSET_USED |
                                           BOX_START_OFFSET_SPAN |
                                           BOX_START_OFFSET_MAX_SPAN |
                                           BOX_START_OFFSET_SLACK |
                                           BOX_START_OFFSET_PARENT);
}

int64_t
box_get_known_end_offset_mask(const struct box *box,
                              enum box_offset_type mask)
{
    if ((mask & BOX_END_OFFSET_USED) && box->end_offset_used >= 0) {
        return box->end_offset_used;
    }
    if ((mask & BOX_END_OFFSET_SPAN) && box->end_offset_span >= 0) {
        return box->end_offset_span;
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

int64_t
box_get_known_end_offset(const struct box *box)
{
    return box_get_known_end_offset_mask(box,
                                         BOX_END_OFFSET_USED |
                                         BOX_END_OFFSET_SPAN |
                                         BOX_END_OFFSET_MAX_SPAN |
                                         BOX_END_OFFSET_SLACK |
                                         BOX_END_OFFSET_PARENT);
}

enum box_offset_type
box_get_known_end_offset_type(const struct box *box)
{
    if (box->end_offset_used >= 0) {
        return BOX_END_OFFSET_USED;
    }
    if (box->end_offset_span >= 0) {
        return BOX_END_OFFSET_SPAN;
    }
    if (box->end_offset_max_span >= 0) {
        return BOX_END_OFFSET_MAX_SPAN;
    }
    if (box->end_offset_slack >= 0) {
        return BOX_END_OFFSET_SLACK;
    }
    return BOX_END_OFFSET_PARENT;
}

bitpunch_status_t
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
        if (box->start_offset_span >= 0) {
            if (start_offset < box->start_offset_span) {
                return box_error_out_of_bounds(box, NULL, type,
                                               start_offset,
                                               BOX_START_OFFSET_SPAN, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_START_OFFSET_SPAN:
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
    case BOX_START_OFFSET_USED:
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
        if (box->start_offset_span >= 0) {
            if (start_offset > box->start_offset_span) {
                return box_error_out_of_bounds(box, NULL, type,
                                               start_offset,
                                               BOX_START_OFFSET_SPAN, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_START_OFFSET_SPAN:
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
    case BOX_START_OFFSET_USED:
        break ;
    default:
        assert(0);
    }
    return BITPUNCH_OK;
}


bitpunch_status_t
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
        if (box->end_offset_span >= 0) {
            if (end_offset > box->end_offset_span) {
                return box_error_out_of_bounds(box, NULL, type,
                                               end_offset,
                                               BOX_END_OFFSET_SPAN, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_SPAN:
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
    case BOX_END_OFFSET_USED:
        // used offsets are at the discretion of the box filter
        // (provided they stay within their output data source)
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
        if (box->end_offset_span >= 0) {
            if (end_offset < box->end_offset_span) {
                return box_error_out_of_bounds(box, NULL, type,
                                               end_offset,
                                               BOX_END_OFFSET_SPAN, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_SPAN:
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
    case BOX_END_OFFSET_USED:
        // used offsets are at the discretion of the box filter
        // (provided they stay within their output data source)
        break ;
    default:
        assert(0);
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
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
        box->start_offset_max_span = start_offset;
        break ;
    case BOX_START_OFFSET_SPAN:
        box->start_offset_span = start_offset;
        break ;
    case BOX_START_OFFSET_MIN_SPAN: {
        int64_t start_offset_hard_min;

        start_offset_hard_min = box->end_offset_span -
            ast_node_get_min_span_size(box->filter);
        box->start_offset_min_span = MIN(start_offset, start_offset_hard_min);
        break ;
    }
    case BOX_START_OFFSET_USED:
        box->start_offset_used = start_offset;
        break ;
    case BOX_START_OFFSET_HARD_MIN:
        break ;
    default:
        assert(0);
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
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
        box->end_offset_max_span = end_offset;
        break ;
    case BOX_END_OFFSET_SPAN:
        box->end_offset_span = end_offset;
        break ;
    case BOX_END_OFFSET_MIN_SPAN: {
        int64_t end_offset_hard_min;

        end_offset_hard_min = box->start_offset_span +
            ast_node_get_min_span_size(box->filter);
        box->end_offset_min_span = MAX(end_offset, end_offset_hard_min);
        break ;
    }
    case BOX_END_OFFSET_USED:
        box->end_offset_used = end_offset;
        break ;
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
    case BOX_START_OFFSET_SPAN:
        return "span start";
    case BOX_START_OFFSET_MIN_SPAN:
        return "min span start";
    case BOX_START_OFFSET_HARD_MIN:
        return "hard min start";
    case BOX_START_OFFSET_USED:
        return "used start";
    case BOX_END_OFFSET_PARENT:
        return "parent end";
    case BOX_END_OFFSET_SLACK:
        return "slack end";
    case BOX_END_OFFSET_MAX_SPAN:
        return "max span end";
    case BOX_END_OFFSET_SPAN:
        return "span end";
    case BOX_END_OFFSET_MIN_SPAN:
        return "min span end";
    case BOX_END_OFFSET_HARD_MIN:
        return "hard min end";
    case BOX_END_OFFSET_USED:
        return "used end";
    default:
        return "(bad offset type)";
    }
}

bitpunch_status_t
box_set_size(struct box *box, int64_t box_size,
             enum box_offset_type size_type,
             struct browse_state *bst)
{
    if (0 != (box->flags & BOX_RALIGN)) {
        assert(-1 != box->end_offset_span);
        return box_set_start_offset(box, box->end_offset_span - box_size,
                                    (size_type & BOX_START_OFFSETS), bst);
    } else {
        assert(-1 != box->start_offset_span);
        return box_set_end_offset(box, box->start_offset_span + box_size,
                                  (size_type & BOX_END_OFFSETS), bst);
    }
}

bitpunch_status_t
box_set_min_span_size(struct box *box, int64_t min_span_size,
                      struct browse_state *bst)
{
    return box_set_size(box, min_span_size, BOX_SIZE_MIN_SPAN, bst);
}

bitpunch_status_t
box_set_span_size(struct box *box, int64_t span_size,
                  struct browse_state *bst)
{
    return box_set_size(box, span_size, BOX_SIZE_SPAN, bst);
}

bitpunch_status_t
box_set_max_span_size(struct box *box, int64_t max_span_size,
                      struct browse_state *bst)
{
    return box_set_size(box, max_span_size, BOX_SIZE_MAX_SPAN, bst);
}

bitpunch_status_t
box_set_used_size(struct box *box, int64_t used_size,
                  struct browse_state *bst)
{
    return box_set_size(box, used_size, BOX_SIZE_USED, bst);
}


static void
box_set_boundary_offset(struct box *box,
                        int64_t boundary_offset, int64_t parent_limit_offset)
{
    if (0 != (box->flags & BOX_RALIGN)) {
        if (-1 == box->end_offset_parent) {
            box->end_offset_parent = boundary_offset;
            box->end_offset_slack = boundary_offset;
            box->end_offset_max_span = boundary_offset;
            box->end_offset_span = boundary_offset;
            box->end_offset_min_span = boundary_offset;
            box->end_offset_used = boundary_offset;
        }
        box->start_offset_parent = parent_limit_offset;
    } else {
        if (-1 == box->start_offset_parent) {
            box->start_offset_parent = boundary_offset;
            box->start_offset_slack = boundary_offset;
            box->start_offset_max_span = boundary_offset;
            box->start_offset_span = boundary_offset;
            box->start_offset_min_span = boundary_offset;
            box->start_offset_used = boundary_offset;
        }
        box->end_offset_parent = parent_limit_offset;
    }
}

static void
box_inherit_boundary_offset(struct box *box)
{
    int64_t boundary_offset;
    int64_t parent_limit_offset;

    if (0 != (box->flags & BOX_RALIGN)) {
        boundary_offset = box_get_known_end_offset(box->parent_box);
        parent_limit_offset = box_get_known_start_offset(box->parent_box);
    } else {
        boundary_offset = box_get_known_start_offset(box->parent_box);
        parent_limit_offset = box_get_known_end_offset(box->parent_box);
    }
    box_set_boundary_offset(box, boundary_offset, parent_limit_offset);
}

static void
box_setup_input_boundaries(struct box *box)
{
    int box_is_right_aligned;

    assert(NULL != box->parent_box);

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
    box_inherit_boundary_offset(box);
}

static void
box_setup_overlay(struct box *box)
{
    box->ds_out = box->ds_in;
    box->flags |= BOX_OVERLAY;
}

bitpunch_status_t
box_construct(struct box *o_box,
              struct box *parent_box,
              struct ast_node_hdl *filter,
              struct box *scope,
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
        return box_error(BITPUNCH_DATA_ERROR, parent_box, filter, bst,
                         "reached maximum box nesting level %d",
                         BOX_MAX_DEPTH_LEVEL);
    }
    //assert(ast_node_is_rexpr_filter(filter));
    o_box->filter = filter;
    o_box->flags = box_flags;
    o_box->start_offset_parent = -1;
    o_box->start_offset_slack = -1;
    o_box->start_offset_max_span = -1;
    o_box->start_offset_span = -1;
    o_box->start_offset_min_span = -1;
    o_box->end_offset_parent = -1;
    o_box->end_offset_slack = -1;
    o_box->end_offset_max_span = -1;
    o_box->end_offset_span = -1;
    o_box->end_offset_min_span = -1;
    o_box->start_offset_used = -1;
    o_box->end_offset_used = -1;
    if (NULL != parent_box) {
        assert(parent_box != o_box);
        o_box->parent_box = parent_box;
        o_box->depth_level = parent_box->depth_level + 1;
        box_acquire(parent_box);
    }
    o_box->scope = scope;
    box_acquire(scope);
    if (-1 != boundary_offset) {
        box_set_boundary_offset(o_box, boundary_offset, -1);
    } else {
        //box_inherit_boundary_offset(o_box);
    }
    /* initialize internal state */
    switch (filter->ndat->type) {
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_SOURCE:
        o_box->u.array_generic.n_items = -1;
        break ;
    case AST_NODE_TYPE_REXPR_FILTER:
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE:
    case AST_NODE_TYPE_BYTE_ARRAY:
        if (NULL != filter->ndat->u.rexpr_filter.f_instance->b_box.init) {
            bt_ret = filter->ndat->u.rexpr_filter.f_instance->b_box.init(
                o_box, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
        }
        break ;
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
        "COMPUTING_SLACK_CHILD_ALLOCATION",
        "BOX_CACHED",
        "BOX_RALIGN",
        "BOX_FILTER",
        "BOX_DATA_SOURCE",
        "BOX_OVERLAY",
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
            ": IN [%"PRIi64"p..[%"PRIi64"sl..[%"PRIi64"M.."
            "[%"PRIi64"sp..[%"PRIi64"m..%"PRIi64"m].."
            "%"PRIi64"sp]..%"PRIi64"M]..%"PRIi64"sl]"
            "..%"PRIi64"p] OUT [%"PRIi64"..%"PRIi64"]\n",
            box->start_offset_parent, box->start_offset_slack,
            box->start_offset_max_span, box->start_offset_span,
            box->start_offset_min_span,
            box->end_offset_min_span, box->end_offset_span,
            box->end_offset_max_span, box->end_offset_slack,
            box->end_offset_parent,
            box->start_offset_used, box->end_offset_used);
    fprintf(out,
            "%*sftype: %s flags: ",
            (indent + box->depth_level) * 4, "",
            ast_node_type_str(box->filter->ndat->type));
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
    cache = box->ds_in->box_cache;
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
box_new_root_box_internal(struct bitpunch_schema *schema,
                          struct bitpunch_env *env,
                          struct browse_state *bst)
{
    struct box *env_box;
    struct box *root_box;
    bitpunch_status_t bt_ret;

    env_box = new_safe(struct box);
    bt_ret = box_construct(env_box, NULL, env->ast_root, NULL,
                           0, 0u, bst);
    if (BITPUNCH_OK != bt_ret) {
        /* TODO error reporting */
        box_delete_non_null(env_box);
        return NULL;
    }
    env_box->ds_in = NULL;
    env_box->ds_out = NULL;

    root_box = new_safe(struct box);
    bt_ret = box_construct(root_box, env_box, schema->ast_root, env_box,
                           0, 0u, bst);
    if (BITPUNCH_OK != bt_ret) {
        /* TODO error reporting */
        box_delete_non_null(root_box);
        box_delete_non_null(env_box);
        return NULL;
    }
    bst->scope = root_box;
    return root_box;
}

struct box *
box_new_root_box(struct bitpunch_schema *schema,
                 struct bitpunch_env *env)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return box_new_root_box_internal(schema, env, &bst);
}

static struct box *
box_new_from_file_internal(const struct bitpunch_schema *def_hdl,
                           struct bitpunch_data_source *ds_in,
                           struct browse_state *bst)
{
    struct box *source_box;
    struct box *composite_box;
    struct ast_node_hdl *root_filter;
    bitpunch_status_t bt_ret;

    // TODO decouple source from composite in bp file syntax: source
    // should be declared as an independent filter that returns a bare
    // dpath, which can be chained with a composite or other type of
    // (usually trackable) filter

    root_filter = def_hdl->ast_root;
    assert(NULL != root_filter);
    source_box = new_safe(struct box);
    bt_ret = box_construct(source_box, NULL,
                           filter_get_global_instance__source(), bst->scope,
                           0, 0u, bst);
    if (BITPUNCH_OK != bt_ret) {
        /* TODO error reporting */
        box_delete_non_null(source_box);
        return NULL;
    }
    source_box->ds_in = NULL;
    source_box->ds_out = ds_in;
    source_box->start_offset_used = 0;
    source_box->end_offset_used = ds_in->ds_data_length;
    source_box->flags |= BOX_FILTER | BOX_FILTER_APPLIED;
    if (NULL == ds_in->box_cache) {
        ds_in->box_cache = box_cache_new(BOX_CACHE_MAX_N_BOXES,
                                         BOX_CACHE_MAX_N_CACHED_CHILDREN);
    }
    composite_box = box_new_filter_box(source_box, root_filter, bst);
    if (NULL == composite_box) {
        box_delete_non_null(source_box);
    }
    // root scope (FIXME: doing this only because file box is the root
    // box for now)
    assert(NULL != composite_box);
    bst->scope = composite_box;
    return composite_box;
}

struct box *
box_new_from_file(const struct bitpunch_schema *def_hdl,
                  struct bitpunch_data_source *ds_in)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return box_new_from_file_internal(def_hdl, ds_in, &bst);
}



struct box *
box_new_filter_box(struct box *parent_box,
                   struct ast_node_hdl *filter,
                   struct browse_state *bst)
{
    struct box *box;
    bitpunch_status_t bt_ret;
    enum box_flag flags;
    struct filter_instance *f_instance;

    box = new_safe(struct box);
    flags = BOX_FILTER;
    if (NULL != parent_box) {
        flags |= (parent_box->flags & BOX_RALIGN);
    }
    assert(ast_node_is_rexpr_filter(filter));
    f_instance = filter->ndat->u.rexpr_filter.f_instance;
    if (NULL != f_instance->get_data_source_func) {
        flags |= BOX_DATA_SOURCE;
    }
    bt_ret = box_construct(box, parent_box, filter, bst->scope, -1, flags, bst);
    if (BITPUNCH_OK != bt_ret) {
        box_delete_non_null(box);
        return NULL;
    }
    box->track_path = TRACK_PATH_NONE;
    return box;
}


static bitpunch_status_t
box_apply_local_filter__data_filter(struct box *box, struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t unfiltered_size;
    expr_value_t filtered_value;
    const char *filtered_data;
    int64_t filtered_size;

    if (0 != (box->flags & BOX_RALIGN)) {
        bt_ret = box_get_span_size(box, &unfiltered_size, bst);
    } else {
        bt_ret = box_get_max_span_size(box, &unfiltered_size, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != box->start_offset_span);
    bt_ret = filter_instance_read_value(
        box->filter, box,
        box->start_offset_span, unfiltered_size,
        &filtered_value, bst);
    if (BITPUNCH_OK != bt_ret) {
        return box_error(bt_ret, box, box->filter, bst,
                         "error reading value through filter");
    }
    if (!expr_value_type_mask_contains_dpath(filtered_value.type)) {
        // dynamic evaluation did not yield a dpath type
        expr_value_destroy(filtered_value);
        box_setup_overlay(box);
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
    if (filtered_data >= box->ds_in->ds_data &&
        filtered_data < box->ds_in->ds_data + box->ds_in->ds_data_length) {
        box->ds_out = box->ds_in;
        box->start_offset_used = filtered_data - box->ds_out->ds_data;
        box->end_offset_used =
            (filtered_data + filtered_size) - box->ds_out->ds_data;
    } else {
        (void) bitpunch_data_source_create_from_memory(
            &box->ds_out, filtered_data, filtered_size, TRUE);
        box->flags |= BOX_DATA_SOURCE;
        box->start_offset_used = 0;
        box->end_offset_used = filtered_size;
    }
    expr_value_destroy(filtered_value);
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_apply_local_filter__data_source(
    struct box *box, struct browse_state *bst)
{
    box->ds_out = box->filter->ndat->u.rexpr_data_source.data_source;
    box->start_offset_used = 0;
    box->end_offset_used = box->ds_out->ds_data_length;
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_apply_local_filter__get_data_source(
    struct box *box, struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = filter_instance_get_data_source(box->filter, box,
                                             &box->ds_out, bst);
    if (BITPUNCH_OK == bt_ret) {
        box->start_offset_used = 0;
        box->end_offset_used = box->ds_out->ds_data_length;
    }
    return bt_ret;
}

static bitpunch_status_t
box_apply_local_filter(struct box *box, struct browse_state *bst)
{
    const struct filter_class *filter_cls;
    struct filter_instance *f_instance;

    assert(NULL == box->ds_out);
    if (0 == (box->flags & BOX_FILTER)) {
        box_setup_overlay(box);
        return BITPUNCH_OK;
    }
    if (AST_NODE_TYPE_REXPR_DATA_SOURCE == box->filter->ndat->type) {
        return box_apply_local_filter__data_source(box, bst);
    }
    filter_cls = box->filter->ndat->u.rexpr_filter.filter_cls;
    if (NULL == filter_cls
        || !expr_value_type_mask_contains_dpath(filter_cls->value_type_mask)) {
        box_setup_input_boundaries(box);
        box_setup_overlay(box);
        return BITPUNCH_OK;
    }
    f_instance = box->filter->ndat->u.rexpr_filter.f_instance;
    if (NULL != f_instance->get_data_source_func) {
        return box_apply_local_filter__get_data_source(box, bst);
    }
    return box_apply_local_filter__data_filter(box, bst);
}

bitpunch_status_t
box_apply_parent_filter_internal(struct box *box,
                                 struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    assert(NULL != box);

    if (NULL != box->parent_box) {
        bt_ret = box_apply_filter_internal(box->parent_box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        box->ds_in = box->parent_box->ds_out;
        if (NULL != box->ds_in) {
            box_inherit_boundary_offset(box);
        }
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
box_apply_filter_internal(struct box *box,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    assert(NULL != box);

    if (0 != (box->flags & BOX_FILTER_APPLIED)) {
        return BITPUNCH_OK;
    }
    bt_ret = box_apply_parent_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = box_apply_local_filter(box, bst);
    if (BITPUNCH_OK == bt_ret) {
        box->flags |= BOX_FILTER_APPLIED;
    }
    return bt_ret;
}


static void
box_free(struct box *box)
{
    struct ast_node_hdl *item;

    item = box->filter;
    /* destroy internal state */
    if (NULL != item) {
        switch (item->ndat->type) {
        case AST_NODE_TYPE_REXPR_FILTER:
        case AST_NODE_TYPE_ARRAY:
            if (NULL != item->ndat->u.rexpr_filter.f_instance->b_box.destroy) {
                item->ndat->u.rexpr_filter.f_instance->b_box.destroy(box);
            }
            break ;
        default:
            break ;
        }
    }
    if (0 != (box->flags & BOX_DATA_SOURCE)) {
        (void)bitpunch_data_source_release(
            (struct bitpunch_data_source *)box->ds_out);
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

void
box_delete_non_null(struct box *box)
{
    assert(box->use_count > 0);
    --box->use_count;
    if (0 == box->use_count) {
        box_delete(box->parent_box);
        box_delete(box->scope);
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


int
box_contains_indexed_items(const struct box *box)
{
    switch (box->filter->ndat->type) {
    case AST_NODE_TYPE_ARRAY_SLICE:
        return box_contains_indexed_items(box->parent_box);
    default:
        return ast_node_is_indexed(box->filter);
    }
}

enum expr_value_type
box_get_index_type(const struct box *box)
{
    switch (box->filter->ndat->type) {
    case AST_NODE_TYPE_ARRAY_SLICE:
        return box_get_index_type(box->parent_box);
    default:
        return ast_node_get_key_type(box->filter);
    }
}

struct ast_node_hdl *
box_get_key_expr(const struct box *box)
{
    switch (box->filter->ndat->type) {
    case AST_NODE_TYPE_ARRAY_SLICE:
        return box_get_key_expr(box->parent_box);
    default:
        return ast_node_get_key_expr(box->filter);
    }
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
        } else if (-1 != tk->item_offset) {
            return TRACKER_STATE_ITEM_OFFSET;
        } else {
            return TRACKER_STATE_ITEM;
        }
    } else {
        return TRACKER_STATE_ITEM_SIZE;
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
    tk->dpath.item = NULL;
}

void
tracker_reset_item_cache(struct tracker *tk)
{
    DBG_TRACKER_DUMP(tk);
    tracker_reset_item_cache_internal(tk);
}

static void
tracker_goto_nil(struct tracker *tk)
{
    struct ast_node_hdl *item;

    item = tk->box->filter;
    assert(ast_node_is_rexpr_filter(item));
    if (NULL != item->ndat->u.rexpr_filter.f_instance->b_tk.goto_nil) {
        item->ndat->u.rexpr_filter.f_instance->b_tk.goto_nil(tk);
    }
}

static void
tracker_set_dangling_internal(struct tracker *tk)
{
    tracker_reset_item_cache_internal(tk);
    tracker_reset_dpath_internal(tk);
    tk->flags &= ~TRACKER_AT_END;
    tracker_goto_nil(tk);
}

void
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
    tracker_goto_nil(o_tk);
}

static void
tracker_destroy(struct tracker *tk)
{
    box_delete_non_null(tk->box);
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
    tracker_destroy(tk);
    memcpy(tk, src_tk, sizeof (*tk));
}

static struct tracker *
tracker_dup_raw(struct tracker *tk)
{
    struct tracker *tk_dup;

    tk_dup = dup_safe(tk);
    box_acquire(tk_dup->box);
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

bitpunch_status_t
tracker_set_end(struct tracker *tk, struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    tracker_set_end_nocheck(tk);
    return BITPUNCH_OK;
}

bitpunch_status_t
tracker_create_item_box_internal(struct tracker *tk,
                                 struct box **item_boxp,
                                 struct browse_state *bst)
{
    struct tracker *xtk;
    struct box *item_box = NULL;
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
    box_flags = 0 != (xtk->flags & TRACKER_REVERSED) ?
        BOX_RALIGN | BOX_OVERLAY : BOX_OVERLAY;
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
        bt_ret = tracker_compute_item_filter_internal(xtk, bst);
        if (BITPUNCH_OK != bt_ret) {
            goto end;
        }
        item_box = new_safe(struct box);
        // it's an item box, so the filter is the item here
        assert(NULL != bst->scope);
        bt_ret = box_construct(item_box, xtk->box, xtk->dpath.item,
                               bst->scope,
                               xtk->item_offset, box_flags, bst);
        if (BITPUNCH_OK != bt_ret) {
            goto end;
        }
    }
    item_box->track_path = xtk->cur;
    if (-1 != item_box->start_offset_span
        && -1 != item_box->end_offset_span) {
        xtk->item_size =
            item_box->end_offset_span - item_box->start_offset_span;
    } else if (-1 != xtk->item_size) {
        bt_ret = box_set_span_size(item_box, xtk->item_size, bst);
        if (BITPUNCH_OK != bt_ret) {
            goto end;
        }
    }
    box_update_cache(item_box, bst);
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
    if (BITPUNCH_OK == bt_ret) {
        *item_boxp = item_box;
    } else {
        box_delete(item_box);
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
    transform.dpath.tk = tracker_dup(tk);
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
    struct box *scope_storage;

    if (-1 != box->start_offset_min_span && -1 != box->end_offset_min_span) {
        /* nothing to do */
        return BITPUNCH_OK;
    }
    bt_ret = box_apply_parent_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    browse_state_push_scope(bst, box, &scope_storage);
    bt_ret = box->filter->ndat->u.rexpr_filter.f_instance->b_box.compute_min_span_size(
        box, bst);
    browse_state_pop_scope(bst, box, &scope_storage);
    return bt_ret;
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

bitpunch_status_t
box_compute_span_size(struct box *box,
                      struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int call_backend;
    struct box *scope_storage;

    if (-1 != box->start_offset_span && -1 != box->end_offset_span) {
        /* nothing to do */
        return BITPUNCH_OK;
    }
    bt_ret = box_compute_min_span_size(box, bst);
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
                bt_ret = box_set_span_size(box, box->end_offset_max_span
                                           - box->start_offset_max_span,
                                           bst);
            } else {
                call_backend = TRUE;
            }
        }
        if (call_backend) {
            browse_state_push_scope(bst, box, &scope_storage);
            bt_ret = box->filter->ndat->u.rexpr_filter.f_instance->b_box.compute_span_size(
                box, bst);
            browse_state_pop_scope(bst, box, &scope_storage);
        }
    }
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(
            box, bst, "when computing used size");
    }
    return bt_ret;
}

bitpunch_status_t
box_compute_used_size(struct box *box, struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *scope_storage;

    if (-1 != box->start_offset_used && -1 != box->end_offset_used) {
        /* nothing to do */
        return BITPUNCH_OK;
    }
    bt_ret = box_apply_parent_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    browse_state_push_scope(bst, box, &scope_storage);
    bt_ret = box->filter->ndat->u.rexpr_filter.f_instance->b_box.compute_used_size(
        box, bst);
    browse_state_pop_scope(bst, box, &scope_storage);
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
box_get_span_size(struct box *box, int64_t *span_sizep,
                  struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = box_compute_span_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (NULL != span_sizep) {
        *span_sizep = box->end_offset_span - box->start_offset_span;
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
box_get_max_span_size(struct box *box, int64_t *max_span_sizep,
                      struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = box_compute_max_span_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (NULL != max_span_sizep) {
        *max_span_sizep = box->end_offset_max_span - box->start_offset_max_span;
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

bitpunch_status_t
box_compute_max_span_size(struct box *box,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *scope_storage;

    bt_ret = box_apply_parent_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if ((-1 != box->start_offset_max_span && -1 != box->end_offset_max_span)
        || 0 != (box->flags & COMPUTING_SPAN_SIZE)) {
        /* nothing to do */
        return BITPUNCH_OK;
    }
    box->flags |= COMPUTING_SPAN_SIZE;
    browse_state_push_scope(bst, box, &scope_storage);
    bt_ret = box->filter->ndat->u.rexpr_filter.f_instance->b_box.compute_max_span_size(box, bst);
    browse_state_pop_scope(bst, box, &scope_storage);
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
bitpunch_status_t
box_compute_slack_size(struct box *box,
                       struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *scope_storage;

    if (-1 != box->start_offset_slack && -1 != box->end_offset_slack) {
        /* nothing to do */
        return BITPUNCH_OK;
    }
    bt_ret = box_apply_parent_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    browse_state_push_scope(bst, box, &scope_storage);
    bt_ret = box->filter->ndat->u.rexpr_filter.f_instance->b_box.compute_slack_size(
        box, bst);
    browse_state_pop_scope(bst, box, &scope_storage);
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(
            box, bst, "when computing slack size");
    }
    return bt_ret;
}

bitpunch_status_t
box_get_slack_child_allocation(struct box *box,
                               int get_left_offset,
                               int64_t *max_slack_offsetp,
                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int flag_set = FALSE;
    struct filter_instance *f_instance;
    struct box *scope_storage;

    // check for circular dependency
    if (!(box->flags & COMPUTING_SLACK_CHILD_ALLOCATION)) {
        box->flags |= COMPUTING_SLACK_CHILD_ALLOCATION;
        flag_set = TRUE;
    }
    bt_ret = box_apply_parent_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    f_instance = box->filter->ndat->u.rexpr_filter.f_instance;
    if (NULL != f_instance->b_box.get_slack_child_allocation) {
        browse_state_push_scope(bst, box, &scope_storage);
        bt_ret = f_instance->b_box.get_slack_child_allocation(
            box, get_left_offset, max_slack_offsetp, bst);
        browse_state_pop_scope(bst, box, &scope_storage);
    } else {
        if (get_left_offset) {
            *max_slack_offsetp = box_get_known_start_offset(box);
        } else {
            *max_slack_offsetp = box_get_known_end_offset(box);
        }
    }
    if (flag_set) {
        box->flags &= ~COMPUTING_SLACK_CHILD_ALLOCATION;
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
    struct box *scope_storage;

    browse_state_push_scope(bst, box, &scope_storage);
    bt_ret = box->filter->ndat->u.rexpr_filter.f_instance->b_box.get_n_items(
        box, n_itemsp, bst);
    browse_state_pop_scope(bst, box, &scope_storage);
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(
            box, bst, "when computing number of array items");
    }
    return bt_ret;
}

static bitpunch_status_t
box_compute_size_internal(struct box *box,
                          enum box_offset_type off_type,
                          int64_t *sizep,
                          struct browse_state *bst)
{
    int64_t size;
    bitpunch_status_t bt_ret;

    switch (off_type) {
    case BOX_START_OFFSET_MIN_SPAN:
    case BOX_END_OFFSET_MIN_SPAN:
    case BOX_SIZE_MIN_SPAN:
        bt_ret = box_compute_min_span_size(box, bst);
        size = box->end_offset_min_span - box->start_offset_min_span;
        break ;
    case BOX_START_OFFSET_SPAN:
    case BOX_END_OFFSET_SPAN:
    case BOX_SIZE_SPAN:
        bt_ret = box_compute_span_size(box, bst);
        size = box->end_offset_span - box->start_offset_span;
        break ;
    case BOX_START_OFFSET_MAX_SPAN:
    case BOX_END_OFFSET_MAX_SPAN:
    case BOX_SIZE_MAX_SPAN:
        bt_ret = box_compute_max_span_size(box, bst);
        size = box->end_offset_max_span - box->start_offset_max_span;
        break ;
    case BOX_START_OFFSET_SLACK:
    case BOX_END_OFFSET_SLACK:
    case BOX_SIZE_SLACK:
        bt_ret = box_compute_slack_size(box, bst);
        size = box->end_offset_slack - box->start_offset_slack;
        break ;
    case BOX_START_OFFSET_PARENT:
    case BOX_END_OFFSET_PARENT:
    case BOX_SIZE_PARENT:
        bt_ret = BITPUNCH_OK;
        size = box->end_offset_parent - box->start_offset_parent;
        break ;
    case BOX_START_OFFSET_USED:
    case BOX_END_OFFSET_USED:
    case BOX_SIZE_USED:
        bt_ret = box_compute_used_size(box, bst);
        size = box->end_offset_used - box->start_offset_used;
        break ;
    default:
        return BITPUNCH_INVALID_PARAM;
    }
    if (BITPUNCH_OK == bt_ret && NULL != sizep) {
        *sizep = size;
    }
    return bt_ret;
}

static bitpunch_status_t
box_compute_offset_internal(struct box *box,
                            enum box_offset_type off_type,
                            int64_t *offsetp,
                            struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t offset;

    bt_ret = box_apply_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if ((0 == (box->flags & BOX_RALIGN)
         && 0 != (off_type & BOX_END_OFFSETS)) ||
        (0 != (box->flags & BOX_RALIGN)
         && 0 != (off_type & BOX_START_OFFSETS))) {
        bt_ret = box_compute_size_internal(box, off_type, NULL, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    offset = box_get_offset(box, off_type);
    assert(-1 != offset);
    if (NULL != offsetp) {
        *offsetp = offset;
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
box_get_location_internal(struct box *box,
                          int64_t *offsetp, int64_t *sizep,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = box_get_span_size(box, sizep, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_compute_offset_internal(box, BOX_START_OFFSET_SPAN,
                                       offsetp, bst);
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
        // FIXME we should pass a data source object from which to
        // read from, in addition to the lexical scope box
        b_item = &box->filter->ndat->u.rexpr_filter.f_instance->b_item;
        bt_ret = b_item->read_value(
            box->filter, box,
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

bitpunch_status_t
box_get_filtered_data_internal(
    struct box *box,
    struct bitpunch_data_source **dsp, int64_t *offsetp, int64_t *sizep,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t used_size;

    bt_ret = box_apply_filter_internal(box, bst);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = box_get_used_size(box, &used_size, bst);
    }
    if (BITPUNCH_OK == bt_ret) {
        assert(-1 != box->start_offset_used);
        *dsp = box->ds_out;
        *offsetp = box->start_offset_used;
        *sizep = used_size;
    }
    return bt_ret;
}

bitpunch_status_t
track_box_contents_internal(struct box *box,
                            struct tracker **tkp, struct browse_state *bst)
{
    *tkp = tracker_new(box);
    return BITPUNCH_OK;
}

struct tracker *
track_file(const struct bitpunch_schema *def_hdl,
           struct bitpunch_data_source *ds_in,
           struct tracker_error **errp)
{
    struct box *box;
    struct tracker *tk;
    bitpunch_status_t bt_ret;

    /* TODO issue warning if root node min span size > file length */
    box = box_new_from_file(def_hdl, ds_in);
    if (NULL == box) {
        return NULL;
    }
    bt_ret = track_box_contents(box, &tk, errp);
    box_delete_non_null(box);
    if (BITPUNCH_OK != bt_ret) {
        return NULL;
    }
    return tk;
}

bitpunch_status_t
track_item_contents_internal(struct tracker *tk,
                             struct tracker **tkp,
                             struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *filtered_box;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_get_filtered_item_box_internal(tk, &filtered_box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = track_box_contents_internal(filtered_box, tkp, bst);
    box_delete_non_null(filtered_box);
    return bt_ret;
}

bitpunch_status_t
tracker_compute_item_filter_internal(struct tracker *tk,
                                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    if (tracker_is_dangling(tk)) {
        return BITPUNCH_NO_ITEM;
    }
    if (NULL != tk->dpath.item) {
        return BITPUNCH_OK;
    }
    bt_ret = expr_evaluate_filter_type_internal(
        tk->dpath.filter, tk->box, FILTER_KIND_ITEM,
        &tk->dpath.item, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (NULL == tk->dpath.item) {
        return tracker_error(
            BITPUNCH_INVALID_PARAM, tk, tk->dpath.filter, bst,
            "cannot compute item filter: not an item type");
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
tracker_compute_item_offset(struct tracker *tk,
                            struct browse_state *bst)
{
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
    switch (tk->cur.type) {
    case TRACK_PATH_FIELD:
        return tracker_goto_field_internal(tk, tk->cur.u.field, TRUE, bst);
    case TRACK_PATH_ARRAY:
        return tracker_goto_ancestor_array_index_internal(
            tk, tk->cur.u.array.index, bst);
    case TRACK_PATH_NOTYPE:
    case TRACK_PATH_ARRAY_SLICE:
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
        if ((tk->box->flags & COMPUTING_SLACK_CHILD_ALLOCATION)) {
            max_offset = box_get_known_start_offset_mask(
                tk->box, (BOX_START_OFFSET_MAX_SPAN |
                          BOX_START_OFFSET_SLACK |
                          BOX_START_OFFSET_PARENT));
        } else {
            max_offset = box_get_known_start_offset(tk->box);
        }
    } else {
        if ((tk->box->flags & COMPUTING_SLACK_CHILD_ALLOCATION)) {
            max_offset = box_get_known_end_offset_mask(
                tk->box, (BOX_END_OFFSET_MAX_SPAN |
                          BOX_END_OFFSET_SLACK |
                          BOX_END_OFFSET_PARENT));
        } else {
            max_offset = box_get_known_end_offset(tk->box);
        }
    }
    if (-1 != tk->item_size) {
        item_size = tk->item_size;
    } else if (!tracker_is_dangling(tk)) {
        item_size = ast_node_get_min_span_size(tk->dpath.item);
    } else {
        item_size = 0;
    }
    if (-1 != tk->item_offset && -1 != max_offset) {
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
    case TRACK_PATH_FIELD:
        if (NULL != tk->cur.u.field) {
            tk->dpath.filter = tk->cur.u.field->filter;
            tk->dpath.item = NULL;
        } else {
            tracker_reset_dpath_internal(tk);
        }
        break ;
    case TRACK_PATH_ARRAY:
        assert(AST_NODE_TYPE_ARRAY == tk->box->filter->ndat->type);
        array = (struct filter_instance_array *)
            tk->box->filter->ndat->u.rexpr_filter.f_instance;
        tk->dpath.filter = array->item_type;
        tk->dpath.item = NULL;
        break ;
    default:
        tracker_reset_dpath_internal(tk);
        break ;
    }
}

bitpunch_status_t
tracker_goto_first_item_internal(struct tracker *tk,
                                 struct browse_state *bst)
{
    struct filter_instance *f_instance;
    bitpunch_status_t bt_ret;
    bitpunch_status_t bt_ret2;

    DBG_TRACKER_DUMP(tk);
    tracker_set_dangling(tk);
    tk->item_offset = -1;
    f_instance = tk->box->filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_first_item) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->filter, bst,
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
                tk->box->end_offset_span : tk->box->start_offset_span;
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

bitpunch_status_t
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
    f_instance = tk->box->filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_next_item) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->filter, bst,
            "filter does not implement goto_next_item() tracker backend function");
    }
    return f_instance->b_tk.goto_next_item(tk, bst);
}


bitpunch_status_t
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
    f_instance = tk->box->filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_nth_item) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->filter, bst,
            "filter does not implement goto_nth_item() tracker backend function");
    }
    return f_instance->b_tk.goto_nth_item(tk, index, bst);
}

bitpunch_status_t
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
    f_instance = tk->box->filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_nth_item) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->filter, bst,
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

bitpunch_status_t
tracker_goto_named_item_internal(struct tracker *tk, const char *name,
                                 struct browse_state *bst)
{
    struct filter_instance *f_instance;

    DBG_TRACKER_DUMP(tk);
    f_instance = tk->box->filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_named_item) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->filter, bst,
            "filter does not implement goto_named_item() tracker backend function");
    }
    return f_instance->b_tk.goto_named_item(tk, name, bst);
}


static void
tracker_set_field_internal(struct tracker *tk,
                           const struct field *field,
                           struct browse_state *bst)
{
    DPRINT("TK set field "ANSI_COLOR_GREEN"%s"ANSI_COLOR_RESET" on:\n",
           field->nstmt.name);
    DBG_TRACKER_DUMP(tk);
    tracker_set_dangling(tk);
    tk->cur = track_path_from_field(field);
    tk->dpath.filter = field->filter;
    DBG_TRACKER_CHECK_STATE(tk);
}

static bitpunch_status_t
tracker_goto_field_int_recur(struct tracker *tk,
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
        tracker_set_field_internal(tk, field, bst);
        return BITPUNCH_OK;
    }
    // recurse into anonymous field's scope
    xtk = tracker_dup(tk);
    do {
        tracker_set_field_internal(xtk, field, bst);
        bt_ret = tracker_enter_item_internal(xtk, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(xtk);
            return bt_ret;
        }
        if (0 != (tk->flags & TRACKER_REVERSED)) {
            stit = filter_riter_statements(
                xtk->box->filter, xtk->box, STATEMENT_TYPE_FIELD, NULL);
        } else {
            stit = filter_iter_statements(
                xtk->box->filter, xtk->box, STATEMENT_TYPE_FIELD, NULL);
        }
        bt_ret = scope_iter_statements_next_internal(&stit, NULL, &stmt, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(xtk);
            return bt_ret;
        }
        field = (const struct field *)stmt;
    } while (NULL == field->nstmt.name
             && 0 == (field->nstmt.stmt.stmt_flags & FIELD_FLAG_HIDDEN));
    tracker_set(tk, xtk);
    tracker_delete(xtk);
    tracker_set_field_internal(tk, field, bst);
    return BITPUNCH_OK;
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
        tracker_set_field_internal(tk, to_field, bst);
        return BITPUNCH_OK;
    }
    if (0 != (tk->box->flags & BOX_RALIGN)) {
        tracker_set_reversed =
            0 == (to_field->nstmt.stmt.stmt_flags & FIELD_FLAG_HEADER);
    } else {
        tracker_set_reversed =
            0 != (to_field->nstmt.stmt.stmt_flags & FIELD_FLAG_TRAILER);
    }
    reverse_direction =
        (tracker_set_reversed && 0 == (tk->flags & TRACKER_REVERSED))
        || (!tracker_set_reversed && 0 != (tk->flags & TRACKER_REVERSED));
    if (reverse_direction) {
        tk->flags ^= TRACKER_REVERSED;
    }
    bt_ret = tracker_goto_first_field_internal(tk, flat, bst);
    while (BITPUNCH_OK == bt_ret && tk->cur.u.field != to_field) {
        bt_ret = tracker_goto_next_field_internal(tk, flat, bst);
    }
    if (reverse_direction && BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_reverse_direction_internal(tk, bst);
    }
    return bt_ret;
}

static struct box *
box_get_container_parent_box(struct box *box)
{
    while (TRUE) {
        switch (box->filter->ndat->type) {
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

static int
tracker_in_anonymous_field(struct tracker *tk)
{
    struct box *composite_box;
    struct box *parent_box;

    composite_box = box_get_container_parent_box(tk->box);
    parent_box = composite_box->parent_box;
    return (NULL != parent_box
            && AST_NODE_TYPE_COMPOSITE == parent_box->filter->ndat->type
            && NULL != composite_box->track_path.u.field
            && NULL == composite_box->track_path.u.field->nstmt.name
            && 0 == (composite_box->track_path.u.field
                     ->nstmt.stmt.stmt_flags & FIELD_FLAG_HIDDEN));
}

bitpunch_status_t
tracker_goto_first_field_internal(struct tracker *tk, int flat,
                                  struct browse_state *bst)
{
    struct statement_iterator stit;
    const struct statement *stmt;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    if (!flat && NULL != tk->cur.u.field) {
        // return to base, non-anonymous level
        while (tracker_in_anonymous_field(tk)) {
            bt_ret = tracker_return_internal(tk, bst);
            assert(BITPUNCH_OK == bt_ret);
        }
    }
    if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        struct filter_instance *f_instance;

        f_instance = tk->box->filter->ndat->u.rexpr_filter.f_instance;
        if (NULL != f_instance->b_tk.init_item_offset) {
            bt_ret = f_instance->b_tk.init_item_offset(tk, bst);
            if (BITPUNCH_OK != bt_ret) {
                DBG_TRACKER_CHECK_STATE(tk);
                return bt_ret;
            }
            assert(tk->item_offset >= 0);
        }
    }
    if (0 != (tk->flags & TRACKER_REVERSED)) {
        stit = filter_riter_statements(
            tk->box->filter, tk->box, STATEMENT_TYPE_FIELD, NULL);
    } else {
        stit = filter_iter_statements(
            tk->box->filter, tk->box, STATEMENT_TYPE_FIELD, NULL);
    }
    bt_ret = scope_iter_statements_next_internal(&stit, NULL, &stmt, bst);
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
    return tracker_goto_field_int_recur(tk, (const struct field *)stmt,
                                        flat, bst);
}


bitpunch_status_t
tracker_goto_next_field_internal(struct tracker *tk, int flat,
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
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
            struct filter_instance *f_instance;

            assert(-1 != tk->item_offset);
            f_instance = tk->box->filter->ndat->u.rexpr_filter.f_instance;
            if (NULL != f_instance->b_tk.advance_item_offset) {
                bt_ret = f_instance->b_tk.advance_item_offset(tk, bst);
                if (BITPUNCH_OK != bt_ret) {
                    DBG_TRACKER_CHECK_STATE(tk);
                    return bt_ret;
                }
                assert(tk->item_offset >= 0);
            }
        }
        tracker_reset_item_cache(tk);
        if (reversed) {
            stit = filter_riter_statements_from(
                tk->box->filter, tk->box,
                (const struct statement *)tk->cur.u.field, NULL);
        } else {
            stit = filter_iter_statements_from(
                tk->box->filter, tk->box,
                (const struct statement *)tk->cur.u.field, NULL);
        }
        bt_ret = scope_iter_statements_next_internal(&stit, NULL, &stmt, bst);
        if (BITPUNCH_NO_ITEM != bt_ret) {
            break ;
        }
        if (!flat && tracker_in_anonymous_field(tk)) {
            // return from anonymous field's scope
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
    return tracker_goto_field_int_recur(tk, (const struct field *)stmt,
                                        flat, bst);
}


bitpunch_status_t
tracker_goto_first_item_with_key_internal(struct tracker *tk,
                                          expr_value_t item_key,
                                          struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_goto_nth_item_with_key_internal(tk, item_key, 0, bst);
}

bitpunch_status_t
tracker_goto_next_item_with_key_internal(struct tracker *tk,
                                         expr_value_t item_key,
                                         struct browse_state *bst)
{
    struct filter_instance *f_instance;

    DBG_TRACKER_DUMP(tk);
    if (tracker_is_dangling(tk)) {
        return tracker_goto_nth_item_with_key_internal(tk, item_key, 0, bst);
    }
    f_instance = tk->box->filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_next_item_with_key) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->filter, bst,
            "filter does not implement goto_next_item_with_key() tracker backend function");
    }
    return f_instance->b_tk.goto_next_item_with_key(
        tk, item_key, bst);
}

bitpunch_status_t
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
    f_instance = tk->box->filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_nth_item_with_key) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->filter, bst,
            "filter does not implement goto_nth_item_with_key() tracker backend function");
    }
    return f_instance->b_tk.goto_nth_item_with_key(
        tk, item_key, nth_twin, bst);
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
    while (0 == (root_box->filter->flags & ASTFLAG_IS_ROOT_BLOCK)) {
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
        tracker_set(tk, eval_dpath.tk);
        tracker_delete(eval_dpath.tk);
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        bt_ret = track_box_contents_internal(eval_dpath.box,
                                             &tk_tmp, bst);
        box_delete(eval_dpath.box);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        tracker_set(tk, tk_tmp);
        tracker_delete(tk_tmp);
        break ;
    default:
        assert(0);
    }
    // TODO free expr_node
    return BITPUNCH_OK;
}

bitpunch_status_t
tracker_goto_ancestor_array_index_internal(struct tracker *tk,
                                           int64_t index,
                                           struct browse_state *bst)
{
    struct box *orig_box;
    struct box *array_box;
    struct filter_instance *array_instance;
    bitpunch_status_t bt_ret;

    orig_box = tk->box;
    array_box = box_array_slice_get_ancestor_array(tk->box);
    array_instance = array_box->filter->ndat->u.rexpr_filter.f_instance;

    tk->box = array_box;
    bt_ret = array_instance->b_tk.goto_nth_item(tk, index, bst);
    tk->box = orig_box;
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_end_path(struct tracker *tk,
                      struct browse_state *bst)
{
    struct filter_instance *f_instance;

    DBG_TRACKER_DUMP(tk);
    f_instance = tk->box->filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.goto_end_path) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->filter, bst,
            "filter does not implement goto_end_path() tracker backend function");
    }
    return f_instance->b_tk.goto_end_path(tk, bst);
}

static bitpunch_status_t
tracker_goto_end_offset(struct tracker *tk,
                        struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        bitpunch_status_t bt_ret;

        bt_ret = box_compute_span_size(tk->box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        tk->item_offset = 0 != (tk->flags & TRACKER_REVERSED) ?
            tk->box->start_offset_span : tk->box->end_offset_span;
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
                            int allow_end_boundary,
                            int is_end_of_slice,
                            struct browse_state *bst)
{
    expr_value_t item_index;
    bitpunch_status_t bt_ret;

    if (NULL != index.key) {
        bt_ret = expr_evaluate_value_internal(index.key, NULL,
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

                // negative indices are interpreted as the number of
                // items to skip from the end of the array (with -1
                // pointing to the last item)
                bt_ret = tracker_get_n_items_internal(tk, &n_items, bst);
                if (BITPUNCH_OK != bt_ret) {
                    return bt_ret;
                }
                if (item_index.integer + n_items < 0) {
                    semantic_error(
                        SEMANTIC_LOGLEVEL_ERROR, &index.key->loc,
                        "index %"PRIi64" points outside %s of size %"PRIu64,
                        item_index.integer,
                        (ast_node_is_slice_container(tk->box->filter) ?
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
                bt_ret = expr_evaluate_value_internal(index.twin, NULL,
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
        // indices may be left unset in slices, which allow end
        // boundaries
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
        box_delete_non_null(tk->box);
        tk->box = filtered_box;
        tracker_rewind_internal(tk);
        bt_ret = box_apply_filter_internal(tk->box, bst);
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return bt_ret;
}

bitpunch_status_t
tracker_set_item_offset_at_box(struct tracker *tk,
                               struct box *box,
                               struct browse_state *bst)
{
    int tracker_is_reversed;
    int box_is_ralign;
    bitpunch_status_t bt_ret;

    tracker_is_reversed = 0 != (tk->flags & TRACKER_REVERSED);
    box_is_ralign = 0 != (box->flags & BOX_RALIGN);

    bt_ret = box_apply_parent_filter_internal(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (tracker_is_reversed != box_is_ralign) {
        bt_ret = box_compute_max_span_size(box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    if (tracker_is_reversed) {
        tk->item_offset = box_get_known_end_offset_mask(
            box, (BOX_END_OFFSET_SPAN |
                  BOX_END_OFFSET_MAX_SPAN));
    } else {
        tk->item_offset = box_get_known_start_offset_mask(
            box, (BOX_START_OFFSET_SPAN |
                  BOX_START_OFFSET_MAX_SPAN));
    }
    assert(-1 != tk->item_offset);
    return BITPUNCH_OK;
}

static void
tracker_return_from_slice(struct tracker *tk,
                          struct browse_state *bst)
{
    struct box *slice_box;

    slice_box = tk->box;
    tk->box = tk->box->parent_box;
    box_acquire(tk->box);
    box_delete_non_null(slice_box);
}

bitpunch_status_t
tracker_return_internal(struct tracker *tk,
                        struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct box *tracked_box;
    struct box *item_box;

    DBG_TRACKER_DUMP(tk);
    if (TRACK_PATH_ARRAY_SLICE == tk->box->track_path.type) {
        tracker_return_from_slice(tk, bst);
        DBG_TRACKER_CHECK_STATE(tk);
        return BITPUNCH_OK;
    }
    tracker_set_dangling(tk);
    tracked_box = tk->box;
    do {
        item_box = tracked_box;
        tracked_box = tracked_box->parent_box;
    } while (NULL != tracked_box &&
             !ast_node_is_trackable(tracked_box->filter));
    if (NULL == tracked_box) {
        return BITPUNCH_NO_ITEM;
    }
    if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        bt_ret = tracker_set_item_offset_at_box(tk, item_box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        if (-1 != item_box->start_offset_span
            && -1 != item_box->end_offset_span) {
            tk->item_size =
                item_box->end_offset_span - item_box->start_offset_span;
        } else {
            tk->item_size = -1;
        }
    }
    tk->box = item_box->parent_box;
    box_acquire(tk->box);
    tk->cur = item_box->track_path;
    tracker_set_dpath_from_cur_internal(tk);
    box_delete(item_box);
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
    case TRACK_PATH_FIELD:
        if (NULL == tp.u.field) {
            return snprintf(dpath_expr_buf, buf_size, ".<NOFIELD>");
        }
        if (dump_separator) {
            return snprintf(dpath_expr_buf, buf_size,
                            ".%s", tp.u.field->nstmt.name);
        } else {
            return snprintf(dpath_expr_buf, buf_size,
                            "%s", tp.u.field->nstmt.name);
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
    case TRACK_PATH_FIELD:
        if (NULL == tp.u.field) {
            return fprintf(stream, ".<NOFIELD>");
        }
        name = tp.u.field->nstmt.name;
        if (NULL != name) {
            return fprintf(stream, "%s%s",
                           (dump_separator ? "." : ""), name);
        } else {
            return fprintf(stream, "%s<ANON:%s>",
                           (dump_separator ? "." : ""),
                           ast_node_type_str(
                               tp.u.field->filter->ndat->type));
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
        0 == (box->parent_box->filter->flags & ASTFLAG_IS_ROOT_BLOCK),
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
    n_out += track_path_elem_dump_to_buf(
        tk->cur,
        /* dump separator? */
        0 == (tk->box->filter->flags & ASTFLAG_IS_ROOT_BLOCK),
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
    bitpunch_status_t bt_ret;
    int64_t max_span_offset;
    struct filter_instance *f_instance;

    bt_ret = tracker_compute_item_filter_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    f_instance = tk->dpath.item->ndat->u.rexpr_filter.f_instance;
    if (NULL != f_instance->b_tk.compute_item_size) {
        return f_instance->b_tk.compute_item_size(tk, item_sizep, bst);
    }
    if (0 != (tk->box->flags & (COMPUTING_SPAN_SIZE |
                                COMPUTING_SLACK_CHILD_ALLOCATION))) {
        if (0 != (tk->flags & TRACKER_REVERSED)) {
            max_span_offset = box_get_known_start_offset_mask(
                tk->box, (BOX_START_OFFSET_MAX_SPAN |
                          BOX_START_OFFSET_SLACK |
                          BOX_START_OFFSET_PARENT));
        } else {
            max_span_offset = box_get_known_end_offset_mask(
                tk->box, (BOX_END_OFFSET_MAX_SPAN |
                          BOX_END_OFFSET_SLACK |
                          BOX_END_OFFSET_PARENT));
        }
    } else {
        bt_ret = box_compute_max_span_size(tk->box, bst);
        if (BITPUNCH_OK == bt_ret) {
            bt_ret = box_get_slack_child_allocation(
                tk->box, 0 != (tk->flags & TRACKER_REVERSED),
                &max_span_offset, bst);
        }
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    f_instance = tk->dpath.item->ndat->u.rexpr_filter.f_instance;
    if (NULL != f_instance->b_item.compute_item_size) {
        return f_instance->b_item.compute_item_size(
            tk->dpath.item, tk->box,
            tk->item_offset, max_span_offset, item_sizep, bst);
    }
    /* use the whole available slack space when filter does not define
     * its size */
    *item_sizep = 0 != (tk->flags & TRACKER_REVERSED) ?
        tk->item_offset - max_span_offset :
        max_span_offset - tk->item_offset;
    assert(*item_sizep >= 0);
    return BITPUNCH_OK;
}

bitpunch_status_t
tracker_compute_item_size(struct tracker *tk,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t item_size;

    DBG_TRACKER_DUMP(tk);
    assert(-1 != tk->item_offset);
    if (tracker_is_dangling(tk)) {
        return BITPUNCH_NO_ITEM;
    }
    bt_ret = tracker_compute_item_size_internal(tk, &item_size, bst);
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
    // FIXME tracker_compute_item_size_internal() also calls this,
    // seems redundant
    bt_ret = tracker_compute_item_filter_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (0 == (tk->dpath.item->ndat->u.item.flags
              & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
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

bitpunch_status_t
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

bitpunch_status_t
tracker_get_item_key_internal(struct tracker *tk,
                              expr_value_t *keyp,
                              struct browse_state *bst)
{
    struct filter_instance *f_instance;

    DBG_TRACKER_DUMP(tk);
    if (tracker_is_dangling(tk)) {
        return BITPUNCH_NO_ITEM;
    }
    f_instance = tk->box->filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.get_item_key) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->filter, bst,
            "filter does not implement get_item_key() tracker backend function");
    }
    return f_instance->b_tk.get_item_key(
        tk, keyp, NULL, bst);
}

bitpunch_status_t
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
    f_instance = tk->box->filter->ndat->u.rexpr_filter.f_instance;
    if (NULL == f_instance->b_tk.get_item_key) {
        return tracker_error(
            BITPUNCH_NOT_IMPLEMENTED, tk, tk->box->filter, bst,
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
        *item_contentsp = tk->box->ds_out->ds_data + tk->item_offset;
    }
    if (NULL != item_sizep) {
        *item_sizep = tk->item_size;
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
tracker_get_filtered_data_internal(
    struct tracker *tk,
    struct bitpunch_data_source **dsp, int64_t *offsetp, int64_t *sizep,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    expr_dpath_t filtered_dpath;

    bt_ret = tracker_get_filtered_dpath_internal(tk, &filtered_dpath, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    switch (filtered_dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        bt_ret = box_apply_filter_internal(filtered_dpath.tk->box, bst);
        if (BITPUNCH_OK == bt_ret) {
            bt_ret = tracker_get_item_location_internal(
                filtered_dpath.tk, offsetp, sizep, bst);
        }
        if (BITPUNCH_OK == bt_ret) {
            *dsp = filtered_dpath.tk->box->ds_out;
        }
        break ;
    case EXPR_DPATH_TYPE_CONTAINER:
        bt_ret = box_get_filtered_data_internal(filtered_dpath.box,
                                                dsp, offsetp, sizep, bst);
        break ;
    default:
        assert(0);
    }
    expr_dpath_destroy(filtered_dpath);
    return bt_ret;
}

static bitpunch_status_t
filtered_dpath_read_value_internal(expr_dpath_t dpath,
                                   expr_value_t *expr_valuep,
                                   struct browse_state *bst)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return tracker_read_item_value_direct_internal(dpath.tk,
                                                       expr_valuep, bst);
    case EXPR_DPATH_TYPE_CONTAINER:
        return box_read_value_internal(dpath.box,
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
            assert(-1 == tk->box->start_offset_span
                   || tk->item_offset - tk->item_size
                   >= tk->box->start_offset_span);
            tk->item_offset -= tk->item_size;
        } else {
            assert(-1 == tk->box->end_offset_span
                   || tk->item_offset + tk->item_size
                   <= tk->box->end_offset_span);
            tk->item_offset += tk->item_size;
        }
    }
    tk->flags ^= TRACKER_REVERSED;
    return BITPUNCH_OK;
}

/*
 * tracker error management
 */

void
tracker_error_init(struct tracker_error *tk_err,
                   bitpunch_status_t bt_ret)
{
    memset(tk_err, 0, sizeof (*tk_err));
    tk_err->bt_ret = bt_ret;
    tk_err->error_buf_end = tk_err->error_buf;
    tk_err->error_buf[0] = '\0';
}

struct tracker_error *
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
                     box->start_offset_span,
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
        tk->box->start_offset_span, box_get_known_end_offset(tk->box),
        item_span_msg);
    tk_err = bst->last_error;
    assert(NULL != tk_err);
    tk_err->u.out_of_bounds.registered_offset_type =
        box_get_known_end_offset_type(tk->box);
    tk_err->u.out_of_bounds.registered_offset =
        box_get_known_end_offset(tk->box);
    tk_err->u.out_of_bounds.requested_offset_type =
        BOX_END_OFFSET_SPAN;
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

bitpunch_status_t
box_compute__error(struct box *box,
                   struct browse_state *bst)
{
    // TODO more precise error
    return box_error(BITPUNCH_DATA_ERROR, box, box->filter, bst,
                     "invalid filter operation requested");
}


bitpunch_status_t
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


bitpunch_status_t
tracker_goto_next_item_with_key__default(struct tracker *tk,
                                         expr_value_t item_key,
                                         struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return BITPUNCH_NO_ITEM;
}

bitpunch_status_t
tracker_goto_nth_item_with_key__default(struct tracker *tk,
                                        expr_value_t item_key,
                                        int nth_twin,
                                        struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return BITPUNCH_NO_ITEM;
}


bitpunch_status_t
tracker_goto_next_item_with_key__not_impl(struct tracker *tk,
                                          expr_value_t item_key,
                                          struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst, NULL);
}

bitpunch_status_t
tracker_goto_nth_item_with_key__not_impl(
    struct tracker *tk, expr_value_t item_key, int nth_twin,
    struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst, NULL);
}


bitpunch_status_t
box_get_n_items__as_used(struct box *box, int64_t *item_countp,
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

    browse_state_init_dpath(&bst, src_dpath);
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

    browse_state_init_dpath(&bst, dpath);
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

    browse_state_init_dpath(&bst, dpath);
    return transmit_error(
        expr_dpath_get_location_internal(dpath, offsetp, sizep, &bst),
        &bst, errp);
}

bitpunch_status_t
expr_dpath_get_filtered_data(
    expr_dpath_t dpath,
    struct bitpunch_data_source **dsp, int64_t *offsetp, int64_t *sizep,
    struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_dpath(&bst, dpath);
    return transmit_error(
        expr_dpath_get_filtered_data_internal(dpath, dsp, offsetp, sizep, &bst),
        &bst, errp);
}

bitpunch_status_t
box_get_n_items(struct box *box, int64_t *n_itemsp,
                struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_box(&bst, box);
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

    browse_state_init_box(&bst, box);
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

    browse_state_init_box(&bst, box);
    return transmit_error(
        box_read_value_internal(box, valuep, &bst),
        &bst, errp);
}

bitpunch_status_t
box_compute_offset(struct box *box,
                   enum box_offset_type off_type,
                   int64_t *offsetp,
                   struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_box(&bst, box);
    return transmit_error(
        box_compute_offset_internal(box, off_type, offsetp, &bst),
        &bst, errp);
}

bitpunch_status_t
box_compute_size(struct box *box,
                 enum box_offset_type size_type,
                 int64_t *sizep,
                 struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_box(&bst, box);
    return transmit_error(
        box_compute_size_internal(box, size_type, sizep, &bst),
        &bst, errp);
}

bitpunch_status_t
box_apply_filter(struct box *box,
                 struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_box(&bst, box);
    return transmit_error(
        box_apply_filter_internal(box, &bst), &bst, errp);
}

bitpunch_status_t
track_item_contents(struct tracker *tk,
                    struct tracker **tkp,
                    struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_tracker(&bst, tk);
    return transmit_error(
        track_item_contents_internal(tk, tkp, &bst),
        &bst, errp);
}

bitpunch_status_t
track_dpath_contents_internal(expr_dpath_t dpath,
                              struct tracker **tkp,
                              struct browse_state *bst)
{
    switch (dpath.type) {
    case EXPR_DPATH_TYPE_ITEM:
        return track_item_contents_internal(dpath.tk, tkp, bst);

    case EXPR_DPATH_TYPE_CONTAINER:
        return track_box_contents_internal(dpath.box, tkp, bst);

    default:
        assert(0);
    }
}

bitpunch_status_t
track_dpath_contents(expr_dpath_t dpath,
                     struct tracker **tkp,
                     struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_dpath(&bst, dpath);
    return transmit_error(
        track_dpath_contents_internal(dpath, tkp, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_get_n_items(struct tracker *tk, int64_t *item_countp,
                    struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_tracker(&bst, tk);
    return transmit_error(
        tracker_get_n_items_internal(tk, item_countp, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_first_item(struct tracker *tk,
                        struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_tracker(&bst, tk);
    return transmit_error(
        tracker_goto_first_item_internal(tk, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_next_item(struct tracker *tk,
                       struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_tracker(&bst, tk);
    return transmit_error(
        tracker_goto_next_item_internal(tk, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_nth_item(struct tracker *tk, int64_t index,
                      struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_tracker(&bst, tk);
    return transmit_error(
        tracker_goto_nth_item_internal(tk, index, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_nth_position(struct tracker *tk, int64_t index,
                          struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_tracker(&bst, tk);
    return transmit_error(
        tracker_goto_nth_position_internal(tk, index, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_named_item(struct tracker *tk, const char *name,
                        struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_tracker(&bst, tk);
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

    browse_state_init_tracker(&bst, tk);
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

    browse_state_init_tracker(&bst, tk);
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

    browse_state_init_tracker(&bst, tk);
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

    browse_state_init_tracker(&bst, tk);
    return transmit_error(
        tracker_goto_abs_dpath_internal(tk, dpath_expr, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_goto_end(struct tracker *tk,
                 struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_tracker(&bst, tk);
    return transmit_error(
        tracker_goto_end_internal(tk, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_enter_item(struct tracker *tk,
                   struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_tracker(&bst, tk);
    return transmit_error(
        tracker_enter_item_internal(tk, &bst),
        &bst, errp);
}


bitpunch_status_t
tracker_return(struct tracker *tk,
               struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_tracker(&bst, tk);
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

    browse_state_init_tracker(&bst, tk);
    return transmit_error(
        tracker_get_item_filter_internal(tk, item_filterp, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_get_item_offset(struct tracker *tk, int64_t *item_offsetp,
                        struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_tracker(&bst, tk);
    return transmit_error(
        tracker_get_item_offset_internal(tk, item_offsetp, &bst),
        &bst, errp);
}


bitpunch_status_t
tracker_get_item_size(struct tracker *tk, int64_t *item_sizep,
                      struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_tracker(&bst, tk);
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

    browse_state_init_tracker(&bst, tk);
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

    browse_state_init_tracker(&bst, tk);
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

    browse_state_init_tracker(&bst, tk);
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

    browse_state_init_tracker(&bst, tk);
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

    browse_state_init_tracker(&bst, tk);
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

    browse_state_init_tracker(&bst, tk);
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

    browse_state_init_tracker(&bst, tk);
    return transmit_error(
        tracker_get_filtered_item_box_internal(tk, filtered_boxp, &bst),
        &bst, errp);
}

bitpunch_status_t
track_box_contents(struct box *box,
                   struct tracker **tkp,
                   struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init_box(&bst, box);
    return transmit_error(
        track_box_contents_internal(box, tkp, &bst), &bst, errp);
}


/*
 *
 */
