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
#include "core/interpreter.h"
#include "core/print.h"
#include "core/browse_internal.h"
#include "core/expr_internal.h"
#include "core/debug.h"

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

static const char *
box_offset_type_str(enum box_offset_type type);

static void
box_delete_non_null(struct box *box);

static bitpunch_status_t
box_iter_statements_next_internal(struct box *box,
                                  struct statement_iterator *it,
                                  const struct statement **stmtp,
                                  struct browse_state *bst);

static bitpunch_status_t
box_lookup_statement_internal(struct box *box,
                              enum statement_type stmt_type,
                              const char *stmt_name,
                              const struct named_statement **stmtp,
                              struct browse_state *bst);

static bitpunch_status_t
box_get_first_statement_internal(struct box *box,
                                 enum statement_type stmt_type,
                                 int stmt_flags,
                                 const struct statement **stmtp,
                                 struct browse_state *bst);

static bitpunch_status_t
box_get_n_statements_internal(struct box *box,
                              enum statement_type stmt_type,
                              int stmt_flags,
                              int64_t *stmt_countp,
                              struct browse_state *bst);

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
box_get_children_slack(struct box *box, int64_t *max_slack_offsetp,
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
tracker_index_cache_add_item(struct tracker *tk, union expr_value item_key);
static void
tracker_goto_last_cached_item_internal(struct tracker *tk,
                                       struct browse_state *bst);
static bitpunch_status_t
tracker_goto_next_key_match_in_mark(struct tracker *tk,
                                    union expr_value key,
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
                                          union expr_value item_key,
                                          struct browse_state *bst);
static bitpunch_status_t
tracker_goto_next_item_with_key_internal(struct tracker *tk,
                                         union expr_value item_key,
                                         struct browse_state *bst);
static bitpunch_status_t
tracker_goto_nth_item_with_key_internal(struct tracker *tk,
                                        union expr_value item_key,
                                        int nth_twin,
                                        struct browse_state *bst);

static bitpunch_status_t
tracker_goto_first_item_int__block(struct tracker *tk, int flat,
                                   struct browse_state *bst);
static bitpunch_status_t
tracker_goto_next_item_int__block(struct tracker *tk, int flat,
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
 * box
 */

static int64_t
box_get_offset(struct box *box, enum box_offset_type type)
{
    switch (type) {
    case BOX_START_OFFSET_USED:
        return box->start_offset_used;
    case BOX_END_OFFSET_HARD_MIN:
        return box->start_offset_used +
            ast_node_get_min_span_size(box->dpath.item);
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

static bitpunch_status_t
box_check_start_offset(struct box *box, int64_t start_offset,
                       struct browse_state *bst)
{
    if (start_offset < box->start_offset_used) {
        return box_error_out_of_bounds(box, NULL,
                                       BOX_START_OFFSET_USED,
                                       start_offset,
                                       BOX_START_OFFSET_USED, bst);
    }
    return BITPUNCH_OK;
}


static bitpunch_status_t
box_check_end_offset(struct box *box, int64_t end_offset,
                     enum box_offset_type type,
                     struct browse_state *bst)
{
    switch (type) {
    case BOX_END_OFFSET_MIN_SPAN:
        if (box->end_offset_used >= 0) {
            if (end_offset > box->end_offset_used) {
                return box_error_out_of_bounds(box, NULL,
                                               BOX_END_OFFSET_MIN_SPAN,
                                               end_offset,
                                               BOX_END_OFFSET_USED, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_USED:
        if (box->end_offset_max_span >= 0) {
            if (end_offset > box->end_offset_max_span) {
                return box_error_out_of_bounds(box, NULL,
                                               BOX_END_OFFSET_USED,
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
                return box_error_out_of_bounds(box, NULL,
                                               BOX_END_OFFSET_SLACK,
                                               end_offset,
                                               BOX_END_OFFSET_PARENT, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_PARENT:
    case BOX_END_OFFSET_MAX_SPAN:
        break ;
    case BOX_END_OFFSET_HARD_MIN:
    default:
        assert(0);
    }

    switch (type) {
    case BOX_END_OFFSET_PARENT:
        if (box->end_offset_slack >= 0) {
            if (end_offset < box->end_offset_slack) {
                return box_error_out_of_bounds(box, NULL,
                                               BOX_END_OFFSET_PARENT,
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
                return box_error_out_of_bounds(box, NULL,
                                               BOX_END_OFFSET_MAX_SPAN,
                                               end_offset,
                                               BOX_END_OFFSET_USED, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_USED:
        if (box->end_offset_min_span >= 0) {
            if (end_offset < box->end_offset_min_span) {
                return box_error_out_of_bounds(box, NULL,
                                               BOX_END_OFFSET_USED,
                                               end_offset,
                                               BOX_END_OFFSET_MIN_SPAN, bst);
            }
            break ;
        }
        /*FALLTHROUGH*/
    case BOX_END_OFFSET_MIN_SPAN:
        break ;
    case BOX_END_OFFSET_HARD_MIN:
    default:
        /* cannot set those offsets */
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
            ast_node_get_min_span_size(box->dpath.item);
        box->end_offset_min_span = MAX(end_offset, end_offset_hard_min);
        break ;
    }
    case BOX_END_OFFSET_HARD_MIN:
    default:
        assert(0);
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_set_start_offset(struct box *box, int64_t start_offset,
                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = box_check_start_offset(box, start_offset, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    box->start_offset_used = start_offset;
    return BITPUNCH_OK;
}

static const char *
box_offset_type_str(enum box_offset_type type)
{
    switch (type) {
    case BOX_START_OFFSET_USED:
        return "start";
    case BOX_END_OFFSET_HARD_MIN:
        return "hard min";
    case BOX_END_OFFSET_PARENT:
        return "from parent";
    case BOX_END_OFFSET_SLACK:
        return "available";
    case BOX_END_OFFSET_MAX_SPAN:
        return "slack";
    case BOX_END_OFFSET_USED:
        return "used";
    case BOX_END_OFFSET_MIN_SPAN:
        return "min span";
    default:
        return "(bad offset type)";
    }
}

static bitpunch_status_t
box_set_size(struct box *box, int64_t box_size,
             enum box_offset_type off_type,
             struct browse_state *bst)
{
    if (0 != (box->flags & BOX_REVERSED)) {
        int64_t end_offset;
        bitpunch_status_t bt_ret;

        // FIXME: off_type is ignored now, box_offset should be split
        // into multiple fields similarly to end offsets to have more
        // flexibility on reversed boxes
        end_offset = box_get_known_end_offset(box);
        assert(-1 != end_offset);
        bt_ret = box_set_start_offset(box, end_offset - box_size, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    if (-1 == box_get_offset(box, off_type)) {
        return box_set_end_offset(box, box->start_offset_used + box_size,
                                  off_type, bst);
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_set_min_span_size(struct box *box, int64_t min_span_size,
                      struct browse_state *bst)
{
    return box_set_size(box, min_span_size, BOX_END_OFFSET_MIN_SPAN, bst);
}

static bitpunch_status_t
box_set_used_size(struct box *box, int64_t used_size,
                  struct browse_state *bst)
{
    return box_set_size(box, used_size, BOX_END_OFFSET_USED, bst);
}

static bitpunch_status_t
box_set_max_span_size(struct box *box, int64_t max_span_size,
                      struct browse_state *bst)
{
    return box_set_size(box, max_span_size, BOX_END_OFFSET_MAX_SPAN, bst);
}


static bitpunch_status_t
box_construct(struct box *o_box,
              struct box *parent_box,
              const struct dpath_node *dpath,
              int64_t start_offset_used,
              enum box_flag box_flags,
              struct browse_state *bst)
{
    int64_t end_offset_hard_min;
    int out_of_bounds;

    if (NULL != parent_box
        && parent_box->depth_level == BOX_MAX_DEPTH_LEVEL) {
        semantic_error(SEMANTIC_LOGLEVEL_ERROR, &dpath->item->loc,
                       "reached maximum box nesting level %d",
                       BOX_MAX_DEPTH_LEVEL);
        return BITPUNCH_DATA_ERROR;
    }
    o_box->dpath = *dpath;
    if (NULL != parent_box) {
        assert(parent_box != o_box);
        o_box->parent_box = parent_box;
        o_box->file_hdl = parent_box->file_hdl;
        o_box->depth_level = parent_box->depth_level + 1;
        box_acquire(parent_box);
        o_box->end_offset_parent = box_get_known_end_offset(parent_box);
    } else {
        o_box->end_offset_parent = -1; /* unknown */
    }
    o_box->end_offset_slack = -1; /* unknown */
    o_box->end_offset_max_span = -1; /* unknown */
    o_box->end_offset_used = -1; /* unknown */
    o_box->end_offset_min_span = -1; /* unknown */
    if (0 == (dpath->u.item.flags & ITEMFLAG_IS_USED_SIZE_DYNAMIC)) {
        /* TODO issue warning */
    }
    o_box->flags = box_flags;
    if (0 != (box_flags & BOX_REVERSED)) {
        assert(NULL != parent_box);
        o_box->start_offset_used = parent_box->start_offset_used;
        o_box->end_offset_max_span = start_offset_used;
        end_offset_hard_min =
            start_offset_used - ast_node_get_min_span_size(dpath->item);
        out_of_bounds =
            -1 != o_box->end_offset_parent
            && end_offset_hard_min < o_box->start_offset_used;
    } else {
        o_box->start_offset_used = start_offset_used;
        end_offset_hard_min =
            start_offset_used + ast_node_get_min_span_size(dpath->item);
        out_of_bounds =
            -1 != o_box->end_offset_parent
            && end_offset_hard_min > o_box->end_offset_parent;
    }
    if (out_of_bounds) {
        assert(NULL != parent_box);
        return box_error_out_of_bounds(parent_box, NULL,
                                       BOX_END_OFFSET_HARD_MIN,
                                       end_offset_hard_min,
                                       box_get_known_end_offset_type(
                                           parent_box), bst);
    }
    TAILQ_INIT(&o_box->cached_children);
    o_box->use_count = 1;

    /* initialize internal state */
    switch (dpath->item->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        break ;
    case AST_NODE_TYPE_ARRAY:
        o_box->u.array_generic.n_items = -1;
        o_box->u.array.last_cached_index = -1;
        o_box->u.array.last_cached_item_offset = -1;
        if (0 != ((ast_node_get_target_item(
                       dpath->item->u.array.item_type.item)->u.item.flags)
                  & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            box_init_mark_offsets_repo(o_box);
            o_box->u.array.cache_log2_n_keys_per_mark =
                BOX_INDEX_CACHE_DEFAULT_LOG2_N_KEYS_PER_MARK;
        }
        if (ast_node_is_indexed(dpath->item)) {
            box_init_index_cache_by_key(o_box);
            o_box->u.array.cache_log2_n_keys_per_mark =
                log2_i(bloom_book_suggested_n_words_per_mark(
                           o_box->u.array.cache_by_key));
        }
        break ;
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_FILTERED:
        o_box->u.array_generic.n_items = -1;
        break ;
    default:
        assert(0);
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
        "BOX_REVERSED",
        "BOX_FILTER",
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
            ": [[[[[%"PRIi64"..%"PRIi64"(minspan)]..%"PRIi64"(used)]"
            "..%"PRIi64"(maxspan)]..%"PRIi64"(slack)]"
            "..%"PRIi64"(parent)]\n",
            box->start_offset_used,
            box->end_offset_min_span, box->end_offset_used,
            box->end_offset_max_span, box->end_offset_slack,
            box->end_offset_parent);
    fprintf(out,
            "%*stype: %s ftype: %s flags: ",
            (indent + box->depth_level) * 4, "",
            ast_node_type_str(box->dpath.item->type),
            (NULL != box->dpath.filter ?
             ast_node_type_str(box->dpath.filter->type) : "N/A"));
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

static void
box_update_cache(struct box *box, struct browse_state *bst)
{
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
}

static struct box *
box_lookup_cached_child(struct box *box, struct track_path path,
                        enum box_flag flags)
{
    struct box *child_box;

    DBG_BOX_DUMP(box);
    TAILQ_FOREACH(child_box, &box->cached_children, cached_children_list) {
        if (track_path_eq(child_box->track_path, path)
            && (child_box->flags & BOX_REVERSED) == (flags & BOX_REVERSED)) {
            return child_box;
        }
    }
    return NULL;
}

static struct box *
box_new_from_file_internal(const struct bitpunch_schema_hdl *def_hdl,
                           struct bitpunch_binary_file_hdl *file_hdl,
                           struct browse_state *bst)
{
    struct box *box;
    const struct dpath_node *root;
    bitpunch_status_t bt_ret;

    root = def_hdl->df_file_block.root;
    assert(NULL != root);
    box = new_safe(struct box);
    bt_ret = box_construct(box, NULL, root, 0, 0u, bst);
    if (BITPUNCH_OK != bt_ret) {
        /* TODO error reporting */
        free(box);
        return NULL;
    }
    box->file_hdl = file_hdl;
    bt_ret = box_set_end_offset(box, file_hdl->bf_data_length,
                                BOX_END_OFFSET_SLACK, bst);
    if (BITPUNCH_OK != bt_ret) {
        /* TODO error reporting */
        free(box);
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
    struct dpath_node *slice_node;
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

    switch (slice_start->box->dpath.item->type) {
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
        slice_node = DPATH_NODE_ARRAY_SLICE;
        break ;
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_FILTERED:
        slice_node = DPATH_NODE_BYTE_SLICE;
        break ;
    default:
        (void) tracker_error(BITPUNCH_INVALID_PARAM, slice_start, NULL, bst,
                             "can't set tracker slice: does not track "
                             "an array");
        return NULL;
    }
    index_start = slice_start->cur.u.array.index;
    index_end = (NULL != slice_end ? slice_end->cur.u.array.index : -1);

    slice_path.type = TRACK_PATH_ARRAY_SLICE;
    if (-1 == index_start) {
        index_start = 0;
    }
    slice_path.u.array.index = index_start;

    if (index_end != -1 && index_end < index_start) {
        (void) tracker_error(BITPUNCH_DATA_ERROR, slice_start, NULL,
                             bst,
                             "slice end index %"PRIi64" before "
                             "start index %"PRIi64"",
                             index_end, index_start);
        return NULL;
    }
    slice_path.u.array_slice.index_end = index_end;

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
                           slice_start->box, slice_node,
                           slice_start_offset_used, 0u, bst);
    if (BITPUNCH_OK != bt_ret) {
        free(slice_box);
        DBG_TRACKER_CHECK_STATE(slice_start);
        return NULL;
    }
    slice_box->track_path = slice_path;
    if (0 != (TRACKER_AT_END & slice_start->flags)) {
        slice_box->u.array_generic.n_items = 0;
    }
    return slice_box;
}

static struct box *
box_new_bytes_box_internal(struct box *parent_box,
                           int64_t start_offset_used,
                           int64_t box_size,
                           struct browse_state *bst)
{
    struct box *bytes_box;
    int64_t index_start;
    int64_t index_end;
    bitpunch_status_t bt_ret;

    bytes_box = new_safe(struct box);
    bt_ret = box_construct(bytes_box, parent_box,
                           DPATH_NODE_AS_BYTES,
                           start_offset_used, 0u, bst);
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = box_set_used_size(bytes_box, box_size, bst);
        if (BITPUNCH_OK != bt_ret) {
            box_delete_non_null(bytes_box);
            return NULL;
        }
    } else {
        free(bytes_box);
        return NULL;
    }
    index_start = start_offset_used - parent_box->start_offset_used;
    index_end = index_start + box_size;
    bytes_box->track_path = track_path_from_array_slice(index_start,
                                                        index_end);
    return bytes_box;
}

struct box *
box_new_bytes_box_from_item(struct tracker *tk, struct browse_state *bst)
{
    int64_t box_size;

    if (NULL == tk->dpath) {
        return NULL;
    }
    if (BITPUNCH_OK != tracker_get_item_size_internal(tk, &box_size, bst)) {
        return NULL;
    }
    if (BITPUNCH_OK != tracker_create_item_box_internal(tk, bst)) {
        return NULL;
    }
    return box_new_bytes_box_internal(tk->item_box,
                                      tk->item_offset, box_size, bst);
}

struct box *
box_new_bytes_box_from_box(struct box *box, struct browse_state *bst)
{
    int64_t box_size;

    if (BITPUNCH_OK != box_compute_used_size(box, bst)) {
        return NULL;
    }
    box_size = box->end_offset_used - box->start_offset_used;
    return box_new_bytes_box_internal(box, box->start_offset_used, box_size, bst);
}

struct box *
box_new_as_box(struct box *parent_box,
               struct dpath_node *as_dpath, int64_t start_offset_used,
               struct browse_state *bst)
{
    struct box *as_box;
    bitpunch_status_t bt_ret;

    if (BITPUNCH_OK != box_compute_max_span_size(parent_box, bst)) {
        return NULL;
    }
    as_box = new_safe(struct box);
    bt_ret = box_construct(as_box, parent_box,
                           as_dpath, start_offset_used, 0u, bst);
    if (BITPUNCH_OK != bt_ret) {
        free(as_box);
        return NULL;
    }
    as_box->track_path = TRACK_PATH_NONE;

    // limit the available space to the slack size of the parent
    bt_ret = box_set_end_offset(as_box,
                                box_get_known_end_offset(parent_box),
                                BOX_END_OFFSET_SLACK, bst);
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
box_new_filter_box(struct box *unfiltered_box,
                   const struct ast_node *filter,
                   const char *filtered_data,
                   size_t filtered_size,
                   int own_buffer,
                   struct browse_state *bst)
{
    struct bitpunch_binary_file_hdl *filtered_data_hdl;
    struct box *box;
    bitpunch_status_t bt_ret;

    box = new_safe(struct box);
    bt_ret = box_construct(box, NULL, DPATH_NODE_FILTERED, 0,
                           BOX_FILTER, bst);
    if (BITPUNCH_OK != bt_ret) {
        free(box);
        return NULL;
    }
    box->unfiltered_box = unfiltered_box;
    box_acquire(unfiltered_box);
    filtered_data_hdl = create_data_hdl_from_buffer(
        filtered_data, filtered_size, own_buffer,
        unfiltered_box->file_hdl);
    box->file_hdl = filtered_data_hdl;
    box->end_offset_slack = (int64_t)filtered_size;
    box->end_offset_max_span = (int64_t)filtered_size;
    box->end_offset_used = (int64_t)filtered_size;
    return box;
}

static bitpunch_status_t
box_apply_filter_as_type(struct box *unfiltered_box,
                         const struct ast_node *filter,
                         struct box **filtered_boxp,
                         struct browse_state *bst)
{
    const struct dpath_node *as_type_dpath;
    struct box *as_type_box;
    bitpunch_status_t bt_ret;

    bt_ret = box_compute_max_span_size(unfiltered_box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    as_type_dpath = &filter->u.rexpr_filter.filter_dpath;
    if (! ast_node_is_container(as_type_dpath->item)) {
        return box_error(BITPUNCH_NOT_CONTAINER,
                         unfiltered_box, as_type_dpath->item, bst,
                         "item type '%s' is not a container",
                         ast_node_type_str(as_type_dpath->item->type));
    }
    as_type_box = new_safe(struct box);
    bt_ret = box_construct(as_type_box, unfiltered_box, as_type_dpath,
                           unfiltered_box->start_offset_used,
                           (BOX_FILTER |
                            (unfiltered_box->flags & BOX_REVERSED)), bst);
    if (BITPUNCH_OK != bt_ret) {
        free(as_type_box);
        return bt_ret;
    }
    *filtered_boxp = as_type_box;
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_apply_filter_interpreter(struct box *unfiltered_box,
                             const struct ast_node *filter,
                             struct box **filtered_boxp,
                             struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t unfiltered_size;
    enum expr_value_type filtered_value_type;
    union expr_value filtered_value;
    const char *item_data;

    bt_ret = box_get_used_size(unfiltered_box, &unfiltered_size, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }

    item_data = (unfiltered_box->file_hdl->bf_data +
                 unfiltered_box->start_offset_used);
    bt_ret = interpreter_rcall_read_value(filter, item_data,
                                          unfiltered_size,
                                          &filtered_value_type,
                                          &filtered_value, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    switch (filtered_value_type) {
    case EXPR_VALUE_TYPE_BYTES:
        *filtered_boxp = box_new_filter_box(unfiltered_box, filter,
                                            filtered_value.bytes.buf,
                                            filtered_value.bytes.len,
                                            TRUE, bst);
        break ;
    case EXPR_VALUE_TYPE_STRING:
        *filtered_boxp = box_new_filter_box(unfiltered_box, filter,
                                            filtered_value.string.str,
                                            filtered_value.string.len,
                                            FALSE, bst);
        break ;
    default:
        return box_error(BITPUNCH_INVALID_PARAM,
                         unfiltered_box, filter, bst,
                         "cannot create filter box from filtered type %s",
                         expr_value_type_str(filtered_value_type));
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_apply_filter(struct box *unfiltered_box,
                 const struct ast_node *filter,
                 struct box **filtered_boxp,
                 struct browse_state *bst)
{
    struct ast_node *target_filter;
    struct box *target_box;
    struct box *filtered_box;
    bitpunch_status_t bt_ret;

    target_filter =
        ast_node_get_named_expr_target(filter->u.rexpr_filter.target);
    if (ast_node_is_filter(target_filter)) {
        bt_ret = box_apply_filter(unfiltered_box, target_filter,
                                  &target_box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    } else {
        target_box = unfiltered_box;
        box_acquire(target_box);
    }
    switch (filter->type) {
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        bt_ret = box_apply_filter_as_type(target_box, filter,
                                          &filtered_box, bst);
        break ;
    case AST_NODE_TYPE_REXPR_INTERPRETER:
        bt_ret = box_apply_filter_interpreter(target_box, filter,
                                              &filtered_box, bst);
        break ;
    default:
        assert(0);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    *filtered_boxp = filtered_box;
    box_delete(target_box);
    return BITPUNCH_OK;
}

bitpunch_status_t
box_apply_filters(struct box *unfiltered_box,
                  struct box **filtered_boxp,
                  struct browse_state *bst)
{
    const struct ast_node *filter;

    filter = unfiltered_box->dpath.filter;
    if (NULL == filter) {
        box_acquire(unfiltered_box);
        *filtered_boxp = unfiltered_box;
        return BITPUNCH_OK;
    }
    return box_apply_filter(unfiltered_box, filter, filtered_boxp, bst);
}

static struct box *
box_get_unfiltered_parent(struct box *box)
{
    while (0 != (box->flags & BOX_FILTER)) {
        if (NULL != box->parent_box) {
            box = box->parent_box;
        } else {
            assert(NULL != box->unfiltered_box);
            box = box->unfiltered_box;
        }
    }
    return box;
}

static void
box_free(struct box *box)
{
    /* destroy internal state */
    switch (box->dpath.item->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        break ;
    case AST_NODE_TYPE_ARRAY:
        if (box_index_cache_exists(box)) {
            box_destroy_index_cache_by_key(box);
        }
        box_destroy_mark_offsets_repo(box);
        break ;
    case AST_NODE_TYPE_FILTERED:
        (void)bitpunch_close_binary_file(
            (struct bitpunch_binary_file_hdl *)box->file_hdl);
        free((struct bitpunch_binary_file_hdl *)box->file_hdl);
        break ;
    default:
        break ;
    }
    free(box);
}

void
box_acquire(struct box *box)
{
    ++box->use_count;
}

static void
box_delete_non_null(struct box *box)
{
    assert(box->use_count > 0);
    --box->use_count;
    if (0 == box->use_count) {
        box_delete(box->parent_box);
        if (NULL != box->unfiltered_box) {
            box_delete(box->unfiltered_box);
        }
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
    switch (box->dpath.item->type) {
    case AST_NODE_TYPE_ARRAY:
        return (NULL != box->u.array.cache_by_key);
    default:
        return FALSE;
    }
}

int
box_contains_indexed_items(const struct box *box)
{
    switch (box->dpath.item->type) {
    case AST_NODE_TYPE_ARRAY_SLICE:
        return box_contains_indexed_items(box->parent_box);
    default:
        return ast_node_is_indexed(box->dpath.item);
    }
}

enum expr_value_type
box_get_index_type(const struct box *box)
{
    switch (box->dpath.item->type) {
    case AST_NODE_TYPE_ARRAY_SLICE:
        return box_get_index_type(box->parent_box);
    default:
        return ast_node_get_key_type(box->dpath.item);
    }
}

struct ast_node *
box_get_key_expr(const struct box *box)
{
    switch (box->dpath.item->type) {
    case AST_NODE_TYPE_ARRAY_SLICE:
        return box_get_key_expr(box->parent_box);
    default:
        return ast_node_get_key_expr(box->dpath.item);
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
    return 0 != (box->dpath.item->u.array.item_type.item->u.item.flags
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
tracker_index_cache_add_item(struct tracker *tk, union expr_value item_key)
{
    const char *key_buf;
    int64_t key_len;
    int64_t mark;

    DBG_TRACKER_DUMP(tk);
    assert(AST_NODE_TYPE_ARRAY == tk->box->dpath.item->type);
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
        expr_value_to_hashable(ast_node_get_key_type(tk->box->dpath.item),
                               item_key, &key_buf, &key_len);
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
    union expr_value key;
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
                                 union expr_value item_key,
                                 struct track_path in_slice_path,
                                 struct index_cache_iterator *iterp,
                                 struct browse_state *bst)
{
    struct dpath_node *item_dpath;
    enum expr_value_type key_type;
    const char *key_buf;
    int64_t key_len;
    bloom_book_mark_t from_mark;

    DBG_BOX_DUMP(box);
    assert(AST_NODE_TYPE_ARRAY == box->dpath.item->type);
    assert(box_index_cache_exists(box));

    key_type = ast_node_get_key_type(box->dpath.item);
    expr_value_to_hashable(key_type, item_key, &key_buf, &key_len);

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
        item_dpath = &box->dpath.item->u.array.item_type;
        tracker_goto_mark_internal(iterp->xtk, item_dpath, iterp->mark, bst);
    }
    iterp->first = TRUE;
}

static bitpunch_status_t
index_cache_iterator_next_twin(struct index_cache_iterator *iter,
                               struct track_path *item_pathp,
                               struct browse_state *bst)
{
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
    item_dpath = &xtk->box->dpath.item->u.array.item_type;
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
    union expr_value item_key,
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
                              union expr_value item_key,
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
                                  union expr_value item_key,
                                  struct track_path in_slice_path,
                                  int *nth_twinp,
                                  struct browse_state *bst)

{
    bitpunch_status_t bt_ret;
    const struct ast_node *node;
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
    node = tk->box->dpath.item;
    xtk = tracker_dup(tk);
    tracker_goto_last_cached_item_internal(xtk, bst);
    cur_twin = *nth_twinp;
    do {
        bt_ret = tracker_goto_next_item_internal(xtk, bst);
        if (BITPUNCH_OK == bt_ret) {
            bt_ret = node->u.container.b_tk.goto_next_key_match(
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
        assert(xtk->item_offset == tk->item_offset);
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
    tk->dpath = item_dpath;
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
        assert(0 == (item_dpath->item->u.item.flags
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
    DBG_TRACKER_DUMP(tk);
    assert(AST_NODE_TYPE_ARRAY == tk->box->dpath.item->type);
    if (-1 != tk->box->u.array.last_cached_index) {
        tk->dpath = &tk->box->dpath.item->u.array.item_type;
        if (0 != (tk->dpath->item->u.item.flags
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
                                    union expr_value key,
                                    int64_t mark,
                                    struct browse_state *bst)
{
    struct track_path search_boundary;

    DBG_TRACKER_DUMP(tk);
    /* limit search to keys belonging to the current mark's scope */
    search_boundary = track_path_from_array_index(
        box_array_get_mark_start_index(tk->box, mark + 1));

    return tk->box->dpath.item->u.container.b_tk.goto_next_key_match(
        tk, key, search_boundary, bst);
}


/*
 * tracker
 */

enum tracker_state
tracker_get_state(const struct tracker *tk)
{
    if (NULL == tk->dpath) {
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
tracker_reset_item_internal(struct tracker *tk)
{
    tk->item_size = -1;
    box_delete(tk->item_box);
    tk->item_box = NULL;
}

static void
tracker_reset_item(struct tracker *tk)
{
    DBG_TRACKER_DUMP(tk);
    tracker_reset_item_internal(tk);
}

static void
tracker_reset_track_path(struct tracker *tk)
{
    switch (tk->box->dpath.item->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        tk->cur = track_path_from_block_field(NULL);
        break ;
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_ARRAY_SLICE:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_BYTE_SLICE:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_FILTERED:
        tk->cur = track_path_from_array_index(-1);
        break ;
    default:
        assert(0);
    }
}

static void
tracker_set_dangling_internal(struct tracker *tk)
{
    tk->dpath = NULL;
    tracker_reset_item_internal(tk);
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
            (NULL != tk->dpath ?
             ast_node_type_str(tk->dpath->item->type) : "N/A"),
            (NULL != tk->dpath && NULL != tk->dpath->filter ?
             ast_node_type_str(tk->dpath->filter->type) : "N/A"),
            tk->item_offset,
            (-1 != tk->item_size ? tk->item_offset + tk->item_size : -1));
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
        tracker_reset_item(tk);
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return bt_ret;
}

static void
tracker_set_end_nocheck(struct tracker *tk)
{
    tracker_reset_item_internal(tk);
    tk->dpath = NULL;
    tk->flags |= TRACKER_AT_END;
}

static bitpunch_status_t
tracker_set_end(struct tracker *tk, struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    if (AST_NODE_TYPE_BLOCK_DEF == tk->box->dpath.item->type
        && BLOCK_TYPE_UNION == tk->box->dpath.item->u.block_def.type) {
        /* union: no offset check */
    } else if (-1 != tk->item_offset
               && -1 != tk->box->end_offset_min_span
               && tk->item_offset < tk->box->end_offset_min_span) {
        if (NULL != error_get_expected(BITPUNCH_OUT_OF_BOUNDS_ERROR, bst)) {
            return BITPUNCH_OUT_OF_BOUNDS_ERROR;
        }
        return tracker_error(
            BITPUNCH_OUT_OF_BOUNDS_ERROR, tk, NULL, bst,
            "box used size is smaller than its minimum span size: "
            "box used size is [%"PRIi64"..%"PRIi64"[, "
            "box minimum span size is [%"PRIi64"..%"PRIi64"[",
            tk->box->start_offset_used, tk->item_offset,
            tk->box->start_offset_used, tk->box->end_offset_min_span);
    }
    tracker_set_end_nocheck(tk);
    return BITPUNCH_OK;
}

bitpunch_status_t
tracker_create_item_box_internal(struct tracker *tk,
                                 struct browse_state *bst)
{
    struct box *item_box;
    enum box_flag box_flags;
    bitpunch_status_t bt_ret;

    if (NULL == tk->item_box) {
        if (NULL == tk->dpath) {
            return BITPUNCH_NO_ITEM;
        }
        if (0 != (tk->flags & TRACKER_REVERSED)) {
            box_flags = BOX_REVERSED;
        } else {
            box_flags = 0u;
        }
        if (-1 == tk->item_offset) {
            bt_ret = tracker_compute_item_offset(tk, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
            assert(tk->item_offset >= 0);
        }
    }
    // need to re-check because tracker_compute_item_offset() may have
    // set the item box
    if (NULL == tk->item_box) {
        item_box = box_lookup_cached_child(tk->box, tk->cur, box_flags);
        if (NULL != item_box) {
#ifdef DEBUG
            if (tracker_debug_mode) {
                printf("found box in cache: ");
                DBG_BOX_DUMP(item_box);
            }
#endif
            box_acquire(item_box);
        } else {
            item_box = new_safe(struct box);
            bt_ret = box_construct(item_box, tk->box, tk->dpath,
                                   tk->item_offset, box_flags, bst);
            if (BITPUNCH_OK != bt_ret) {
                free(item_box);
                DBG_TRACKER_CHECK_STATE(tk);
                return bt_ret;
            }
        }
        tk->item_box = item_box;
        item_box->track_path = tk->cur;
        if (-1 != item_box->end_offset_used) {
            tk->item_size =
                item_box->end_offset_used - item_box->start_offset_used;
        }
    } else {
        item_box = tk->item_box;
    }
    box_update_cache(item_box, bst);
    if (-1 != tk->item_size) {
        box_set_used_size(tk->item_box, tk->item_size, bst);
    }
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_min_span_size(struct box *box,
                          struct browse_state *bst)
{
    if (-1 != box->end_offset_min_span) {
        /* nothing to do */
        return BITPUNCH_OK;
    }
    return box->dpath.item->u.container.b_box.compute_min_span_size(box, bst);
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
        *min_span_sizep = box->end_offset_min_span - box->start_offset_used;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_used_size(struct box *box,
                      struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    if (-1 != box->end_offset_used) {
        /* nothing to do */
        return BITPUNCH_OK;
    }
    bt_ret = box_compute_min_span_size(box, bst);
    if (BITPUNCH_OK == bt_ret
        && 0 == (box->flags & COMPUTING_SPAN_SIZE)) {
        bt_ret = box_compute_max_span_size(box, bst);
    }
    if (BITPUNCH_OK == bt_ret) {
        if (0 == (box->flags & COMPUTING_SPAN_SIZE)
            && box->end_offset_min_span == box->end_offset_max_span) {
            box->end_offset_used = box->end_offset_max_span;
            bt_ret = box_set_end_offset(box, box->end_offset_max_span,
                                        BOX_END_OFFSET_USED, bst);
        } else {
            bt_ret = box->dpath.item->u.container.b_box.compute_used_size(
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

static bitpunch_status_t
box_compute_max_span_size(struct box *box,
                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    if (-1 != box->end_offset_max_span) {
        /* nothing to do */
        return BITPUNCH_OK;
    }
    assert(0 == (box->flags & COMPUTING_SPAN_SIZE));
    box->flags |= COMPUTING_SPAN_SIZE;
    bt_ret = box->dpath.item->u.container.b_box.compute_max_span_size(box, bst);
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
    struct box *parent_box;
    bitpunch_status_t bt_ret;

    if (-1 != box->end_offset_slack) {
        /* nothing to do */
        return BITPUNCH_OK;
    }
    parent_box = box->parent_box;
    /* root box available space is known at init */
    assert(NULL != parent_box);
    if (-1 == parent_box->end_offset_slack) {
        bt_ret = box_compute_slack_size(parent_box, bst);
        if (BITPUNCH_OK == bt_ret) {
            /* early bound check */
            bt_ret = box_check_end_offset(
                box, box_get_known_end_offset(parent_box),
                BOX_END_OFFSET_SLACK, bst);
        }
    } else {
        bt_ret = BITPUNCH_OK;
    }
    if (BITPUNCH_OK == bt_ret) {
        bt_ret = box->dpath.item->u.container.b_box.compute_slack_size(box, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(
            box, bst, "when computing available size");
    }
    return bt_ret;
}

static bitpunch_status_t
box_get_children_slack(struct box *box, int64_t *max_slack_offsetp,
                       struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int flag_set = FALSE;

    // check for circular dependency
    if (!(box->flags & COMPUTING_MAX_SLACK_OFFSET)) {
        box->flags |= COMPUTING_MAX_SLACK_OFFSET;
        flag_set = TRUE;
    }
    if (NULL != box->dpath.item->u.container.b_box.get_max_slack_offset) {
        bt_ret = box->dpath.item->u.container.b_box.get_max_slack_offset(
            box, max_slack_offsetp, bst);
    } else {
        bt_ret = box_compute_max_span_size(box, bst);
        if (BITPUNCH_OK == bt_ret) {
            *max_slack_offsetp = box_get_known_end_offset(box);
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

    bt_ret = box->dpath.item->u.container.b_box.get_n_items(box, n_itemsp, bst);
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(
            box, bst, "when computing number of array items");
    }
    return bt_ret;
}

static bitpunch_status_t
read_value_bytes(struct box *scope,
                 int64_t item_offset,
                 int64_t item_size,
                 enum expr_value_type *typep,
                 union expr_value *valuep,
                 struct browse_state *bst)
{
    if (NULL != typep) {
        *typep = EXPR_VALUE_TYPE_BYTES;
    }
    if (NULL != valuep) {
        valuep->bytes.buf = scope->file_hdl->bf_data + item_offset;
        valuep->bytes.len = item_size;
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
box_read_value_internal(struct box *box,
                        enum expr_value_type *typep,
                        union expr_value *valuep,
                        struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    bt_ret = box_compute_used_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }

    if (NULL != box->dpath.filter) {
        enum expr_value_type value_type;

        bt_ret = box->dpath.filter->u.rexpr_filter.b_filter.read_value(
            box->dpath.filter, box, box->start_offset_used,
            box->end_offset_used - box->start_offset_used,
            &value_type, valuep, bst);
        if (BITPUNCH_OK == bt_ret) {
            if (EXPR_VALUE_TYPE_STRING == value_type) {
                // string values share the box data buffer, so inc ref
                // count
                box_acquire(box);
            }
            if (NULL != typep) {
                *typep = value_type;
            }
        }
    } else {
        bt_ret = read_value_bytes(
            box, box->start_offset_used,
            box->end_offset_used - box->start_offset_used,
            typep, valuep, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(box, bst, "when reading value");
    }
    return bt_ret;
}

bitpunch_status_t
box_compute_end_offset_internal(struct box *box,
                                enum box_offset_type off_type,
                                int64_t *end_offsetp,
                                struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t end_offset;

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
    const struct ast_node *node;
    bitpunch_status_t bt_ret;
    int64_t n_items;
    int64_t index_start;

    DBG_BOX_DUMP(box);
    node = box->dpath.item;
    switch (node->type) {
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_FILTERED:
        bt_ret = box_get_n_items_internal(box, &n_items, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        end_pathp->type = TRACK_PATH_ARRAY;
        end_pathp->u.array.index = n_items;
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
        end_pathp->type = TRACK_PATH_ARRAY;
        end_pathp->u.array.index = index_start + n_items;
        break ;
    case AST_NODE_TYPE_BLOCK_DEF:
        end_pathp->type = TRACK_PATH_BLOCK;
        end_pathp->u.block.field = NULL;
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
    bitpunch_status_t bt_ret;
    struct box *filtered_box;
    struct tracker *tk;

    bt_ret = box_apply_filters(box, &filtered_box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return NULL;
    }
    tk = tracker_new(filtered_box);
    box_delete(filtered_box);
    return tk;
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

static struct tracker *
track_item_contents_internal(struct tracker *tk,
                             struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_create_item_box_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return NULL;
    }
    return track_box_contents_internal(tk->item_box, bst);
}

static bitpunch_status_t
tracker_compute_item_offset(struct tracker *tk,
                            struct browse_state *bst)
{
    const struct ast_node *node;

    DBG_TRACKER_DUMP(tk);
    if (NULL == tk->dpath) {
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
    /* do deferred evaluation of item_offset */
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    node = tk->box->dpath.item;
    switch (node->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        return tracker_goto_field_internal(tk, tk->cur.u.block.field,
                                           TRUE, bst);
    case AST_NODE_TYPE_ARRAY:
    case AST_NODE_TYPE_BYTE_ARRAY:
    case AST_NODE_TYPE_AS_BYTES:
    case AST_NODE_TYPE_FILTERED:
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
tracker_get_item_offset_internal(struct tracker *tk, int64_t *item_offsetp,
                                 struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    if (NULL == tk->dpath) {
        return BITPUNCH_NO_ITEM;
    }
    if (-1 == tk->item_offset) {
        bt_ret = tracker_compute_item_offset(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        assert(tk->item_offset >= 0);
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
    assert(ast_node_is_container(tk->box->dpath.item));
    return box_get_n_items_internal(tk->box, item_countp, bst);
}

bitpunch_status_t
tracker_check_item(struct tracker *tk,
                   struct browse_state *bst)
{
    int reversed_iter;
    int64_t max_offset;
    int64_t item_size;

    DBG_TRACKER_DUMP(tk);
    reversed_iter = (0 != (tk->flags & TRACKER_REVERSED));
    if (NULL != tk->dpath
        && 0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)
        && -1 == tk->item_offset) {
        bitpunch_status_t bt_ret;

        bt_ret = tracker_compute_item_offset(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        assert(tk->item_offset >= 0);
    }
    if (reversed_iter) {
        max_offset = tk->box->start_offset_used;
    } else if ((tk->box->flags & COMPUTING_MAX_SLACK_OFFSET)) {
        max_offset = box_get_known_end_offset_mask(
            tk->box, (BOX_END_OFFSET_MAX_SPAN |
                      BOX_END_OFFSET_SLACK |
                      BOX_END_OFFSET_PARENT));
    } else {
        max_offset = box_get_known_end_offset(tk->box);
    }
    assert(-1 != max_offset);

    if (-1 != tk->item_size) {
        item_size = tk->item_size;
    } else if (NULL != tk->dpath) {
        item_size = ast_node_get_min_span_size(tk->dpath->item);
    } else {
        item_size = 0;
    }
    if (reversed_iter) {
        if (tk->item_offset - item_size < max_offset) {
            return tracker_error_item_out_of_bounds(tk, bst);
        }
    } else {
        if (tk->item_offset + item_size > max_offset) {
            return tracker_error_item_out_of_bounds(tk, bst);
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

static bitpunch_status_t
tracker_goto_first_item_internal(struct tracker *tk,
                                 struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    bitpunch_status_t bt_ret2;

    DBG_TRACKER_DUMP(tk);
    tracker_set_dangling(tk);
    tk->item_offset = -1;
    bt_ret = tk->box->dpath.item->u.container.b_tk.goto_first_item(tk, bst);
    switch (bt_ret) {
    case BITPUNCH_NO_ITEM:
        tk->flags |= TRACKER_AT_END;
        /*FALLTHROUGH*/
    case BITPUNCH_OK:
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
            tk->item_offset = tk->box->start_offset_used;
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
    DBG_TRACKER_DUMP(tk);
    if (0 != (tk->flags & TRACKER_AT_END)) {
        return BITPUNCH_NO_ITEM;
    }
    if (NULL == tk->dpath) {
        return tracker_goto_first_item_internal(tk, bst);
    }
    return tk->box->dpath.item->u.container.b_tk.goto_next_item(tk, bst);
}


static bitpunch_status_t
tracker_goto_nth_item_internal(struct tracker *tk, int64_t index,
                               struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    if (index < 0) {
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, NULL, bst, NULL);
    }
    return tk->box->dpath.item->u.container.b_tk.goto_nth_item(tk, index, bst);
}

static bitpunch_status_t
tracker_goto_nth_position_internal(struct tracker *tk, int64_t index,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    if (index < 0) {
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, NULL, bst, NULL);
    }
    bt_ret = tk->box->dpath.item->u.container.b_tk.goto_nth_item(tk, index, bst);
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
    DBG_TRACKER_DUMP(tk);
    if (NULL == tk->box->dpath.item->u.container.b_tk.goto_named_item) {
        return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst, NULL);
    }
    return tk->box->dpath.item->u.container.b_tk.goto_named_item(tk, name, bst);
}


static bitpunch_status_t
tracker_goto_first_item_with_key_internal(struct tracker *tk,
                                          union expr_value item_key,
                                          struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_goto_nth_item_with_key_internal(tk, item_key, 0, bst);
}

static bitpunch_status_t
tracker_goto_next_item_with_key_internal(struct tracker *tk,
                                         union expr_value item_key,
                                         struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    if (NULL == tk->dpath) {
        return tracker_goto_nth_item_with_key_internal(tk, item_key, 0, bst);
    }
    return tk->box->dpath.item->u.container.b_tk.goto_next_item_with_key(
        tk, item_key, bst);
}

static bitpunch_status_t
tracker_goto_nth_item_with_key_internal(struct tracker *tk,
                                        union expr_value item_key,
                                        int nth_twin,
                                        struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    if (nth_twin < 0) {
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, NULL, bst,
                             "parameter \"nth_twin\" must be >= 0 (is %d)",
                             nth_twin);
    }
    return tk->box->dpath.item->u.container.b_tk.goto_nth_item_with_key(
        tk, item_key, nth_twin, bst);
}


static void
tracker_set_field_internal__block(struct tracker *tk,
                                  const struct field *field,
                                  struct browse_state *bst)
{
    DPRINT("TK set field "ANSI_COLOR_GREEN"%s"ANSI_COLOR_RESET" on:\n",
           field->nstmt.name);
    DBG_TRACKER_DUMP(tk);
    tracker_set_dangling_internal(tk);
    tk->cur.u.block.field = field;
    tk->dpath = &field->dpath;
    DBG_TRACKER_CHECK_STATE(tk);
}

bitpunch_status_t
tracker_goto_field_internal(struct tracker *tk,
                            const struct field *to_field, int flat,
                            struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int is_trailer_field;
    int reverse_direction;

    DBG_TRACKER_DUMP(tk);
    assert(flat || NULL != to_field->nstmt.name);

    if (flat && 0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        tracker_set_field_internal__block(tk, to_field, bst);
        return BITPUNCH_OK;
    }
    // union may be nesting anonymous structures, so an easy
    // optimization is incorrect because offset may not be box' offset
    // (maybe a more complex optim is possible, keep it for later)
    is_trailer_field = (0 != (to_field->nstmt.stmt.stmt_flags
                              & FIELD_FLAG_SLACK_TRAILER));
    reverse_direction =
        (is_trailer_field && 0 == (tk->flags & TRACKER_REVERSED))
        || (!is_trailer_field && 0 != (tk->flags & TRACKER_REVERSED));
    if (reverse_direction) {
        tk->flags ^= TRACKER_REVERSED;
    }
    bt_ret = tracker_goto_first_item_int__block(tk, flat, bst);
    while (BITPUNCH_OK == bt_ret && tk->cur.u.block.field != to_field) {
        bt_ret = tracker_goto_next_item_int__block(tk, flat, bst);
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
    struct ast_node *expr_node;
    struct parser_ctx *parser_ctx = NULL;
    struct box *root_box;
    bitpunch_status_t bt_ret;
    union expr_dpath eval_dpath;
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
    if (-1 == bitpunch_resolve_expr(&expr_node, root_box)) {
        free(parser_ctx);
        /* TODO free expr_node */
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, NULL, bst, NULL);
    }
    if (EXPR_DPATH_TYPE_ITEM != expr_node->u.rexpr.dpath_type &&
        EXPR_DPATH_TYPE_CONTAINER != expr_node->u.rexpr.dpath_type) {
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
    switch (expr_node->u.rexpr.dpath_type) {
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
    const struct ast_node *node;

    DBG_TRACKER_DUMP(tk);
    node = tk->box->dpath.item;
    if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        if (AST_NODE_TYPE_BLOCK_DEF == node->type
            && BLOCK_TYPE_UNION == node->u.block_def.type) {
            /* union: no offset change */
        } else if (0 != (tk->flags & TRACKER_REVERSED)) {
            tk->item_offset = tk->box->start_offset_used;
        } else {
            bitpunch_status_t bt_ret;

            bt_ret = box_compute_used_size(tk->box, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
            tk->item_offset = tk->box->end_offset_used;
        }
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
    union expr_value item_index;
    bitpunch_status_t bt_ret;

    if (NULL != index.key) {
        bt_ret = expr_evaluate_value_internal(index.key, scope,
                                              &item_index, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_error_add_tracker_context(
                tk, bst, "when evaluating item index expression");
            return bt_ret;
        }
        if (EXPR_VALUE_TYPE_INTEGER == index.key->u.rexpr.value_type) {
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
                        (ast_node_is_slice_container(tk->box->dpath.item) ?
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
            union expr_value twin_index;

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
                    == index.key->u.rexpr.value_type)) {
                semantic_error(
                    SEMANTIC_LOGLEVEL_ERROR, &index.key->loc,
                    "key '%.*s'{%"PRIi64"} does not exist",
                    (int)item_index.string.len, item_index.string.str,
                    twin_index.integer);
            }
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
    bt_ret = tracker_create_item_box_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        DBG_TRACKER_CHECK_STATE(tk);
        return bt_ret;
    }
    bt_ret = box_apply_filters(tk->item_box, &filtered_box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    box_delete_non_null(tk->box);
    tk->box = filtered_box;
    tk->item_box = NULL;
    tracker_rewind_internal(tk);
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
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
    if (0 != (tk->flags & TRACKER_REVERSED)) {
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
            int64_t end_offset;
            bitpunch_status_t bt_ret;

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
        }
    } else {
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
            tk->item_offset = box->start_offset_used;
        }
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
    if (NULL != tk->dpath) {
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

static bitpunch_status_t
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
        tk->item_box = (NULL != tk->item_box->parent_box ?
                        tk->item_box->parent_box :
                        tk->item_box->unfiltered_box);
    }
    if (tk->item_box != orig_box) {
        box_acquire(tk->item_box);
        box_delete(orig_box);
    }
    //FIXME should restore the original dpath
    tk->dpath = &tk->item_box->dpath;
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
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}


static int
track_path_elem_dump_to_buf(struct track_path tp, int dump_separator,
                            char *dpath_expr_buf, int buf_size)
{
    switch (tp.type) {
    case TRACK_PATH_NOTYPE:
        return snprintf(dpath_expr_buf, buf_size, "(none)");
    case TRACK_PATH_BLOCK:
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
        assert(dump_separator); /* arrays always have a parent */
        return snprintf(dpath_expr_buf, buf_size,
                        "[%"PRIi64"]", tp.u.array.index);
    case TRACK_PATH_ARRAY_SLICE:
        assert(dump_separator); /* array slices always have a parent */
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

    switch (tp.type) {
    case TRACK_PATH_NOTYPE:
        return fprintf(stream, "(none)");
    case TRACK_PATH_BLOCK:
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
                               tp.u.block.field->dpath.item->type));
        }
        break ;
    case TRACK_PATH_ARRAY:
        return fprintf(stream, "[%"PRIi64"]", tp.u.array.index);
    case TRACK_PATH_ARRAY_SLICE:
        return fprintf(stream, "[%"PRIi64":%"PRIi64"]",
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
    n_out += track_path_elem_dump_to_buf(box->track_path,
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
    if (NULL == tk->dpath) {
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

bitpunch_status_t
tracker_get_tracked_container_type(struct tracker *tk,
                                   enum container_type *typep)
{
    DBG_TRACKER_DUMP(tk);
    assert(NULL != typep);
    switch (tk->box->dpath.item->type) {
    case AST_NODE_TYPE_BLOCK_DEF:
        *typep = CONTAINER_TYPE_BLOCK;
        break ;
    case AST_NODE_TYPE_ARRAY:
        *typep = CONTAINER_TYPE_ARRAY;
        break ;
    default:
        return BITPUNCH_NOT_CONTAINER;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_compute_item_size_internal(struct tracker *tk,
                                   int64_t *item_sizep,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    /* If span size is being computed, skip this, it normally means an
     * item is being read to know its container box size. */
    if (0 == (tk->box->flags & COMPUTING_SPAN_SIZE)) {
        bt_ret = box_compute_max_span_size(tk->box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    return tk->dpath->item->u.container.b_tk.compute_item_size(
        tk, item_sizep, bst);
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
        && -1 != tk->item_box->end_offset_used) {
        tk->item_size =
            tk->item_box->end_offset_used - tk->item_box->start_offset_used;
        if (0 != (tk->flags & TRACKER_REVERSED)) {
            tk->item_size =
                tk->item_box->start_offset_used - tk->item_box->end_offset_used;
        } else {
            tk->item_size =
                tk->item_box->end_offset_used - tk->item_box->start_offset_used;
        }
        DBG_TRACKER_CHECK_STATE(tk);
        return BITPUNCH_OK;
    }
    if (NULL == tk->dpath) {
        return BITPUNCH_NO_ITEM;
    }
    assert(ast_node_is_item(tk->dpath->item));
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
    if (NULL == tk->dpath) {
        return BITPUNCH_NO_ITEM;
    }
    if (0 == (tk->dpath->u.item.flags & ITEMFLAG_IS_USED_SIZE_DYNAMIC)) {
        if (NULL != item_sizep) {
            *item_sizep = tk->dpath->u.item.min_span_size;
        }
        return BITPUNCH_OK;
    }
    if (-1 == tk->item_offset) {
        bt_ret = tracker_compute_item_offset(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        assert(tk->item_offset >= 0);
    }
    if (-1 == tk->item_size) {
        bt_ret = tracker_compute_item_size(tk, bst);
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
tracker_get_item_key_internal(struct tracker *tk,
                              enum expr_value_type *key_typep,
                              union expr_value *keyp,
                              struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    if (NULL == tk->dpath) {
        return BITPUNCH_NO_ITEM;
    }
    return tk->box->dpath.item->u.container.b_tk.get_item_key(
        tk, key_typep, keyp, NULL, bst);
}

static bitpunch_status_t
tracker_get_item_key_multi_internal(struct tracker *tk,
                                    enum expr_value_type *key_typep,
                                    union expr_value *keyp,
                                    int *nth_twinp,
                                    struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    if (NULL == tk->dpath) {
        return BITPUNCH_NO_ITEM;
    }
    return tk->box->dpath.item->u.container.b_tk.get_item_key(
        tk, key_typep, keyp, nth_twinp, bst);
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

    bt_ret = tracker_compute_item_offset(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = tracker_compute_item_size(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != tk->item_offset);
    assert(-1 != tk->item_size);
    if (NULL != item_contentsp) {
        *item_contentsp = tk->box->file_hdl->bf_data + tk->item_offset;
    }
    if (NULL != item_sizep) {
        *item_sizep = tk->item_size;
    }
    return BITPUNCH_OK;
}

bitpunch_status_t
tracker_read_item_value_internal(struct tracker *tk,
                                 enum expr_value_type *typep,
                                 union expr_value *valuep,
                                 struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t item_offset;
    int64_t item_size;

    DBG_TRACKER_DUMP(tk);
    if (NULL == tk->dpath) {
        return BITPUNCH_NO_ITEM;
    }
    bt_ret = tracker_get_item_location_internal(tk, &item_offset,
                                                &item_size, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (NULL != tk->dpath->filter) {
        enum expr_value_type value_type;

        bt_ret = tk->dpath->filter->u.rexpr_filter.b_filter.read_value(
            tk->dpath->filter, tk->box, item_offset, item_size,
            &value_type, valuep, bst);
        if (BITPUNCH_OK == bt_ret) {
            if (EXPR_VALUE_TYPE_STRING == value_type) {
                // string values share the box data buffer, so inc ref
                // count
                box_acquire(tk->box);
            }
            if (NULL != typep) {
                *typep = value_type;
            }
        }
        return bt_ret;
    } else {
        return read_value_bytes(tk->box, item_offset, item_size,
                                typep, valuep, bst);
    }
}

bitpunch_status_t
tracker_reverse_direction_internal(struct tracker *tk,
                                   struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    if (NULL != tk->dpath
        && 0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        if (-1 == tk->item_size) {
            bt_ret = tracker_compute_item_size(tk, bst);
            if (BITPUNCH_OK != bt_ret) {
                return bt_ret;
            }
        }
        if (0 != (tk->flags & TRACKER_REVERSED)) {
            assert(tk->item_offset - tk->item_size >= tk->box->start_offset_used);
            tk->item_offset -= tk->item_size;
            // reverse item box is less precise than forward item box
            if (NULL != tk->item_box) {
                box_delete(tk->item_box);
                tk->item_box = NULL;
            }
        } else {
            tk->item_offset += tk->item_size;
        }
    }
    tk->flags ^= TRACKER_REVERSED;
    return BITPUNCH_OK;
}

/*
 * Generic statements API
 */


const char *
statement_type_str(enum statement_type stmt_type)
{
    switch (stmt_type) {
    case STATEMENT_TYPE_FIELD:
        return "field";
    case STATEMENT_TYPE_NAMED_EXPR:
        return "named expr";
    case STATEMENT_TYPE_SPAN:
        return "span";
    case STATEMENT_TYPE_KEY:
        return "key";
    case STATEMENT_TYPE_LAST:
        return "last";
    case STATEMENT_TYPE_MATCH:
        return "match";
    }
    return "unknown statement type";
}

struct statement_iterator
box_iter_statements(struct box *box,
                    enum statement_type stmt_type,
                    int stmt_flags)
{
    struct statement_iterator it;
    const struct block_stmt_list *stmt_lists;

    it.stmt_flags = stmt_flags;
    it.it_flags = 0;
    it.next_stmt = NULL;
    if (AST_NODE_TYPE_BLOCK_DEF != box->dpath.item->type) {
        return it;
    }
    stmt_lists = &box->dpath.item->u.block_def.block_stmt_list;
    switch (stmt_type) {
    case STATEMENT_TYPE_FIELD:
        it.next_stmt = TAILQ_FIRST(stmt_lists->field_list);
        break ;
    case STATEMENT_TYPE_NAMED_EXPR:
        it.next_stmt = TAILQ_FIRST(stmt_lists->named_expr_list);
        break ;
    case STATEMENT_TYPE_SPAN:
        it.next_stmt = TAILQ_FIRST(stmt_lists->span_list);
        break ;
    case STATEMENT_TYPE_KEY:
        it.next_stmt = TAILQ_FIRST(stmt_lists->key_list);
        break ;
    case STATEMENT_TYPE_LAST:
        it.next_stmt = TAILQ_FIRST(stmt_lists->last_stmt_list);
        break ;
    case STATEMENT_TYPE_MATCH:
        it.next_stmt = TAILQ_FIRST(stmt_lists->match_list);
        break ;
    }
    return it;
}

struct statement_iterator
box_iter_statements_from(struct box *box,
                         const struct statement *stmt,
                         int stmt_flags)
{
    struct statement_iterator it;

    it.stmt_flags = stmt_flags;
    it.it_flags = 0;
    it.next_stmt = TAILQ_NEXT(stmt, list);
    return it;
}

struct statement_iterator
box_riter_statements(struct box *box,
                     enum statement_type stmt_type,
                     int stmt_flags)
{
    struct statement_iterator it;
    const struct block_stmt_list *stmt_lists;

    it.stmt_flags = stmt_flags;
    it.it_flags = STATEMENT_ITERATOR_FLAG_REVERSE;
    it.next_stmt = NULL;
    if (AST_NODE_TYPE_BLOCK_DEF != box->dpath.item->type) {
        return it;
    }
    stmt_lists = &box->dpath.item->u.block_def.block_stmt_list;
    switch (stmt_type) {
    case STATEMENT_TYPE_FIELD:
        it.next_stmt = TAILQ_LAST(stmt_lists->field_list, statement_list);
        break ;
    case STATEMENT_TYPE_NAMED_EXPR:
        it.next_stmt = TAILQ_LAST(stmt_lists->named_expr_list, statement_list);
        break ;
    case STATEMENT_TYPE_SPAN:
        it.next_stmt = TAILQ_LAST(stmt_lists->span_list, statement_list);
        break ;
    case STATEMENT_TYPE_KEY:
        it.next_stmt = TAILQ_LAST(stmt_lists->key_list, statement_list);
        break ;
    case STATEMENT_TYPE_LAST:
        it.next_stmt = TAILQ_LAST(stmt_lists->last_stmt_list,
                                  statement_list);
        break ;
    case STATEMENT_TYPE_MATCH:
        it.next_stmt = TAILQ_LAST(stmt_lists->match_list, statement_list);
        break ;
    }
    return it;
}

struct statement_iterator
box_riter_statements_from(struct box *box,
                          const struct statement *stmt,
                          int stmt_flags)
{
    struct statement_iterator it;

    it.stmt_flags = stmt_flags;
    it.it_flags = STATEMENT_ITERATOR_FLAG_REVERSE;
    it.next_stmt = TAILQ_PREV(stmt, statement_list, list);
    return it;
}

static bitpunch_status_t
box_iter_statements_next_internal(struct box *box,
                                  struct statement_iterator *it,
                                  const struct statement **stmtp,
                                  struct browse_state *bst)
{
    const struct statement *stmt;

    stmt = it->next_stmt;
    while (NULL != stmt) {
        int cond_eval;
        bitpunch_status_t bt_ret;

        if (0 == it->stmt_flags
            || 0 != (it->stmt_flags & stmt->stmt_flags)) {
            bt_ret = evaluate_conditional_internal(stmt->cond, box,
                                                   &cond_eval, bst);
            if (BITPUNCH_OK != bt_ret) {
                tracker_error_add_box_context(box, bst,
                                              "when evaluating condition");
                return bt_ret;
            }
            if (cond_eval) {
                if ((it->it_flags & STATEMENT_ITERATOR_FLAG_REVERSE)) {
                    it->next_stmt = TAILQ_PREV(stmt, statement_list, list);
                } else {
                    it->next_stmt = TAILQ_NEXT(stmt, list);
                }
                if (NULL != stmtp) {
                    *stmtp = stmt;
                }
                return BITPUNCH_OK;
            }
            // condition is false: go on with next statement
        }
        if ((it->it_flags & STATEMENT_ITERATOR_FLAG_REVERSE)) {
            stmt = TAILQ_PREV(stmt, statement_list, list);
        } else {
            stmt = TAILQ_NEXT(stmt, list);
        }
    }
    return BITPUNCH_NO_ITEM;
}

static bitpunch_status_t
box_lookup_statement_recur(struct box *box,
                           const struct block_stmt_list *stmt_lists,
                           enum statement_type stmt_type,
                           const char *stmt_name,
                           const struct named_statement **stmtp,
                           struct browse_state *bst)
{
    const struct named_statement *stmt;

    switch (stmt_type) {
    case STATEMENT_TYPE_FIELD:
        stmt = (const struct named_statement *)
            TAILQ_FIRST(stmt_lists->field_list);
        break ;
    case STATEMENT_TYPE_NAMED_EXPR:
        stmt = (const struct named_statement *)
            TAILQ_FIRST(stmt_lists->named_expr_list);
        break ;
    default:
        return box_error(BITPUNCH_INVALID_PARAM, box, box->dpath.item, bst,
                         "cannot lookup unnamed statements (type '%s')",
                         statement_type_str(stmt_type));
    }
    while (NULL != stmt) {
        int cond_eval;
        bitpunch_status_t bt_ret;

        if (NULL != stmt->name) {
            if (0 == strcmp(stmt_name, stmt->name)) {
                bt_ret = evaluate_conditional_internal(stmt->stmt.cond, box,
                                                       &cond_eval, bst);
                if (BITPUNCH_OK != bt_ret) {
                    tracker_error_add_box_context(box, bst,
                                                  "when evaluating condition");
                    return bt_ret;
                }
                if (cond_eval) {
                    if (NULL != stmtp) {
                        *stmtp = stmt;
                    }
                    return BITPUNCH_OK;
                }
            }
        } else {
            assert(STATEMENT_TYPE_FIELD == stmt_type);
            if (!(stmt->stmt.stmt_flags & FIELD_FLAG_HIDDEN)) {
                const struct field *field;
                const struct ast_node *as_type;

                field = (const struct field *)stmt;
                as_type = dpath_node_get_as_type(&field->dpath);
                assert(AST_NODE_TYPE_BLOCK_DEF == as_type->type);
                bt_ret = box_lookup_statement_recur(
                    box, &as_type->u.block_def.block_stmt_list,
                    stmt_type, stmt_name, stmtp, bst);
                if (BITPUNCH_NO_ITEM != bt_ret) {
                    return bt_ret;
                }
            }
        }
        stmt = (const struct named_statement *)
            TAILQ_NEXT(&stmt->stmt, list);
    }
    return BITPUNCH_NO_ITEM;
}

static bitpunch_status_t
box_lookup_statement_internal(struct box *box,
                              enum statement_type stmt_type,
                              const char *stmt_name,
                              const struct named_statement **stmtp,
                              struct browse_state *bst)
{
    assert(AST_NODE_TYPE_BLOCK_DEF == box->dpath.item->type);
    return box_lookup_statement_recur(
        box, &box->dpath.item->u.block_def.block_stmt_list, stmt_type, stmt_name,
        stmtp, bst);
}

static bitpunch_status_t
box_get_first_statement_internal(struct box *box,
                                 enum statement_type stmt_type,
                                 int stmt_flags,
                                 const struct statement **stmtp,
                                 struct browse_state *bst)
{
    struct statement_iterator it;

    it = box_iter_statements(box, stmt_type, stmt_flags);
    return box_iter_statements_next_internal(box, &it, stmtp, bst);
}

bitpunch_status_t
box_get_n_statements_internal(struct box *box,
                              enum statement_type stmt_type,
                              int stmt_flags,
                              int64_t *stmt_countp,
                              struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct statement_iterator it;
    int64_t stmt_count;

    it = box_iter_statements(box, stmt_type, stmt_flags);
    stmt_count = -1;
    do {
        ++stmt_count;
        bt_ret = box_iter_statements_next_internal(box, &it, NULL, bst);
    } while (BITPUNCH_OK == bt_ret);
    if (BITPUNCH_NO_ITEM != bt_ret) {
        return bt_ret;
    }
    if (NULL != stmt_countp) {
        *stmt_countp = stmt_count;
    }
    return BITPUNCH_OK;
}

/*
 * named expressions API
 */

tnamed_expr_iterator
box_iter_named_exprs(struct box *box)
{
    return box_iter_statements(box, STATEMENT_TYPE_NAMED_EXPR, 0);
}

static bitpunch_status_t
box_iter_named_exprs_next_internal(struct box *box,
                                   tnamed_expr_iterator *it,
                                   const struct named_expr **named_exprp,
                                   struct browse_state *bst)
{
    return box_iter_statements_next_internal(
        box, it, (const struct statement **)named_exprp, bst);
}

bitpunch_status_t
box_lookup_named_expr_internal(struct box *box, const char *named_expr_name,
                               const struct named_expr **named_exprp,
                               struct browse_state *bst)
{
    return box_lookup_statement_internal(
        box, STATEMENT_TYPE_NAMED_EXPR, named_expr_name,
        (const struct named_statement **)named_exprp, bst);
}

bitpunch_status_t
box_lookup_attribute_internal(struct box *box, const char *name,
                              enum statement_type *stmt_typep,
                              const struct named_statement **named_stmt,
                              struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    if (AST_NODE_TYPE_BLOCK_DEF != box->dpath.item->type) {
        return BITPUNCH_NO_ITEM;
    }
    bt_ret = box_lookup_statement_internal(
        box, STATEMENT_TYPE_NAMED_EXPR, name, named_stmt, bst);
    if (BITPUNCH_OK == bt_ret) {
        *stmt_typep = STATEMENT_TYPE_NAMED_EXPR;
        return BITPUNCH_OK;
    }
    if (BITPUNCH_NO_ITEM != bt_ret) {
        return bt_ret;
    }
    bt_ret = box_lookup_statement_internal(
        box, STATEMENT_TYPE_FIELD, name, named_stmt, bst);
    if (BITPUNCH_OK == bt_ret) {
        *stmt_typep = STATEMENT_TYPE_FIELD;
        return BITPUNCH_OK;
    }
    return bt_ret;
}

bitpunch_status_t
box_evaluate_attribute_value_internal(struct box *box,
                                      const char *attr_name,
                                      enum expr_value_type *value_typep,
                                      union expr_value *eval_valuep,
                                      struct browse_state *bst)
{
    return box_evaluate_attribute_internal(box, attr_name,
                                           value_typep, eval_valuep,
                                           NULL, NULL, bst);
}

bitpunch_status_t
box_evaluate_attribute_dpath_internal(struct box *box,
                                      const char *attr_name,
                                      enum expr_dpath_type *dpath_typep,
                                      union expr_dpath *eval_dpathp,
                                      struct browse_state *bst)
{
    return box_evaluate_attribute_internal(box, attr_name,
                                           NULL, NULL,
                                           dpath_typep, eval_dpathp, bst);
}

bitpunch_status_t
box_evaluate_attribute_internal(struct box *box,
                                const char *attr_name,
                                enum expr_value_type *value_typep,
                                union expr_value *eval_valuep,
                                enum expr_dpath_type *dpath_typep,
                                union expr_dpath *eval_dpathp,
                                struct browse_state *bst)
{
    struct box *filtered_box;
    bitpunch_status_t bt_ret;
    enum statement_type stmt_type;
    const struct named_statement *named_stmt;

    bt_ret = box_apply_filters(box, &filtered_box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = box_lookup_attribute_internal(filtered_box, attr_name,
                                           &stmt_type, &named_stmt, bst);
    if (BITPUNCH_OK != bt_ret) {
        box_delete(filtered_box);
        return bt_ret;
    }
    switch (stmt_type) {
    case STATEMENT_TYPE_NAMED_EXPR: {
        const struct named_expr *named_expr;
        struct ast_node *expr;
        union expr_value eval_value;
        union expr_dpath eval_dpath;
        int eval_dpath_computed = FALSE;

        named_expr = (const struct named_expr *)named_stmt;
        expr = named_expr->expr;
        if (NULL != eval_dpathp
            && EXPR_DPATH_TYPE_NONE != expr->u.rexpr.dpath_type) {
            bt_ret = expr_evaluate_dpath_internal(expr, filtered_box,
                                                  &eval_dpath, bst);
            eval_dpath_computed = TRUE;
        }
        if (BITPUNCH_OK == bt_ret
            && NULL != eval_valuep
            && EXPR_VALUE_TYPE_UNSET != expr->u.rexpr.value_type) {
            if (eval_dpath_computed) {
                bt_ret = expr_read_dpath_value_internal(expr,
                                                        eval_dpath,
                                                        &eval_value, bst);
            } else {
                bt_ret = expr_evaluate_value_internal(expr,
                                                      filtered_box,
                                                      &eval_value, bst);
            }
        }
        box_delete(filtered_box);
        if (BITPUNCH_OK == bt_ret) {
            if (NULL != eval_valuep) {
                *eval_valuep = eval_value;
            }
            if (NULL != value_typep) {
                *value_typep = expr->u.rexpr.value_type;
            }
            if (NULL != eval_dpathp) {
                *eval_dpathp = eval_dpath;
            }
            if (NULL != dpath_typep) {
                *dpath_typep = expr->u.rexpr.dpath_type;
            }
        }
        return bt_ret;
    }
    case STATEMENT_TYPE_FIELD: {
        struct tracker *tk;

        tk = tracker_new(filtered_box);
        box_delete(filtered_box);
        bt_ret = tracker_goto_field_internal(
            tk, (const struct field *)named_stmt, FALSE, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(tk);
            return bt_ret;
        }
        if (NULL != eval_valuep) {
            if (NULL != tk->dpath->filter
                && AST_NODE_TYPE_REXPR_INTERPRETER == tk->dpath->filter->type) {
                bt_ret = tracker_read_item_value_internal(
                    tk, value_typep, eval_valuep, bst);
            } else {
                if (NULL != value_typep) {
                    *value_typep = EXPR_VALUE_TYPE_UNSET;
                }
            }
        }
        if (BITPUNCH_OK == bt_ret && NULL != eval_dpathp) {
            eval_dpathp->item.tk = tk;
        } else {
            tracker_delete(tk);
        }
        if (BITPUNCH_OK == bt_ret && NULL != dpath_typep) {
            *dpath_typep = EXPR_DPATH_TYPE_ITEM;
        }
        return bt_ret;
    }
    default:
        assert(0);
    }
    /*NOT REACHED*/
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
                  const struct ast_node *node,
                  const char *message_fmt, va_list message_args)
{
    struct tracker_error *tk_err;

    tk_err = new_safe(struct tracker_error);
    if (NULL != tk) {
        assert(NULL == box);
        tk_err->tk = tracker_dup_raw(tk);
    } else {
        assert(NULL == tk);
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
        if (NULL != tk_err->tk) {
            assert(NULL == tk_err->box);
            tracker_delete(tk_err->tk);
        } else {
            assert(NULL != tk_err->box);
            box_delete(tk_err->box);
        }
        for (ctx_i = 0; ctx_i < tk_err->n_contexts; ++ctx_i) {
            struct tracker_error_context_info *ctx_info;

            ctx_info = &tk_err->contexts[ctx_i];
            if (NULL != ctx_info->tk) {
                tracker_delete(ctx_info->tk);
            }
            if (NULL != ctx_info->box) {
                box_delete(ctx_info->box);
            }
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
              const struct ast_node *node,
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
          const struct ast_node *node,
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

bitpunch_status_t
box_error_out_of_bounds(struct box *box,
                        const struct ast_node *node,
                        enum box_offset_type requested_end_offset_type,
                        int64_t requested_end_offset,
                        enum box_offset_type registered_end_offset_type,
                        struct browse_state *bst)
{
    struct tracker_error *tk_err;

    DBG_BOX_DUMP(box);
    if (NULL != error_get_expected(BITPUNCH_OUT_OF_BOUNDS_ERROR, bst)) {
        return BITPUNCH_OUT_OF_BOUNDS_ERROR;
    }
    (void) box_error(BITPUNCH_OUT_OF_BOUNDS_ERROR, box, node, bst,
                     "request offset out of box bounds: "
                     "box %s space is [%"PRIi64"..%"PRIi64"[, "
                     "requested %s offset at %"PRIi64"",
                     box_offset_type_str(registered_end_offset_type),
                     box->start_offset_used,
                     box_get_offset(box, registered_end_offset_type),
                     box_offset_type_str(requested_end_offset_type),
                     requested_end_offset);
    tk_err = bst->last_error;
    assert(NULL != tk_err);
    tk_err->u.out_of_bounds.registered_end_offset_type =
        registered_end_offset_type;
    tk_err->u.out_of_bounds.registered_end_offset =
        box_get_offset(box, registered_end_offset_type);
    tk_err->u.out_of_bounds.requested_end_offset_type =
        requested_end_offset_type;
    tk_err->u.out_of_bounds.requested_end_offset =
        requested_end_offset;

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
    assert(tk->item_offset >= 0);
    if (NULL != error_get_expected(BITPUNCH_OUT_OF_BOUNDS_ERROR, bst)) {
        return BITPUNCH_OUT_OF_BOUNDS_ERROR;
    }
    if (-1 != tk->item_size) {
        snprintf(item_span_msg, sizeof (item_span_msg),
                 "item spans [%"PRIi64"..%"PRIi64"[",
                 tk->item_offset, tk->item_offset + tk->item_size);
        out_of_bounds_offset = tk->item_offset + tk->item_size;
    } else if (NULL != tk->dpath) {
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
        BITPUNCH_OUT_OF_BOUNDS_ERROR, tk, tk->dpath->item, bst,
        "item location out of container box bounds: "
        "box %s space is [%"PRIi64"..%"PRIi64"[, %s",
        box_offset_type_str(box_get_known_end_offset_type(tk->box)),
        tk->box->start_offset_used, box_get_known_end_offset(tk->box),
        item_span_msg);
    tk_err = bst->last_error;
    assert(NULL != tk_err);
    tk_err->u.out_of_bounds.registered_end_offset_type =
        box_get_known_end_offset_type(tk->box);
    tk_err->u.out_of_bounds.registered_end_offset =
        box_get_known_end_offset(tk->box);
    tk_err->u.out_of_bounds.requested_end_offset_type =
        BOX_END_OFFSET_USED;
    tk_err->u.out_of_bounds.requested_end_offset =
        out_of_bounds_offset;

    return BITPUNCH_OUT_OF_BOUNDS_ERROR;
}

static void
tracker_error_add_context_internal(struct tracker *tk,
                                   struct box *box,
                                   const struct ast_node *node,
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
tracker_error_add_node_context(const struct ast_node *node,
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
    *item_sizep = tk->dpath->item->u.item.min_span_size;
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_used_size__static_size(struct box *box,
                                   struct browse_state *bst)
{
    DBG_BOX_DUMP(box);
    return box_set_used_size(box, box->dpath.item->u.item.min_span_size, bst);
}


static bitpunch_status_t
box_compute_slack_size__from_parent(struct box *box,
                                     struct browse_state *bst)
{
    DBG_BOX_DUMP(box);
    assert(NULL != box->parent_box);
    /* this should have been previously ensured by
     * box_compute_slack_size() */
    assert(-1 != box->parent_box->end_offset_slack);
    return box_set_end_offset(box,
                              box_get_known_end_offset(box->parent_box),
                              BOX_END_OFFSET_SLACK, bst);
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
    assert(-1 != box->end_offset_used);
    return box_set_end_offset(box, box->end_offset_used,
                              BOX_END_OFFSET_MAX_SPAN, bst);
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
    assert(-1 != box->end_offset_slack);
    return box_set_end_offset(box, box->end_offset_slack,
                              BOX_END_OFFSET_MAX_SPAN, bst);
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
    return box_set_end_offset(box, box->parent_box->end_offset_used,
                              BOX_END_OFFSET_USED, bst);
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
    return box_set_end_offset(box, box->end_offset_max_span,
                              BOX_END_OFFSET_USED, bst);
}

static bitpunch_status_t
box_compute_min_span_size__as_hard_min(struct box *box,
                                       struct browse_state *bst)
{
    DBG_BOX_DUMP(box);
    box->end_offset_min_span =
        box->start_offset_used + ast_node_get_min_span_size(box->dpath.item);
    return box_set_end_offset(box, box->end_offset_min_span,
                              BOX_END_OFFSET_MIN_SPAN, bst);
}

static bitpunch_status_t
box_compute_slack_size__block_file(struct box *box,
                                    struct browse_state *bst)
{
    int64_t file_size;

    DBG_BOX_DUMP(box);
    file_size = (int64_t)box->file_hdl->bf_data_length;
    return box_set_end_offset(box, file_size, BOX_END_OFFSET_SLACK, bst);
}

static bitpunch_status_t
box_compute_min_span_size__span_expr(struct box *box,
                                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    const struct span_stmt *span_stmt;
    struct ast_node *span_expr;
    union expr_value span_size;

    DBG_BOX_DUMP(box);
    bt_ret = box_get_first_statement_internal(
        box, STATEMENT_TYPE_SPAN, SPAN_FLAG_MIN,
        (const struct statement **)&span_stmt, bst);
    if (BITPUNCH_OK != bt_ret) {
        if (BITPUNCH_NO_ITEM == bt_ret) {
            // no dynamic span enabled by conditional: min span is the
            // hard minimum
            return box_compute_min_span_size__as_hard_min(box, bst);
        } else {
            return bt_ret;
        }
    }
    span_expr = span_stmt->span_expr;
    bt_ret = expr_evaluate_value_internal(span_expr, box, &span_size, bst);
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(
            box, bst, "when evaluating expression of min span size");
        return bt_ret;
    }
    /* TODO: show data path info in errors */
    if (span_size.integer < 0) {
        return box_error(BITPUNCH_DATA_ERROR, box, span_expr, bst,
                         "evaluation of span size expression gives "
                         "negative value (%"PRIi64")", span_size.integer);
    }
    bt_ret = box_set_min_span_size(box, span_size.integer, bst);
    if (0 != (SPAN_FLAG_MAX & span_stmt->stmt.stmt_flags)
        && BITPUNCH_OK == bt_ret) {
        bt_ret = box_set_max_span_size(box, span_size.integer, bst);
    }
    return bt_ret;
}

static bitpunch_status_t
box_compute_max_span_size__span_expr(struct box *box,
                                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    const struct span_stmt *span_stmt;
    struct ast_node *span_expr;
    union expr_value span_size;

    DBG_BOX_DUMP(box);
    bt_ret = box_get_first_statement_internal(
        box, STATEMENT_TYPE_SPAN, SPAN_FLAG_MAX,
        (const struct statement **)&span_stmt, bst);
    if (BITPUNCH_OK != bt_ret) {
        if (BITPUNCH_NO_ITEM == bt_ret) {
            // no dynamic span enabled by conditional
            if (0 == (box->dpath.u.item.flags
                      & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
                return box_compute_max_span_size__as_used(box, bst);
            } else {
                return box_compute_max_span_size__as_slack(box, bst);
            }
        } else {
            return bt_ret;
        }
    }
    span_expr = span_stmt->span_expr;
    bt_ret = expr_evaluate_value_internal(span_expr, box, &span_size, bst);
    if (BITPUNCH_OK != bt_ret) {
        tracker_error_add_box_context(
            box, bst, "when evaluating expression of max span size");
        return bt_ret;
    }
    /* TODO: show data path info in errors */
    if (span_size.integer < 0) {
        return box_error(BITPUNCH_DATA_ERROR, box, span_expr, bst,
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
    if (0 != (SPAN_FLAG_MIN & span_stmt->stmt.stmt_flags)
        && BITPUNCH_OK == bt_ret) {
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
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    if (AST_NODE_TYPE_BLOCK_DEF == box->dpath.item->type) {
        bt_ret = tracker_goto_first_item_int__block(tk, TRUE, bst);
        while (BITPUNCH_OK == bt_ret) {
            bt_ret = tracker_goto_next_item_int__block(tk, TRUE, bst);
        }
    } else {
        bt_ret = tracker_goto_first_item_internal(tk, bst);
        while (BITPUNCH_OK == bt_ret) {
            bt_ret = tracker_goto_next_item_internal(tk, bst);
        }
    }
    if (BITPUNCH_NO_ITEM == bt_ret) {
        assert(-1 != tk->item_offset);
        bt_ret = box_set_end_offset(tk->box, tk->item_offset,
                                    BOX_END_OFFSET_USED, bst);
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
    bt_ret = tracker_goto_first_item_int__block(tk, TRUE, bst);
    while (BITPUNCH_OK == bt_ret) {
        bt_ret = tracker_get_item_size_internal(tk, &subitem_size, bst);
        if (BITPUNCH_OK != bt_ret) {
            break ;
        }
        max_subitem_size = MAX(max_subitem_size, subitem_size);
        bt_ret = tracker_goto_next_item_int__block(tk, TRUE, bst);
    }
    if (BITPUNCH_NO_ITEM == bt_ret) {
        bt_ret = box_set_used_size(box, max_subitem_size, bst);
    }
    tracker_delete(tk);
    return bt_ret;
}

static bitpunch_status_t
box_get_n_items__block(struct box *box, int64_t *item_countp,
                       struct browse_state *bst)
{
    DBG_BOX_DUMP(box);
    return box_get_n_statements_internal(box, STATEMENT_TYPE_FIELD, 0,
                                         item_countp, bst);
}

static bitpunch_status_t
tracker_get_item_key__block(struct tracker *tk,
                            enum expr_value_type *key_typep,
                            union expr_value *keyp,
                            int *nth_twinp,
                            struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    assert(NULL != tk->cur.u.block.field);
    if (NULL != key_typep) {
        *key_typep = EXPR_VALUE_TYPE_STRING;
    }
    if (NULL != keyp) {
        keyp->string.str = tk->cur.u.block.field->nstmt.name;
        keyp->string.len = strlen(tk->cur.u.block.field->nstmt.name);
    }
    if (NULL != nth_twinp) {
        /* field names are unique */
        *nth_twinp = 0;
    }
    return BITPUNCH_OK;
}


static bitpunch_status_t
tracker_goto_item_int__block(struct tracker *tk,
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
        tracker_set_field_internal__block(tk, field, bst);
        return BITPUNCH_OK;
    }
    // recurse into anonymous struct's fields
    xtk = tracker_dup(tk);
    do {
        tracker_set_field_internal__block(xtk, field, bst);
        bt_ret = tracker_enter_item_internal(xtk, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(xtk);
            return bt_ret;
        }
        if (0 != (tk->flags & TRACKER_REVERSED)) {
            stit = box_riter_statements(xtk->box, STATEMENT_TYPE_FIELD, 0);
        } else {
            stit = box_iter_statements(xtk->box, STATEMENT_TYPE_FIELD, 0);
        }
        bt_ret = box_iter_statements_next_internal(xtk->box, &stit,
                                                   &stmt, bst);
        if (BITPUNCH_OK != bt_ret) {
            tracker_delete(xtk);
            return bt_ret;
        }
        field = (const struct field *)stmt;
    } while (NULL == field->nstmt.name
             && 0 == (field->nstmt.stmt.stmt_flags & FIELD_FLAG_HIDDEN));
    tracker_set(tk, xtk);
    tracker_delete(xtk);
    tracker_set_field_internal__block(tk, field, bst);
    return BITPUNCH_OK;
}

static int
tracker_in_anonymous_block(struct tracker *tk)
{
    struct box *unfiltered_box;
    struct box *parent_box;

    unfiltered_box = box_get_unfiltered_parent(tk->box);
    parent_box = unfiltered_box->parent_box;
    return (NULL != parent_box
            && AST_NODE_TYPE_BLOCK_DEF == parent_box->dpath.item->type
            && NULL != unfiltered_box->track_path.u.block.field
            && NULL == unfiltered_box->track_path.u.block.field->nstmt.name
            && 0 == (unfiltered_box->track_path.u.block.field
                     ->nstmt.stmt.stmt_flags & FIELD_FLAG_HIDDEN));
}

static bitpunch_status_t
tracker_goto_first_item_int__block(struct tracker *tk, int flat,
                                   struct browse_state *bst)
{
    struct statement_iterator stit;
    const struct statement *stmt;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    if (!flat && NULL != tk->cur.u.block.field) {
        // return to base, non-anonymous level
        while (tracker_in_anonymous_block(tk)) {
            bt_ret = tracker_return_internal(tk, bst);
            assert(BITPUNCH_OK == bt_ret);
        }
    }
    bt_ret = tracker_set_item_offset_at_box(tk, tk->box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    if (0 != (tk->flags & TRACKER_REVERSED)) {
        stit = box_riter_statements(tk->box, STATEMENT_TYPE_FIELD, 0);
    } else {
        stit = box_iter_statements(tk->box, STATEMENT_TYPE_FIELD, 0);
    }
    bt_ret = box_iter_statements_next_internal(tk->box,
                                               &stit, &stmt, bst);
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
    return tracker_goto_item_int__block(tk, (const struct field *)stmt,
                                        flat, bst);
}


static bitpunch_status_t
tracker_goto_next_item_int__block(struct tracker *tk, int flat,
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
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)
            && BLOCK_TYPE_STRUCT == tk->box->dpath.item->u.block_def.type) {
            int64_t item_size;

            bt_ret = tracker_get_item_size_internal(tk, &item_size, bst);
            if (BITPUNCH_OK != bt_ret) {
                DBG_TRACKER_CHECK_STATE(tk);
                return bt_ret;
            }
            DBG_TRACKER_CHECK_STATE(tk);
            if (reversed) {
                tk->item_offset -= item_size;
            } else {
                tk->item_offset += item_size;
            }
        }
        tracker_reset_item(tk);
        if (reversed) {
            stit = box_riter_statements_from(
                tk->box, (const struct statement *)tk->cur.u.block.field, 0);
        } else {
            stit = box_iter_statements_from(
                tk->box, (const struct statement *)tk->cur.u.block.field, 0);
        }
        bt_ret = box_iter_statements_next_internal(tk->box, &stit,
                                                   &stmt, bst);
        if (BITPUNCH_NO_ITEM != bt_ret) {
            break ;
        }
        if (!flat && tracker_in_anonymous_block(tk)) {
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
    return tracker_goto_item_int__block(tk, (const struct field *)stmt,
                                        flat, bst);
}

static bitpunch_status_t
tracker_goto_first_item__block(struct tracker *tk,
                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_goto_first_item_int__block(tk, FALSE, bst);
    // skip hidden fields
    while (BITPUNCH_OK == bt_ret
           && 0 != (tk->cur.u.block.field->nstmt.stmt.stmt_flags
                    & FIELD_FLAG_HIDDEN)) {
        bt_ret = tracker_goto_next_item_int__block(tk, FALSE, bst);
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_next_item__block(struct tracker *tk,
                              struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    // skip hidden fields
    do {
        bt_ret = tracker_goto_next_item_int__block(tk, FALSE, bst);
    } while (BITPUNCH_OK == bt_ret
             && 0 != (tk->cur.u.block.field->nstmt.stmt.stmt_flags
                      & FIELD_FLAG_HIDDEN));
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_nth_item__block(struct tracker *tk, int64_t index,
                             struct browse_state *bst)
{
    struct tracker *xtk;
    bitpunch_status_t bt_ret;
    int64_t cur_idx;

    DBG_TRACKER_DUMP(tk);
    xtk = tracker_dup(tk);
    bt_ret = tracker_goto_first_item__block(xtk, bst);
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
        bt_ret = tracker_goto_next_item__block(xtk, bst);
    }
    tracker_set(tk, xtk);
    tracker_delete(xtk);
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_named_item__block(struct tracker *tk, const char *name,
                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    bt_ret = tracker_goto_first_item_int__block(tk, FALSE, bst);
    while (BITPUNCH_OK == bt_ret) {
        if (NULL != tk->cur.u.block.field->nstmt.name
            && 0 == strcmp(tk->cur.u.block.field->nstmt.name, name)) {
            return BITPUNCH_OK;
        }
        bt_ret = tracker_goto_next_item_int__block(tk, FALSE, bst);
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_next_key_match__block(struct tracker *tk,
                                   union expr_value index,
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
    const struct array *array;
    bitpunch_status_t bt_ret;
    int64_t child_item_size;
    union expr_value item_count;

    array = &tk->dpath->item->u.array;
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
    item_size = ast_node_get_min_span_size(
        box->dpath.item->u.array.item_type.item);
    return box_set_used_size(box, n_elems * item_size, bst);
}

static bitpunch_status_t
box_compute_slack_size__container_slack(struct box *box,
                                         struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t max_slack_offset;

    DBG_BOX_DUMP(box);
    assert(NULL != box->parent_box);
    /* slack child is limited by parent's maximum slack offset */
    bt_ret = box_get_children_slack(box->parent_box, &max_slack_offset,
                                    bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_end_offset(box, max_slack_offset, BOX_END_OFFSET_SLACK,
                              bst);
}

static bitpunch_status_t
box_get_children_slack__struct(struct box *box,
                               int64_t *max_slack_offsetp,
                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t max_offset;
    struct tracker *tk;
    struct statement_iterator stit;
    const struct field *cur_field;

    DBG_BOX_DUMP(box);
    bt_ret = box_compute_max_span_size(box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    max_offset = box->end_offset_max_span;

    /* To compute the maximum slack offset, jump to first non-slack
     * field following a slack field. For this, first find this field,
     * then browse backwards all trailing non-slack fields. */
    stit = box_iter_statements(box, STATEMENT_TYPE_FIELD, 0);
    while (TRUE) {
        bt_ret = box_iter_statements_next_internal(
            box, &stit, (const struct statement **)&cur_field, bst);
        if (BITPUNCH_NO_ITEM == bt_ret) {
            break ;
        }
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        /* FIXME dynamic need_slack eval */
        if (0 != (cur_field->nstmt.stmt.stmt_flags
                  & FIELD_FLAG_SLACK_TRAILER)) {
            break ;
        }
    }
    if (BITPUNCH_NO_ITEM == bt_ret) {
        /* no trailing non-slack field */
        *max_slack_offsetp = max_offset;
        return BITPUNCH_OK;
    }
    tk = tracker_new(box);
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    bt_ret = tracker_goto_field_internal(tk, cur_field, TRUE, bst);
    if (BITPUNCH_OK == bt_ret) {
        *max_slack_offsetp = tk->item_offset;
    }
    tracker_delete(tk);
    return bt_ret;
}

static bitpunch_status_t
box_get_children_slack__from_parent(struct box *box,
                                    int64_t *max_slack_offsetp,
                                    struct browse_state *bst)
{
    DBG_BOX_DUMP(box);
    return box_get_children_slack(box->parent_box, max_slack_offsetp,
                                  bst);
}


static bitpunch_status_t
tracker_get_item_key__array_generic(struct tracker *tk,
                                    enum expr_value_type *key_typep,
                                    union expr_value *keyp,
                                    int *nth_twinp,
                                    struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    assert(-1 != tk->cur.u.array.index);
    if (NULL != key_typep) {
        *key_typep = EXPR_VALUE_TYPE_INTEGER;
    }
    if (NULL != keyp) {
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
    enum expr_value_type *key_typep,
    union expr_value *keyp,
    int *nth_twinp,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    struct ast_node *key_expr;
    union expr_value item_key;
    struct box *filtered_box;

    DBG_TRACKER_DUMP(tk);
    assert(-1 != tk->cur.u.array.index);
    assert(AST_NODE_TYPE_BLOCK_DEF == tk->dpath->item->type);
    bt_ret = tracker_create_item_box_internal(tk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    bt_ret = box_apply_filters(tk->item_box, &filtered_box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    key_expr = ast_node_get_key_expr(tk->box->dpath.item);
    bt_ret = expr_evaluate_value_internal(key_expr, filtered_box,
                                          &item_key, bst);
    box_delete(filtered_box);
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
            return bt_ret;
        }
    }
    if (NULL != key_typep) {
        *key_typep = key_expr->u.rexpr.value_type;
    }
    if (NULL != keyp) {
        *keyp = item_key;
    }
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_get_item_key__indexed_array(struct tracker *tk,
                                    enum expr_value_type *key_typep,
                                    union expr_value *keyp,
                                    int *nth_twinp,
                                    struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_get_item_key__indexed_array_internal(
        tk, 0 /* count twins from first item */,
        key_typep, keyp, nth_twinp, bst);
}

static bitpunch_status_t
box_get_n_items__array_non_slack(struct box *box, int64_t *item_countp,
                                 struct browse_state *bst)
{
    const struct array *array;
    union expr_value item_count;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        bitpunch_status_t bt_ret;

        array = &box->dpath.item->u.array;
        assert(array->item_count->u.rexpr.value_type
               == EXPR_VALUE_TYPE_INTEGER);
        /* root box is a block so an array always has a parent box */
        assert(NULL != box->parent_box);
        bt_ret = expr_evaluate_value_internal(array->item_count,
                                              box->parent_box,
                                              &item_count, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
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
    bitpunch_status_t bt_ret;
    const struct ast_node *node;
    int64_t item_size;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        bt_ret = box_compute_slack_size(box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        assert(-1 != box->end_offset_slack);
        node = box->dpath.item;
        item_size = ast_node_get_min_span_size(node->u.array.item_type.item);
        if (0 == item_size) {
            return box_error(
                BITPUNCH_DATA_ERROR, box, node->u.array.item_type.item, bst,
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
            assert(0 != (box->dpath.item->u.array.item_type.item->flags
                         & ASTFLAG_CONTAINS_LAST_STMT));
            if (box->u.array_generic.n_items != n_items) {
                tracker_delete(tk);
                return box_error(
                    BITPUNCH_DATA_ERROR, box, box->dpath.item, bst,
                    "'last' keyword triggered in array item %"PRIi64" that "
                    "was not the last item of the array of size %"PRIi64"",
                    (n_items - 1), box->u.array_generic.n_items);
            }
        } else {
            box->u.array_generic.n_items = n_items;
        }
        bt_ret = box_set_end_offset(box, tk->item_offset,
                                    BOX_END_OFFSET_USED, bst);
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
filter_read_value__bytes(const struct ast_node *item_filter,
                         struct box *scope,
                         int64_t item_offset,
                         int64_t item_size,
                         enum expr_value_type *typep,
                         union expr_value *valuep,
                         struct browse_state *bst)
{
    return read_value_bytes(scope, item_offset, item_size,
                            typep, valuep, bst);
}

static bitpunch_status_t
filter_read_value__interpreter(const struct ast_node *item_filter,
                               struct box *scope,
                               int64_t item_offset,
                               int64_t item_size,
                               enum expr_value_type *typep,
                               union expr_value *valuep,
                               struct browse_state *bst)
{
    const char *item_data;

    item_data = scope->file_hdl->bf_data + item_offset;
    return interpreter_rcall_read_value(item_filter,
                                        item_data, item_size,
                                        typep, valuep, bst);
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
    tk->dpath = item_dpath;
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_goto_first_item__array_non_slack(struct tracker *tk,
                                         struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_goto_first_item__array_generic(
        tk, &tk->box->dpath.item->u.array.item_type, bst);
}

static bitpunch_status_t
tracker_goto_first_item__array_slack(struct tracker *tk,
                                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    bt_ret = box_compute_slack_size(tk->box, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != tk->box->end_offset_slack);
    /* slack arrays require a maintained item offset to browse their
     * elements */
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    tk->item_offset = tk->box->start_offset_used;
    tk->cur.u.array.index = 0;
    tk->dpath = &tk->box->dpath.item->u.array.item_type;
    DBG_TRACKER_CHECK_STATE(tk);

    /* check if there's size for at least one element */
    WITH_EXPECTED_ERROR(BITPUNCH_OUT_OF_BOUNDS_ERROR, {
        bt_ret = tracker_compute_item_size(tk, bst);
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
    int is_last;

    DBG_TRACKER_DUMP(tk);
    is_last = FALSE;
    if (0 != (tk->dpath->item->flags & ASTFLAG_CONTAINS_LAST_STMT)) {
        bt_ret = tracker_create_item_box_internal(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = box_apply_filters(tk->item_box, &filtered_box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = box_get_first_statement_internal(filtered_box,
                                                  STATEMENT_TYPE_LAST,
                                                  0, NULL, bst);
        box_delete(filtered_box);
        switch (bt_ret) {
        case BITPUNCH_OK:
            // explicit 'last': last item of the array
            is_last = TRUE;
            break ;
        case BITPUNCH_NO_ITEM:
            break ;
        default:
            return bt_ret;
        }
    }
    if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        bt_ret = tracker_compute_item_offset(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = tracker_compute_item_size(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        item_size = tk->item_size;
        box_delete(tk->item_box);
        tk->item_box = NULL;
        tk->item_offset += tk->item_size;
        if (0 != (tk->dpath->u.item.flags
                  & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            tk->item_size = -1;
        }
    }
    ++tk->cur.u.array.index;
    if ((-1 != tk->box->u.array_generic.n_items
         && tk->cur.u.array.index == tk->box->u.array_generic.n_items)
        || (-1 == tk->box->u.array_generic.n_items
            && tk->item_offset == tk->box->end_offset_slack)
        || is_last) {
        bt_ret = tracker_set_end(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        return BITPUNCH_NO_ITEM;
    }
    /* check new item */
    if (-1 == tk->box->u.array_generic.n_items) {
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
        tracker_reset_item(tk);
        if (0 != (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
            tk->item_offset -= item_size;
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
    bitpunch_status_t bt_ret;
    int64_t item_size;
    struct tracker *xtk;

    DBG_TRACKER_DUMP(tk);
    bt_ret = box_get_n_items_internal(tk->box, NULL, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != tk->box->u.array_generic.n_items);
    if (index >= tk->box->u.array_generic.n_items) {
        return BITPUNCH_NO_ITEM;
    }
    if (0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        tk->cur.u.array.index = index;
        tk->dpath = &tk->box->dpath.item->u.array.item_type;
        tk->flags &= ~TRACKER_AT_END;
        DBG_TRACKER_CHECK_STATE(tk);
        return BITPUNCH_OK;
    }
    xtk = tracker_dup(tk);
    xtk->dpath = &tk->box->dpath.item->u.array.item_type;
    xtk->flags &= ~TRACKER_AT_END;
    box_delete(xtk->item_box);
    xtk->item_box = NULL;
    item_size = ast_node_get_min_span_size(xtk->dpath->item);
    /* no item cleanup, transform item info instead */
    xtk->item_offset = xtk->box->start_offset_used + index * item_size;
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
    struct tracker *xtk;
    struct dpath_node *item_dpath;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    item_dpath = &tk->box->dpath.item->u.array.item_type;
    xtk = tracker_dup(tk);
    if (index < tk->box->u.array.last_cached_index) {
        int64_t mark;

        mark = box_array_get_index_mark(xtk->box, index);
        tracker_goto_mark_internal(xtk, item_dpath, mark, bst);
    } else {
        tracker_goto_last_cached_item_internal(xtk, bst);
        if (NULL == xtk->dpath) {
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
                union expr_value index_key;

                bt_ret = tracker_get_item_key_internal(xtk, NULL,
                                                       &index_key, bst);
                if (BITPUNCH_OK != bt_ret) {
                    break ;
                }
                tracker_index_cache_add_item(xtk, index_key);
                expr_value_destroy(
                    ast_node_get_key_type(xtk->box->dpath.item), index_key);
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
            union expr_value dummy;

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
    item_dpath = &tk->box->dpath.item->u.array.item_type;
    if (0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        tk->dpath = item_dpath;
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
    union expr_value item_key;

    DBG_TRACKER_DUMP(tk);
    item_key.string.str = name;
    item_key.string.len = strlen(name);
    return tracker_goto_first_item_with_key_internal(tk, item_key, bst);
}


static bitpunch_status_t
tracker_goto_next_key_match__generic(struct tracker *tk,
                                     struct ast_node *key_expr,
                                     union expr_value key,
                                     struct track_path search_boundary,
                                     struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    union expr_value item_key;
    struct box *filtered_box;

    DBG_TRACKER_DUMP(tk);
    while (search_boundary.type == TRACK_PATH_NOTYPE
           || ! track_path_eq(tk->cur, search_boundary)) {
        bt_ret = tracker_create_item_box_internal(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = box_apply_filters(tk->item_box, &filtered_box, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        bt_ret = expr_evaluate_value_internal(key_expr, filtered_box,
                                              &item_key, bst);
        box_delete(filtered_box);
        if (BITPUNCH_OK != bt_ret) {
            tracker_error_add_tracker_context(
                tk, bst, "when evaluating item key expression");
            return bt_ret;
        }
        if (tk->cur.u.array.index ==
            tk->box->u.array.last_cached_index + 1) {
            tracker_index_cache_add_item(tk, item_key);
        }
        if (0 == expr_value_cmp(key_expr->u.rexpr.value_type,
                                item_key, key)) {
            return BITPUNCH_OK;
        }
        bt_ret = tracker_goto_next_item_internal(tk, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    return BITPUNCH_NO_ITEM;
}

static bitpunch_status_t
tracker_goto_next_key_match__array(struct tracker *tk,
                                   union expr_value index,
                                   struct track_path search_boundary,
                                   struct browse_state *bst)
{
    struct ast_node *item_type;
    struct ast_node *key_expr;

    DBG_TRACKER_DUMP(tk);
    assert(AST_NODE_TYPE_ARRAY == tk->box->dpath.item->type);
    item_type = tk->box->dpath.item->u.array.item_type.item;
    if (AST_NODE_TYPE_BLOCK_DEF != item_type->type) {
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, item_type, bst,
                             "only arrays which items are structures can "
                             "be accessed through named index");
    }
    key_expr = ast_node_get_key_expr(tk->box->dpath.item);
    if (NULL == key_expr) {
        return tracker_error(BITPUNCH_INVALID_PARAM, tk, key_expr, bst,
                             "array is not indexed");
    }
    return tracker_goto_next_key_match__generic(tk, key_expr, index,
                                                search_boundary, bst);
}

static bitpunch_status_t
tracker_goto_next_item_with_key__default(struct tracker *tk,
                                         union expr_value item_key,
                                         struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_error(BITPUNCH_INVALID_PARAM, tk, NULL, bst,
                         "not an indexed container");
}

static bitpunch_status_t
tracker_goto_next_item_with_key__indexed_array_internal(
    struct tracker *tk,
    union expr_value item_key,
    int64_t end_index,
    struct browse_state *bst)
{
    struct index_cache_iterator twin_iter;
    bitpunch_status_t bt_ret;
    struct track_path in_slice_path;
    struct track_path item_path;
    struct tracker *xtk;
    const struct ast_node *node;
 
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
    node = tk->box->dpath.item;
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
            bt_ret = node->u.container.b_tk.goto_next_key_match(
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
                                               union expr_value item_key,
                                               struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_goto_next_item_with_key__indexed_array_internal(
        tk, item_key, -1 /* no end boundary */, bst);
}

static bitpunch_status_t
tracker_goto_nth_item_with_key__default(struct tracker *tk,
                                        union expr_value item_key,
                                        int nth_twin,
                                        struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_error(BITPUNCH_INVALID_PARAM, tk, NULL, bst,
                         "not an indexed container");
}

static bitpunch_status_t
tracker_goto_nth_item_with_key__indexed_array_internal(
    struct tracker *tk,
    union expr_value item_key,
    int nth_twin,
    int64_t from_index,
    int64_t end_index,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int cur_twin;
    struct tracker *xtk;
    const struct ast_node *node;

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
    node = tk->box->dpath.item;
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
                bt_ret = node->u.container.b_tk.goto_next_key_match(
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
    struct tracker *tk, union expr_value item_key, int nth_twin,
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
    union expr_value byte_count;
    bitpunch_status_t bt_ret;

    assert(tk->dpath->item->u.byte_array.size->u.rexpr.value_type
           == EXPR_VALUE_TYPE_INTEGER);
    bt_ret = expr_evaluate_value_internal(tk->dpath->item->u.byte_array.size,
                                          tk->box, &byte_count, bst);
    if (BITPUNCH_OK != bt_ret) {
        // FIXME more appropriate context
        tracker_error_add_box_context(
            tk->box, bst, "when evaluating byte array size expression");
        return bt_ret;
    }
    if (byte_count.integer < 0) {
        return box_error(
            BITPUNCH_DATA_ERROR,
            tk->box, tk->dpath->item->u.byte_array.size, bst,
            "evaluation of byte array size gives negative value (%"PRIi64")",
            byte_count.integer);
    }
    *item_sizep = byte_count.integer;
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_item_size_internal__byte_array_interpreter_size(
    struct box *box,
    const struct dpath_node *item_dpath,
    int64_t item_offset,
    int64_t *item_sizep,
    struct browse_state *bst)
{
    const struct ast_node *interpreter;
    const char *item_data;
    struct ast_node *params;
    size_t value_size;
    bitpunch_status_t bt_ret;
    int64_t max_slack_offset;

    DBG_BOX_DUMP(box);
    interpreter = item_dpath->filter_defining_size;
    assert(NULL != interpreter->u.rexpr_interpreter.get_size_func);
    item_data = box->file_hdl->bf_data + item_offset;
    params = interpreter_rcall_get_params(interpreter);

    if ((box->flags & (COMPUTING_SPAN_SIZE |
                       COMPUTING_MAX_SLACK_OFFSET))) {
        max_slack_offset = box_get_known_end_offset_mask(
            box, (BOX_END_OFFSET_MAX_SPAN |
                  BOX_END_OFFSET_SLACK |
                  BOX_END_OFFSET_PARENT));
    } else {
        bt_ret = box_get_children_slack(box, &max_slack_offset, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    if (-1 == interpreter->u.rexpr_interpreter.get_size_func(
            &value_size, item_data,
            max_slack_offset - item_offset,
            params)) {
        return box_error(BITPUNCH_DATA_ERROR, box, item_dpath->item, bst,
                         "interpreter couldn't return field's size");
    }
    *item_sizep = value_size;
    return BITPUNCH_OK;
}

static bitpunch_status_t
tracker_compute_item_size__byte_array_slack(
    struct tracker *tk,
    int64_t *item_sizep,
    struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t max_slack_offset;

    if (NULL != tk->dpath->filter_defining_size) {
        return box_compute_item_size_internal__byte_array_interpreter_size(
            tk->box, tk->dpath, tk->item_offset, item_sizep, bst);
    }
    DBG_TRACKER_DUMP(tk);
    if ((tk->box->flags & COMPUTING_MAX_SLACK_OFFSET)) {
        bt_ret = box_compute_max_span_size(tk->box, bst);
        max_slack_offset = tk->box->end_offset_max_span;
    } else {
        bt_ret = box_get_children_slack(tk->box, &max_slack_offset, bst);
    }
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    /* slack byte arrays use the whole available slack space */
    *item_sizep = max_slack_offset - tk->item_offset;
    return BITPUNCH_OK;
}

static bitpunch_status_t
box_compute_used_size__byte_array_dynamic_size(struct box *box,
                                               struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int64_t size;

    DBG_BOX_DUMP(box);
    bt_ret = box_get_n_items_internal(box, &size, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_used_size(box, size, bst);
}

static bitpunch_status_t
box_get_n_items__byte_array_non_slack(
    struct box *box, int64_t *item_countp,
    struct browse_state *bst)
{
    const struct byte_array *byte_array;
    union expr_value size;

    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        bitpunch_status_t bt_ret;

        byte_array = &box->dpath.item->u.byte_array;
        assert(byte_array->size->u.rexpr.value_type == EXPR_VALUE_TYPE_INTEGER);
        /* root box is a block so a byte array always has a parent box */
        assert(NULL != box->parent_box);
        bt_ret = expr_evaluate_value_internal(byte_array->size,
                                              box->parent_box, &size, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
        if (size.integer < 0) {
            return box_error(BITPUNCH_DATA_ERROR, box, byte_array->size, bst,
                             "evaluation of byte array size gives "
                             "negative value (%"PRIi64")", size.integer);
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
        if (NULL != box->dpath.filter_defining_size) {
            bt_ret =
                box_compute_item_size_internal__byte_array_interpreter_size(
                    box, &box->dpath, box->start_offset_used,
                    &box->u.array_generic.n_items, bst);
        } else {
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
        }
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
tracker_goto_first_item__byte_array_generic(struct tracker *tk,
                                            struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_goto_first_item__array_generic(tk, DPATH_NODE_BYTE, bst);
}

static bitpunch_status_t
tracker_goto_first_item__byte_array_slack(struct tracker *tk,
                                          struct browse_state *bst)
{
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
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
    tk->cur.u.array.index = 0;
    tk->dpath = DPATH_NODE_BYTE;
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
    tk->dpath = DPATH_NODE_BYTE;
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
    bt_ret = box_get_n_items_internal(tk->box, &n_items, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    assert(-1 != n_items);
    if (index >= n_items) {
        return BITPUNCH_NO_ITEM;
    }
    tk->flags &= ~TRACKER_AT_END;
    tk->dpath = DPATH_NODE_BYTE;
    if (0 == (tk->flags & TRACKER_NEED_ITEM_OFFSET)) {
        tk->cur.u.array.index = index;
        DBG_TRACKER_CHECK_STATE(tk);
        return BITPUNCH_OK;
    }
    tk->item_offset = tk->box->start_offset_used + index;
    tk->item_size = 1;
    tk->cur.u.array.index = index;
    return tracker_check_item(tk, bst);
}


static struct box *
box_array_slice_get_ancestor_array(struct box *box)
{
    struct box *array_box;

    DBG_BOX_DUMP(box);
    array_box = box->parent_box;
    while (AST_NODE_TYPE_ARRAY != array_box->dpath.item->type &&
           AST_NODE_TYPE_BYTE_ARRAY != array_box->dpath.item->type &&
           AST_NODE_TYPE_AS_BYTES != array_box->dpath.item->type &&
           AST_NODE_TYPE_FILTERED != array_box->dpath.item->type) {
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
    struct box *array_box;
    const struct ast_node *array_node;
    struct ast_node *item_type;
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    array_box = box_array_slice_get_ancestor_array(box);
    array_node = array_box->dpath.item;
    item_type = ast_node_get_target_item(
        array_node->u.array.item_type.item);
    if (0 == (array_node->u.array.item_type.u.item.flags
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
        bt_ret = array_node->u.container.b_box.compute_used_size(box, bst);
    }
    return bt_ret;
}

static bitpunch_status_t
tracker_get_item_key__array_slice(struct tracker *tk,
                                  enum expr_value_type *key_typep,
                                  union expr_value *keyp,
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
    if (ast_node_is_indexed(array_box->dpath.item)) {
        bt_ret = tracker_get_item_key__indexed_array_internal(
            tk, from_index, key_typep, keyp, nth_twinp, bst);
    } else {
        if (NULL != key_typep) {
            *key_typep = EXPR_VALUE_TYPE_INTEGER;
        }
        if (NULL != keyp) {
            assert(tk->cur.u.array.index >= from_index);
            keyp->integer = tk->cur.u.array.index - from_index;
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
        tk->cur.u.array.index = slice_box->track_path.u.array.index;
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
    bt_ret = array_box->dpath.item->u.container.b_tk.goto_nth_item(tk, index, bst);
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
    slice_box = tk->box;
    index_end = slice_box->track_path.u.array_slice.index_end;
    if (tk->cur.u.array.index + 1 == index_end) {
        bt_ret = tracker_compute_item_size(tk, bst);
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
    bt_ret = array_box->dpath.item->u.container.b_tk.goto_next_item(tk, bst);
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
    bt_ret = array_box->dpath.item->u.container.b_tk.goto_nth_item(
        tk, index_start + index, bst);
    tk->box = slice_box;
    return bt_ret;
}

static bitpunch_status_t
tracker_goto_named_item__array_slice(struct tracker *tk, const char *name,
                                     struct browse_state *bst)
{
    union expr_value item_key;

    DBG_TRACKER_DUMP(tk);
    item_key.string.str = name;
    item_key.string.len = strlen(name);
    return tracker_goto_first_item_with_key_internal(tk, item_key, bst);
}

static bitpunch_status_t
tracker_goto_next_key_match__array_slice(struct tracker *tk,
                                         union expr_value index,
                                         struct track_path search_boundary,
                                         struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    // should not be called, only used internally by indexed arrays
    return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst, NULL);
}

static bitpunch_status_t
tracker_goto_next_item_with_key__array_slice(struct tracker *tk,
                                             union expr_value item_key,
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
    if (ast_node_is_indexed(array_box->dpath.item)) {
        bt_ret = tracker_goto_next_item_with_key__indexed_array_internal(
            tk, item_key, end_index, bst);
    } else {
        bt_ret = array_box->dpath.item->u.container.b_tk.goto_next_item_with_key(
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
    struct tracker *tk, union expr_value item_key, int nth_twin,
    struct browse_state *bst)
{
    struct box *slice_box;
    struct box *array_box;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    slice_box = tk->box;
    array_box = box_array_slice_get_ancestor_array(slice_box);

    tk->box = array_box;
    if (ast_node_is_indexed(array_box->dpath.item)) {
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
        bt_ret = array_box->dpath.item->u.container.b_tk.goto_nth_item_with_key(
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
    assert(TRACK_PATH_ARRAY_SLICE == box->track_path.type);
    bt_ret = box_get_n_items__slice_generic(box, &n_bytes, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    return box_set_used_size(box, n_bytes, bst);
}

static bitpunch_status_t
tracker_get_item_key__byte_slice(struct tracker *tk,
                                 enum expr_value_type *key_typep,
                                 union expr_value *keyp,
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
    if (NULL != key_typep) {
        *key_typep = EXPR_VALUE_TYPE_INTEGER;
    }
    if (NULL != keyp) {
        assert(tk->cur.u.array.index >= from_index);
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
                                          union expr_value item_key,
                                          struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst, NULL);
}

static bitpunch_status_t
tracker_goto_nth_item_with_key__not_impl(
    struct tracker *tk, union expr_value item_key, int nth_twin,
    struct browse_state *bst)
{
    DBG_TRACKER_DUMP(tk);
    return tracker_error(BITPUNCH_NOT_IMPLEMENTED, tk, NULL, bst, NULL);
}


static bitpunch_status_t
box_get_n_items__as_bytes(struct box *box, int64_t *item_countp,
                          struct browse_state *bst)
{
    DBG_BOX_DUMP(box);
    if (-1 == box->u.array_generic.n_items) {
        /* end_offset_used should have been set at box creation */
        assert(-1 != box->end_offset_used);
        box->u.array_generic.n_items =
            box->end_offset_used - box->start_offset_used;
    }
    if (NULL != item_countp) {
        *item_countp = box->u.array_generic.n_items;
    }
    return BITPUNCH_OK;
}


/*
 * setup backends
 */

static void
browse_setup_backends__filter__generic(struct dpath_node *dpath)
{
    struct ast_node *filter;
    struct filter_backend *b_filter = NULL;

    filter = dpath->filter;
    b_filter = &filter->u.rexpr_filter.b_filter;
    memset(b_filter, 0, sizeof (*b_filter));

    switch (filter->type) {
    case AST_NODE_TYPE_REXPR_INTERPRETER: {
        const struct interpreter *interpreter;

        interpreter = filter->u.rexpr_interpreter.interpreter;
        if (interpreter->semantic_type == EXPR_VALUE_TYPE_BYTES) {
            // for bytes -> bytes filters, the default read
            // implementation does not use the filter, for that
            // the 'filter' operator (*) has to be used to filter
            // contents.
            b_filter->read_value = filter_read_value__bytes;
        } else {
            b_filter->read_value = filter_read_value__interpreter;
        }
        break ;
    }
    case AST_NODE_TYPE_REXPR_AS_TYPE:
        b_filter->read_value = filter_read_value__bytes;
        break ;
    default:
        assert(0);
    }
}


static void
browse_setup_backends__box__block(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct box_backend *b_box = NULL;

    item = dpath->item;
    b_box = &item->u.container.b_box;
    memset(b_box, 0, sizeof (*b_box));

    if (0 != (item->flags & ASTFLAG_IS_ROOT_BLOCK)) {
        b_box->compute_used_size = box_compute_used_size__as_max_span;
        b_box->compute_slack_size = box_compute_slack_size__block_file;
    } else if (0 != (item->u.item.flags & ITEMFLAG_NEED_SLACK)) {
        b_box->compute_slack_size = box_compute_slack_size__container_slack;
    } else {
        b_box->compute_slack_size = box_compute_slack_size__from_parent;
    }
    if (0 == (item->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_box->compute_used_size = box_compute_used_size__static_size;
    } else if (0 != (item->flags & ASTFLAG_HAS_FOOTER)) {
        b_box->compute_used_size = box_compute_used_size__as_max_span;
    } else if (BLOCK_TYPE_STRUCT == item->u.block_def.type) {
        b_box->compute_used_size = box_compute_used_size__packed_dynamic_size;
    } else /* union */ {
        b_box->compute_used_size = box_compute_used_size__union_dynamic_size;
    }
    if (0 != (item->flags & ASTFLAG_IS_ROOT_BLOCK)) {
    } else if (0 != (item->u.item.flags & ITEMFLAG_NEED_SLACK)) {
        b_box->compute_slack_size = box_compute_slack_size__container_slack;
    } else {
        b_box->compute_slack_size = box_compute_slack_size__from_parent;
    }
    if (!TAILQ_EMPTY(item->u.block_def.block_stmt_list.span_list)) {
        b_box->compute_min_span_size = box_compute_min_span_size__span_expr;
        b_box->compute_max_span_size = box_compute_max_span_size__span_expr;
    } else {
        b_box->compute_min_span_size =
            box_compute_min_span_size__as_hard_min;
        if (0 == (item->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            b_box->compute_max_span_size =
                box_compute_max_span_size__as_used;
        } else {
            b_box->compute_max_span_size =
                box_compute_max_span_size__as_slack;
        }
    }
    if (BLOCK_TYPE_STRUCT == item->u.block_def.type) {
        b_box->get_max_slack_offset = box_get_children_slack__struct;
    }
    b_box->get_n_items = box_get_n_items__block;
}

static void
browse_setup_backends__tracker__block(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct tracker_backend *b_tk = NULL;

    item = dpath->item;
    b_tk = &item->u.container.b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

    b_tk->get_item_key = tracker_get_item_key__block;
    if (0 == (item->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_tk->compute_item_size = tracker_compute_item_size__static_size;
    } else {
        b_tk->compute_item_size = tracker_compute_item_size__item_box;
    }
    b_tk->goto_first_item = tracker_goto_first_item__block;
    b_tk->goto_next_item = tracker_goto_next_item__block;
    b_tk->goto_nth_item = tracker_goto_nth_item__block;
    b_tk->goto_named_item = tracker_goto_named_item__block;
    b_tk->goto_next_key_match = tracker_goto_next_key_match__block;
    b_tk->goto_next_item_with_key =
        tracker_goto_next_item_with_key__default;
    b_tk->goto_nth_item_with_key =
        tracker_goto_nth_item_with_key__default;
}

static void
browse_setup_backends__box__array(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct box_backend *b_box = NULL;
    const struct ast_node *item_type;

    item = dpath->item;
    b_box = &item->u.container.b_box;
    memset(b_box, 0, sizeof (*b_box));
    item_type = item->u.array.item_type.item;

    if (0 != (item->u.item.flags & ITEMFLAG_NEED_SLACK)) {
        b_box->compute_slack_size =
            box_compute_slack_size__container_slack;
    } else {
        b_box->compute_slack_size = box_compute_slack_size__from_parent;
    }
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->compute_max_span_size = box_compute_max_span_size__as_slack;
    if (0 == (item->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_box->compute_used_size = box_compute_used_size__static_size;
    } else if (0 == (item_type->u.item.flags
                     & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_box->compute_used_size =
            box_compute_used_size__array_static_item_size;
    } else {
        b_box->compute_used_size =
            box_compute_used_size__packed_dynamic_size;
    }
    if (0 != (item_type->flags & ASTFLAG_CONTAINS_LAST_STMT)) {
        if (NULL != item->u.array.item_count) {
            b_box->get_n_items = box_get_n_items__array_non_slack_with_last;
        } else {
            b_box->get_n_items = box_get_n_items__by_iteration;
        }
    } else if (NULL != item->u.array.item_count) {
        b_box->get_n_items = box_get_n_items__array_non_slack;
    } else if (0 == (item_type->u.item.flags
                     & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_box->get_n_items =
            box_get_n_items__array_slack_static_item_size;
    } else {
        b_box->get_n_items = box_get_n_items__by_iteration;
    }
}

static void
browse_setup_backends__tracker__array(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct tracker_backend *b_tk = NULL;
    const struct ast_node *item_type;

    item = dpath->item;
    b_tk = &item->u.container.b_tk;
    memset(b_tk, 0, sizeof (*b_tk));
    item_type = item->u.array.item_type.item;

    if (AST_NODE_TYPE_BLOCK_DEF == item_type->type
        && NULL != ast_node_get_key_expr(item)) {
        b_tk->get_item_key = tracker_get_item_key__indexed_array;
    } else {
        b_tk->get_item_key = tracker_get_item_key__array_generic;
    }
    if (0 == (item->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_tk->compute_item_size = tracker_compute_item_size__static_size;
    } else if (NULL != item->u.array.item_count
               && 0 == (item_type->u.item.flags
                        & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_tk->compute_item_size =
            tracker_compute_item_size__array_static_item_size;
    } else {
        b_tk->compute_item_size = tracker_compute_item_size__item_box;
    }
    if (NULL != item->u.array.item_count) {
        b_tk->goto_first_item = tracker_goto_first_item__array_non_slack;
    } else {
        b_tk->goto_first_item = tracker_goto_first_item__array_slack;
    }
    b_tk->goto_next_item = tracker_goto_next_item__array;
    if (0 == (item_type->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_tk->goto_nth_item =
            tracker_goto_nth_item__array_static_item_size;
    } else if (NULL != item->u.array.item_count) {
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
browse_setup_backends__tracker__byte(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct tracker_backend *b_tk = NULL;

    item = dpath->item;
    b_tk = &item->u.container.b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

    b_tk->compute_item_size = tracker_compute_item_size__byte;
}


static void
browse_setup_backends__box__byte_array(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct box_backend *b_box = NULL;

    item = dpath->item;
    b_box = &item->u.container.b_box;
    memset(b_box, 0, sizeof (*b_box));

    if (0 != (item->u.item.flags & ITEMFLAG_NEED_SLACK)) {
        b_box->compute_slack_size =
            box_compute_slack_size__container_slack;
    } else {
        b_box->compute_slack_size = box_compute_slack_size__from_parent;
    }
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    if (0 == (item->u.item.flags & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
        b_box->compute_used_size = box_compute_used_size__static_size;
        b_box->compute_max_span_size = box_compute_max_span_size__as_used;
    } else {
        b_box->compute_used_size =
            box_compute_used_size__byte_array_dynamic_size;
        b_box->compute_max_span_size = box_compute_max_span_size__as_slack;
    }
    if (NULL != item->u.byte_array.size) {
        b_box->get_n_items = box_get_n_items__byte_array_non_slack;
    } else {
        b_box->get_n_items = box_get_n_items__byte_array_slack;
    }
}

static void
browse_setup_backends__tracker__byte_array(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct tracker_backend *b_tk = NULL;

    item = dpath->item;
    b_tk = &item->u.container.b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

    b_tk->get_item_key = tracker_get_item_key__array_generic;
    if (NULL != item->u.byte_array.size) {
        b_tk->compute_item_size =
            tracker_compute_item_size__byte_array_dynamic_size;
    } else {
        b_tk->compute_item_size =
            tracker_compute_item_size__byte_array_slack;
    }
    if (NULL != item->u.byte_array.size) {
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


static void
browse_setup_backends__box__array_slice(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct box_backend *b_box = NULL;

    item = dpath->item;
    b_box = &item->u.container.b_box;
    memset(b_box, 0, sizeof (*b_box));

    b_box->compute_slack_size = box_compute_slack_size__from_parent;
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->compute_max_span_size = box_compute_max_span_size__as_used;
    b_box->compute_used_size = box_compute_used_size__array_slice;
    b_box->get_max_slack_offset = box_get_children_slack__from_parent;
    b_box->get_n_items = box_get_n_items__slice_generic;
}

static void
browse_setup_backends__tracker__array_slice(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct tracker_backend *b_tk = NULL;

    item = dpath->item;
    b_tk = &item->u.container.b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

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
}


static void
browse_setup_backends__box__byte_slice(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct box_backend *b_box = NULL;

    item = dpath->item;
    b_box = &item->u.container.b_box;
    memset(b_box, 0, sizeof (*b_box));

    b_box->compute_slack_size = box_compute_slack_size__from_parent;
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->compute_max_span_size = box_compute_max_span_size__as_used;
    b_box->compute_used_size = box_compute_used_size__byte_slice;
    b_box->get_n_items = box_get_n_items__slice_generic;
}

static void
browse_setup_backends__tracker__byte_slice(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct tracker_backend *b_tk = NULL;

    item = dpath->item;
    b_tk = &item->u.container.b_tk;
    memset(b_tk, 0, sizeof (*b_tk));

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
}


static void
browse_setup_backends__box__as_bytes(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct box_backend *b_box = NULL;

    item = dpath->item;
    b_box = &item->u.container.b_box;
    memset(b_box, 0, sizeof (*b_box));

    b_box->compute_slack_size = box_compute_slack_size__from_parent;
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->compute_max_span_size = box_compute_max_span_size__as_slack;
    b_box->compute_used_size = box_compute_used_size__from_parent;
    b_box->get_n_items = box_get_n_items__as_bytes;
}

static void
browse_setup_backends__tracker__as_bytes(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct tracker_backend *b_tk = NULL;

    item = dpath->item;
    b_tk = &item->u.container.b_tk;
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
browse_setup_backends__box__filtered(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct box_backend *b_box = NULL;

    item = dpath->item;
    b_box = &item->u.container.b_box;
    memset(b_box, 0, sizeof (*b_box));

    b_box->compute_slack_size = NULL; // set at construction
    b_box->compute_max_span_size = NULL; // set at construction
    b_box->compute_used_size = NULL; // set at construction
    b_box->compute_min_span_size = box_compute_min_span_size__as_hard_min;
    b_box->get_n_items = box_get_n_items__as_bytes;
}

static void
browse_setup_backends__tracker__filtered(struct dpath_node *dpath)
{
    struct ast_node *item;
    struct tracker_backend *b_tk = NULL;

    item = dpath->item;
    b_tk = &item->u.container.b_tk;
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

int
browse_setup_backends(struct dpath_node *dpath)
{
    if (NULL != dpath->item) {
        switch (dpath->item->type) {
        case AST_NODE_TYPE_BLOCK_DEF:
            browse_setup_backends__box__block(dpath);
            browse_setup_backends__tracker__block(dpath);
            break ;
        case AST_NODE_TYPE_ARRAY:
            browse_setup_backends__box__array(dpath);
            browse_setup_backends__tracker__array(dpath);
            break ;
        case AST_NODE_TYPE_BYTE:
            browse_setup_backends__tracker__byte(dpath);
            break ;
        case AST_NODE_TYPE_BYTE_ARRAY:
            browse_setup_backends__box__byte_array(dpath);
            browse_setup_backends__tracker__byte_array(dpath);
            break ;
        case AST_NODE_TYPE_ARRAY_SLICE:
            browse_setup_backends__box__array_slice(dpath);
            browse_setup_backends__tracker__array_slice(dpath);
            break ;
        case AST_NODE_TYPE_BYTE_SLICE:
            browse_setup_backends__box__byte_slice(dpath);
            browse_setup_backends__tracker__byte_slice(dpath);
            break ;
        case AST_NODE_TYPE_AS_BYTES:
            browse_setup_backends__box__as_bytes(dpath);
            browse_setup_backends__tracker__as_bytes(dpath);
            break ;
        case AST_NODE_TYPE_FILTERED:
            browse_setup_backends__box__filtered(dpath);
            browse_setup_backends__tracker__filtered(dpath);
            break ;
        default:
            break ;
        }
    }
    if (NULL != dpath->filter) {
        browse_setup_backends__filter__generic(dpath);
    }
    return 0;
}

/*
 *
 */


/*
 * external API wrappers
 */

static bitpunch_status_t
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
box_read_value(struct box *box,
               enum expr_value_type *typep, union expr_value *valuep,
               struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_read_value_internal(box, typep, valuep, &bst),
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

struct tracker *
track_item_contents(struct tracker *tk,
                    struct tracker_error **errp)
{
    struct browse_state bst;
    struct tracker *item_tk;

    browse_state_init(&bst);
    item_tk = track_item_contents_internal(tk, &bst);
    (void) transmit_error(BITPUNCH_OK, &bst, errp);
    return item_tk;
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
                                 union expr_value item_key,
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
                                union expr_value item_key,
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
                               union expr_value item_key,
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
                     enum expr_value_type *key_typep,
                     union expr_value *keyp,
                     struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_get_item_key_internal(tk, key_typep, keyp, &bst),
        &bst, errp);
}


bitpunch_status_t
tracker_get_item_key_multi(struct tracker *tk,
                           enum expr_value_type *key_typep,
                           union expr_value *keyp,
                           int *nth_twinp,
                           struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_get_item_key_multi_internal(tk, key_typep, keyp,
                                            nth_twinp, &bst),
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
                        enum expr_value_type *typep,
                        union expr_value *valuep,
                        struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_read_item_value_internal(tk, typep, valuep, &bst),
        &bst, errp);
}

bitpunch_status_t
tracker_create_item_box(struct tracker *tk,
                        struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        tracker_create_item_box_internal(tk, &bst),
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


bitpunch_status_t
box_iter_statements_next(struct box *box, struct statement_iterator *it,
                         const struct statement **stmtp,
                         struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_iter_statements_next_internal(box, it, stmtp, &bst),
        &bst, errp);
}


bitpunch_status_t
box_lookup_statement(struct box *box,
                     enum statement_type stmt_type,
                     const char *stmt_name,
                     const struct named_statement **stmtp,
                     struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_lookup_statement_internal(box, stmt_type, stmt_name, stmtp,
                                      &bst),
        &bst, errp);
}

bitpunch_status_t
box_get_first_statement(struct box *box,
                        enum statement_type stmt_type,
                        int stmt_flags,
                        const struct statement **stmtp,
                        struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_get_first_statement_internal(box, stmt_type, stmt_flags,
                                         stmtp, &bst),
        &bst, errp);
}

bitpunch_status_t
box_get_n_statements(struct box *box,
                     enum statement_type stmt_type,
                     int stmt_flags,
                     int64_t *stmt_countp,
                     struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_get_n_statements_internal(box, stmt_type, stmt_flags,
                                      stmt_countp, &bst),
        &bst, errp);
}


bitpunch_status_t
box_evaluate_attribute_value(struct box *box,
                             const char *attr_name,
                             enum expr_value_type *value_typep,
                             union expr_value *eval_valuep,
                             struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_evaluate_attribute_value_internal(box, attr_name,
                                              value_typep, eval_valuep,
                                              &bst),
        &bst, errp);
}

bitpunch_status_t
box_evaluate_attribute_dpath(struct box *box,
                             const char *attr_name,
                             enum expr_dpath_type *dpath_typep,
                             union expr_dpath *eval_dpathp,
                             struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_evaluate_attribute_dpath_internal(box, attr_name,
                                              dpath_typep, eval_dpathp,
                                              &bst),
        &bst, errp);
}

bitpunch_status_t
box_evaluate_attribute(struct box *box,
                       const char *attr_name,
                       enum expr_value_type *value_typep,
                       union expr_value *eval_valuep,
                       enum expr_dpath_type *dpath_typep,
                       union expr_dpath *eval_dpathp,
                       struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_evaluate_attribute_internal(box, attr_name,
                                        value_typep, eval_valuep,
                                        dpath_typep, eval_dpathp,
                                        &bst),
        &bst, errp);
}

bitpunch_status_t
box_iter_named_exprs_next(struct box *box, tnamed_expr_iterator *it,
                          const struct named_expr **named_exprp,
                          struct tracker_error **errp)
{
    struct browse_state bst;

    browse_state_init(&bst);
    return transmit_error(
        box_iter_named_exprs_next_internal(box, it, named_exprp, &bst),
        &bst, errp);
}

/*
 *
 */
