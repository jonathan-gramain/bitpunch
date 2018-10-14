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

#include <assert.h>

#include "utils/bloom.h"
#include "core/expr_internal.h"
#include "core/debug.h"
#include "filters/array_index_cache.h"

static bitpunch_status_t
box_mark_offsets_repo_should_exist(struct box *box, int *should_existp,
                                   struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    struct ast_node_hdl *item_type;

    array = (struct filter_instance_array *)
        box->filter->ndat->u.rexpr_filter.f_instance;
    bt_ret = expr_evaluate_filter_type_internal(
        array->item_type, box, FILTER_KIND_ITEM, &item_type, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    *should_existp = 0 != (item_type->ndat->u.item.flags
                           & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC);
    return BITPUNCH_OK;
}

void
box_init_mark_offsets_repo(struct box *box)
{
    box->u.array.mark_offsets_exists = TRUE;
    ARRAY_INIT(&box->u.array.mark_offsets, 0,
               struct index_cache_mark_offset);
}

static int
box_mark_offsets_repo_exists(struct box *box)
{
    return box->u.array.mark_offsets_exists;
}

static void
box_destroy_mark_offsets_repo(struct box *box)
{
    ARRAY_DESTROY(&box->u.array.mark_offsets);
    box->u.array.mark_offsets_exists = FALSE;
}

static int
box_array_index_is_marked(struct box *box, int64_t index)
{
    return 0 == (index & ((1 << box->u.array.cache_log2_n_keys_per_mark)
                          - 1));
}

int64_t
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
box_init_index_cache_by_key(struct box *box)
{
    box->u.array.cache_by_key = bloom_book_create();
}

int
box_index_cache_exists(const struct box *box)
{
    switch (box->filter->ndat->type) {
    case AST_NODE_TYPE_ARRAY:
        return (NULL != box->u.array.cache_by_key);
    default:
        return FALSE;
    }
}

static void
box_destroy_index_cache_by_key(struct box *box)
{
    bloom_book_destroy(box->u.array.cache_by_key);
    box->u.array.cache_by_key = NULL;
}

bitpunch_status_t
array_box_init_index_cache(struct box *box, struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int mark_offsets_should_exist;

    bt_ret = box_mark_offsets_repo_should_exist(
        box, &mark_offsets_should_exist, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    box->u.array.last_cached_index = -1;
    box->u.array.last_cached_item = NULL;
    box->u.array.last_cached_item_offset = -1;
    if (mark_offsets_should_exist) {
        box_init_mark_offsets_repo(box);
        box->u.array.cache_log2_n_keys_per_mark =
            BOX_INDEX_CACHE_DEFAULT_LOG2_N_KEYS_PER_MARK;
    }
    if (ast_node_is_indexed(box->filter)) {
        box_init_index_cache_by_key(box);
        box->u.array.cache_log2_n_keys_per_mark =
            log2_i(bloom_book_suggested_n_words_per_mark(
                       box->u.array.cache_by_key));
    }
    return BITPUNCH_OK;
}

void
array_box_destroy_index_cache(struct box *box)
{
    if (box_index_cache_exists(box)) {
        box_destroy_index_cache_by_key(box);
    }
    box_destroy_mark_offsets_repo(box);
}

bitpunch_status_t
tracker_index_cache_add_item(struct tracker *tk, expr_value_t item_key,
                             struct browse_state *bst)
{
    const char *key_buf;
    int64_t key_len;
    int64_t mark;

    DBG_TRACKER_DUMP(tk);
    assert(AST_NODE_TYPE_ARRAY == tk->box->filter->ndat->type);
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
    tk->box->u.array.last_cached_item = tk->dpath.item;
    tk->box->u.array.last_cached_item_offset = tk->item_offset;
    return BITPUNCH_OK;
}


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
bitpunch_status_t
box_index_cache_lookup_key_twins(struct box *box,
                                 expr_value_t item_key,
                                 struct track_path in_slice_path,
                                 struct index_cache_iterator *iterp,
                                 struct browse_state *bst)
{
    const char *key_buf;
    int64_t key_len;
    bloom_book_mark_t from_mark;
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    assert(AST_NODE_TYPE_ARRAY == box->filter->ndat->type);
    assert(box_index_cache_exists(box));

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
    bt_ret = track_box_contents_internal(box, &iterp->xtk, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    iterp->in_slice_path = in_slice_path;
    iterp->from_mark = from_mark;
    iterp->mark = bloom_book_lookup_word_get_next_candidate(
        box->u.array.cache_by_key, &iterp->bloom_cookie);
    if (BLOOM_BOOK_MARK_NONE != iterp->mark) {
        bt_ret = tracker_goto_mark_internal(iterp->xtk, iterp->mark, bst);
        if (BITPUNCH_OK != bt_ret) {
            return bt_ret;
        }
    }
    iterp->first = TRUE;
    return BITPUNCH_OK;
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

    return tk->box->filter->ndat->u.rexpr_filter.f_instance->b_tk.goto_next_key_match(
        tk, key, search_boundary, bst);
}

bitpunch_status_t
index_cache_iterator_next_twin(struct index_cache_iterator *iter,
                               struct track_path *item_pathp,
                               struct browse_state *bst)
{
    struct tracker *xtk;
    bitpunch_status_t bt_ret;
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
        bt_ret = tracker_goto_mark_internal(xtk, iter->mark, bst);
        if (BITPUNCH_OK != bt_ret) {
            break ;
        }
    }
    if (BITPUNCH_OK == bt_ret) {
        *item_pathp = xtk->cur;
    }
    /* skip the previous check for next iterations */
    iter->from_mark = BLOOM_BOOK_MARK_NONE;
    iter->first = FALSE;
    return bt_ret;
}

void
index_cache_iterator_done(struct index_cache_iterator *iter)
{
    tracker_delete(iter->xtk);
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
bitpunch_status_t
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
    bt_ret = box_index_cache_lookup_key_twins(tk->box, item_key, in_slice_path,
                                              &twin_iter, bst);
    if (BITPUNCH_OK != bt_ret) {
      return bt_ret;
    }
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
bitpunch_status_t
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
    bt_ret = box_index_cache_lookup_key_twins(tk->box, item_key, in_slice_path,
                                              &twin_iter, bst);
    if (BITPUNCH_OK != bt_ret) {
      return bt_ret;
    }
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

bitpunch_status_t
tracker_goto_mark_internal(struct tracker *tk,
                           int64_t mark,
                           struct browse_state *bst)
{
    struct filter_instance_array *array;
    struct track_path item_path;
    struct ast_node_hdl *array_item;
    int64_t item_offset;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    array = (struct filter_instance_array *)
        tk->box->filter->ndat->u.rexpr_filter.f_instance;
    bt_ret = expr_evaluate_filter_type_internal(
        array->item_type, tk->box, FILTER_KIND_ITEM, &array_item, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    memset(&item_path, 0, sizeof (item_path));
    item_path = track_path_from_array_index(
        box_array_get_mark_start_index(tk->box, mark));
    if (box_mark_offsets_repo_exists(tk->box)) {
        item_offset = box_array_get_mark_offset_at_index(
            tk->box, item_path.u.array.index);
    } else {
        assert(0 == (array_item->ndat->u.item.flags
                     & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC));
        item_offset = tk->box->start_offset_span
            + (item_path.u.array.index
               * ast_node_get_min_span_size(array_item));
    }
    tracker_set_dangling(tk);
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    tk->dpath.filter = array->item_type;
    tk->dpath.item = array_item;
    tk->cur = item_path;
    tk->item_offset = item_offset;
    DBG_TRACKER_CHECK_STATE(tk);
    return BITPUNCH_OK;
}

void
tracker_goto_last_cached_item_internal(struct tracker *tk,
                                       struct browse_state *bst)
{
    struct filter_instance_array *array;

    DBG_TRACKER_DUMP(tk);
    array = (struct filter_instance_array *)
        tk->box->filter->ndat->u.rexpr_filter.f_instance;
    assert(AST_NODE_TYPE_ARRAY == tk->box->filter->ndat->type);
    if (-1 != tk->box->u.array.last_cached_index) {
        tk->dpath.filter = array->item_type;
        tk->dpath.item = tk->box->u.array.last_cached_item;
        if (0 != (tk->dpath.item->ndat->u.item.flags
                  & ITEMFLAG_IS_SPAN_SIZE_DYNAMIC)) {
            tk->item_size = -1;
        }
        tk->item_offset = tk->box->u.array.last_cached_item_offset;
        tk->flags &= ~TRACKER_AT_END;
        tk->cur.u.array.index = tk->box->u.array.last_cached_index;
    } else {
        tracker_rewind(tk);
    }
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    DBG_TRACKER_CHECK_STATE(tk);
}
