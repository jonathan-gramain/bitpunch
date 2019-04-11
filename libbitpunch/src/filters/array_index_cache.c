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
#include "filters/array.h"

static bitpunch_status_t
mark_offsets_repo_should_exist(struct ast_node_hdl *filter, struct box *scope,
                               int *should_existp,
                               struct browse_state *bst)
{
    struct filter_instance_array *array;
    bitpunch_status_t bt_ret;
    struct ast_node_hdl *item_type;

    array = (struct filter_instance_array *)
        filter->ndat->u.rexpr_filter.f_instance;
    bt_ret = expr_evaluate_filter_type_internal(
        array->item_type, scope, FILTER_KIND_ITEM, &item_type, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    *should_existp = 0 != (item_type->ndat->u.item.flags
                           & ITEMFLAG_IS_SPAN_SIZE_VARIABLE);
    return BITPUNCH_OK;
}

static void
init_mark_offsets_repo(struct array_cache *cache)
{
    cache->mark_offsets_exists = TRUE;
    ARRAY_INIT(&cache->mark_offsets, 0);
}

static int
mark_offsets_repo_exists(struct array_cache *cache)
{
    return cache->mark_offsets_exists;
}

static void
destroy_mark_offsets_repo(struct array_cache *cache)
{
    ARRAY_DESTROY(&cache->mark_offsets);
    cache->mark_offsets_exists = FALSE;
}

static int
array_index_is_marked(struct array_cache *cache, int64_t index)
{
    return 0 == (index & ((1 << cache->cache_log2_n_keys_per_mark) - 1));
}

int64_t
array_get_index_mark(struct array_cache *cache, int64_t index)
{
    return index >> cache->cache_log2_n_keys_per_mark;
}

static int64_t
array_get_mark_start_index(struct array_cache *cache, int64_t mark)
{
    return mark << cache->cache_log2_n_keys_per_mark;
}

static int64_t
array_get_mark_offset_at_index(struct array_cache *cache, int64_t index)
{
    int64_t marks_index;

    marks_index = array_get_index_mark(cache, index);
    assert(ARRAY_SIZE(&cache->mark_offsets) > marks_index);

    return ARRAY_ITEM(&cache->mark_offsets, marks_index).item_offset;
}

static void
array_add_mark_offset(struct array_cache *cache,
                      int64_t mark, int64_t item_offset)
{
    struct index_cache_mark_offset mark_offset;

    assert(ARRAY_SIZE(&cache->mark_offsets) == mark);

    mark_offset.item_offset = item_offset;
    ARRAY_PUSH(&cache->mark_offsets, mark_offset);
}

static void
init_index_cache_by_key(struct array_cache *cache)
{
    cache->cache_by_key = bloom_book_create();
}

int
index_cache_exists(struct array_cache *cache)
{
    return NULL != cache->cache_by_key;
}

static void
destroy_index_cache_by_key(struct array_cache *cache)
{
    bloom_book_destroy(cache->cache_by_key);
    cache->cache_by_key = NULL;
}

bitpunch_status_t
array_index_cache_init(struct array_cache *cache, struct box *scope,
                       struct ast_node_hdl *filter, struct browse_state *bst)
{
    bitpunch_status_t bt_ret;
    int mark_offsets_should_exist;

    bt_ret = mark_offsets_repo_should_exist(
        filter, scope, &mark_offsets_should_exist, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    cache->last_cached_index = -1;
    cache->last_cached_item = NULL;
    cache->last_cached_item_offset = -1;
    if (mark_offsets_should_exist) {
        init_mark_offsets_repo(cache);
        cache->cache_log2_n_keys_per_mark =
            BOX_INDEX_CACHE_DEFAULT_LOG2_N_KEYS_PER_MARK;
    }
    if (ast_node_is_indexed(filter)) {
        init_index_cache_by_key(cache);
        cache->cache_log2_n_keys_per_mark =
            log2_i(bloom_book_suggested_n_words_per_mark(cache->cache_by_key));
    }
    return BITPUNCH_OK;
}

void
array_index_cache_destroy(struct array_cache *cache)
{
    if (index_cache_exists(cache)) {
        destroy_index_cache_by_key(cache);
    }
    destroy_mark_offsets_repo(cache);
}

bitpunch_status_t
tracker_index_cache_add_item(struct tracker *tk, expr_value_t item_key,
                             struct browse_state *bst)
{
    struct array_cache *cache;
    const char *key_buf;
    int64_t key_len;
    int64_t mark;

    DBG_TRACKER_DUMP(tk);
    cache = box_array_cache(tk->box);
    assert(tk->cur.u.array.index == cache->last_cached_index + 1);

    if (index_cache_exists(cache)) {
        if (array_index_is_marked(cache, tk->cur.u.array.index)) {
            mark = (int64_t)bloom_book_add_mark(cache->cache_by_key);
            if (mark_offsets_repo_exists(cache)) {
                array_add_mark_offset(cache, mark, tk->item_offset);
            }
        }
        expr_value_to_hashable(item_key, &key_buf, &key_len);
        bloom_book_insert_word(cache->cache_by_key, key_buf, key_len);
    } else {
        if (mark_offsets_repo_exists(cache)
            && array_index_is_marked(cache, tk->cur.u.array.index)) {
            mark = array_get_index_mark(cache, tk->cur.u.array.index);
            array_add_mark_offset(cache, mark, tk->item_offset);
        }
    }
    cache->last_cached_index = tk->cur.u.array.index;
    cache->last_cached_item = tk->dpath.item;
    cache->last_cached_item_offset = tk->item_offset;
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
    struct array_cache *cache;
    const char *key_buf;
    int64_t key_len;
    bloom_book_mark_t from_mark;
    bitpunch_status_t bt_ret;

    DBG_BOX_DUMP(box);
    cache = box_array_cache(box);
    assert(index_cache_exists(cache));

    expr_value_to_hashable(item_key, &key_buf, &key_len);

    if (! track_path_eq(in_slice_path, TRACK_PATH_NONE)) {
        assert(TRACK_PATH_ARRAY_SLICE == in_slice_path.type);
        from_mark = array_get_index_mark(cache, in_slice_path.u.array.index);
    } else {
        from_mark = BLOOM_BOOK_MARK_NONE;
    }
    bloom_book_lookup_word_from_mark(cache->cache_by_key,
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
        cache->cache_by_key, &iterp->bloom_cookie);
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
    struct array_cache *cache;
    struct track_path search_boundary;

    DBG_TRACKER_DUMP(tk);
    cache = box_array_cache(tk->box);
    /* limit search to keys belonging to the current mark's scope */
    search_boundary = track_path_from_array_index(
        array_get_mark_start_index(cache, mark + 1));

    return tk->box->filter->ndat->u.rexpr_filter.f_instance->b_tk.goto_next_key_match(
        tk, key, search_boundary, bst);
}

bitpunch_status_t
index_cache_iterator_next_twin(struct index_cache_iterator *iter,
                               struct track_path *item_pathp,
                               struct browse_state *bst)
{
    struct tracker *xtk;
    struct array_cache *cache;
    bitpunch_status_t bt_ret;
    int64_t index_end;

    xtk = iter->xtk;
    cache = box_array_cache(xtk->box);
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
            cache->cache_by_key, &iter->bloom_cookie);
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
    struct array_cache *cache;
    struct track_path item_path;
    struct ast_node_hdl *array_item;
    int64_t item_offset;
    bitpunch_status_t bt_ret;

    DBG_TRACKER_DUMP(tk);
    array = (struct filter_instance_array *)
        tk->box->filter->ndat->u.rexpr_filter.f_instance;
    cache = box_array_cache(tk->box);
    bt_ret = expr_evaluate_filter_type_internal(
        array->item_type, tk->box, FILTER_KIND_ITEM, &array_item, bst);
    if (BITPUNCH_OK != bt_ret) {
        return bt_ret;
    }
    memset(&item_path, 0, sizeof (item_path));
    item_path = track_path_from_array_index(
        array_get_mark_start_index(cache, mark));
    if (mark_offsets_repo_exists(cache)) {
        item_offset = array_get_mark_offset_at_index(
            cache, item_path.u.array.index);
    } else {
        assert(0 == (array_item->ndat->u.item.flags
                     & ITEMFLAG_IS_SPAN_SIZE_VARIABLE));
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
    struct array_cache *cache;

    DBG_TRACKER_DUMP(tk);
    array = (struct filter_instance_array *)
        tk->box->filter->ndat->u.rexpr_filter.f_instance;
    cache = box_array_cache(tk->box);
    if (-1 != cache->last_cached_index) {
        tk->dpath.filter = array->item_type;
        tk->dpath.item = cache->last_cached_item;
        if (0 != (tk->dpath.item->ndat->u.item.flags
                  & ITEMFLAG_IS_SPAN_SIZE_VARIABLE)) {
            tk->item_size = -1;
        }
        tk->item_offset = cache->last_cached_item_offset;
        tk->flags &= ~TRACKER_AT_END;
        tk->cur.u.array.index = cache->last_cached_index;
    } else {
        tracker_rewind(tk);
    }
    tk->flags |= TRACKER_NEED_ITEM_OFFSET;
    DBG_TRACKER_CHECK_STATE(tk);
}
