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

#ifndef __TRACK_STRUCTS_H__
#define __TRACK_STRUCTS_H__

struct tracker;
struct box;
union expr_value;

enum container_type {
    CONTAINER_TYPE_BLOCK = 1,
    CONTAINER_TYPE_ARRAY = 2
};

struct track_path {
    enum track_path_type {
        TRACK_PATH_NOTYPE,
        TRACK_PATH_BLOCK,
        TRACK_PATH_ARRAY,
        TRACK_PATH_ARRAY_SLICE
    } type;
    union {
        struct {
            const struct field *field;
        } block;
        struct track_path_array {
            int64_t index;
        } array;
        struct {
            struct track_path_array array; /* inherits */
            int64_t index_end;
        } array_slice;
    } u;
};

static const struct track_path TRACK_PATH_NONE = {
    .type = TRACK_PATH_NOTYPE
};

static inline struct track_path
track_path_from_block_field(const struct field *field)
{
    struct track_path ret;

    memset(&ret, 0, sizeof (ret));
    ret.type = TRACK_PATH_BLOCK;
    ret.u.block.field = field;
    return ret;
}

static inline struct track_path
track_path_from_array_index(int64_t index)
{
    struct track_path ret;

    memset(&ret, 0, sizeof (ret));
    ret.type = TRACK_PATH_ARRAY;
    ret.u.array.index = index;
    return ret;
}

static inline struct track_path
track_path_from_array_slice(int64_t index_start, int64_t index_end)
{
    struct track_path ret;

    memset(&ret, 0, sizeof (ret));
    ret.type = TRACK_PATH_ARRAY_SLICE;
    ret.u.array.index = index_start;
    ret.u.array_slice.index_end = index_end;
    return ret;
}

static inline int
track_path_eq(struct track_path p1, struct track_path p2)
{
    if (p1.type != p2.type) {
        return FALSE;
    }
    switch (p1.type) {
    case TRACK_PATH_NOTYPE:
        return TRUE;
    case TRACK_PATH_BLOCK:
        return p1.u.block.field == p2.u.block.field;
    case TRACK_PATH_ARRAY:
        return p1.u.array.index == p2.u.array.index;
    case TRACK_PATH_ARRAY_SLICE:
        return (p1.u.array.index == p2.u.array.index &&
                p1.u.array_slice.index_end == p2.u.array_slice.index_end);
    default:
        return FALSE;
    }
}

struct browse_state {
    struct tracker_error_slist *expected_errors;
    struct tracker_error *last_error;
};


struct filter_backend {
    bitpunch_status_t (*read_value)(const struct ast_node *item_filter,
                                    struct box *scope,
                                    int64_t item_offset,
                                    int64_t item_size,
                                    enum expr_value_type *typep,
                                    union expr_value *valuep,
                                    struct browse_state *bst);
};

struct box_backend {
    bitpunch_status_t (*get_n_items)(struct box *box,
                                    int64_t *item_countp,
                                    struct browse_state *bst);
    bitpunch_status_t (*compute_slack_size)(struct box *box,
                                           struct browse_state *bst);
    bitpunch_status_t (*compute_max_span_size)(struct box *box,
                                          struct browse_state *bst);
    bitpunch_status_t (*compute_used_size)(struct box *box,
                                          struct browse_state *bst);
    bitpunch_status_t (*compute_min_span_size)(struct box *box,
                                              struct browse_state *bst);
    bitpunch_status_t (*get_max_slack_offset)(struct box *box,
                                             int64_t *max_slack_offsetp,
                                             struct browse_state *bst);
};

struct tracker_backend {
    bitpunch_status_t (*get_item_key)(struct tracker *tk,
                                     enum expr_value_type *key_typep,
                                     union expr_value *keyp,
                                     int *nth_twinp,
                                     struct browse_state *bst);
    bitpunch_status_t (*compute_item_size)(struct tracker *tk,
                                          int64_t *item_sizep,
                                          struct browse_state *bst);
    bitpunch_status_t (*goto_first_item)(struct tracker *tk,
                                        struct browse_state *bst);
    bitpunch_status_t (*goto_next_item)(struct tracker *tk,
                                       struct browse_state *bst);
    bitpunch_status_t (*goto_nth_item)(struct tracker *tk,
                                      int64_t index,
                                      struct browse_state *bst);
    bitpunch_status_t (*goto_named_item)(struct tracker *tk,
                                        const char *name,
                                        struct browse_state *bst);
    bitpunch_status_t (*goto_next_key_match)(struct tracker *tk,
                                            union expr_value index,
                                            struct track_path search_boundary,
                                            struct browse_state *bst);
    bitpunch_status_t (*goto_next_item_with_key)(struct tracker *tk,
                                                union expr_value item_key,
                                                struct browse_state *bst);
    bitpunch_status_t (*goto_nth_item_with_key)(struct tracker *tk,
                                               union expr_value item_key,
                                               int nth_twin,
                                               struct browse_state *bst);
};

#endif /*__TRACK_STRUCTS_H__*/
