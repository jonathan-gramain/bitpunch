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

#ifndef __BROWSE_INTERNAL_H__
#define __BROWSE_INTERNAL_H__

#include "core/browse.h"

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

struct item_backend {
    bitpunch_status_t (*compute_item_size)(struct ast_node_hdl *item_filter,
                                           struct box *scope,
                                           int64_t item_offset,
                                           int64_t max_span_offset,
                                           int64_t *item_sizep,
                                           struct browse_state *bst);
    bitpunch_status_t (*read_value)(struct ast_node_hdl *item_filter,
                                    struct box *scope,
                                    int64_t item_offset,
                                    int64_t item_size,
                                    expr_value_t *valuep,
                                    struct browse_state *bst);
};

struct box_backend {
    bitpunch_status_t (*init)(struct box *box, struct browse_state *bst);
    void              (*destroy)(struct box *box);
    bitpunch_status_t (*get_n_items)(struct box *box,
                                    int64_t *item_countp,
                                    struct browse_state *bst);
    bitpunch_status_t (*compute_slack_size)(struct box *box,
                                           struct browse_state *bst);
    bitpunch_status_t (*compute_max_span_size)(struct box *box,
                                          struct browse_state *bst);
    bitpunch_status_t (*compute_span_size)(struct box *box,
                                           struct browse_state *bst);
    bitpunch_status_t (*compute_min_span_size)(struct box *box,
                                              struct browse_state *bst);
    bitpunch_status_t (*compute_used_size)(struct box *box,
                                           struct browse_state *bst);
    bitpunch_status_t (*get_slack_child_allocation)(struct box *box,
                                                    int get_left_offset,
                                                    int64_t *max_slack_offsetp,
                                                    struct browse_state *bst);
};

struct tracker_backend {
    bitpunch_status_t (*get_item_key)(struct tracker *tk,
                                     expr_value_t *keyp,
                                     int *nth_twinp,
                                     struct browse_state *bst);
    bitpunch_status_t (*compute_item_size)(struct tracker *tk,
                                          int64_t *item_sizep,
                                          struct browse_state *bst);
    bitpunch_status_t (*init_item_offset)(struct tracker *tk,
                                          struct browse_state *bst);
    bitpunch_status_t (*advance_item_offset)(struct tracker *tk,
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
                                            expr_value_t index,
                                            struct track_path search_boundary,
                                            struct browse_state *bst);
    bitpunch_status_t (*goto_next_item_with_key)(struct tracker *tk,
                                                expr_value_t item_key,
                                                struct browse_state *bst);
    bitpunch_status_t (*goto_nth_item_with_key)(struct tracker *tk,
                                               expr_value_t item_key,
                                               int nth_twin,
                                               struct browse_state *bst);
    bitpunch_status_t (*goto_end_path)(struct tracker *tk,
                                       struct browse_state *bst);
    void              (*goto_nil)(struct tracker *tk);
};

int
track_path_eq(struct track_path p1, struct track_path p2);

void
browse_state_init(struct browse_state *bst);
void
browse_state_init_scope(struct browse_state *bst, struct box *scope);
void
browse_state_init_box(struct browse_state *bst, struct box *box);
void
browse_state_init_tracker(struct browse_state *bst, struct tracker *tk);
void
browse_state_init_dpath(struct browse_state *bst, expr_dpath_t dpath);
void
browse_state_cleanup(struct browse_state *bst);
bitpunch_status_t
browse_state_set_environment(struct browse_state *bst,
                             struct bitpunch_board *env);
void
browse_state_push_scope(struct browse_state *bst, struct box *scope,
                        struct box **storagep);
void
browse_state_pop_scope(struct browse_state *bst, struct box *scope,
                       struct box **storagep);

bitpunch_status_t
box_construct(struct box *o_box,
              struct box *parent_box,
              struct ast_node_hdl *filter,
              struct box *scope,
              int64_t boundary_offset,
              enum box_flag box_flags,
              struct browse_state *bst);
bitpunch_status_t
box_set_size(struct box *box, int64_t box_size,
             enum box_offset_type size_type,
             struct browse_state *bst);
bitpunch_status_t
box_set_min_span_size(struct box *box, int64_t min_span_size,
                      struct browse_state *bst);
bitpunch_status_t
box_set_span_size(struct box *box, int64_t span_size,
                  struct browse_state *bst);
bitpunch_status_t
box_set_max_span_size(struct box *box, int64_t max_span_size,
                      struct browse_state *bst);
bitpunch_status_t
box_set_used_size(struct box *box, int64_t used_size,
                  struct browse_state *bst);
bitpunch_status_t
box_compute_max_span_size(struct box *box,
                          struct browse_state *bst);
bitpunch_status_t
box_compute_slack_size(struct box *box,
                       struct browse_state *bst);
bitpunch_status_t
box_compute__error(struct box *box,
                   struct browse_state *bst);
bitpunch_status_t
filter_read_value__operator_filter(struct ast_node_hdl *filter,
                                   struct box *scope,
                                   int64_t item_offset,
                                   int64_t item_size,
                                   expr_value_t *valuep,
                                   struct browse_state *bst);
bitpunch_status_t
box_get_slack_child_allocation(struct box *box,
                               int get_left_offset,
                               int64_t *max_slack_offsetp,
                               struct browse_state *bst);

struct box *
box_new_filter_box(struct box *parent_box,
                   struct ast_node_hdl *filter,
                   struct browse_state *bst);
bitpunch_status_t
box_apply_parent_filter_internal(struct box *box,
                                 struct browse_state *bst);
bitpunch_status_t
box_apply_filter_internal(struct box *box,
                          struct browse_state *bst);

bitpunch_status_t
box_get_n_items_internal(struct box *box, int64_t *item_countp,
                         struct browse_state *bst);
bitpunch_status_t
box_get_n_items__as_used(struct box *box, int64_t *item_countp,
                         struct browse_state *bst);
bitpunch_status_t
box_get_min_span_size(struct box *box, int64_t *min_span_sizep,
                      struct browse_state *bst);
bitpunch_status_t
box_compute_span_size(struct box *box,
                      struct browse_state *bst);
bitpunch_status_t
box_compute_used_size(struct box *box, struct browse_state *bst);
bitpunch_status_t
box_get_used_size(struct box *box, int64_t *used_sizep,
                  struct browse_state *bst);
bitpunch_status_t
box_get_span_size(struct box *box, int64_t *used_sizep,
                  struct browse_state *bst);
bitpunch_status_t
box_get_max_span_size(struct box *box, int64_t *max_span_sizep,
                      struct browse_state *bst);
bitpunch_status_t
box_get_slack_size(struct box *box, int64_t *slack_sizep,
                   struct browse_state *bst);
bitpunch_status_t
box_get_location_internal(struct box *box,
                          int64_t *offsetp, int64_t *sizep,
                          struct browse_state *bst);
int64_t
box_get_offset(struct box *box, enum box_offset_type type);
int64_t
box_get_known_start_offset_mask(const struct box *box,
                                enum box_offset_type mask);
int64_t
box_get_known_start_offset(const struct box *box);
int64_t
box_get_known_end_offset_mask(const struct box *box,
                              enum box_offset_type mask);
int64_t
box_get_known_end_offset(const struct box *box);
enum box_offset_type
box_get_known_end_offset_type(const struct box *box);
bitpunch_status_t
box_check_start_offset(struct box *box, int64_t start_offset,
                       enum box_offset_type type,
                       struct browse_state *bst);
bitpunch_status_t
box_check_end_offset(struct box *box, int64_t end_offset,
                     enum box_offset_type type,
                     struct browse_state *bst);
bitpunch_status_t
box_set_start_offset(struct box *box, int64_t start_offset,
                     enum box_offset_type type,
                     struct browse_state *bst);
bitpunch_status_t
box_set_end_offset(struct box *box, int64_t end_offset,
                   enum box_offset_type type,
                   struct browse_state *bst);
bitpunch_status_t
box_read_value_internal(struct box *box,
                        expr_value_t *valuep,
                        struct browse_state *bst);
bitpunch_status_t
box_get_filtered_data_internal(
    struct box *box,
    struct bitpunch_data_source **dsp, int64_t *offsetp, int64_t *sizep,
    struct browse_state *bst);

bitpunch_status_t
tracker_get_filtered_dpath_internal(struct tracker *tk,
                                    expr_dpath_t *filtered_dpathp,
                                    struct browse_state *bst);
bitpunch_status_t
tracker_get_filtered_dpath(struct tracker *tk,
                           expr_dpath_t *filtered_dpathp,
                           struct tracker_error **errp);
bitpunch_status_t
tracker_create_item_box_internal(struct tracker *tk,
                                 struct box **item_boxp,
                                 struct browse_state *bst);
bitpunch_status_t
tracker_set_end(struct tracker *tk, struct browse_state *bst);

bitpunch_status_t
tracker_get_filtered_item_box_internal(struct tracker *tk,
                                       struct box **filtered_boxp,
                                       struct browse_state *bst);
bitpunch_status_t
tracker_get_filtered_item_box(struct tracker *tk,
                              struct box **filtered_boxp,
                              struct tracker_error **errp);

bitpunch_status_t
track_item_contents_internal(struct tracker *tk,
                             struct tracker **tkp,
                             struct browse_state *bst);
bitpunch_status_t
tracker_compute_item_filter_internal(struct tracker *tk,
                                     struct browse_state *bst);

bitpunch_status_t
track_box_contents(struct box *box,
                   struct tracker **tkp,
                   struct tracker_error **errp);

bitpunch_status_t
tracker_check_item(struct tracker *tk,
                   struct browse_state *bst);

bitpunch_status_t
tracker_goto_first_item_internal(struct tracker *tk,
                                 struct browse_state *bst);
bitpunch_status_t
tracker_goto_next_item_internal(struct tracker *tk,
                                struct browse_state *bst);
bitpunch_status_t
tracker_goto_nth_item_internal(struct tracker *tk, int64_t index,
                               struct browse_state *bst);
bitpunch_status_t
tracker_goto_nth_position_internal(struct tracker *tk, int64_t index,
                                   struct browse_state *bst);
bitpunch_status_t
tracker_goto_named_item_internal(struct tracker *tk, const char *name,
                                 struct browse_state *bst);
bitpunch_status_t
tracker_goto_first_item_with_key_internal(struct tracker *tk,
                                          expr_value_t item_key,
                                          struct browse_state *bst);
bitpunch_status_t
tracker_goto_next_item_with_key_internal(struct tracker *tk,
                                         expr_value_t item_key,
                                         struct browse_state *bst);
bitpunch_status_t
tracker_goto_nth_item_with_key_internal(struct tracker *tk,
                                        expr_value_t item_key,
                                        int nth_twin,
                                        struct browse_state *bst);
bitpunch_status_t
tracker_goto_next_item_with_key__default(struct tracker *tk,
                                         expr_value_t item_key,
                                         struct browse_state *bst);
bitpunch_status_t
tracker_goto_next_item_with_key__not_impl(struct tracker *tk,
                                          expr_value_t item_key,
                                          struct browse_state *bst);
bitpunch_status_t
tracker_goto_nth_item_with_key__default(struct tracker *tk,
                                        expr_value_t item_key,
                                        int nth_twin,
                                        struct browse_state *bst);
bitpunch_status_t
tracker_goto_nth_item_with_key__not_impl(
    struct tracker *tk, expr_value_t item_key, int nth_twin,
    struct browse_state *bst);
bitpunch_status_t
tracker_goto_ancestor_array_index_internal(struct tracker *tk,
                                           int64_t index,
                                           struct browse_state *bst);
bitpunch_status_t
tracker_goto_index_internal(struct tracker *tk,
                            struct subscript_index index,
                            const char *index_desc,
                            int allow_end_boundary,
                            int is_end_of_slice,
                            struct browse_state *bst);

bitpunch_status_t
tracker_goto_field_internal(struct tracker *tk,
                            const struct field *to_field, int flat,
                            struct browse_state *bst);
bitpunch_status_t
tracker_goto_first_field_internal(struct tracker *tk, int flat,
                                   struct browse_state *bst);
bitpunch_status_t
tracker_goto_next_field_internal(struct tracker *tk, int flat,
                                 struct browse_state *bst);

bitpunch_status_t
tracker_enter_item_internal(struct tracker *tk,
                            struct browse_state *bst);
bitpunch_status_t
tracker_return_internal(struct tracker *tk,
                        struct browse_state *bst);

bitpunch_status_t
tracker_get_item_filter_internal(struct tracker *tk,
                                 struct ast_node_hdl **item_filterp,
                                 struct browse_state *bst);
bitpunch_status_t
tracker_get_item_offset_internal(struct tracker *tk, int64_t *item_offsetp,
                                 struct browse_state *bst);
bitpunch_status_t
tracker_get_item_size_internal(struct tracker *tk, int64_t *item_sizep,
                               struct browse_state *bst);
bitpunch_status_t
tracker_get_item_location_internal(struct tracker *tk,
                                   int64_t *item_offsetp,
                                   int64_t *item_sizep,
                                   struct browse_state *bst);
bitpunch_status_t
tracker_compute_item_offset(struct tracker *tk,
                            struct browse_state *bst);
bitpunch_status_t
tracker_compute_item_size(struct tracker *tk,
                          struct browse_state *bst);
bitpunch_status_t
tracker_compute_item_location(struct tracker *tk,
                              struct browse_state *bst);
bitpunch_status_t
tracker_get_item_key_internal(struct tracker *tk,
                              expr_value_t *keyp,
                              struct browse_state *bst);
bitpunch_status_t
tracker_get_item_key_multi_internal(struct tracker *tk,
                                    expr_value_t *keyp,
                                    int *nth_twinp,
                                    struct browse_state *bst);
bitpunch_status_t
tracker_set_item_offset_at_box(struct tracker *tk,
                               struct box *box,
                               struct browse_state *bst);
bitpunch_status_t
tracker_read_item_value_internal(struct tracker *tk,
                                 expr_value_t *valuep,
                                 struct browse_state *bst);
bitpunch_status_t
tracker_read_item_value_direct_internal(struct tracker *tk,
                                        expr_value_t *valuep,
                                        struct browse_state *bst);

bitpunch_status_t
tracker_read_item_raw_internal(struct tracker *tk,
                               const char **item_contentsp,
                               int64_t *item_sizep,
                               struct browse_state *bst);
bitpunch_status_t
tracker_get_filtered_data_internal(
    struct tracker *tk,
    struct bitpunch_data_source **dsp, int64_t *offsetp, int64_t *sizep,
    struct box **exported_data_boxp,
    struct browse_state *bst);
bitpunch_status_t
tracker_reverse_direction_internal(struct tracker *tk,
                                   struct browse_state *bst);

void
tracker_reset_item_cache(struct tracker *tk);
struct track_path
track_path_from_field(const struct field *field);
struct track_path
track_path_from_array_index(int64_t index);
struct track_path
track_path_from_array_slice(int64_t index_start, int64_t index_end);


void
tracker_error_init(struct tracker_error *tk_err,
                   bitpunch_status_t bt_ret);
struct tracker_error *
tracker_error_new(bitpunch_status_t bt_ret,
                  struct tracker *tk, struct box *box,
                  const struct ast_node_hdl *node,
                  const char *message_fmt, va_list message_args);

bitpunch_status_t
transmit_error(bitpunch_status_t bt_ret, struct browse_state *bst,
               struct tracker_error **errp);

bitpunch_status_t
tracker_error(bitpunch_status_t bt_ret, struct tracker *tk,
              const struct ast_node_hdl *node,
              struct browse_state *bst,
              const char *message_fmt, ...)
    __attribute__((format(printf, 5, 6)));
bitpunch_status_t
box_error(bitpunch_status_t bt_ret, struct box *box,
          const struct ast_node_hdl *node,
          struct browse_state *bst,
          const char *message_fmt, ...)
    __attribute__((format(printf, 5, 6)));
bitpunch_status_t
node_error(bitpunch_status_t bt_ret,
           const struct ast_node_hdl *node,
           struct browse_state *bst,
           const char *message_fmt, ...)
    __attribute__((format(printf, 4, 5)));
bitpunch_status_t
box_error_out_of_bounds(struct box *box,
                        const struct ast_node_hdl *node,
                        enum box_offset_type requested_end_offset_type,
                        int64_t requested_end_offset,
                        enum box_offset_type registered_end_offset_type,
                        struct browse_state *bst);
bitpunch_status_t
tracker_error_item_out_of_bounds(struct tracker *tk,
                                 struct browse_state *bst);
void
tracker_error_add_context_message(struct browse_state *bst,
                                  const char *context_fmt, ...)
    __attribute__((format(printf, 2, 3), unused));
void
tracker_error_add_tracker_context(struct tracker *tk,
                                  struct browse_state *bst,
                                  const char *context_fmt, ...)
    __attribute__((format(printf, 3, 4), unused));
void
tracker_error_add_box_context(struct box *box,
                              struct browse_state *bst,
                              const char *context_fmt, ...)
    __attribute__((format(printf, 3, 4), unused));
void
tracker_error_add_node_context(const struct ast_node_hdl *node,
                               struct browse_state *bst,
                               const char *context_fmt, ...)
    __attribute__((format(printf, 3, 4), unused));

void
browse_state_clear_error(struct browse_state *bst);

bitpunch_status_t
browse_state_get_last_error_status(struct browse_state *bst);

#endif /* __BROWSE_INTERNAL_H__ */
