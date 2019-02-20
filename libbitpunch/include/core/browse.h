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

#ifndef __BROWSE_H__
#define __BROWSE_H__

#include <alloca.h>

#include "api/bitpunch-structs.h"
#include "utils/queue.h"
#include "utils/dynarray.h"
#include PATH_TO_PARSER_TAB_H

#if defined DEBUG
extern int tracker_debug_mode;
#endif

struct box;
enum filter_kind;

struct browse_state {
    struct bitpunch_env *env;
    struct box *scope;
    struct tracker_error_slist *expected_errors;
    struct tracker_error *last_error;
};

struct index_cache_mark_offset {
    int64_t item_offset;
};

enum box_offset_type {
    BOX_START_OFFSET_HARD_MIN = (1<<0),
    BOX_START_OFFSET_MIN_SPAN = (1<<1),
    BOX_START_OFFSET_SPAN     = (1<<2),
    BOX_START_OFFSET_MAX_SPAN = (1<<3),
    BOX_START_OFFSET_SLACK    = (1<<4),
    BOX_START_OFFSET_PARENT   = (1<<5),
    BOX_START_OFFSET_USED     = (1<<6),
    BOX_START_OFFSETS         = (BOX_START_OFFSET_HARD_MIN |
                                 BOX_START_OFFSET_MIN_SPAN |
                                 BOX_START_OFFSET_SPAN |
                                 BOX_START_OFFSET_MAX_SPAN |
                                 BOX_START_OFFSET_SLACK |
                                 BOX_START_OFFSET_PARENT |
                                 BOX_START_OFFSET_USED),

    BOX_END_OFFSET_HARD_MIN   = (1<<7),
    BOX_END_OFFSET_MIN_SPAN   = (1<<8),
    BOX_END_OFFSET_SPAN       = (1<<9),
    BOX_END_OFFSET_MAX_SPAN   = (1<<10),
    BOX_END_OFFSET_SLACK      = (1<<11),
    BOX_END_OFFSET_PARENT     = (1<<12),
    BOX_END_OFFSET_USED       = (1<<13),
    BOX_END_OFFSETS           = (BOX_END_OFFSET_HARD_MIN |
                                 BOX_END_OFFSET_MIN_SPAN |
                                 BOX_END_OFFSET_SPAN |
                                 BOX_END_OFFSET_MAX_SPAN |
                                 BOX_END_OFFSET_SLACK |
                                 BOX_END_OFFSET_PARENT |
                                 BOX_END_OFFSET_USED),

    BOX_SIZE_HARD_MIN         = (BOX_START_OFFSET_HARD_MIN |
                                 BOX_END_OFFSET_HARD_MIN),
    BOX_SIZE_MIN_SPAN         = (BOX_START_OFFSET_MIN_SPAN |
                                 BOX_END_OFFSET_MIN_SPAN),
    BOX_SIZE_SPAN             = (BOX_START_OFFSET_SPAN |
                                 BOX_END_OFFSET_SPAN),
    BOX_SIZE_MAX_SPAN         = (BOX_START_OFFSET_MAX_SPAN |
                                 BOX_END_OFFSET_MAX_SPAN),
    BOX_SIZE_SLACK            = (BOX_START_OFFSET_SLACK |
                                 BOX_END_OFFSET_SLACK),
    BOX_SIZE_PARENT           = (BOX_START_OFFSET_PARENT |
                                 BOX_END_OFFSET_PARENT),
    BOX_SIZE_USED             = (BOX_START_OFFSET_USED |
                                 BOX_END_OFFSET_USED),
};

TAILQ_HEAD(box_tailq, box);

struct track_path {
    enum track_path_type {
        TRACK_PATH_NOTYPE,
        TRACK_PATH_FIELD,
        TRACK_PATH_ARRAY,
        TRACK_PATH_ARRAY_SLICE,
    } type;
    enum track_path_flags {
        TRACK_PATH_IS_ANCESTOR = (1u<<0),
        TRACK_PATH_HEADER      = (1u<<1),
        TRACK_PATH_TRAILER     = (1u<<2),
    } flags;
    union {
        const struct field *field;
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

struct box {
    struct box *parent_box;
    struct ast_node_hdl *filter;
    struct bitpunch_data_source *ds_in;
    struct bitpunch_data_source *ds_out;
    struct box *scope;
    struct bitpunch_env *env;

    /** [ds_in] inherited parent's max offset */
    int64_t start_offset_parent;

    /** [ds_in] limit of space that box may use from its parent */
    int64_t start_offset_slack;

    /** [ds_in] defined span size available to box */
    int64_t start_offset_max_span;

    /** [ds_in] start offset actually spanned by box */
    int64_t start_offset_span;

    /** [ds_in] minimum start offset spanned by box' children */
    int64_t start_offset_min_span;

    /** [ds_in] inherited parent's max offset */
    int64_t end_offset_parent;

    /** [ds_in] limit of space that box may use from its parent */
    int64_t end_offset_slack;

    /** [ds_in] defined span size available to box */
    int64_t end_offset_max_span;

    /** [ds_in] end offset actually spanned by box */
    int64_t end_offset_span;

    /** [ds_in] minimum end offset spanned by box' children */
    int64_t end_offset_min_span;


    /** [ds_out] start offset actually used by box output data */
    int64_t start_offset_used;

    /** [ds_out] end offset actually used by box output data */
    int64_t end_offset_used;

#define BOX_MAX_DEPTH_LEVEL 4096
    int depth_level;        /**< number of container nesting levels */

    int use_count;          /**< reference counter */

    /* internal box state */

    /** box flags (don't forget to update box_dump_flags()) */
    enum box_flag {
        COMPUTING_SPAN_SIZE        = (1u<<0),
        COMPUTING_SLACK_CHILD_ALLOCATION = (1u<<1),
        BOX_CACHED                 = (1u<<2),
        BOX_RALIGN                 = (1u<<3),
        BOX_FILTER                 = (1u<<4),
        BOX_DATA_SOURCE            = (1u<<5),
        BOX_OVERLAY                = (1u<<6),
        BOX_FILTER_APPLIED         = (1u<<7),
        BOX_MANAGE_ENV             = (1u<<8),
    } flags;
    union {
        struct box_composite {
        } composite;
        struct box_array_generic {
            int64_t n_items;
        } array_generic;
        struct box_array {
            struct box_array_generic array_generic; /* inherits */
            struct bloom_book *cache_by_key;
            ARRAY_HEAD(index_cache_mark_offset_repo,
                       struct index_cache_mark_offset) mark_offsets;
            int mark_offsets_exists;
            int64_t last_cached_index;
            struct ast_node_hdl *last_cached_item;
            int64_t last_cached_item_offset;
            int cache_log2_n_keys_per_mark;
        } array;
        struct box_byte_array {
            struct box_array_generic array_generic; /* inherits */
        } byte_array;
        struct box_array_slice {
            struct box_array_generic array_generic; /* inherits */
        } array_slice;
        struct box_byte_slice {
            struct box_array_generic array_generic; /* inherits */
        } byte_slice;
    } u;

    struct track_path track_path;

    /* cache-related inclusive lists */
    struct box_tailq cached_children;
    int n_cached_children;

    TAILQ_ENTRY(box) cached_boxes_list;
    TAILQ_ENTRY(box) cached_children_list;
};

#define BOX_INDEX_CACHE_DEFAULT_LOG2_N_KEYS_PER_MARK 5

struct tracker_error;

struct tracker {
    struct box *box;         /**< container box */

    struct dpath_node dpath; /**< tracked item dpath */

    int64_t item_offset;     /**< current item's absolute byte offset
                              * in file data (-1 if unset) */

    int64_t item_size;       /**< cached item size (-1 if unset) */

    enum tracker_flags {
        /* internal flags */
        TRACKER_AT_END           = (1u<<0), /**< positioned at end of
                                             *   container */
        TRACKER_NEED_ITEM_OFFSET = (1u<<1), /**< force computing the
                                             *   item offset (disable
                                             *   lazy evaluation) */
        TRACKER_REVERSED         = (1u<<2),
    } flags;

    /** internal tracking state */
    struct track_path cur;
};

enum tracker_state {
    TRACKER_STATE_UNSET = 0,
    TRACKER_STATE_DANGLING = 1,
    TRACKER_STATE_ITEM = 2,
    TRACKER_STATE_ITEM_OFFSET = 3,
    TRACKER_STATE_ITEM_SIZE = 4,
    TRACKER_STATE_AT_END = 7,
    TRACKER_STATE_COUNT = 8
};

struct tracker_error_context_info {
    /** error context messages pointing to offsets in @ref error_buf */
    const char *message;
    struct tracker *tk;
    struct box *box;
    const struct ast_node_hdl *node;
};

/**
 * @brief information about last error that occurred during a tracker
 * API call
 */
struct tracker_error {
    struct tracker *tk;      /**< copy of tracker when the error
                              * occurred (mutually exclusive with @ref
                              * box) */

    struct box *box;         /**< box when the error occurred
                              * (mutually exclusive with @ref tk) */

    bitpunch_status_t bt_ret; /**< error status code */

    enum tracker_error_flags {
        TRACKER_ERROR_STATIC = (1<<0), /**< error is statically allocated */
    } flags;               /**< error flags */

    const struct ast_node_hdl *node; /**< node that relates to the error */

#define BITPUNCH_ERROR_BUF_SIZE 2048
    /**< string table holding custom error message and context
     * information */
    char error_buf[BITPUNCH_ERROR_BUF_SIZE];

    char *error_buf_end;     /**< end of @ref error_buf contents */

    const char *reason;      /**< points to the reason phrase in @ref
                              * error_buf */

    int n_contexts;          /**< number of error context messages */

#define BITPUNCH_ERROR_MAX_CONTEXTS 64
    struct tracker_error_context_info contexts[BITPUNCH_ERROR_MAX_CONTEXTS];

    union tracker_error_info { /** error-specific additional info */
        struct tracker_error_info_out_of_bounds {
            enum box_offset_type registered_offset_type;
            int64_t registered_offset;
            enum box_offset_type requested_offset_type;
            int64_t requested_offset;
        } out_of_bounds;
    } u;
};

void
box_acquire(struct box *box);
void
box_delete(struct box *box);
void
box_delete_non_null(struct box *box);
int
box_contains_indexed_items(const struct box *box);
enum expr_value_type
box_get_index_type(const struct box *box);
struct ast_node_hdl *
box_get_index_expr(const struct box *box);
struct box *
box_new_root_box(struct ast_node_hdl *schema,
                 struct bitpunch_env *env,
                 int manage_env);
void
box_dump(const struct box *box);
void
box_fdump(const struct box *box, FILE *out);
int
box_get_abs_dpath(const struct box *box,
                  char *dpath_expr_buf, int buf_size);
char *
box_get_abs_dpath_alloc(const struct box *box);
int
box_dump_abs_dpath(const struct box *box, FILE *stream);

void
tracker_set(struct tracker *tk, const struct tracker *src_tk);
struct tracker *
tracker_dup(struct tracker *tk);
void
tracker_delete(struct tracker *tk);
void
tracker_dump(const struct tracker *tk);
void
tracker_fdump(const struct tracker *tk, FILE *out);

enum tracker_state
tracker_get_state(const struct tracker *tk);
void
tracker_set_dangling(struct tracker *tk);
static inline int
tracker_is_dangling(const struct tracker *tk);

struct tracker *
track_data_source(struct ast_node_hdl *schema,
                  const char *ds_name, struct bitpunch_data_source *ds,
                  struct tracker_error **errp);
bitpunch_status_t
box_get_n_items(struct box *box, int64_t *n_itemsp,
                struct tracker_error **errp);
bitpunch_status_t
box_get_location(struct box *box,
                 int64_t *offsetp, int64_t *sizep,
                 struct tracker_error **errp);
bitpunch_status_t
box_read_value(struct box *box,
               expr_value_t *valuep,
               struct tracker_error **errp);
bitpunch_status_t
box_compute_offset(struct box *box,
                   enum box_offset_type off_type,
                   int64_t *offsetp,
                   struct tracker_error **errp);
bitpunch_status_t
box_compute_size(struct box *box,
                 enum box_offset_type size_type,
                 int64_t *sizep,
                 struct tracker_error **errp);
bitpunch_status_t
track_box_contents_internal(struct box *box,
                            struct tracker **tkp, struct browse_state *bst);
bitpunch_status_t
box_apply_filter(struct box *box,
                 struct tracker_error **errp);
bitpunch_status_t
track_item_contents(struct tracker *tk,
                    struct tracker **tkp,
                    struct tracker_error **errp);
bitpunch_status_t
track_dpath_contents_internal(expr_dpath_t dpath,
                              struct tracker **tkp,
                              struct browse_state *bst);
bitpunch_status_t
track_dpath_contents(expr_dpath_t dpath,
                     struct tracker **tkp,
                     struct tracker_error **errp);

bitpunch_status_t
tracker_get_n_items(struct tracker *tk, int64_t *item_countp,
                    struct tracker_error **errp);


void
tracker_rewind(struct tracker *tk);
bitpunch_status_t
tracker_goto_first_item(struct tracker *tk,
                        struct tracker_error **errp);
bitpunch_status_t
tracker_goto_next_item(struct tracker *tk,
                       struct tracker_error **errp);
bitpunch_status_t
tracker_goto_nth_item(struct tracker *tk, int64_t index,
                      struct tracker_error **errp);
bitpunch_status_t
tracker_goto_nth_position(struct tracker *tk, int64_t index,
                          struct tracker_error **errp);
bitpunch_status_t
tracker_goto_named_item(struct tracker *tk, const char *name,
                        struct tracker_error **errp);
bitpunch_status_t
tracker_goto_first_item_with_key(struct tracker *tk,
                                 expr_value_t item_key,
                                 struct tracker_error **errp);
bitpunch_status_t
tracker_goto_next_item_with_key(struct tracker *tk,
                                expr_value_t item_key,
                                struct tracker_error **errp);
bitpunch_status_t
tracker_goto_nth_item_with_key(struct tracker *tk,
                               expr_value_t item_key,
                               int nth_twin,
                               struct tracker_error **errp);
bitpunch_status_t
tracker_goto_abs_dpath(struct tracker *tk, const char *dpath_expr,
                       struct tracker_error **errp);
bitpunch_status_t
tracker_goto_end(struct tracker *tk,
                 struct tracker_error **errp);
bitpunch_status_t
tracker_enter_item(struct tracker *tk,
                   struct tracker_error **errp);
bitpunch_status_t
tracker_enter_slice(struct tracker *tk, struct tracker *slice_end,
                    struct tracker_error **errp);
bitpunch_status_t
tracker_return(struct tracker *tk,
               struct tracker_error **errp);
int
tracker_get_abs_dpath(const struct tracker *tk,
                      char *dpath_expr_buf, int buf_size);
char *
tracker_get_abs_dpath_alloc(const struct tracker *tk);
int
tracker_dump_abs_dpath(const struct tracker *tk, FILE *stream);


bitpunch_status_t
tracker_get_item_filter(struct tracker *tk,
                        struct ast_node_hdl **item_filterp,
                        struct tracker_error **errp);

bitpunch_status_t
tracker_get_item_offset(struct tracker *tk, int64_t *item_offsetp,
                        struct tracker_error **errp);

bitpunch_status_t
tracker_get_item_size(struct tracker *tk, int64_t *item_sizep,
                      struct tracker_error **errp);

bitpunch_status_t
tracker_get_item_key(struct tracker *tk,
                     expr_value_t *keyp,
                     struct tracker_error **errp);

bitpunch_status_t
tracker_get_item_key_multi(struct tracker *tk,
                           expr_value_t *keyp,
                           int *nth_twinp,
                           struct tracker_error **errp);

bitpunch_status_t
tracker_get_item_location(struct tracker *tk,
                          int64_t *item_offsetp,
                          int64_t *item_sizep,
                          struct tracker_error **errp);

bitpunch_status_t
tracker_read_item_raw(struct tracker *tk,
                      const char **item_contentsp,
                      int64_t *item_sizep,
                      struct tracker_error **errp);

bitpunch_status_t
tracker_read_item_value(struct tracker *tk,
                        expr_value_t *valuep,
                        struct tracker_error **errp);

/* dpath API */

bitpunch_status_t
expr_dpath_to_dpath(expr_dpath_t src_dpath,
                    enum expr_dpath_type dst_type,
                    expr_dpath_t *dst_dpathp,
                    struct tracker_error **errp);

bitpunch_status_t
expr_dpath_get_size(expr_dpath_t dpath,
                    int64_t *dpath_sizep,
                    struct tracker_error **errp);

bitpunch_status_t
expr_dpath_get_location(expr_dpath_t dpath,
                        int64_t *offsetp, int64_t *sizep,
                        struct tracker_error **errp);

bitpunch_status_t
expr_dpath_get_filtered_data(
    expr_dpath_t dpath,
    struct bitpunch_data_source **dsp, int64_t *offsetp, int64_t *sizep,
    struct box **exported_data_boxp,
    struct tracker_error **errp);

/* error reporting */

void
tracker_error_dump(struct tracker_error *tk_err, FILE *out);

void
tracker_error_destroy(struct tracker_error *tk_err);


#include "core/browse_inlines.h"

#endif /*__BROWSE_H__*/
