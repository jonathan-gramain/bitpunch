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

#include "utils/dynarray.h"
#include "core/parser.h"
#include PATH_TO_PARSER_TAB_H
#include "core/track-structs.h"

#if defined DEBUG
extern int tracker_debug_mode;
#endif

struct box;

struct index_cache_mark_offset {
    int64_t item_offset;
};

enum box_offset_type {
    BOX_START_OFFSET_USED = (1<<0),
    BOX_END_OFFSET_HARD_MIN = (1<<1),
    BOX_END_OFFSET_MIN_SPAN = (1<<2),
    BOX_END_OFFSET_USED = (1<<3),
    BOX_END_OFFSET_MAX_SPAN = (1<<4),
    BOX_END_OFFSET_SLACK = (1<<5),
    BOX_END_OFFSET_PARENT = (1<<6),
};

TAILQ_HEAD(box_tailq, box);

struct box {
    struct box *parent_box;
    const struct ast_node *node;
    const struct bitpunch_binary_file_hdl *file_hdl;

    int64_t start_offset_used; /**< absolute start offset of box in file
                                * data */

    int64_t end_offset_parent; /**< inherited parent's max offset */

    int64_t end_offset_slack; /**< limit of space that box may use
                               * from its parent */

    int64_t end_offset_max_span; /**< defined span size available to box */

    int64_t end_offset_used;  /**< end offset actually used by box */

    int64_t end_offset_min_span;   /**< minimum end offset spanned by box'
                               * children */

#define BOX_MAX_DEPTH_LEVEL 4096
    int depth_level;        /**< number of container nesting levels */

    int use_count;          /**< reference counter */

    /** internal box state */
    enum box_flag {
        COMPUTING_SPAN_SIZE        = (1u<<0),
        COMPUTING_MAX_SLACK_OFFSET = (1u<<1),
        BOX_CACHED                 = (1u<<2),
        BOX_REVERSED               = (1u<<3),
    } flags;
    union {
        struct box_block {
        } block;
        struct box_array_generic {
            int64_t n_items;
        } array_generic;
        struct box_array {
            struct box_array_generic array_generic; /* inherits */
            struct bloom_book *cache_by_key;
            ARRAY_HEAD(index_cache_mark_offset_repo,
                       struct index_cache_mark_offset) mark_offsets;
            int64_t last_cached_index;
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

#define BOX_CACHE_MAX_N_BOXES                   256
#define BOX_CACHE_MAX_N_CACHED_CHILDREN         8
#define BOX_INDEX_CACHE_DEFAULT_LOG2_N_KEYS_PER_MARK 5


struct box_cache {
    struct box_tailq mru_boxes;
    int n_boxes;
    int max_n_boxes;
    int max_n_cached_children;
};

struct tracker_error;

struct tracker {
    struct box *box;         /**< container box */

    const struct ast_node *item_node; /**< tracked item node */

    int64_t item_offset;     /**< current item's absolute byte offset
                              * in file data (-1 if unset) */

    int64_t item_size;       /**< cached item size (-1 if unset) */

    struct box *item_box;    /**< cached item box (NULL if unset) */

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
    TRACKER_STATE_ITEM_BOX = 5,
    TRACKER_STATE_ITEM_BOX_SIZE = 6,
    TRACKER_STATE_AT_END = 7,
    TRACKER_STATE_COUNT = 8
};

struct tracker_error_context_info {
    /** error context messages pointing to offsets in @ref error_buf */
    const char *message;
    struct tracker *tk;
    struct box *box;
    const struct ast_node *node;
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

    const struct ast_node *node; /**< node that relates to the error */

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
            enum box_offset_type registered_end_offset_type;
            int64_t registered_end_offset;
            enum box_offset_type requested_end_offset_type;
            int64_t requested_end_offset;
        } out_of_bounds;
    } u;
};

void
box_acquire(struct box *box);
void
box_delete(struct box *box);
int
box_contains_indexed_items(const struct box *box);
enum expr_value_type
box_get_index_type(const struct box *box);
struct ast_node *
box_get_index_expr(const struct box *box);
struct box *
box_new_from_file(const struct bitpunch_schema_hdl *def_hdl,
                  struct bitpunch_binary_file_hdl *file_hdl);
void
box_dump(const struct box *box, FILE *out);
void
box_cache_clear(struct box_cache *cache);
void
box_cache_free(struct box_cache *cache);
void
box_cache_dump(struct box_cache *cache, FILE *out);
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
tracker_dump(const struct tracker *tk, FILE *out);

enum tracker_state
tracker_get_state(const struct tracker *tk);

struct tracker *
track_file(const struct bitpunch_schema_hdl *def_hdl,
           struct bitpunch_binary_file_hdl *file_hdl);
bitpunch_status_t
box_get_n_items(struct box *box, int64_t *n_itemsp,
                struct tracker_error **errp);
bitpunch_status_t
box_read_value(struct box *box,
               enum expr_value_type *typep, union expr_value *valuep,
               struct tracker_error **errp);
static inline int64_t
box_get_start_offset(struct box *box) { return box->start_offset_used; }
bitpunch_status_t
box_compute_end_offset(struct box *box,
                       enum box_offset_type off_type,
                       int64_t *end_offsetp,
                       struct tracker_error **errp);
struct tracker *
track_box(struct box *box);
struct tracker *
track_item_contents(struct tracker *tk,
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
                                 union expr_value item_key,
                                 struct tracker_error **errp);
bitpunch_status_t
tracker_goto_next_item_with_key(struct tracker *tk,
                                union expr_value item_key,
                                struct tracker_error **errp);
bitpunch_status_t
tracker_goto_nth_item_with_key(struct tracker *tk,
                               union expr_value item_key,
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
bitpunch_status_t
tracker_track_link(struct tracker *tk, const char *link_name,
                   struct tracker **tkp,
                   struct tracker_error **errp);
bitpunch_status_t
box_track_link(struct box *box, const char *link_name,
               struct tracker **tkp,
               struct tracker_error **errp);
int
tracker_get_abs_dpath(const struct tracker *tk,
                      char *dpath_expr_buf, int buf_size);
char *
tracker_get_abs_dpath_alloc(const struct tracker *tk);
int
tracker_dump_abs_dpath(const struct tracker *tk, FILE *stream);

bitpunch_status_t
tracker_get_tracked_container_type(struct tracker *tk,
                                   enum container_type *typep);

bitpunch_status_t
tracker_get_item_offset(struct tracker *tk, int64_t *item_offsetp,
                        struct tracker_error **errp);

bitpunch_status_t
tracker_get_item_size(struct tracker *tk, int64_t *item_sizep,
                      struct tracker_error **errp);

bitpunch_status_t
tracker_get_item_key(struct tracker *tk,
                     enum expr_value_type *key_typep,
                     union expr_value *keyp,
                     struct tracker_error **errp);

bitpunch_status_t
tracker_get_item_key_multi(struct tracker *tk,
                           enum expr_value_type *key_typep,
                           union expr_value *keyp,
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
                        enum expr_value_type *typep,
                        union expr_value *valuep,
                        struct tracker_error **errp);

/* generic statement API */

enum statement_type {
    STATEMENT_TYPE_FIELD,
    STATEMENT_TYPE_LINK,
    STATEMENT_TYPE_SPAN,
    STATEMENT_TYPE_KEY,
    STATEMENT_TYPE_LAST,
    STATEMENT_TYPE_MATCH,
};

enum statement_iterator_flag {
    STATEMENT_ITERATOR_FLAG_REVERSE = (1<<0),
};
struct statement_iterator {
    int stmt_flags;
    enum statement_iterator_flag it_flags;
    const struct statement *next_stmt;
};

const char *
statement_type_str(enum statement_type stmt_type);

struct statement_iterator
box_iter_statements(struct box *box,
                    enum statement_type stmt_type,
                    int stmt_flags);

struct statement_iterator
box_iter_statements_from(struct box *box,
                         const struct statement *stmt,
                         int stmt_flags);

struct statement_iterator
box_riter_statements(struct box *box,
                     enum statement_type stmt_type,
                     int stmt_flags);

struct statement_iterator
box_riter_statements_from(struct box *box,
                          const struct statement *stmt,
                          int stmt_flags);

bitpunch_status_t
box_iter_statements_next(struct box *box, struct statement_iterator *it,
                         const struct statement **stmtp,
                         struct tracker_error **errp);

bitpunch_status_t
box_lookup_statement(struct box *box,
                     enum statement_type stmt_type,
                     const char *stmt_name,
                     const struct named_statement **stmtp,
                     struct tracker_error **errp);

bitpunch_status_t
box_get_first_statement(struct box *box,
                        enum statement_type stmt_type,
                        int stmt_flags,
                        const struct statement **stmtp,
                        struct tracker_error **errp);

bitpunch_status_t
box_get_n_statements(struct box *box,
                     enum statement_type stmt_type,
                     int stmt_flags,
                     int64_t *stmt_countp,
                     struct tracker_error **errp);


/* links API */

typedef struct statement_iterator tlink_iterator;

tlink_iterator
box_iter_links(struct box *box);

bitpunch_status_t
box_iter_links_next(struct box *box, tlink_iterator *it,
                    const struct link **linkp,
                    struct tracker_error **errp);

bitpunch_status_t
box_evaluate_link_target(struct box *box, const char *link_name,
                         enum expr_dpath_type *dpath_typep,
                         union expr_dpath *eval_dpathp,
                         struct tracker_error **errp);

/* error reporting */

void
tracker_error_dump(struct tracker_error *tk_err, FILE *out);

void
tracker_error_destroy(struct tracker_error *tk_err);


/* internal use for other modules */

int
browse_setup_backends(struct ast_node *node);

#endif /*__BROWSE_H__*/
