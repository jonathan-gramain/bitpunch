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

#ifndef __DEP_RESOLVER_H__
#define __DEP_RESOLVER_H__

#include "utils/port.h"
#include "utils/queue.h"

#define DEP_RESOLVER_MAX_TAGS 32

#if DEP_RESOLVER_MAX_TAGS == 8
typedef uint8_t dep_resolver_tagset_t;
#elif DEP_RESOLVER_MAX_TAGS == 16
typedef uint16_t dep_resolver_tagset_t;
#elif DEP_RESOLVER_MAX_TAGS == 32
typedef uint32_t dep_resolver_tagset_t;
#elif DEP_RESOLVER_MAX_TAGS == 64
typedef uint64_t dep_resolver_tagset_t;
#else
#error DEP_RESOLVER_MAX_TAGS not defined to an acceptable value
#endif

struct dep_resolver;

enum dep_resolver_status {
    DEP_RESOLVER_OK = 0,
    DEP_RESOLVER_ERROR = 1,
    DEP_RESOLVER_AGAIN = 2,
    DEP_RESOLVER_CIRCULAR_DEPENDENCY = 3,
};

typedef enum dep_resolver_status dep_resolver_status_t;

struct dep_resolver_node {
    /** bit field of tags being processed, to check circular dependencies */
    dep_resolver_tagset_t processing_tags;

    /** bit field of tags already resolved */
    dep_resolver_tagset_t resolved_tags;
};

typedef dep_resolver_tagset_t
(*dep_resolver_func_t)(struct dep_resolver *dr,
                       struct dep_resolver_node *node,
                       dep_resolver_tagset_t tags,
                       void *arg);

struct dep_resolver_node_entry {
    TAILQ_ENTRY(dep_resolver_node_entry) tasks;
    SLIST_ENTRY(dep_resolver_node_entry) deps;
    struct dep_resolver_node *node;
    dep_resolver_tagset_t tags;
    void *arg;
};

struct dep_resolver *
dep_resolver_create(dep_resolver_func_t resolve_f);

void
dep_resolver_destroy(struct dep_resolver *dr);
void
dep_resolver_node_init(struct dep_resolver_node *node);

dep_resolver_status_t
dep_resolver_schedule_tags(struct dep_resolver *dr,
                           struct dep_resolver_node *node,
                           dep_resolver_tagset_t tags_pre,
                           dep_resolver_tagset_t tags_post,
                           void *arg);
dep_resolver_status_t
dep_resolver_get_status(struct dep_resolver *dr);
dep_resolver_status_t
dep_resolver_resolve(struct dep_resolver *dr);
struct dep_resolver_node_entry *
dep_resolver_get_error_entry(struct dep_resolver *dr);
struct dep_resolver_node_entry **
dep_resolver_get_circular_dependency(struct dep_resolver *dr);



#endif /*__DEP_RESOLVER_H__*/
