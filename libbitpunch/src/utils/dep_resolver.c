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
#include <assert.h>
#include <check.h>
#include <stdio.h> // for tests

#include "utils/port.h"
#include "utils/dep_resolver.h"

static inline struct dep_resolver_node_entry *
new_entry(struct dep_resolver_node *node,
          dep_resolver_tagset_t tags, void *arg)
{
    struct dep_resolver_node_entry *entry;

    entry = new_safe(struct dep_resolver_node_entry);
    entry->node = node;
    entry->tags = tags;
    entry->arg = arg;
    return entry;
}

enum dep_resolver_flag {
    /** one or more dependencies have just been added from resolve_f() */
    DR_FLAG_NEW_DEPS = (1u<<0),

    /** circular dependency detected, return an error next */
    DR_FLAG_CIRCULAR_DEP = (1u<<1),

    /** generic error returned by resolve callback */
    DR_FLAG_ERROR = (1u<<2),
};

struct dep_resolver {
    TAILQ_HEAD(task_list, dep_resolver_node_entry) tasks;
    SLIST_HEAD(dep_list, dep_resolver_node_entry) dep_chain;
    dep_resolver_func_t resolve_f;
    dep_resolver_free_arg_t free_arg_f;
    enum dep_resolver_flag flags;
    struct dep_resolver_node_entry *error_entry;
};

static inline void
set_processing(struct dep_resolver_node *node, dep_resolver_tagset_t tags)
{
    node->processing_tags |= tags;
}

static inline void
clear_processing(struct dep_resolver_node *node, dep_resolver_tagset_t tags)
{
    node->processing_tags &= ~tags;
}

static inline int
any_is_processing(struct dep_resolver_node *node, dep_resolver_tagset_t tags)
{
    return 0 != (node->processing_tags & tags);
}

static inline void
set_resolved(struct dep_resolver_node *node, dep_resolver_tagset_t tags)
{
    node->resolved_tags |= tags;
}

static inline dep_resolver_tagset_t
get_unresolved(struct dep_resolver_node *node, dep_resolver_tagset_t tags)
{
    return tags & ~node->resolved_tags;
}

struct dep_resolver *
dep_resolver_create(dep_resolver_func_t resolve_f,
                    dep_resolver_free_arg_t free_arg_f)
{
    struct dep_resolver *dr;

    dr = new_safe(struct dep_resolver);
    TAILQ_INIT(&dr->tasks);
    SLIST_INIT(&dr->dep_chain);
    dr->resolve_f = resolve_f;
    dr->free_arg_f = free_arg_f;
    return dr;
}

void
dep_resolver_destroy(struct dep_resolver *dr)
{
    free(dr);
}

void
dep_resolver_node_init(struct dep_resolver_node *node)
{
    node->processing_tags = 0u;
    node->resolved_tags = 0u;
}

dep_resolver_status_t
dep_resolver_schedule_tags(struct dep_resolver *dr,
                           struct dep_resolver_node *node,
                           dep_resolver_tagset_t tags_pre,
                           dep_resolver_tagset_t tags_post,
                           void *arg)
{
    struct dep_resolver_node_entry *entry;
    dep_resolver_tagset_t unresolved_pre;
    dep_resolver_tagset_t unresolved_post;
    
    unresolved_pre = get_unresolved(node, tags_post);
    if (0 != unresolved_pre) {
        entry = new_entry(node, unresolved_pre, arg);
        TAILQ_INSERT_TAIL(&dr->tasks, entry, tasks);
    }
    unresolved_post = get_unresolved(node, tags_pre);
    if (0 == unresolved_post) {
        if (0 == unresolved_pre && NULL != dr->free_arg_f) {
            // nothing to resolve, free arg now
            dr->free_arg_f(dr, arg);
        }
        return DEP_RESOLVER_OK;
    }
    entry = new_entry(node, unresolved_post, arg);
    if (0 != unresolved_pre) {
        // make sure we do not delete arg after the pre task
        // completes, because there is a following one for post tags
        entry->keep_arg_on_free = TRUE;
    }
    TAILQ_INSERT_HEAD(&dr->tasks, entry, tasks);
    dr->flags |= DR_FLAG_NEW_DEPS;
    if (any_is_processing(node, unresolved_post)) {
        dr->flags |= DR_FLAG_CIRCULAR_DEP;
        SLIST_INSERT_HEAD(&dr->dep_chain, entry, deps);
    }
    if (0 != (dr->flags & DR_FLAG_CIRCULAR_DEP)) {
        return DEP_RESOLVER_CIRCULAR_DEPENDENCY;
    }
    return DEP_RESOLVER_AGAIN;
}

dep_resolver_status_t
dep_resolver_get_status(struct dep_resolver *dr)
{
    if (0 != (dr->flags & DR_FLAG_ERROR)) {
        return DEP_RESOLVER_ERROR;
    }
    if (0 != (dr->flags & DR_FLAG_CIRCULAR_DEP)) {
        return DEP_RESOLVER_CIRCULAR_DEPENDENCY;
    }
    if (0 != (dr->flags & DR_FLAG_NEW_DEPS)) {
        return DEP_RESOLVER_AGAIN;
    }
    return DEP_RESOLVER_OK;
}

dep_resolver_status_t
dep_resolver_resolve(struct dep_resolver *dr)
{
    struct dep_resolver_node_entry *entry;
    dep_resolver_tagset_t unresolved_tags;
    dep_resolver_tagset_t resolved_tags;

    while (!TAILQ_EMPTY(&dr->tasks)) {
        dr->flags &= ~DR_FLAG_NEW_DEPS;
        entry = TAILQ_FIRST(&dr->tasks);
        if (entry == SLIST_FIRST(&dr->dep_chain)) {
            SLIST_REMOVE_HEAD(&dr->dep_chain, deps);
        }
        unresolved_tags = get_unresolved(entry->node, entry->tags);
        if (0 != unresolved_tags) {
            set_processing(entry->node, unresolved_tags);

            resolved_tags = dr->resolve_f(dr, entry->node,
                                          unresolved_tags, entry->arg);
            clear_processing(entry->node, resolved_tags);
            set_resolved(entry->node, resolved_tags);

            if (0 != (dr->flags & DR_FLAG_CIRCULAR_DEP)) {
                return DEP_RESOLVER_CIRCULAR_DEPENDENCY;
            }
            if (entry != TAILQ_FIRST(&dr->tasks)) {
                // restart from tasks list head to handle new
                // dependencies
                SLIST_INSERT_HEAD(&dr->dep_chain, entry, deps);
                continue ;
            }
            if (resolved_tags != unresolved_tags) {
                dr->error_entry = entry;
                dr->flags |= DR_FLAG_ERROR;
                return DEP_RESOLVER_ERROR;
            }
        }
        TAILQ_REMOVE(&dr->tasks, entry, tasks);
        if (NULL != dr->free_arg_f && !entry->keep_arg_on_free) {
            dr->free_arg_f(dr, entry->arg);
        }
        free(entry);
    }
    return DEP_RESOLVER_OK;
}

void
dep_resolver_set_resolved(struct dep_resolver_node *node,
                          dep_resolver_tagset_t tags)
{
    set_resolved(node, tags);
}

struct dep_resolver_node_entry *
dep_resolver_get_error_entry(struct dep_resolver *dr)
{
    return dr->error_entry;
}

struct dep_resolver_node_entry **
dep_resolver_get_circular_dependency(struct dep_resolver *dr)
{
    struct dep_resolver_node_entry **circ_entries = NULL;
    int circ_entries_i = 0;
    int circ_entries_j;
    struct dep_resolver_node_entry *entry;

    SLIST_FOREACH(entry, &dr->dep_chain, deps) {
        if (entry != SLIST_FIRST(&dr->dep_chain) &&
            entry->node == SLIST_FIRST(&dr->dep_chain)->node) {
            break ;
        }
        circ_entries = realloc_safe(
            circ_entries, (circ_entries_i + 2) * sizeof(*circ_entries));
        circ_entries[circ_entries_i] = entry;
        ++circ_entries_i;
    }
    circ_entries[circ_entries_i] = NULL;

    // reverse dep list before returning it
    for (circ_entries_j = 0, --circ_entries_i;
         circ_entries_j < circ_entries_i;
         ++circ_entries_j, --circ_entries_i) {
        entry = circ_entries[circ_entries_i];
        circ_entries[circ_entries_i] = circ_entries[circ_entries_j];
        circ_entries[circ_entries_j] = entry;
    }
    return circ_entries;
}

static void
dump_tags(dep_resolver_tagset_t tags)
{
    int t;
    int first;

    first = TRUE;
    for (t = 0; (1u<<t) <= tags; ++t) {
        if (0 != (tags & (1u<<t))) {
            printf("%s%d", (first ? "" : ","), (1u<<t));
            first = FALSE;
        }
    }
}

static void
dep_resolver_dump_node(struct dep_resolver_node *node)
{
    printf("node %p processing_tags=", node);
    dump_tags(node->processing_tags);
    printf(" resolved_tags=");
    dump_tags(node->resolved_tags);
    printf("\n");
}

static void
dep_resolver_dump_entry(struct dep_resolver_node_entry *entry)
{
    printf("requested_tags=");
    dump_tags(entry->tags);
    printf("\n    arg=%p node: ", entry->arg);
    dep_resolver_dump_node(entry->node);
}

void
dep_resolver_dump(struct dep_resolver *dr)
{
    struct dep_resolver_node_entry *entry;
    
    TAILQ_FOREACH(entry, &dr->tasks, tasks) {
        printf("task entry: ");
        dep_resolver_dump_entry(entry);
    }
    SLIST_FOREACH(entry, &dr->dep_chain, deps) {
        printf("dep chain entry: ");
        dep_resolver_dump_entry(entry);
    }
}

//TESTS

#define TEST_N_TAGS 2
#define TEST_MAX_DEPS_PER_TAG 2

struct test_resolver_node {
    struct dep_resolver_node dr_node;
    struct test_resolver_node *deps[TEST_N_TAGS][TEST_N_TAGS][
        TEST_MAX_DEPS_PER_TAG];
    int n_deps[TEST_N_TAGS][TEST_N_TAGS];
    int resolved[TEST_N_TAGS];
};

static int test_resolver_args[64];
int test_resolver_n_args;

#define CLEAR_ARGS() do {                       \
        test_resolver_n_args = 0;               \
    } while (0)

#define PUSH_ARG() ({                                                   \
            assert(test_resolver_n_args < N_ELEM(test_resolver_args));  \
            test_resolver_args[test_resolver_n_args++] = 0;             \
            &test_resolver_args[test_resolver_n_args - 1];              \
        })

#define CHECK_ARGS() do {                                       \
        int i;                                                  \
                                                                \
        for (i = 0; i < test_resolver_n_args; ++i) {            \
            ck_assert_int_eq(test_resolver_args[i], TRUE);      \
        }                                                       \
    } while (0)

static dep_resolver_tagset_t test_resolve(struct dep_resolver *dr,
                                          struct dep_resolver_node *_node,
                                          dep_resolver_tagset_t tags,
                                          void *arg)
{
    struct test_resolver_node *node = (struct test_resolver_node *)_node;
    int i, j;
    int ret;
    int tag;

    for (tag = 0; (1u<<tag) < tags; ++tag) {
        // no-op
    }
    ck_assert_int_eq((1u<<tag), tags);
    for (i = 0; i < N_ELEM(node->deps[tag]); ++i) {
        for (j = 0; j < N_ELEM(node->deps[tag][i]); ++j) {
            if (NULL != node->deps[tag][i][j]) {
                ret = dep_resolver_schedule_tags(
                    dr, &node->deps[tag][i][j]->dr_node,
                    (1u<<i), 0, PUSH_ARG());
                ck_assert(ret == DEP_RESOLVER_OK ||
                          ret == DEP_RESOLVER_AGAIN ||
                          ret == DEP_RESOLVER_CIRCULAR_DEPENDENCY);
            }
        }
    }
    if (dep_resolver_get_status(dr) != DEP_RESOLVER_OK) {
        return 0u;
    }
    ck_assert_int_ge(tag, 0);
    ck_assert_int_lt(tag, TEST_N_TAGS);
    ck_assert_int_eq(node->resolved[tag], FALSE);
    for (i = 0; i < N_ELEM(node->deps[tag]); ++i) {
        for (j = 0; j < N_ELEM(node->deps[tag][i]); ++j) {
            if (NULL != node->deps[tag][i][j]) {
                ck_assert_int_eq(node->deps[tag][i][j]->resolved[i], TRUE);
            }
        }
    }
    node->resolved[tag] = TRUE;
    return tags;
}

static void
init_nodes_array(struct test_resolver_node nodes[], int n_nodes)
{
    int i;

    for (i = 0; i < n_nodes; ++i) {
        dep_resolver_node_init(&nodes[i].dr_node);
        memset(nodes[i].resolved, 0, sizeof (nodes[i].resolved));
    }
}

static void test_dep_resolver_free_func(struct dep_resolver *dr,
                                        void *arg)
{
    ck_assert_int_eq(*(int *)arg, FALSE);
    *(int *)arg = TRUE;
}

START_TEST(test_dep_resolver_no_cycle)
{
    struct test_resolver_node nodes[10];
    struct test_resolver_node *node;
    struct test_resolver_node *root;
    struct dep_resolver *dr;
    dep_resolver_status_t ret;
    int n_deps;
    struct test_dep {
        int source_id;
        int source_tag;
        int dep_id;
        int dep_tag;
    } deps[] = {
        { 0, 0, 1, 0 },
        { 0, 0, 2, 0 },
        { 1, 0, 1, 1 },
        { 1, 1, 4, 0 },
        { 1, 1, 4, 1 },
        { 1, 1, 2, 0 },
        { 4, 0, 3, 1 },
        { 4, 1, 5, 1 },
        { 3, 1, 2, 0 },
    };
    int i;

    memset(nodes, 0, sizeof (nodes));
    for (i = 0; i < N_ELEM(deps); ++i) {
        node = &nodes[deps[i].source_id];
        n_deps = node->n_deps[deps[i].source_tag][deps[i].dep_tag]++;
        assert(n_deps < TEST_MAX_DEPS_PER_TAG);
        node->deps[deps[i].source_tag][deps[i].dep_tag][n_deps]
            = &nodes[deps[i].dep_id];
    }
    root = &nodes[0];

    init_nodes_array(nodes, N_ELEM(nodes));
    CLEAR_ARGS();
    dr = dep_resolver_create(test_resolve,
                             test_dep_resolver_free_func);
    dep_resolver_schedule_tags(dr, &root->dr_node, 1u<<0, 0u, PUSH_ARG());
    dep_resolver_schedule_tags(dr, &root->dr_node, 1u<<1, 0u, PUSH_ARG());
    ret = dep_resolver_resolve(dr);
    ck_assert_int_eq(ret, DEP_RESOLVER_OK);
    ck_assert_int_eq(root->resolved[0], TRUE);
    ck_assert_int_eq(root->resolved[1], TRUE);
    CHECK_ARGS();
    dep_resolver_destroy(dr);

    init_nodes_array(nodes, N_ELEM(nodes));
    CLEAR_ARGS();
    dr = dep_resolver_create(test_resolve,
                             test_dep_resolver_free_func);
    dep_resolver_schedule_tags(dr, &root->dr_node, 1u<<0, 0u, PUSH_ARG());
    dep_resolver_schedule_tags(dr, &root->dr_node, 0u, 1u<<1, PUSH_ARG());
    ret = dep_resolver_resolve(dr);
    ck_assert_int_eq(ret, DEP_RESOLVER_OK);
    ck_assert_int_eq(root->resolved[0], TRUE);
    ck_assert_int_eq(root->resolved[1], TRUE);
    CHECK_ARGS();
    dep_resolver_destroy(dr);

    init_nodes_array(nodes, N_ELEM(nodes));
    CLEAR_ARGS();
    dr = dep_resolver_create(test_resolve,
                             test_dep_resolver_free_func);
    dep_resolver_schedule_tags(dr, &root->dr_node, 0u, 1u<<0, PUSH_ARG());
    dep_resolver_schedule_tags(dr, &root->dr_node, 1u<<1, 0u, PUSH_ARG());
    ret = dep_resolver_resolve(dr);
    ck_assert_int_eq(ret, DEP_RESOLVER_OK);
    ck_assert_int_eq(root->resolved[0], TRUE);
    ck_assert_int_eq(root->resolved[1], TRUE);
    CHECK_ARGS();
    dep_resolver_destroy(dr);

    init_nodes_array(nodes, N_ELEM(nodes));
    CLEAR_ARGS();
    dr = dep_resolver_create(test_resolve,
                             test_dep_resolver_free_func);
    dep_resolver_schedule_tags(dr, &root->dr_node, 0u, 1u<<0, PUSH_ARG());
    dep_resolver_schedule_tags(dr, &root->dr_node, 0u, 1u<<1, PUSH_ARG());
    ret = dep_resolver_resolve(dr);
    ck_assert_int_eq(ret, DEP_RESOLVER_OK);
    ck_assert_int_eq(root->resolved[0], TRUE);
    ck_assert_int_eq(root->resolved[1], TRUE);
    CHECK_ARGS();
    dep_resolver_destroy(dr);

    init_nodes_array(nodes, N_ELEM(nodes));
    CLEAR_ARGS();
    dr = dep_resolver_create(test_resolve,
                             test_dep_resolver_free_func);
    dep_resolver_schedule_tags(dr, &root->dr_node, 1u<<0, 1u<<1, PUSH_ARG());
    ret = dep_resolver_resolve(dr);
    ck_assert_int_eq(ret, DEP_RESOLVER_OK);
    ck_assert_int_eq(root->resolved[0], TRUE);
    ck_assert_int_eq(root->resolved[1], TRUE);
    CHECK_ARGS();
    dep_resolver_destroy(dr);
}
END_TEST

START_TEST(test_dep_resolver_cycle)
{
    struct test_resolver_node nodes[10];
    struct test_resolver_node *node;
    struct test_resolver_node *root;
    struct dep_resolver *dr;
    dep_resolver_status_t ret;
    int n_deps;
    struct test_dep {
        int source_id;
        int source_tag;
        int dep_id;
        int dep_tag;
    } deps[] = {
        { 0, 0, 1, 0 },
        { 0, 0, 2, 0 },
        { 1, 0, 1, 1 },
        { 1, 1, 4, 0 },
        { 1, 1, 4, 1 },
        { 1, 1, 2, 0 },
        { 4, 0, 3, 1 },
        { 4, 1, 5, 1 },
        { 3, 1, 2, 0 },
        // introduce dependency cycle
        // (3, 1) -> (1, 0) -> (1, 1) -> (4, 0) -> (3, 1)
        { 3, 1, 1, 0 },
    };
    int i;

    memset(nodes, 0, sizeof (nodes));
    for (i = 0; i < N_ELEM(deps); ++i) {
        node = &nodes[deps[i].source_id];
        n_deps = node->n_deps[deps[i].source_tag][deps[i].dep_tag]++;
        assert(n_deps < TEST_MAX_DEPS_PER_TAG);
        node->deps[deps[i].source_tag][deps[i].dep_tag][n_deps]
            = &nodes[deps[i].dep_id];
    }
    root = &nodes[0];

    init_nodes_array(nodes, N_ELEM(nodes));
    dr = dep_resolver_create(test_resolve, NULL);
    dep_resolver_schedule_tags(dr, &root->dr_node, 1u<<0, 0u, NULL);
    dep_resolver_schedule_tags(dr, &root->dr_node, 1u<<1, 0u, NULL);
    ret = dep_resolver_resolve(dr);
    // circular dependency shall cause an error return
    ck_assert_int_eq(ret, DEP_RESOLVER_CIRCULAR_DEPENDENCY);
    dep_resolver_destroy(dr);

    init_nodes_array(nodes, N_ELEM(nodes));
    dr = dep_resolver_create(test_resolve, NULL);
    dep_resolver_schedule_tags(dr, &root->dr_node, 0u, 1u<<0, NULL);
    dep_resolver_schedule_tags(dr, &root->dr_node, 1u<<1, 0u, NULL);
    ret = dep_resolver_resolve(dr);
    ck_assert_int_eq(ret, DEP_RESOLVER_CIRCULAR_DEPENDENCY);
    dep_resolver_destroy(dr);

    init_nodes_array(nodes, N_ELEM(nodes));
    dr = dep_resolver_create(test_resolve, NULL);
    dep_resolver_schedule_tags(dr, &root->dr_node, 0u, 1u<<0, NULL);
    dep_resolver_schedule_tags(dr, &root->dr_node, 1u<<1, 0u, NULL);
    ret = dep_resolver_resolve(dr);
    ck_assert_int_eq(ret, DEP_RESOLVER_CIRCULAR_DEPENDENCY);
    dep_resolver_destroy(dr);

    init_nodes_array(nodes, N_ELEM(nodes));
    dr = dep_resolver_create(test_resolve, NULL);
    dep_resolver_schedule_tags(dr, &root->dr_node, 0u, 1u<<0, NULL);
    dep_resolver_schedule_tags(dr, &root->dr_node, 0u, 1u<<1, NULL);
    ret = dep_resolver_resolve(dr);
    ck_assert_int_eq(ret, DEP_RESOLVER_CIRCULAR_DEPENDENCY);
    dep_resolver_destroy(dr);
}
END_TEST

void check_dep_resolver_add_tcases(Suite *s)
{
    TCase *tc;

    tc = tcase_create("dep_resolver_no_cycle");
    tcase_add_test(tc, test_dep_resolver_no_cycle);
    suite_add_tcase(s, tc);

    tc = tcase_create("dep_resolver_cycle");
    tcase_add_test(tc, test_dep_resolver_cycle);
    suite_add_tcase(s, tc);
}
