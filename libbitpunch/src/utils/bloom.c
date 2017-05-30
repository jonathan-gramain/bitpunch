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
#include <stddef.h>
#include <check.h>
#include <stdio.h>

#include "utils/bloom.h"
#include "utils/queue.h"

#define LOG2_N_BLOOMS_PER_PAGE_LEVEL  8
#define N_BLOOMS_PER_PAGE_LEVEL       (1u << LOG2_N_BLOOMS_PER_PAGE_LEVEL)

#define SUGGESTED_WORDS_PER_BLOOM     8
#define N_PAGE_LEVELS                 2

const bloom_book_mark_t BLOOM_BOOK_MARK_NONE = -1;

static const int word_prefix_len_per_page_level[N_PAGE_LEVELS] = {
    2 /* level 0 */,
    8 /* level 1 */,
};

static int
log2_n_blooms_per_prefix_level(int level)
{
    return (LOG2_N_BLOOMS_PER_PAGE_LEVEL
            - word_prefix_len_per_page_level[level]);
}

#define N_MARKS_PER_PAGE (1u << log2_n_blooms_per_prefix_level(0))
#define PAGE_MARK_NOMORE  -1

static inline void bloom_init_range(bloom_t *blooms, int n_blooms)
{
    memset(blooms, 0xff, n_blooms * sizeof (*blooms));
}

static inline void bloom_insert(bloom_t *bloom, bloom_t mask)
{
    *bloom &= ~mask;
}

static inline int bloom_contains(bloom_t bloom, bloom_t mask)
{
    return 0 == (bloom & mask);
}


struct bloom_book_page {
    /** Series of word hashes bloom filters to find the matching
     *  mark(s) for a word, one series per word prefix.
     */
    bloom_t *blooms;
};

struct bloom_book {
    struct bloom_book_page *pages;
    int n_pages;
    int cur_page_mark;
};


struct bloom_book *
bloom_book_create(void)
{
    struct bloom_book *book;

    book = new_safe(struct bloom_book);
    book->cur_page_mark = N_MARKS_PER_PAGE - 1;
    book->pages = NULL;
    book->n_pages = 0;
    return book;
}

void
bloom_book_destroy(struct bloom_book *book)
{
    struct bloom_book_page *pagep;

    for (pagep = book->pages;
         pagep < book->pages + book->n_pages; ++pagep) {
        free(pagep->blooms);
    }
    free(book->pages);
    free(book);
}

int
bloom_book_suggested_n_words_per_mark(struct bloom_book *book)
{
    return (SUGGESTED_WORDS_PER_BLOOM
            * (1 << word_prefix_len_per_page_level[0]));
}

bloom_book_mark_t
bloom_book_add_mark(struct bloom_book *book)
{
    ++book->cur_page_mark;
    if (book->cur_page_mark == N_MARKS_PER_PAGE) {
        struct bloom_book_page *page;

        book->pages = realloc_safe(book->pages,
                                   (book->n_pages + 1)
                                   * sizeof (*book->pages));
        page = &book->pages[book->n_pages];
        page->blooms = malloc_safe(N_PAGE_LEVELS
                                   * N_BLOOMS_PER_PAGE_LEVEL
                                   * sizeof(bloom_t));
        bloom_init_range(page->blooms,
                         N_PAGE_LEVELS * N_BLOOMS_PER_PAGE_LEVEL);
        book->cur_page_mark = 0;
        ++book->n_pages;
    }
    return (book->n_pages - 1) * N_MARKS_PER_PAGE + book->cur_page_mark;
}

bloom_book_mark_t
bloom_book_get_cookie_mark(const struct bloom_book *book,
                           const struct bloom_book_cookie *cookie)
{
    if (NULL == cookie->page ||
        PAGE_MARK_NOMORE == cookie->cur_page_mark) {
        return BLOOM_BOOK_MARK_NONE;
    }
    return ((cookie->page - book->pages) * N_MARKS_PER_PAGE
            + cookie->cur_page_mark);
}

static int
bloom_book_set_mark_in_cookie(const struct bloom_book *book,
                              bloom_book_mark_t mark,
                              struct bloom_book_cookie *cookiep)
{
    if (BLOOM_BOOK_MARK_NONE == mark) {
        cookiep->page = NULL;
        cookiep->cur_page_mark = 0;
    } else {
        int page_id;

        page_id = mark / N_MARKS_PER_PAGE;
        if (page_id >= book->n_pages) {
            return -1;
        }
        cookiep->page = &book->pages[page_id];
        cookiep->cur_page_mark = mark % N_MARKS_PER_PAGE;
    }
    return 0;
}

/**
 * @brief Function based on Daniel J. Bernstein's hash algorithm, but
 * using modular arithmetic modulo 2^64 (instead of modulo 2^32).
 */
static h_word_t
hash_word_djb2_64bit(const uint8_t *word, int word_size)
{
    const uint8_t *word_end;
    h_word_t hash = 5381;
    h_word_t c;

    word_end = word + word_size;
    while (word < word_end) {
        c = (h_word_t)*word;
        ++word;
        hash = ((hash << 5) + hash) ^ c; // hash(i) = (hash(i-1) * 33) ^ c
    }
    return hash;
}

static void
extract_hash_prefix_and_bloom_mask(h_word_t h_word,
                                   h_word_t *h_prefixp,
                                   bloom_t *h_bloom_maskp)
{
    bloom_t h_bloom_mask;

    h_bloom_mask = (1LU << (h_word & 63LU));
    h_word >>= 6;
    h_bloom_mask |= (1LU << (h_word & 63LU));
    h_word >>= 6;
    h_bloom_mask |= (1LU << (h_word & 63LU));
    h_word >>= 6;
    h_bloom_mask |= (1LU << (h_word & 63LU));
    h_word >>= 6;
    *h_prefixp = h_word;
    *h_bloom_maskp = h_bloom_mask;
}

static inline int
truncate_prefix(h_word_t prefix, int prefix_len)
{
    return prefix & ((1 << prefix_len) - 1);
}

static int
get_page_level_prefix_bloom_idx(int level, int h_prefix_level)
{
    return h_prefix_level << log2_n_blooms_per_prefix_level(level);
}

static int
get_page_mark_bloom_idx_in_level_prefix(int level, int page_mark)
{
    return (page_mark
            >> (word_prefix_len_per_page_level[level] -
                word_prefix_len_per_page_level[0]));
}

static int
get_start_page_mark_from_bloom_idx(int level, int bloom_idx,
                                   int h_prefix_level)
{
    return ((bloom_idx
             - get_page_level_prefix_bloom_idx(level, h_prefix_level))
            << (word_prefix_len_per_page_level[level] -
                word_prefix_len_per_page_level[0]));
}

static int
get_page_mark_bloom_idx_in_level(int level, int page_mark,
                                h_word_t h_prefix)
{
    int h_prefix_level;
    int bloom_idx;

    h_prefix_level =
        truncate_prefix(h_prefix, word_prefix_len_per_page_level[level]);
    bloom_idx = (get_page_level_prefix_bloom_idx(level, h_prefix_level)
                 + get_page_mark_bloom_idx_in_level_prefix(level,
                                                           page_mark));
    assert(bloom_idx < N_BLOOMS_PER_PAGE_LEVEL);
    return bloom_idx;
}

static void
bloom_book_insert_hash_internal(struct bloom_book *book, h_word_t h_word)
{
    h_word_t h_prefix;
    bloom_t h_bloom_mask;
    struct bloom_book_page *page;
    bloom_t *level_blooms;
    int bloom_idx;
    int level;

    /* at least one mark shall have been added */
    assert(book->n_pages > 0);

    extract_hash_prefix_and_bloom_mask(h_word, &h_prefix, &h_bloom_mask);
    page = &book->pages[book->n_pages - 1];

    level_blooms = page->blooms;
    for (level = 0; level < N_PAGE_LEVELS; ++level) {
        bloom_idx = get_page_mark_bloom_idx_in_level(level,
                                                     book->cur_page_mark,
                                                     h_prefix);
        bloom_insert(&level_blooms[bloom_idx], h_bloom_mask);
        level_blooms += N_BLOOMS_PER_PAGE_LEVEL;
    }
}

void
bloom_book_insert_word(struct bloom_book *book,
                       const char *word, int word_size)
{
    h_word_t h_word;

    h_word = hash_word_djb2_64bit((const uint8_t *)word, word_size);
    bloom_book_insert_hash_internal(book, h_word);
}

void
bloom_book_lookup_word_from_mark(struct bloom_book *book,
                                 const char *word, int word_size,
                                 bloom_book_mark_t from_mark,
                                 struct bloom_book_cookie *cookiep)
{
    h_word_t h_word;
    bloom_book_mark_t cookie_mark;

    assert(NULL != cookiep);
    h_word = hash_word_djb2_64bit((const uint8_t *)word, word_size);
    extract_hash_prefix_and_bloom_mask(h_word,
                                       &cookiep->h_prefix,
                                       &cookiep->h_bloom_mask);
    if (BLOOM_BOOK_MARK_NONE == from_mark || 0 == from_mark) {
        cookie_mark = BLOOM_BOOK_MARK_NONE;
    } else {
        assert(from_mark > 0);
        cookie_mark = from_mark - 1;
    }
    if (-1 == bloom_book_set_mark_in_cookie(book, cookie_mark, cookiep)) {
        cookiep->page = NULL;
        cookiep->cur_page_mark = PAGE_MARK_NOMORE;
    }
}

static void
bloom_book_lookup_word_internal(struct bloom_book *book,
                                struct bloom_book_cookie *cookie)
{
    h_word_t h_prefix;
    bloom_t h_bloom_mask;
    struct bloom_book_page *page;
    int cur_page_mark;
    bloom_t *level_blooms;
    int h_prefix_level[N_PAGE_LEVELS];
    int bloom_idx;
    int bloom_end_idx[N_PAGE_LEVELS];
    int level;

    h_prefix = cookie->h_prefix;
    h_bloom_mask = cookie->h_bloom_mask;
    page = cookie->page;
    if (NULL != page) {
        cur_page_mark = cookie->cur_page_mark + 1;
        if (cur_page_mark == N_MARKS_PER_PAGE) {
            ++page;
            cur_page_mark = 0;
        }
    } else if (PAGE_MARK_NOMORE == cookie->cur_page_mark) {
        return ;
    } else {
        page = book->pages;
        cur_page_mark = 0;
    }
    for (level = 0; level < N_PAGE_LEVELS; ++level) {
        h_prefix_level[level] =
            truncate_prefix(h_prefix,
                            word_prefix_len_per_page_level[level]);

        bloom_end_idx[level] =
            get_page_level_prefix_bloom_idx(level,
                                            h_prefix_level[level] + 1u);
    }
    while (page < book->pages + book->n_pages) {
        level_blooms =
            page->blooms + N_PAGE_LEVELS * N_BLOOMS_PER_PAGE_LEVEL;
        for (level = N_PAGE_LEVELS - 1; level >= 0; --level) {
            level_blooms -= N_BLOOMS_PER_PAGE_LEVEL;
            bloom_idx = get_page_mark_bloom_idx_in_level(level,
                                                         cur_page_mark,
                                                         h_prefix);
            if (page == book->pages + book->n_pages - 1) {
                bloom_end_idx[level] =
                    get_page_mark_bloom_idx_in_level(level,
                                                     book->cur_page_mark,
                                                     h_prefix) + 1;
            }
            for (; bloom_idx < bloom_end_idx[level]; ++bloom_idx) {
                if (bloom_contains(level_blooms[bloom_idx], h_bloom_mask)) {
                    cur_page_mark =
                        get_start_page_mark_from_bloom_idx(
                            level, bloom_idx, h_prefix_level[level]);
                    if (cur_page_mark <= cookie->cur_page_mark
                        && page == cookie->page) {
                        cur_page_mark = cookie->cur_page_mark + 1;
                    }
                    break ;
                }
            }
            if (bloom_idx == bloom_end_idx[level]) {
                /* word is definitely not in page */
                goto next_page;
            }
        }
        assert(level < 0);
        /* word is probably in set */
        cookie->page = page;
        cookie->cur_page_mark =
            bloom_idx
            - get_page_level_prefix_bloom_idx(0, h_prefix_level[0]);
        return ;

      next_page:
        ++page;
        cur_page_mark = 0;
    }
    /* word is definitely not in set */
    cookie->page = NULL;
    cookie->cur_page_mark = PAGE_MARK_NOMORE;
}

bloom_book_mark_t
bloom_book_lookup_word_get_next_candidate(struct bloom_book *book,
                                          struct bloom_book_cookie *cookie)
{
    assert(NULL != cookie);

    bloom_book_lookup_word_internal(book, cookie);

    return bloom_book_get_cookie_mark(book, cookie);
}


//TESTS

START_TEST(test_index_simple)
{
    struct bloom_book *book;
    struct bloom_book_cookie cookie;
    bloom_book_mark_t mark;
    bloom_book_mark_t from_mark;
    bloom_book_mark_t from_mark_2;

    book = bloom_book_create();
    from_mark = bloom_book_add_mark(book);
    ck_assert_int_eq(from_mark, 0);
    bloom_book_insert_word(book, "See", 3);
    bloom_book_insert_word(book, "Ya", 2);
    bloom_book_insert_word(book, "Later", 5);
    bloom_book_insert_word(book, "!", 1);
    from_mark_2 = bloom_book_add_mark(book);
    bloom_book_insert_word(book, "Or", 2);
    bloom_book_insert_word(book, "Not", 3);
    bloom_book_insert_word(book, "Later", 5);
    bloom_book_insert_word(book, "!", 1);

    bloom_book_lookup_word(book, "Later", 5, &cookie);
    mark = bloom_book_lookup_word_get_next_candidate(book, &cookie);
    ck_assert_int_eq(mark, 0);
    bloom_book_lookup_word_from_mark(book, "Later", 5,
                                     from_mark, &cookie);
    mark = bloom_book_lookup_word_get_next_candidate(book, &cookie);
    ck_assert_int_eq(mark, from_mark);
    bloom_book_lookup_word_from_mark(book, "Later", 5,
                                     from_mark_2, &cookie);
    mark = bloom_book_lookup_word_get_next_candidate(book, &cookie);
    ck_assert_int_eq(mark, from_mark_2);

    bloom_book_lookup_word(book, "Later on...", 11, &cookie);
    mark = bloom_book_lookup_word_get_next_candidate(book, &cookie);
    ck_assert(mark == BLOOM_BOOK_MARK_NONE || mark == 0);
    bloom_book_lookup_word_from_mark(book, "Later on...", 11,
                                     from_mark, &cookie);
    mark = bloom_book_lookup_word_get_next_candidate(book, &cookie);
    ck_assert(mark == BLOOM_BOOK_MARK_NONE || mark == from_mark);
    bloom_book_lookup_word_from_mark(book, "Later on...", 11,
                                     from_mark_2, &cookie);
    mark = bloom_book_lookup_word_get_next_candidate(book, &cookie);
    ck_assert(mark == BLOOM_BOOK_MARK_NONE || mark == from_mark_2);

    bloom_book_lookup_word(book, "See", 3, &cookie);
    mark = bloom_book_lookup_word_get_next_candidate(book, &cookie);
    ck_assert_int_eq(mark, 0);
    bloom_book_lookup_word_from_mark(book, "See", 3, from_mark, &cookie);
    mark = bloom_book_lookup_word_get_next_candidate(book, &cookie);
    ck_assert_int_eq(mark, from_mark);
    bloom_book_lookup_word_from_mark(book, "See", 3, from_mark_2, &cookie);
    mark = bloom_book_lookup_word_get_next_candidate(book, &cookie);
    ck_assert(mark == BLOOM_BOOK_MARK_NONE || mark == from_mark_2);

    bloom_book_destroy(book);
}
END_TEST

START_TEST(test_index_100K)
{
    const char *word_chars = "0123456789abcdefghijklmnopqrstuvwxyz";
    struct bloom_book *book;
    bloom_book_mark_t mark;
    struct bloom_book_cookie cookie;
    int i;
    int k_i;
    char word_buf[64];
    int word_size;
    int k_c;
    int i_canary;
    int n_words = 100000;
    int step;
    int ret __attribute__((unused));
    int nb_hops;

    book = bloom_book_create();

    srand(0);
    nb_hops = 0;

    step = bloom_book_suggested_n_words_per_mark(book);

    // insert test
    for (i = 0; i < n_words; ++i) {
        k_i = i;
        if (k_i % step == 0) {
            mark = bloom_book_add_mark(book);
            ck_assert_int_eq(mark, k_i / step);
        }
        srand(k_i);
        word_size = 32 + rand() % 31;
        for (k_c = 0; k_c < word_size; ++k_c) {
            word_buf[k_c] = word_chars[rand() % (sizeof (word_chars) - 1)];
        }
        bloom_book_insert_word(book, word_buf, word_size);
    }

    // positive lookup tests
    for (i = 0; i < n_words; ++i) {
        k_i = rand() % n_words;
        srand(k_i);
        word_size = 32 + rand() % 31;
        for (k_c = 0; k_c < word_size; ++k_c) {
            word_buf[k_c] = word_chars[rand() % (sizeof (word_chars) - 1)];
        }
        bloom_book_lookup_word(book, word_buf, word_size, &cookie);
        i_canary = 0;
        do {
            ++nb_hops;
            ++i_canary;
            /* break after high bound on number of iterations, in case
             * a broken implementation causes infinite loop */
            if (i_canary == n_words) {
                ck_assert_int_lt(i_canary, n_words);
            }
            /* search until the correct offset is found */
            mark = bloom_book_lookup_word_get_next_candidate(book, &cookie);
            ck_assert_int_ne(mark, BLOOM_BOOK_MARK_NONE);
        } while (mark != k_i / step);
        ck_assert_int_eq(mark, k_i / step); /* always passes */
    }

    printf("nb_hops=%d avg_hops_per_word=%.1f\n",
           nb_hops,
           ((double)nb_hops / (double)n_words));

    bloom_book_destroy(book);
}
END_TEST

void check_index_add_tcases(Suite *s)
{
    TCase *tc_index;

    tc_index = tcase_create("index");
    tcase_add_test(tc_index, test_index_simple);
    tcase_add_test(tc_index, test_index_100K);
    suite_add_tcase(s, tc_index);
}
