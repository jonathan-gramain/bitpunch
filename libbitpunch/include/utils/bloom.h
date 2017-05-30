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

#ifndef __BLOOM_H__
#define __BLOOM_H__

#include "utils/port.h"

typedef int64_t bloom_book_mark_t;

extern const bloom_book_mark_t BLOOM_BOOK_MARK_NONE;

struct bloom_book;
struct bloom_book_cookie;

struct bloom_book *
bloom_book_create(void);

void
bloom_book_destroy(struct bloom_book *book);

int
bloom_book_suggested_n_words_per_mark(struct bloom_book *book);

bloom_book_mark_t
bloom_book_add_mark(struct bloom_book *book);

bloom_book_mark_t
bloom_book_get_cookie_mark(const struct bloom_book *book,
                           const struct bloom_book_cookie *cookie);

void
bloom_book_insert_word(struct bloom_book *book,
                       const char *word, int word_size);

static inline void
bloom_book_lookup_word(struct bloom_book *book,
                       const char *word, int word_size,
                       struct bloom_book_cookie *cookiep);

void
bloom_book_lookup_word_from_mark(struct bloom_book *book,
                                 const char *word, int word_size,
                                 bloom_book_mark_t from_mark,
                                 struct bloom_book_cookie *cookiep);

bloom_book_mark_t
bloom_book_lookup_word_get_next_candidate(struct bloom_book *book,
                                          struct bloom_book_cookie *cookie);



/* internal implementation */

typedef uint64_t h_word_t;
typedef uint64_t bloom_t;

struct bloom_book;
struct bloom_book_page;

struct bloom_book_cookie {
    bloom_t h_bloom_mask;
    h_word_t h_prefix;
    struct bloom_book_page *page;
    int cur_page_mark;
};

static inline void
bloom_book_lookup_word(struct bloom_book *book,
                       const char *word, int word_size,
                       struct bloom_book_cookie *cookiep)
{
    bloom_book_lookup_word_from_mark(book, word, word_size,
                                     BLOOM_BOOK_MARK_NONE,
                                     cookiep);
}


#endif /*__BLOOM_H__*/
