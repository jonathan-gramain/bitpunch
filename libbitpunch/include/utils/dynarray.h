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

#ifndef __DYNARRAY_H__
#define __DYNARRAY_H__

#include <stdlib.h>

#include "utils/port.h"

#ifndef __unused
#define __unused __attribute__((unused))
#endif

#define ARRAY_HEAD(name, type)                  \
    struct name {                               \
        type *data;                             \
        size_t n_item;                          \
    }

#define __ARRAY_ALLOC_SIZE(min_size)                                    \
    (((1LU << (log2_i(((min_size) - 1) | 0xFLU) + 1)) & 0xFFFFLU)       \
     | (((min_size) + 0xFFFFLU) & ~0xFFFFLU))

#define ARRAY_RESIZE(array, min_n_item) do {                            \
        size_t __cur_size;                                              \
        size_t __cur_alloc;                                             \
        size_t __new_size;                                              \
        size_t __new_alloc;                                             \
        __cur_size = (array)->n_item * sizeof (*(array)->data);         \
        __cur_alloc = __ARRAY_ALLOC_SIZE(__cur_size);                   \
        __new_size = (min_n_item) * sizeof (*(array)->data);            \
        __new_alloc = __ARRAY_ALLOC_SIZE(__new_size);                   \
        if (__new_alloc != __cur_alloc) {                               \
            (array)->data = realloc_safe((array)->data, __new_alloc);   \
        }                                                               \
    } while (0)

#define ARRAY_INIT(array, initial_n_items) do {                         \
        (array)->data = NULL;                                           \
        (array)->n_item = 0;                                            \
        ARRAY_RESIZE(array, initial_n_items);                           \
        (array)->n_item = initial_n_items;                              \
    } while (0)

#define ARRAY_PUSH(array, item) do {                                    \
        ARRAY_RESIZE(array, (array)->n_item + 1);                       \
        (array)->data[(array)->n_item++] = (item);                      \
    } while (0)

#define ARRAY_POP(array) ({                                     \
            ARRAY_RESIZE(array, ARRAY_SIZE(array));             \
            ARRAY_ITEM(array, --ARRAY_SIZE(array));             \
        })
#define ARRAY_ITEM(array, index) (array)->data[index]
#define ARRAY_FIRST(array) ARRAY_ITEM(array, 0)
#define ARRAY_LAST(array) ARRAY_ITEM(array, ARRAY_SIZE(array) - 1)
#define ARRAY_SIZE(array) (array)->n_item
#define ARRAY_ALLOC_SIZE(array, type)                   \
    __ARRAY_ALLOC_SIZE((array)->n_item * sizeof (type))
#define ARRAY_DESTROY(array) free((array)->data)

#define ARRAY_FOREACH(array, pvar)                              \
    for ((pvar) = &ARRAY_FIRST(array);                          \
         (pvar) != &ARRAY_ITEM(array, ARRAY_SIZE(array));       \
         ++(pvar))

#define ARRAY_GENERATE_API_DECLS(name, type)            \
    ARRAY_HEAD(name, type);                             \
                                                        \
    void name##_init(                                   \
        struct name *array,                             \
        size_t initial_n_items);                        \
    void name##_destroy(                                \
        struct name *array);                            \
    void name##_push(                                   \
        struct name *array,                             \
        type item);                                     \
    void name##_pushp(                                  \
        struct name *array,                             \
        type *item);                                    \
    void name##_pop(                                    \
        struct name *array);                            \

#define ARRAY_GENERATE_API_DEFS(name, type)             \
    void name##_init(                                   \
        struct name *array,                             \
        size_t initial_n_items) {                       \
        ARRAY_INIT(array, initial_n_items);             \
    }                                                   \
    void name##_destroy(                                \
        struct name *array) {                           \
        ARRAY_DESTROY(array);                           \
    }                                                   \
    void name##_push(                                   \
        struct name *array,                             \
        type item) {                                    \
        ARRAY_PUSH(array, item);                        \
    }                                                   \
    void name##_pushp(                                  \
        struct name *array,                             \
        type *item) {                                   \
        ARRAY_PUSH(array, *item);                       \
    }                                                   \
    void name##_pop(                                    \
        struct name *array) {                           \
        ARRAY_POP(array);                               \
    }                                                   \


#endif // __DYNARRAY_H__
