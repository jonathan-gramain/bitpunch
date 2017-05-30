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

#ifndef __PORT_H__
#define __PORT_H__

#include <sys/types.h>
#include <sys/param.h>
#include <inttypes.h>

#define ERRBUF_MAXSIZE 4096

#ifndef TRUE
# define TRUE 1
#endif
#ifndef FALSE
# define FALSE 0
#endif

#ifndef MAX
# define MAX(a, b) ({                           \
            typeof (a) __a = a;                  \
            typeof (b) __b = b;                  \
            return (__a >= __b ? __a : __b);    \
        })
#endif
#ifndef MIN
# define MIN(a, b) ({                           \
            typeof (a) __a = a;                  \
            typeof (b) __b = b;                  \
            return (__a <= __b ? __a : __b);    \
        })
#endif
#ifndef SWAP
# define SWAP(a, b) ({                          \
            typeof (*(a)) __a = *(a);            \
            *(a) = *(b);                        \
            *(b) = __a;                         \
        })
#endif

#ifndef N_ELEM
# define N_ELEM(array) (sizeof (array)/sizeof ((array)[0]))
#endif

#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

void *malloc_safe(size_t size);
void *realloc_safe(void *pt, size_t new_size);
#define malloc0_safe(size) memset(malloc_safe(size), 0, (size))
#define new_safe(type) ((type *) malloc0_safe(sizeof (type)))
#define dup_safe(pt) ((typeof (pt)) memcpy(malloc_safe(sizeof (*(pt))), (pt), sizeof (*(pt))))
char *strdup_safe(const char *s);
int is_little_endian(void);

static inline int is_pow2(unsigned int n)
{
    return n && ! ((n - 1) & (n));
}

static inline int log2_i(unsigned long n)
{
    /* undefined for 0 */
    return NBBY * sizeof (n) - 1 - __builtin_clzl(n);
}

#define STATIC_ASSERT(cond) do {                \
        switch (cond) {                         \
        case 0:                                 \
        case cond:                              \
            ;                                   \
        }                                       \
    } while (0)

#endif
