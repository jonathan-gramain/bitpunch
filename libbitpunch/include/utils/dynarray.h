#ifndef _DYNARRAY_H_
#define _DYNARRAY_H_

#include "utils/port.h"

#ifndef __unused
#define __unused __attribute__((unused))
#endif

#define ARRAY_HEAD(name, type)                  \
  struct name {                                 \
    type *data;                                 \
    size_t n_item;                              \
  }

#define __ARRAY_ALLOC_SIZE(min_size)                            \
  (((1LU << (log2_i(((min_size) - 1) | 0xFLU) + 1)) & 0xFFFFLU) \
   | (((min_size) + 0xFFFFLU) & ~0xFFFFLU))

#define ARRAY_RESIZE(array, type, min_n_item) do {              \
    size_t __cur_size;                                          \
    size_t __cur_alloc;                                         \
    size_t __new_size;                                          \
    size_t __new_alloc;                                         \
    __cur_size = (array)->n_item * sizeof (type);               \
    __cur_alloc = __ARRAY_ALLOC_SIZE(__cur_size);               \
    __new_size = (min_n_item) * sizeof (type);                  \
    __new_alloc = __ARRAY_ALLOC_SIZE(__new_size);               \
    if (__new_alloc != __cur_alloc) {                           \
      (array)->data = realloc_safe((array)->data, __new_alloc); \
    }                                                           \
  } while (0)

#define ARRAY_INIT(array, initial_n_items, type) do {   \
    (array)->data = NULL;                               \
    (array)->n_item = 0;                                \
    ARRAY_RESIZE(array, type, initial_n_items);         \
    (array)->n_item = initial_n_items;                  \
  } while (0)

#define ARRAY_APPEND(array, item, type) do {            \
    ARRAY_RESIZE(array, type, (array)->n_item + 1);     \
    (array)->data[(array)->n_item++] = (item);          \
  } while (0)

#define ARRAY_POP(array, type) ({                       \
      ARRAY_RESIZE(array, type, ARRAY_SIZE(array));     \
      ARRAY_ITEM(array, --ARRAY_SIZE(array));           \
    })
#define ARRAY_ITEM(array, index) (array)->data[index]
#define ARRAY_FIRST(array) ARRAY_ITEM(array, 0)
#define ARRAY_LAST(array) ARRAY_ITEM(array, ARRAY_SIZE(array) - 1)
#define ARRAY_SIZE(array) (array)->n_item
#define ARRAY_ALLOC_SIZE(array, type)                                   \
    __ARRAY_ALLOC_SIZE((array)->n_item * sizeof (type))
#define ARRAY_DESTROY(array) do { free((array)->data); } while (0)

#endif // _DYNARRAY_H_
