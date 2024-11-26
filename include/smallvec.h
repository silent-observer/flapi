#include "types.h"
#include <stc/common.h>

#ifdef i_TYPE
#define i_type _c_SEL(_c_SEL31, i_TYPE)
#define i_val _c_SEL(_c_SEL32, i_TYPE)
#define i_N _c_SEL(_c_SEL33, i_TYPE)
#endif

#ifndef i_type
#error "i_type not defined"
#endif

#ifndef i_val
#error "i_val not defined"
#endif

#ifndef i_N
#error "i_N not defined"
#endif

#if defined STC_ALLOCATOR && !defined i_allocator
#define i_allocator STC_ALLOCATOR
#elif !defined i_allocator
#define i_allocator c
#endif
#define i_malloc c_JOIN(i_allocator, _malloc)
#define i_realloc c_JOIN(i_allocator, _realloc)
#define i_free c_JOIN(i_allocator, _free)

#define _c_MEMB(name) c_JOIN(i_type, name)

typedef struct {
    i32 cap;
    union {
        i_val data[i_N];
        struct {
            i_val *ptr;
            i32 len;
        };
    };
} i_type;

static inline i_type _c_MEMB(_init)(void) {
    return (i_type){.data = {0}, .cap = 0};
}

static inline void _c_MEMB(_drop)(const i_type *self) {
    if (self->cap <= i_N)
        return;
    assert(self->len == 0);
    i_free(self->ptr, self->cap * sizeof(i_val));
}

static inline void _c_MEMB(_clear)(i_type *self) {
    self->len = 0;
}

static inline i32 _c_MEMB(_size)(const i_type *self) {
    return self->cap <= i_N ? self->cap : self->len;
}

static inline i_val *_c_MEMB(_data)(i_type *self) {
    return self->cap <= i_N ? &self->data[0] : self->ptr;
}

static inline i_val *_c_MEMB(_at)(i_type *self, i32 i) {
    return self->cap <= i_N ? &self->data[i] : &self->ptr[i];
}

static inline void _c_MEMB(_push)(i_type *self, i_val val) {
    if (self->cap < i_N) {
        self->data[self->cap++] = val;
    } else if (self->cap == i_N) {
        i_val *ptr = i_malloc(self->cap * 2 * sizeof(i_val));
        c_memcpy(ptr, self->data, self->cap * sizeof(i_val));
        ptr[i_N] = val;
        self->ptr = ptr;
        self->cap *= 2;
        self->len = i_N + 1;
    } else {
        if (self->len == self->cap) {
            self->ptr = i_realloc(self->ptr, self->cap * sizeof(i_val), self->cap * 2 * sizeof(i_val));
            self->cap *= 2;
        }
        self->data[self->len++] = val;
    }
}

#define i_iter _c_MEMB(_iter)

typedef struct {
    i_val *ref;
    i_val *end;
} i_iter;

static inline i_iter _c_MEMB(_begin)(i_type *self) {
    if (self->cap <= i_N)
        return (i_iter){.ref = &self->data[0], .end = &self->data[self->cap]};
    return (i_iter){.ref = self->ptr, .end = self->ptr + self->len};
}

static inline i_iter _c_MEMB(_end)(i_type *self) {
    return (i_iter){.ref = NULL, .end = NULL};
}

static inline void _c_MEMB(_next)(i_iter *it) {
    if (!it->ref)
        return;
    it->ref += 1;
    if (it->ref == it->end)
        it->ref = NULL;
}

#undef i_TYPE
#undef i_type
#undef i_val
#undef i_N
#undef i_iter

#undef i_allocator
#undef i_malloc
#undef i_realloc
#undef i_free