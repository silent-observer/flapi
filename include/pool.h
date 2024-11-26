#include "error.h"

#ifdef i_TYPE
#define i_type _c_SEL(_c_SEL21, i_TYPE)
#define i_val _c_SEL(_c_SEL22, i_TYPE)
#endif

#ifndef i_type
#error "i_type not defined"
#endif

#ifndef i_val
#error "i_val not defined"
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

typedef struct _c_MEMB(__NodeHeader) {
    struct _c_MEMB(__NodeHeader) * next;
} _c_MEMB(__NodeHeader);

typedef union _c_MEMB(__Item) {
    union _c_MEMB(__Item) * nextFree;
    i_val data;
} _c_MEMB(__Item);

typedef struct {
    i32 len;
    i32 n;
    _c_MEMB(__NodeHeader) * head;
    _c_MEMB(__Item) * freeList;
} i_type;

static inline i_type _c_MEMB(_init)(i32 n) {
    return (i_type){.len = 0, .n = n, .head = NULL};
}

static inline _c_MEMB(__NodeHeader) * _c_MEMB(_allocBlock)(i32 n) {
    isize size = sizeof(_c_MEMB(__NodeHeader)) +
                 n * sizeof(_c_MEMB(__Item));
    _c_MEMB(__NodeHeader) *block = i_malloc(size);
    block->next = NULL;
    return block;
}

static inline i_val *_c_MEMB(_new)(i_type *self) {
    if (self->freeList) {
        _c_MEMB(__Item) *item = self->freeList;
        self->freeList = item->nextFree;
        memset(&item->data, 0, sizeof(i_val));
        return &item->data;
    }

    i32 curN = self->n;
    i32 curLen = self->len;
    if (!self->head)
        self->head = _c_MEMB(_allocBlock)(curN);
    _c_MEMB(__NodeHeader) *block = self->head;

    while (true) {
        if (curLen >= curN) {
            curLen -= curN;
            curN *= 2;

            if (block->next == NULL)
                block->next = _c_MEMB(_allocBlock)(curN);
            block = block->next;
        } else {
            void *ptr = (void *)block +
                        sizeof(_c_MEMB(__NodeHeader)) +
                        curLen * sizeof(_c_MEMB(__Item));
            self->len++;
            memset(ptr, 0, sizeof(i_val));
            return (i_val *)ptr;
        }
    }
}

static inline void _c_MEMB(_free)(i_type *self, i_val *ptr) {
    _c_MEMB(__Item) *freePtr = (_c_MEMB(__Item) *)ptr;
    freePtr->nextFree = self->freeList;
    self->freeList = freePtr;
}

static inline void _c_MEMB(_clear)(i_type *self) {
    self->len = 0;
    self->freeList = NULL;
}

static inline void _c_MEMB(_drop)(i_type *self) {
    _c_MEMB(_clear)(self);

    _c_MEMB(__NodeHeader) *block = self->head;
    i32 curN = self->n;
    while (block) {
        _c_MEMB(__NodeHeader) *nextBlock = block->next;
        i_free(block,
               sizeof(_c_MEMB(__NodeHeader)) +
                   curN * sizeof(_c_MEMB(__Item)));
        block = nextBlock;
        curN *= 2;
    }
    self->head = NULL;
}

#undef i_TYPE
#undef i_type
#undef i_val