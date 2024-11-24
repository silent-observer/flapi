#ifndef MEMCXT_H
#define MEMCXT_H

#include "balloc.h"

typedef struct {
    BAllocator allocator;
} MemCxtData;
typedef MemCxtData *MemCxt;

MemCxt newMemCxt(void);
MemCxt switchMemCxt(MemCxt cxt);
void freeMemCxt(MemCxt cxt);

void pushMemCxt(void);
void popMemCxt(void);

extern MemCxt topMemCxt;
extern MemCxt currentMemCxt;

#define mc_malloc(x) balloc(&currentMemCxt->allocator, x)
#define mc_malloc0(x) balloc0(&currentMemCxt->allocator, x)
#define mc_calloc(x, c) bcalloc(&currentMemCxt->allocator, x, c)
#define mc_realloc(ptr, szold, sznew) ((ptr) ? brealloc(ptr, sznew) : balloc(&currentMemCxt->allocator, sznew))
#define mc_free(ptr, sz) bfree(ptr)

#define STC_ALLOCATOR mc

#endif