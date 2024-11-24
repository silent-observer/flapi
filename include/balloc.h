#ifndef BALLOC_H
#define BALLOC_H

#include "types.h"
#include <stc/common.h>
#include <string.h>

#define MIN_BITS 4
#define MAX_BITS 13
#define BUCKETS (MAX_BITS - MIN_BITS + 1)
#define CHUNK_SIZE (1 << MAX_BITS)

typedef struct ChunkHeader {
    struct ChunkHeader *next;
} ChunkHeader;

typedef struct {
    u64 bucketIndex : 4;
    i64 offset : 60;
} BlockHeader;

typedef struct BigBlockHeader {
    struct BigBlockHeader *next;
    struct BigBlockHeader *prev;
    u64 bucketIndex : 4;
    i64 offset : 60;
} BigBlockHeader;

typedef struct FreeBlock {
    BlockHeader header;
    struct FreeBlock *next;
} FreeBlock;

typedef struct {
    u32 len;
    ChunkHeader *head;
    FreeBlock *freePtr;
} Bucket;

typedef struct {
    Bucket buckets[BUCKETS];
    BigBlockHeader *bigBlocks;
    u64 totalAllocated;
    u64 totalUsed;
} BAllocator;

void *balloc(BAllocator *allocator, u32 size);
BAllocator *bfind(void *ptr);
void bfree(void *ptr);
void bdrop(BAllocator *allocator);

static inline void *balloc0(BAllocator *allocator, u32 size) {
    void *p = balloc(allocator, size);
    memset(p, 0, size);
    return p;
}
static inline void *bcalloc(BAllocator *allocator, u32 size, u32 count) {
    return balloc0(allocator, size * count);
}

static inline void *brealloc(void *ptr, u32 size) {
    assert(ptr);
    BAllocator *allocator = bfind(ptr);
    void *newPtr = balloc(allocator, size);
    c_memcpy(newPtr, ptr, size);
    bfree(ptr);
    return newPtr;
}

#endif