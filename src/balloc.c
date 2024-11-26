#include "balloc.h"
#include <stdlib.h>

#define BIG_BUCKET 0xF

static usize findBucket(usize size) {
    usize minBlockSize = size + sizeof(BlockHeader);
    if (minBlockSize * 2 > CHUNK_SIZE - sizeof(ChunkHeader))
        return BIG_BUCKET;

    usize bucket = 0;
    while (bucket < BUCKETS) {
        if (minBlockSize <= (1 << (bucket + MIN_BITS)))
            break;
        bucket++;
    }
    return bucket;
}

static BlockHeader *allocBlock(Bucket *bucket, usize index) {
    assert(index < BUCKETS);

    if (bucket->freePtr) {
        FreeBlock *block = bucket->freePtr;
        bucket->freePtr = block->next;
        return &block->header;
    }

    usize blockSize = (1 << (index + MIN_BITS));
    usize curSize = CHUNK_SIZE;

    if (!bucket->head) {
        bucket->head = malloc(curSize);
        bucket->head->next = NULL;
    }

    ChunkHeader *cur = bucket->head;
    usize curIndex = bucket->len;
    while (1) {
        usize curLen = curSize / blockSize - 1;
        if (curIndex < curLen) {
            bucket->len++;
            return (void *)cur + sizeof(ChunkHeader) + curIndex * blockSize;
        }

        curIndex -= curLen;
        curSize *= 2;
        if (!cur->next) {
            cur->next = malloc(curSize - blockSize + sizeof(ChunkHeader));
            cur->next->next = NULL;
        }
        cur = cur->next;
    }
}

static BigBlockHeader *allocBigBlock(BAllocator *allocator, usize size) {
    BigBlockHeader *header = malloc(sizeof(BigBlockHeader) + size);
    header->bucketIndex = BIG_BUCKET;
    header->offset = (void *)allocator - (void *)header;
    header->prev = NULL;
    header->next = allocator->bigBlocks;
    if (header->next)
        header->next->prev = header;
    allocator->bigBlocks = header;
    return header;
}

void *balloc(BAllocator *allocator, u32 size) {
    usize bucketIndex = findBucket(size);

    if (bucketIndex == BIG_BUCKET) {
        BigBlockHeader *header = allocBigBlock(allocator, size);
        return (void *)header + sizeof(BigBlockHeader);
    } else {
        Bucket *bucket = &allocator->buckets[bucketIndex];
        BlockHeader *header = allocBlock(bucket, bucketIndex);
        header->bucketIndex = bucketIndex;
        header->offset = (void *)allocator - (void *)header;
        return (void *)header + sizeof(BlockHeader);
    }
}

BAllocator *bfind(void *ptr) {
    BlockHeader *header = ptr - sizeof(BlockHeader);
    void *result = (void *)header + header->offset;
    if (header->bucketIndex == BIG_BUCKET)
        result -= sizeof(BigBlockHeader) - sizeof(BlockHeader);
    return result;
}

void bfree(void *ptr) {
    if (!ptr)
        return;

    BlockHeader *header = ptr - sizeof(BlockHeader);
    BAllocator *allocator = bfind(ptr);
    if (header->bucketIndex == BIG_BUCKET) {
        BigBlockHeader *bigHeader = ptr - sizeof(BigBlockHeader);

        if (bigHeader->prev)
            bigHeader->prev->next = bigHeader->next;
        else {
            allocator->bigBlocks = bigHeader->next;
            allocator->bigBlocks->prev = NULL;
        }

        if (bigHeader->next)
            bigHeader->next->prev = bigHeader->prev;

        free(bigHeader);
    } else {
        Bucket *bucket = &allocator->buckets[header->bucketIndex];
        FreeBlock *block = (FreeBlock *)header;
        block->next = bucket->freePtr;
        bucket->freePtr = block;
    }
}

void bdrop(BAllocator *allocator) {
    for (usize i = 0; i < BUCKETS; i++) {
        Bucket *bucket = &allocator->buckets[i];
        ChunkHeader *cur = bucket->head;
        while (cur) {
            ChunkHeader *next = cur->next;
            free(cur);
            cur = next;
        }
    }

    BigBlockHeader *cur = allocator->bigBlocks;
    while (cur) {
        BigBlockHeader *next = cur->next;
        free(cur);
        cur = next;
    }
}