#include "memcxt.h"

static MemCxtData topMemCxtData = {0};

MemCxt topMemCxt = &topMemCxtData;
MemCxt currentMemCxt = &topMemCxtData;

MemCxt newMemCxt(void) {
    MemCxtData *data = balloc0(&topMemCxt->allocator, sizeof(MemCxtData));
    return data;
}

MemCxt switchMemCxt(MemCxt cxt) {
    MemCxt old = currentMemCxt;
    currentMemCxt = cxt;
    return old;
}

void freeMemCxt(MemCxt cxt) {
    bdrop(&cxt->allocator);
    bfree(cxt);
}

#define MAX_MEM_CXTS 32

static MemCxt memCxtStack[MAX_MEM_CXTS];
static u32 memCxtStackCount = 0;

void pushMemCxt(void) {
    assert(memCxtStackCount < MAX_MEM_CXTS);
    MemCxt new = newMemCxt();
    memCxtStack[memCxtStackCount++] = switchMemCxt(new);
}
void popMemCxt(void) {
    assert(memCxtStackCount > 0);
    switchMemCxt(memCxtStack[--memCxtStackCount]);
}