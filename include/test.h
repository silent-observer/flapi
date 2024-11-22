#ifndef TEST_H
#define TEST_H

#include <stdio.h>
#include <string.h>
#define __FILENAME__ (strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') + 1 : __FILE__)

#ifdef TEST_CRASH
#define test_crash() __builtin_unreachable()
#else
#define test_crash() return 1
#endif

#define test_fail(msg)                                                           \
    do {                                                                         \
        fprintf(stderr, "\x1b[31m[X]\x1b[39m %s\n    Test failed: %s (%s:%d)\n", \
                __func__, msg, __FILENAME__, __LINE__);                          \
        test_crash();                                                            \
    } while (0)

#define test_success()                                         \
    do {                                                       \
        fprintf(stderr, "\x1b[32m[✓]\x1b[39m %s\n", __func__); \
        return 0;                                              \
    } while (0)

#define test_assert(c) \
    if (!(c))          \
    test_fail(#c)

#endif