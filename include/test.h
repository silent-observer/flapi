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

#define test_header() fprintf(stderr, "--> %s\n", __func__)

#define subtest_fail(msg, subtest)                                                   \
    do {                                                                             \
        fprintf(stderr, "    \x1b[31m[X]\x1b[39m %s\n    Test failed: %s (%s:%d)\n", \
                subtest, msg, __FILENAME__, __LINE__);                               \
        return 1;                                                                    \
    } while (0)

#define subtest_success(subtest)                                  \
    do {                                                          \
        fprintf(stderr, "    \x1b[32m[✓]\x1b[39m %s\n", subtest); \
        return 0;                                                 \
    } while (0)

#define test_assert(c) \
    if (!(c))          \
    test_fail(#c)

#define subtest_assert(c, subtest) \
    if (!(c))                      \
    subtest_fail(#c, subtest)

#define test_assert_cleanup(c, ...) \
    do                              \
        if (!(c)) {                 \
            __VA_ARGS__             \
            test_fail(#c);          \
        }                           \
    while (0)

#define test_assert_defer(c) test_assert_cleanup(c, DEFER)

#define test_foreach_file(f, list)                \
    do {                                          \
        test_header();                            \
        StrVec __names = readTextFileLines(list); \
                                                  \
        int __err = 0;                            \
        c_foreach(__it, StrVec, __names) {        \
            pushMemCxt();                         \
            __err |= f(cstr_str(__it.ref));       \
            popMemCxt();                          \
        }                                         \
        StrVec_drop(&__names);                    \
        test_assert(!__err);                      \
        return 0;                                 \
    } while (0)

#endif