#ifndef ERROR_H
#define ERROR_H

#include <stdio.h>
#include <string.h>
#define __FILENAME__ (strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') + 1 : __FILE__)

#define panic_fmt(format, ...)                                                        \
    do {                                                                              \
        fprintf(stderr, "\x1b[31m[X]\x1b[39m %s\n    PANIC: " format " at (%s:%d)\n", \
                __func__, __FILENAME__, __LINE__);                                    \
        abort();                                                                      \
    } while (0)

#define panic(msg)                                                            \
    do {                                                                      \
        fprintf(stderr, "\x1b[31m[X]\x1b[39m %s\n    PANIC: %s at (%s:%d)\n", \
                __func__, msg, __FILENAME__, __LINE__);                       \
        abort();                                                              \
    } while (0)

#endif