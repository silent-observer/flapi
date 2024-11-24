#ifndef SOURCESPAN_H
#define SOURCESPAN_H

#include "error.h"
#include "types.h"

typedef struct {
    i32 line;
    i32 col;
} SourcePoint;

typedef struct {
    SourcePoint start;
    SourcePoint end;
} SourceSpan;

static inline SourceSpan makeSourceSpan(i32 line, i32 col, i32 len) {
    SourceSpan span;
    span.start.line = line;
    span.start.col = col;
    span.end.line = line;
    span.end.col = col + len;
    return span;
}

static inline int cmpSourcePoint(SourcePoint a, SourcePoint b) {
    if (a.line != b.line)
        return a.line - b.line;
    return a.col - b.col;
}

static inline SourceSpan combineSourceSpans(SourceSpan a, SourceSpan b) {
    SourceSpan span;
    assert(cmpSourcePoint(a.start, a.end) < 0);
    assert(cmpSourcePoint(a.end, b.start) < 0);
    assert(cmpSourcePoint(b.start, b.end) < 0);
    span.start = a.start;
    span.end = b.end;
    return span;
}

static inline SourceSpan unionSourceSpans(SourceSpan a, SourceSpan b) {
    SourceSpan span;
    assert(cmpSourcePoint(a.start, a.end) < 0);
    assert(cmpSourcePoint(b.start, b.end) < 0);
    if (cmpSourcePoint(a.start, b.start) < 0)
        span.start = a.start;
    else
        span.start = b.start;

    if (cmpSourcePoint(a.end, b.end) > 0)
        span.end = a.end;
    else
        span.end = b.end;
    return span;
}

#endif