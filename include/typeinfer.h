#ifndef TYPEINFER_H
#define TYPEINFER_H

#include "ast.h"
#include "typeid.h"

typedef struct {
    SourceSpan s;
    cstr msg;
} TypingError;

#define i_TYPE TypingErrorVec, TypingError
#include <stc/vec.h>

TypingErrorVec typeinfer(Ast *ast);

static inline cstr printTypingError(const TypingError *error, const TypeMap *tm, const SymbolTable *symbols) {
    cstr str = cstr_init();
    csview sv = cstr_sv(&error->msg);
    cstr_printf(&str, "Type error at (%d:%d): %.*s\n",
                error->s.start.line, error->s.start.col,
                c_SV(sv));
    return str;
}

#endif