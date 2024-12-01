#ifndef ASTTRANS_H
#define ASTTRANS_H

#include "ast.h"
#include "cst.h"

typedef struct {
    czview msg;
    SourceSpan span;
} AstTransformError;

#define i_TYPE AstTransformErrorVec, AstTransformError
#include <stc/vec.h>

typedef struct {
    Ast ast;
    AstTransformErrorVec errors;
} AstTransformResult;

AstTransformResult astFromCst(Cst *cst);

#endif