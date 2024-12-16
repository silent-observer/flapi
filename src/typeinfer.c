#include "typeinfer.h"
#include "typecheck/common.h"
#include "typecheck/global.h"

TypingErrorVec typeinfer(Ast *ast) {
    TypeInferContext ctx = {
        .ast = ast,
        .symbols = &ast->symbols,
        .types = &ast->types,
        .functionReturnType = Type_simple(TYPE_NONE),
        .errors = {0},
        .breakStack = {0},
    };

    typecollectProgram(&ctx, ast->root);
    typecheckProgram(&ctx, ast->root);
    return ctx.errors;
}