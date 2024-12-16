#ifndef TYPECHECK_COMMON_H
#define TYPECHECK_COMMON_H

#include "typeinfer.h"

#define i_TYPE TypeVec, TypeId
#include <stc/vec.h>

typedef struct {
    Ast *ast;
    SymbolTable *symbols;
    TypeMap *types;
    TypeId functionReturnType;
    TypingErrorVec errors;
    TypeVec breakStack;
} TypeInferContext;

TypeId typecheckStmt(TypeInferContext *ctx, AstNode *node);
void typecheckExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);
TypeId typeinferExpr(TypeInferContext *ctx, AstNode *node);
b32 typeconvertExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);
TypeId typecommon(TypeInferContext *ctx, TypeId a, TypeId b);
void typeconvertOrErr(TypeInferContext *ctx, AstNode *node, TypeId expected);

#define ERROR(span, ...)                  \
    do {                                  \
        cstr __msg = cstr_init();         \
        cstr_printf(&__msg, __VA_ARGS__); \
        TypingErrorVec_push(              \
            &ctx->errors,                 \
            (TypingError){                \
                .s = (span),              \
                .msg = __msg,             \
            });                           \
    } while (0)

#endif