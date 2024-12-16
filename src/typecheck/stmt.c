#include "typecheck/stmt.h"

void typecheckExprStmt(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_EXPR_STMT);
    typeinferExpr(ctx, node->exprStmt.expr);
}
void typecheckLetStmt(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_LET_STMT);
    if (Type_isKnown(node->letStmt.varDef.type))
        typecheckExpr(ctx, node->letStmt.initExpr, node->letStmt.varDef.type);
    else
        node->letStmt.varDef.type = typeinferExpr(ctx, node->letStmt.initExpr);

    if (node->letStmt.varDef.type.id == Type_simple(TYPE_INTLIT).id) {
        b32 r = typeconvertExpr(ctx, node->letStmt.initExpr, Type_simple(TYPE_I64));
        assert(r);
        node->letStmt.varDef.type = Type_simple(TYPE_I64);
    }

    Symbol *s = SymbolTable_lookup(ctx->symbols, node->letStmt.varDef.symbol);
    s->type = node->letStmt.varDef.type;
}
void typecheckWithStmt(TypeInferContext *ctx, AstNode *node) {
    // TODO: unsupported
    assert(0);
}
void typecheckReturnStmt(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_RETURN_STMT);
    if (ctx->functionReturnType.id != Type_simple(TYPE_TUPLE).id) {
        if (node->returnStmt.expr)
            typecheckExpr(ctx, node->returnStmt.expr, ctx->functionReturnType);
        else
            ERROR(node->span, "missing return value");
    } else {
        if (node->returnStmt.expr) {
            ERROR(node->span, "this function does not return a value");
            typeinferExpr(ctx, node->returnStmt.expr);
        }
    }
}
void typecheckBreakStmt(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_BREAK_STMT);
    if (node->breakStmt.expr) {
        // TODO: unsupported
        assert(0);
    }
}
void typecheckContinueStmt(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_CONTINUE_STMT);
}