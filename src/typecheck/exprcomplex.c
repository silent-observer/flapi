#include "typecheck/exprcomplex.h"

void typecheckAssignExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    assert(node->kind == AST_ASSIGN_EXPR);
    typecheckExpr(ctx, node->assignExpr.lhs, expected);
    typecheckExpr(ctx, node->assignExpr.rhs, expected);
}

TypeId typeinferAssignExpr(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_ASSIGN_EXPR);
    TypeId type = typeinferExpr(ctx, node->assignExpr.rhs);
    typecheckExpr(ctx, node->assignExpr.lhs, type);
    return type;
}

void typecheckIfExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    assert(node->kind == AST_IF_EXPR);
    c_foreach(it, AstChildren, node->ifExpr.clauses) {
        typecheckIfClause(ctx, *it.ref, expected);
    }
}

TypeId typeinferIfExpr(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_IF_EXPR);
    TypeId expected = Type_simple(TYPE_UNKNOWN);
    c_foreach(it, AstChildren, node->ifExpr.clauses) {
        if (Type_isKnown(expected))
            typecheckIfClause(ctx, *it.ref, expected);
        else
            expected = typeinferIfClause(ctx, *it.ref);
    }
    return Type_isKnown(expected) ? expected : Type_simple(TYPE_NONE);
}

void typecheckIfClause(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    assert(node->kind == AST_IF_CLAUSE);
    if (node->ifClause.condition)
        typecheckExpr(ctx, node->ifClause.condition,
                      Type_simple(TYPE_BOOL));
    i32 n = AstChildren_size(&node->ifClause.body);
    c_forrange(i, 0, AstChildren_size(&node->ifClause.body)) {
        AstNode *stmt = *AstChildren_at(&node->ifClause.body, i);
        TypeId result = typecheckStmt(ctx, stmt);
        if (i == n - 1) {
            if (result.id == Type_simple(TYPE_NONE).id) {
                if (expected.id != Type_simple(TYPE_NONE).id) {
                    cstr t = TypeId_print(ctx->types, expected);
                    ERROR(stmt->span,
                          "missing value for an if branch, expected something of type %s",
                          cstr_str(&t));
                    cstr_drop(&t);
                }
            } else {
                if (expected.id != Type_simple(TYPE_NONE).id)
                    typeconvertOrErr(ctx, stmt, expected);
            }
        }
    }
    node->type = expected;
}

TypeId typeinferIfClause(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_IF_CLAUSE);
    if (node->ifClause.condition)
        typecheckExpr(ctx, node->ifClause.condition,
                      Type_simple(TYPE_BOOL));

    i32 n = AstChildren_size(&node->ifClause.body);
    c_forrange(i, 0, AstChildren_size(&node->ifClause.body)) {
        AstNode *stmt = *AstChildren_at(&node->ifClause.body, i);
        TypeId result = typecheckStmt(ctx, stmt);
        if (i == n - 1)
            node->type = result;
    }
    return node->type;
}