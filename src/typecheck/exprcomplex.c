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
    c_foreach(it, AstChildren, node->ifClause.body) {
        if (Ast_isExpr((*it.ref)->kind))
            typecheckExpr(ctx, *it.ref, expected);
        else {
            typecheckStmt(ctx, *it.ref);
        }
    }
    node->type = Type_simple(TYPE_NONE);
}

TypeId typeinferIfClause(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_IF_CLAUSE);
    if (node->ifClause.condition)
        typecheckExpr(ctx, node->ifClause.condition,
                      Type_simple(TYPE_BOOL));

    TypeId expected = Type_simple(TYPE_UNKNOWN);
    c_foreach(it, AstChildren, node->ifClause.body) {
        if (Ast_isExpr((*it.ref)->kind))
            expected = typeinferExpr(ctx, *it.ref);
        else {
            typecheckStmt(ctx, *it.ref);
        }
    }
    node->type = Type_simple(TYPE_NONE);
    return Type_isKnown(expected) ? expected : Type_simple(TYPE_NONE);
}

void typecheckWhileExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    assert(node->kind == AST_WHILE_EXPR);
    if (node->whileExpr.condition)
        typecheckExpr(ctx, node->whileExpr.condition,
                      Type_simple(TYPE_BOOL));
    assert(node->whileExpr.elseClause == NULL);
    c_foreach(it, AstChildren, node->ifClause.body) {
        typecheckStmt(ctx, *it.ref);
    }
    if (expected.id != Type_simple(TYPE_NONE).id) {
        // TODO: unsupported
        assert(0);
    }
}
TypeId typeinferWhileExpr(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_WHILE_EXPR);
    if (node->whileExpr.condition)
        typecheckExpr(ctx, node->whileExpr.condition,
                      Type_simple(TYPE_BOOL));
    assert(node->whileExpr.elseClause == NULL);
    c_foreach(it, AstChildren, node->whileExpr.body) {
        typecheckStmt(ctx, *it.ref);
    }
    return Type_simple(TYPE_NONE);
}