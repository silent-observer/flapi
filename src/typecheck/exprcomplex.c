#include "typecheck/exprcomplex.h"

void typecheckAssignExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    assert(node->kind == AST_ASSIGN_EXPR);
    typeinferAssignExpr(ctx, node);
    if (expected.id != Type_simple(TYPE_NONE).id) {
        cstr t = TypeId_print(ctx->types, expected);
        ERROR(node->span,
              "assignments do not return anything, do not use them "
              "inside other expressions (expected type %s)",
              cstr_str(&t));
        cstr_drop(&t);
    }
}

TypeId typeinferAssignExpr(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_ASSIGN_EXPR);
    TypeId type = typeinferExpr(ctx, node->assignExpr.lhs);
    if (!node->assignExpr.lhs->isMutable) {
        ERROR(node->span, "expression is not mutable, cannot assign to it");
    }
    typecheckExpr(ctx, node->assignExpr.rhs, type);
    return Type_simple(TYPE_NONE);
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

void typecheckMakeStructExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    assert(node->kind == AST_MAKE_STRUCT_EXPR);
    node->type = typeinferMakeStructExpr(ctx, node);
    typeconvertOrErr(ctx, node, expected);
}

TypeId typeinferMakeStructExpr(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_MAKE_STRUCT_EXPR);
    TypeId result = node->makeStructExpr.type;
    CustomTypeEntry *resultEntry = CustomTypeTable_find(
        ctx->customTypes,
        ctx->types,
        result);

    if (!resultEntry) {
        cstr t = TypeId_print(ctx->types, result);
        ERROR(node->span,
              "couldn't find a type definition for %s",
              cstr_str(&t));
        cstr_drop(&t);
        return result;
    }

    if (resultEntry->kind != CUSTOM_TYPE_STRUCT) {
        cstr t = TypeId_print(ctx->types, result);
        ERROR(node->span,
              "type %s is not a struct",
              cstr_str(&t));
        cstr_drop(&t);
        return result;
    }

    c_foreach(it, AstChildren, node->makeStructExpr.entries) {
        assert((*it.ref)->kind == AST_MAKE_STRUCT_ENTRY);
        MakeStructEntryNode *entry = &(*it.ref)->makeStructEntry;
        TypeId entryType = CustomTypeTable_getFieldType(
            ctx->customTypes,
            ctx->types,
            result,
            entry->symbol);
        typecheckExpr(ctx, entry->expr, entryType);
        (*it.ref)->type = entryType;
    }
    return result;
}

void typecheckMakeAnyOfExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    assert(node->kind == AST_MAKE_ANYOF_EXPR);
    node->type = typeinferMakeAnyOfExpr(ctx, node);
    typeconvertOrErr(ctx, node, expected);
}

TypeId typeinferMakeAnyOfExpr(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_MAKE_ANYOF_EXPR);
    TypeId result = node->makeAnyOfExpr.type;
    CustomTypeEntry *resultEntry = CustomTypeTable_find(
        ctx->customTypes,
        ctx->types,
        result);

    if (!resultEntry) {
        cstr t = TypeId_print(ctx->types, result);
        ERROR(node->span,
              "couldn't find a type definition for %s",
              cstr_str(&t));
        cstr_drop(&t);
        return result;
    }

    if (resultEntry->kind != CUSTOM_TYPE_ANYOF) {
        cstr t = TypeId_print(ctx->types, result);
        ERROR(node->span,
              "type %s is not an anyof",
              cstr_str(&t));
        cstr_drop(&t);
        return result;
    }

    TypeId entryType = CustomTypeTable_getFieldType(
        ctx->customTypes,
        ctx->types,
        result,
        node->makeAnyOfExpr.variant);

    typecheckExpr(ctx, node->makeAnyOfExpr.expr, entryType);
    return result;
}
