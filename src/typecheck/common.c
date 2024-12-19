#include "typecheck/common.h"
#include "typecheck/exprcomplex.h"
#include "typecheck/exprsimple.h"
#include "typecheck/stmt.h"

b32 typeconvertExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    if (node->type.id == expected.id)
        return true;

    Type *from = Type_lookup(ctx->types, node->type);
    Type *to = Type_lookup(ctx->types, expected);

    b32 from_signed = from->kind >= TYPE_I8 && from->kind <= TYPE_I64;
    b32 to_signed = to->kind >= TYPE_I8 && to->kind <= TYPE_I64;
    b32 from_unsigned = from->kind >= TYPE_I8 && from->kind <= TYPE_I64;
    b32 to_unsigned = to->kind >= TYPE_I8 && to->kind <= TYPE_I64;
    b32 from_lit = from->kind == TYPE_INTLIT;

    b32 canConvert = false;
    if ((from_signed && to_signed) || (from_unsigned && to_unsigned)) {
        if (to->kind > from->kind)
            canConvert = true;
    } else if (from_lit && (to_signed || to_unsigned))
        canConvert = true;

    if (canConvert) {
        AstNode *new = AstPool_new(&ctx->ast->pool);
        *new = *node;
        node->kind = AST_TYPECONVERT_EXPR;
        node->type = expected;
        node->typeConvertExpr.expr = new;
        return true;
    } else {
        return false;
    }
}

#define max(a, b) ((a) > (b) ? (a) : (b))

TypeId typecommon(TypeInferContext *ctx, TypeId a, TypeId b) {
    if (a.id == b.id)
        return a;

    Type *ta = Type_lookup(ctx->types, a);
    Type *tb = Type_lookup(ctx->types, b);

    b32 ta_signed = ta->kind >= TYPE_I8 && ta->kind <= TYPE_I64;
    b32 tb_signed = tb->kind >= TYPE_I8 && tb->kind <= TYPE_I64;
    b32 ta_unsigned = ta->kind >= TYPE_I8 && ta->kind <= TYPE_I64;
    b32 tb_unsigned = tb->kind >= TYPE_I8 && tb->kind <= TYPE_I64;
    b32 ta_lit = ta->kind == TYPE_INTLIT;
    b32 tb_lit = tb->kind == TYPE_INTLIT;

    if ((ta_signed && tb_signed) || (ta_unsigned && tb_unsigned))
        return Type_simple(max(ta->kind, tb->kind));

    if (ta_lit && (tb_signed || tb_unsigned))
        return b;
    if ((ta_signed || ta_unsigned) && tb_lit)
        return a;

    return Type_simple(TYPE_ERROR);
}

void typeconvertOrErr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    if (!typeconvertExpr(ctx, node, expected)) {
        cstr t1 = TypeId_print(ctx->types, expected);
        cstr t2 = TypeId_print(ctx->types, node->type);
        ERROR(node->span, "expected type %s, got %s",
              cstr_str(&t1), cstr_str(&t2));
        cstr_drop(&t1);
        cstr_drop(&t2);
    }
}

TypeId typecheckStmt(TypeInferContext *ctx, AstNode *node) {
    node->type = Type_simple(TYPE_NONE);
    switch (node->kind) {
        case AST_ERROR:
            break;
        case AST_EXPR_STMT:
            node->type = typecheckExprStmt(ctx, node);
            break;
        case AST_LET_STMT:
            typecheckLetStmt(ctx, node);
            break;
        case AST_WITH_STMT:
            typecheckWithStmt(ctx, node);
            break;
        case AST_RETURN_STMT:
            typecheckReturnStmt(ctx, node);
            break;
        case AST_BREAK_STMT:
            typecheckBreakStmt(ctx, node);
            break;
        case AST_CONTINUE_STMT:
            typecheckContinueStmt(ctx, node);
            break;
        case AST_WHILE_STMT:
            typecheckWhileStmt(ctx, node);
            break;
        default:
            assert(0);
    }
    return node->type;
}

void typecheckExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    switch (node->kind) {
        case AST_ERROR:
            break;
        case AST_IF_EXPR:
            typecheckIfExpr(ctx, node, expected);
            break;
        case AST_ASSIGN_EXPR:
            typecheckAssignExpr(ctx, node, expected);
            break;
        case AST_BINARY_EXPR:
            typecheckBinaryExpr(ctx, node, expected);
            break;
        case AST_UNARY_EXPR:
            typecheckUnaryExpr(ctx, node, expected);
            break;
        case AST_CALL_EXPR:
            typecheckCallExpr(ctx, node, expected);
            break;
        case AST_CALL_EXPR_CONST:
            typecheckCallExprConst(ctx, node, expected);
            break;
        case AST_LAMBDA_EXPR:
            typecheckLambdaExpr(ctx, node, expected);
            break;
        case AST_INDEX_EXPR:
            typecheckIndexExpr(ctx, node, expected);
            break;
        case AST_DOT_EXPR:
            typecheckDotExpr(ctx, node, expected);
            break;
        case AST_VAR_EXPR:
            typecheckVarExpr(ctx, node, expected);
            break;
        case AST_INT_LITERAL_EXPR:
        case AST_STR_LITERAL_EXPR:
        case AST_CHAR_LITERAL_EXPR:
        case AST_BOOL_LITERAL_EXPR:
            typecheckLiteralExpr(ctx, node, expected);
            break;
        default:
            assert(0);
    }
    node->type = expected;
}
TypeId typeinferExpr(TypeInferContext *ctx, AstNode *node) {
    switch (node->kind) {
        case AST_ERROR:
            node->type = Type_simple(TYPE_ERROR);
            break;
        case AST_IF_EXPR:
            node->type = typeinferIfExpr(ctx, node);
            break;
        case AST_MAKE_STRUCT_EXPR:
            node->type = typeinferMakeStructExpr(ctx, node);
            break;
        case AST_MAKE_ANYOF_EXPR:
            node->type = typeinferMakeAnyOfExpr(ctx, node);
            break;
        case AST_ASSIGN_EXPR:
            node->type = typeinferAssignExpr(ctx, node);
            break;
        case AST_BINARY_EXPR:
            node->type = typeinferBinaryExpr(ctx, node);
            break;
        case AST_UNARY_EXPR:
            node->type = typeinferUnaryExpr(ctx, node);
            break;
        case AST_CALL_EXPR:
            node->type = typeinferCallExpr(ctx, node);
            break;
        case AST_CALL_EXPR_CONST:
            node->type = typeinferCallExprConst(ctx, node);
            break;
        case AST_LAMBDA_EXPR:
            node->type = typeinferLambdaExpr(ctx, node);
            break;
        case AST_INDEX_EXPR:
            node->type = typeinferIndexExpr(ctx, node);
            break;
        case AST_DOT_EXPR:
            node->type = typeinferDotExpr(ctx, node);
            break;
        case AST_VAR_EXPR:
            node->type = typeinferVarExpr(ctx, node);
            break;
        case AST_INT_LITERAL_EXPR:
        case AST_STR_LITERAL_EXPR:
        case AST_CHAR_LITERAL_EXPR:
        case AST_BOOL_LITERAL_EXPR:
            node->type = typeinferLiteralExpr(ctx, node);
            break;
        default:
            assert(0);
    }
    return node->type;
}