#include "typecheck/exprsimple.h"

#define ARITH_SUPPORTED "'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64'"
#define CMP_SUPPORTED ARITH_SUPPORTED ", 'str', 'char', 'bool'"

static TypeId
typeinferArithCmpExpr(TypeInferContext *ctx, AstNode *node) {
    BinaryExprKind kind = node->binaryExpr.kind;
    b32 isArith = kind < BINARY_EQ;
    b32 isCmp = kind >= BINARY_EQ && kind <= BINARY_LE;

    assert(isArith || isCmp);

    TypeId lhs = typeinferExpr(ctx, node->binaryExpr.lhs);
    TypeId rhs = typeinferExpr(ctx, node->binaryExpr.rhs);
    TypeId common = typecommon(ctx, lhs, rhs);

    b32 isInteger = Type_isInteger(common);
    b32 isOther = common.id == Type_simple(TYPE_STR).id ||
                  common.id == Type_simple(TYPE_CHAR).id ||
                  common.id == Type_simple(TYPE_BOOL).id;

    b32 acceptable = (isArith && isInteger) ||
                     (isCmp && (isInteger || isOther));

    if (!acceptable) {
        cstr t1 = TypeId_print(ctx->types, lhs);
        cstr t2 = TypeId_print(ctx->types, rhs);
        ERROR(node->span,
              "expected one of [%s] but got %s '%s' %s",
              node->binaryExpr.kind < BINARY_EQ ? ARITH_SUPPORTED : CMP_SUPPORTED,
              cstr_str(&t1),
              BINARY_EXPR_KIND_STRS[node->binaryExpr.kind].str,
              cstr_str(&t2));
        cstr_drop(&t1);
        cstr_drop(&t2);
        return Type_simple(TYPE_ERROR);
    }

    b32 lhsResult = typeconvertExpr(ctx, node->binaryExpr.lhs, common);
    b32 rhsResult = typeconvertExpr(ctx, node->binaryExpr.rhs, common);
    assert(lhsResult && rhsResult);
    return isArith ? common : Type_simple(TYPE_BOOL);
}

TypeId typeinferBinaryExpr(TypeInferContext *ctx, AstNode *node) {
    BinaryExprKind kind = node->binaryExpr.kind;
    b32 isArith = kind < BINARY_EQ;
    b32 isCmp = kind >= BINARY_EQ && kind <= BINARY_LE;
    b32 isLogic = kind == BINARY_AND || kind == BINARY_OR;
    b32 isPipe = kind == BINARY_PIPE;

    assert(isArith || isCmp || isLogic || isPipe);
    if (isArith || isCmp)
        return typeinferArithCmpExpr(ctx, node);
    else if (isLogic) {
        typecheckExpr(ctx, node->binaryExpr.lhs, Type_simple(TYPE_BOOL));
        typecheckExpr(ctx, node->binaryExpr.rhs, Type_simple(TYPE_BOOL));
        return Type_simple(TYPE_BOOL);
    } else {
        // TODO: not supported yet
        assert(0);
    }
}

void typecheckBinaryExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    TypeId type = typeinferBinaryExpr(ctx, node);
    node->type = type;
    typeconvertOrErr(ctx, node, expected);
}

TypeId typeinferUnaryExpr(TypeInferContext *ctx, AstNode *node) {
    UnaryExprKind kind = node->unaryExpr.kind;
    switch (kind) {
        case UNARY_NOT:
            typecheckExpr(ctx, node->unaryExpr.expr, Type_simple(TYPE_BOOL));
            return Type_simple(TYPE_BOOL);
        case UNARY_NEG: {
            TypeId t = typeinferExpr(ctx, node->unaryExpr.expr);
            if (Type_isInteger(t))
                return t;
            else {
                cstr s = TypeId_print(ctx->types, t);
                ERROR(node->span, "cannot apply negation operator to type %s, expected a signed integer",
                      cstr_str(&s));
                cstr_drop(&s);
                return Type_simple(TYPE_ERROR);
            }
        }
        default:
            assert(0);
    }
}

void typecheckUnaryExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    UnaryExprKind kind = node->unaryExpr.kind;
    typecheckExpr(ctx, node->unaryExpr.expr, expected);
    TypeId t = node->unaryExpr.expr->type;
    if (t.id == Type_simple(TYPE_ERROR).id)
        return;
    switch (kind) {
        case UNARY_NOT:
            if (t.id != Type_simple(TYPE_BOOL).id) {
                cstr s = TypeId_print(ctx->types, t);
                ERROR(node->span, "cannot apply 'not' operator to type %s, expected bool",
                      cstr_str(&s));
                cstr_drop(&s);
            }
            break;
        case UNARY_NEG: {
            if (!Type_isSignedInteger(t)) {
                cstr s = TypeId_print(ctx->types, t);
                ERROR(node->span, "cannot apply negation operator to type %s, expected a signed integer",
                      cstr_str(&s));
                cstr_drop(&s);
                return;
            }
        }
        default:
            assert(0);
    }
}

static TypeId typeinferFuncCall(TypeInferContext *ctx, const SourceSpan *ss, AstChildren *args, TypeId fn) {
    Type *t = Type_lookup(ctx->types, fn);
    if (!TypeKind_isFunc(t->kind)) {
        cstr s = TypeId_print(ctx->types, fn);
        ERROR(*ss, "expected a function, got %s", cstr_str(&s));
        cstr_drop(&s);
        return Type_simple(TYPE_ERROR);
    }
    Type *params = Type_lookup(ctx->types, t->children.data[0]);
    assert(params->kind == TYPE_TUPLE);

    i32 argLen = AstChildren_size(args);
    if (TypeChildren_size(&params->children) != argLen) {
        ERROR(*ss, "expected %d arguments, got %d", TypeChildren_size(&params->children), argLen);
        if (argLen > TypeChildren_size(&params->children))
            argLen = TypeChildren_size(&params->children);
    }

    for (i32 i = 0; i < argLen; i++) {
        typecheckExpr(ctx, *AstChildren_at(args, i), *TypeChildren_at(&params->children, i));
    }
    return t->children.data[1];
}

TypeId typeinferCallExpr(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_CALL_EXPR);
    TypeId fn = typeinferExpr(ctx, node->callExpr.fn);
    return typeinferFuncCall(ctx, &node->span, &node->callExpr.args, fn);
}
TypeId typeinferCallExprConst(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_CALL_EXPR_CONST);
    Symbol *s = SymbolTable_lookup(ctx->symbols, node->callExprConst.symbol);
    return typeinferFuncCall(ctx, &node->span, &node->callExprConst.args, s->type);
}

void typecheckCallExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    assert(node->kind == AST_CALL_EXPR);
    TypeId type = typeinferCallExpr(ctx, node);
    node->type = type;
    typeconvertOrErr(ctx, node, expected);
}
void typecheckCallExprConst(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    assert(node->kind == AST_CALL_EXPR_CONST);
    TypeId type = typeinferCallExprConst(ctx, node);
    node->type = type;
    typeconvertOrErr(ctx, node, expected);
}

void typecheckLambdaExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    assert(0); // TODO: not supported yet
}
TypeId typeinferLambdaExpr(TypeInferContext *ctx, AstNode *node) {
    assert(0); // TODO: not supported yet
}
void typecheckIndexExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    assert(0); // TODO: not supported yet
}
TypeId typeinferIndexExpr(TypeInferContext *ctx, AstNode *node) {
    assert(0); // TODO: not supported yet
}
void typecheckDotExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    assert(node->kind == AST_DOT_EXPR);
    node->type = typeinferDotExpr(ctx, node);
    typeconvertOrErr(ctx, node, expected);
}
TypeId typeinferDotExpr(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_DOT_EXPR);
    TypeId t = typeinferExpr(ctx, node->dotExpr.expr);
    CustomTypeEntry *entry = CustomTypeTable_find(
        ctx->customTypes,
        ctx->types,
        t);

    if (!entry) {
        cstr s = TypeId_print(ctx->types, t);
        ERROR(node->span,
              "couldn't find a type definition for %s",
              cstr_str(&s));
        cstr_drop(&s);
        return Type_simple(TYPE_ERROR);
    }

    if (entry->kind != CUSTOM_TYPE_STRUCT) {
        cstr s = TypeId_print(ctx->types, t);
        ERROR(node->span,
              "type %s is not a struct",
              cstr_str(&s));
        cstr_drop(&s);
        return Type_simple(TYPE_ERROR);
    }

    TypeId fieldType = CustomTypeTable_getFieldType(
        ctx->customTypes,
        ctx->types,
        t,
        node->dotExpr.field);
    if (fieldType.id == Type_simple(TYPE_ERROR).id) {
        cstr s = TypeId_print(ctx->types, t);
        Symbol *sym = SymbolTable_lookup(ctx->symbols, node->dotExpr.field);
        ERROR(node->span,
              "couldn't find field %.*s in type %s",
              c_SV(sym->name),
              cstr_str(&s));
        cstr_drop(&s);
    }
    node->isMutable = node->dotExpr.expr->isMutable;
    return fieldType;
}

TypeId typeinferVarExpr(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_VAR_EXPR);
    Symbol *s = SymbolTable_lookup(ctx->symbols, node->varExpr.var);
    node->isMutable = s->isMutable;
    return s->type;
}

void typecheckVarExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    assert(node->kind == AST_VAR_EXPR);
    TypeId type = typeinferVarExpr(ctx, node);
    node->type = type;
    typeconvertOrErr(ctx, node, expected);
}

TypeId typeinferLiteralExpr(TypeInferContext *ctx, AstNode *node) {
    switch (node->kind) {
        case AST_INT_LITERAL_EXPR:
            return Type_simple(TYPE_INTLIT);
        case AST_STR_LITERAL_EXPR:
            return Type_simple(TYPE_STR);
        case AST_CHAR_LITERAL_EXPR:
            return Type_simple(TYPE_CHAR);
        case AST_BOOL_LITERAL_EXPR:
            return Type_simple(TYPE_BOOL);
        default:
            assert(0);
    }
}

void typecheckLiteralExpr(TypeInferContext *ctx, AstNode *node, TypeId expected) {
    TypeId type = typeinferLiteralExpr(ctx, node);
    node->type = type;
    typeconvertOrErr(ctx, node, expected);
}
