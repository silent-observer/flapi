#include "typecheck/global.h"

void typecollectProgram(TypeInferContext *ctx, AstNode *n) {
    assert(n->kind == AST_PROGRAM);
    c_foreach(it, AstChildren, n->program.children) {
        switch ((*it.ref)->kind) {
            case AST_ERROR:
                break;
            case AST_FN_DEF:
                typecollectFnDef(ctx, *it.ref);
                break;
            case AST_TYPE_DEF:
                typecollectTypeDef(ctx, *it.ref);
                break;
            default:
                assert(0);
        }
    }
}

void typecheckProgram(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_PROGRAM);
    c_foreach(it, AstChildren, node->program.children) {
        switch ((*it.ref)->kind) {
            case AST_ERROR:
                break;
            case AST_FN_DEF:
                typecheckFnDef(ctx, *it.ref);
                break;
            case AST_TYPE_DEF:
                break;
            default:
                assert(0);
        }
    }
    node->type = Type_simple(TYPE_NONE);
}

void typecollectFnDef(TypeInferContext *ctx, AstNode *n) {
    assert(n->kind == AST_FN_DEF);
    Symbol *s = SymbolTable_lookup(ctx->symbols, n->fnDef.symbol);
    Type t = {.kind = TYPE_FN_GLOBAL};
    Type paramT = {.kind = TYPE_TUPLE};
    c_foreach(it, AstChildren, n->fnDef.params) {
        FnParamNode *p = &(*it.ref)->fnParam;
        Symbol *sv = SymbolTable_lookup(ctx->symbols, p->symbol);
        if (p->type.id != Type_simple(TYPE_UNKNOWN).id)
            sv->type = p->type;
        TypeChildren_push(&paramT.children, p->type);
        (*it.ref)->type = p->type;
    }

    TypeChildren_push(&t.children, Type_intern(ctx->types, &paramT));
    TypeChildren_push(&t.children, n->fnDef.returnType);
    s->type = Type_intern(ctx->types, &t);
}

void typecheckFnDef(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_FN_DEF);
    ctx->functionReturnType = node->fnDef.returnType;
    c_foreach(it, AstChildren, node->fnDef.body) {
        typecheckStmt(ctx, *it.ref);
    }
    ctx->functionReturnType = Type_simple(TYPE_NONE);
    node->type = Type_simple(TYPE_NONE);
}

static void typecollectStructDef(TypeInferContext *ctx, AstNode *node, CustomTypeEntry *entry) {
    assert(node->kind == AST_STRUCT_DEF);
    c_foreach(it, VarDefVec, node->structDef.fields) {
        CustomTypeElement elem = {
            .name = it.ref->symbol,
            .type = it.ref->type,
        };
        CustomTypeElementVec_push(&entry->elements, elem);
    }
}
static void typecollectAnyOfDef(TypeInferContext *ctx, AstNode *node, CustomTypeEntry *entry) {
    assert(node->kind == AST_ANYOF_DEF);
    c_foreach(it, VarDefVec, node->anyOfDef.variants) {
        CustomTypeElement elem = {
            .name = it.ref->symbol,
            .type = it.ref->type,
        };
        CustomTypeElementVec_push(&entry->elements, elem);
    }
}
void typecollectTypeDef(TypeInferContext *ctx, AstNode *node) {
    assert(node->kind == AST_TYPE_DEF);
    CustomTypeEntry entry;
    entry.baseType = node->typeDef.baseType;
    c_foreach(it, AstChildren, node->typeDef.typeParams) {
        assert((*it.ref)->kind == AST_TYPE_DEF_PARAM);
        TypeId param = (*it.ref)->typeDefParam.param;
        Type *t = Type_lookup(ctx->types, param);
        assert(t->kind == TYPE_GENERIC_PARAM);
        TypeChildren_push(&entry.typeParams, param);
    }

    switch (node->typeDef.def->kind) {
        case AST_STRUCT_DEF:
            typecollectStructDef(ctx, node->typeDef.def, &entry);
            break;
        case AST_ANYOF_DEF:
            typecollectAnyOfDef(ctx, node->typeDef.def, &entry);
            break;
        default:
            assert(0);
    }

    CustomTypeTable_add(ctx->customTypes, ctx->types, &entry);
}