#include "asttrans.h"
#include "error.h"
#include <stc/czview.h>

#define i_TYPE ScopeStack, ScopeId
#include <stc/stack.h>

typedef struct {
    AstPool pool;
    SymbolTable symbols;
    TypeMap types;
    AstTransformErrorVec errors;
    ScopeStack scopes;
    CustomTypeTable customTypes;
} AstTransformer;

static SourceSpan getSpan(CstChild *c) {
    switch (c->kind) {
        case CST_CHILD_TOKEN:
            return makeSourceSpan(c->token->src.line,
                                  c->token->src.col,
                                  c->token->text.size);
        case CST_CHILD_NODE:
            return c->node->span;
        case CST_CHILD_NONE:
            return makeSourceSpan(c->point.line, c->point.col, 0);
        default:
            assert(0);
    }
}

#define err_n(m, n)            \
    AstTransformErrorVec_push( \
        &astTrans->errors,     \
        (AstTransformError){   \
            .msg = c_zv(m),    \
            .span = (n)->span, \
        })

#define err(m, c)               \
    AstTransformErrorVec_push(  \
        &astTrans->errors,      \
        (AstTransformError){    \
            .msg = c_zv(m),     \
            .span = getSpan(c), \
        })
static AstNode *newErr(AstTransformer *astTrans, CstChild *c) {
    AstNode *n = AstPool_new(&astTrans->pool);
    n->kind = AST_ERROR;
    n->span = getSpan(c);
    return n;
}

static AstNode *newErrNode(AstTransformer *astTrans, CstNode *c) {
    AstNode *n = AstPool_new(&astTrans->pool);
    n->kind = AST_ERROR;
    n->span = c->span;
    return n;
}

#define make_err(m, c) (err(m, c), newErr(astTrans, c))

#define is_token(c, k) ((c)->kind == CST_CHILD_TOKEN && (c)->token->kind == (k))
#define is_node(c, k) ((c)->kind == CST_CHILD_NODE && (c)->node->kind == (k))
#define check(c, msg)                  \
    if ((c)->kind == CST_CHILD_NONE) { \
        err(msg, c);                   \
    }
#define at(i) (CstChildren_at(&cst->children, (i)))
#define none_at(i) (at(i)->kind == CST_CHILD_NONE)
#define node_at(i) (at(i)->kind == CST_CHILD_NODE ? at(i)->node : NULL)
#define token_at(i) (at(i)->kind == CST_CHILD_TOKEN ? at(i)->token : NULL)

static SymbolId makeSymbol(AstTransformer *astTrans, Token *t) {
    assert(t);
    assert(t->kind == TOKEN_IDENT);
    assert(ScopeStack_size(&astTrans->scopes) > 1);

    ScopeId scope = *ScopeStack_top(&astTrans->scopes);
    SymbolId id = SymbolTable_find(&astTrans->symbols, scope, t);
    if (id != NO_SYMBOL_ID) {
        Symbol *s = SymbolTable_lookup(&astTrans->symbols, id);
        if (s->scope == scope) {
            AstTransformErrorVec_push(
                &astTrans->errors,
                (AstTransformError){
                    .msg = c_zv("symbol already defined"),
                    .span = makeSourceSpan(t->src.line, t->src.col, t->text.size),
                    .firstDefinition = s->span,
                });
        }

        return id;
    }
    return SymbolTable_add(&astTrans->symbols, scope, t);
}

static SymbolId findSymbol(AstTransformer *astTrans, Token *t) {
    assert(t);
    assert(t->kind == TOKEN_IDENT);
    assert(ScopeStack_size(&astTrans->scopes) > 1);

    ScopeId scope = *ScopeStack_top(&astTrans->scopes);
    SymbolId id = SymbolTable_find(&astTrans->symbols, scope, t);
    if (id == NO_SYMBOL_ID) {
        AstTransformErrorVec_push(
            &astTrans->errors,
            (AstTransformError){
                .msg = c_zv("symbol already defined"),
                .span = makeSourceSpan(t->src.line, t->src.col, t->text.size),
            });
        return SymbolTable_add(&astTrans->symbols, scope, t);
    }
    return id;
}

static SymbolId makeField(AstTransformer *astTrans, Token *t) {
    assert(t);
    assert(t->kind == TOKEN_IDENT);

    const ScopeId scope = 0;
    return SymbolTable_add(&astTrans->symbols, scope, t);
}

static void pushScope(AstTransformer *astTrans) {
    assert(!ScopeStack_empty(&astTrans->scopes));
    ScopeId parent = *ScopeStack_top(&astTrans->scopes);
    ScopeId new = SymbolTable_scope(&astTrans->symbols, parent);
    ScopeStack_push(&astTrans->scopes, new);
}

static void popScope(AstTransformer *astTrans) {
    assert(!ScopeStack_empty(&astTrans->scopes));
    ScopeStack_pop(&astTrans->scopes);
}

static TypeId transformTypeExpr(AstTransformer *astTrans, CstNode *cst);

// BaseTypeExpr(1) = // everything is [0]
//    'i8'
//  | 'i16'
//  | 'i32'
//  | 'i64'
//  | 'u8'
//  | 'u16'
//  | 'u32'
//  | 'u64'
//  | 'str'
//  | 'char'
//  | 'bool'
static TypeId transformBaseTypeExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_BASE_TYPE_EXPR);
    assert(CstChildren_size(&cst->children) == 1);
    assert(token_at(0));

    TypeKind tk;
#define token_case(x)  \
    case TOKEN_K_##x:  \
        tk = TYPE_##x; \
        break
    switch (token_at(0)->kind) {
        token_case(I8);
        token_case(I16);
        token_case(I32);
        token_case(I64);
        token_case(U8);
        token_case(U16);
        token_case(U32);
        token_case(U64);
        token_case(STR);
        token_case(CHAR);
        token_case(BOOL);
        default:
            assert(0);
    }
#undef token_case
    return Type_simple(tk);
}

#define expect_type_expr(cond) \
    if (!(cond))               \
    return Type_simple(TYPE_ERROR)

// GenericParamName(2) = '%'[0!] 'IDENT'[1]
static TypeId transformGenericParamName(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_GENERIC_PARAM_NAME);
    assert(CstChildren_size(&cst->children) == 2);

    assert(is_token(at(0), TOKEN_PERCENT));
    expect_type_expr(is_token(at(1), TOKEN_IDENT));

    return Type_named(&astTrans->types, TYPE_GENERIC_PARAM, token_at(1)->text);
}

// TypeArg(2) = TypeExpr[0!E] ','?[1]
static TypeId transformTypeArg(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_TYPE_ARG);
    assert(CstChildren_size(&cst->children) == 2);
    assert(node_at(0));
    return transformTypeExpr(astTrans, node_at(0));
}

// GenericArgList(*) = '['[0!] TypeArg+[*!E] ']'[?]
static void transformGenericArgList(AstTransformer *astTrans, CstNode *cst, TypeChildren *args) {
    assert(cst);
    assert(cst->kind == CST_GENERIC_ARG_LIST);
    c_foreach(it, CstChildren, cst->children) {
        if (it.ref->kind != CST_CHILD_NODE || is_node(it.ref, CST_ERROR))
            continue;
        assert(is_node(it.ref, CST_TYPE_ARG));
        TypeId arg = transformTypeArg(astTrans, it.ref->node);
        TypeChildren_push(args, arg);
    }
}

// CustomTypeExpr(2) = 'IDENT'[0!] GenericArgList?[1]
static TypeId transformCustomTypeExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_CUSTOM_TYPE_EXPR);
    assert(CstChildren_size(&cst->children) == 2);
    assert(is_token(at(0), TOKEN_IDENT));

    TypeId named = Type_named(&astTrans->types, TYPE_NAMED, token_at(0)->text);
    if (is_node(at(1), CST_GENERIC_ARG_LIST)) {
        Type generic = {.kind = TYPE_GENERIC};
        TypeChildren_push(&generic.children, named);
        transformGenericArgList(astTrans, node_at(1), &generic.children);
        return Type_intern(&astTrans->types, &generic);
    } else {
        return named;
    }
}

// TupleTypeExpr(*) = '('[0!] TypeArg*[*!E] ')'[?]
static TypeId transformTupleTypeExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_TUPLE_TYPE_EXPR);

    Type t = {.kind = TYPE_TUPLE};

    c_foreach(it, CstChildren, cst->children) {
        if (it.ref->kind != CST_CHILD_NODE || is_node(it.ref, CST_ERROR))
            continue;

        assert(is_node(it.ref, CST_TYPE_ARG));
        TypeId arg = transformTypeArg(astTrans, it.ref->node);
        TypeChildren_push(&t.children, arg);
    }
    return Type_intern(&astTrans->types, &t);
}

// FunctionSpec(5) = 'fn'[0!] ('['[1] '1'[2] ('+'[3] | '?'[3])? ']'[4])?
static TypeKind transformFunctionTypeSpec(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_FUNCTION_SPEC);
    assert(CstChildren_size(&cst->children) == 5);
    assert(is_token(at(0), TOKEN_K_FN_TYPE));

    if (is_token(at(1), TOKEN_LBRACK)) {
        if (!is_token(at(2), TOKEN_DECIMAL) ||
            !csview_equals(token_at(2)->text, "1")) {
            err("only function types fn, fn[1], fn[1+] and fn[1?] are allowed", at(0));
            return TYPE_ERROR;
        }

        if (is_token(at(3), TOKEN_QUESTION)) {
            return TYPE_FN_ONCE_OR_ZERO;
        } else if (is_token(at(3), TOKEN_PLUS)) {
            return TYPE_FN_ONCE_OR_MORE;
        } else
            return TYPE_FN_ONCE;
    } else {
        return TYPE_FN_ANY;
    }
}

// FunctionTypeExpr(5) = FunctionSpec[0!] TupleTypeExpr[1] ('->'[2] TypeExpr[3E])? FnModifierList?[4]
static TypeId transformFunctionTypeExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_FUNCTION_TYPE_EXPR);
    assert(CstChildren_size(&cst->children) == 5);

    assert(is_node(at(0), CST_FUNCTION_SPEC));
    TypeKind kind = transformFunctionTypeSpec(astTrans, node_at(0));

    Type t = {.kind = kind};

    TypeId params = Type_simple(TYPE_ERROR);
    if (is_node(at(1), CST_TUPLE_TYPE_EXPR)) {
        params = transformTupleTypeExpr(astTrans, node_at(1));
    } else {
        err("expected function parameter type list", at(1));
    }
    TypeChildren_push(&t.children, params);

    TypeId returnType = Type_simple(TYPE_TUPLE);
    if (is_token(at(2), TOKEN_ARROW)) {
        returnType = transformTypeExpr(astTrans, node_at(3));
    }
    TypeChildren_push(&t.children, returnType);

    //  TODO
    // if (is_node(at(4), CST_FN_MODIFIER_LIST)) {
    //     VarDefVec given = {0};
    //     // transformFnModifierList(astTrans, node_at(4), &t.children);
    //
    // }

    return Type_intern(&astTrans->types, &t);
}

// TypeExpr :=
//    BaseTypeExpr
//  | GenericParamName
//  | CustomTypeExpr
//  | FunctionTypeExpr
//  | TupleTypeExpr
static TypeId transformTypeExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    switch (cst->kind) {
        case CST_ERROR:
            return Type_simple(TYPE_ERROR);
        case CST_BASE_TYPE_EXPR:
            return transformBaseTypeExpr(astTrans, cst);
        case CST_GENERIC_PARAM_NAME:
            return transformGenericParamName(astTrans, cst);
        case CST_CUSTOM_TYPE_EXPR:
            return transformCustomTypeExpr(astTrans, cst);
        case CST_TUPLE_TYPE_EXPR:
            return transformTupleTypeExpr(astTrans, cst);
        case CST_FUNCTION_TYPE_EXPR:
            return transformFunctionTypeExpr(astTrans, cst);
        default:
            err_n("expected type", cst);
            return Type_simple(TYPE_ERROR);
    }
}

#define DEF_NODE(n, k)                         \
    AstNode *n = AstPool_new(&astTrans->pool); \
    n->kind = (k);                             \
    n->span = cst->span

// FnParam(5) = 'IDENT'[0!] ':'[1] 'var'?[2] TypeExpr[3!E] ','?[4]
// TODO: 'var' is ignored
static AstNode *transformFnParam(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_FN_PARAM);
    assert(CstChildren_size(&cst->children) == 5);

    DEF_NODE(n, AST_FN_PARAM);

    assert(is_token(at(0), TOKEN_IDENT));
    n->fnParam.symbol = makeSymbol(astTrans, token_at(0));

    if (is_token(at(1), TOKEN_COLON)) {
        assert(at(3)->kind == CST_CHILD_NODE);
        n->fnParam.type = transformTypeExpr(astTrans, node_at(3));
    } else {
        err("function parameters must be declared with explicit types", at(0));
        n->fnParam.type = Type_simple(TYPE_UNKNOWN);
    }

    n->fnParam.isMutable = is_token(at(2), TOKEN_K_VAR);

    return n;
}

// FnParamList(*) = '('[0!] FnParam*[*!E] ')'[?]
static void transformFnParamList(AstTransformer *astTrans, CstNode *cst, AstChildren *params) {
    assert(cst);
    assert(cst->kind == CST_FN_PARAM_LIST);
    c_foreach(it, CstChildren, cst->children) {
        if (it.ref->kind != CST_CHILD_NODE || is_node(it.ref, CST_ERROR))
            continue;
        assert(is_node(it.ref, CST_FN_PARAM));
        AstNode *param = transformFnParam(astTrans, it.ref->node);
        AstChildren_push(params, param);
    }
}

// ImplicitClause(*) = ('IDENT'[0] ':'[1])? TypeExpr[2!E] ','?[3]
static VarDef transformImplicitClause(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_IMPLICIT_CLAUSE);
    assert(CstChildren_size(&cst->children) == 4);

    VarDef v;
    v.span = cst->span;

    if (is_token(at(0), TOKEN_IDENT))
        v.symbol = makeSymbol(astTrans, token_at(0));
    else
        v.symbol = NO_SYMBOL_ID;

    assert(node_at(2));
    v.type = transformTypeExpr(astTrans, node_at(2));

    return v;
}

// GivenModifier(*) = 'given'[0!] '('[1] ImplicitClause+[*!E] ')'[?]
static void transformGivenModifier(AstTransformer *astTrans, CstNode *cst, VarDefVec *given) {
    assert(cst);
    assert(cst->kind == CST_GIVEN_MODIFIER);
    c_foreach(it, CstChildren, cst->children) {
        if (it.ref->kind != CST_CHILD_NODE || is_node(it.ref, CST_ERROR))
            continue;

        assert(is_node(it.ref, CST_IMPLICIT_CLAUSE));
        VarDef c = transformImplicitClause(astTrans, it.ref->node);
        VarDefVec_push(given, c);
    }
}

// FnModifierList(*) = FnModifier+[*!]
static void transformFnModifierList(AstTransformer *astTrans, CstNode *cst, VarDefVec *given) {
    assert(cst);
    assert(cst->kind == CST_FN_MODIFIER_LIST);
    c_foreach(it, CstChildren, cst->children) {
        assert(is_node(it.ref, CST_GIVEN_MODIFIER));
        transformGivenModifier(astTrans, it.ref->node, given);
    }
}

static AstNode *transformExpr(AstTransformer *astTrans, CstNode *cst);

// AssignExpr(3) = SimpleExpr[0!E] AssignOp[1!] Expr[2!E]
// AssignOp := '=' | '+=' | '-=' | '*=' | '/=';
static AstNode *transformAssignExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_ASSIGN_EXPR);
    assert(CstChildren_size(&cst->children) == 3);

    DEF_NODE(n, AST_ASSIGN_EXPR);

    assert(node_at(0));
    n->assignExpr.lhs = transformExpr(astTrans, node_at(0));

    assert(token_at(1));
    switch (token_at(1)->kind) {
        case TOKEN_EQUAL:
            n->assignExpr.kind = ASSIGN_REGULAR;
            break;
        case TOKEN_PLUS_EQUAL:
            n->assignExpr.kind = ASSIGN_ADD;
            break;
        case TOKEN_MINUS_EQUAL:
            n->assignExpr.kind = ASSIGN_SUB;
            break;
        case TOKEN_STAR_EQUAL:
            n->assignExpr.kind = ASSIGN_MUL;
            break;
        case TOKEN_SLASH_EQUAL:
            n->assignExpr.kind = ASSIGN_DIV;
            break;
        default:
            err("expected assignment operator", at(1));
            break;
    }

    assert(node_at(2));
    n->assignExpr.rhs = transformExpr(astTrans, node_at(2));
    return n;
}

static void transformBlock(AstTransformer *astTrans, CstNode *cst, AstChildren *body);

// IfClause(3) = 'if'[0!] SimpleExpr[1E] Block[2]
static AstNode *transformIfClause(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_IF_CLAUSE);
    assert(CstChildren_size(&cst->children) == 3);

    DEF_NODE(n, AST_IF_CLAUSE);

    assert(is_token(at(0), TOKEN_K_IF));
    n->ifClause.condition = node_at(1)
                                ? transformExpr(astTrans, node_at(1))
                                : make_err("expected a condition", at(1));
    if (node_at(2)) {
        assert(is_node(at(2), CST_BLOCK));
        pushScope(astTrans);
        transformBlock(astTrans, node_at(2), &n->ifClause.body);
        popScope(astTrans);
    } else
        err("expected a body for if statement", at(2));

    return n;
}

// ElseIfClause(4) = 'else'[0!] 'if'[1] SimpleExpr[2E] Block[3]
static AstNode *transformElseIfClause(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_ELSE_IF_CLAUSE);
    assert(CstChildren_size(&cst->children) == 4);

    DEF_NODE(n, AST_IF_CLAUSE);

    assert(is_token(at(0), TOKEN_K_ELSE));
    assert(is_token(at(1), TOKEN_K_IF));
    n->ifClause.condition = node_at(2)
                                ? transformExpr(astTrans, node_at(2))
                                : make_err("expected a condition", at(2));
    if (node_at(3)) {
        assert(is_node(at(3), CST_BLOCK));
        pushScope(astTrans);
        transformBlock(astTrans, node_at(3), &n->ifClause.body);
        popScope(astTrans);
    } else
        err("expected a body for else if clause", at(3));

    return n;
}

// ElseClause(2) = 'else'[0!] Block[1]
static AstNode *transformElseClause(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_ELSE_CLAUSE);
    assert(CstChildren_size(&cst->children) == 2);

    DEF_NODE(n, AST_IF_CLAUSE);

    assert(is_token(at(0), TOKEN_K_ELSE));
    n->ifClause.condition = NULL;
    if (node_at(1)) {
        assert(is_node(at(1), CST_BLOCK));
        pushScope(astTrans);
        transformBlock(astTrans, node_at(1), &n->ifClause.body);
        popScope(astTrans);
    } else
        err("expected a body for else clause", at(1));

    return n;
}

// IfExpr(*) = IfClause[0!] ElseIfClause*[*!] ElseClause?[?!]
static AstNode *transformIfExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_IF_EXPR);

    DEF_NODE(n, AST_IF_EXPR);

    c_foreach(it, CstChildren, cst->children) {
        assert(is_node(it.ref, CST_IF_CLAUSE) ||
               is_node(it.ref, CST_ELSE_CLAUSE) ||
               is_node(it.ref, CST_ELSE_IF_CLAUSE));
        AstNode *clause = NULL;
        switch (it.ref->node->kind) {
            case CST_IF_CLAUSE:
                clause = transformIfClause(astTrans, it.ref->node);
                break;
            case CST_ELSE_IF_CLAUSE:
                clause = transformElseIfClause(astTrans, it.ref->node);
                break;
            case CST_ELSE_CLAUSE:
                clause = transformElseClause(astTrans, it.ref->node);
                break;
            default:
                assert(0);
        }
        AstChildren_push(&n->ifExpr.clauses, clause);
    }
    return n;
}

// StructInitEntry(4) = 'IDENT'[0!] '='[1] Expr[2!E] ','?[3]
static AstNode *transformStructInitEntry(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_STRUCT_INIT_ENTRY);
    assert(CstChildren_size(&cst->children) == 4);

    DEF_NODE(n, AST_MAKE_STRUCT_ENTRY);

    assert(is_token(at(0), TOKEN_IDENT));
    n->makeStructEntry.symbol = makeField(astTrans, token_at(0));

    if (is_token(at(1), TOKEN_EQUAL)) {
        assert(at(2)->kind == CST_CHILD_NODE);
        n->makeStructEntry.expr = transformExpr(astTrans, at(2)->node);
    } else {
        err("struct initializer must have explicitly specified expressions", at(0));
        n->makeStructEntry.expr = newErr(astTrans, at(0));
    }

    return n;
}

// StructInitExpr(*) = '{'[0!] StructInitEntry*[*!E] '}'[?]
static void transformStructInitExpr(AstTransformer *astTrans, CstNode *cst, AstChildren *list) {
    assert(cst);
    assert(cst->kind == CST_STRUCT_INIT_EXPR);

    c_foreach(it, CstChildren, cst->children) {
        if (it.ref->kind != CST_CHILD_NODE || is_node(it.ref, CST_ERROR))
            continue;
        assert(is_node(it.ref, CST_STRUCT_INIT_ENTRY));
        AstNode *entry = transformStructInitEntry(astTrans, it.ref->node);
        AstChildren_push(list, entry);
    }
}

// AnyOfInitExpr(2) = 'IDENT'[0!] Expr?[1E]
static AstNode *transformAnyOfInitExpr(AstTransformer *astTrans, CstNode *cst, SymbolId *variant) {
    assert(cst);
    assert(cst->kind == CST_ANYOF_INIT_EXPR);
    assert(CstChildren_size(&cst->children) == 2);

    assert(is_token(at(0), TOKEN_IDENT));
    *variant = makeField(astTrans, token_at(0));

    if (node_at(1)) {
        return transformExpr(astTrans, node_at(1));
    } else {
        return NULL;
    }
}

// MakeExpr(3) = 'make'[0!] CustomTypeExpr[1] (StructInitExpr | AnyOfInitExpr)[2]
static AstNode *transformMakeExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_MAKE_EXPR);
    assert(CstChildren_size(&cst->children) == 3);

    DEF_NODE(n, AST_MAKE_STRUCT_EXPR);

    assert(is_token(at(0), TOKEN_K_MAKE));

    if (!node_at(2)) {
        err_n("expected a make expression", cst);
        return newErrNode(astTrans, cst);
    }

    switch (node_at(2)->kind) {
        case CST_STRUCT_INIT_EXPR:
            n->kind = AST_MAKE_STRUCT_EXPR;
            n->makeStructExpr.type = transformTypeExpr(astTrans, node_at(1));
            transformStructInitExpr(astTrans, node_at(2), &n->makeStructExpr.entries);
            break;

        case CST_ANYOF_INIT_EXPR:
            n->kind = AST_MAKE_ANYOF_EXPR;
            n->makeAnyOfExpr.type = transformTypeExpr(astTrans, node_at(1));
            n->makeAnyOfExpr.expr = transformAnyOfInitExpr(astTrans, node_at(2), &n->makeAnyOfExpr.variant);
            break;

        default:
            assert(0);
    }
    return n;
}

// WhileStmt(4) = 'while'[0!] SimpleExpr[1E] Block[2] ElseClause?[3]
static AstNode *transformWhileStmt(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_WHILE_STMT);
    assert(CstChildren_size(&cst->children) == 4);

    DEF_NODE(n, AST_WHILE_STMT);

    assert(is_token(at(0), TOKEN_K_WHILE));
    n->whileStmt.condition = node_at(1)
                                 ? transformExpr(astTrans, node_at(1))
                                 : make_err("expected a condition", at(1));
    if (node_at(2)) {
        assert(is_node(at(2), CST_BLOCK));
        pushScope(astTrans);
        transformBlock(astTrans, node_at(2), &n->whileStmt.body);
        popScope(astTrans);
    } else
        err("expected a body for while loop", at(2));

    if (node_at(3)) {
        assert(is_node(at(3), CST_ELSE_CLAUSE));
        CstNode *elseClause = at(3)->node;
        CstChild *blockChild = CstChildren_at(&elseClause->children, 1);
        if (blockChild->kind == CST_CHILD_NODE) {
            assert(blockChild->node->kind == CST_BLOCK);
            pushScope(astTrans);
            transformBlock(astTrans,
                           blockChild->node,
                           &n->whileStmt.elseClause);
            popScope(astTrans);
        }
    }
    return n;
}

// 'IDENT'[0] (':'[1] TypeExpr[2E])?
static VarDef transformVarDef(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_VAR_DEF);
    assert(CstChildren_size(&cst->children) == 3);

    VarDef v;
    v.span = cst->span;
    if (is_token(at(0), TOKEN_IDENT))
        v.symbol = makeSymbol(astTrans, token_at(0));
    else
        v.symbol = NO_SYMBOL_ID;

    if (is_token(at(1), TOKEN_COLON)) {
        assert(node_at(2));
        v.type = transformTypeExpr(astTrans, node_at(2));
    } else
        v.type = Type_simple(TYPE_UNKNOWN);

    return v;
}

// ParenExpr(3) = '('[0!] Expr[1E] ')'[2]
static AstNode *transformParenExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_PAREN_EXPR);
    assert(CstChildren_size(&cst->children) == 3);

    return node_at(1)
               ? transformExpr(astTrans, node_at(1))
               : make_err("expected an expression inside parentheses", at(1));
}

// UnaryExpr(2) = UnaryOp[0!] PostfixExpr[1E]
// UnaryOp := '-' | 'not'
static AstNode *transformUnaryExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_UNARY_EXPR);
    assert(CstChildren_size(&cst->children) == 2);

    DEF_NODE(n, AST_UNARY_EXPR);

    assert(token_at(0));
    switch (token_at(0)->kind) {
        case TOKEN_K_NOT:
            n->unaryExpr.kind = UNARY_NOT;
            break;
        case TOKEN_MINUS:
            n->unaryExpr.kind = UNARY_NEG;
            break;
        default:
            assert(0);
    }
    assert(node_at(1));
    n->unaryExpr.expr = transformExpr(astTrans, node_at(1));
    return n;
}

// BinaryExpr(3) = UnaryExpr[0!E] BinaryOp[1!] UnaryExpr[2!E]
static AstNode *transformBinaryExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_BINARY_EXPR);
    assert(CstChildren_size(&cst->children) == 3);

    DEF_NODE(n, AST_BINARY_EXPR);

    assert(node_at(0));
    n->binaryExpr.lhs = transformExpr(astTrans, node_at(0));

    assert(token_at(1));
    switch (token_at(1)->kind) {
        case TOKEN_PLUS:
            n->binaryExpr.kind = BINARY_ADD;
            break;
        case TOKEN_MINUS:
            n->binaryExpr.kind = BINARY_SUB;
            break;
        case TOKEN_STAR:
            n->binaryExpr.kind = BINARY_MUL;
            break;
        case TOKEN_SLASH:
            n->binaryExpr.kind = BINARY_DIV;
            break;
        case TOKEN_PERCENT:
            n->binaryExpr.kind = BINARY_MOD;
            break;
        case TOKEN_DOUBLE_EQUAL:
            n->binaryExpr.kind = BINARY_EQ;
            break;
        case TOKEN_NOT_EQUAL:
            n->binaryExpr.kind = BINARY_NE;
            break;
        case TOKEN_LESS:
            n->binaryExpr.kind = BINARY_LT;
            break;
        case TOKEN_GREATER:
            n->binaryExpr.kind = BINARY_GT;
            break;
        case TOKEN_LESS_OR_EQUAL:
            n->binaryExpr.kind = BINARY_LE;
            break;
        case TOKEN_GREATER_OR_EQUAL:
            n->binaryExpr.kind = BINARY_GE;
            break;
        case TOKEN_K_AND:
            n->binaryExpr.kind = BINARY_AND;
            break;
        case TOKEN_K_OR:
            n->binaryExpr.kind = BINARY_OR;
            break;
        case TOKEN_PIPE_ARROW:
            n->binaryExpr.kind = BINARY_PIPE;
            break;
        default:
            assert(0);
    }

    assert(node_at(2));
    n->binaryExpr.rhs = transformExpr(astTrans, node_at(2));
    return n;
}

// LambdaParam(4) = 'IDENT'[0!] (':'[1] 'var'?[2] Type[3E])?
// TODO: 'var' is ignored
static VarDef transformLambdaParam(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_FN_PARAM);
    assert(CstChildren_size(&cst->children) == 4);

    VarDef v;
    v.span = cst->span;

    assert(is_token(at(0), TOKEN_IDENT));
    v.symbol = makeSymbol(astTrans, token_at(0));

    if (is_token(at(1), TOKEN_COLON)) {
        assert(at(3)->kind == CST_CHILD_NODE);
        v.type = transformTypeExpr(astTrans, node_at(2));
    } else
        v.type = Type_simple(TYPE_UNKNOWN);

    return v;
}

// LambdaParamList(*) = '|'[0!] LambdaParam*[*!E] '|'[?]
static void transformLambdaParamList(AstTransformer *astTrans, CstNode *cst, VarDefVec *params) {
    assert(cst);
    assert(cst->kind == CST_LAMBDA_PARAM_LIST);
    c_foreach(it, CstChildren, cst->children) {
        if (it.ref->kind != CST_CHILD_NODE || is_node(it.ref, CST_ERROR))
            continue;
        assert(is_node(it.ref, CST_LAMBDA_PARAM));
        VarDef param = transformLambdaParam(astTrans, it.ref->node);
        VarDefVec_push(params, param);
    }
}

// LambdaExpr(2) = LambdaParamList[0!] Block[1]
static AstNode *transformLambdaExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_LAMBDA_EXPR);
    assert(CstChildren_size(&cst->children) == 2);

    DEF_NODE(n, AST_LAMBDA_EXPR);

    assert(is_node(at(0), CST_LAMBDA_PARAM_LIST));
    pushScope(astTrans);
    transformLambdaParamList(astTrans, node_at(0), &n->lambdaExpr.params);

    if (node_at(1)) {
        assert(is_node(at(1), CST_BLOCK));
        transformBlock(astTrans, node_at(1), &n->whileStmt.body);
    } else
        err("expected a body for a lambda expression", at(1));
    popScope(astTrans);
    return n;
}

// Arg(2) = Expr[0!E] ','?[1]
static AstNode *transformArg(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_ARG);
    assert(CstChildren_size(&cst->children) == 2);

    return transformExpr(astTrans, node_at(0));
}

// ArgList(*) = '('[0!] Arg*[*!E] ')'[?]
static void transformArgList(AstTransformer *astTrans, CstNode *cst, AstChildren *args) {
    assert(cst);
    assert(cst->kind == CST_ARG_LIST);
    c_foreach(it, CstChildren, cst->children) {
        if (it.ref->kind != CST_CHILD_NODE || is_node(it.ref, CST_ERROR))
            continue;
        assert(is_node(it.ref, CST_ARG));
        AstNode *arg = transformArg(astTrans, it.ref->node);
        AstChildren_push(args, arg);
    }
}

// CallExpr(3) = PostfixExpr[0!E] ArgList?[1] LambdaExpr?[2]
static AstNode *transformCallExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_CALL_EXPR);
    assert(CstChildren_size(&cst->children) == 3);

    DEF_NODE(n, AST_CALL_EXPR);

    assert(node_at(0));
    n->callExpr.fn = transformExpr(astTrans, node_at(0));

    assert(node_at(1) || node_at(2));

    if (node_at(1)) {
        assert(is_node(at(1), CST_ARG_LIST));
        transformArgList(astTrans, node_at(1), &n->callExpr.args);
    }
    if (node_at(2)) {
        assert(is_node(at(2), CST_LAMBDA_EXPR));
        n->callExpr.lambdaExpr = transformLambdaExpr(astTrans, node_at(2));
    }

    return n;
}

// DotCallExpr(5) = PostfixExpr[0!E] '.'[!1] 'IDENT'[2] ArgList?[3] LambdaExpr?[4]
static AstNode *transformDotCallExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_DOT_CALL_EXPR);
    assert(CstChildren_size(&cst->children) == 5);

    DEF_NODE(n, AST_CALL_EXPR_CONST);

    assert(node_at(0));
    AstNode *firstArg = transformExpr(astTrans, node_at(0));
    AstChildren_push(&n->callExprConst.args, firstArg);

    assert(is_token(at(1), TOKEN_DOT));
    if (token_at(2)) {
        n->callExprConst.symbol = findSymbol(astTrans, token_at(2));
    } else
        n->callExprConst.symbol = NO_SYMBOL_ID;

    assert(node_at(3) || node_at(4));

    if (node_at(3)) {
        assert(is_node(at(3), CST_ARG_LIST));
        transformArgList(astTrans, node_at(3), &n->callExprConst.args);
    }
    if (node_at(4)) {
        assert(is_node(at(4), CST_LAMBDA_EXPR));
        n->callExprConst.lambdaExpr = transformLambdaExpr(astTrans, node_at(4));
    }

    return n;
}

// IndexExpr(4) = PostfixExpr[0!E] '['[1!] Expr[2!E] ']'[3]
static AstNode *transformIndexExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_INDEX_EXPR);
    assert(CstChildren_size(&cst->children) == 4);

    DEF_NODE(n, AST_INDEX_EXPR);

    assert(node_at(0));
    n->indexExpr.expr = transformExpr(astTrans, node_at(0));
    assert(node_at(2));
    n->indexExpr.index = transformExpr(astTrans, node_at(2));
    return n;
}

// DotExpr(3) = PostfixExpr[0!E] '.'[!1] 'IDENT'[2]
static AstNode *transformDotExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_DOT_EXPR);
    assert(CstChildren_size(&cst->children) == 3);

    DEF_NODE(n, AST_DOT_EXPR);

    assert(node_at(0));
    n->dotExpr.expr = transformExpr(astTrans, node_at(0));
    assert(token_at(2));
    n->dotExpr.field = makeField(astTrans, token_at(2));
    return n;
}

// VarExpr(1) = 'IDENT'[0!]
static AstNode *transformVarExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_VAR_EXPR);
    assert(CstChildren_size(&cst->children) == 1);

    DEF_NODE(n, AST_VAR_EXPR);

    assert(token_at(0));
    n->varExpr.var = findSymbol(astTrans, token_at(0));
    return n;
}

// ExprStmt(2) =
//    BlocklessExpr[0E] ';'[1]
//  | BlockExpr[0E]
static AstNode *transformExprStmt(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_EXPR_STMT);
    assert(CstChildren_size(&cst->children) == 2);

    DEF_NODE(n, AST_EXPR_STMT);

    assert(node_at(0));
    n->exprStmt.expr = transformExpr(astTrans, node_at(0));
    n->exprStmt.canReturn = !is_token(at(1), TOKEN_SEMI);
    return n;
}

// LetStmT(5) = 'let'[0!] VarDef[1!] '='[2] Expr[3E] ';'[4]
// VarStmt(5) = 'var'[0!] VarDef[1!] '='[2] Expr[3E] ';'[4]
static AstNode *transformLetStmt(AstTransformer *astTrans, CstNode *cst, b32 isMutable) {
    assert(cst);
    if (isMutable)
        assert(cst->kind == CST_VAR_STMT);
    else
        assert(cst->kind == CST_LET_STMT);
    assert(CstChildren_size(&cst->children) == 5);

    DEF_NODE(n, AST_LET_STMT);
    n->letStmt.isMutable = isMutable;

    assert(is_token(at(0), (isMutable ? TOKEN_K_VAR : TOKEN_K_LET)));
    assert(is_node(at(1), CST_VAR_DEF));
    n->letStmt.varDef = transformVarDef(astTrans, node_at(1));

    n->letStmt.initExpr = node_at(3)
                              ? transformExpr(astTrans, node_at(3))
                              : make_err("expected an expression", at(3));

    return n;
}

// WithStmt(5) = 'with'[0!] VarDef[1!] '='[2] Expr[3E] ';'[4]
static AstNode *transformWithStmt(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_WITH_STMT);
    assert(CstChildren_size(&cst->children) == 5);

    DEF_NODE(n, AST_WITH_STMT);

    assert(is_token(at(0), TOKEN_K_WITH));
    assert(is_node(at(1), CST_VAR_DEF));
    n->withStmt.varDef = transformVarDef(astTrans, node_at(1));

    n->withStmt.initExpr = node_at(3)
                               ? transformExpr(astTrans, node_at(3))
                               : make_err("expected an expression", at(3));

    return n;
}

// ReturnStmt(3) = 'return'[0!] Expr?[1E] ';'[2]
static AstNode *transformReturnStmt(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_RETURN_STMT);
    assert(CstChildren_size(&cst->children) == 3);

    DEF_NODE(n, AST_RETURN_STMT);

    assert(is_token(at(0), TOKEN_K_RETURN));

    n->returnStmt.expr = node_at(1)
                             ? transformExpr(astTrans, node_at(1))
                             : NULL;

    return n;
}

// BreakStmt(2) = 'break'[0!] ';'[2]
static AstNode *transformBreakStmt(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_BREAK_STMT);
    assert(CstChildren_size(&cst->children) == 2);

    DEF_NODE(n, AST_BREAK_STMT);
    return n;
}

// ContinueStmt(2) = 'continue'[0!] ';'[1]
static AstNode *transformContinueStmt(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_CONTINUE_STMT);
    assert(CstChildren_size(&cst->children) == 2);

    DEF_NODE(n, AST_CONTINUE_STMT);
    return n;
}

static u64 parseDecimal(csview s) {
    u64 val = 0;
    c_foreach(c, csview, s)
        val = val * 10 + (*c.ref - '0');
    return val;
}

static u64 parseHex(csview s) {
    u64 val = 0;
    assert(s.buf[0] == '0' && (s.buf[1] == 'x' || s.buf[1] == 'X'));
    s = csview_slice(s, 2, s.size);
    c_foreach(c, csview, s) {
        if (*c.ref >= '0' && *c.ref <= '9')
            val = val * 16 + (*c.ref - '0');
        else if (*c.ref >= 'a' && *c.ref <= 'f')
            val = val * 16 + (*c.ref - 'a' + 10);
        else if (*c.ref >= 'A' && *c.ref <= 'F')
            val = val * 16 + (*c.ref - 'A' + 10);
    }
    return val;
}

static u64 parseBinary(csview s) {
    u64 val = 0;
    c_foreach(c, csview, s)
        val = val * 2 + (*c.ref - '0');
    return val;
}

static AstNode *transformLiteralExpr(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_LITERAL_EXPR);
    assert(CstChildren_size(&cst->children) == 1);

    assert(token_at(0));
    switch (token_at(0)->kind) {
        case TOKEN_K_TRUE:
        case TOKEN_K_FALSE: {
            DEF_NODE(n, AST_BOOL_LITERAL_EXPR);
            n->boolLiteralExpr.val = token_at(0)->kind == TOKEN_K_TRUE;
            return n;
        }
        case TOKEN_DECIMAL: {
            DEF_NODE(n, AST_INT_LITERAL_EXPR);
            n->intLiteralExpr.val = parseDecimal(token_at(0)->text);
            return n;
        }
        case TOKEN_HEX: {
            DEF_NODE(n, AST_INT_LITERAL_EXPR);
            n->intLiteralExpr.val = parseHex(token_at(0)->text);
            return n;
        }
        case TOKEN_BINARY: {
            DEF_NODE(n, AST_INT_LITERAL_EXPR);
            n->intLiteralExpr.val = parseBinary(token_at(0)->text);
            return n;
        }
        case TOKEN_STRING: {
            DEF_NODE(n, AST_STR_LITERAL_EXPR);
            csview sv = token_at(0)->text;
            n->strLiteralExpr.str = csview_slice(sv, 1, sv.size - 2);
            return n;
        }
        case TOKEN_CHAR: {
            DEF_NODE(n, AST_CHAR_LITERAL_EXPR);
            csview sv = token_at(0)->text;
            n->charLiteralExpr.str = csview_slice(sv, 1, sv.size - 2);
            return n;
        }
        default:
            assert(0);
    }
}

static AstNode *transformExpr(AstTransformer *astTrans, CstNode *cst) {
    switch (cst->kind) {
        case CST_ERROR:
            return newErrNode(astTrans, cst);
        case CST_ASSIGN_EXPR:
            return transformAssignExpr(astTrans, cst);
        case CST_IF_EXPR:
            return transformIfExpr(astTrans, cst);
        case CST_PAREN_EXPR:
            return transformParenExpr(astTrans, cst);
        case CST_UNARY_EXPR:
            return transformUnaryExpr(astTrans, cst);
        case CST_BINARY_EXPR:
            return transformBinaryExpr(astTrans, cst);
        case CST_CALL_EXPR:
            return transformCallExpr(astTrans, cst);
        case CST_INDEX_EXPR:
            return transformIndexExpr(astTrans, cst);
        case CST_DOT_EXPR:
            return transformDotExpr(astTrans, cst);
        case CST_DOT_CALL_EXPR:
            return transformDotCallExpr(astTrans, cst);
        case CST_VAR_EXPR:
            return transformVarExpr(astTrans, cst);
        case CST_LITERAL_EXPR:
            return transformLiteralExpr(astTrans, cst);
        case CST_MAKE_EXPR:
            return transformMakeExpr(astTrans, cst);
        default:
            err_n("expected expression", cst);
            return newErrNode(astTrans, cst);
    }
}

// Block(*) = '{'[0!] Statement+[*!E] '}'[?]
// Statement :=
//    ExprStmt
//  | LetStmt
//  | VarStmt
//  | WithStmt
//  | ReturnStmt
//  | BreakStmt
//  | ContinueStmt
static void transformBlock(AstTransformer *astTrans, CstNode *cst, AstChildren *body) {
    assert(cst);
    assert(cst->kind == CST_BLOCK);

    c_foreach(it, CstChildren, cst->children) {
        if (it.ref->kind != CST_CHILD_NODE)
            continue;

        AstNode *n;
        switch (it.ref->node->kind) {
            case CST_ERROR:
                n = newErr(astTrans, it.ref);
            case CST_EXPR_STMT:
                n = transformExprStmt(astTrans, it.ref->node);
                break;
            case CST_LET_STMT:
                n = transformLetStmt(astTrans, it.ref->node, false);
                break;
            case CST_VAR_STMT:
                n = transformLetStmt(astTrans, it.ref->node, true);
                break;
            case CST_WITH_STMT:
                n = transformWithStmt(astTrans, it.ref->node);
                break;
            case CST_RETURN_STMT:
                n = transformReturnStmt(astTrans, it.ref->node);
                break;
            case CST_BREAK_STMT:
                n = transformBreakStmt(astTrans, it.ref->node);
                break;
            case CST_CONTINUE_STMT:
                n = transformContinueStmt(astTrans, it.ref->node);
                break;
            case CST_WHILE_STMT:
                n = transformWhileStmt(astTrans, it.ref->node);
                break;
            default:
                n = make_err("expected statement", it.ref);
                break;
        }

        AstChildren_push(body, n);
    }
}

// FnDef(7) = 'def'[0!] 'IDENT'[1] FnParamList[2] ('->'[3] TypeExpr[4E])? FnModifierList?[5] Block[6]
static AstNode *transformFnDef(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_FN_DEF);
    assert(CstChildren_size(&cst->children) == 7);

    DEF_NODE(n, AST_FN_DEF);

    assert(is_token(at(0), TOKEN_K_FN));

    n->fnDef.symbol = is_token(at(1), TOKEN_IDENT)
                          ? findSymbol(astTrans, token_at(1))
                          : NO_SYMBOL_ID;

    pushScope(astTrans);
    if (node_at(2)) {
        assert(is_node(at(2), CST_FN_PARAM_LIST));
        transformFnParamList(astTrans, node_at(2), &n->fnDef.params);
    } else
        err("expected a list of function parameters", at(2));

    if (token_at(3)) {
        assert(is_token(at(3), TOKEN_ARROW));
        assert(node_at(4));
        n->fnDef.returnType = transformTypeExpr(astTrans, node_at(4));
    } else {
        n->fnDef.returnType = Type_simple(TYPE_TUPLE);
    }

    if (is_node(at(5), CST_FN_MODIFIER_LIST)) {
        transformFnModifierList(astTrans, node_at(5), &n->fnDef.given);
    }

    if (node_at(6)) {
        assert(is_node(at(6), CST_BLOCK));
        transformBlock(astTrans, node_at(6), &n->fnDef.body);
    } else
        err("expected a function body", at(6));

    popScope(astTrans);
    return n;
}

// TypeDefParam(2) = TypeDefParamSpec[0!] ','?[1]
// TypeDefParamSpec := GenericParamName
static AstNode *transformTypeDefParam(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_TYPE_DEF);
    assert(CstChildren_size(&cst->children) == 2);

    DEF_NODE(n, AST_TYPE_DEF_PARAM);

    assert(node_at(0));

    switch (node_at(0)->kind) {
        case CST_GENERIC_PARAM_NAME:
            n->typeDefParam.param = transformGenericParamName(astTrans, node_at(0));
            break;
        default:
            assert(0);
    }

    return n;
}

// TypeDefParamList(*) = '['[0!] TypeDefParam+[*!E] ']'[?]
static void transformTypeDefParamList(AstTransformer *astTrans, CstNode *cst, AstChildren *typeParams) {
    assert(cst);
    assert(cst->kind == CST_TYPE_DEF_PARAM_LIST);

    c_foreach(it, CstChildren, cst->children) {
        if (it.ref->kind != CST_CHILD_NODE || is_node(it.ref, CST_ERROR))
            continue;
        assert(is_node(it.ref, CST_TYPE_DEF_PARAM));
        AstNode *param = transformTypeDefParam(astTrans, it.ref->node);
        AstChildren_push(typeParams, param);
    }
}

// TypeDefName(2) = 'IDENT'[0] TypeDefParamList?[1]
static void transformTypeDefName(AstTransformer *astTrans, CstNode *cst, AstNode *typeDef) {
    assert(cst);
    assert(cst->kind == CST_TYPE_DEF_NAME);
    assert(CstChildren_size(&cst->children) == 2);

    if (is_token(at(0), TOKEN_IDENT))
        typeDef->typeDef.baseType = Type_named(&astTrans->types, TYPE_NAMED, token_at(0)->text);
    else
        typeDef->typeDef.baseType = Type_simple(TYPE_ERROR);

    if (node_at(1)) {
        assert(is_node(at(1), CST_TYPE_DEF_PARAM_LIST));
        transformTypeDefParamList(astTrans, node_at(1), &typeDef->typeDef.typeParams);
    }
}

// StructEntry(4) = 'IDENT'[0!] ':'[1] Type[2] ','?[3]
static VarDef transformStructEntry(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_STRUCT_ENTRY);
    assert(CstChildren_size(&cst->children) == 4);

    VarDef v;
    v.span = cst->span;

    assert(is_token(at(0), TOKEN_IDENT));
    v.symbol = makeField(astTrans, token_at(0));

    if (is_token(at(1), TOKEN_COLON)) {
        assert(at(2)->kind == CST_CHILD_NODE);
        v.type = transformTypeExpr(astTrans, node_at(2));
    } else {
        err("struct fields must be declared with explicit types", at(0));
        v.type = Type_simple(TYPE_UNKNOWN);
    }

    return v;
}

// StructEntryList(*) = '{'[0!] StructEntry*[*] '}'[?]
static void transformStructEntryList(AstTransformer *astTrans, CstNode *cst, VarDefVec *list) {
    assert(cst);
    assert(cst->kind == CST_STRUCT_ENTRY_LIST);

    c_foreach(it, CstChildren, cst->children) {
        if (it.ref->kind != CST_CHILD_NODE || is_node(it.ref, CST_ERROR))
            continue;
        assert(is_node(it.ref, CST_STRUCT_ENTRY));
        VarDef entry = transformStructEntry(astTrans, it.ref->node);
        VarDefVec_push(list, entry);
    }
}

// StructDef(2) = 'struct'[0!] StructEntryList[1]
static AstNode *transformStructDef(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_STRUCT_DEF);
    assert(CstChildren_size(&cst->children) == 2);

    DEF_NODE(n, AST_STRUCT_DEF);

    if (node_at(1))
        transformStructEntryList(astTrans, node_at(1), &n->structDef.fields);
    return n;
}

// AnyOfEntry(4) = 'IDENT'[0!] (':'[1] Type[2])? ','?[3]
static VarDef transformAnyOfEntry(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_ANYOF_ENTRY);
    assert(CstChildren_size(&cst->children) == 4);

    VarDef v;
    v.span = cst->span;

    assert(is_token(at(0), TOKEN_IDENT));
    v.symbol = makeField(astTrans, token_at(0));

    if (is_token(at(1), TOKEN_COLON)) {
        assert(at(2)->kind == CST_CHILD_NODE);
        v.type = transformTypeExpr(astTrans, node_at(2));
    } else {
        v.type = Type_simple(TYPE_NONE);
    }

    return v;
}

// AnyOfEntryList(*) = '{'[0!] AnyOfEntry*[*] '}'[?]
static void transformAnyOfEntryList(AstTransformer *astTrans, CstNode *cst, VarDefVec *list) {
    assert(cst);
    assert(cst->kind == CST_ANYOF_ENTRY_LIST);

    c_foreach(it, CstChildren, cst->children) {
        if (it.ref->kind != CST_CHILD_NODE || is_node(it.ref, CST_ERROR))
            continue;
        assert(is_node(it.ref, CST_ANYOF_ENTRY));
        VarDef entry = transformAnyOfEntry(astTrans, it.ref->node);
        VarDefVec_push(list, entry);
    }
}

// AnyOfDef(2) = 'anyof'[0!] AnyOfEntryList[1]
static AstNode *transformAnyOfDef(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_ANYOF_DEF);
    assert(CstChildren_size(&cst->children) == 2);

    DEF_NODE(n, AST_ANYOF_DEF);

    if (node_at(1))
        transformAnyOfEntryList(astTrans, node_at(1), &n->anyOfDef.variants);
    return n;
}

// TypeDef(4) = 'type'[0!] TypeDefName[1!] '='[2] (StructDef | AnyOfDef)[3]
static AstNode *transformTypeDef(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_TYPE_DEF);
    DEF_NODE(n, AST_TYPE_DEF);

    assert(is_token(at(0), TOKEN_K_TYPE));

    transformTypeDefName(astTrans, node_at(1), n);

    if (!node_at(3)) {
        n->typeDef.def = newErr(astTrans, at(3));
        return n;
    }

    switch (node_at(3)->kind) {
        case CST_STRUCT_DEF:
            n->typeDef.def = transformStructDef(astTrans, node_at(3));
            break;
        case CST_ANYOF_DEF:
            n->typeDef.def = transformAnyOfDef(astTrans, node_at(3));
            break;
        default:
            n->typeDef.def = newErrNode(astTrans, node_at(3));
            break;
    }
    return n;
}

static AstNode *transformProgram(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_PROGRAM);
    DEF_NODE(n, AST_PROGRAM);

    c_foreach(it, CstChildren, cst->children) {
        if (it.ref->kind != CST_CHILD_NODE)
            continue;

        AstNode *new;
        switch (it.ref->node->kind) {
            case CST_FN_DEF:
                new = transformFnDef(astTrans, it.ref->node);
                break;
            case CST_TYPE_DEF:
                new = transformTypeDef(astTrans, it.ref->node);
                break;
            default:
                new = make_err("expected function definition", it.ref);
                break;
        }
        AstChildren_push(&n->program.children, new);
    }
    return n;
}

// FnDef(7) = 'fn'[0!] 'IDENT'[1] FnParamList[2] ('->'[3] TypeExpr[4E])? FnModifierList?[5] Block[6]
static void collectGlobalsFnDef(AstTransformer *astTrans, CstNode *cst) {
    assert(cst);
    assert(cst->kind == CST_FN_DEF);
    assert(CstChildren_size(&cst->children) == 7);
    assert(is_token(at(0), TOKEN_K_FN));

    if (is_token(at(1), TOKEN_IDENT))
        makeSymbol(astTrans, token_at(1));
}

void collectGlobalsNode(AstTransformer *astTrans, CstNode *cst) {
    switch (cst->kind) {
        case CST_PROGRAM:
            c_foreach(it, CstChildren, cst->children) {
                collectGlobalsNode(astTrans, it.ref->node);
            }
            break;
        case CST_FN_DEF:
            collectGlobalsFnDef(astTrans, cst);
            break;
        default:
    }
}

AstTransformResult astFromCst(Cst *cst) {
    Ast ast = {0};
    AstTransformer astTrans = {
        .pool = AstPool_init(64),
        .symbols = SymbolTable_init(),
        .errors = AstTransformErrorVec_init(),
        .scopes = ScopeStack_init(),
        .types = TypeTable_init(),
        .customTypes = {0},
    };

    ScopeStack_push(&astTrans.scopes, 0);
    assert(SymbolTable_scope(&astTrans.symbols, 1) == 1);
    ScopeStack_push(&astTrans.scopes, 1);

    collectGlobalsNode(&astTrans, cst->root);
    AstNode *program = transformProgram(&astTrans, cst->root);
    ast.pool = astTrans.pool;
    ast.root = program;
    ast.symbols = astTrans.symbols;
    ast.types = astTrans.types;
    ast.customTypes = astTrans.customTypes;
    return (AstTransformResult){.ast = ast, .errors = astTrans.errors};
}