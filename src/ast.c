#include "ast.h"

typedef struct {
    const SymbolTable *symbols;
    const TypeMap *types;
    cstr out;
    u32 indent;
    b32 printTypes;
    const char *spaces;
} AstPrinter;

static void printNewline(AstPrinter *p) {
    cstr_append(&p->out, "\n");
    c_forrange(p->indent)
        cstr_append(&p->out, p->spaces);
}

static void printTag(AstPrinter *p, csview tag) {
    cstr_append_sv(&p->out, tag);
    cstr_append(&p->out, ": ");
}

static void printIndex(AstPrinter *p, u32 index) {
    cstr_append_fmt(&p->out, "%u: ", index);
}

static void printNode(AstPrinter *p, const AstNode *n);
static void printVarDef(AstPrinter *p, const VarDef *n);
static void printType(AstPrinter *p, TypeId t);

static void printSymbol(AstPrinter *p, SymbolId s) {
    Symbol *symbol = SymbolTable_lookup(p->symbols, s);
    cstr_append_fmt(&p->out, "<%.*s@%u>", c_SV(symbol->name), symbol->scope);
    if (p->printTypes) {
        cstr_append(&p->out, " : ");
        printType(p, symbol->type);
    }
}

static void printAstChildren(AstPrinter *p, const AstChildren *n) {
    if (AstChildren_size(n) == 0) {
        cstr_append(&p->out, "[]");
        return;
    }

    cstr_append(&p->out, "[");
    p->indent++;
    u32 i = 0;
    c_foreach(it, AstChildren, *n) {
        printNewline(p);
        printIndex(p, i++);
        // p->indent++;
        printNode(p, *it.ref);
        // p->indent--;
    }
    p->indent--;
    printNewline(p);
    cstr_append(&p->out, "]");
}

static void printVarDefVec(AstPrinter *p, const VarDefVec *n) {
    if (VarDefVec_size(n) == 0) {
        cstr_append(&p->out, "[]");
        return;
    }

    cstr_append(&p->out, "[");
    p->indent++;
    u32 i = 0;
    c_foreach(it, VarDefVec, *n) {
        printNewline(p);
        printIndex(p, i++);
        printVarDef(p, it.ref);
    }
    p->indent--;
    printNewline(p);
    cstr_append(&p->out, "]");
}

#define START(name)                      \
    do {                                 \
        cstr_append(&p->out, name);      \
        if (p->printTypes) {             \
            cstr_append(&p->out, " : "); \
            printType(p, n->type);       \
        }                                \
        cstr_append(&p->out, " {");      \
        p->indent++;                     \
    } while (0)
#define END()                      \
    do {                           \
        p->indent--;               \
        printNewline(p);           \
        cstr_append(&p->out, "}"); \
    } while (0)
#define END_EMPTY()                \
    do {                           \
        p->indent--;               \
        cstr_append(&p->out, "}"); \
    } while (0)
#define ONE(tag, c)             \
    do {                        \
        printNewline(p);        \
        printTag(p, c_sv(tag)); \
        printNode(p, c);        \
    } while (0)
#define MANY(tag, c)             \
    do {                         \
        printNewline(p);         \
        printTag(p, c_sv(tag));  \
        printAstChildren(p, &c); \
    } while (0)
#define ONE_VARDEF(tag, v)      \
    do {                        \
        printNewline(p);        \
        printTag(p, c_sv(tag)); \
        printVarDef(p, &v);     \
    } while (0)
#define MANY_VARDEF(tag, v)     \
    do {                        \
        printNewline(p);        \
        printTag(p, c_sv(tag)); \
        printVarDefVec(p, &v);  \
    } while (0)

#define SYMBOL(tag, s)          \
    do {                        \
        printNewline(p);        \
        printTag(p, c_sv(tag)); \
        printSymbol(p, s);      \
    } while (0)
#define TYPE(tag, t)            \
    do {                        \
        printNewline(p);        \
        printTag(p, c_sv(tag)); \
        printType(p, t);        \
    } while (0)

static void printProgram(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_PROGRAM);
    START("Program");
    MANY("fns", n->program.children);
    END();
}

static void printFnDef(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_FN_DEF);
    START("FnDef");
    SYMBOL("name", n->fnDef.symbol);
    MANY_VARDEF("params", n->fnDef.params);
    TYPE("return", n->fnDef.returnType);
    MANY_VARDEF("given", n->fnDef.given);
    MANY("body", n->fnDef.body);
    END();
}

static void printExprStmt(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_EXPR_STMT);
    START("ExprStmt");
    ONE("expr", n->exprStmt.expr);
    END();
}

static void printLetStmt(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_LET_STMT);
    START("LetStmt");
    ONE_VARDEF("var", n->letStmt.varDef);
    printNewline(p);
    printTag(p, c_sv("mutable"));
    if (n->letStmt.isMutable)
        cstr_append(&p->out, "true");
    else
        cstr_append(&p->out, "false");
    if (n->letStmt.initExpr)
        ONE("init", n->letStmt.initExpr);
    END();
}

static void printWithStmt(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_WITH_STMT);
    START("WithStmt");
    ONE_VARDEF("var", n->withStmt.varDef);
    ONE("init", n->withStmt.initExpr);
    END();
}

static void printReturnStmt(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_RETURN_STMT);
    START("ReturnStmt");
    if (n->returnStmt.expr) {
        ONE("expr", n->returnStmt.expr);
        END();
    } else
        END_EMPTY();
}

static void printBreakStmt(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_BREAK_STMT);
    START("BreakStmt");
    END_EMPTY();
}

static void printContinueStmt(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_CONTINUE_STMT);
    START("ContinueStmt");
    END_EMPTY();
}

static const char *const ASSIGN_EXPR_OP_STRS[] = {
    "",       // ASSIGN_REGULAR
    "\"+=\"", // ASSIGN_ADD
    "\"-=\"", // ASSIGN_SUB
    "\"*=\"", // ASSIGN_MUL
    "\"/=\"", // ASSIGN_DIV
};

static void
printAssignExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_ASSIGN_EXPR);
    START("AssignExpr");
    if (n->assignExpr.kind != ASSIGN_REGULAR) {
        printNewline(p);
        printTag(p, c_sv("op"));
        cstr_append(&p->out, ASSIGN_EXPR_OP_STRS[n->assignExpr.kind]);
    }
    ONE("lhs", n->assignExpr.lhs);
    ONE("rhs", n->assignExpr.rhs);
    END();
}

static void printIfExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_IF_EXPR);
    START("IfExpr");
    MANY("clauses", n->ifExpr.clauses);
    END();
}

static void printIfClause(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_IF_CLAUSE);
    if (n->ifClause.condition) {
        START("IfClause");
        ONE("cond", n->ifClause.condition);
        MANY("body", n->ifClause.body);
        END();
    } else {
        START("ElseClause");
        MANY("body", n->ifClause.body);
        END();
    }
}

static void printWhileStmt(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_WHILE_STMT);
    START("WhileStmt");
    ONE("cond", n->whileStmt.condition);
    MANY("body", n->whileStmt.body);
    if (AstChildren_size(&n->whileStmt.elseClause) > 0)
        MANY("else", n->whileStmt.elseClause);
    END();
}

static const char *const BINARY_EXPR_OP_STRS[] = {
    "\"+\"",   // BINARY_ADD
    "\"-\"",   // BINARY_SUB
    "\"*\"",   // BINARY_MUL
    "\"/\"",   // BINARY_DIV
    "\"%\"",   // BINARY_MOD
    "\"==\"",  // BINARY_EQ
    "\"!=\"",  // BINARY_NE
    "\"<\"",   // BINARY_LT
    "\">\"",   // BINARY_GT
    "\"<=\"",  // BINARY_LE
    "\">=\"",  // BINARY_GE
    "\"and\"", // BINARY_AND
    "\"or\"",  // BINARY_OR
    "\"|>\"",  // BINARY_PIPE
};

static void printBinaryExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_BINARY_EXPR);
    START("BinaryExpr");
    printNewline(p);
    printTag(p, c_sv("op"));
    cstr_append(&p->out, BINARY_EXPR_OP_STRS[n->binaryExpr.kind]);
    ONE("lhs", n->binaryExpr.lhs);
    ONE("rhs", n->binaryExpr.rhs);
    END();
}

static const char *const UNARY_EXPR_OP_STRS[] = {
    "\"+\"",   // UNARY_PLUS
    "\"-\"",   // UNARY_NEG
    "\"not\"", // UNARY_NOT
};

static void printUnaryExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_UNARY_EXPR);
    START("UnaryExpr");
    printNewline(p);
    printTag(p, c_sv("op"));
    cstr_append(&p->out, UNARY_EXPR_OP_STRS[n->unaryExpr.kind]);
    ONE("expr", n->unaryExpr.expr);
    END();
}

static void printCallExprConst(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_CALL_EXPR_CONST);
    START("CallExprConst");
    SYMBOL("name", n->callExprConst.symbol);
    MANY("args", n->callExprConst.args);
    if (n->callExprConst.lambdaExpr)
        ONE("lambda", n->callExprConst.lambdaExpr);
    END();
}

static void printCallExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_CALL_EXPR);
    START("CallExpr");
    ONE("fn", n->callExpr.fn);
    MANY("args", n->callExpr.args);
    if (n->callExpr.lambdaExpr)
        ONE("lambda", n->callExpr.lambdaExpr);
    END();
}

static void printLambdaExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_LAMBDA_EXPR);
    START("LambdaExpr");
    MANY_VARDEF("params", n->lambdaExpr.params);
    if (n->lambdaExpr.returnType.id != Type_simple(TYPE_TUPLE).id)
        TYPE("return", n->lambdaExpr.returnType);
    MANY("body", n->lambdaExpr.body);
    END();
}

static void printIndexExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_INDEX_EXPR);
    START("IndexExpr");
    ONE("expr", n->indexExpr.expr);
    ONE("index", n->indexExpr.index);
    END();
}

static void printDotExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_DOT_EXPR);
    START("DotExpr");
    ONE("expr", n->dotExpr.expr);
    SYMBOL("field", n->dotExpr.field);
    END();
}

static void printVarExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_VAR_EXPR);
    START("VarExpr");
    SYMBOL("var", n->varExpr.var);
    END();
}

static void printIntLiteralExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_INT_LITERAL_EXPR);
    cstr_append_fmt(&p->out, "IntLiteralExpr(%llu)", n->intLiteralExpr.val);
    if (p->printTypes) {
        cstr_append(&p->out, " : ");
        printType(p, n->type);
    }
}

static void printStrLiteralExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_STR_LITERAL_EXPR);
    cstr_append_fmt(&p->out, "StrLiteralExpr(\"%.*s\")", c_SV(n->strLiteralExpr.str));
    if (p->printTypes) {
        cstr_append(&p->out, " : ");
        printType(p, n->type);
    }
}

static void printCharLiteralExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_CHAR_LITERAL_EXPR);
    cstr_append_fmt(&p->out, "CharLiteralExpr(\'%.*s\')", c_SV(n->charLiteralExpr.str));
    if (p->printTypes) {
        cstr_append(&p->out, " : ");
        printType(p, n->type);
    }
}

static void printBoolLiteralExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_BOOL_LITERAL_EXPR);
    if (n->boolLiteralExpr.val)
        cstr_append(&p->out, "BoolLiteralExpr(true)");
    else
        cstr_append(&p->out, "BoolLiteralExpr(false)");
    if (p->printTypes) {
        cstr_append(&p->out, " : ");
        printType(p, n->type);
    }
}

static void printTypeConvertExpr(AstPrinter *p, const AstNode *n) {
    assert(n->kind == AST_TYPECONVERT_EXPR);
    START("TypeConvertExpr");
    ONE("expr", n->typeConvertExpr.expr);
    END();
}

static void printNode(AstPrinter *p, const AstNode *n) {
    switch (n->kind) {
        case AST_ERROR:
            cstr_append(&p->out, "<ERROR>");
            break;
        case AST_PROGRAM:
            printProgram(p, n);
            break;
        case AST_FN_DEF:
            printFnDef(p, n);
            break;
        case AST_EXPR_STMT:
            printExprStmt(p, n);
            break;
        case AST_LET_STMT:
            printLetStmt(p, n);
            break;
        case AST_WITH_STMT:
            printWithStmt(p, n);
            break;
        case AST_RETURN_STMT:
            printReturnStmt(p, n);
            break;
        case AST_BREAK_STMT:
            printBreakStmt(p, n);
            break;
        case AST_CONTINUE_STMT:
            printContinueStmt(p, n);
            break;
        case AST_WHILE_STMT:
            printWhileStmt(p, n);
            break;
        case AST_ASSIGN_EXPR:
            printAssignExpr(p, n);
            break;
        case AST_IF_EXPR:
            printIfExpr(p, n);
            break;
        case AST_IF_CLAUSE:
            printIfClause(p, n);
            break;
        case AST_BINARY_EXPR:
            printBinaryExpr(p, n);
            break;
        case AST_UNARY_EXPR:
            printUnaryExpr(p, n);
            break;
        case AST_CALL_EXPR_CONST:
            printCallExprConst(p, n);
            break;
        case AST_CALL_EXPR:
            printCallExpr(p, n);
            break;
        case AST_LAMBDA_EXPR:
            printLambdaExpr(p, n);
            break;
        case AST_INDEX_EXPR:
            printIndexExpr(p, n);
            break;
        case AST_DOT_EXPR:
            printDotExpr(p, n);
            break;
        case AST_VAR_EXPR:
            printVarExpr(p, n);
            break;
        case AST_INT_LITERAL_EXPR:
            printIntLiteralExpr(p, n);
            break;
        case AST_STR_LITERAL_EXPR:
            printStrLiteralExpr(p, n);
            break;
        case AST_CHAR_LITERAL_EXPR:
            printCharLiteralExpr(p, n);
            break;
        case AST_BOOL_LITERAL_EXPR:
            printBoolLiteralExpr(p, n);
            break;
        case AST_TYPECONVERT_EXPR:
            printTypeConvertExpr(p, n);
            break;
        default:
            assert(0);
    }
}

static void printVarDef(AstPrinter *p, const VarDef *v) {
    Symbol *symbol = SymbolTable_lookup(p->symbols, v->symbol);
    cstr_append_fmt(&p->out, "VarDef <%.*s@%u>", c_SV(symbol->name), symbol->scope);
    if (v->type.id != Type_simple(TYPE_UNKNOWN).id) {
        cstr_append(&p->out, " : ");
        printType(p, v->type);
    } else if (symbol->type.id != Type_simple(TYPE_UNKNOWN).id) {
        cstr_append(&p->out, " :? ");
        printType(p, symbol->type);
    }
}

static void printType(AstPrinter *p, TypeId t) {
    cstr s = TypeId_print(p->types, t);
    cstr_append_s(&p->out, s);
    cstr_drop(&s);
}

cstr printAst(const Ast *ast, b32 printTypes) {
    AstPrinter p = {
        .symbols = &ast->symbols,
        .types = &ast->types,
        .out = cstr_init(),
        .indent = 0,
        .spaces = "  ",
        .printTypes = printTypes,
    };
    printNode(&p, ast->root);
    return p.out;
}