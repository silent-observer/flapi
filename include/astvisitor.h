#include "ast.h"

#ifndef i_visit
#error "i_visit not defined"
#endif

#ifndef i_context
#error "i_context not defined"
#endif

#ifndef i_func
#error "i_func not defined"
#endif

#define ONE(n) n ? i_func(ctx, n) : NULL
#define MANY(c)                         \
    do {                                \
        c_foreach(it, AstChildren, c) { \
            i_func(ctx, it.ref);        \
        }                               \
    } while (0)

static inline void i_visit(i_context *ctx, AstNode *n) {
    switch (n->kind) {
        case AST_PROGRAM:
            MANY(n->program.children);
            break;
        case AST_FN_DEF:
            MANY(n->fnDef.body);
            break;
        case AST_EXPR_STMT:
            ONE(n->exprStmt.expr);
            break;
        case AST_LET_STMT:
            ONE(n->letStmt.initExpr);
            break;
        case AST_WITH_STMT:
            ONE(n->withStmt.initExpr);
            break;
        case AST_RETURN_STMT:
            ONE(n->returnStmt.expr);
            break;
        case AST_BREAK_STMT:
            ONE(n->breakStmt.expr);
            break;
        case AST_ASSIGN_EXPR:
            ONE(n->assignExpr.lhs);
            ONE(n->assignExpr.rhs);
            break;
        case AST_IF_EXPR:
            MANY(n->ifExpr.clauses);
            break;
        case AST_IF_CLAUSE:
            ONE(n->ifClause.condition);
            MANY(n->ifClause.body);
            break;
        case AST_WHILE_EXPR:
            ONE(n->whileStmt.condition);
            MANY(n->whileStmt.body);
            break;
        case AST_FOR_EXPR:
            ONE(n->forExpr.iterExpr);
            MANY(n->forExpr.body);
            break;
        case AST_BINARY_EXPR:
            ONE(n->binaryExpr.lhs);
            ONE(n->binaryExpr.rhs);
            break;
        case AST_UNARY_EXPR:
            ONE(n->unaryExpr.expr);
            break;
        case AST_CALL_EXPR_CONST:
            MANY(n->callExprConst.args);
            break;
        case AST_CALL_EXPR:
            ONE(n->callExpr.fn);
            MANY(n->callExpr.args);
            break;
        case AST_LAMBDA_EXPR:
            MANY(n->lambdaExpr.body);
            break;
        case AST_INDEX_EXPR:
            ONE(n->indexExpr.expr);
            ONE(n->indexExpr.index);
            break;
        case AST_DOT_EXPR:
            ONE(n->dotExpr.expr);
            break;

        case AST_ERROR:
        case AST_INT_LITERAL_EXPR:
        case AST_STR_LITERAL_EXPR:
        case AST_CHAR_LITERAL_EXPR:
        case AST_BOOL_LITERAL_EXPR:
            break;
        default:
            assert(0);
    }
}

#undef i_context
#undef i_func
#undef i_func_vardef
#undef ONE
#undef MANY