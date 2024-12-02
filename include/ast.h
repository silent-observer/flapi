#ifndef AST_H
#define AST_H

#include "memcxt.h"

#include "sourcespan.h"
#include "symbols.h"
#include "typeid.h"
#include <stc/csview.h>

#define AST_KIND_LIST                            \
    XX(AST_ERROR, "Error")                       \
    XX(AST_PROGRAM, "Program")                   \
    XX(AST_FN_DEF, "FnDef")                      \
    XX(AST_EXPR_STMT, "ExprStmt")                \
    XX(AST_LET_STMT, "LetStmt")                  \
    XX(AST_WITH_STMT, "WithStmt")                \
    XX(AST_RETURN_STMT, "ReturnStmt")            \
    XX(AST_BREAK_STMT, "BreakStmt")              \
    XX(AST_CONTINUE_STMT, "ContinueStmt")        \
    XX(AST_ASSIGN_EXPR, "AssignExpr")            \
    XX(AST_IF_EXPR, "IfExpr")                    \
    XX(AST_IF_CLAUSE, "IfClause")                \
    XX(AST_WHILE_EXPR, "WhileExpr")              \
    XX(AST_FOR_EXPR, "ForExpr")                  \
    XX(AST_BINARY_EXPR, "BinaryExpr")            \
    XX(AST_UNARY_EXPR, "UnaryExpr")              \
    XX(AST_CALL_EXPR_CONST, "CallExprConst")     \
    XX(AST_CALL_EXPR, "CallExpr")                \
    XX(AST_LAMBDA_EXPR, "LambdaExpr")            \
    XX(AST_INDEX_EXPR, "IndexExpr")              \
    XX(AST_DOT_EXPR, "DotExpr")                  \
    XX(AST_VAR_EXPR, "VarExpr")                  \
    XX(AST_INT_LITERAL_EXPR, "IntLiteralExpr")   \
    XX(AST_STR_LITERAL_EXPR, "StrLiteralExpr")   \
    XX(AST_CHAR_LITERAL_EXPR, "CharLiteralExpr") \
    XX(AST_BOOL_LITERAL_EXPR, "BoolLiteralExpr")

typedef enum {
#define XX(kind, name) kind,
    AST_KIND_LIST AST_NODE_TYPE_MAX
#undef XX
} AstNodeKind;

static const csview AST_KIND_NAMES[AST_NODE_TYPE_MAX] = {
#define XX(kind, name) c_sv(name),
    AST_KIND_LIST
#undef XX
};

struct AstNode;
typedef struct AstNode AstNode;

typedef struct {
    SymbolId symbol;
    TypeId type;
    SourceSpan span;
} VarDef;

#define i_TYPE AstChildren, AstNode *, 2
#include "smallvec.h"

#define i_TYPE VarDefVec, VarDef, 2
#include "smallvec.h"

typedef struct {
    AstChildren children;
} ProgramNode;

typedef struct {
    SymbolId symbol;
    VarDefVec params;
    TypeId returnType;
    VarDefVec given;
    AstChildren body;
} FnDefNode;

typedef struct {
    AstNode *expr;
} ExprStmtNode;

typedef struct {
    VarDef varDef;
    b8 isMutable;
    AstNode *initExpr;
} LetStmtNode;

typedef struct {
    VarDef varDef;
    AstNode *initExpr;
} WithStmtNode;

typedef struct {
    AstNode *expr; // Can be NULL to indicate "return;"
} ReturnNode;

typedef struct {
    AstNode *expr; // Can be NULL to indicate "break;"
} BreakNode;

typedef struct {
} ContinueNode;

typedef enum {
    ASSIGN_REGULAR,
    ASSIGN_ADD,
    ASSIGN_SUB,
    ASSIGN_MUL,
    ASSIGN_DIV,
} AssignExprKind;

typedef struct {
    AssignExprKind kind;
    AstNode *lhs;
    AstNode *rhs;
} AssignExprNode;

typedef struct {
    AstChildren clauses;
} IfExprNode;

typedef struct {
    AstNode *condition; // Can be NULL to indicate "else"
    AstChildren body;
} IfClauseNode;

typedef struct {
    AstNode *condition; // Can be NULL to indicate "while true"
    AstChildren body;
} WhileExprNode;

typedef struct {
    VarDef varDef;
    AstNode *iterExpr;
    AstChildren body;
} ForExprNode;

typedef enum {
    BINARY_ADD,
    BINARY_SUB,
    BINARY_MUL,
    BINARY_DIV,
    BINARY_MOD,

    BINARY_EQ,
    BINARY_NE,
    BINARY_GT,
    BINARY_GE,
    BINARY_LT,
    BINARY_LE,

    BINARY_AND,
    BINARY_OR,

    BINARY_PIPE,
} BinaryExprKind;
typedef struct {
    BinaryExprKind kind;
    AstNode *lhs;
    AstNode *rhs;
} BinaryExprNode;

typedef enum {
    UNARY_PLUS,
    UNARY_NEG,
    UNARY_NOT,
} UnaryExprKind;
typedef struct {
    UnaryExprKind kind;
    AstNode *expr;
} UnaryExprNode;

typedef struct {
    SymbolId symbol;
    AstChildren args;
    AstNode *lambdaExpr;
} CallExprConstNode;

typedef struct {
    AstNode *fn;
    AstChildren args;
    AstNode *lambdaExpr;
} CallExprNode;

typedef struct {
    VarDefVec params;
    TypeId returnType;
    AstChildren body;
} LambdaExprNode;

typedef struct {
    AstNode *expr;
    AstNode *index;
} IndexExprNode;

typedef struct {
    AstNode *expr;
    SymbolId field;
} DotExprNode;

typedef struct {
    SymbolId var;
} VarExprNode;

typedef struct {
    u64 val;
} IntLiteralExprNode;

typedef struct {
    csview str;
} StrLiteralExprNode;

typedef struct {
    csview str;
} CharLiteralExprNode;

typedef struct {
    b32 val;
} BoolLiteralExprNode;

struct AstNode {
    AstNodeKind kind;
    SourceSpan span;
    TypeId type;
    union {
        ProgramNode program;
        FnDefNode fnDef;
        ExprStmtNode exprStmt;
        LetStmtNode letStmt;
        WithStmtNode withStmt;
        ReturnNode returnStmt;
        BreakNode breakStmt;
        ContinueNode continueStmt;
        AssignExprNode assignExpr;
        IfExprNode ifExpr;
        IfClauseNode ifClause;
        WhileExprNode whileExpr;
        ForExprNode forExpr;
        BinaryExprNode binaryExpr;
        UnaryExprNode unaryExpr;
        CallExprNode callExpr;
        CallExprConstNode callExprConst;
        LambdaExprNode lambdaExpr;
        IndexExprNode indexExpr;
        DotExprNode dotExpr;
        VarExprNode varExpr;
        IntLiteralExprNode intLiteralExpr;
        StrLiteralExprNode strLiteralExpr;
        CharLiteralExprNode charLiteralExpr;
        BoolLiteralExprNode boolLiteralExpr;
    };
};

#define i_TYPE AstPool, AstNode
#include "pool.h"

typedef struct {
    AstPool pool;
    AstNode *root;
    SymbolTable symbols;
    TypeMap types;
} Ast;

cstr printAst(const Ast *ast);

#endif