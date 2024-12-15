#ifndef CST_H
#define CST_H

#include "memcxt.h"

#include "token.h"

#define CST_KIND_LIST                              \
    XX(CST_ERROR, "Error")                         \
    XX(CST_PROGRAM, "Program")                     \
    XX(CST_FN_DEF, "FnDef")                        \
    XX(CST_FN_PARAM_LIST, "FnParamList")           \
    XX(CST_FN_PARAM, "FnParam")                    \
    XX(CST_FN_MODIFIER_LIST, "FnModifierList")     \
    XX(CST_GIVEN_MODIFIER, "GivenModifier")        \
    XX(CST_WITH_MODIFIER, "WithModifier")          \
    XX(CST_IMPLICIT_CLAUSE, "ImplicitClause")      \
    XX(CST_BLOCK, "Block")                         \
    XX(CST_EXPR_STMT, "ExprStmt")                  \
    XX(CST_LET_STMT, "LetStmt")                    \
    XX(CST_VAR_STMT, "VarStmt")                    \
    XX(CST_WITH_STMT, "WithStmt")                  \
    XX(CST_VAR_DEF, "VarDef")                      \
    XX(CST_RETURN_STMT, "ReturnStmt")              \
    XX(CST_BREAK_STMT, "BreakStmt")                \
    XX(CST_CONTINUE_STMT, "ContinueStmt")          \
    XX(CST_ASSIGN_EXPR, "AssignExpr")              \
    XX(CST_BASE_TYPE_EXPR, "BaseTypeExpr")         \
    XX(CST_GENERIC_PARAM_NAME, "GenericParamName") \
    XX(CST_CUSTOM_TYPE_EXPR, "CustomTypeExpr")     \
    XX(CST_GENERIC_ARG_LIST, "GenericArgList")     \
    XX(CST_GENERIC_ARG, "GenericArg")              \
    XX(CST_FUNCTION_TYPE_EXPR, "FunctionTypeExpr") \
    XX(CST_FUNCTION_SPEC, "FunctionSpec")          \
    XX(CST_TUPLE_TYPE_EXPR, "TupleTypeExpr")       \
    XX(CST_TYPE_ARG, "TypeArg")                    \
    XX(CST_IF_EXPR, "IfExpr")                      \
    XX(CST_IF_CLAUSE, "IfClause")                  \
    XX(CST_ELSE_IF_CLAUSE, "ElseIfClause")         \
    XX(CST_ELSE_CLAUSE, "ElseClause")              \
    XX(CST_WHILE_EXPR, "WhileExpr")                \
    XX(CST_LOOP_EXPR, "LoopExpr")                  \
    XX(CST_PAREN_EXPR, "ParenExpr")                \
    XX(CST_BINARY_EXPR, "BinaryExpr")              \
    XX(CST_UNARY_EXPR, "UnaryExpr")                \
    XX(CST_CALL_EXPR, "CallExpr")                  \
    XX(CST_ARG_LIST, "ArgList")                    \
    XX(CST_ARG, "Arg")                             \
    XX(CST_LAMBDA_EXPR, "LambdaExpr")              \
    XX(CST_LAMBDA_PARAM_LIST, "LambdaParamList")   \
    XX(CST_LAMBDA_PARAM, "LambdaParam")            \
    XX(CST_INDEX_EXPR, "IndexExpr")                \
    XX(CST_DOT_EXPR, "DotExpr")                    \
    XX(CST_DOT_CALL_EXPR, "DotCallExpr")           \
    XX(CST_VAR_EXPR, "VarExpr")                    \
    XX(CST_LITERAL_EXPR, "LiteralExpr")            \
    XX(CST_STRUCT_DEF, "StructDef")                \
    XX(CST_STRUCT_ENTRY_LIST, "StructEntryList")   \
    XX(CST_STRUCT_ENTRY, "StructEntry")            \
    XX(CST_ENUM_DEF, "EnumDef")                    \
    XX(CST_ENUM_ENTRY_LIST, "EnumEntryList")       \
    XX(CST_ENUM_ENTRY_SIMPLE, "EnumEntrySimple")   \
    XX(CST_ENUM_ENTRY_TUPLE, "EnumEntryTuple")     \
    XX(CST_ENUM_ENTRY_STRUCT, "EnumEntryStruct")

typedef enum {
#define XX(kind, name) kind,
    CST_KIND_LIST CST_NODE_TYPE_MAX
#undef XX
} CstNodeKind;

static const csview CST_KIND_NAMES[CST_NODE_TYPE_MAX] = {
#define XX(kind, name) c_sv(name),
    CST_KIND_LIST
#undef XX
};

struct CstNode;
typedef struct CstNode CstNode;

typedef enum {
    CST_CHILD_NONE,
    CST_CHILD_NODE,
    CST_CHILD_TOKEN,
} CstChildKind;

typedef struct {
    CstChildKind kind;
    union {
        const Token *token;
        CstNode *node;
        SourcePoint point;
    };
} CstChild;

#define i_TYPE CstChildren, CstChild, 6
#include "smallvec.h"

struct CstNode {
    CstNodeKind kind;
    SourceSpan span;
    CstChildren children;
};

#define i_TYPE CstPool, CstNode
#include "pool.h"
#define i_TYPE TokenPool, Token
#include "pool.h"

typedef struct {
    CstPool pool;
    CstNode *root;
} Cst;

b32 cstMatches(Cst *lhs, Cst *rhs);

typedef struct {
    Cst cst;
    TokenPool tokens;
} ParseCstResult;
ParseCstResult parseSExprCst(const char *input);
cstr printSExprCst(const CstNode *cst);

#endif