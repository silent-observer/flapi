#ifndef AST_H
#define AST_H

#include "memcxt.h"

#include "token.h"

#define AST_KIND_LIST                              \
    XX(AST_ERROR, "Error")                         \
    XX(AST_PROGRAM, "Program")                     \
    XX(AST_FN_DEF, "FnDef")                        \
    XX(AST_FN_PARAM_LIST, "FnParamList")           \
    XX(AST_FN_PARAM, "FnParam")                    \
    XX(AST_FN_MODIFIER_LIST, "FnModifierList")     \
    XX(AST_GIVEN_MODIFIER, "GivenModifier")        \
    XX(AST_WITH_MODIFIER, "WithModifier")          \
    XX(AST_IMPLICIT_CLAUSE, "ImplicitClause")      \
    XX(AST_BLOCK, "Block")                         \
    XX(AST_EXPR_STMT, "ExprStmt")                  \
    XX(AST_LET_STMT, "LetStmt")                    \
    XX(AST_VAR_STMT, "VarStmt")                    \
    XX(AST_WITH_STMT, "WithStmt")                  \
    XX(AST_VAR_DEF, "VarDef")                      \
    XX(AST_RETURN_STMT, "ReturnStmt")              \
    XX(AST_BREAK_STMT, "BreakStmt")                \
    XX(AST_CONTINUE_STMT, "ContinueStmt")          \
    XX(AST_ASSIGN_EXPR, "AssignExpr")              \
    XX(AST_BASE_TYPE_EXPR, "BaseTypeExpr")         \
    XX(AST_GENERIC_PARAM_NAME, "GenericParamName") \
    XX(AST_CUSTOM_TYPE_EXPR, "CustomTypeExpr")     \
    XX(AST_GENERIC_ARG_LIST, "GenericArgList")     \
    XX(AST_GENERIC_ARG, "GenericArg")              \
    XX(AST_FUNCTION_TYPE_EXPR, "FunctionTypeExpr") \
    XX(AST_FUNCTION_SPEC, "FunctionSpec")          \
    XX(AST_TUPLE_TYPE_EXPR, "TupleTypeExpr")       \
    XX(AST_TYPE_ARG, "TypeArg")                    \
    XX(AST_IF_EXPR, "IfExpr")                      \
    XX(AST_IF_CLAUSE, "IfClause")                  \
    XX(AST_ELSE_IF_CLAUSE, "ElseIfClause")         \
    XX(AST_ELSE_CLAUSE, "ElseClause")              \
    XX(AST_WHILE_EXPR, "WhileExpr")                \
    XX(AST_LOOP_EXPR, "LoopExpr")                  \
    XX(AST_FOR_EXPR, "ForExpr")                    \
    XX(AST_PAREN_EXPR, "ParenExpr")                \
    XX(AST_BINARY_EXPR, "BinaryExpr")              \
    XX(AST_UNARY_EXPR, "UnaryExpr")                \
    XX(AST_CALL_EXPR, "CallExpr")                  \
    XX(AST_ARG_LIST, "ArgList")                    \
    XX(AST_ARG, "Arg")                             \
    XX(AST_LAMBDA_EXPR, "LambdaExpr")              \
    XX(AST_LAMBDA_PARAM_LIST, "LambdaParamList")   \
    XX(AST_LAMBDA_PARAM, "LambdaParam")            \
    XX(AST_INDEX_EXPR, "IndexExpr")                \
    XX(AST_DOT_EXPR, "DotExpr")                    \
    XX(AST_VAR_EXPR, "VarExpr")                    \
    XX(AST_LITERAL_EXPR, "LiteralExpr")            \
    XX(AST_STRUCT_DEF, "StructDef")                \
    XX(AST_STRUCT_ENTRY_LIST, "StructEntryList")   \
    XX(AST_STRUCT_ENTRY, "StructEntry")            \
    XX(AST_ENUM_DEF, "EnumDef")                    \
    XX(AST_ENUM_ENTRY_LIST, "EnumEntryList")       \
    XX(AST_ENUM_ENTRY_SIMPLE, "EnumEntrySimple")   \
    XX(AST_ENUM_ENTRY_TUPLE, "EnumEntryTuple")     \
    XX(AST_ENUM_ENTRY_STRUCT, "EnumEntryStruct")

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
    b8 isToken;
    union {
        const Token *token;
        AstNode *node;
    };
} AstChild;

#define i_TYPE AstChildren, AstChild, 6
#include "smallvec.h"

struct AstNode {
    AstNodeKind kind;
    AstChildren children;
};

#define i_TYPE AstPool, AstNode
#include "pool.h"
#define i_TYPE TokenPool, Token
#include "pool.h"

typedef struct {
    AstPool pool;
    AstNode *root;
} Ast;

b32 astMatches(Ast *lhs, Ast *rhs);

typedef struct {
    Ast ast;
    TokenPool tokens;
} ParseAstResult;
ParseAstResult parseSExprAst(const char *input);
cstr printSExprAst(const AstNode *ast);

#endif