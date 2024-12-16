#ifndef TYPECHECK_EXPRSIMPLE_H
#define TYPECHECK_EXPRSIMPLE_H

#include "typecheck/common.h"

void typecheckBinaryExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);
void typecheckUnaryExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);
void typecheckCallExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);
void typecheckCallExprConst(TypeInferContext *ctx, AstNode *node, TypeId expected);
void typecheckLambdaExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);
void typecheckIndexExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);
void typecheckDotExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);
void typecheckVarExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);
void typecheckLiteralExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);

TypeId typeinferBinaryExpr(TypeInferContext *ctx, AstNode *node);
TypeId typeinferUnaryExpr(TypeInferContext *ctx, AstNode *node);
TypeId typeinferCallExpr(TypeInferContext *ctx, AstNode *node);
TypeId typeinferCallExprConst(TypeInferContext *ctx, AstNode *node);
TypeId typeinferLambdaExpr(TypeInferContext *ctx, AstNode *node);
TypeId typeinferIndexExpr(TypeInferContext *ctx, AstNode *node);
TypeId typeinferDotExpr(TypeInferContext *ctx, AstNode *node);
TypeId typeinferVarExpr(TypeInferContext *ctx, AstNode *node);
TypeId typeinferLiteralExpr(TypeInferContext *ctx, AstNode *node);

#endif