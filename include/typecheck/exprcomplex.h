#ifndef TYPECHECK_EXPRCOMPLEX_H
#define TYPECHECK_EXPRCOMPLEX_H

#include "typecheck/common.h"

void typecheckAssignExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);
void typecheckIfExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);
void typecheckIfClause(TypeInferContext *ctx, AstNode *node, TypeId expected);
void typecheckMakeStructExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);
void typecheckMakeAnyOfExpr(TypeInferContext *ctx, AstNode *node, TypeId expected);

TypeId typeinferAssignExpr(TypeInferContext *ctx, AstNode *node);
TypeId typeinferIfExpr(TypeInferContext *ctx, AstNode *node);
TypeId typeinferIfClause(TypeInferContext *ctx, AstNode *node);
TypeId typeinferMakeStructExpr(TypeInferContext *ctx, AstNode *node);
TypeId typeinferMakeAnyOfExpr(TypeInferContext *ctx, AstNode *node);

#endif