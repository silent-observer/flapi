#ifndef TYPECHECK_STMT_H
#define TYPECHECK_STMT_H

#include "typecheck/common.h"

void typecheckExprStmt(TypeInferContext *ctx, AstNode *node);
void typecheckLetStmt(TypeInferContext *ctx, AstNode *node);
void typecheckWithStmt(TypeInferContext *ctx, AstNode *node);
void typecheckReturnStmt(TypeInferContext *ctx, AstNode *node);
void typecheckBreakStmt(TypeInferContext *ctx, AstNode *node);
void typecheckContinueStmt(TypeInferContext *ctx, AstNode *node);
void typecheckWhileStmt(TypeInferContext *ctx, AstNode *node);

#endif