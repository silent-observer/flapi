#ifndef TYPECHECK_GLOBAL_H
#define TYPECHECK_GLOBAL_H

#include "typecheck/common.h"

void typecollectProgram(TypeInferContext *ctx, AstNode *node);
void typecollectFnDef(TypeInferContext *ctx, AstNode *node);
void typecollectTypeDef(TypeInferContext *ctx, AstNode *node);

void typecheckProgram(TypeInferContext *ctx, AstNode *node);
void typecheckFnDef(TypeInferContext *ctx, AstNode *node);

#endif