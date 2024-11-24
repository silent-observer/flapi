#ifndef LEXER_H
#define LEXER_H

#include "memcxt.h"

#include "token.h"

TokenVec lex(czview input);

#endif