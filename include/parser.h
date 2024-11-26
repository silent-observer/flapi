#ifndef PARSER_H
#define PARSER_H

#include "memcxt.h"

#include "cst.h"
#include "token.h"
#include <stc/cstr.h>
#include <stdio.h>

typedef enum {
    PARSER_ERROR_EXPECTED,
    PARSER_ERROR_MSG,
    PARSER_ERROR_PRECEDENCE_AMBIGUOUS,
} ParserErrorKind;

typedef struct {
    ParserErrorKind kind;
    union {
        struct {
            TokenKind expected;
            const Token *got;
        };
        struct {
            TokenKind left_token;
            const Token *right_token;
        };
    };

    const char *msg;
} ParserError;

#define i_TYPE ParserErrorVec, ParserError
#include <stc/vec.h>

typedef struct {
    Cst cst;
    ParserErrorVec errors;
} ParseResult;

ParseResult parse(const Token tokens[], usize n);

static inline cstr printParserError(const ParserError *error) {
    cstr str = cstr_init();
    switch (error->kind) {
        case PARSER_ERROR_EXPECTED:
            cstr_printf(&str,
                        "Syntax error at (%d:%d): Expected '%s' but got '%.*s'\n",
                        error->got->src.line, error->got->src.col,
                        tokenKindStr(error->expected),
                        c_SV(error->got->text));
            break;
        case PARSER_ERROR_MSG:
            cstr_printf(&str,
                        "Syntax error at (%d:%d): %s\n",
                        error->got->src.line, error->got->src.col,
                        error->msg);
            break;
        case PARSER_ERROR_PRECEDENCE_AMBIGUOUS:
            cstr_printf(&str,
                        "Syntax error at (%d:%d): Precedence between operators '%s' and '%s' is ambiguous, please use parentheses\n",
                        error->right_token->src.line, error->right_token->src.col,
                        tokenKindStr(error->left_token),
                        tokenKindStr(error->right_token->kind));
            break;
    }
    return str;
}

#endif