#ifndef TOKEN_H
#define TOKEN_H

#include "memcxt.h"

#include "sourcespan.h"
#include "types.h"
#include <stc/csview.h>
#include <stc/czview.h>

#define TOKEN_BIG_LIST               \
    XX(TOKEN_EOF, "Eof", "")         \
    XX(TOKEN_ERROR, "Error", "")     \
    XX(TOKEN_IDENT, "Ident", "")     \
    XX(TOKEN_DECIMAL, "Decimal", "") \
    XX(TOKEN_HEX, "Hex", "")         \
    XX(TOKEN_BINARY, "Binary", "")   \
    XX(TOKEN_STRING, "String", "")   \
    XX(TOKEN_CHAR, "Char", "")

#define TOKEN_SYM_LIST                                 \
    XX(TOKEN_EQUAL, "Equal", "=")                      \
    XX(TOKEN_DOUBLE_EQUAL, "Double_Equal", "==")       \
    XX(TOKEN_GREATER, "Greater", ">")                  \
    XX(TOKEN_LESS, "Less", "<")                        \
    XX(TOKEN_GREATER_OR_EQUAL, "GreaterOrEqual", ">=") \
    XX(TOKEN_LESS_OR_EQUAL, "LessOrEqual", "<=")       \
    XX(TOKEN_NOT_EQUAL, "NotEqual", "!=")              \
    XX(TOKEN_PLUS, "Plus", "+")                        \
    XX(TOKEN_MINUS, "Minus", "-")                      \
    XX(TOKEN_STAR, "Star", "*")                        \
    XX(TOKEN_SLASH, "Slash", "/")                      \
    XX(TOKEN_PLUS_EQUAL, "PlusEqual", "+=")            \
    XX(TOKEN_MINUS_EQUAL, "MinusEqual", "-=")          \
    XX(TOKEN_STAR_EQUAL, "StarEqual", "*=")            \
    XX(TOKEN_SLASH_EQUAL, "SlashEqual", "/=")          \
    XX(TOKEN_COLON, "Colon", ":")                      \
    XX(TOKEN_DOT, "Dot", ".")                          \
    XX(TOKEN_PERCENT, "Percent", "%")                  \
    XX(TOKEN_SEMI, "Semi", ";")                        \
    XX(TOKEN_PIPE, "Pipe", "|")                        \
    XX(TOKEN_COMMA, "Comma", ",")                      \
    XX(TOKEN_QUESTION, "Question", "?")                \
    XX(TOKEN_BANG, "Bang", "!")                        \
    XX(TOKEN_ARROW, "Arrow", "->")                     \
    XX(TOKEN_PIPE_ARROW, "PipeArrow", "|>")            \
    XX(TOKEN_LPAREN, "LParen", "(")                    \
    XX(TOKEN_RPAREN, "RParen", ")")                    \
    XX(TOKEN_LBRACK, "LBrack", "[")                    \
    XX(TOKEN_RBRACK, "RBrack", "]")                    \
    XX(TOKEN_LCURLY, "LCurly", "{")                    \
    XX(TOKEN_RCURLY, "RCurly", "}")

#define TOKEN_KEY_LIST                              \
    XX(TOKEN_K_FN, "Fn", "fn")                      \
    XX(TOKEN_K_IF, "If", "if")                      \
    XX(TOKEN_K_ELSE, "Else", "else")                \
    XX(TOKEN_K_FOR, "For", "for")                   \
    XX(TOKEN_K_IN, "In", "in")                      \
    XX(TOKEN_K_WHILE, "While", "while")             \
    XX(TOKEN_K_LOOP, "Loop", "loop")                \
    XX(TOKEN_K_BREAK, "Break", "break")             \
    XX(TOKEN_K_CONTINUE, "Continue", "continue")    \
    XX(TOKEN_K_VAR, "Var", "var")                   \
    XX(TOKEN_K_LET, "Let", "let")                   \
    XX(TOKEN_K_RETURN, "Return", "return")          \
    XX(TOKEN_K_AND, "And", "and")                   \
    XX(TOKEN_K_OR, "Or", "or")                      \
    XX(TOKEN_K_NOT, "Not", "not")                   \
    XX(TOKEN_K_GIVEN, "Given", "given")             \
    XX(TOKEN_K_INTERFACE, "Interface", "interface") \
    XX(TOKEN_K_EXEC, "Exec", "exec")                \
    XX(TOKEN_K_WITH, "With", "with")                \
    XX(TOKEN_K_FORWARD, "Forward", "forward")       \
    XX(TOKEN_K_TRUE, "True", "true")                \
    XX(TOKEN_K_FALSE, "False", "false")             \
    XX(TOKEN_K_I8, "I8", "i8")                      \
    XX(TOKEN_K_I16, "I16", "i16")                   \
    XX(TOKEN_K_I32, "I32", "i32")                   \
    XX(TOKEN_K_I64, "I64", "i64")                   \
    XX(TOKEN_K_U8, "U8", "u8")                      \
    XX(TOKEN_K_U16, "U16", "u16")                   \
    XX(TOKEN_K_U32, "U32", "u32")                   \
    XX(TOKEN_K_U64, "U64", "u64")                   \
    XX(TOKEN_K_STR, "Str", "str")                   \
    XX(TOKEN_K_CHAR, "Char", "char")                \
    XX(TOKEN_K_BOOL, "Bool", "bool")

typedef enum {
#define XX(token, name, str) token,
    TOKEN_BIG_LIST TOKEN_SYM_LIST TOKEN_KEY_LIST TOKEN_KIND_COUNT
#undef XX
} TokenKind;

static const czview TOKEN_KIND_NAMES[TOKEN_KIND_COUNT] = {
#define XX(token, name, str) c_zv(name),
    TOKEN_BIG_LIST TOKEN_SYM_LIST TOKEN_KEY_LIST
#undef XX
};

static const czview TOKEN_KIND_STRS[TOKEN_KIND_COUNT] = {
#define XX(token, name, str) c_zv(str),
    TOKEN_BIG_LIST TOKEN_SYM_LIST TOKEN_KEY_LIST
#undef XX
};

#define tokenKindStr(kind)              \
    ((kind) < TOKEN_EQUAL               \
         ? TOKEN_KIND_NAMES[(kind)].str \
         : TOKEN_KIND_STRS[(kind)].str)

typedef struct {
    csview text;
    TokenKind kind;
    SourcePoint src;
} Token;

#define i_TYPE TokenVec, Token
#include <stc/vec.h>

#endif