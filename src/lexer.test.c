#include "lexer.h"
#include "test.h"

static const czview CODE = c_zv(
    "fn add(a: i32, b: i32) -> i32 {\n"
    "    return a + b;\n"
    "}");

static const Token TOKENS[] = {
    {c_sv("fn"), TOKEN_K_FN, 1, 1},
    {c_sv("add"), TOKEN_IDENT, 1, 4},
    {c_sv("("), TOKEN_LPAREN, 1, 7},
    {c_sv("a"), TOKEN_IDENT, 1, 8},
    {c_sv(":"), TOKEN_COLON, 1, 9},
    {c_sv("i32"), TOKEN_K_I32, 1, 11},
    {c_sv(","), TOKEN_COMMA, 1, 14},
    {c_sv("b"), TOKEN_IDENT, 1, 16},
    {c_sv(":"), TOKEN_COLON, 1, 17},
    {c_sv("i32"), TOKEN_K_I32, 1, 19},
    {c_sv(")"), TOKEN_RPAREN, 1, 22},
    {c_sv("->"), TOKEN_ARROW, 1, 24},
    {c_sv("i32"), TOKEN_K_I32, 1, 27},
    {c_sv("{"), TOKEN_LCURLY, 1, 31},
    {c_sv("return"), TOKEN_K_RETURN, 2, 5},
    {c_sv("a"), TOKEN_IDENT, 2, 12},
    {c_sv("+"), TOKEN_PLUS, 2, 14},
    {c_sv("b"), TOKEN_IDENT, 2, 16},
    {c_sv(";"), TOKEN_SEMI, 2, 17},
    {c_sv("}"), TOKEN_RCURLY, 3, 1},
};

int lexer_test() {
    pushMemCxt();
#define DEFER popMemCxt();

    TokenVec tokens = lex(CODE);
    test_assert_defer(TokenVec_size(&tokens) == c_arraylen(TOKENS) + 1);

    c_forrange(i, c_arraylen(TOKENS)) {
        Token token = tokens.data[i];
        test_assert_defer(token.kind == TOKENS[i].kind);
        test_assert_defer(csview_eq(&token.text, &TOKENS[i].text));
        test_assert_defer(token.line == TOKENS[i].line);
        test_assert_defer(token.col == TOKENS[i].col);
    }
    test_assert_defer(TokenVec_back(&tokens)->kind == TOKEN_EOF);

    DEFER
    test_success();
}