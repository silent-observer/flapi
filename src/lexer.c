#include "lexer.h"

typedef struct {
    czview input;
    czview_iter it;
    i32 line;
    i32 col;
} Lexer;

static void initLexer(Lexer *lexer, czview input) {
    lexer->input = input;
    lexer->it = czview_begin(&lexer->input);
    lexer->line = 1;
    lexer->col = 1;
}

static Token lexWord(Lexer *lexer);
static Token lexNumber(Lexer *lexer);
static Token lexString(Lexer *lexer);
static Token lexChar(Lexer *lexer);

#define cur_char() (*lexer->it.ref)
#define char_at(i) (*czview_advance(lexer->it, i).ref)
#define cur_pos() (lexer->it.ref - lexer->input.str)
#define token_slice(len) csview_u8_substr(czview_sv(lexer->input), cur_pos(), len)

static Token nextToken(Lexer *lexer) {
    Token token = {0};

    // Skip whitespace
    while (true) {
        if (!lexer->it.ref) {
            token.kind = TOKEN_EOF;
            return token;
        }

        switch (*lexer->it.ref) {

            case '\n':
                czview_next(&lexer->it);
                lexer->line++;
                lexer->col = 1;
                continue;
            case '\t':
            case ' ':
            case '\r':
                czview_next(&lexer->it);
                lexer->col++;
                continue;
            default:
                break;
        }
        break;
    }

    token.line = lexer->line;
    token.col = lexer->col;

    // Special tokens
    u32 c = utf8_peek(lexer->it.ref);
    if (utf8_isalpha(c) || c == '_') {
        return lexWord(lexer);
    } else if (isdigit(c)) {
        return lexNumber(lexer);
    } else if (c == '"') {
        return lexString(lexer);
    } else if (c == '\'') {
        return lexChar(lexer);
    }

    // Symbols
    usize len = 0;
    switch (c) {
#define case_1char(ch, k) \
    case ch:              \
        token.kind = k;   \
        len = 1;          \
        break
        case_1char('.', TOKEN_DOT);
        case_1char(':', TOKEN_COLON);
        case_1char(';', TOKEN_SEMI);
        case_1char(',', TOKEN_COMMA);
        case_1char('%', TOKEN_PERCENT);
        case_1char('?', TOKEN_QUESTION);
        case_1char('(', TOKEN_LPAREN);
        case_1char(')', TOKEN_RPAREN);
        case_1char('[', TOKEN_LBRACK);
        case_1char(']', TOKEN_RBRACK);
        case_1char('{', TOKEN_LCURLY);
        case_1char('}', TOKEN_RCURLY);

#define case_1char_eq(ch, k, k_eq) \
    case ch:                       \
        if (char_at(1) == '=') {   \
            token.kind = k_eq;     \
            len = 2;               \
        } else {                   \
            token.kind = k;        \
            len = 1;               \
        }                          \
        break

        case_1char_eq('+', TOKEN_PLUS, TOKEN_PLUS_EQUAL);
        case_1char_eq('*', TOKEN_STAR, TOKEN_STAR_EQUAL);
        case_1char_eq('/', TOKEN_SLASH, TOKEN_SLASH_EQUAL);
        case_1char_eq('>', TOKEN_GREATER, TOKEN_GREATER_OR_EQUAL);
        case_1char_eq('<', TOKEN_LESS, TOKEN_LESS_OR_EQUAL);
        case_1char_eq('!', TOKEN_BANG, TOKEN_NOT_EQUAL);
        case_1char_eq('=', TOKEN_EQUAL, TOKEN_DOUBLE_EQUAL);
        case '|':
            if (char_at(1) == '>') {
                token.kind = TOKEN_PIPE_ARROW;
                len = 2;
            } else {
                token.kind = TOKEN_PIPE;
                len = 1;
            }
            break;
        case '-':
            if (char_at(1) == '>') {
                token.kind = TOKEN_ARROW;
                len = 2;
            } else if (char_at(1) == '=') {
                token.kind = TOKEN_MINUS_EQUAL;
                len = 2;
            } else {
                token.kind = TOKEN_MINUS;
                len = 1;
            }
            break;
    }

    if (len > 0) {
        token.text = token_slice(len);
        lexer->it = czview_advance(lexer->it, len);
        lexer->col += len;
    } else {
        token.kind = TOKEN_ERROR;
        token.text = c_sv("Invalid token");
    }
    return token;
}

typedef struct {
    TokenKind kind;
    csview str;
} KeywordTableEntry;

static const KeywordTableEntry KEYWORD_TABLE[] = {
#define XX(token, name, str) {token, c_sv(str)},
    TOKEN_KEY_LIST
#undef XX
};

static TokenKind findKeyword(const csview *s) {
    for (i32 i = 0; i < c_arraylen(KEYWORD_TABLE); i++) {
        if (csview_eq(&KEYWORD_TABLE[i].str, s))
            return KEYWORD_TABLE[i].kind;
    }
    return TOKEN_IDENT;
}

static Token lexWord(Lexer *lexer) {
    assert(lexer->it.ref);
    {
        u32 c = utf8_peek(lexer->it.ref);
        assert(utf8_isalpha(c) || c == '_');
    }

    i32 col = lexer->col + 1;
    czview_iter i = czview_advance(lexer->it, 1);
    for (; i.ref; czview_next(&i)) {
        u32 c = utf8_peek(i.ref);
        if (!utf8_isalnum(c) && c != '_')
            break;
        col++;
    }

    Token token;
    token.text = csview_from_n(lexer->it.ref, i.ref - lexer->it.ref);
    token.kind = findKeyword(&token.text);
    token.line = lexer->line;
    token.col = lexer->col;
    lexer->it = i;
    lexer->col = col;
    return token;
}

static Token lexNumber(Lexer *lexer) {
    assert(lexer->it.ref);

    char c = cur_char();
    char c_next = char_at(1);
    assert(isdigit(c));

    b32 is_hex = c == '0' && (c_next == 'x' || c_next == 'X');
    b32 is_bin = c == '0' && (c_next == 'b' || c_next == 'B');
    b32 is_dec = !is_hex && !is_bin;

    i32 col = lexer->col + 1;
    czview_iter i = czview_advance(lexer->it, 1);
    for (; i.ref; czview_next(&i)) {
        c = *i.ref;

        b32 bit = c >= '0' && c <= '1';
        b32 digit = c >= '0' && c <= '9';
        b32 hex_lower = c >= 'a' && c <= 'f';
        b32 hex_upper = c >= 'A' && c <= 'F';
        b32 allowed_hex = is_hex && (digit || hex_lower || hex_upper);
        b32 allowed_dec = is_dec && digit;
        b32 allowed_bin = is_bin && bit;

        if (!allowed_hex && !allowed_dec && !allowed_bin)
            break;
        col++;
    }

    Token token;
    token.text = csview_from_n(lexer->it.ref, i.ref - lexer->it.ref);
    token.kind = is_hex ? TOKEN_HEX : is_bin ? TOKEN_BINARY
                                             : TOKEN_DECIMAL;
    token.line = lexer->line;
    token.col = lexer->col;
    lexer->it = i;
    lexer->col = col;
    return token;
}

static Token lexString(Lexer *lexer) {
    assert(lexer->it.ref);
    assert(cur_char() == '"');

    Token token;
    token.line = lexer->line;
    token.col = lexer->col;

    i32 lines = 0;
    i32 col = lexer->col + 1;

    czview_iter i = czview_advance(lexer->it, 1);
    for (; i.ref; czview_next(&i)) {
        char c = *i.ref;

        if (c == '\0') {
            token.kind = TOKEN_ERROR;
            token.text = c_sv("Unclosed string literal");
            return token;
        } else if (c == '\\') {
            char c2 = *czview_advance(i, 1).ref;
            if (c2 == 'n' || c2 == 't' || c2 == '\'' || c2 == '"') {
                czview_next(&i);
                col++;
            } else {
                token.kind = TOKEN_ERROR;
                token.text = c_sv("Invalid escape sequence");
                token.col = col;
                return token;
            }
        } else if (c == '\n') {
            lines++;
        } else if (c == '"') {
            czview_next(&i);
            col++;
            break;
        }
        col++;
    }

    token.text = csview_from_n(lexer->it.ref, i.ref - lexer->it.ref);
    token.kind = TOKEN_STRING;
    lexer->col = col;
    lexer->line += lines;
    return token;
}
static Token lexChar(Lexer *lexer) {
    assert(lexer->it.ref);
    assert(char_at(0) == '\'');

    Token token;
    token.line = lexer->line;
    token.col = lexer->col;

    if (char_at(1) == '\\') {
        char c2 = char_at(2);
        if (c2 == 'n' || c2 == 't' || c2 == '\'' || c2 == '"') {
            if (char_at(3) == '\'') {
                token.kind = TOKEN_CHAR;
                token.text = token_slice(4);
                lexer->it = czview_advance(lexer->it, 4);
                lexer->col += 4;
                return token;
            } else {
                token.kind = TOKEN_ERROR;
                token.text = c_sv("Unclosed char literal");
                return token;
            }
        } else {
            token.kind = TOKEN_ERROR;
            token.text = c_sv("Invalid escape sequence");
            return token;
        }
    } else {
        if (char_at(2) == '\'') {
            token.kind = TOKEN_CHAR;
            token.text = token_slice(3);
            lexer->it = czview_advance(lexer->it, 3);
            lexer->col += 3;
            return token;
        } else {
            token.kind = TOKEN_ERROR;
            token.text = c_sv("Unclosed char literal");
            return token;
        }
    }
}

TokenVec lex(czview input) {
    Lexer lexer;
    initLexer(&lexer, input);
    TokenVec tokens = TokenVec_init();
    while (true) {
        Token token = nextToken(&lexer);
        TokenVec_push(&tokens, token);
        if (token.kind == TOKEN_EOF)
            break;
    }
    return tokens;
}