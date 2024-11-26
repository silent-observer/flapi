#include "parser.h"
#include "error.h"
#include <stc/algorithm.h>

typedef enum {
    Open = '(',
    Close = ')',
    Advance = '.',
} EventKind;

typedef struct {
    EventKind kind;
    CstNodeKind cstKind;
} Event;

#define i_TYPE EventVec, Event
#include <stc/vec.h>

typedef usize OpenIndex;
typedef usize ClosedIndex;

typedef struct {
    const Token *tokens;
    usize count;
    usize pos;
    u32 fuel;
    EventVec events;
    usize nodeCount;
    ParserErrorVec errors;
} Parser;

static OpenIndex openEvent(Parser *parser) {
    OpenIndex i = EventVec_size(&parser->events);
    EventVec_push(&parser->events, (Event){Open, CST_ERROR});
    return i;
}

static ClosedIndex closeEvent(Parser *parser, OpenIndex i, CstNodeKind cstKind) {
    parser->events.data[i].cstKind = cstKind;
    EventVec_push(&parser->events, (Event){Close, CST_ERROR});
    parser->nodeCount++;
    return i;
}

static OpenIndex openBeforeEvent(Parser *parser, ClosedIndex i) {
    EventVec_insert_n(&parser->events, i, &(Event){Open, CST_ERROR}, 1);
    return (OpenIndex)i;
}

static b32 eof(const Parser *parser) {
    return parser->tokens[parser->pos].kind == TOKEN_EOF;
}

static void advance(Parser *parser) {
    assert(!eof(parser));
    parser->fuel = 256;
    parser->pos++;
    parser->nodeCount++;
    EventVec_push(&parser->events, (Event){Advance, CST_ERROR});
}

static TokenKind nth(Parser *parser, i32 n) {
    if (parser->fuel == 0) {
        panic("Parser is stuck");
    }
    parser->fuel--;
    if (parser->pos + n >= parser->count)
        return TOKEN_EOF;
    else
        return parser->tokens[parser->pos + n].kind;
}

static b32 at(Parser *parser, TokenKind kind) {
    return nth(parser, 0) == kind;
}

static b32 eat(Parser *parser, TokenKind kind) {
    if (at(parser, kind)) {
        advance(parser);
        return true;
    } else
        return false;
}

static b32 expect(Parser *parser, TokenKind kind) {
    if (eat(parser, kind))
        return true;
    ParserErrorVec_push(
        &parser->errors,
        (ParserError){
            .kind = PARSER_ERROR_EXPECTED,
            .expected = kind,
            .got = &parser->tokens[parser->pos]});
    return false;
}

static void advanceWithError(Parser *parser, const char *error) {
    OpenIndex o = openEvent(parser);
    ParserErrorVec_push(
        &parser->errors,
        (ParserError){
            .kind = PARSER_ERROR_MSG,
            .msg = error});
    advance(parser);
    closeEvent(parser, o, CST_ERROR);
}

typedef enum {
    RIGHT_TIGTER,
    LEFT_TIGTER,
    AMBIGUOUS
} BindingPrecedence;

static void parseProgram(Parser *p);
static void parseFnDef(Parser *p);
static void parseFnParamList(Parser *p);
static void parseFnParam(Parser *p);
static void parseFnModifierList(Parser *p);
static void parseFnModifier(Parser *p);
static void parseGivenModifier(Parser *p);
static void parseWithModifier(Parser *p);
static void parseImplicitClause(Parser *p);

static void parseBlock(Parser *p);
static void parseExprStmt(Parser *p);
static void parseLetStmt(Parser *p);
static void parseVarStmt(Parser *p);
static void parseWithStmt(Parser *p);
static void parseVarDef(Parser *p);
static void parseReturnStmt(Parser *p);
static void parseBreakStmt(Parser *p);
static void parseContinueStmt(Parser *p);

static void parseTypeExpr(Parser *p);
static void parseGenericParamName(Parser *p);
static void parseCustomTypeExpr(Parser *p);
static void parseGenericArgList(Parser *p);
static void parseGenericArg(Parser *p);
static void parseFunctionTypeExpr(Parser *p);
static void parseFunctionSpec(Parser *p);
static void parseTupleTypeExpr(Parser *p);
static void parseTypeArg(Parser *p);

static void parseExpr(Parser *p);
static void parseBlockExpr(Parser *p);
static void parseBlocklessExpr(Parser *p);
static void parseIfExpr(Parser *p);
static void parseIfClause(Parser *p);
static void parseElseClause(Parser *p);
static void parseElseIfClause(Parser *p);
static void parseWhileExpr(Parser *p);
static void parseLoopExpr(Parser *p);
static void parseForExpr(Parser *p);

static ClosedIndex parseSimpleExpr(Parser *p);
static ClosedIndex parseDelimetedExpr(Parser *p);
static ClosedIndex parsePrattExpr(Parser *p, TokenKind left);
static BindingPrecedence bindingPrecedence(TokenKind left, TokenKind right);
static ClosedIndex parseDotOp(Parser *p, ClosedIndex lhs);
static ClosedIndex parseCallOp(Parser *p, ClosedIndex lhs);
static void parseArgList(Parser *p);
static void parseArg(Parser *p);
static ClosedIndex parseLambdaExpr(Parser *p);
static void parseLambdaParamList(Parser *p);
static void parseLambdaParam(Parser *p);
static ClosedIndex parseIndexOp(Parser *p, ClosedIndex lhs);

// Program = Definition*
// Definition =
//   FnDef
// | StructDef (TODO)
// | EnumDef (TODO)
static void parseProgram(Parser *p) {
    OpenIndex o = openEvent(p);

    while (!eof(p)) {
        if (at(p, TOKEN_K_FN))
            parseFnDef(p);
        else
            advanceWithError(p, "Expected a function definition");
    }

    closeEvent(p, o, CST_PROGRAM);
}

// FnDef ='fn' 'IDENT' FnParamList ('->' TypeExpr)? FnModifierList? Block
static void parseFnDef(Parser *p) {
    assert(at(p, TOKEN_K_FN));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_FN);
    expect(p, TOKEN_IDENT);
    if (at(p, TOKEN_LPAREN))
        parseFnParamList(p);
    if (eat(p, TOKEN_ARROW))
        parseTypeExpr(p);
    if (at(p, TOKEN_K_GIVEN))
        parseFnModifierList(p);
    if (at(p, TOKEN_LCURLY))
        parseBlock(p);

    closeEvent(p, o, CST_FN_DEF);
}

// FnParamList = '(' FnParam* ')'
static void parseFnParamList(Parser *p) {
    assert(at(p, TOKEN_LPAREN));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_LPAREN);
    while (!at(p, TOKEN_RPAREN) && !at(p, TOKEN_EOF)) {
        if (at(p, TOKEN_IDENT))
            parseFnParam(p);
        else
            break;
    }
    expect(p, TOKEN_RPAREN);

    closeEvent(p, o, CST_FN_PARAM_LIST);
}

// FnParam = 'IDENT' ':' 'var'? TypeExpr ','?
static void parseFnParam(Parser *p) {
    assert(at(p, TOKEN_IDENT));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_IDENT);
    expect(p, TOKEN_COLON);
    if (at(p, TOKEN_K_VAR))
        advance(p);
    parseTypeExpr(p);
    if (!at(p, TOKEN_RPAREN))
        expect(p, TOKEN_COMMA);

    closeEvent(p, o, CST_FN_PARAM);
}

// FnModifierList = FnModifier+
static void parseFnModifierList(Parser *p) {
    OpenIndex o = openEvent(p);

    do {
        parseFnModifier(p);
    } while (at(p, TOKEN_K_GIVEN) || at(p, TOKEN_K_WITH));

    closeEvent(p, o, CST_FN_MODIFIER_LIST);
}

// FnModifier = GivenModifier | WithModifier
static void parseFnModifier(Parser *p) {
    if (at(p, TOKEN_K_GIVEN))
        parseGivenModifier(p);
    else if (at(p, TOKEN_K_WITH))
        parseWithModifier(p);
    else
        assert(false);
}

static b32 isTypeExprStarter(Parser *p) {
    switch (nth(p, 0)) {
        case TOKEN_IDENT:
        case TOKEN_PERCENT:
        case TOKEN_LPAREN:
        case TOKEN_K_FN:
        case TOKEN_K_I8:
        case TOKEN_K_I16:
        case TOKEN_K_I32:
        case TOKEN_K_I64:
        case TOKEN_K_U8:
        case TOKEN_K_U16:
        case TOKEN_K_U32:
        case TOKEN_K_U64:
        case TOKEN_K_STR:
        case TOKEN_K_CHAR:
        case TOKEN_K_BOOL:
            return true;
        default:
            return false;
    }
}

static b32 isExprStarter(Parser *p) {
    switch (nth(p, 0)) {
        case TOKEN_IDENT:
        case TOKEN_DECIMAL:
        case TOKEN_BINARY:
        case TOKEN_STRING:
        case TOKEN_CHAR:
        case TOKEN_PIPE:
        case TOKEN_PLUS:
        case TOKEN_MINUS:
        case TOKEN_K_NOT:
        case TOKEN_K_IF:
        case TOKEN_K_WHILE:
        case TOKEN_K_LOOP:
        case TOKEN_K_FOR:
            return true;
        default:
            return false;
    }
}

// GivenModifier = 'given' '(' ImplicitClause+ ')'
static void parseGivenModifier(Parser *p) {
    assert(at(p, TOKEN_K_GIVEN));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_GIVEN);
    expect(p, TOKEN_LPAREN);
    while (!at(p, TOKEN_RPAREN) && !at(p, TOKEN_EOF)) {
        if (isTypeExprStarter(p))
            parseImplicitClause(p);
        else
            break;
    }
    expect(p, TOKEN_RPAREN);

    closeEvent(p, o, CST_GIVEN_MODIFIER);
}

// WithModifier = 'with' '(' ImplicitClause+ ')'
static void parseWithModifier(Parser *p) {
    assert(at(p, TOKEN_K_WITH));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_WITH);
    expect(p, TOKEN_LPAREN);
    while (!at(p, TOKEN_RPAREN) && !at(p, TOKEN_EOF)) {
        if (isTypeExprStarter(p))
            parseImplicitClause(p);
        else
            break;
    }
    expect(p, TOKEN_RPAREN);

    closeEvent(p, o, CST_WITH_MODIFIER);
}

// ImplicitClause = ('IDENT' ':')? TypeExpr ','?
static void parseImplicitClause(Parser *p) {
    assert(isTypeExprStarter(p));
    OpenIndex o = openEvent(p);

    if (at(p, TOKEN_IDENT) && nth(p, 1) == TOKEN_COLON) {
        expect(p, TOKEN_IDENT);
        expect(p, TOKEN_COLON);
    }
    parseTypeExpr(p);
    if (!at(p, TOKEN_RPAREN))
        expect(p, TOKEN_COMMA);

    closeEvent(p, o, CST_IMPLICIT_CLAUSE);
}

// Block = '{' Statement+ '}'
// Statement =
//    ExprStmt
//  | LetStmt
//  | VarStmt
//  | WithStmt
//  | ReturnStmt
//  | BreakStmt
//  | ContinueStmt
//  | AssignStmt
static void parseBlock(Parser *p) {
    assert(at(p, TOKEN_LCURLY));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_LCURLY);
    while (!at(p, TOKEN_RCURLY) && !at(p, TOKEN_EOF)) {
        switch (nth(p, 0)) {
            case TOKEN_K_LET:
                parseLetStmt(p);
                break;
            case TOKEN_K_VAR:
                parseVarStmt(p);
                break;
            case TOKEN_K_WITH:
                parseWithStmt(p);
                break;
            case TOKEN_K_RETURN:
                parseReturnStmt(p);
                break;
            case TOKEN_K_BREAK:
                parseBreakStmt(p);
                break;
            case TOKEN_K_CONTINUE:
                parseContinueStmt(p);
                break;
            default:
                parseExprStmt(p);
        }
    }
    expect(p, TOKEN_RCURLY);

    closeEvent(p, o, CST_BLOCK);
}

// LetStmt = 'let' VarDef '=' Expr ';'
static void parseLetStmt(Parser *p) {
    assert(at(p, TOKEN_K_LET));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_LET);
    parseVarDef(p);
    expect(p, TOKEN_EQUAL);
    parseExpr(p);
    expect(p, TOKEN_SEMI);

    closeEvent(p, o, CST_LET_STMT);
}

// VarStmt = 'var' VarDef '=' Expr ';'
static void parseVarStmt(Parser *p) {
    assert(at(p, TOKEN_K_VAR));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_VAR);
    parseVarDef(p);
    expect(p, TOKEN_EQUAL);
    parseExpr(p);
    expect(p, TOKEN_SEMI);

    closeEvent(p, o, CST_VAR_STMT);
}

// WithStmt = 'with' VarDef = Expr ';'
static void parseWithStmt(Parser *p) {
    assert(at(p, TOKEN_K_WITH));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_WITH);
    parseVarDef(p);
    expect(p, TOKEN_EQUAL);
    parseExpr(p);
    expect(p, TOKEN_SEMI);

    closeEvent(p, o, CST_WITH_STMT);
}

// VarDef = 'IDENT' (':' TypeExpr)?
static void parseVarDef(Parser *p) {
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_IDENT);
    if (eat(p, TOKEN_COLON))
        parseTypeExpr(p);

    closeEvent(p, o, CST_VAR_DEF);
}

// ReturnStmt = 'return' Expr? ';'
static void parseReturnStmt(Parser *p) {
    assert(at(p, TOKEN_K_RETURN));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_RETURN);
    if (isExprStarter(p))
        parseExpr(p);
    expect(p, TOKEN_SEMI);

    closeEvent(p, o, CST_RETURN_STMT);
}

// BreakStmt = 'break' Expr? ';'
static void parseBreakStmt(Parser *p) {
    assert(at(p, TOKEN_K_BREAK));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_BREAK);
    if (isExprStarter(p))
        parseExpr(p);
    expect(p, TOKEN_SEMI);

    closeEvent(p, o, CST_BREAK_STMT);
}

// ContinueStmt = 'continue' ';'
static void parseContinueStmt(Parser *p) {
    assert(at(p, TOKEN_K_CONTINUE));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_CONTINUE);
    expect(p, TOKEN_SEMI);

    closeEvent(p, o, CST_CONTINUE_STMT);
}

// ExprStmt =
//    BlocklessExpr ';'
//  | BlockExpr
static void parseExprStmt(Parser *p) {
    OpenIndex o = openEvent(p);
    switch (nth(p, 0)) {
        case TOKEN_K_IF:
        case TOKEN_K_WHILE:
        case TOKEN_K_FOR:
        case TOKEN_K_LOOP:
            parseBlockExpr(p);
            break;
        default:
            parseBlocklessExpr(p);
            expect(p, TOKEN_SEMI);
            break;
    }
    closeEvent(p, o, CST_EXPR_STMT);
}

// TypeExpr =
//    BaseTypeExpr
//  | GenericParamName
//  | CustomTypeExpr
//  | FunctionTypeExpr
//  | TupleTypeExpr
// BaseTypeExpr =
//    'i8'
//  | 'i16'
//  | 'i32'
//  | 'i64'
//  | 'u8'
//  | 'u16'
//  | 'u32'
//  | 'u64'
//  | 'str'
//  | 'char'
//  | 'bool'
static void parseTypeExpr(Parser *p) {
    switch (nth(p, 0)) {
        case TOKEN_K_I8:
        case TOKEN_K_I16:
        case TOKEN_K_I32:
        case TOKEN_K_I64:
        case TOKEN_K_U8:
        case TOKEN_K_U16:
        case TOKEN_K_U32:
        case TOKEN_K_U64:
        case TOKEN_K_STR:
        case TOKEN_K_CHAR:
        case TOKEN_K_BOOL: {
            OpenIndex o = openEvent(p);
            advance(p);
            closeEvent(p, o, CST_BASE_TYPE_EXPR);
            break;
        }
        case TOKEN_PERCENT:
            parseGenericParamName(p);
            break;
        case TOKEN_IDENT:
            parseCustomTypeExpr(p);
            break;
        case TOKEN_K_FN:
            parseFunctionTypeExpr(p);
            break;
        case TOKEN_LPAREN:
            parseTupleTypeExpr(p);
            break;
        default:
            ParserErrorVec_push(
                &p->errors,
                (ParserError){
                    .kind = PARSER_ERROR_MSG,
                    .msg = "Expected type"});
            return;
    }
}

// GenericParamName = '%' 'IDENT'
static void parseGenericParamName(Parser *p) {
    assert(at(p, TOKEN_PERCENT));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_PERCENT);
    expect(p, TOKEN_IDENT);

    closeEvent(p, o, CST_GENERIC_PARAM_NAME);
}

// CustomTypeExpr = 'IDENT' GenericArgList?
static void parseCustomTypeExpr(Parser *p) {
    assert(at(p, TOKEN_IDENT));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_IDENT);
    if (at(p, TOKEN_LPAREN))
        parseGenericArgList(p);

    closeEvent(p, o, CST_CUSTOM_TYPE_EXPR);
}

// GenericArgList = '[' GenericArg+ ']'
static void parseGenericArgList(Parser *p) {
    assert(at(p, TOKEN_LBRACK));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_LBRACK);
    while (!at(p, TOKEN_RBRACK) && !at(p, TOKEN_EOF)) {
        parseGenericArg(p);
    }
    expect(p, TOKEN_RBRACK);

    closeEvent(p, o, CST_GENERIC_ARG_LIST);
}

// GenericArg = (GenericParamName '=')? TypeExpr ','?
static void parseGenericArg(Parser *p) {
    OpenIndex o = openEvent(p);

    if (at(p, TOKEN_PERCENT)) {
        parseGenericParamName(p);
        expect(p, TOKEN_EQUAL);
    }
    parseTypeExpr(p);
    if (!at(p, TOKEN_RBRACK))
        expect(p, TOKEN_COMMA);

    closeEvent(p, o, CST_GENERIC_ARG);
}

// FunctionTypeExpr = FunctionSpec TupleTypeExpr ('->' TypeExpr)? FnModifierList?
static void parseFunctionTypeExpr(Parser *p) {
    assert(at(p, TOKEN_K_FN));
    OpenIndex o = openEvent(p);

    parseFunctionSpec(p);
    if (at(p, TOKEN_LPAREN))
        parseTupleTypeExpr(p);
    if (eat(p, TOKEN_ARROW))
        parseTypeExpr(p);
    if (at(p, TOKEN_K_GIVEN))
        parseFnModifierList(p);

    closeEvent(p, o, CST_FUNCTION_TYPE_EXPR);
}

// FunctionSpec = 'fn' ('{' '1' ('+' | '?')? '}')?
static void parseFunctionSpec(Parser *p) {
    assert(at(p, TOKEN_K_FN));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_FN);
    if (at(p, TOKEN_LCURLY)) {
        expect(p, TOKEN_LCURLY);
        expect(p, TOKEN_DECIMAL);
        if (at(p, TOKEN_PLUS) || at(p, TOKEN_QUESTION))
            advance(p);
        expect(p, TOKEN_RCURLY);
    }

    closeEvent(p, o, CST_FUNCTION_SPEC);
}

// TupleTypeExpr = '(' TupleArg* ')'
static void parseTupleTypeExpr(Parser *p) {
    assert(at(p, TOKEN_LPAREN));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_LPAREN);
    while (!at(p, TOKEN_RPAREN) && !at(p, TOKEN_EOF)) {
        parseTypeArg(p);
    }
    expect(p, TOKEN_RPAREN);

    closeEvent(p, o, CST_TUPLE_TYPE_EXPR);
}

// TypeArg = TypeExpr ','?
static void parseTypeArg(Parser *p) {
    OpenIndex o = openEvent(p);

    parseTypeExpr(p);
    if (!at(p, TOKEN_RPAREN))
        expect(p, TOKEN_COMMA);

    closeEvent(p, o, CST_TYPE_ARG);
}

// Expr = BlocklessExpr | BlockExpr
static void parseExpr(Parser *p) {
    switch (nth(p, 0)) {
        case TOKEN_K_IF:
        case TOKEN_K_WHILE:
        case TOKEN_K_FOR:
        case TOKEN_K_LOOP:
            parseBlockExpr(p);
            break;
        default:
            parseBlocklessExpr(p);
    }
}

// BlocklessExpr = SimpleExpr AssignPostfix?
// AssignPostfix = AssignOp Expr
// AssignOp = '=' | '+=' | '-=' | '*=' | '/=';
static void parseBlocklessExpr(Parser *p) {
    ClosedIndex c = parseSimpleExpr(p);
    switch (nth(p, 0)) {
        case TOKEN_EQUAL:
        case TOKEN_PLUS_EQUAL:
        case TOKEN_MINUS_EQUAL:
        case TOKEN_STAR_EQUAL:
        case TOKEN_SLASH_EQUAL: {
            OpenIndex o = openBeforeEvent(p, c);
            advance(p);
            parseExpr(p);
            closeEvent(p, o, CST_ASSIGN_EXPR);
            break;
        }
        default:
            break;
    }
}

// BlockExpr =
//   IfExpr
// | WhileExpr
// | LoopExpr
// | ForExpr
static void parseBlockExpr(Parser *p) {
    switch (nth(p, 0)) {
        case TOKEN_K_IF:
            parseIfExpr(p);
            break;
        case TOKEN_K_WHILE:
            parseWhileExpr(p);
            break;
        case TOKEN_K_FOR:
            parseForExpr(p);
            break;
        case TOKEN_K_LOOP:
            parseLoopExpr(p);
            break;
        default:
            assert(0);
    }
}

// IfExpr = IfClause ElseIfClause* ElseClause?
static void parseIfExpr(Parser *p) {
    assert(at(p, TOKEN_K_IF));
    OpenIndex o = openEvent(p);

    parseIfClause(p);
    while (at(p, TOKEN_K_ELSE) && nth(p, 1) == TOKEN_K_IF) {
        parseElseIfClause(p);
    }
    if (at(p, TOKEN_K_ELSE))
        parseElseClause(p);

    closeEvent(p, o, CST_IF_EXPR);
}

// IfClause = 'if' SimpleExpr Block
static void parseIfClause(Parser *p) {
    assert(at(p, TOKEN_K_IF));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_IF);
    if (isExprStarter(p))
        parseSimpleExpr(p);
    if (at(p, TOKEN_LCURLY))
        parseBlock(p);

    closeEvent(p, o, CST_IF_CLAUSE);
}

// ElseIfClause = 'else' 'if' SimpleExpr Block
static void parseElseIfClause(Parser *p) {
    assert(at(p, TOKEN_K_ELSE) && nth(p, 1) == TOKEN_K_IF);
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_ELSE);
    expect(p, TOKEN_K_IF);
    if (isExprStarter(p))
        parseSimpleExpr(p);
    if (at(p, TOKEN_LCURLY))
        parseBlock(p);

    closeEvent(p, o, CST_ELSE_IF_CLAUSE);
}

// ElseClause = 'else' Block
static void parseElseClause(Parser *p) {
    assert(at(p, TOKEN_K_ELSE));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_ELSE);
    if (at(p, TOKEN_LCURLY))
        parseBlock(p);

    closeEvent(p, o, CST_ELSE_CLAUSE);
}

// WhileExpr = 'while' SimpleExpr Block
static void parseWhileExpr(Parser *p) {
    assert(at(p, TOKEN_K_WHILE));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_WHILE);
    if (isExprStarter(p))
        parseSimpleExpr(p);
    if (at(p, TOKEN_LCURLY))
        parseBlock(p);

    closeEvent(p, o, CST_WHILE_EXPR);
}

// LoopExpr = 'loop' Block
static void parseLoopExpr(Parser *p) {
    assert(at(p, TOKEN_K_LOOP));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_LOOP);
    if (at(p, TOKEN_LCURLY))
        parseBlock(p);

    closeEvent(p, o, CST_LOOP_EXPR);
}

// ForExpr = 'for' VarDef 'in' SimpleExpr Block
static void parseForExpr(Parser *p) {
    assert(at(p, TOKEN_K_FOR));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_K_FOR);
    parseVarDef(p);
    expect(p, TOKEN_K_IN);
    if (isExprStarter(p))
        parseSimpleExpr(p);
    if (at(p, TOKEN_LCURLY))
        parseBlock(p);

    closeEvent(p, o, CST_FOR_EXPR);
}

// SimpleExpr =
//    LiteralExpr
//  | VarExpr
//  | LambdaExpr
//  | ParenExpr
//  | PrattExpr
static ClosedIndex parseSimpleExpr(Parser *p) {
    return parsePrattExpr(p, TOKEN_EOF);
}

// DelimetedExpr =
//    LiteralExpr
//  | VarExpr
//  | LambdaExpr
//  | ParenExpr
// LiteralExpr =
//    'DECIMAL'
//  | 'BINARY'
//  | 'STRING'
//  | 'CHAR'
//  | 'true'
//  | 'false'
// ParenExpr = '(' Expr ')'
static ClosedIndex parseDelimetedExpr(Parser *p) {
    OpenIndex o = openEvent(p);
    switch (nth(p, 0)) {
        case TOKEN_DECIMAL:
        case TOKEN_BINARY:
        case TOKEN_STRING:
        case TOKEN_CHAR:
        case TOKEN_K_TRUE:
        case TOKEN_K_FALSE:
            advance(p);
            return closeEvent(p, o, CST_LITERAL_EXPR);

        case TOKEN_IDENT:
            advance(p);
            return closeEvent(p, o, CST_VAR_EXPR);

        case TOKEN_LPAREN:
            advance(p);
            parseExpr(p);
            expect(p, TOKEN_RPAREN);
            return closeEvent(p, o, CST_PAREN_EXPR);

        case TOKEN_PIPE:
            return parseLambdaExpr(p);
        default:
            if (!eof(p))
                advance(p);
            return closeEvent(p, o, CST_ERROR);
    }
}

// PostfixOp =
//    DotOp
//  | CallOp
//  | IndexOp
static ClosedIndex parsePrattExpr(Parser *p, TokenKind left) {
    ClosedIndex lhs = parseDelimetedExpr(p);

    while (at(p, TOKEN_DOT) || at(p, TOKEN_LPAREN) || at(p, TOKEN_PIPE) || at(p, TOKEN_LBRACK)) {
        if (at(p, TOKEN_DOT))
            lhs = parseDotOp(p, lhs);
        else if (at(p, TOKEN_LPAREN) || at(p, TOKEN_PIPE))
            lhs = parseCallOp(p, lhs);
        else if (at(p, TOKEN_LBRACK))
            lhs = parseIndexOp(p, lhs);
        else
            assert(0);
    }

    while (true) {
        TokenKind right = nth(p, 0);
        BindingPrecedence bp = bindingPrecedence(left, right);
        if (bp == AMBIGUOUS) {
            ParserErrorVec_push(
                &p->errors,
                (ParserError){
                    .kind = PARSER_ERROR_EXPECTED,
                    .left_token = left,
                    .right_token = right,
                });
        }
        if (bp == RIGHT_TIGTER || bp == AMBIGUOUS) {
            OpenIndex o = openBeforeEvent(p, lhs);
            advance(p);
            parsePrattExpr(p, right);
            lhs = closeEvent(p, o, CST_BINARY_EXPR);
        } else
            break;
    }

    return lhs;
}

static b32 isArith(TokenKind kind) {
    switch (kind) {
        case TOKEN_PLUS:
        case TOKEN_MINUS:
        case TOKEN_STAR:
        case TOKEN_SLASH:
        case TOKEN_PERCENT:
            return true;
        default:
            return false;
    }
}

static b32 isComp(TokenKind kind) {
    switch (kind) {
        case TOKEN_DOUBLE_EQUAL:
        case TOKEN_NOT_EQUAL:
        case TOKEN_LESS:
        case TOKEN_GREATER:
        case TOKEN_LESS_OR_EQUAL:
        case TOKEN_GREATER_OR_EQUAL:
            return true;
        default:
            return false;
    }
}

static b32 isLogic(TokenKind kind) {
    switch (kind) {
        case TOKEN_K_AND:
        case TOKEN_K_OR:
            return true;
        default:
            return false;
    }
}

static b32 isOp(TokenKind kind) {
    return isArith(kind) || isComp(kind) || isLogic(kind) || kind == TOKEN_PIPE_ARROW;
}

static BindingPrecedence bindingPrecedence(TokenKind left, TokenKind right) {
    if (!isOp(right))
        return LEFT_TIGTER;
    if (!isOp(left)) {
        assert(left == TOKEN_EOF);
        return RIGHT_TIGTER;
    }

    if (isArith(left)) {
        if (isArith(right)) {
            switch (left) {
                case TOKEN_STAR:
                    return LEFT_TIGTER;
                case TOKEN_SLASH:
                    return right == TOKEN_STAR ? AMBIGUOUS : LEFT_TIGTER;
                case TOKEN_PERCENT:
                    return right == TOKEN_STAR || right == TOKEN_SLASH ? AMBIGUOUS : LEFT_TIGTER;
                case TOKEN_MINUS:
                case TOKEN_PLUS:
                    return right == TOKEN_STAR || right == TOKEN_SLASH ? RIGHT_TIGTER : LEFT_TIGTER;
                default:
                    assert(0);
            }
        } else
            return LEFT_TIGTER;
    } else if (isComp(left)) {
        if (isArith(right))
            return RIGHT_TIGTER;
        else if (isComp(right))
            return AMBIGUOUS;
        else
            return LEFT_TIGTER;
    } else if (isLogic(left)) {
        if (isArith(right) || isComp(right))
            return RIGHT_TIGTER;
        else if (isLogic(right)) {
            if (left == TOKEN_K_OR && right == TOKEN_K_AND)
                return RIGHT_TIGTER;
            else
                return LEFT_TIGTER;
        } else
            return LEFT_TIGTER;
    } else if (left == TOKEN_PIPE_ARROW) {
        if (right == TOKEN_PIPE_ARROW)
            return LEFT_TIGTER;
        else
            return RIGHT_TIGTER;
    } else
        assert(false);
}

// DotOp = '.' 'IDENT'
static ClosedIndex parseDotOp(Parser *p, ClosedIndex lhs) {
    OpenIndex o = openBeforeEvent(p, lhs);
    advance(p);
    expect(p, TOKEN_IDENT);
    return closeEvent(p, o, CST_DOT_EXPR);
}

// CallOp =
//    ArgList LambdaExpr?
//  | LambdaExpr
static ClosedIndex parseCallOp(Parser *p, ClosedIndex lhs) {
    assert(at(p, TOKEN_LPAREN) || at(p, TOKEN_PIPE));
    OpenIndex o = openBeforeEvent(p, lhs);
    if (at(p, TOKEN_LPAREN))
        parseArgList(p);
    if (at(p, TOKEN_PIPE))
        parseLambdaExpr(p);
    return closeEvent(p, o, CST_CALL_EXPR);
}

// ArgList = '(' Arg* ')'
static void parseArgList(Parser *p) {
    assert(at(p, TOKEN_LPAREN));
    OpenIndex o = openEvent(p);

    expect(p, TOKEN_LPAREN);
    while (!at(p, TOKEN_RPAREN) && !at(p, TOKEN_EOF)) {
        parseArg(p);
    }
    expect(p, TOKEN_RPAREN);

    closeEvent(p, o, CST_ARG_LIST);
}

// Arg = Expr ','?
static void parseArg(Parser *p) {
    OpenIndex o = openEvent(p);
    parseExpr(p);
    if (!at(p, TOKEN_RPAREN))
        expect(p, TOKEN_COMMA);
    closeEvent(p, o, CST_ARG);
}

// LambdaExpr = LambdaParamList Block
static ClosedIndex parseLambdaExpr(Parser *p) {
    assert(at(p, TOKEN_PIPE));
    OpenIndex o = openEvent(p);

    parseLambdaParamList(p);
    if (at(p, TOKEN_LCURLY))
        parseBlock(p);

    return closeEvent(p, o, CST_LAMBDA_EXPR);
}

// LambdaParamList = '|' LambdaParam* '|'
static void parseLambdaParamList(Parser *p) {
    assert(at(p, TOKEN_PIPE));
    OpenIndex o = openEvent(p);
    expect(p, TOKEN_PIPE);
    while (!at(p, TOKEN_PIPE) && !at(p, TOKEN_EOF)) {
        parseLambdaParam(p);
    }
    expect(p, TOKEN_PIPE);
    closeEvent(p, o, CST_LAMBDA_PARAM_LIST);
}

// LambdaParam = 'IDENT' (':' 'var'? Type)?
static void parseLambdaParam(Parser *p) {
    OpenIndex o = openEvent(p);
    expect(p, TOKEN_IDENT);
    if (at(p, TOKEN_COLON)) {
        advance(p);
        if (at(p, TOKEN_K_VAR))
            advance(p);
        parseTypeExpr(p);
    }
    closeEvent(p, o, CST_LAMBDA_PARAM);
}

// IndexOp = '[' Expr ']'
static ClosedIndex parseIndexOp(Parser *p, ClosedIndex lhs) {
    assert(at(p, TOKEN_LBRACK));
    OpenIndex o = openBeforeEvent(p, lhs);

    expect(p, TOKEN_LBRACK);
    parseExpr(p);
    expect(p, TOKEN_RBRACK);

    return closeEvent(p, o, CST_INDEX_EXPR);
}

#define i_TYPE NodeStack, CstNode *
#include <stc/stack.h>

static Cst buildCst(Parser *p) {
    CstPool pool = CstPool_init(p->nodeCount);
    const Token *t = p->tokens;
    NodeStack s = NodeStack_init();

    Event lcst_e = EventVec_pull(&p->events);
    assert(lcst_e.kind == Close);

    c_foreach(e, EventVec, p->events) {
        switch (e.ref->kind) {
            case Open: {
                CstNode *node = CstPool_new(&pool);
                node->kind = e.ref->cstKind;
                node->children = CstChildren_init();
                NodeStack_push(&s, node);
                break;
            }
            case Close: {
                assert(!NodeStack_empty(&s));
                CstNode *node = NodeStack_pull(&s);

                assert(!NodeStack_empty(&s));
                CstNode *parentNode = *NodeStack_top(&s);

                CstChildren_push(
                    &parentNode->children,
                    (CstChild){
                        .isToken = false,
                        .node = node,
                    });
                extendSourceSpan(&parentNode->span, &node->span);
                break;
            }
            case Advance: {
                assert(!NodeStack_empty(&s));
                CstNode *parentNode = *NodeStack_top(&s);

                CstChildren_push(
                    &parentNode->children,
                    (CstChild){
                        .isToken = true,
                        .token = t,
                    });
                SourceSpan s = makeSourceSpan(t->src.line, t->src.col, t->text.size);
                extendSourceSpan(&parentNode->span, &s);
                t++;
                break;
            }
            default:
                assert(0);
        }
    }

    assert(NodeStack_size(&s) == 1);
    assert(t->kind == TOKEN_EOF);

    CstNode *node = NodeStack_pull(&s);
    NodeStack_drop(&s);

    return (Cst){
        .pool = pool,
        .root = node,
    };
}

ParseResult parse(const Token tokens[], usize n) {
    Parser p = (Parser){
        .tokens = tokens,
        .count = n,
        .pos = 0,
        .fuel = 256,
        .events = EventVec_init(),
        .nodeCount = 0,
        .errors = ParserErrorVec_init(),
    };

    parseProgram(&p);
    Cst cst = buildCst(&p);
    EventVec_drop(&p.events);
    return (ParseResult){.cst = cst, .errors = p.errors};
}