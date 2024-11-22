#include "ast.h"
#include <stc/cstr.h>
#include <stdio.h>

static inline b32 astChildMatches(AstChild *lhs, AstChild *rhs);

static inline b32 astNodeMatches(AstNode *lhs, AstNode *rhs) {
    if (lhs->kind != rhs->kind)
        return false;
    if (AstChildren_size(&lhs->children) != AstChildren_size(&rhs->children))
        return false;
    AstChild *lhs_children = AstChildren_data(&lhs->children);
    AstChild *rhs_children = AstChildren_data(&rhs->children);
    for (u32 i = 0; i < AstChildren_size(&lhs->children); i++)
        if (!astChildMatches(&lhs_children[i], &rhs_children[i]))
            return false;
    return true;
}

static inline b32 astChildMatches(AstChild *lhs, AstChild *rhs) {
    if (lhs->isToken != rhs->isToken)
        return false;

    if (lhs->isToken)
        return lhs->token->kind == rhs->token->kind &&
               csview_eq(&lhs->token->text, &rhs->token->text);
    else
        return astNodeMatches(lhs->node, rhs->node);
}

b32 astMatches(Ast *lhs, Ast *rhs) {
    return astNodeMatches(lhs->root, rhs->root);
}

typedef struct {
    Ast ast;
    TokenPool tokens;
    const char *input;
} AstParser;

static inline usize countUntilEnd(const AstParser *p) {
    usize count = 0;
    while (true)
        switch (p->input[count]) {
            case '\0':
            case ' ':
            case '\t':
            case '\n':
            case '\r':
            case ')':
            case ']':
                return count;
            default:
                count++;
                break;
        }
}

static inline void skipWhiteSpace(AstParser *p) {
    while (true)
        switch (*p->input) {
            case ' ':
            case '\t':
            case '\n':
            case '\r':
                p->input++;
                break;
            default:
                return;
        }
}

static AstNodeKind parseNodeKind(csview word) {
    for (u32 i = 0; i < c_arraylen(AST_KIND_NAMES); i++) {
        if (csview_eq(&AST_KIND_NAMES[i], &word)) {
            return (AstNodeKind)i;
        }
    }
    fprintf(stderr, "Unknown node kind: %.*s\n", c_SV(word));
    assert(0);
}

static TokenKind parseTokenKind(csview word) {
    for (u32 i = 0; i < c_arraylen(TOKEN_KIND_NAMES); i++) {
        if (csview_equals_sv(czview_sv(TOKEN_KIND_NAMES[i]), word)) {
            return (TokenKind)i;
        }
    }
    fprintf(stderr, "Unknown token kind: %.*s\n", c_SV(word));
    assert(0);
}

static AstChild parseChild(AstParser *p);

static AstNode *parseNode(AstParser *p) {
    assert(p->input[0] == '(');
    p->input++;

    i32 wordLength = countUntilEnd(p);
    csview word = csview_from_n(p->input, wordLength);
    p->input += wordLength;

    AstNodeKind kind = parseNodeKind(word);
    AstNode *node = AstPool_new(&p->ast.pool);
    node->kind = kind;
    node->children = AstChildren_init();

    while (true) {
        skipWhiteSpace(p);
        if (*p->input == '\0') {
            fprintf(stderr, "Unmatched parentheses\n");
            assert(0);
        }
        if (*p->input == ')') {
            p->input++;
            break;
        }
        AstChildren_push(
            &node->children,
            parseChild(p));
    }
    return node;
}

static Token *parseToken(AstParser *p) {
    assert(p->input[0] == '[');
    p->input++;

    i32 wordLength = countUntilEnd(p);
    csview word = csview_from_n(p->input, wordLength);
    p->input += wordLength;

    TokenKind kind = parseTokenKind(word);
    csview text = czview_sv(TOKEN_KIND_STRS[kind]);
    if (kind < TOKEN_EQUAL) {
        skipWhiteSpace(p);
        i32 textLength = countUntilEnd(p);
        text = csview_from_n(p->input, textLength);
        p->input += textLength;
    }

    Token *token = TokenPool_new(&p->tokens);
    *token = (Token){
        .text = text,
        .kind = kind,
        .line = 0,
        .col = 0,
    };

    skipWhiteSpace(p);
    assert(p->input[0] == ']');
    p->input++;
    return token;
}

static AstChild parseChild(AstParser *p) {
    if (p->input[0] == '(') {
        return (AstChild){
            .isToken = false,
            .node = parseNode(p),
        };
    } else {
        return (AstChild){
            .isToken = true,
            .token = parseToken(p),
        };
    }
}

ParseAstResult parseSExprAst(const char *input) {
    AstParser p = {
        .input = input,
        .tokens = TokenPool_init(16),
        .ast = (Ast){
            .pool = AstPool_init(16),
            .root = NULL,
        },
    };
    skipWhiteSpace(&p);
    p.ast.root = parseNode(&p);
    return (ParseAstResult){.ast = p.ast, .tokens = p.tokens};
}

typedef struct {
    cstr out;
    u32 currentIdent;
    const char *spaces;
} AstNodePrinter;

// static u32 calcTokenLen(const Token *token) {
//     if (token->kind < TOKEN_EQUAL) {
//         return 3 + TOKEN_KIND_STRS[token->kind].size + token->text.size;
//     } else {
//         return 2 + TOKEN_KIND_STRS[token->kind].size;
//     }
// }

// static u32 calcNodeLen(const AstNode *n) {
//     u32 len = 2 + AST_KIND_NAMES[n->kind].size;
//     c_foreach(it, AstChildren, n->children) {
//         if (it.ref->isToken)
//             len += calcTokenLen(it.ref->token) + 1;
//         else
//             len += calcNodeLen(it.ref->node) + 1;
//     }
//     return len;
// }

static void printToken(AstNodePrinter *p, const Token *token) {
    cstr_append(&p->out, "[");
    cstr_append(&p->out, TOKEN_KIND_NAMES[token->kind].str);
    if (token->kind < TOKEN_EQUAL) {
        cstr_append(&p->out, " ");
        cstr_append_sv(&p->out, token->text);
    }
    cstr_append(&p->out, "]");
}

static void printNode(AstNodePrinter *p, AstNode *n) {
    cstr_append(&p->out, "(");
    cstr_append_sv(&p->out, AST_KIND_NAMES[n->kind]);
    // i32 len = calcNodeLen(n);
    i32 childrenCount = AstChildren_size(&n->children);
    AstChild *data = AstChildren_data(&n->children);
    b32 isMultiline = childrenCount > 1 ||
                      (childrenCount == 1 && !data[0].isToken); // len + p->currentIdent > 80;
    if (isMultiline)
        p->currentIdent++;

    c_foreach(it, AstChildren, n->children) {
        if (isMultiline) {
            cstr_append(&p->out, "\n");
            c_forrange(p->currentIdent)
                cstr_append(&p->out, p->spaces);
        } else {
            cstr_append(&p->out, " ");
        }

        if (it.ref->isToken)
            printToken(p, it.ref->token);
        else
            printNode(p, it.ref->node);
    }

    if (isMultiline) {
        p->currentIdent--;
        cstr_append(&p->out, "\n");
        c_forrange(p->currentIdent)
            cstr_append(&p->out, p->spaces);
    }
    cstr_append(&p->out, ")");
}

cstr printSExprAst(const AstNode *n) {
    AstNodePrinter p = {
        .out = cstr_init(),
        .currentIdent = 0,
        .spaces = "  ",
    };
    printNode(&p, (AstNode *)n);
    return p.out;
}