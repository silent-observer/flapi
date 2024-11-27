#include "cst.h"
#include <stc/cstr.h>
#include <stdio.h>

static inline b32 cstChildMatches(CstChild *lhs, CstChild *rhs);

static inline b32 cstNodeMatches(CstNode *lhs, CstNode *rhs) {
    if (lhs->kind != rhs->kind)
        return false;
    if (CstChildren_size(&lhs->children) != CstChildren_size(&rhs->children))
        return false;
    CstChild *lhs_children = CstChildren_data(&lhs->children);
    CstChild *rhs_children = CstChildren_data(&rhs->children);
    for (u32 i = 0; i < CstChildren_size(&lhs->children); i++)
        if (!cstChildMatches(&lhs_children[i], &rhs_children[i]))
            return false;
    return true;
}

static inline b32 cstChildMatches(CstChild *lhs, CstChild *rhs) {
    if (lhs->kind != rhs->kind)
        return false;

    if (lhs->kind == CST_CHILD_TOKEN)
        return lhs->token->kind == rhs->token->kind &&
               csview_eq(&lhs->token->text, &rhs->token->text);
    else if (lhs->kind == CST_CHILD_NODE)
        return cstNodeMatches(lhs->node, rhs->node);
    else
        return true;
}

b32 cstMatches(Cst *lhs, Cst *rhs) {
    return cstNodeMatches(lhs->root, rhs->root);
}

typedef struct {
    Cst cst;
    TokenPool tokens;
    const char *input;
} CstParser;

static inline usize countUntilEnd(const CstParser *p) {
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

static inline void skipWhiteSpace(CstParser *p) {
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

static CstNodeKind parseNodeKind(csview word) {
    for (u32 i = 0; i < c_arraylen(CST_KIND_NAMES); i++) {
        if (csview_eq(&CST_KIND_NAMES[i], &word)) {
            return (CstNodeKind)i;
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

static CstChild parseChild(CstParser *p);

static CstNode *parseNode(CstParser *p) {
    assert(p->input[0] == '(');
    p->input++;

    i32 wordLength = countUntilEnd(p);
    csview word = csview_from_n(p->input, wordLength);
    p->input += wordLength;

    CstNodeKind kind = parseNodeKind(word);
    CstNode *node = CstPool_new(&p->cst.pool);
    node->kind = kind;
    node->children = CstChildren_init();

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
        CstChildren_push(
            &node->children,
            parseChild(p));
    }
    return node;
}

static Token *parseToken(CstParser *p) {
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
        .src.line = 0,
        .src.col = 0,
    };

    skipWhiteSpace(p);
    assert(p->input[0] == ']');
    p->input++;
    return token;
}

static CstChild parseChild(CstParser *p) {
    if (p->input[0] == '_') {
        p->input++;
        return (CstChild){.kind = CST_CHILD_NONE};
    }

    if (p->input[0] == '(') {
        return (CstChild){
            .kind = CST_CHILD_NODE,
            .node = parseNode(p),
        };
    } else {
        return (CstChild){
            .kind = CST_CHILD_TOKEN,
            .token = parseToken(p),
        };
    }
}

ParseCstResult parseSExprCst(const char *input) {
    CstParser p = {
        .input = input,
        .tokens = TokenPool_init(16),
        .cst = (Cst){
            .pool = CstPool_init(16),
            .root = NULL,
        },
    };
    skipWhiteSpace(&p);
    p.cst.root = parseNode(&p);
    return (ParseCstResult){.cst = p.cst, .tokens = p.tokens};
}

typedef struct {
    cstr out;
    u32 currentIdent;
    const char *spaces;
} CstNodePrinter;

// static u32 calcTokenLen(const Token *token) {
//     if (token->kind < TOKEN_EQUAL) {
//         return 3 + TOKEN_KIND_STRS[token->kind].size + token->text.size;
//     } else {
//         return 2 + TOKEN_KIND_STRS[token->kind].size;
//     }
// }

// static u32 calcNodeLen(const CstNode *n) {
//     u32 len = 2 + CST_KIND_NAMES[n->kind].size;
//     c_foreach(it, CstChildren, n->children) {
//         if (it.ref->isToken)
//             len += calcTokenLen(it.ref->token) + 1;
//         else
//             len += calcNodeLen(it.ref->node) + 1;
//     }
//     return len;
// }

static void printToken(CstNodePrinter *p, const Token *token) {
    cstr_append(&p->out, "[");
    cstr_append(&p->out, TOKEN_KIND_NAMES[token->kind].str);
    if (token->kind < TOKEN_EQUAL) {
        cstr_append(&p->out, " ");
        cstr_append_sv(&p->out, token->text);
    }
    cstr_append(&p->out, "]");
}

static void printNode(CstNodePrinter *p, CstNode *n) {
    cstr_append(&p->out, "(");
    cstr_append_sv(&p->out, CST_KIND_NAMES[n->kind]);
    // i32 len = calcNodeLen(n);
    i32 childrenCount = CstChildren_size(&n->children);
    b32 isMultiline = childrenCount > 1 ||
                      (childrenCount == 1 &&
                       CstChildren_at(&n->children, 0)->kind == CST_CHILD_NODE); // len + p->currentIdent > 80;
    if (isMultiline)
        p->currentIdent++;

    if (childrenCount == 0) {
        cstr_append(&p->out, ")");
        return;
    }

    c_foreach(it, CstChildren, n->children) {
        if (isMultiline) {
            cstr_append(&p->out, "\n");
            c_forrange(p->currentIdent)
                cstr_append(&p->out, p->spaces);
        } else {
            cstr_append(&p->out, " ");
        }

        switch (it.ref->kind) {
            case CST_CHILD_NONE:
                cstr_append(&p->out, "_");
                break;
            case CST_CHILD_TOKEN:
                printToken(p, it.ref->token);
                break;
            case CST_CHILD_NODE:
                printNode(p, it.ref->node);
                break;
            default:
                assert(0);
        }
    }

    if (isMultiline) {
        p->currentIdent--;
        cstr_append(&p->out, "\n");
        c_forrange(p->currentIdent)
            cstr_append(&p->out, p->spaces);
    }
    cstr_append(&p->out, ")");
}

cstr printSExprCst(const CstNode *n) {
    CstNodePrinter p = {
        .out = cstr_init(),
        .currentIdent = 0,
        .spaces = "  ",
    };
    printNode(&p, (CstNode *)n);
    return p.out;
}