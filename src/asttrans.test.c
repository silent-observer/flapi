#include "asttrans.h"
#include "ast.h"
#include "cst.h"
#include "fs.h"
#include "lexer.h"
#include "parser.h"
#include "test.h"
#include <stc/cstr.h>

static const char *AST_TESTS[] = {
    "while_counter",
    "fibonacci",
    "broken_1",
    "broken_2",
    "broken_3",
    "broken_4",
};

int asttrans_test(void) {
    static char filename[512];
    b32 ok = true;
    c_forrange(i, c_arraylen(AST_TESTS)) {
        pushMemCxt();
#define DEFER popMemCxt();

        const char *name = AST_TESTS[i];
        sprintf(filename, TEST_DIR "/code/%s.fj", name);
        cstr code = readTextFile(filename);
        test_assert_defer(!cstr_empty(&code));

        sprintf(filename, TEST_DIR "/ast/%s.ast", name);
        cstr expected = readTextFile(filename);

        TokenVec tokens = lex(cstr_zv(&code));
        ParseResult parseResult = parse(tokens.data, TokenVec_size(&tokens));
        AstTransformResult astResult = astFromCst(&parseResult.cst);

        cstr astDump = printAst(&astResult.ast);
        if (AstTransformErrorVec_size(&astResult.errors) > 0) {
            cstr_append(&astDump, "\n\n------ ERRORS ------\n");
            c_foreach(it, AstTransformErrorVec, astResult.errors) {
                cstr_append_fmt(&astDump, "Syntax error at (%d:%d): %s\n",
                                it.ref->span.start.line,
                                it.ref->span.start.col,
                                it.ref->msg.str);
            }
        }

        sprintf(filename, TEST_DIR "/ast_results/%s.ast", name);
        writeTextFile(filename, cstr_sv(&astDump));
        if (!cstr_eq(&expected, &astDump)) {
            fprintf(stderr, "AST doesn't match: %s\n", name);
            ok = false;
        }

        DEFER
    }
    test_assert(ok);
    test_success();
}