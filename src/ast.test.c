#include "ast.h"
#include "fs.h"
#include "test.h"
#include <stc/cstr.h>

static const char *AST_TESTS[] = {
    "while_counter",
    "fibonacci",
};

int ast_test(void) {
    static char filename[512];
    b32 ok = true;
    c_forrange(i, c_arraylen(AST_TESTS)) {
        pushMemCxt();
#define DEFER popMemCxt();

        const char *name = AST_TESTS[i];
        sprintf(filename, TEST_DIR "/ast/%s.ast", name);
        cstr astDump = readTextFile(filename);
        test_assert_defer(!cstr_empty(&astDump));

        ParseAstResult ast1 = parseSExprAst(cstr_str(&astDump));
        ParseAstResult ast2 = parseSExprAst(cstr_str(&astDump));
        if (!astMatches(&ast1.ast, &ast2.ast)) {
            fprintf(stderr, "AST parsing is not pure: %s\n", name);
            ok = false;
            continue;
        }

        cstr out1 = printSExprAst(ast1.ast.root);
        sprintf(filename, TEST_DIR "/ast_results/%s.ast", name);
        writeTextFile(filename, cstr_sv(&out1));
        if (!cstr_eq(&out1, &astDump)) {
            fprintf(stderr, "AST doesn't match: %s\n", name);
            ok = false;
        }

        DEFER
    }
    test_assert(ok);
    test_success();
}