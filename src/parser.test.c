#include "parser.h"
#include "ast.h"
#include "fs.h"
#include "lexer.h"
#include "test.h"
#include <stc/cstr.h>

static const char *PARSER_TESTS[] = {
    "while_counter",
    "fibonacci",
};

int parser_test(void) {
    static char filename[512];
    b32 ok = true;
    c_forrange(i, c_arraylen(PARSER_TESTS)) {
        pushMemCxt();
#define DEFER popMemCxt();

        const char *name = PARSER_TESTS[i];
        sprintf(filename, TEST_DIR "/code/%s.fj", name);
        cstr code = readTextFile(filename);
        test_assert_defer(!cstr_empty(&code));

        sprintf(filename, TEST_DIR "/ast/%s.ast", name);
        cstr expected = readTextFile(filename);

        TokenVec tokens = lex(cstr_zv(&code));
        ParseResult parseResult = parse(tokens.data, TokenVec_size(&tokens));

        cstr astDump = printSExprAst(parseResult.ast.root);
        if (ParserErrorVec_size(&parseResult.errors) > 0) {
            cstr_append(&astDump, "\n\n------ ERRORS ------\n");
            c_foreach(it, ParserErrorVec, parseResult.errors) {
                cstr err = printParserError(it.ref);
                cstr_append_sv(&astDump, cstr_sv(&err));
                cstr_append(&astDump, "\n");
                cstr_drop(&err);
            }
        }

        sprintf(filename, TEST_DIR "/code_results/%s.ast", name);
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