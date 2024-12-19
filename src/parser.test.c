#include "parser.h"
#include "cst.h"
#include "fs.h"
#include "lexer.h"
#include "test.h"
#include <stc/cstr.h>

static int parser_subtest(const char *name) {
    cstr filename = cstr_init();
    cstr_printf(&filename, TEST_DIR "/code_input/%s.fj", name);
    cstr code = readTextFile(cstr_str(&filename));
    subtest_assert(!cstr_empty(&code), name);

    cstr_printf(&filename, TEST_DIR "/parse/expected/%s.cst", name);
    cstr expected = readTextFile(cstr_str(&filename));

    TokenVec tokens = lex(cstr_zv(&code));
    ParseResult parseResult = parse(tokens.data, TokenVec_size(&tokens));

    cstr cstDump = printSExprCst(parseResult.cst.root);
    if (ParserErrorVec_size(&parseResult.errors) > 0) {
        cstr_append(&cstDump, "\n\n------ ERRORS ------\n");
        c_foreach(it, ParserErrorVec, parseResult.errors) {
            cstr err = printParserError(it.ref);
            cstr_append_sv(&cstDump, cstr_sv(&err));
            cstr_drop(&err);
        }
    }

    cstr_printf(&filename, TEST_DIR "/parse/results/%s.cst", name);
    writeTextFile(cstr_str(&filename), cstr_sv(&cstDump));
    if (!cstr_eq(&expected, &cstDump))
        subtest_fail("Parse result doesn't match", name);

    subtest_success(name);
}

int parser_test(void) {
    test_foreach_file(parser_subtest, TEST_DIR "/parse/list.txt");
}