#include "typeinfer.h"
#include "ast.h"
#include "asttrans.h"
#include "cst.h"
#include "fs.h"
#include "lexer.h"
#include "parser.h"
#include "test.h"
#include <stc/cstr.h>

int typeinfer_subtest(const char *name) {
    cstr filename = cstr_init();
    cstr_printf(&filename, TEST_DIR "/code_input/%s.fj", name);
    cstr code = readTextFile(cstr_str(&filename));
    subtest_assert(!cstr_empty(&code), name);

    cstr_printf(&filename, TEST_DIR "/typeinfer/expected/%s.ast", name);
    cstr expected = readTextFile(cstr_str(&filename));

    TokenVec tokens = lex(cstr_zv(&code));
    ParseResult parseResult = parse(tokens.data, TokenVec_size(&tokens));
    AstTransformResult astResult = astFromCst(&parseResult.cst);
    TypingErrorVec typingErrors = typeinfer(&astResult.ast);

    cstr astDump = printAst(&astResult.ast, true);
    if (TypingErrorVec_size(&typingErrors) > 0) {
        cstr_append(&astDump, "\n\n------ ERRORS ------\n");
        c_foreach(it, TypingErrorVec, typingErrors) {
            cstr err = printTypingError(it.ref, &astResult.ast.types, &astResult.ast.symbols);
            cstr_append_s(&astDump, err);
            cstr_drop(&err);
        }
    }

    cstr_printf(&filename, TEST_DIR "/typeinfer/results/%s.ast", name);
    writeTextFile(cstr_str(&filename), cstr_sv(&astDump));
    if (!cstr_eq(&expected, &astDump))
        subtest_fail("Typed AST doesn't match", name);

    subtest_success(name);
}

int typeinfer_test(void) {
    test_foreach_file(typeinfer_subtest, TEST_DIR "/typeinfer/list.txt");
}