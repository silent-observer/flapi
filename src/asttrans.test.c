#include "asttrans.h"
#include "ast.h"
#include "cst.h"
#include "fs.h"
#include "lexer.h"
#include "parser.h"
#include "test.h"
#include <stc/cstr.h>

static int asttrans_subtest(const char *name) {
    cstr filename = cstr_init();
    cstr_printf(&filename, TEST_DIR "/code_input/%s.fj", name);
    cstr code = readTextFile(cstr_str(&filename));
    subtest_assert(!cstr_empty(&code), name);

    cstr_printf(&filename, TEST_DIR "/asttrans/expected/%s.ast", name);
    cstr expected = readTextFile(cstr_str(&filename));

    TokenVec tokens = lex(cstr_zv(&code));
    ParseResult parseResult = parse(tokens.data, TokenVec_size(&tokens));
    AstTransformResult astResult = astFromCst(&parseResult.cst);

    cstr astDump = printAst(&astResult.ast, false);
    if (CustomTypeMap_size(&astResult.ast.customTypes.map) > 0) {
        cstr_append(&astDump, "\n\n------ CUSTOM TYPES ------\n");
        cstr_append_s(&astDump,
                      CustomTypeTable_print(
                          &astResult.ast.customTypes,
                          &astResult.ast.types,
                          &astResult.ast.symbols));
    }
    if (AstTransformErrorVec_size(&astResult.errors) > 0) {
        cstr_append(&astDump, "\n\n------ ERRORS ------\n");
        c_foreach(it, AstTransformErrorVec, astResult.errors) {
            if (it.ref->span.start.line == 0 && it.ref->span.start.col == 0) {
                cstr_append_fmt(&astDump, "Syntax error at (%d:%d): %s, first definition at (%d:%d)\n",
                                it.ref->span.start.line,
                                it.ref->span.start.col,
                                it.ref->msg.str,
                                it.ref->firstDefinition.start.line,
                                it.ref->firstDefinition.start.col);
            } else {
                cstr_append_fmt(&astDump, "Syntax error at (%d:%d): %s\n",
                                it.ref->span.start.line,
                                it.ref->span.start.col,
                                it.ref->msg.str);
            }
        }
    }

    cstr_printf(&filename, TEST_DIR "/asttrans/results/%s.ast", name);
    writeTextFile(cstr_str(&filename), cstr_sv(&astDump));
    if (!cstr_eq(&expected, &astDump))
        subtest_fail("AST doesn't match", name);
    subtest_success(name);
}

int asttrans_test(void) {
    test_foreach_file(asttrans_subtest, TEST_DIR "/asttrans/list.txt");
}