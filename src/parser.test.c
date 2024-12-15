#include "parser.h"
#include "cst.h"
#include "fs.h"
#include "lexer.h"
#include "test.h"
#include <stc/cstr.h>

int parser_test(void) {
    FilenameVec names = readDir(TEST_DIR "/code_input");

    static char filename[512];
    b32 ok = true;
    c_foreach(it, FilenameVec, names) {
        pushMemCxt();
#define DEFER popMemCxt();

        const char *name = cstr_str(it.ref);
        sprintf(filename, TEST_DIR "/code_input/%s", name);
        cstr code = readTextFile(filename);
        test_assert_defer(!cstr_empty(&code));

        cstr nameCst = replaceFilenameExt(name, "cst");
        sprintf(filename, TEST_DIR "/parse/expected/%s", cstr_str(&nameCst));
        cstr expected = readTextFile(filename);

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

        sprintf(filename, TEST_DIR "/parse/results/%s", cstr_str(&nameCst));
        writeTextFile(filename, cstr_sv(&cstDump));
        if (!cstr_eq(&expected, &cstDump)) {
            fprintf(stderr, "CST doesn't match: %s\n", name);
            ok = false;
        }

        DEFER
    }
    FilenameVec_drop(&names);
    test_assert(ok);
    test_success();
}