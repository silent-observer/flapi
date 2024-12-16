#include "typeinfer.h"
#include "ast.h"
#include "asttrans.h"
#include "cst.h"
#include "fs.h"
#include "lexer.h"
#include "parser.h"
#include "test.h"
#include <stc/cstr.h>

int typeinfer_test(void) {
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

        cstr nameAst = replaceFilenameExt(name, "ast");
        sprintf(filename, TEST_DIR "/typeinfer/expected/%s", cstr_str(&nameAst));
        cstr expected = readTextFile(filename);

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

        sprintf(filename, TEST_DIR "/typeinfer/results/%s", cstr_str(&nameAst));
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