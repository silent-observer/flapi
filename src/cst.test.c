#include "cst.h"
#include "fs.h"
#include "test.h"
#include <stc/cstr.h>

int cst_test(void) {
    FilenameVec names = readDir(TEST_DIR "/cst_dump/input");

    static char filename[512];
    b32 ok = true;
    c_foreach(it, FilenameVec, names) {
        pushMemCxt();
#define DEFER popMemCxt();

        const char *name = cstr_str(it.ref);
        sprintf(filename, TEST_DIR "/cst_dump/input/%s", name);
        cstr cstDump = readTextFile(filename);
        test_assert_defer(!cstr_empty(&cstDump));

        ParseCstResult cst1 = parseSExprCst(cstr_str(&cstDump));
        ParseCstResult cst2 = parseSExprCst(cstr_str(&cstDump));
        if (!cstMatches(&cst1.cst, &cst2.cst)) {
            fprintf(stderr, "CST parsing is not pure: %s\n", name);
            ok = false;
            continue;
        }

        cstr out1 = printSExprCst(cst1.cst.root);
        sprintf(filename, TEST_DIR "/cst_dump/results/%s", name);
        writeTextFile(filename, cstr_sv(&out1));
        if (!cstr_equals_s(&out1, cstDump)) {
            fprintf(stderr, "CST doesn't match: %s\n", name);
            ok = false;
        }

        DEFER
    }
    FilenameVec_drop(&names);
    test_assert(ok);
    test_success();
}