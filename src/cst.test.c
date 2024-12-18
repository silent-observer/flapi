#include "cst.h"
#include "fs.h"
#include "test.h"
#include <stc/cstr.h>

static const char *CST_TESTS[] = {
    "while_counter",
    "fibonacci",
    "broken_1",
    "broken_2",
    "broken_3",
    "broken_4",
};

int cst_test(void) {
    static char filename[512];
    b32 ok = true;
    c_forrange(i, c_arraylen(CST_TESTS)) {
        pushMemCxt();
#define DEFER popMemCxt();

        const char *name = CST_TESTS[i];
        sprintf(filename, TEST_DIR "/cst/%s.cst", name);
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
        sprintf(filename, TEST_DIR "/cst_results/%s.cst", name);
        writeTextFile(filename, cstr_sv(&out1));

        // Remove errors
        isize pos = cstr_find(&cstDump, "\n\n------ ERRORS ------\n");
        csview expected = pos == c_NPOS
                              ? cstr_sv(&cstDump)
                              : csview_slice(cstr_sv(&cstDump), 0, pos);

        if (!cstr_equals_sv(&out1, expected)) {
            fprintf(stderr, "CST doesn't match: %s\n", name);
            ok = false;
        }

        DEFER
    }
    test_assert(ok);
    test_success();
}