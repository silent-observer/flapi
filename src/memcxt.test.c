#include "memcxt.h"
#include "test.h"

#define i_TYPE IntVec, i32
#include <stc/vec.h>

int memcxt_test() {
    pushMemCxt();

    for (int i = 0; i < 10; i++) {
        MemCxt outer = pushMemCxt();
        IntVec v1 = {0};
        for (int j = 0; j < 10; j++) {
            pushMemCxt();
            IntVec v2 = {0};
            for (int k = 0; k < 10; k++) {
                c_with(MemCxt old = switchMemCxt(outer), switchMemCxt(old)) {
                    IntVec_push(&v1, k);
                }
                IntVec_push(&v2, k);
            }
            for (int k = 0; k < 10; k++) {
                test_assert_cleanup(v2.data[k] == k, popMemCxt(); popMemCxt(); popMemCxt(););
            }
            popMemCxt();
        }
        popMemCxt();
    }

    popMemCxt();
    test_success();
}