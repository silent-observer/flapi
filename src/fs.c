#include "memcxt.h"

#include "fs.h"

cstr readTextFile(const char *path) {
    FILE *f = fopen(path, "rb");
    if (f == NULL)
        return cstr_null;

    int r = fseek(f, 0, SEEK_END);
    if (r != 0) {
        fclose(f);
        return cstr_null;
    }
    intptr_t len = ftell(f);

    r = fseek(f, 0, SEEK_SET);
    if (r != 0) {
        fclose(f);
        return cstr_null;
    }

    cstr s = cstr_with_size(len, '\0');

    size_t count = fread(cstr_data(&s), 1, len, f);
    if (count != len) {
        fclose(f);
        return cstr_null;
    }

    fclose(f);
    return s;
}

b32 writeTextFile(const char *path, csview s) {
    FILE *f = fopen(path, "wb");
    if (f == NULL)
        return false;

    size_t count = fwrite(s.buf, 1, s.size, f);
    if (count != s.size) {
        fclose(f);
        return false;
    }

    fclose(f);
    return true;
}