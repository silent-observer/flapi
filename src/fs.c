#include "memcxt.h"

#include "fs.h"

#ifdef __unix__
#define __USE_MISC
#include <dirent.h>
#else
#error "OS not supported yet"
#endif

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

#ifdef __unix__
FilenameVec readDir(const char *path) {
    FilenameVec files = {0};
    DIR *dir = opendir(path);
    if (dir == NULL)
        return files;

    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        if ((entry->d_type == DT_REG || entry->d_type == DT_LNK) && entry->d_name[0] != '.')
            FilenameVec_push(&files, cstr_from(entry->d_name));
    }
    closedir(dir);
    return files;
}
#else
#error "OS not supported yet"
#endif

cstr replaceFilenameExt(const char *path, const char *with) {
    const char *dot = strrchr(path, '.');
    if (dot == NULL)
        return cstr_from(path);
    else {
        csview start = csview_from_n(path, dot - path);
        return cstr_from_fmt("%.*s.%s", c_SV(start), with);
    }
}