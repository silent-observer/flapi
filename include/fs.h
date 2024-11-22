#ifndef FS_H
#define FS_H

#include "types.h"
#include <stc/cstr.h>
#include <stc/csview.h>

cstr readTextFile(const char *path);
b32 writeTextFile(const char *path, csview s);

#endif