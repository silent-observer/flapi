#ifndef FS_H
#define FS_H

#include "memcxt.h"

#include "types.h"
#include <stc/cstr.h>
#include <stc/csview.h>

#define i_type FilenameVec
#define i_key_str
#include <stc/vec.h>

cstr readTextFile(const char *path);
b32 writeTextFile(const char *path, csview s);
FilenameVec readDir(const char *path);
cstr replaceFilenameExt(const char *path, const char *with);

#endif