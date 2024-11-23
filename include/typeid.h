#ifndef TYPEID_H
#define TYPEID_H

#include "types.h"
typedef u64 TypeId;

typedef enum {
    TYPE_UNKNOWN,
    TYPE_ERROR,
    TYPE_NONE,

    TYPE_I8,
    TYPE_I16,
    TYPE_I32,
    TYPE_I64,
    TYPE_U8,
    TYPE_U16,
    TYPE_U32,
    TYPE_U64,
    TYPE_STR,
    TYPE_CHAR,
    TYPE_BOOL,

    TYPE_NAMED,
    TYPE_GENERIC_PARAM,
    TYPE_TUPLE,
} TypeKind;

struct Type;

#define i_TYPE TypeChildren, TypeId, 2
#include "smallvec.h"

typedef struct Type {
    TypeKind kind;
    union {
        TypeChildren children;
        cstr str;
    };
} Type;

static inline u64 TypeId_hash(TypeId *id) { return *id; }

#define i_TYPE TypeMap, TypeId, Type
#define i_hash TypeId_hash
#include <stc/hmap.h>

TypeId hashType(const Type *t);
TypeId hashTypeSimple(TypeKind kind);
TypeId hashTypeNamed(TypeKind kind, csview str);
TypeId hashTypeRecursive1(TypeKind kind, TypeId arg);
TypeId saveType(const Type *t);
Type *getType(TypeId id);

#endif