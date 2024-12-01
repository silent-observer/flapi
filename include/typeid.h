#ifndef TYPEID_H
#define TYPEID_H

#include "memcxt.h"

#include <stc/cstr.h>

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
    TYPE_GENERIC,
    TYPE_GENERIC_PARAM,
    TYPE_TUPLE,

    TYPE_FN_GLOBAL,
    TYPE_FN_ANY,
    TYPE_FN_ONCE,
    TYPE_FN_ONCE_OR_MORE,
    TYPE_FN_ONCE_OR_ZERO,
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

TypeId TypeId_build(const Type *t);
TypeId TypeId_simple(TypeKind kind);
TypeId TypeId_named(TypeKind kind, csview str);
TypeId TypeId_rec1(TypeKind kind, TypeId arg);
TypeId TypeId_intern(const Type *t);
const Type *TypeId_lookup(TypeId id);
void TypeTable_init(void);
void TypeTable_drop(void);

cstr TypeId_print(TypeId id);

#endif