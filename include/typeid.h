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

TypeId Type_simple(TypeMap *tm, TypeKind kind);
TypeId Type_named(TypeMap *tm, TypeKind kind, csview str);
TypeId Type_rec1(TypeMap *tm, TypeKind kind, TypeId arg);
TypeId Type_intern(TypeMap *tm, const Type *t);
const Type *Type_lookup(const TypeMap *tm, TypeId id);

TypeMap TypeTable_init(void);

cstr TypeId_print(const TypeMap *tm, TypeId id);

#define TYPEID_UNKNOWN ((TypeId)TYPE_UNKNOWN)
#define TYPEID_ERROR ((TypeId)TYPE_ERROR)
#define TYPEID_NONE ((TypeId)TYPE_NONE)

#endif