#ifndef TYPEID_H
#define TYPEID_H

#include "memcxt.h"

#include <stc/cstr.h>

#include "types.h"
typedef struct {
    u64 id;
} TypeId;

typedef enum {
    TYPE_UNKNOWN,
    TYPE_ERROR,
    TYPE_NONE,
    TYPE_DROP,

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
    TYPE_INTLIT,

    TYPE_TUPLE,

    TYPE_NAMED,
    TYPE_GENERIC,
    TYPE_GENERIC_PARAM,

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

#define i_TYPE TypeMap, TypeId, Type
#define i_hash(x) ((x)->id)
#define i_eq(a, b) ((a)->id == (b)->id)
#include <stc/hmap.h>

TypeId Type_named(TypeMap *tm, TypeKind kind, csview str);
TypeId Type_rec1(TypeMap *tm, TypeKind kind, TypeId arg);
TypeId Type_intern(TypeMap *tm, const Type *t);
const Type *Type_lookup(const TypeMap *tm, TypeId id);

TypeMap TypeTable_init(void);

cstr TypeId_print(const TypeMap *tm, TypeId id);

static inline TypeId Type_simple(TypeKind kind) {
    assert(kind != TYPE_NAMED && kind != TYPE_GENERIC_PARAM);
    return (TypeId){.id = kind};
}

static inline b32 Type_isKnown(TypeId t) {
    return t.id != Type_simple(TYPE_UNKNOWN).id;
}

static inline b32 Type_isInteger(TypeId t) {
    switch (t.id) {
        case TYPE_I8:
        case TYPE_I16:
        case TYPE_I32:
        case TYPE_I64:
        case TYPE_U8:
        case TYPE_U16:
        case TYPE_U32:
        case TYPE_U64:
        case TYPE_INTLIT:
            return true;
        default:
            return false;
    }
}

static inline b32 Type_isSignedInteger(TypeId t) {
    switch (t.id) {
        case TYPE_I8:
        case TYPE_I16:
        case TYPE_I32:
        case TYPE_I64:
        case TYPE_INTLIT:
            return true;
        default:
            return false;
    }
}

static inline b32 TypeKind_isFunc(TypeKind t) {
    switch (t) {
        case TYPE_FN_GLOBAL:
        case TYPE_FN_ANY:
        case TYPE_FN_ONCE:
        case TYPE_FN_ONCE_OR_MORE:
        case TYPE_FN_ONCE_OR_ZERO:
            return true;
        default:
            return false;
    }
}

#endif