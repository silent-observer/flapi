#include "typeid.h"
#include <stc/cstr.h>

static TypeMap typeMap = {0};

TypeId hashTypeSimple(TypeKind kind) {
    return c_hash_n(&kind, sizeof(kind));
}

TypeId hashTypeNamed(TypeKind kind, csview str) {
    u64 h = hashTypeSimple(kind);
    return c_hash_mix(h, c_hash_n(str.buf, str.size));
}
TypeId hashTypeRecursive1(TypeKind kind, TypeId arg) {
    u64 h = hashTypeSimple(kind);
    u64 h2 = c_hash_n(&arg, sizeof(arg));
    return c_hash_mix(h, h2);
}

TypeId hashType(const Type *t) {
    u64 h = hashTypeSimple(t->kind);
    if (t->kind == TYPE_NAMED || t->kind == TYPE_GENERIC_PARAM) {
        return c_hash_mix(h, cstr_hash(&t->str));
    }
    c_foreach(it, TypeChildren, t->children) {
        u64 h2 = c_hash_n(it.ref, sizeof(*it.ref));
        h = c_hash_mix(h, h2);
    }
    return h;
}

TypeId saveType(const Type *t) {
    TypeId id = hashType(t);
    TypeMap_insert(&typeMap, id, *t);
    return id;
}

Type *getType(TypeId id) {
    return TypeMap_get(&typeMap, id);
}