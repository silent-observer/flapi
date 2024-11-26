#include "typeid.h"

static TypeMap typeMap = {0};

TypeId TypeId_simple(TypeKind kind) {
    return c_hash_n(&kind, sizeof(kind));
}

TypeId TypeId_named(TypeKind kind, csview str) {
    u64 h = TypeId_simple(kind);
    return c_hash_mix(h, c_hash_n(str.buf, str.size));
}
TypeId TypeId_rec1(TypeKind kind, TypeId arg) {
    u64 h = TypeId_simple(kind);
    u64 h2 = c_hash_n(&arg, sizeof(arg));
    return c_hash_mix(h, h2);
}

TypeId TypeId_build(const Type *t) {
    u64 h = TypeId_simple(t->kind);
    if (t->kind == TYPE_NAMED || t->kind == TYPE_GENERIC_PARAM) {
        u64 h2 = cstr_hash(&t->str);
        return c_hash_mix(h, h2);
    }
    c_foreach(it, TypeChildren, t->children) {
        u64 h2 = c_hash_n(it.ref, sizeof(*it.ref));
        h = c_hash_mix(h, h2);
    }
    return h;
}

TypeId TypeId_intern(const Type *t) {
    TypeId id = TypeId_build(t);
    TypeMap_insert(&typeMap, id, *t);
    return id;
}

Type *TypeId_lookup(TypeId id) {
    return &TypeMap_get(&typeMap, id)->second;
}

void TypeTable_init(void) {
    TypeMap_drop(&typeMap);
    typeMap = TypeMap_init();
    for (TypeKind kind = TYPE_UNKNOWN; kind < TYPE_NAMED; kind++) {
        TypeMap_insert(&typeMap, TypeId_simple(kind), (Type){.kind = kind});
    }
}