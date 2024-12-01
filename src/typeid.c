#include "typeid.h"

static TypeMap typeMap = {0};
static MemCxt typeMapCxt = NULL;

TypeId TypeId_simple(TypeKind kind) {
    Type t = {.kind = kind};
    return TypeId_intern(&t);
}

TypeId TypeId_named(TypeKind kind, csview str) {
    Type t = {.kind = kind, .str = cstr_from_sv(str)};
    return TypeId_intern(&t);
}
TypeId TypeId_rec1(TypeKind kind, TypeId arg) {
    Type t = {.kind = kind};
    TypeChildren_push(&t.children, arg);
    return TypeId_intern(&t);
}

TypeId TypeId_build(const Type *t) {
    u64 h = c_hash_n(&t->kind, sizeof(t->kind));
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
    MemCxt old = switchMemCxt(typeMapCxt);
    TypeMap_insert(&typeMap, id, *t);
    switchMemCxt(old);
    return id;
}

const Type *TypeId_lookup(TypeId id) {
    TypeMap_value *result = TypeMap_get(&typeMap, id);
    assert(result);
    return &result->second;
}

void TypeTable_init(void) {
    if (!typeMapCxt)
        typeMapCxt = newMemCxt();

    MemCxt old = switchMemCxt(typeMapCxt);
    TypeMap_drop(&typeMap);
    typeMap = TypeMap_init();
    for (TypeKind kind = TYPE_UNKNOWN; kind < TYPE_NAMED; kind++) {
        TypeMap_insert(&typeMap, TypeId_simple(kind), (Type){.kind = kind});
    }
    switchMemCxt(old);
}

static const char *const typeNames[] = {
    "<unknown>", "<error>", "<none>",
    "i8", "i16", "i32", "i64",
    "u8", "u16", "u32", "u64",
    "str", "char", "bool"};

static void printType(cstr *out, TypeId id) {
    const Type *t = TypeId_lookup(id);
    switch (t->kind) {
        case TYPE_UNKNOWN:
        case TYPE_ERROR:
        case TYPE_NONE:
        case TYPE_I8:
        case TYPE_I16:
        case TYPE_I32:
        case TYPE_I64:
        case TYPE_U8:
        case TYPE_U16:
        case TYPE_U32:
        case TYPE_U64:
        case TYPE_STR:
        case TYPE_CHAR:
        case TYPE_BOOL:
            cstr_append(out, typeNames[t->kind]);
            break;

        case TYPE_NAMED:
            cstr_append_sv(out, cstr_sv(&t->str));
            break;
        case TYPE_GENERIC:
            printType(out, *TypeChildren_at(&t->children, 0));
            cstr_append(out, "[");
            c_forrange(i, 1, TypeChildren_size(&t->children)) {
                if (i != 1)
                    cstr_append(out, ", ");
                printType(out, *TypeChildren_at(&t->children, i));
            }
            cstr_append(out, "]");
            break;
        case TYPE_GENERIC_PARAM:
            cstr_append(out, "%");
            cstr_append_sv(out, cstr_sv(&t->str));
            break;
        case TYPE_TUPLE:
            cstr_append(out, "(");
            c_forrange(i, 0, TypeChildren_size(&t->children)) {
                if (i != 0)
                    cstr_append(out, ", ");
                printType(out, *TypeChildren_at(&t->children, i));
            }
            cstr_append(out, ")");
            break;
        case TYPE_FN_GLOBAL:
        case TYPE_FN_ANY:
        case TYPE_FN_ONCE:
        case TYPE_FN_ONCE_OR_ZERO:
        case TYPE_FN_ONCE_OR_MORE:
            switch (t->kind) {
                case TYPE_FN_GLOBAL:
                    cstr_append(out, "def");
                    break;
                case TYPE_FN_ANY:
                    cstr_append(out, "fn");
                    break;
                case TYPE_FN_ONCE:
                    cstr_append(out, "fn[1]");
                    break;
                case TYPE_FN_ONCE_OR_ZERO:
                    cstr_append(out, "fn[1+]");
                    break;
                case TYPE_FN_ONCE_OR_MORE:
                    cstr_append(out, "fn[1?]");
                    break;
                default:
                    assert(0);
            }
            printType(out, *TypeChildren_at(&t->children, 0));
            cstr_append(out, " -> ");
            printType(out, *TypeChildren_at(&t->children, 1));
            break;
        default:
            assert(0);
    }
}

cstr TypeId_print(TypeId id) {
    cstr out = cstr_init();
    printType(&out, id);
    return out;
}