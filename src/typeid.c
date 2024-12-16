#include "typeid.h"

TypeId Type_named(TypeMap *tm, TypeKind kind, csview str) {
    Type t = {.kind = kind, .str = cstr_from_sv(str)};
    return Type_intern(tm, &t);
}
TypeId Type_rec1(TypeMap *tm, TypeKind kind, TypeId arg) {
    Type t = {.kind = kind};
    TypeChildren_push(&t.children, arg);
    return Type_intern(tm, &t);
}

static TypeId TypeId_build(const Type *t) {
    if (t->kind <= TYPE_TUPLE && TypeChildren_size(&t->children) == 0)
        return (TypeId){.id = t->kind};

    u64 h = c_hash_n(&t->kind, sizeof(t->kind));
    if (t->kind == TYPE_NAMED || t->kind == TYPE_GENERIC_PARAM) {
        u64 h2 = cstr_hash(&t->str);
        return (TypeId){.id = c_hash_mix(h, h2)};
    }
    c_foreach(it, TypeChildren, t->children) {
        u64 h2 = c_hash_n(it.ref, sizeof(*it.ref)) + 42;
        h = c_hash_mix(h, h2);
    }
    return (TypeId){.id = h};
}

TypeId Type_intern(TypeMap *tm, const Type *t) {
    TypeId id = TypeId_build(t);
    TypeMap_insert(tm, id, *t);
    return id;
}

const Type *Type_lookup(const TypeMap *tm, TypeId id) {
    const TypeMap_value *result = TypeMap_get(tm, id);
    assert(result);
    return &result->second;
}

TypeMap TypeTable_init(void) {
    TypeMap tm = TypeMap_init();
    for (TypeKind tk = TYPE_UNKNOWN; tk <= TYPE_TUPLE; tk++) {
        Type t = {.kind = tk};
        Type_intern(&tm, &t);
    }
    return tm;
}

static const char *const typeNames[] = {
    "<unknown>", "<error>", "<none>", "<drop>",
    "i8", "i16", "i32", "i64",
    "u8", "u16", "u32", "u64",
    "str", "char", "bool", "<intlit>"};

static void printType(const TypeMap *tm, cstr *out, TypeId id) {
    const Type *t = Type_lookup(tm, id);
    switch (t->kind) {
        case TYPE_UNKNOWN:
        case TYPE_ERROR:
        case TYPE_NONE:
        case TYPE_DROP:
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
        case TYPE_INTLIT:
            cstr_append(out, typeNames[t->kind]);
            break;

        case TYPE_NAMED:
            cstr_append_sv(out, cstr_sv(&t->str));
            break;
        case TYPE_GENERIC:
            printType(tm, out, *TypeChildren_at(&t->children, 0));
            cstr_append(out, "[");
            c_forrange(i, 1, TypeChildren_size(&t->children)) {
                if (i != 1)
                    cstr_append(out, ", ");
                printType(tm, out, *TypeChildren_at(&t->children, i));
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
                printType(tm, out, *TypeChildren_at(&t->children, i));
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
                    cstr_append(out, "fn");
                    break;
                case TYPE_FN_ANY:
                    cstr_append(out, "Fn");
                    break;
                case TYPE_FN_ONCE:
                    cstr_append(out, "Fn[1]");
                    break;
                case TYPE_FN_ONCE_OR_ZERO:
                    cstr_append(out, "Fn[1+]");
                    break;
                case TYPE_FN_ONCE_OR_MORE:
                    cstr_append(out, "Fn[1?]");
                    break;
                default:
                    assert(0);
            }
            printType(tm, out, *TypeChildren_at(&t->children, 0));
            cstr_append(out, " -> ");
            printType(tm, out, *TypeChildren_at(&t->children, 1));
            break;
        default:
            assert(0);
    }
}

cstr TypeId_print(const TypeMap *tm, TypeId id) {
    cstr out = cstr_init();
    printType(tm, &out, id);
    return out;
}