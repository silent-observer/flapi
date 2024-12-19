#include "customtype.h"

static TypeId getBaseType(TypeMap *types, TypeId id) {
    const Type *t = Type_lookup(types, id);
    if (t->kind == TYPE_GENERIC) {
        return *TypeChildren_at(&t->children, 0);
    }
    return id;
}

static TypeId substituteTypeGenericParam(TypeMap *types,
                                         TypeId genericBase,
                                         TypeChildren *genericParams,
                                         TypeId concrete,
                                         TypeId id) {
    const Type *concreteT = Type_lookup(types, concrete);
    const Type *t = Type_lookup(types, id);

    assert(concreteT->kind == TYPE_GENERIC);
    assert(t->kind == TYPE_GENERIC_PARAM);
    TypeId concreteBase = *TypeChildren_at(&concreteT->children, 0);
    assert(genericBase.id == concreteBase.id);
    assert(TypeChildren_size(&concreteT->children) ==
           TypeChildren_size(genericParams));

    c_forrange(i, 1, TypeChildren_size(genericParams)) {
        TypeId param = *TypeChildren_at(genericParams, i);
        TypeId concreteParam = *TypeChildren_at(&concreteT->children, i);
        if (param.id == id.id) {
            return concreteParam;
        }
    }
    return Type_simple(TYPE_ERROR);
}

static TypeId substituteType(TypeMap *types,
                             TypeId genericBase,
                             TypeChildren *genericParams,
                             TypeId concrete,
                             TypeId id) {
    const Type *t = Type_lookup(types, id);
    if (t->kind == TYPE_GENERIC_PARAM)
        return substituteTypeGenericParam(types, genericBase, genericParams, concrete, id);
    if (TypeChildren_size(&t->children) == 0)
        return id;

    Type new = *t;
    b32 hasNew = false;
    c_foreach(it, TypeChildren, new.children) {
        TypeId newChild = substituteType(types, genericBase, genericParams, concrete, *it.ref);
        if (newChild.id != it.ref->id) {
            hasNew = true;
            *it.ref = newChild;
        }
    }

    if (hasNew) {
        TypeId newId = Type_intern(types, &new);
        return newId;
    } else {
        return id;
    }
}

void CustomTypeTable_add(CustomTypeTable *table,
                         TypeMap *types,
                         const CustomTypeEntry *entry) {
    CustomTypeMap_insert(&table->map, entry->baseType, *entry);
}

CustomTypeEntry *CustomTypeTable_find(CustomTypeTable *table,
                                      TypeMap *types,
                                      TypeId id) {
    TypeId baseId = getBaseType(types, id);
    return &CustomTypeMap_get_mut(&table->map, baseId)->second;
}

TypeId CustomTypeTable_getFieldType(CustomTypeTable *table,
                                    TypeMap *types,
                                    TypeId id,
                                    SymbolId field) {
    TypeId baseId = getBaseType(types, id);
    const CustomTypeEntry *entry = &CustomTypeMap_get(&table->map, baseId)->second;
    c_foreach(it, CustomTypeElementVec, entry->elements) {
        if (it.ref->name == field) {
            return substituteType(types, entry->baseType, &entry->typeParams, id, it.ref->type);
        }
    }
    return Type_simple(TYPE_ERROR);
}

cstr CustomTypeTable_print(CustomTypeTable *table, TypeMap *types, SymbolTable *symbols) {
    cstr out = cstr_init();
    cstr_append(&out, "CustomTypeTable {\n");
    c_foreach(it, CustomTypeMap, table->map) {
        CustomTypeEntry *entry = &it.ref->second;
        cstr baseType = TypeId_print(types, entry->baseType);
        switch (entry->kind) {
            case CUSTOM_TYPE_STRUCT:
                cstr_append(&out, "  struct ");
                break;
            case CUSTOM_TYPE_ANYOF:
                cstr_append(&out, "  anyof ");
                break;
        }
        cstr_append_s(&out, baseType);
        cstr_drop(&baseType);

        cstr_append(&out, " {");
        c_foreach(it2, CustomTypeElementVec, entry->elements) {
            CustomTypeElement *el = it2.ref;
            cstr elementType = TypeId_print(types, el->type);
            Symbol *s = SymbolTable_lookup(symbols, el->name);
            cstr_append_fmt(&out, "\n    %.*s : %s,", c_SV(s->name), cstr_str(&elementType));
            cstr_drop(&elementType);
        }
        cstr_append(&out, "\n  }\n");
    }
    cstr_append(&out, "}");
    return out;
}