#ifndef CUSTOMTYPE_H
#define CUSTOMTYPE_H

#include "memcxt.h"

#include "symbols.h"
#include "typeid.h"

typedef struct {
    SymbolId name;
    TypeId type;
} CustomTypeElement;

#define i_TYPE CustomTypeElementVec, CustomTypeElement
#include <stc/vec.h>

typedef enum {
    CUSTOM_TYPE_STRUCT,
    CUSTOM_TYPE_ANYOF,
} CustomTypeKind;

typedef struct {
    CustomTypeKind kind;
    CustomTypeElementVec elements;
    TypeId baseType;
    TypeChildren typeParams;
} CustomTypeEntry;

#define i_type CustomTypeMap
#define i_key TypeId
#define i_val CustomTypeEntry
#define i_hash(x) ((x)->id)
#define i_eq(a, b) ((a)->id == (b)->id)
#include <stc/hmap.h>

typedef struct {
    CustomTypeMap map;
} CustomTypeTable;

void CustomTypeTable_add(CustomTypeTable *table,
                         TypeMap *types,
                         const CustomTypeEntry *entry);
CustomTypeEntry *CustomTypeTable_find(CustomTypeTable *table,
                                      TypeMap *types,
                                      TypeId id);
TypeId CustomTypeTable_getFieldType(CustomTypeTable *table,
                                    TypeMap *types,
                                    TypeId id,
                                    SymbolId field);
cstr CustomTypeTable_print(CustomTypeTable *table, TypeMap *types, SymbolTable *symbols);

#endif