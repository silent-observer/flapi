#ifndef SYMBOLS_H
#define SYMBOLS_H

#include "memcxt.h"

#include "typeid.h"
#include "types.h"

#include <stc/csview.h>

#define NO_SYMBOL_ID UINT32_MAX
typedef u32 SymbolId;
typedef u32 ScopeId;
typedef struct {
    SymbolId id;
    ScopeId scope;
    csview name;
    TypeId type;
} Symbol;

#define i_type SymbolMap
#define i_key_ssv
#define i_val SymbolId
#include <stc/smap.h>

typedef struct {
    ScopeId id;
    ScopeId parent;
    SymbolMap map;
} Scope;

#define i_TYPE SymbolVec, Symbol
#include <stc/vec.h>

#define i_TYPE ScopeVec, Scope
#include <stc/vec.h>

typedef struct {
    ScopeVec scopes;
    SymbolVec symbols;
} SymbolTable;

SymbolTable SymbolTable_init();
ScopeId SymbolTable_scope(SymbolTable *table, ScopeId parent);
SymbolId SymbolTable_add(SymbolTable *table, ScopeId scope, csview name);
SymbolId SymbolTable_find(SymbolTable *table, ScopeId scope, csview name);
Symbol *SymbolTable_lookup(SymbolTable *table, SymbolId id);

#endif