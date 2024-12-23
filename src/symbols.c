#include "symbols.h"

SymbolTable SymbolTable_init() {
    SymbolTable table = {0};

    Scope scope = {
        .id = 0,
        .parent = 0,
        .map = SymbolMap_init(),
    };
    ScopeVec_push(&table.scopes, scope);

    return table;
}

ScopeId SymbolTable_scope(SymbolTable *table, ScopeId parent) {
    ScopeId id = ScopeVec_size(&table->scopes);
    Scope scope = {
        .id = id,
        .parent = parent,
        .map = SymbolMap_init(),
    };
    ScopeVec_push(&table->scopes, scope);
    return id;
}

SymbolId SymbolTable_add(SymbolTable *table, ScopeId scope, Token *t) {
    SymbolId id = SymbolTable_find(table, scope, t);
    if (id != NO_SYMBOL_ID)
        return id;

    Symbol symbol = {
        .id = SymbolVec_size(&table->symbols),
        .name = t->text,
        .scope = scope,
        .type = Type_simple(TYPE_UNKNOWN),
        .span = makeSourceSpan(t->src.line, t->src.col, t->text.size),
    };
    SymbolVec_push(&table->symbols, symbol);
    SymbolMap_insert(&table->scopes.data[scope].map, cstr_from_sv(t->text), symbol.id);
    return symbol.id;
}
SymbolId SymbolTable_find(SymbolTable *table, ScopeId scope, Token *t) {
    Scope *s = &table->scopes.data[scope];
    const SymbolMap_value *p = SymbolMap_get(&s->map, t->text);

    if (p)
        return p->second;
    else if (s->parent != scope)
        return SymbolTable_find(table, s->parent, t);
    else
        return NO_SYMBOL_ID;
}
Symbol *SymbolTable_lookup(SymbolTable *table, SymbolId id) {
    if (id >= SymbolVec_size(&table->symbols))
        return NULL;
    return &table->symbols.data[id];
}