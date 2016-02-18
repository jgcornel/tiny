#include "symtab.H"
using std::make_pair;

int SymTab::par_offset() const {
    if (!parent() || is_root()) 
        return par_offset_;
    else
        return parent()->par_offset();
}

void SymTab::incr_par_offset_(int i) {
    if(!parent() || is_root())
        par_offset_ += i;
    else
        parent()->incr_par_offset_(i);
}

int SymTab::var_offset() const {
    if (!parent() || is_root())
        return var_offset_;
    else
        return parent()->var_offset();
}

void SymTab::incr_var_offset_(int i) {
    if(!parent() || is_root())
        var_offset_ += i;
    else
        parent()->incr_var_offset_(i);
}

Symbol* SymTab::get(const string& id) const {
    map<string, Symbol*>::const_iterator p = entries_.find(id);
    if (p != entries_.end())
        return p->second;
    else if (parent())
        return parent()->get(id);
    else
        return 0;
}

pair<Symbol*, bool> SymTab::put(Symbol* symbol) {
    pair<map<string, Symbol*>::iterator, bool> status;
    status = entries_.insert(make_pair(symbol->id(), symbol));
    if (!status.second) 
        return make_pair(status.first->second, false);
    else {
        if (symbol->is_var())
            incr_var_offset_(symbol->type()->byte_size());
        else if (symbol->is_arg()) 
            incr_par_offset_(IntType::get()->byte_size());

        return make_pair(symbol, true);
    }
}
