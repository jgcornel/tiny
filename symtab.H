#ifndef SYMTAB_H
#define SYMTAB_H

#include "symbol.H"
#include <string>
#include <map>
using std::string;
using std::map;
using std::pair;

class SymTab {
public:
    SymTab(SymTab* p, const string& s, bool r) :
        parent_(p), name_(s), root_(r), par_offset_(0), var_offset_(0) {}

    SymTab*       parent()     const { return parent_;     }
    const string& name()       const { return name_;       }
    bool          is_root()    const { return root_;       }
    const map<string, Symbol*>& entries() const { return entries_; }

    int                 par_offset() const;
    int                 var_offset() const;
    Symbol*             get(const string& s) const;
    pair<Symbol*, bool> put(Symbol* s);

private:
    map<string, Symbol*> entries_;

    SymTab* parent_;
    string name_;
    bool root_;
    int par_offset_;
    int var_offset_;

    void incr_par_offset_(int);
    void incr_var_offset_(int);
};

#endif
