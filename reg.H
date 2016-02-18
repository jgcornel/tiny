#ifndef REG_H
#define REG_H

#include "symbol.H"
#include <string>
#include <set>
#include <map>
using std::string;
using std::set;
using std::map;

class Symbol; // forward declaration

class Reg {
public:
    static Reg* get(const string&);

    const string& name() const { return name_; }
    const set<Symbol*>& symbols() const { return symbols_; }
    set<Symbol*>& symbols() { return symbols_; }

    bool empty() const { return symbols_.empty(); }
    bool is_dead(const set<Symbol*>&);

    string byte_name() const;

    bool has_symbol(Symbol*) const;
    void add_symbol(Symbol*);
    void rem_symbol(Symbol*);

private:
    string name_;
    set<Symbol*> symbols_;

    static map<string, Reg*> reg_store_;

    Reg(const string& n) : name_(n) {}
    
};

#endif
