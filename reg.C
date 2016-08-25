#include <iostream>
#include <cstdlib>
#include <algorithm>
#include "reg.H"
#include "error.H"
using std::remove;
using std::count;
using std::exit;

map<string, Reg*> Reg::reg_store_ = map<string, Reg*>();

Reg* Reg::get(const string& s) {
    if (reg_store_.find(s) == reg_store_.end())
        reg_store_[s] = new Reg(s);
    return reg_store_[s];
}

string Reg::byte_name() const {
    if (name_ == "%eax")
        return "%al";
    if (name_ == "%ebx")
        return "%bl";
    if (name_ == "%ecx")
        return "%cl";
    if (name_ == "%edx")
        return "%dl";
    say_unexpected(0, "Reg::byte_name()" + name_);
    exit(1);
}

bool Reg::is_dead(const set<Symbol*>& ss) {
    set<Symbol*>::const_iterator spp;
    for (spp = symbols_.begin(); spp != symbols_.end(); ++spp)
        if (count(ss.begin(), ss.end(), *spp))
            return false;
    return true;
}

void Reg::add_symbol(Symbol* s) {
    symbols_.insert(s);
}

void Reg::rem_symbol(Symbol* s) {
    symbols_.erase(s);
}

bool Reg::has_symbol(Symbol* s) const {
    set<Symbol*>::iterator sp = symbols_.find(s);
    return sp != symbols_.end();
}
