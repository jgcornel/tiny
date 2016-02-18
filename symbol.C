#include "symbol.H"
#include <iostream>
#include <sstream>
#include <algorithm>
using std::count;
using std::ostringstream;

map<int, ConstSym*> ConstSym::int_store_ = map<int, ConstSym*>();
map<char, ConstSym*> ConstSym::char_store_ = map<char, ConstSym*>();

void Symbol::rem_register(Reg* r) {
    set<Reg*>::iterator s = registers_.find(r);
    if (s != registers_.end())
        registers_.erase(r);
}

bool Symbol::is_alive(const set<Symbol*>& symbolss) const {
    return count(symbolss.begin(), symbolss.end(), this) > 0;
} 

bool Symbol::is_somewhere() const {
    return valid_ || registers_.size() > 0;
}


string VarSym::new_name_() {
    static int i = 0;
    ostringstream oss;
    oss << "T" << ++i;
    return oss.str();
}

string ConstSym::chr_to_str_(char c) {
    ostringstream oss;
    int i = static_cast<int>(c);
    oss << i;
    return oss.str();
}

string ConstSym::int_to_str_(int i) {
    ostringstream oss;
    oss << i;
    return oss.str();
}

ConstSym* ConstSym::get(char c) {
    if (char_store_.find(c) == char_store_.end())
        char_store_[c] = new ConstSym(c);
    return char_store_[c];
}

ConstSym* ConstSym::get(int i)  {
    if (int_store_.find(i) == int_store_.end())
        int_store_[i] = new ConstSym(i);
    return int_store_[i];
}
