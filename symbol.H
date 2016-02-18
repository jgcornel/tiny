#ifndef SYMBOL_H
#define SYMBOL_H

#include "type.H"
#include "reg.H" 
#include <string>
#include <map>
#include <set>
using std::string;
using std::set;
using std::map;

class Reg;  // forward declaration

///////////////////////////////////////////////////////////////////////////////
// Symbol
///////////////////////////////////////////////////////////////////////////////
class Symbol {
public:
    Symbol(int i, Type* t, const string& s, bool gl = false) :
        lineno_(i), type_(t), id_(s), valid_(true), global_(gl) {}
    virtual ~Symbol() {}

    virtual bool is_arg()    const { return false; }
    virtual bool is_fun()    const { return false; }
    virtual bool is_var()    const { return false; }
    virtual bool is_temp()   const { return false; }
    virtual bool is_const()  const { return false; }
    virtual bool is_global() const { return global_; }

    virtual int           lineno() const { return lineno_; } 
    virtual Type*         type()   const { return type_;   }
    virtual const string& id()     const { return id_;     }
    virtual int           index()  const { return 0;       }

    virtual int int_value() const { return 0; }
    virtual char chr_value() const { return 0; }

    // Stuff that concerns memory locations for this symbol
    // For convenience it is put in the root symbol class even if it does not 
    // make sense for functions and constants ..
    //-*-*-*-*-*-
    virtual const set<Reg*>& registers() const { return registers_; }
    virtual void  add_register(Reg* r)  { registers_.insert(r); }
    virtual void  rem_register(Reg* r);

    virtual bool is_mem_valid() const { return valid_; }
    virtual void set_mem_valid(bool v) { valid_ = v; }

    virtual bool is_alive(const set<Symbol*>&) const;
    virtual bool is_somewhere() const;

private:
    int lineno_;
    Type* type_;
    string id_;

    // Stuff that concerns memory locations for this symbol
    // For convenience it is put in the root symbol class even if it does not 
    // make sense for functions and constants ..
    //-*-*-*-*-*-
    bool valid_;
    bool global_;
    set<Reg*> registers_;
};

///////////////////////////////////////////////////////////////////////////////
// FunSym
///////////////////////////////////////////////////////////////////////////////
class FunSym : public Symbol {
public:

    // functions are always global...
    FunSym(int i, Type* t, const string& s, const list<Type*>& tt) :
        Symbol(i, t, s, true), par_types_(tt) {}

    const list<Type*>& par_types() const { return par_types_; }
    
    bool is_fun() const { return true; }

private:
    list<Type*> par_types_;
};

///////////////////////////////////////////////////////////////////////////////
// ArgSym
///////////////////////////////////////////////////////////////////////////////
class ArgSym : public Symbol {
public:
    // arguments are never global...
    ArgSym(int i, Type* t, const string& s, int j) : 
        Symbol(i, t, s), index_(j) {}

    bool is_arg() const { return true; }
    int  index()  const { return index_; }

private:
    int index_;
};

///////////////////////////////////////////////////////////////////////////////
// VarSym
///////////////////////////////////////////////////////////////////////////////
class VarSym : public Symbol {
public:
    // variables might be global
    VarSym(int i, Type* t, const string& s, int j, bool g) :
        Symbol(i, t, s, g), index_(j), temp_(false) {}
    // temporary variables are never global
    VarSym(int i, Type* t) :
        Symbol(i, t, new_name_()), index_(0), temp_(true) {}

    bool is_var()  const { return true; }
    bool is_temp() const { return temp_; }
    int  index()   const { return index_; }

private:
    int index_;
    bool temp_;

    static string new_name_();
};

///////////////////////////////////////////////////////////////////////////////
// ConstSym
///////////////////////////////////////////////////////////////////////////////
class ConstSym : public Symbol {
public:
    static ConstSym* get(char);
    static ConstSym* get(int);

    int int_value() const { return int_val_; }
    char chr_value() const { return chr_val_; }

    bool is_const() const { return true; }
    
private:
    char chr_val_;
    int  int_val_;

    static map<int, ConstSym*> int_store_;
    static map<char, ConstSym*> char_store_;

    ConstSym(char v) : 
        Symbol(0, ChrType::get(), chr_to_str_(v)), chr_val_(v), int_val_(0) {}
    ConstSym(int v) :
        Symbol(0, IntType::get(), int_to_str_(v)), chr_val_(0), int_val_(v) {}

    string chr_to_str_(char);
    string int_to_str_(int);
};

#endif
