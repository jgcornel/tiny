#ifndef AST_H
#define AST_H

#include "util.H"
#include "type.H"
#include "symbol.H"
#include "symtab.H"
#include "instr.H"
#include <vector>
#include <list>
#include <string>
using std::vector;
using std::list;
using std::string;

class Symbol;
class SymTab;

class Node;
class Id;
class Program;
class Dec;
class VarDec;
class ParDec;
class FunDec;
class TypeName;
class ChrTypeName;
class IntTypeName;
class ArrTypeName;
class Stm;
class Block;
class IfStm;
class ElseStm;
class WhileStm;
class SetStm;
class ReturnStm;
class FunStm;
class WriteStm;
class ReadStm;
class Exp;
class LeftExp;
class ArrExp;
class VarExp;
class BinExp;
class RelExp;
class NegExp;
class NotExp;
class FunExp;
class LengthExp;
class IntExp;
class ChrExp;

class Node {
    int lineno_;
public:
    Node(int i) : lineno_(i) {}
    virtual ~Node() {}

    // getter for derived classes
    virtual int lineno() const { return lineno_; }
    
    virtual void build(SymTab*);
    virtual void check(SymTab*);
    virtual void label();
    virtual void inter(SymTab*, Code*, bool rexp = true);
    virtual void pluck(vector<Code*>*);
};

class Program : public Node {
    list<Dec*> decs_;
public:
    Program(int i) : Node(i) {}
    
    void add(Dec* e) { decs_.push_front(e); }

    void build(SymTab*);
    void check(SymTab*);
    void label();
    void inter(SymTab*, Code*, bool rexp = true);
    virtual void pluck(vector<Code*>*);
};

///////////////////////////////////////////////////////////////////////////////
// Container classes
///////////////////////////////////////////////////////////////////////////////
class Id : public Node {
public:
    string str;
    Id(int i, const string& s) : Node(i), str(s) {}
};

class VarDecList : public Node {
public:
    list<VarDec*> elems;
    VarDecList(int i) : Node(i) {}
    void add(VarDec* vd) { elems.push_back(vd); }
};

class ParDecList : public Node {
public:
    list<ParDec*> elems;
    ParDecList(int i) : Node(i) {}
    void add(ParDec* pd) { elems.push_back(pd); }
};

class ExpList : public Node {
public:
    list<Exp*> elems;
    ExpList(int i) : Node(i) {}
    void add(Exp* e) { elems.push_back(e); }
};

class StmList : public Node {
public:
    list<Stm*> elems;
    StmList(int i) : Node(i) {}
    void add(Stm* s) { elems.push_back(s); }
};

///////////////////////////////////////////////////////////////////////////////
// Declaration sub-hierarchy
///////////////////////////////////////////////////////////////////////////////
class Dec : public Node {
    TypeName* type_name_;
    string id_;
public:
    Dec(int i, TypeName* tn, const string& s) : 
        Node(i), type_name_(tn), id_(s) {}

    // getters for derived classes
    virtual Type* type();
    virtual const string& id() const { return id_; }
};

class VarDec : public Dec {
public:
    VarDec(int i, TypeName* tn, const string& s) : Dec(i, tn, s) {}

    void build(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

class ParDec : public Dec {
public:
    ParDec(int i, TypeName* tn, const string& s) : Dec(i, tn, s) {}

    void build(SymTab*);
};

class FunDec : public Dec {
    list<ParDec*> pars_;
    Block* block_;
    Code* code_;
public:
    FunDec(int i, TypeName* tn, const string& s, const list<ParDec*>& pp, Block* b) :
        Dec(i, tn, s), pars_(pp), block_(b), code_(new Code(id(), 0)) {}

    void build(SymTab*);
    void check(SymTab*);
    void label();
    void inter(SymTab*, Code*, bool rexp = true);
    void pluck(vector<Code*>*);
};

///////////////////////////////////////////////////////////////////////////////
// TypeName sub-hierarchy
///////////////////////////////////////////////////////////////////////////////
class TypeName : public Node {
    Type* type_;
public:
    TypeName(int i, Type* t) : Node(i), type_(t) {}

    // getter for derived classes
    virtual Type* type() const { return type_; }

};

class ChrTypeName : public TypeName {
public:
    ChrTypeName(int i) : TypeName(i, ChrType::get()) {}
};

class IntTypeName : public TypeName {
public:
    IntTypeName(int i) : TypeName(i, IntType::get()) {}
};

class ArrTypeName : public TypeName {
    TypeName* type_name_;
    Exp* exp_;
public:
    ArrTypeName(int, TypeName*, Exp*);
};

///////////////////////////////////////////////////////////////////////////////
// Statement sub-hierarchy
///////////////////////////////////////////////////////////////////////////////
class Stm : public Node {
    LabelId begin_;
    LabelId end_;
public:
    Stm(int i) : Node(i), begin_(0), end_(0) {}

    // getters and setters for derived classes
    LabelId begin() const     { return begin_; }
    LabelId end()   const     { return end_;   }
    void set_begin(LabelId b) { begin_ = b;    }
    void set_end(LabelId e)   { end_ = e;      }

    virtual void label();
};

class Block : public Stm {
    list<VarDec*> var_decs_;
    list<Stm*> stms_;
    SymTab* sym_tab_;
public:
    Block(int i, const list<VarDec*>& vds, const list<Stm*>& ss) :
        Stm(i), var_decs_(vds), stms_(ss) {}

    // getter and setter for FunDec
    SymTab* sym_tab() const       { return sym_tab_; }
    void set_sym_tab(SymTab* tab) { sym_tab_ = tab;  }

    void build(SymTab*);
    void check(SymTab*);
    void label();
    void inter(SymTab*, Code*, bool rexp = true);
};

class IfStm : public Stm {
    Exp* exp_;
    Stm* stm_;
public:
    IfStm(int i, Exp* e, Stm* s) : Stm(i), exp_(e), stm_(s) {}

    void build(SymTab*);
    void check(SymTab*);
    void label();
    void inter(SymTab*, Code*, bool rexp = true);
};

class ElseStm : public Stm {
    Exp* exp_;
    Stm* stm1_;
    Stm* stm2_;
public:
    ElseStm(int i, Exp* e, Stm* s1, Stm* s2) : 
        Stm(i), exp_(e), stm1_(s1), stm2_(s2) {}

    void build(SymTab*);
    void check(SymTab*);
    void label();
    void inter(SymTab*, Code*, bool rexp = true);
};

class WhileStm : public Stm {
    Exp* exp_;
    Stm* stm_;
public:
    WhileStm(int i, Exp* e, Stm* s) : Stm(i), exp_(e), stm_(s) {}

    void build(SymTab*);
    void check(SymTab*);
    void label();
    void inter(SymTab*, Code*, bool rexp = true);
};

class SetStm : public Stm {
    LeftExp* left_exp_;
    Exp* exp_;
public:
    SetStm(int i, LeftExp* le, Exp* e) : Stm(i), left_exp_(le), exp_(e) {}

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

class ReturnStm : public Stm {
    Exp* exp_;
public:
    ReturnStm(int i, Exp* e) : Stm(i), exp_(e) {}

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

class FunStm : public Stm {
    string id_;
    list<Exp*> args_;
public:
    FunStm(int i, const string& s, const list<Exp*>& es) : 
        Stm(i), id_(s), args_(es) {}

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

class WriteStm : public Stm {
    Exp* exp_;
public:
    WriteStm(int i, Exp* e) : Stm(i), exp_(e) {}

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

class ReadStm : public Stm {
    LeftExp* left_exp_;
public:
    ReadStm(int i, LeftExp* le) : Stm(i), left_exp_(le) {}

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

///////////////////////////////////////////////////////////////////////////////
// Expression sub-hierarchy
///////////////////////////////////////////////////////////////////////////////
class Exp : public Node {
    Type* type_;
    Symbol* symbol_;
public:

    Exp(int i) : Node(i), type_(0), symbol_(0) {}

    // getters and setters for derived classes
    Type*   type()   const     { return type_;   }
    Symbol* symbol() const     { return symbol_; }
    void set_type(Type* t)     { type_ = t;      }
    void set_symbol(Symbol* s) { symbol_ = s;    }

    virtual bool is_const() const;
    virtual int value() const;

    virtual void inter_jump(SymTab*, Code*, LabelId, LabelId);
};

class LeftExp : public Exp {
public:
    LeftExp(int i) : Exp(i) {}

    virtual bool is_var() const = 0;
    virtual int  index() const = 0;
    virtual const VarExp* get_var() const = 0;
};

class ArrExp : public LeftExp {
    LeftExp* left_exp_;
    Exp* exp_;
public:
    ArrExp(int i, LeftExp* le, Exp* e) : LeftExp(i), left_exp_(le), exp_(e) {}

    bool is_var() const { return false; }
    int  index()  const { return left_exp_->index() + 1; }
    const VarExp* get_var() const { return left_exp_->get_var(); }

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

class VarExp : public LeftExp {
    string id_;
public:
    VarExp(int i, const string& s) : LeftExp(i), id_(s) {}

    bool is_var() const { return true; }
    int  index()  const { return 0; }
    const VarExp* get_var() const { return this; }

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

class BinExp : public Exp {
    TINY_OP tiny_op_;
    Exp* exp1_;
    Exp* exp2_;
public:
    BinExp(int i, TINY_OP o, Exp* e1, Exp* e2) : 
        Exp(i), tiny_op_(o), exp1_(e1), exp2_(e2) {}
    bool is_const() const;
    int value() const;

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

class RelExp : public Exp {
    TINY_OP tiny_op_;
    Exp* exp1_;
    Exp* exp2_;
public:
    RelExp(int i, TINY_OP to, Exp* e1, Exp* e2) : 
        Exp(i), tiny_op_(to), exp1_(e1), exp2_(e2) {}
    bool is_const() const;
    int value() const;

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
    void inter_jump(SymTab*, Code*, LabelId, LabelId);
};

class NegExp : public Exp {
    TINY_OP tiny_op_;
    Exp* exp_;

public:
    NegExp(int i, TINY_OP to, Exp* e) : Exp(i), tiny_op_(to), exp_(e) {}
    bool is_const() const;
    int value() const;

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

class NotExp : public Exp {
    TINY_OP tiny_op_;
    Exp* exp_;
public:
    NotExp(int i, TINY_OP to, Exp* e) : Exp(i), tiny_op_(to), exp_(e) {}
    bool is_const() const;
    int value() const;

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
    void inter_jump(SymTab*, Code*, LabelId, LabelId);
};

class FunExp : public Exp {
    string id_;
    list<Exp*> args_;
public:
    FunExp(int i, const string& s, const list<Exp*>& as) : 
        Exp(i), id_(s), args_(as) {}

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

class LengthExp : public Exp {
    LeftExp* left_exp_;
public:
    LengthExp(int i, LeftExp* le) : Exp(i), left_exp_(le) {}

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

class IntExp : public Exp {
    int val_;
public:
    IntExp(int i, int v) : Exp(i), val_(v) {}

    bool is_const() const;
    int  value()    const;

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

class ChrExp : public Exp {
    char val_;
public:
    ChrExp(int i, char v) : Exp(i), val_(v) {}

    bool is_const() const;
    int  value()    const;

    void check(SymTab*);
    void inter(SymTab*, Code*, bool rexp = true);
};

#endif
