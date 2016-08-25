#ifndef INSTR_H
#define INSTR_H

#include "symbol.H"
#include "util.H"
#include <sstream>
#include <vector>
using std::ostream;
using std::vector;

class Instr {
public:
    Instr(TINY_OP o, Symbol* d, Symbol* a1, Symbol* a2, LabelId l) :
        op_(o), dest_(d), arg1_(a1), arg2_(a2), label_(l) {}

    TINY_OP op()    const { return op_;    }
    Symbol* dest()  const { return dest_;  }
    Symbol* arg1()  const { return arg1_;  }
    Symbol* arg2()  const { return arg2_;  }
    LabelId label() const { return label_; }

    friend ostream& operator<<(ostream&, const Instr&);
    friend bool operator==(const Instr&, const Instr&);

    bool is_call()  const;
    bool is_label() const;
    bool is_jump()  const;
    bool is_unconditional_jump() const;

private:
    TINY_OP op_;
    Symbol* dest_;
    Symbol* arg1_;
    Symbol* arg2_;
    LabelId label_;
};

class Code {
public:
    Code(string fn, int bs) : function_name_(fn), byte_size_(bs) {}

    const vector<Instr>& instructions() const { return instrs_; }

    const string& function_name() { return function_name_; }

    int byte_size() const { return byte_size_; } 
    void set_byte_size(int i) { byte_size_ = i; }

    void emit_binop(TINY_OP, Symbol* ,Symbol*, Symbol*);
    void emit_unop(TINY_OP, Symbol*, Symbol*);
    void emit_binif(TINY_OP, Symbol*, Symbol*, LabelId);
    void emit_copy(Symbol*, Symbol* );
    void emit_jump(LabelId);
    void emit_par(Symbol*);
    void emit_ret(Symbol*);
    void emit_get(Symbol*);
    void emit_call(Symbol*, Symbol*, Symbol*);
    void emit_aref(Symbol*, Symbol*, Symbol*);
    void emit_aset(Symbol*, Symbol*, Symbol*);
    void emit_label(LabelId);
    void emit_read(Symbol*);
    void emit_read(Symbol*, Symbol*);
    void emit_writ(Symbol*);

private:
    vector<Instr> instrs_;
    string function_name_;      // corresponding function name 
    int byte_size_;             // memory taken by local variables
};


#endif
