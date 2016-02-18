#include "instr.H"
#include <string>
#include <iostream>
using std::string;
using std::cout;
using std::endl;

ostream& operator<<(ostream& os, const Instr& instr) {
    string o  = TINY_OP_STR(instr.op_);
    string zz = string("        ");
    string d  = instr.dest_ ? instr.dest_->id() : string();
    string a1 = instr.arg1_ ? instr.arg1_->id() : string();
    string a2 = instr.arg2_ ? instr.arg2_->id() : string();
    LabelId l = instr.label_;

    switch(instr.op_) {
        case OP_ADD  :
        case OP_SUB  :
        case OP_MUL  :
        case OP_DIV  : return os << zz << d << " = " << a1 << o << a2;
        case OP_ADDR :
        case OP_NEG  :
        case OP_CCI  :
        case OP_CIC  : return os << zz << d << " = " << o << a1;
        case OP_EQ   :
        case OP_NE   :
        case OP_G    :
        case OP_L    : return os << zz << o << a1 << " " << a2 << " GOTO " << l;
        case OP_CP   : return os << zz << d << " = " << a1;
        case OP_JMP  : return os << zz << o << l;
        case OP_CALL : return os << zz << d << " = " << o << " " << a1 << " " << a2;
        case OP_PAR  :
        case OP_RET  :
        case OP_WRIT : return os << zz << o << a1;
        case OP_AREF : return os << zz << d << " = " << a1 << "[" << a2 << "]";
        case OP_ASET : return os << zz << d << "[" << a1 << "]" << " = " << a2;
        case OP_LBL  : return os << l << ":";
        case OP_GET  :
        case OP_READ : if (instr.arg1_)
                           return os << zz << d << "[" << a1 << "]" << " = " << o;
                       else
                           return os << zz << d << " = " << o;
        default      : return os;
    }
}

bool operator==(const Instr& i1, const Instr& i2) {
    return i1.is_label() && i2.is_label() && i1.label() == i2.label();
}

bool Instr::is_call() const {
    return op_ == OP_CALL;
}

bool Instr::is_label() const {
    switch(op_) {
        case OP_LBL : return true;
        default     : return false;
    }
}

bool Instr::is_jump() const {
    switch(op_) {
        case OP_EQ  :
        case OP_NE  :
        case OP_G   :
        case OP_L   :
        case OP_JMP :
        case OP_RET : return true;
        default     : return false;
    }
}

bool Instr::is_unconditional_jump() const {
    switch(op_) {
        case OP_JMP : return true;
        default     : return false;
    }
}

void Code::emit_binop(TINY_OP o, Symbol* d, Symbol* a1, Symbol* a2) { 
    instrs_.push_back(Instr(o, d, a1, a2, 0));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_unop(TINY_OP o, Symbol* d, Symbol* a1) { 
    instrs_.push_back(Instr(o, d, a1, 0, 0));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_binif(TINY_OP o, Symbol* a1, Symbol* a2, LabelId l) { 
    instrs_.push_back(Instr(o, 0, a1, a2, l));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_copy(Symbol* d, Symbol* a1) { 
    instrs_.push_back(Instr(OP_CP, d, a1, 0, 0));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_jump(LabelId l) { 
    instrs_.push_back(Instr(OP_JMP, 0, 0, 0, l));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_par(Symbol* a1) { 
    instrs_.push_back(Instr(OP_PAR, 0, a1, 0, 0));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_ret(Symbol* a1) { 
    instrs_.push_back(Instr(OP_RET, 0, a1, 0, 0));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_get(Symbol* d) {
    instrs_.push_back(Instr(OP_GET, d, 0, 0, 0));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_call(Symbol* d, Symbol* a1, Symbol* a2) { 
    instrs_.push_back(Instr(OP_CALL, d, a1, a2, 0));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_aref(Symbol* d, Symbol* a1, Symbol* a2) { 
    instrs_.push_back(Instr(OP_AREF, d, a1, a2, 0));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_aset(Symbol* d, Symbol* a1, Symbol* a2) { 
    instrs_.push_back(Instr(OP_ASET, d, a1, a2, 0));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_label(LabelId l) { 
    instrs_.push_back(Instr(OP_LBL, 0, 0, 0, l));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_read(Symbol* d) { 
    instrs_.push_back(Instr(OP_READ, d, 0, 0, 0));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_read(Symbol* d, Symbol* a1) {
    instrs_.push_back(Instr(OP_READ, d, a1, 0, 0));
    if (DEBUG)
        cout << instrs_.back() << endl;
}

void Code::emit_writ(Symbol* a1) { 
    instrs_.push_back(Instr(OP_WRIT, 0, a1, 0, 0));
    if (DEBUG)
        cout << instrs_.back() << endl;
}
