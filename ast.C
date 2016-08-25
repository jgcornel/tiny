#include "ast.H"
#include "error.H"
#include <vector>
#include <iostream>
#include <algorithm>
#include <cstdlib>
using std::vector;
using std::for_each;
using std::bind2nd;
using std::mem_fun;
using std::transform;
using std::back_inserter;
using std::exit;

///////////////////////////////////////////////////////////////////////////////
// Dec helper methods
///////////////////////////////////////////////////////////////////////////////
Type* Dec::type() {
    return type_name_->type();
}

///////////////////////////////////////////////////////////////////////////////
// ArrTypeName constructor
///////////////////////////////////////////////////////////////////////////////
ArrTypeName::ArrTypeName(int i, TypeName* tn, Exp* e) :
    TypeName(i, new ArrType(tn->type(), e->value()))
{
    if (!e->is_const()) {
        say_bad_array_size(i);
        exit(1);
    }
}


///////////////////////////////////////////////////////////////////////////////
// Exp helper methods
///////////////////////////////////////////////////////////////////////////////
bool Exp::is_const() const {
    return false;
}

bool BinExp::is_const() const {
    return exp1_->is_const() && exp2_->is_const();
}

bool RelExp::is_const() const {
    return exp1_->is_const() && exp2_->is_const();
}

bool NegExp::is_const() const {
    return exp_->is_const();
}

bool NotExp::is_const() const {
    return exp_->is_const();
}

bool IntExp::is_const() const {
    return true;
}

bool ChrExp::is_const() const {
    return true;
}

int Exp::value() const {
    return 0;
}

int BinExp::value() const {
    int v1 = exp1_->value();
    int v2 = exp2_->value();

    switch(tiny_op_) {
        case OP_ADD : return v1 + v2;
        case OP_SUB : return v1 - v2;
        case OP_MUL : return v1 * v2;
        case OP_DIV : return v1 / v2;
        default :
            say_unexpected(lineno(), "Expression with unexpected operator");
            exit(1);
    }
}

int RelExp::value() const {
    int v1 = exp1_->value();
    int v2 = exp2_->value();

    switch(tiny_op_) {
        case OP_EQ : return v1 == v2;
        case OP_NE : return v1 != v2;
        case OP_G  : return v1 > v2;
        case OP_L  : return v1 == v2;
        default :
            say_unexpected(lineno(), "Expression with unexpected operator");
            exit(1);
    }
}

int NegExp::value() const {
    return -exp_->value();
}

int NotExp::value() const {
    return !exp_->value();
}

int IntExp::value() const {
    return val_;
}

int ChrExp::value() const {
    return val_;
}

///////////////////////////////////////////////////////////////////////////////
// Helper functions for the storage and retrieval of symbols
///////////////////////////////////////////////////////////////////////////////
void put_or_exit_(SymTab* tab, Symbol* symbol) {
    pair<Symbol*, bool> result = tab->put(symbol);
    if (!result.second) {
        say_redeclared(symbol->lineno(), symbol->id(), result.first->lineno());
        exit(1);
    }
}

Symbol* get_or_exit_(SymTab* tab, const string& id, int lineno) {
    Symbol* s = tab->get(id);

    if (!s) {
        say_undeclared(lineno, id);
        exit(1);
    }
    return s;
}

///////////////////////////////////////////////////////////////////////////////
// THE GREAT BUILDING PHASE
///////////////////////////////////////////////////////////////////////////////

void Node::build(SymTab* tab) {
}

void Program::build(SymTab* tab) {
    for_each(decs_.begin(), decs_.end(), bind2nd(mem_fun(&Dec::build), tab));
}

// DEC
void VarDec::build(SymTab* tab) {
    bool glob = !tab->parent(); // hardly elegant...
    VarSym* vs = new VarSym(lineno(), type(), id(), tab->var_offset(), glob);
    put_or_exit_(tab, vs);
}

void ParDec::build(SymTab* tab) {
    ArgSym* as = new ArgSym(lineno(), type(), id(), tab->par_offset());
    put_or_exit_(tab, as);
}

void FunDec::build(SymTab* tab) {
    list<Type*> types;

    transform(pars_.begin(), 
              pars_.end(), 
              back_inserter<list<Type*> >(types),
              mem_fun(&ParDec::type));
    FunSym* fs = new FunSym(lineno(), type(), id(), types);
    put_or_exit_(tab, fs);
    SymTab* st = new SymTab(tab, id(), true);
    block_->set_sym_tab(st);
    for_each(pars_.begin(), pars_.end(), bind2nd(mem_fun(&ParDec::build), st));
    block_->build(st); 
}

// STM
void Block::build(SymTab* tab) {
    if (!sym_tab_)
        sym_tab_ = new SymTab(tab, tab->name(), false);
    for_each(var_decs_.begin(), 
             var_decs_.end(), 
             bind2nd(mem_fun(&VarDec::build), sym_tab_));
    for_each(stms_.begin(),
             stms_.end(),
             bind2nd(mem_fun(&Stm::build), sym_tab_));
}

void IfStm::build(SymTab* tab) {
    stm_->build(tab);
}

void ElseStm::build(SymTab* tab) {
    stm1_->build(tab);
    stm2_->build(tab);
}

void WhileStm::build(SymTab* tab) {
    stm_->build(tab);
}

///////////////////////////////////////////////////////////////////////////////
// THE GREAT CHECKING PHASE
///////////////////////////////////////////////////////////////////////////////

void check_fun_(SymTab* tab, 
                int lineno, 
                const string& id, 
                const list<Exp*>& args) 
{
    list<Type*> arg_types;

    Symbol* s = get_or_exit_(tab, id, lineno);
    if (!s->is_fun()) {
        say_not_a_function(lineno, id, s->lineno());
        exit(1);
    }
    for_each(args.begin(), args.end(), bind2nd(mem_fun(&Exp::check), tab));
    FunSym* fs = dynamic_cast<FunSym*>(s);
    list<Type*> par_types = fs->par_types();
    transform(args.begin(), 
              args.end(), 
              back_inserter<list<Type*> >(arg_types),
              mem_fun(&Exp::type));
    if (par_types.size() != arg_types.size()) {
        say_bad_argument_count(lineno, id, par_types.size(), arg_types.size());
        exit(1);
    }
    if (!Type::types_match(par_types, arg_types)) {
        say_bad_argument_type(lineno, id, fs->lineno());
        exit(1);
    }
}

void Node::check(SymTab* tab) {
}

void Program::check(SymTab* tab) {
    for_each(decs_.begin(), decs_.end(), bind2nd(mem_fun(&Dec::check), tab));

    Symbol* tiny = tab->get("tiny");
    if (!tiny) {
        say_no_tiny(lineno());
        exit(1);
    }
    if (!tiny->is_fun()) {
        say_bad_tiny_type(lineno());
        exit(1);
    }
    FunSym* fs = dynamic_cast<FunSym*>(tiny);
    if (!(fs->type() == IntType::get()) || !(fs->par_types().size() == 0)) {
        say_bad_tiny_type(lineno());
        exit(1);
    }
}

// DEC
void FunDec::check(SymTab* tab) {
    if (!type()->is_primitive()) {
        say_bad_function_type(lineno(), id());
        exit(1);
    }
    block_->check(tab);
}

// STM
void Block::check(SymTab* tab) {
    for_each(stms_.begin(), stms_.end(), bind2nd(mem_fun(&Stm::check), sym_tab_));
}

void IfStm::check(SymTab* tab) {
    exp_->check(tab);
    if (!exp_->type()->is_primitive()) {
        say_bad_condition_type(lineno());
        exit(1);
    }
    stm_->check(tab);
}

void ElseStm::check(SymTab* tab) {
    exp_->check(tab);
    if (!exp_->type()->is_primitive()) {
        say_bad_condition_type(lineno());
        exit(1);
    }
    stm1_->check(tab);
    stm2_->check(tab);
}

void WhileStm::check(SymTab* tab) {
    exp_->check(tab);
    if (!exp_->type()->is_primitive()) {
        say_bad_condition_type(lineno());
        exit(1);
    }
    stm_->check(tab);
}

void SetStm::check(SymTab* tab) {
    left_exp_->check(tab);
    if (!left_exp_->type()->is_primitive()) {
        say_bad_assignee_type(lineno());
        exit(1);
    }
    exp_->check(tab);
    if (!exp_->type()->is_primitive()) {
        say_bad_assigner_type(lineno());
        exit(1);
    }
}

void ReturnStm::check(SymTab* tab) {
    exp_->check(tab);
    if (!exp_->type()->is_primitive()) {
        say_bad_return_type(lineno());
        exit(1);
    }
}

void FunStm::check(SymTab* tab) {
    check_fun_(tab, lineno(), id_, args_);
}

void WriteStm::check(SymTab* tab) {
    exp_->check(tab);
    if (!exp_->type()->is_primitive()) {
        say_bad_write_type(lineno());
        exit(1);
    }
}

void ReadStm::check(SymTab* tab) {
    left_exp_->check(tab);
    if (!left_exp_->type()->is_primitive()) {
        say_bad_read_type(lineno());
        exit(1);
    }
}

// EXP
void ArrExp::check(SymTab* tab) {
    left_exp_->check(tab);
    if (left_exp_->type()->is_primitive()) {
        say_bad_indexee_type(lineno());
        exit(1);
    }
    exp_->check(tab);
    if (!exp_->type()->is_primitive()) {
        say_bad_indexer_type(lineno());
        exit(1);
    }
    set_type(left_exp_->type()->element_type());
}

void VarExp::check(SymTab* tab) {
    set_symbol(get_or_exit_(tab, id_, lineno()));
    if (!symbol()->is_arg() && !symbol()->is_var()) {
        say_not_a_variable(lineno(), id_, symbol()->lineno());
        exit(1);
    }
    set_type(symbol()->type());
}

void BinExp::check(SymTab* tab) {
    exp1_->check(tab);
    exp2_->check(tab);
    set_type(IntType::get());
    if (!type()) {
        say_bad_subexpr_type(lineno());
        exit(1);
    }
}

void RelExp::check(SymTab* tab) {
    exp1_->check(tab);
    exp2_->check(tab);
    set_type(IntType::get());
    if (!type()) {
        say_bad_subexpr_type(lineno());
        exit(1);
    }
}

void NegExp::check(SymTab* tab) {
    exp_->check(tab);
    set_type(IntType::get());
    if (!exp_->type()->is_primitive()) {
        say_bad_subexpr_type(lineno());
        exit(1);
    }
}

void NotExp::check(SymTab* tab) {
    exp_->check(tab);
    set_type(IntType::get());
    if (!exp_->type()->is_primitive()) {
        say_bad_subexpr_type(lineno());
        exit(1);
    }
}

void FunExp::check(SymTab* tab) {
    check_fun_(tab, lineno(), id_, args_);
    set_type(tab->get(id_)->type());
}

void LengthExp::check(SymTab* tab) {
    left_exp_->check(tab);
    set_type(IntType::get());
    if (left_exp_->type()->is_primitive()) {
        say_bad_length_type(lineno());
        exit(1);
    }
}

void IntExp::check(SymTab* tab) {
    set_type(IntType::get());
}

void ChrExp::check(SymTab* tab) {
    set_type(ChrType::get());
}

///////////////////////////////////////////////////////////////////////////////
// THE GREAT LABELING PHASE
///////////////////////////////////////////////////////////////////////////////

void Node::label() {
}

void Program::label() {
    for_each(decs_.begin(), decs_.end(), mem_fun(&Dec::label));
}

// DEC
void FunDec::label() {
    block_->label();
}

// STM
void Stm::label() {
    if (!begin())
        set_begin(get_next_label_id());
    if (!end())
        set_end(get_next_label_id());
}

void Block::label() {
    if (!begin())
        set_begin(get_next_label_id());
    LabelId next = begin();
    list<Stm*>::iterator s;
    for (s = stms_.begin(); s != stms_.end(); ++s) {
        (*s)->set_begin(next);
        (*s)->label();
        next = (*s)->end();
    }
    if (!end()) 
        set_end(next);
}

void IfStm::label() {
    if (!begin())
        set_begin(get_next_label_id());
    stm_->label();
    if (!end())
        set_end(stm_->end());
}

void ElseStm::label() {
    if (!begin())
        set_begin(get_next_label_id());
    stm1_->label();
    stm2_->label();
    if (!end())
        set_end(stm2_->end()); 
}

void WhileStm::label() {
    if (!begin())
        set_begin(get_next_label_id());
    stm_->label();
    if (!end())
        set_end(get_next_label_id());
}

///////////////////////////////////////////////////////////////////////////////
// THE GREAT INTERMEDIATE CODE PHASE
///////////////////////////////////////////////////////////////////////////////

Symbol* convert_type_(Code* code, Symbol* symbol, Type* type) {
    Symbol* cs = symbol;

    if (symbol->type() != type) {
        cs = new VarSym(symbol->lineno(), type);
        if (type == IntType::get())
            code->emit_unop(OP_CCI, cs, symbol);
        else if (type == ChrType::get())
            code->emit_unop(OP_CIC, cs, symbol);
    }
    return cs;
}

VarSym* inter_fun_(SymTab* tab,
                Code* code,
                int lineno, 
                const string& id, 
                const list<Exp*>& args) 
{
    FunSym* fs = dynamic_cast<FunSym*>(tab->get(id));
    list<Type*> types = fs->par_types();
    list<Symbol*> symbols = list<Symbol*>();

    list<Exp*>::const_iterator e;
    list<Type*>::const_iterator t;
    for (e = args.begin(), t = types.begin(); e != args.end(); ++e, ++t) {
        (*e)->inter(tab, code);
        Symbol* cs = convert_type_(code, (*e)->symbol(), *t);
        symbols.push_front(cs); // in reverse order
    }
    list<Symbol*>::const_iterator s;
    for (s = symbols.begin(); s != symbols.end(); ++s)
    {
        if ((*s)->type()->is_primitive() || (*s)->is_arg())
            code->emit_par(*s);
        else {
            VarSym* as = new VarSym(lineno, IntType::get());
            code->emit_unop(OP_ADDR, as, *s);
            code->emit_par(as);
        }
    }
    ConstSym* argc = ConstSym::get(static_cast<int>(args.size()));
    VarSym* tmp = new VarSym(lineno, fs->type());
    code->emit_call(tmp, fs, argc);
    return tmp;
}

void Node::inter(SymTab* tab, Code* code, bool rexp) {
}

void Program::inter(SymTab* tab, Code* code, bool rexp) {
    list<Dec*>::iterator d;

    for (d = decs_.begin(); d != decs_.end(); ++d)
        (*d)->inter(tab, code);
}

// DEC
void write_length_(Code* c, Symbol* s, Type* t, int i) {
    if (t->is_primitive())
        return;
    else {
        ConstSym* index = ConstSym::get(i);
        ConstSym* size = ConstSym::get(t->size());
        c->emit_aset(s, index, size);
        write_length_(c, s, t->element_type(), i + 4);
    }
}

void VarDec::inter(SymTab* tab, Code* code, bool rexp) {
    if (code) { // global variables will be handled elsewhere...
        Symbol* s = tab->get(id());
        write_length_(code, s, type(), 0);
    }
}

void FunDec::inter(SymTab* tab, Code* code, bool rexp) {
    if (DEBUG)
        std::cout << std::endl << id() << ":" << std::endl;
    code_->set_byte_size(block_->sym_tab()->var_offset()); // make clearer...
    block_->inter(tab, code_);
    code_->emit_label(block_->end());
}

// STM
void Block::inter(SymTab* tab, Code* code, bool rexp) {
    list<VarDec*>::iterator vd;
    for (vd = var_decs_.begin(); vd != var_decs_.end(); ++vd)
        (*vd)->inter(sym_tab_, code);
    list<Stm*>::iterator s;
    for (s = stms_.begin(); s != stms_.end(); ++s)
        (*s)->inter(sym_tab_, code);
}

void IfStm::inter(SymTab* tab, Code* code, bool rexp) {
    exp_->inter_jump(tab, code, stm_->begin(), end());
    code->emit_label(stm_->begin());
    stm_->inter(tab, code);
    code->emit_label(end());
}

void ElseStm::inter(SymTab* tab, Code* code, bool rexp) {
    exp_->inter_jump(tab, code, stm1_->begin(), stm2_->begin());
    code->emit_label(stm1_->begin());
    stm1_->inter(tab, code);
    code->emit_jump(end());
    code->emit_label(stm2_->begin());
    stm2_->inter(tab, code);
    code->emit_label(end());
}

void WhileStm::inter(SymTab* tab, Code* code, bool rexp) {
    code->emit_label(begin());
    exp_->inter_jump(tab, code, stm_->begin(), end());
    code->emit_label(stm_->begin());
    stm_->inter(tab, code);
    code->emit_jump(begin());
    code->emit_label(end());
}

void SetStm::inter(SymTab* tab, Code* code, bool rexp) {
    Symbol* var;

    exp_->inter(tab, code);
    Symbol* cs = convert_type_(code, exp_->symbol(), left_exp_->type());
    left_exp_->inter(tab, code, false);
    var = left_exp_->get_var()->symbol();
    if (left_exp_->is_var()) 
        code->emit_copy(var, cs);
    else
        code->emit_aset(var, left_exp_->symbol(), cs);
}

void ReturnStm::inter(SymTab* tab, Code* code, bool rexp) {
    Symbol* fs = tab->get(tab->name());
    exp_->inter(tab, code);
    Symbol* cs = convert_type_(code, exp_->symbol(), fs->type());
    code->emit_ret(cs);
}

void FunStm::inter(SymTab* tab, Code* code, bool rexp) {
    inter_fun_(tab, code, lineno(), id_, args_);
}

void WriteStm::inter(SymTab* tab, Code* code, bool rexp) {
    exp_->inter(tab, code);
    code->emit_writ(exp_->symbol());
}

void ReadStm::inter(SymTab* tab, Code* code, bool rexp) {
    Symbol* var;

    left_exp_->inter(tab, code, false);
    var = left_exp_->get_var()->symbol();
    if (left_exp_->is_var())
        code->emit_read(var);
    else 
        code->emit_read(var, left_exp_->symbol());
}

// EXP
void BinExp::inter(SymTab* tab, Code* code, bool rexp) {
    if (is_const()) 
        set_symbol(ConstSym::get(value()));
    else {
        exp1_->inter(tab, code);
        exp2_->inter(tab, code);
        set_symbol(new VarSym(lineno(), type()));
        Symbol* cs1 = convert_type_(code, exp1_->symbol(), type());
        Symbol* cs2 = convert_type_(code, exp2_->symbol(), type());
        code->emit_binop(tiny_op_, symbol(), cs1, cs2);
    }
}

void RelExp::inter(SymTab* tab, Code* code, bool rexp) {
    if (is_const())
        set_symbol(ConstSym::get(value()));
    else {
        LabelId true_label = get_next_label_id();
        LabelId next_label = get_next_label_id();
        exp1_->inter(tab, code);
        exp2_->inter(tab, code);
        set_symbol(new VarSym(lineno(), type()));
        Symbol* cs1 = convert_type_(code, exp1_->symbol(), type());
        Symbol* cs2 = convert_type_(code, exp2_->symbol(), type());
        code->emit_binif(tiny_op_, cs1, cs2, true_label);
        code->emit_copy(symbol(), ConstSym::get(0));
        code->emit_jump(next_label);
        code->emit_label(true_label);
        code->emit_copy(symbol(), ConstSym::get(1));
        code->emit_label(next_label);
    }
}

void NegExp::inter(SymTab* tab, Code* code, bool rexp) {
    if (is_const()) 
        set_symbol(ConstSym::get(value()));
    else {
        exp_->inter(tab, code);
        set_symbol(new VarSym(lineno(), type()));
        Symbol* cs = convert_type_(code, exp_->symbol(), type());
        code->emit_unop(tiny_op_, symbol(), cs);
    }
}

void NotExp::inter(SymTab* tab, Code* code, bool rexp) {
    if (is_const())
        set_symbol(ConstSym::get(value()));
    else {
        LabelId true_label = get_next_label_id();
        LabelId next_label = get_next_label_id();
        exp_->inter(tab, code);
        set_symbol(new VarSym(lineno(), type()));
        code->emit_binif(OP_EQ, exp_->symbol(), ConstSym::get(0), true_label);
        code->emit_copy(symbol(), ConstSym::get(0));
        code->emit_jump(next_label);
        code->emit_label(true_label);
        code->emit_copy(symbol(), ConstSym::get(1));
        code->emit_label(next_label);
    }
}

void FunExp::inter(SymTab* tab, Code* code, bool rexp) {
    VarSym* symbol = inter_fun_(tab, code, lineno(), id_, args_);
    set_symbol(symbol);
}

void IntExp::inter(SymTab* tab, Code* code, bool rexp) {
    set_symbol(ConstSym::get(val_));
}

void ChrExp::inter(SymTab* tab, Code* code, bool rexp) {
    set_symbol(ConstSym::get(val_));
}

void VarExp::inter(SymTab* tab, Code* code, bool rexp) {
    set_symbol(tab->get(id_));
}

void ArrExp::inter(SymTab* tab, Code* code, bool rexp) {
    left_exp_->inter(tab, code);
    exp_->inter(tab, code);
    set_symbol(new VarSym(lineno(), IntType::get()));

    if (index() == 1)
        code->emit_copy(symbol(), exp_->symbol());
    else if (left_exp_->symbol()->is_var()) {
        ConstSym* size = ConstSym::get(left_exp_->type()->size());
        code->emit_binop(OP_MUL, symbol(), left_exp_->symbol(), size);
        code->emit_binop(OP_ADD, symbol(), symbol(), exp_->symbol());
    }
    else if (left_exp_->symbol()->is_arg()) {
        VarSym* size = new VarSym(lineno(), IntType::get());
        ConstSym* offset  = ConstSym::get(ADDR_BYTE_SIZE * (index() - 1));
        code->emit_aref(size, get_var()->symbol(), offset);
        code->emit_binop(OP_MUL, symbol(), left_exp_->symbol(), size);
        code->emit_binop(OP_ADD, symbol(), symbol(), exp_->symbol());
    }

    if (type()->is_primitive()) {
        ConstSym* type_size = ConstSym::get(type()->byte_size());
        ConstSym* offset_size = ConstSym::get(ADDR_BYTE_SIZE * index());
        code->emit_binop(OP_MUL, symbol(), symbol(), type_size);
        code->emit_binop(OP_ADD, symbol(), symbol(), offset_size);

        if (rexp) {
            VarSym* vs = new VarSym(lineno(), type());
            code->emit_aref(vs, get_var()->symbol(), symbol());
            set_symbol(vs);
        }
    }
}

void LengthExp::inter(SymTab* tab, Code* code, bool rexp) {
    if (left_exp_->get_var()->symbol()->is_var())
        set_symbol(ConstSym::get(left_exp_->type()->size()));
    else if (left_exp_->get_var()->symbol()->is_arg()) {
        set_symbol(new VarSym(lineno(), IntType::get()));
        ConstSym* offset = ConstSym::get(ADDR_BYTE_SIZE * left_exp_->index());
        code->emit_aref(symbol(), left_exp_->get_var()->symbol(), offset);
    }
}

// inter_jump methods for expressions evaluated within an if
void Exp::inter_jump(SymTab* tab, Code* code, LabelId ok_lbl, LabelId ko_lbl) {
    if (is_const()) {
        if (value())
            code->emit_jump(ok_lbl);
        else
            code->emit_jump(ko_lbl);
    }
    inter(tab, code);
    code->emit_binif(OP_NE, symbol(), ConstSym::get(0), ok_lbl);
    code->emit_jump(ko_lbl);
}

void RelExp::inter_jump(SymTab* tab, Code* code, LabelId ok_lbl, LabelId ko_lbl) {
    if (is_const()) {
        if (value())
            code->emit_jump(ok_lbl);
        else
            code->emit_jump(ko_lbl);
    }
    exp1_->inter(tab, code);
    exp2_->inter(tab, code);
    Symbol* cs1 = convert_type_(code, exp1_->symbol(), type());
    Symbol* cs2 = convert_type_(code, exp2_->symbol(), type());
    code->emit_binif(tiny_op_, cs1, cs2, ok_lbl);
    code->emit_jump(ko_lbl);
}

void NotExp::inter_jump(SymTab* tab, Code* code, LabelId ok_lbl, LabelId ko_lbl) {
    if (is_const()) {
        if (value())
            code->emit_jump(ok_lbl);
        else
            code->emit_jump(ko_lbl);
    }
    exp_->inter(tab, code);
    code->emit_binif(OP_EQ, exp_->symbol(), ConstSym::get(0), ok_lbl);
    code->emit_jump(ko_lbl);
}

///////////////////////////////////////////////////////////////////////////////
// THE GREAT HARVEST PHASE
///////////////////////////////////////////////////////////////////////////////

void Node::pluck(vector<Code*>* crop) {
}

void Program::pluck(vector<Code*>* crop) {
    for_each(decs_.begin(), decs_.end(), bind2nd(mem_fun(&Dec::pluck), crop));
}

void FunDec::pluck(vector<Code*>* crop) {
    crop->push_back(code_);
}
