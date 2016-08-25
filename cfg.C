#include "cfg.H"
#include "error.H"
#include <iostream>
#include <sstream>
#include <algorithm>
using std::copy;
using std::mem_fun;
using std::set_union;
using std::set_difference;
using std::inserter;
using std::equal;
using std::pair;
using std::make_pair;
using std::find_if;
using std::bind2nd;

using std::cout;
using std::endl;

using std::ostringstream;

///////////////////////////////////////////////////////////////////////////////
// endless debugging...
///////////////////////////////////////////////////////////////////////////////
void SHOW(const string& s, const SymbolSet& ss) {
    std::cout << s << ": ";
    SymbolSet::const_iterator spp;
    for (spp = ss.begin(); spp != ss.end(); ++spp)
        std::cout << (*spp)->id() << " ";
    std::cout << std::endl;
}

///////////////////////////////////////////////////////////////////////////////
// BasicBlock regular methods
///////////////////////////////////////////////////////////////////////////////

BasicBlock::BasicBlock(LabelId l, CFG* c) : label_(l), cfg_(c) {
    // a symbol set corresponds to a point in the basic block
    // there is one point more than there are instructions...
    live_symbols_.push_back(SymbolSet());
    // initialize the registers.
    registers_.insert(Reg::get("%eax"));
    registers_.insert(Reg::get("%ebx"));
    registers_.insert(Reg::get("%ecx"));
    registers_.insert(Reg::get("%edx"));
    registers_.insert(Reg::get("%edi"));
    registers_.insert(Reg::get("%esi"));
}

void BasicBlock::add_instruction(const Instr& instr) {
    instructions_.push_back(instr);
    live_symbols_.push_back(SymbolSet()); // keep both in sync
}

void BasicBlock::add_incoming_edge(Edge* ie) {
    if (this != ie->to()) {
        say_unexpected(0, "adding an incoming edge");
        exit(1);
    }
    incoming_edges_.push_back(ie);
}

void BasicBlock::add_outgoing_edge(Edge* oe) {
    if (this != oe->from()) {
        say_unexpected(0, "adding an outgoing edge");
        exit(1);
    }
    outgoing_edges_.push_back(oe);
}

void BasicBlock::display() const {
    if (label())
        std::cout << label() << ":" << std::endl;
    vector<Instr>::const_iterator it;
    for (it = instructions_.begin(); it != instructions_.end(); ++it)
        std::cout << *it << std::endl;
    std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
}

///////////////////////////////////////////////////////////////////////////////
// CFG regular methods
///////////////////////////////////////////////////////////////////////////////
bool dedup_code(const Instr& i1, const Instr& i2);

CFG::CFG(Code* code) : 
    byte_size_(code->byte_size()), function_name_(code->function_name())
{
    vector<Instr> instructions;
    vector<Instr>::iterator it;

    instructions = code->instructions();
    it = unique(instructions.begin(), instructions.end(), dedup_code);
    instructions.erase(it, instructions.end());

    basic_blocks_.push_back(new BasicBlock(0, this));

    for (size_t i = 0; i < instructions.size(); ++i) {
        Instr curr_instr = instructions[i];
        if (curr_instr.is_label()) {
            LabelId l = curr_instr.label();
            BasicBlock* bb = new BasicBlock(l, this);
            basic_blocks_.push_back(bb);
            basic_block_map_[l] = bb;
        }
        else if (i != 0) {
            Instr prev_instr = instructions[i-1];
            if (prev_instr.is_jump())
                basic_blocks_.push_back(new BasicBlock(0, this));
            basic_blocks_.back()->add_instruction(curr_instr);
        }
        else {
            basic_blocks_.push_back(new BasicBlock(0, this));
            basic_blocks_.back()->add_instruction(curr_instr);
        }
    }

    
    if (basic_blocks_.back()->size() != 0)
        basic_blocks_.push_back(new BasicBlock(0, this));
    basic_block_map_[0] = basic_blocks_.back();

    initialize_edges_();
}

void CFG::initialize_edges_() {
    for (size_t i = 1; i < basic_blocks_.size() - 1; ++i) {
        BasicBlock* curr_bb = basic_blocks_[i];
        BasicBlock* next_bb = basic_blocks_[i+1];
        const Instr& instr = curr_bb->instructions().back();

        if (instr.is_jump()) {
            BasicBlock* dest_bb = basic_block_map_[instr.label()];
            Edge* e = new Edge(curr_bb, dest_bb);
            curr_bb->add_outgoing_edge(e);
            dest_bb->add_incoming_edge(e);
        }
        if (! instr.is_unconditional_jump()) {
            Edge* e = new Edge(curr_bb, next_bb);
            curr_bb->add_outgoing_edge(e);
            next_bb->add_incoming_edge(e);
        }
    }
}

bool dedup_code(const Instr& i1, const Instr& i2) {
    if (i1.is_label() && i2.is_label() && i1.label() == i2.label())
        return true;
    if (i1.op() == OP_JMP && !i2.is_label())
        return true;
    return false;
}

void CFG::display() const {
    cout << function_name_ << ":" << endl;
    for_each(basic_blocks_.begin(), basic_blocks_.end(),
             mem_fun(&BasicBlock::display));
}

LabelId CFG::end_label() const {
    map<LabelId, BasicBlock*>::const_iterator bbp = basic_block_map_.find(0);
    if (bbp != basic_block_map_.end()) {
        BasicBlock* lbb = bbp->second;
        return lbb->label();
    }
    say_unexpected(0, "CFG::end_label, invalid CFG");
    exit(1);
}

///////////////////////////////////////////////////////////////////////////////
// DAG regular methods
///////////////////////////////////////////////////////////////////////////////

Symbol* DAG::primary_symbol() const {
    if (!var_symbols_.empty()) {
        return var_symbols_.front();
    }
    else if (!tmp_symbols_.empty()) {
        return tmp_symbols_.front();
    }
    else if (!const_symbols_.empty()) {
        return const_symbols_.front();
    }
    else {
        return 0;
    }
}

vector<Symbol*> DAG::secondary_symbols() const {
    vector<Symbol*> symbols;
    if (!var_symbols_.empty()) {
        symbols.resize(var_symbols_.size() - 1);
        copy(var_symbols_.begin() + 1, var_symbols_.end(), symbols.begin());
    }
    return symbols;
}

bool DAG::valid() const {
    return valid_ && dag1_->valid() && dag2_->valid();
}

void DAG::invalidate() {
    valid_ = false;
}

void DAG::add(Symbol* s) {
    if (s->is_temp())
        tmp_symbols_.push_back(s);
    else if (s->is_const())
        const_symbols_.push_back(s);
    else
        var_symbols_.push_back(s);
}

///////////////////////////////////////////////////////////////////////////////
// Basic block optimization
// Algorithm 8 p. 95
// Algorithm 9 p. 99
///////////////////////////////////////////////////////////////////////////////

void process_instruction_(const Instr&, 
                          vector<DAG*>&, 
                          map<Symbol*, DAG*>&,
                          map<Symbol*, DAG*>&);

void process_node_(const DAG*, vector<Instr>&);
DAG* find_or_create_(Symbol*, map<Symbol*, DAG*>&, map<Symbol*, DAG*>&);
DAG* find_node_(TINY_OP, DAG*, DAG*, const vector<DAG*>&);
void invalidate_nodes_(Symbol*, vector<DAG*>&);

void CFG::optimize_basic_blocks() {
    for_each(basic_blocks_.begin(), basic_blocks_.end(),
             mem_fun(&BasicBlock::optimize));
}

void BasicBlock::optimize() {
    vector<DAG*> nodes;
    map<Symbol*, DAG*> nodes_map;
    map<Symbol*, DAG*> leafs_map;

    vector<Instr>::iterator it;
    for (it = instructions_.begin(); it != instructions_.end(); ++it)
        process_instruction_(*it, nodes, nodes_map, leafs_map);

    instructions_.clear();

    vector<DAG*>::iterator dt;
    for (dt = nodes.begin(); dt != nodes.end(); ++dt)
        process_node_(*dt, instructions_);

    for (dt = nodes.begin(); dt != nodes.end(); ++dt) {
        DAG* dp = *dt;
        if (dp)
            delete dp;
    }

    map<Symbol*, DAG*>::iterator p;
}

void process_instruction_(const Instr& instr,
                          vector<DAG*>& nodes,
                          map<Symbol*, DAG*>& nodes_map,
                          map<Symbol*, DAG*>& leafs_map)
{
    TINY_OP op = instr.op();
    Symbol* d = instr.dest();
    Symbol* a1 = instr.arg1();
    Symbol* a2 = instr.arg2();
    LabelId l  = instr.label();

    DAG* nd;
    DAG* d1 = a1 ? find_or_create_(a1, nodes_map, leafs_map) : 0;
    DAG* d2 = a2 ? find_or_create_(a2, nodes_map, leafs_map) : 0;

    switch(op) {
        case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV:
        case OP_NEG: case OP_CCI: case OP_CIC: case OP_AREF:
        {
            nd = find_node_(op, d1, d2, nodes);
            if (!nd) {
                nd = new DAG(op, d1, d2, l);
                nodes.push_back(nd);
            }
            nd->add(d);
            nodes_map[d] = nd;
            break;
        }
        case OP_ASET: case OP_ADDR:
        {
            nd = new DAG(op, d1, d2, l);
            nodes.push_back(nd);
            nd->add(d);
            nodes_map[d] = nd;
            invalidate_nodes_(d, nodes);
            break;
        }
        case OP_CP:
        {
            if (a1->is_const()) {
                nd = new DAG(op, d1, d2, l);
                nodes.push_back(nd);
                nd->add(d);
                nodes_map[d] = nd;
            }
            else {
                d1->add(d);
                nodes_map[d] = d1;
            }
            break;
        }
        case OP_CALL:
        {
            nd = new DAG(op, d1, d2, l);
            nodes.push_back(nd);
            nd->add(d);
            nodes_map[d] = nd;
            break;
        }
        case OP_READ:
        {
            nd = new DAG(op, d1, d2, l);
            nodes.push_back(nd);
            nd->add(d);
            nodes_map[d] = nd;
            break;
        }
        case OP_EQ: case OP_NE: case OP_G: case OP_L: case OP_NOT:
        case OP_JMP: case OP_RET: case OP_WRIT: case OP_PAR:
        {
            nd = new DAG(op, d1, d2, l);
            nodes.push_back(nd);
            break;
        }
        default:
            break;
    }
}

void process_node_(const DAG* node, vector<Instr>& instructions) {
    TINY_OP op = node->op();
    Symbol* d  = node->primary_symbol();
    Symbol* a1 = node->dag1() ? node->dag1()->primary_symbol() : 0;
    Symbol* a2 = node->dag2() ? node->dag2()->primary_symbol() : 0;
    LabelId l  = node->label();

    Instr instr = Instr(op, d, a1, a2, l);

    if (op != OP_LEAF)
        instructions.push_back(instr);

    vector<Symbol*> secondary_symbols = node->secondary_symbols();
    if (!secondary_symbols.empty()) {
        vector<Symbol*>::const_iterator s;
        for (s = secondary_symbols.begin(); s != secondary_symbols.end(); ++s)
            instructions.push_back(Instr(OP_CP, *s, d, 0, 0));
    }
}

DAG* find_or_create_(Symbol* s, 
                     map<Symbol*, DAG*>& nodes_map, 
                     map<Symbol*, DAG*>& leafs_map) 
{
    DAG* n;
    map<Symbol*, DAG*>::const_iterator pt = nodes_map.find(s);
    if (pt == nodes_map.end()) {
        n = new DAG(OP_LEAF, 0, 0, 0);
        n->add(s);
        nodes_map[s] = n;
        leafs_map[s] = n;
        return n;
    }
    else 
        return pt->second;
}

DAG* find_node_(TINY_OP op, DAG* d1, DAG* d2, const vector<DAG*>& nodes) {
    vector<DAG*>::const_iterator dt;
    for (dt = nodes.begin(); dt != nodes.end(); ++dt) {
        DAG* d = *dt;
        if (op == d->op() && d1 == d->dag1() && d2 == d->dag2())
            return d;
    }
    return 0;
}

void invalidate_nodes_(Symbol* s, vector<DAG*>& nodes) {
    vector<DAG*>::iterator it;
    for (it = nodes.begin(); it != nodes.end(); ++it) {
        DAG* d = *it;
        if (d->primary_symbol() == s)
            d->invalidate();
    }
}

///////////////////////////////////////////////////////////////////////////////
// Liveness Analysis
// Algorithm 14 p. 111
///////////////////////////////////////////////////////////////////////////////

pair<SymbolSet, SymbolSet> compute_gen_kill_(BasicBlock*);

void CFG::analyze_liveness() {
    map<BasicBlock*, pair<SymbolSet, SymbolSet> > gen_kill_map;

    vector<BasicBlock*>::const_iterator bt;
    for (bt = basic_blocks_.begin(); bt != basic_blocks_.end(); ++bt)
        gen_kill_map[*bt] = compute_gen_kill_(*bt);

    bool change = true;
    vector<BasicBlock*>::reverse_iterator br;
    while (change) {
        change = false;
        for (br = basic_blocks_.rbegin(); br != basic_blocks_.rend(); ++br) {
            SymbolSet gen = (gen_kill_map[*br]).first;
            SymbolSet kill = (gen_kill_map[*br]).second;
            bool result = (*br)->compute_in_out_live(gen, kill);
            if (result)
                change = true;
        }
    }
    // debug

    for (bt = basic_blocks_.begin(); bt != basic_blocks_.end(); ++bt)
        (*bt)->compute_live_symbols();
}

bool BasicBlock::compute_in_out_live(const SymbolSet& gen_live, 
                                     const SymbolSet& kill_live) 
{
    bool change = false;

    SymbolSet old_in  = in_live();
    SymbolSet old_out = out_live();
    SymbolSet temp_set;

    // out_live[b] = Union { in_live[s] | b < s }
    //-------------------------------------------------------------------------
    vector<Edge*>::const_iterator et;
    for (et = outgoing_edges_.begin(); et != outgoing_edges_.end(); ++et) {
        BasicBlock* successor = (*et)->to();
        SymbolSet successor_in = successor->in_live();
        set_union(out_live().begin(), out_live().end(),
                  successor_in.begin(), successor_in.end(),
                  inserter(out_live(), out_live().begin()));
    }
    if (!equal(out_live().begin(), out_live().end(), old_out.begin()))
        change = true;

    // in_live[b] = gen_live[b] Union { out_live[b] \ kill_live[b] }
    //-------------------------------------------------------------------------
    in_live().clear();
    set_difference(out_live().begin(), out_live().end(),
                   kill_live.begin(), kill_live.end(),
                   inserter(temp_set, temp_set.begin()));
    set_union(gen_live.begin(), gen_live.end(), 
              temp_set.begin(), temp_set.end(),
              inserter(in_live(), in_live().begin()));
    if (!equal(in_live().begin(), in_live().end(), old_in.begin()))
        change = true;

    return change;
}

void BasicBlock::compute_live_symbols() {
    for (int i = instructions_.size() - 1; i >= 0; --i) {
        Instr instr = instructions_[i];
        live_symbols_[i] = live_symbols_[i+1];

        Symbol* d = instr.dest();
        Symbol* a1 = instr.arg1();
        Symbol* a2 = instr.arg2();

        if (d) {
            SymbolSet::iterator st = live_symbols_[i].find(d); 
            if (st != live_symbols_[i].end())
                live_symbols_[i].erase(st);
        }
        if (a1 && (a1->is_var() || a1->is_arg()))
            live_symbols_[i].insert(a1);
        if (a2 && (a2->is_var() || a2->is_arg()))
            live_symbols_[i].insert(a2);
    }
}

pair<SymbolSet, SymbolSet> compute_gen_kill_(BasicBlock* bb) {

    SymbolSet killed;
    SymbolSet used;
    SymbolSet generated;

    vector<Instr> instructions = bb->instructions();
    for (int i = instructions.size() -1; i >= 0; --i) {

        Symbol* d = instructions[i].dest();
        Symbol* a1 = instructions[i].arg1();
        Symbol* a2 = instructions[i].arg2();


        if (d) {
            killed.insert(d);
        }
        if (a1 && (a1->is_var() || a1->is_arg())) {
            SymbolSet::iterator it = killed.find(a1);
            if (it != killed.end())
                killed.erase(it);
            used.insert(a1);
        }
        if (a2 && (a2->is_var() || a2->is_arg())) {
            SymbolSet::iterator it = killed.find(a2);
            if (it != killed.end())
                killed.erase(it);
            used.insert(a2);
        }
    }

    set_difference(used.begin(), used.end(), killed.begin(), killed.end(),
                   inserter(generated, generated.begin()));

    return make_pair(generated, used);
}

///////////////////////////////////////////////////////////////////////////////
// emitting assembly
// The final part of our first attempt to write a compiler
///////////////////////////////////////////////////////////////////////////////

// functions that will help us emit assembly code...

void generate_assembly(const Instr&, const SymbolSet&, const set<Reg*>&, LabelId);

Reg* pick_reg(const set<Reg*>&, const SymbolSet&);

void cleanup_reg(Reg*); // a simpler version of the above

void cleanup_symbol(Symbol*);

void emit_store(Symbol*, Reg*);

void emit_load(Symbol*, Reg*);

void emit_move(Reg*, Reg*);

void emit_copy(Symbol*, Symbol*);

string get_address(Symbol*);


template<class T>
set<T> diff_set(set<T> s1, set<T> s2) {
    set<T> sr;
    set_difference(s1.begin(), s1.end(), s2.begin(), s2.end(),
                   inserter(sr, sr.begin())); 
    return sr;
}


int bytes_to_words(int bb) {
    int q = bb/4;
    int r = bb%4;
    return r == 0 ? q : q + 1;
}

void CFG::emit_assembly() {
    int reserve_size = byte_size() + 4;
    string entry_label = function_name_ + ":";
    string exit_label  = "exit_" + entry_label;
    string zz = "  ";

    // entry code
    cout << entry_label << endl;
    cout << zz << "pushl %ebp" << endl;
    cout << zz << "movl %esp, %ebp" << endl;
    cout << zz << "subl $" << reserve_size << ", %esp" << endl;

    cout << zz << "pushl %eax" << endl;
    cout << zz << "pushl %ebx" << endl;
    cout << zz << "pushl %ecx" << endl;
    cout << zz << "pushl %edx" << endl;
    cout << zz << "pushl %edi" << endl;
    cout << zz << "pushl %esi" << endl;

    // translate each basic block
    vector<BasicBlock*>::iterator bbpp;
    for (bbpp = basic_blocks_.begin(); bbpp != basic_blocks_.end(); ++bbpp)
        (*bbpp)->emit_assembly();
    
    // exit code
    cout << zz << "popl %esi" << endl;
    cout << zz << "popl %edi" << endl;
    cout << zz << "popl %edx" << endl;
    cout << zz << "popl %ecx" << endl;
    cout << zz << "popl %ebx" << endl;
    cout << zz << "popl %eax" << endl;

    cout << zz << "movl %ebp, %esp" << endl;
    cout << zz << "popl %ebp" << endl;
    cout << zz << "ret" << endl;
}

void BasicBlock::emit_assembly() {
    // cleanup the registers first ...
    set<Reg*>::iterator rpp;
    for (rpp = registers_.begin(); rpp != registers_.end(); ++rpp)
        cleanup_reg(*rpp);
    LabelId end_label = cfg_->end_label();
    // prelude - emit a label if necessary
    if (label_)
        cout << "L" << label_ << ":" << endl;
    for (size_t i = 0; i < instructions_.size(); ++i)
        generate_assembly(instructions_[i],
                          live_symbols_[i+1],
                          registers_,
                          end_label);
}

void generate_assembly(const Instr& instr, const SymbolSet& livess, const
set<Reg*>& regs, LabelId end_label) {

    string zz = "  ";

    string od;  // will refer to d
    string o1;  // will refer to a1
    string o2;  // will refer to a2

    TINY_OP op = instr.op();
    Symbol* d  = instr.dest();
    Symbol* a1 = instr.arg1();
    Symbol* a2 = instr.arg2();
    LabelId l  = instr.label();

    Reg* r1 = 0;
    Reg* r2 = 0;
    Reg* rd = 0;

    if (a1 && !a1->is_const() && a1->registers().size() > 0) {
        r1 = *a1->registers().begin();
    }
    if (a2 && !a2->is_const() && a2->registers().size() > 0) {
        r2 = *a2->registers().begin();
    }
    if (d && d->registers().size() > 0) {
        rd = *d->registers().begin();
    }

    switch(op) {
        ////////////////////////////////////////////////////////////////////////
        // JMP L
        case OP_JMP:
        {
            cout << zz << "jmp L" << l << endl;
            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // D = A1 + A2
        case OP_ADD: case OP_SUB: case OP_MUL:
        {
            // rd should be a register
            if (rd);             
            else if (r1) rd = r1;
            else {
                set<Reg*> taboo;
                taboo.insert(r2);
                set<Reg*> rs = diff_set<Reg*>(regs, taboo);
                rd = pick_reg(rs, livess);
            }
            od = rd->name();
            
            // a1 should end up in rd
            if (r1)                     o1 = r1->name();
            else if (!a1->is_const())   o1 = get_address(a1);
            else                        o1 = "$" + a1->id();
            if (o1 != od) cout << zz << "movl " << o1 << ", " << od << endl;

            // a2 can be represented in whatever way i.e. I/M/R
            if (r2)                     o2 = r2->name();
            else if (!a2->is_const())   o2 = get_address(a2);
            else                        o2 = "$" + a2->id();

            if (op == OP_ADD) cout << zz << "addl " << o2 << ", " << od << endl;
            if (op == OP_SUB) cout << zz << "subl " << o2 << ", " << od << endl;
            if (op == OP_MUL) cout << zz << "imull " << o2 << ", " << od << endl;

            // cleanup + store...
            cleanup_symbol(d);
            cleanup_reg(rd);
            rd->add_symbol(d);
            d->add_register(rd);
            if (!d->is_temp()) emit_store(d, rd);

            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // D = A1 / A2
        case OP_DIV:
        {
            // rd should be %eax
            rd = Reg::get("%eax");
            od = rd->name();

            // a1 should end up in %eax
            if (r1)                   o1 = r1->name();
            else if (!a1->is_const()) o1 = get_address(a1);
            else                      o1 = "$" + a1->id();
            if (o1 != od) cout << zz << "movl " << o1 << ", " << od << endl;

            // for simplicity we take a2 to be in a register always
            if (r2  && r2 != Reg::get("%eax") && r2 != Reg::get("%edx"))
                o2 = r2->name();
            else { 
                Reg* r2_;
                set<Reg*> taboo;
                taboo.insert(Reg::get("%eax"));
                taboo.insert(Reg::get("%edx"));
                set<Reg*> rs = diff_set<Reg*>(regs, taboo);
                r2_ = pick_reg(rs, livess);
                cleanup_reg(r2_);
                if (r2)
                    cout << "movl " << r2->name() << ", " << r2_->name() << endl;
                else
                    emit_load(a2, r2_);
                r2_->add_symbol(a2);
                a2->add_register(r2_);
                o2 = r2_->name();
            }

            cout << zz << "cdq" << endl;
            cout << zz << "divl " << o2 << endl;

            // cleanup + store ...
            cleanup_symbol(d);
            cleanup_reg(Reg::get("%eax"));
            cleanup_reg(Reg::get("%edx"));
            Reg::get("%eax")->add_symbol(d);
            d->add_register(Reg::get("%eax"));
            if (!d->is_temp())
                emit_store(d, Reg::get("%eax"));

            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // IF== A1 A2 GOTO L
        case OP_EQ: case OP_NE: case OP_G: case OP_L:
        {
            // r1 should be a register    
            if (r1) o1 = r1->name();
            else { 
                set<Reg*> taboo;
                taboo.insert(r2);
                set<Reg*> rs = diff_set(regs, taboo);
                r1 = pick_reg(rs, livess);
                cleanup_reg(r1);
                emit_load(a1, r1);
                r1->add_symbol(a1);
                a1->add_register(r1);
                o1 = r1->name();
            }

            // a2 can be whatever it wants I/M/R
            if (r2)                     o2 = r2->name();
            else if (!a2->is_const())   o2 = get_address(a2);
            else                        o2 = "$" + a2->id();

            cout << zz << "cmpl " << o2 << ", " << o1 << endl;

            if      (op == OP_EQ) cout << zz << "je L" << l << endl;
            else if (op == OP_NE) cout << zz << "jne L" << l << endl;
            else if (op == OP_G)  cout << zz << "jg L" << l << endl;
            else if (op == OP_L)  cout << zz << "jl L" << l << endl;

            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // D = A1
        case OP_CP:
        {
            if (rd) od = rd->name();
            else if (r1) {
                r1->add_symbol(d);
                d->add_register(r1);
                if (!d->is_temp()) 
                    emit_store(d, r1);
            }
            else {
                if (!d->is_temp()) {
                    od = get_address(d);
                }
                else {
                    set<Reg*> taboo, rs;
                    taboo.insert(Reg::get("%edi"));
                    taboo.insert(Reg::get("%esi"));
                    rs = diff_set(regs, rs);
                    rd = pick_reg(rs, livess);
                    cleanup_reg(rd);
                    cleanup_symbol(d);
                    rd->add_symbol(d);
                    d->add_register(rd);
                    od = rd->name();
                }
                o1 = a1->is_const() ? "$" + a1->id() : get_address(a1);
                if (a1->type() == IntType::get())
                    cout << zz << "movl " << o1 << ", " << od << endl;
                else {
                    cout << zz << "movb " << o1 << ", " << od << endl;
                }
            }
            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // PAR A1
        case OP_PAR:
        {
                if (r1)
                    cout << zz << "pushl " << r1->name() << endl;
                else if (!a1->is_const()) {
                    if (a1->type() == ChrType::get()) {
                        // first ``convert'' to an integer
                        r1 = pick_reg(regs, livess);
                        string oa1 = get_address(a1);
                        string oi1 = r1->name();
                        string ob1 = r1->byte_name();
                        cout << zz << "andl $0, " << oi1 << endl;
                        cout << zz << "movb " << oa1 << ", " << ob1 << endl;
                        cout << zz << "pushl " << oi1 << endl;
                    }
                    else
                        cout << zz << "pushl " << get_address(a1) << endl;
                }
                else
                    cout << zz << "pushl $" << a1->id() << endl;
            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // D = CALL A1 A2
        case OP_CALL:
        {
            if (rd)                           ; // OK
            else {
                set<Reg*> taboo, rs;
                taboo.insert(Reg::get("%edi"));
                taboo.insert(Reg::get("%esi"));
                rs = diff_set(regs, taboo);
                rd = pick_reg(rs, livess);
            } 
            cleanup_reg(rd);
            cleanup_symbol(d);
            rd->add_symbol(d);
            d->add_register(rd);
          
            cout << zz << "call " << a1->id() << endl;
            if (d->type() == IntType::get())
                cout << zz << "movl -12(%esp), " << rd->name() << endl;
            else
                cout << zz << "movb -12(%esp), " << rd->byte_name() << endl;

            int argc = a2->int_value();
            if (argc > 0) {
                int bytes = 4 * argc;
                cout << zz << "addl $" << bytes << ", %esp" << endl;
            }
            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // RETURN A1
        case OP_RET:
        {
            if (r1) o1 = r1->name();
            else {
                set<Reg*> taboo, rs;
                taboo.insert(Reg::get("%edi"));
                taboo.insert(Reg::get("%esi"));
                rs = diff_set(regs, taboo);
                r1 = pick_reg(rs, livess);
                cleanup_reg(r1);
                emit_load(a1, r1);
                o1 = r1->name();
            }
            cout << zz << "movl " << o1 << ", -4(%ebp)" << endl;
            cout << zz << "jmp L" << end_label << endl;
            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // D = -A1
        case OP_NEG:
        {
            if (rd)         od = rd->name();
            else if (r1)    od = r1->name();
            else {
                rd = pick_reg(regs, livess);
                od = rd->name();
            }

            if (r1)                     o1 = r1->name();
            else if (a1->is_const())    o1 = "$" + a1->id();
            else                        o1 = get_address(a1);

            if (o1 != od) cout << zz << "movl " << o1 << ", " << od << endl;
            cout << zz << "negl " << od << endl;

            cleanup_reg(rd);
            cleanup_symbol(d);
            rd->add_symbol(d);
            d->add_register(rd);
            if (!d->is_temp())
                emit_store(d, rd);
            
            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // D = ADDR A1 
        case OP_ADDR:
        {
            rd = pick_reg(regs, livess);
            od = rd->name();
            o1 = get_address(a1);
            cleanup_reg(rd);
            cleanup_symbol(d);
            rd->add_symbol(d);
            d->add_register(rd);
            cout << zz << "leal " << o1 << ", " << od << endl;
            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // D = A1[A2]
        // movl (%base, %index, $1), addr(D)
        case OP_AREF:
        {
            if (!r1) { 
                string base_addr = get_address(a1);
                set<Reg*> taboo;
                taboo.insert(r2);
                set<Reg*> rs = diff_set(regs, taboo);
                r1 = pick_reg(rs, livess);
                cleanup_reg(r1);
                r1->add_symbol(a1);
                a1->add_register(r1);
                o1 = r1->name();
                if (a1->is_var())
                    cout << zz << "leal " << base_addr << ", " << o1 << endl;
                else if (a1->is_arg())
                    cout << zz << "movl " << base_addr << ", " << o1 << std::endl;
            }
            o1 = r1->name();

            if (!r2) {
                set<Reg*> taboo;
                taboo.insert(r1);
                set<Reg*> rs = diff_set(regs, taboo);
                r2 = pick_reg(rs, livess);
                cleanup_reg(r2);
                emit_load(a2, r2);
            }
            o2 = r2->name();

            if (!rd) {
                set<Reg*> taboo;
                taboo.insert(r1);
                taboo.insert(r2);
                taboo.insert(Reg::get("%edi"));
                taboo.insert(Reg::get("%esi"));
                set<Reg*> rs = diff_set(regs, taboo);
                rd = pick_reg(rs, livess);
            }
            if (!a1->is_global()) {
               LabelId l = get_next_label_id();
               cout << zz << "cmpl $0, " << o2 << endl; 
               cout << zz << "jl L" << l << std::endl;
               cout << zz << "negl " << o2 << endl;
               cout << "L" << l << ":" << endl;
            }
            od = rd->name();
            if (d->type() == IntType::get())
                cout << zz << "movl (" << o1 << ", " << o2 << ", 1), " << od << endl;
            else {
                od = rd->byte_name();
                cout << zz << "movb (" << o1 << ", " << o2 << ", 1), " << od << endl;
            }

            cleanup_symbol(d);
            cleanup_reg(rd);
            rd->add_symbol(d);
            d->add_register(rd);
            if (!d->is_temp()) 
                emit_store(d, rd);

            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // D[A1] = A2
        case OP_ASET:
        {
            // rd will hold the base address
            if (!rd && d->is_var()) {
                string base_addr;
                set<Reg*> taboo;
                taboo.insert(r1);
                taboo.insert(r2);
                set<Reg*> rs = diff_set(regs, taboo);
                rd = pick_reg(rs, livess);
                od = rd->name();
                base_addr = get_address(d);
                cleanup_reg(rd);
                od = rd->name();
                cout << zz << "leal " << base_addr << ", " << od << std::endl;
            }
            else if (!rd && d->is_arg()) {
                string base_addr = get_address(d);
                set<Reg*> taboo;
                taboo.insert(r1);
                taboo.insert(r2);
                set<Reg*> rs = diff_set(regs, taboo);
                rd = pick_reg(rs, livess);
                od = rd->name();
                cleanup_reg(rd);
                cout << zz << "movl " << base_addr << ", " << od << std::endl;
            }
            od = rd->name();

            if (!r1) {
                set<Reg*> taboo;
                taboo.insert(rd);
                taboo.insert(r2);
                set<Reg*> rs = diff_set(regs, taboo);
                r1 = pick_reg(rs, livess);
                cleanup_reg(r1);
                emit_load(a1, r1);
            }
            o1 = r1->name();

            if (!d->is_global()) {
               LabelId l = get_next_label_id();
               cout << zz << "cmpl $0, " << o1 << endl; 
               cout << zz << "jl L" << l << std::endl;
               cout << zz << "negl " << o1 << endl;
               cout << "L" << l << ":" << endl;
            }

            if (r2)                     o2 = r2->name();
            else if (!a2->is_const())   o2 = get_address(a2);
            else                        o2 = "$" + a2->id();

            if (a2->type() == IntType::get())
                cout << zz << "movl " << o2 << ", (" << od << ", " << o1 << ", 1)" << endl;
            else {
                if (r2) o2 = r2->byte_name();
                cout << zz << "movb " << o2 << ", (" << od << ", " << o1 << ", 1)" << endl;
            }

            rd->add_symbol(d);
            d->add_register(rd);
            
            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // D = (int)A1
        case OP_CCI:
        {

            if (rd)      od = rd->byte_name();
            else if (r1) {
                rd = r1;
                od = r1->byte_name();
            }
            else {
                set<Reg*> taboo, rs;
                taboo.insert(Reg::get("%edi"));
                taboo.insert(Reg::get("%esi"));
                rs = diff_set(regs, taboo);
                rd = pick_reg(rs, livess);
                od = rd->byte_name();
            }

            if (r1)                     o1 = r1->byte_name();
            else if (a1->is_const())    o1 = "$" + a1->id();
            else if (!a1->is_temp())    o1 = get_address(a1);

            if (o1 != od) {
                cout << zz << "movb " << o1 << ", " << od << endl;
            }
            if (!rd)
                std::exit(0);
            cout << zz << "andl $255, " << rd->name() << endl;

            cleanup_reg(rd);
            cleanup_symbol(d);
            rd->add_symbol(d);
            d->add_register(rd);
            if (!d->is_temp()) 
                emit_store(d, rd);
                
            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // D = (char)A1
        case OP_CIC:
        {
            if (rd) ; // nothing
            if (r1) {
                rd = r1;
                if (!d->is_temp())
                    emit_store(d, rd);
            }
            else {
                set<Reg*> taboo, rs;
                taboo.insert(Reg::get("%edi"));
                taboo.insert(Reg::get("%esi"));
                rs = diff_set(regs, taboo);
                rd = pick_reg(rs, livess);
                if (!a1->is_temp())
                    emit_load(a1, rd);
            }
            cleanup_reg(rd);
            cleanup_symbol(d);
            rd->add_symbol(d);
            d->add_register(rd);

            if (!d->is_temp())
                emit_store(d, rd);

            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // WRITE A1
        case OP_WRIT:
        {
            if (r1)                   o1 = r1->name();
            else if (!a1->is_const()) o1 = get_address(a1);
            else                      o1 = "$" + a1->id();
            // call print_int or print_chr
            if (a1->type() == IntType::get()) {
                cout << zz << "pushl " << o1 << endl;
                cout << zz << "call print_int" << endl;
                cout << zz << "addl $4, %esp" << endl;
            }
            if (a1->type() == ChrType::get()) {
                if (r1) {
                    cout << zz << "andl $255, " << r1->name() << endl;
                    cout << zz << "pushl " << r1->name() << endl;
                }
                else {
                    set<Reg*> rs, taboo;
                    taboo.insert(Reg::get("%edi"));
                    taboo.insert(Reg::get("%esi"));
                    rs = diff_set(regs, taboo);
                    r1 = pick_reg(rs, livess);
                    string oa1 = get_address(a1);
                    string oi1 = r1->name();
                    string ob1 = r1->byte_name();
                    cout << zz << "andl $0, " << oi1 << endl;
                    cout << zz << "movb " << oa1 << ", " << ob1 << endl;
                    cout << zz << "pushl " << oi1 << endl;
                }
                cout << zz << "call print_chr" << endl;
                cout << zz << "addl $4, %esp" << endl;
            }
            break;
        }
        ////////////////////////////////////////////////////////////////////////
        // D = READ
        case OP_READ:
        {
            if (rd) ;
            else {
                set<Reg*> rs, taboo;
                taboo.insert(Reg::get("%edi"));
                taboo.insert(Reg::get("%esi"));
                rs = diff_set(regs, taboo);
                rd = pick_reg(rs, livess);
            }
            od = rd->name();

            if (d->type() == IntType::get()) {
                cout << zz << "call read_int" << endl;
                cout << zz << "movl -12(%esp), " << od << endl;
            }

            cleanup_symbol(d);
            cleanup_reg(rd);
            d->add_register(rd);
            rd->add_symbol(d);
            if (!d->is_temp())
                emit_store(d, rd);

            break;
        }
        default:
            break;
    }

}

Reg* pick_reg(const set<Reg*>& regs, const SymbolSet& livess) {
    // first look for an emtpy register
    set<Reg*>::const_iterator regpp = 
        find_if(regs.begin(), regs.end(), mem_fun(&Reg::empty));
    if (regpp != regs.end()) {
        return *regpp;
    }

    // then look for a dead register
    // can't use find_if anymore because the stupid STL cannot form a reference
    // of a reference >-(((
    for (regpp = regs.begin(); regpp != regs.end(); ++regpp)
        if ((*regpp)->is_dead(livess)) {
            return *regpp;
        }

    // just return some register
    // this is pretty crude and definitely suboptimal...
    return *regs.begin();
}

//---
// Don't use the iterator to delete!
//---
void cleanup_reg(Reg* r) {
    SymbolSet::iterator spp;
    for (spp = r->symbols().begin(); spp != r->symbols().end(); ) {
        Symbol* s = *spp;
        ++spp;
        r->rem_symbol(s);
        s->rem_register(r);
    }
}

void cleanup_symbol(Symbol* s) {
    set<Reg*>::iterator rpp;
    for (rpp = s->registers().begin(); rpp != s->registers().end(); ) {
        Reg* r = *rpp;
        ++rpp;
        s->rem_register(r);
        r->rem_symbol(s);
    }
    s->set_mem_valid(false);
}

void emit_store(Symbol* s, Reg* r) {
    string addr = get_address(s);
    string zz = "  ";
    if (s->type() == IntType::get())
        cout << zz << "movl " << r->name() << ", " << addr << endl;
    else
        cout << zz << "movb " << r->byte_name() << ", " << addr << endl;
}

void emit_load(Symbol* s, Reg* r) {
    string n1;
    // temporaries used for array references might be used...
    if (s->is_temp())
        return;
    if (!s->is_const())
        n1 = get_address(s);
    else 
        n1 = "$" + s->id();

    string zz = "  ";
    if (s->type() == IntType::get())
        cout << zz << "movl " << n1 << ", " << r->name() << endl;
    else
        cout << zz << "movb " << n1 << ", " << r->byte_name() << endl;
    if (!s->is_const()) {
        s->add_register(r);
        r->add_symbol(s);
    }
}

void emit_move(Reg* fr, Reg* tr) {
    string zz = "  ";
    cout << zz << "movl " << fr->name() << ", " << tr->name() << endl;
}

void emit_copy(Symbol* fs, Symbol* ts) {
    string zz = "  ";
    if (fs->is_const()) {
        // distinguish types svp
        cout << zz << "movl " << "$" << fs->id() << ", " << get_address(ts) << endl;
    }
    else {
        // distinguish types svp
        cout << zz << "movl " << get_address(fs) << ", " << get_address(ts) << endl;
    }
}

// this function never works for global symbols
// this should not be forgotten
// and maybe debugged!!!
string get_address(Symbol* s) {

    ostringstream oss;

    int i = s->index();
    int j;
    if (s->is_arg()) {
        j = 8 + i;
        oss << j << "(%ebp)";
        return oss.str();
    }
    if (s->is_var()) {
        j = -8 - i;
        oss << j << "(%ebp)";
        return oss.str();
    }
    
    say_unexpected(0, "get_address");
    exit(1);
}

///////////////////////////////////////////////////////////////////////////////
// emit_assembly
// prepare global variables
// emit the prelude
// call emit_assembly on the cfgs...
///////////////////////////////////////////////////////////////////////////////
void global_arr_init_(Symbol*);

void emit_assembly(SymTab* sym_tab, vector<CFG*> cfgs) {
    string zz = "  ";   // move this up somehow

    // Prepare global variables
    cout << zz << ".section .data" << endl;
    map<string, Symbol*> entries = sym_tab->entries();
    map<string, Symbol*>::const_iterator p;
    for (p = entries.begin(); p != entries.end(); ++p) {
        Symbol* sp = p->second;
        if (!sp->is_fun()) {
            cout << sp->id() << ":" << endl;
            if (sp->type() == ChrType::get())
                cout << zz << "byte" << endl;
            else if (sp->type() == IntType::get())
                cout << zz << "long" << endl;
            else 
                global_arr_init_(sp);
        }
    }
    
    // Emit the prelude
    cout << zz << ".section .text" << endl
         << zz << ".include \"print_int.s\"" << endl
         << zz << ".include \"read_int.s\"" << endl
         << zz << ".include \"print_chr.s\"" << endl
         << zz << ".include \"read_chr.s\"" << endl
         << zz << ".globl _start"   << endl
         << "_start:"               << endl
         << zz << "call tiny"       << endl
         << zz << "movl -12(%esp), %ebx" << endl
         << zz << "movl $1, %eax"   << endl
         << zz << "int $0x80"       << endl;

    // call emit_assembly for each cfg
    for_each(cfgs.begin(), cfgs.end(), mem_fun(&CFG::emit_assembly));
}

void global_arr_len_(Type* t) {
    string zz = "  ";
    if (! t->is_primitive()) {
        int size = t->size();
        cout << zz << ".long " << size << endl;
        global_arr_len_(t->element_type());
    }
}

void global_arr_init_(Symbol* sp) {
    string zz = "  ";
    Type* t = sp->type();
    global_arr_len_(t);
    int num_of_chrs = sp->type()->pure_byte_size();
    int num_of_ints = num_of_chrs/4;
    if (t->base_type() == ChrType::get()) {
        cout << zz << ".rept " << num_of_chrs << endl;
        cout << zz << ".byte 0" << endl;
    }
    if (t->base_type() == IntType::get()) {
        cout << zz << ".rept " << num_of_ints << endl;
        cout << zz << ".long 0" << endl;
    }
    cout << zz << ".endr" << endl;
} 
