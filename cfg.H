#ifndef CFG_H
#define CFG_H

#include "instr.H"
#include "reg.H"
#include "symtab.H"
#include <vector>
#include <set>
#include <string>
#include <stack>
using std::vector;
using std::set;
using std::string;
using std::stack;

typedef set<Symbol*> SymbolSet;

class BasicBlock;
class Edge;
class CFG;
class DAG;

void emit_assembly(SymTab*, vector<CFG*>);

class BasicBlock {
public:
    BasicBlock(LabelId, CFG*);

    LabelId label() const { return label_; }
    const vector<Instr>& instructions() const { return instructions_; }
    const vector<Edge*>& incoming_edges() const { return incoming_edges_; }
    const vector<Edge*>& outgoing_edges() const { return outgoing_edges_; }
    const vector<SymbolSet>& live_symbols() const { return live_symbols_; }

    size_t size() const { return instructions_.size(); }

    const SymbolSet& in_live() const { return live_symbols_.front(); }
    const SymbolSet& out_live() const { return live_symbols_.back(); }
    SymbolSet& in_live() { return live_symbols_.front(); }
    SymbolSet& out_live() { return live_symbols_.back(); }

    void add_instruction(const Instr&);
    void add_incoming_edge(Edge*);
    void add_outgoing_edge(Edge*);

    void optimize();
    bool compute_in_out_live(const SymbolSet&, const SymbolSet&);
    void compute_live_symbols();

    void emit_assembly();

    void display() const;

private:
    LabelId label_;
    CFG* cfg_;
    vector<Instr> instructions_;

    vector<Edge*> incoming_edges_;
    vector<Edge*> outgoing_edges_;

    vector<SymbolSet> live_symbols_;

    set<Reg*> registers_;
};

class Edge {
public:
    Edge(BasicBlock* f, BasicBlock* t) : from_(f), to_(t) {}

    BasicBlock* from() const { return from_; }
    BasicBlock* to() const { return to_; }

private:
    BasicBlock* from_;
    BasicBlock* to_;
};

class CFG {
public:
    CFG(Code*);

    int byte_size() const { return byte_size_; }
    const string& function_name() const { return function_name_; }
    const vector<BasicBlock*>& basic_blocks() const { return basic_blocks_; }

    void optimize_basic_blocks();
    void analyze_liveness();

    void emit_assembly();
    
    void display() const;

    LabelId end_label() const;

private:
    int byte_size_;
    string function_name_;

    vector<BasicBlock*> basic_blocks_;
    map<LabelId, BasicBlock*> basic_block_map_;

    stack<string> pop_stack_; // goal : pop off parameters after pushing...

    void initialize_edges_();    
};

class DAG {
public:
    DAG(TINY_OP o, DAG* d1, DAG* d2, LabelId l) :
        op_(o), dag1_(d1), dag2_(d2), label_(l), valid_(true) {}

    TINY_OP op() const { return op_; }
    const DAG* dag1() const { return dag1_; }
    const DAG* dag2() const { return dag2_; }
    LabelId label() const { return label_; }

    const vector<Symbol*> var_symbols() const { return var_symbols_; }
    const vector<Symbol*> tmp_symbols() const { return tmp_symbols_; }
    const vector<Symbol*> const_symbols() const { return const_symbols_; }

    Symbol* primary_symbol() const;
    vector<Symbol*> secondary_symbols() const;
    bool valid() const;
    void invalidate();
    void add(Symbol*);

private:
    TINY_OP op_;
    DAG* dag1_;
    DAG* dag2_;
    LabelId label_;

    bool valid_;

    vector<Symbol*> var_symbols_;
    vector<Symbol*> tmp_symbols_;
    vector<Symbol*> const_symbols_;
};

#endif
