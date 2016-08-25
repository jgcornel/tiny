//---------------------------------------------------------------------------//
//                                                                           //
// tiny.y                                                                    //
// Author: Jan Cornelis                                                      //
// Date  : July 2009                                                         //
//                                                                           //
// Provides the parser specification for our tiny compiler.                  //
//                                                                           //
//---------------------------------------------------------------------------//

%{

#include <iostream>
#include <stack>
#include <string>
#include <vector>
#include <cstdlib>
#include "error.H"
#include "ast.H"
#include "cfg.H"

using std::vector;
using std::exit;

extern "C" {
    int yyparse(void);
    int yylex(void);
    char* yytext;
    int yywrap() { return 1; }
}

std::stack<Node*> nodes;
int lineno = 1;

void yyerror(const char* msg) {
    std::cerr << "On line " << lineno << ": " << msg << std::endl;
    exit(1);
}

int main(int argc, char* argv[]) {
    vector<Code*> codes;    // holds intermediate code for each function
    vector<CFG*>  cfgs;     // holds a cfg for each function
    vector<Code*>::iterator code;
    vector<CFG*>::iterator cfg; 

    // front end
    yyparse();
    Node* ast = nodes.top();
    SymTab* sym_tab = new SymTab(0, "__GLOB__", true);
    ast->build(sym_tab);
    ast->check(sym_tab);
    ast->label();
    ast->inter(sym_tab, 0);
    ast->pluck(&codes);     // pluck the fruit of your labour

    // back end
    for (code = codes.begin(); code != codes.end(); ++code) 
        cfgs.push_back(new CFG(*code));
    for (cfg = cfgs.begin(); cfg != cfgs.end(); ++cfg) {
        (*cfg)->optimize_basic_blocks();
        (*cfg)->analyze_liveness();
    }

    emit_assembly(sym_tab, cfgs);

    return 0;
}

template<class T>
T* get() {
    if (T* elt = dynamic_cast<T*>(nodes.top())) {
        nodes.pop();
    return elt;
    }
    else {
        say_unexpected(lineno, "get<T>() has failed");
        exit(1);
    }
}

%}

%token INT CHAR IF ELSE WHILE READ WRITE RETURN LENGTH
%token IDENTIFIER NUMBER QCHAR 
%token LPAR RPAR LBRACE RBRACE LBRACK RBRACK
%token ASSIGN SEMICOLON COMMA PLUS MINUS TIMES DIVIDE 
%token EQUAL NEQUAL GREATER LESS NOT

%left     EQUAL NEQUAL GREATER LESS
%left     PLUS MINUS
%left     TIMES DIVIDE
%nonassoc UMINUS

%%
program
: declaration_list
{
    Program* p = new Program(lineno);
    while (!nodes.empty())
        p->add(get<Dec>());
    nodes.push(p);
}
;

declaration_list
: declaration
| declaration_list declaration
;

declaration
: function_declaration
| variable_declaration
;

function_declaration
: type_name identifier LPAR parameter_list RPAR block
{
    Block* b = get<Block>();
    ParDecList* dd = get<ParDecList>();
    Id* id = get<Id>();
    TypeName* tn = get<TypeName>();
    nodes.push(new FunDec(id->lineno(), tn, id->str, dd->elems, b));
    delete id; delete dd; // throw away container objects
}
;

parameter_list
:
{
    nodes.push(new ParDecList(lineno));
}
| more_parameters
;

more_parameters
: parameter_declaration
{
    ParDec* d = get<ParDec>();
    ParDecList* dd = new ParDecList(lineno);
    dd->add(d);
    nodes.push(dd);
}
| more_parameters COMMA parameter_declaration
{
    ParDec* d = get<ParDec>();
    Node* n = nodes.top();
    if (ParDecList* dd = dynamic_cast<ParDecList*>(n))
        dd->add(d);
    else {
        say_unexpected(lineno, "dynamic_cast<ParDecList*> failed");
        exit(1);
    }
}
;

parameter_declaration
: type_name identifier
{
    Id* id = get<Id>();
    TypeName* tn = get<TypeName>();
    nodes.push(new ParDec(lineno, tn, id->str));
    delete id; // throw away container objects
}
;

variable_declaration_list
:
{
    nodes.push(new VarDecList(lineno));
}
| more_variable_declarations
;

more_variable_declarations
: variable_declaration
{
    VarDec* d = get<VarDec>();
    VarDecList* dd = new VarDecList(lineno);
    dd->add(d);
    nodes.push(dd);
}
| more_variable_declarations variable_declaration
{
    VarDec* d = get<VarDec>();
    Node* n = nodes.top();
    if (VarDecList* dd = dynamic_cast<VarDecList*>(n))
        dd->add(d);
    else {
        say_unexpected(lineno, "dynamic_cast<VarDecList*> failed");
        exit(1);
    }
}
;

variable_declaration
: type_name identifier SEMICOLON
{
    Id* id = get<Id>();
    TypeName* tn = get<TypeName>();
    nodes.push(new VarDec(lineno, tn, id->str));
    delete id; // throw away container objects
}
;

type_name
: INT
{
    nodes.push(new IntTypeName(lineno));
}
| CHAR
{
    nodes.push(new ChrTypeName(lineno));
}
| type_name LBRACK expression RBRACK
{
    Exp* e = get<Exp>();
    TypeName* tn = get<TypeName>();
    nodes.push(new ArrTypeName(lineno, tn, e));
}
;

statement_list
: statement
{
    Stm* s = get<Stm>();
    StmList* ss = new StmList(lineno);
    ss->add(s);
    nodes.push(ss);
}
| statement_list statement
{
    Stm* s = get<Stm>();
    Node* n = nodes.top();
    if (StmList* ss = dynamic_cast<StmList*>(n))
        ss->add(s);
    else  {
        say_unexpected(lineno, "dynamic_cast<StmList*> failed");
        exit(1);
    }
}
;

block
: LBRACE variable_declaration_list statement_list RBRACE
{
    StmList* ss = get<StmList>();
    VarDecList* dd = get<VarDecList>();
    nodes.push(new Block(lineno, dd->elems, ss->elems));
    delete dd; delete ss; // throw away container objects
}
;

statement
: IF LPAR expression RPAR statement
{
    Stm* s = get<Stm>();
    Exp* e = get<Exp>();
    nodes.push(new IfStm(lineno, e, s));
}
| IF LPAR expression RPAR statement ELSE statement
{
    Stm* s2 = get<Stm>();
    Stm* s1 = get<Stm>();
    Exp* e = get<Exp>();
    nodes.push(new ElseStm(lineno, e, s1, s2));
}
| WHILE LPAR expression RPAR statement
{
    Stm* s = get<Stm>();
    Exp* e = get<Exp>();
    nodes.push(new WhileStm(lineno, e, s));
}
| left_expression ASSIGN expression SEMICOLON
{
    Exp* e = get<Exp>();
    LeftExp* le = get<LeftExp>();
    nodes.push(new SetStm(lineno, le, e));
}
| RETURN expression SEMICOLON
{
    Exp* e = get<Exp>();
    nodes.push(new ReturnStm(lineno, e));
}
| identifier LPAR expression_list RPAR SEMICOLON
{
    ExpList* ee = get<ExpList>();
    Id* id = get<Id>();
    nodes.push(new FunStm(lineno, id->str, ee->elems));
    delete id; delete ee; // throw away container objects
}
| block
| WRITE expression SEMICOLON
{
    Exp* e = get<Exp>();
    nodes.push(new WriteStm(lineno, e));
}
| READ left_expression SEMICOLON
{
    LeftExp* le = get<LeftExp>();
    nodes.push(new ReadStm(lineno, le));
}
;

expression
: left_expression
| expression PLUS    expression
{
    Exp* e2 = get<Exp>();
    Exp* e1 = get<Exp>();
    TINY_OP op = OP_ADD;
    nodes.push(new BinExp(lineno, op, e1, e2));
}
| expression MINUS   expression
{
    Exp* e2 = get<Exp>();
    Exp* e1 = get<Exp>();
    TINY_OP op = OP_SUB;
    nodes.push(new BinExp(lineno, op, e1, e2));
}
| expression TIMES   expression
{
    Exp* e2 = get<Exp>();
    Exp* e1 = get<Exp>();
    TINY_OP op = OP_MUL;
    nodes.push(new BinExp(lineno, op, e1, e2));
}
| expression DIVIDE  expression
{
    Exp* e2 = get<Exp>();
    Exp* e1 = get<Exp>();
    TINY_OP op = OP_DIV;
    nodes.push(new BinExp(lineno, op, e1, e2));
}
| expression EQUAL   expression
{
    Exp* e2 = get<Exp>();
    Exp* e1 = get<Exp>();
    TINY_OP op = OP_EQ;
    nodes.push(new RelExp(lineno, op, e1, e2));
}
| expression NEQUAL  expression
{
    Exp* e2 = get<Exp>();
    Exp* e1 = get<Exp>();
    TINY_OP op = OP_NE;
    nodes.push(new RelExp(lineno, op, e1, e2));
}
| expression GREATER expression
{
    Exp* e2 = get<Exp>();
    Exp* e1 = get<Exp>();
    TINY_OP op = OP_G;
    nodes.push(new RelExp(lineno, op, e1, e2));
}
| expression LESS    expression
{
    Exp* e2 = get<Exp>();
    Exp* e1 = get<Exp>();
    TINY_OP op = OP_L;
    nodes.push(new RelExp(lineno, op, e1, e2));
}
| MINUS expression %prec UMINUS
{
    Exp* e = get<Exp>();
    TINY_OP op = OP_NEG;
    nodes.push(new NegExp(lineno, op, e));
}
| NOT expression %prec UMINUS
{
    Exp* e = get<Exp>();
    TINY_OP op = OP_NOT;
    nodes.push(new NotExp(lineno, op, e));
}
| LPAR expression RPAR
| NUMBER
{
    nodes.push(new IntExp(lineno, $1));
}
| identifier LPAR expression_list RPAR
{
    ExpList* ee = get<ExpList>();
    Id* id = get<Id>();
    nodes.push(new FunExp(lineno, id->str, ee->elems));
    delete id; delete ee; // throw away container objects
}
| QCHAR
{
    nodes.push(new ChrExp(lineno, yytext[1]));
}
| LENGTH left_expression
{
    LeftExp* le = get<LeftExp>();
    nodes.push(new LengthExp(lineno, le));
}
;

left_expression
: variable
| left_expression LBRACK expression RBRACK
{
    Exp* e = get<Exp>();
    LeftExp* le = get<LeftExp>();
    nodes.push(new ArrExp(lineno, le, e));
}
;

expression_list
:
{
    nodes.push(new ExpList(lineno));
}
| more_expressions
;

more_expressions
: expression
{
    Exp* e = get<Exp>();
    ExpList* ee = new ExpList(lineno);
    ee->add(e);
            nodes.push(ee);
        }
    | more_expressions COMMA expression
        {
            Exp* e = get<Exp>();
            Node* n = nodes.top();
            if (ExpList* ee = dynamic_cast<ExpList*>(n))
                ee->add(e);
            else {
                say_unexpected(lineno, "dynamic_cast<ExpList*> failed");
                exit(1);
            }
        }
    ;

variable
    : identifier
        {
            Id* id = get<Id>();
            nodes.push(new VarExp(lineno, id->str));
            delete id; // throw away container object
        }
    ;

identifier
    : IDENTIFIER 
        {
            nodes.push(new Id(lineno, std::string(yytext)));
        }
    ;
%%
