#include "error.H"
#include <iostream>
using std::cerr;
using std::endl;

void say_bad_argument_count(int i, const string& id, int m, int n) {
    cerr << i << ": error: "
         << "function " << id << " expects " << m << " arguments "
         << "but got " << n << endl;
}

void say_bad_argument_type(int i, const string& id, int j) {
    cerr << i << ": error: "
         << "an argument of function " << id
         << " that was declared on line " << j
         << " has a wrong type" << endl;
}

void say_bad_array_size(int i) {
    cerr << i << ": error: "
         << "cannot declare an array using a non const expression" << endl;
}

void say_bad_assignee_type(int i) {
    cerr << i << ": error: "
         << "cannot assign to an lvalue "
         << "that does not have a primitive type" << endl;
}

void say_bad_assigner_type(int i) {
    cerr << i << ": error: "
         << "cannot assign a value that does not have a primitive type" << endl;
}

void say_bad_condition_type(int i) {
    cerr << i << ": error: "
         << "condition does not have a primitive type" << endl;
}

void say_bad_function_type(int i, const string& id) {
    cerr << i << ": error: "
         << "function " << id 
         << " does not have a primitive return type" << endl;
}

void say_bad_indexee_type(int i) {
    cerr << i << ": error: "
         << "cannot index an lvalue that has a primitive type" << endl;
}

void say_bad_indexer_type(int i) {
    cerr << i << ": error: "
         << "cannot use a values that does not have a primitive type "
         << "as an index" << endl;
}

void say_bad_length_type(int i) {
    cerr << i << ": error: "
         << "cannot get the length of an lvalue "
         << "that has a primitive type" << endl;
}

void say_bad_read_type(int i) {
    cerr << i << ": error: "
         << "cannot read into an lvalue "
         << "that does not have a primitive type" << endl;
}

void say_bad_return_type(int i) {
    cerr << i << ": error: "
         << "cannot return a value that does not have a primitive type" << endl;
}

void say_bad_subexpr_type(int i) {
    cerr << i << ": error: "
         << "a subexpression does not have a primitive type" << endl;
}

void say_bad_tiny_type(int i) {
    cerr << i << ": error: "
         << "tiny should be declared as ``int tiny()''" << endl;
}

void say_bad_write_type(int i) {
    cerr << i << ": error: "
         << "cannot write a value that does not have a primitive type" << endl;
}

void say_not_a_function(int i, const string& id, int j) {
    cerr << i << ": error: "
         << id << " has not been declared as a function on line " << j << endl;
}

void say_not_a_variable(int i, const string& id, int j) {
    cerr << i << ": error: "
         << id << " has not been declared as a variable on line " << j << endl;
}

void say_no_tiny(int i) {
    cerr << i << ": error: "
         << "a function ``int tiny()'' has not been declared" << endl;
}

void say_redeclared(int i, const string& id, int j) {
    cerr << i << ": error: "
         << id << " has already been declared on line " << j << endl;
}

void say_undeclared(int i, const string& id) {
    cerr << i << ": error: "
         << id << " has not been declared" << endl;
}

void say_unexpected(int i, const string& msg) {
    cerr << i << ": fatal: "
         << "an unexpected error has ocurred: " << msg << endl;
}
