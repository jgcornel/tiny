#ifndef ERROR_H
#define ERROR_H

#include <string>
using std::string;

void say_bad_argument_count(int, const string&, int, int);
void say_bad_argument_type(int, const string&, int);
void say_bad_array_size(int);
void say_bad_assignee_type(int);
void say_bad_assigner_type(int);
void say_bad_condition_type(int);
void say_bad_function_type(int, const string&);
void say_bad_indexee_type(int);
void say_bad_indexer_type(int);
void say_bad_length_type(int);
void say_bad_read_type(int);
void say_bad_return_type(int);
void say_bad_subexpr_type(int);
void say_bad_tiny_type(int);
void say_bad_write_type(int);
void say_not_a_function(int, const string&, int);
void say_not_a_variable(int, const string&, int);
void say_no_tiny (int);
void say_redeclared(int, const string&, int);
void say_undeclared(int, const string&);
void say_unexpected(int, const string&);

#endif
