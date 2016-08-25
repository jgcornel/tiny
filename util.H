#ifndef UTIL_H
#define UTIL_H

#include <string>
using std::string;

const bool DEBUG = false;

const int CHR_BYTE_SIZE  = 1;
const int INT_BYTE_SIZE  = 4;
const int ADDR_BYTE_SIZE = 4;

typedef int LabelId;
LabelId get_next_label_id();

enum TINY_OP {
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_NEG,
    OP_EQ,
    OP_NE,
    OP_G,
    OP_L,
    OP_NOT,
    OP_CP,
    OP_JMP,
    OP_PAR,
    OP_CALL,
    OP_RET,
    OP_GET,
    OP_AREF,
    OP_ASET,
    OP_ADDR,
    OP_LBL,
    OP_CCI,
    OP_CIC,
    OP_READ,
    OP_WRIT,
    OP_LEAF
};

string TINY_OP_STR(TINY_OP);

#endif
