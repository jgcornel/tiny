#include "util.H"

LabelId get_next_label_id() {
    static LabelId i = 0;
    return ++i;
}

string TINY_OP_STR(TINY_OP op) {
    switch(op) {
        case OP_ADD  : return string(" + ");
        case OP_SUB  : return string(" - ");
        case OP_MUL  : return string(" * ");
        case OP_DIV  : return string(" / ");
        case OP_NEG  : return string("-");
        case OP_EQ   : return string("IF== ");
        case OP_NE   : return string("IF!= ");
        case OP_G    : return string("IF>  ");
        case OP_L    : return string("IF<  ");
        case OP_NOT  : return string("!");
        case OP_JMP  : return string("GOTO ");
        case OP_PAR  : return string("PARAM ");
        case OP_CALL : return string("CALL ");
        case OP_RET  : return string("RETURN ");
        case OP_GET  : return string("GET ");
        case OP_LBL  : return string("L");
        case OP_CCI  : return string(" CCI ");
        case OP_CIC  : return string(" CIC ");
        case OP_READ : return string(" READ ");
        case OP_WRIT : return string("WRITE ");
        case OP_ADDR : return string(" ADDRESS ");
        default      : return string();
    }
}
