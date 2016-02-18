#include "type.H"
using std::equal;

IntType* IntType::instance_ = 0;
ChrType* ChrType::instance_ = 0;

IntType* IntType::get() {
    if (!instance_)
        instance_  = new IntType();
    return instance_;
}

ChrType* ChrType::get() {
    if (!instance_)
        instance_ = new ChrType();
    return instance_;
}

int ArrType::pure_byte_size() const {
    return size_ * element_type_->pure_byte_size();
}

int ArrType::byte_size() const {
    return dimension() * ADDR_BYTE_SIZE + size_ * element_type_->pure_byte_size();
}

Type* Type::max_type(Type* type1, Type* type2) {
    if (!(type1->is_primitive() && type2->is_primitive()))
        return 0;
    return type1 == type2 ? type1 : IntType::get();
}

bool Type::type_match_(Type* type1, Type* type2) {
    return (type1->dimension() == 0 && type2->dimension() == 0) ||
           (type1->dimension() == type2->dimension() &&
            type1->base_type() == type2->base_type());
}

bool Type::types_match(const list<Type*>& types1, const list<Type*>& types2) {
    if (types1.size() != types2.size())
        return false;
    return equal(types1.begin(), types1.end(), types2.begin(), type_match_);
}
