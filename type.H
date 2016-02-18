#ifndef TYPE_H
#define TYPE_H

#include "util.H"
#include <list>
using std::list;

///////////////////////////////////////////////////////////////////////////////
// Type
///////////////////////////////////////////////////////////////////////////////
class Type {
public:
    Type() {}
    virtual ~Type() {}

    virtual bool  is_primitive()   const = 0;
    virtual int   dimension()      const = 0;
    virtual int   size()           const = 0;
    virtual int   byte_size()      const = 0;
    virtual int   pure_byte_size() const = 0;
    virtual Type* element_type()   const = 0;
    virtual Type* base_type()      const = 0;

    static Type* max_type(Type*, Type*);
    static bool types_match(const list<Type*>&, const list<Type*>&);

private:
    static bool type_match_(Type*, Type*);
};

///////////////////////////////////////////////////////////////////////////////
// IntType
///////////////////////////////////////////////////////////////////////////////
class IntType : public Type {
public:
    static IntType* get();

    bool  is_primitive()   const { return true;          }
    int   dimension()      const { return 0;             }
    int   size()           const { return 1;             }
    int   byte_size()      const { return INT_BYTE_SIZE; }
    int   pure_byte_size() const { return INT_BYTE_SIZE; }
    Type* element_type()   const { return instance_;     }
    Type* base_type()      const { return instance_;     }

private:
    static IntType* instance_;
    IntType() {}
};

///////////////////////////////////////////////////////////////////////////////
// ChrType
///////////////////////////////////////////////////////////////////////////////
class ChrType : public Type {
public:
    static ChrType* get();

    bool  is_primitive()   const { return true;          }
    int   dimension()      const { return 0;             }
    int   size()           const { return 1;             }
    int   byte_size()      const { return CHR_BYTE_SIZE; }
    int   pure_byte_size() const { return CHR_BYTE_SIZE; }
    Type* element_type()   const { return instance_;     }
    Type* base_type()      const { return instance_;     }

private:
    static ChrType* instance_;
    ChrType() {}
};

///////////////////////////////////////////////////////////////////////////////
// ArrType
///////////////////////////////////////////////////////////////////////////////
class ArrType : public Type {
public:
    ArrType(Type* t, int i) : element_type_(t), size_(i) {}

    bool  is_primitive()   const { return false; }
    int   dimension()      const { return element_type_->dimension() + 1; }
    int   size()           const { return size_; }
    int   byte_size()      const;
    int   pure_byte_size() const;
    Type* element_type()   const { return element_type_;              }
    Type* base_type()      const { return element_type_->base_type(); }

private:
    Type* element_type_;
    int size_;
};

#endif
