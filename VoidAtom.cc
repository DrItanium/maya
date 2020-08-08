//
// Created by jwscoggins on 7/26/20.
//

#include "VoidAtom.h"
#include "Environment.h"
#include <memory>
namespace maya {
    Void::Void(Environment &parent) : Atom(parent, VOID_TYPE) { }
    UDFValue::Ptr
    Void::evaluate() {
        return std::make_shared<UDFValue>(getParent(), getParent().getFalseSymbol(), nullptr);
    }

    size_t
    Void::hash(size_t range) const {
        return 0;
    }

   void
   Void::write(const std::string &logicalName) {
        // do nothing
    }
} // end namespace maya
