//
// Created by jwscoggins on 7/26/20.
//

#include "VoidAtom.h"
#include "Environment.h"
namespace maya {
    Void::Void(Environment &parent) : Atom(parent, VOID_TYPE) { }
    bool
    Void::evaluate(UDFValue::Ptr retVal) {
        return true;
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
