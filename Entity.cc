//
// Created by jwscoggins on 7/26/20.
//

#include "Entity.h"
#include "Environment.h"
namespace maya {

    UDFValue::Ptr
    Entity::evaluate() {
        return std::make_shared<UDFValue>(getParent(), getParent().getFalseSymbol(), nullptr);
    }
    void
    Entity::longPrint(const std::string &logicalName) {

    }

    void
    Entity::shortPrint(const std::string &logicalName) {

    }
    void
    Entity::write(const std::string& logicalName) {
        longPrint(logicalName);
    }
} // end namespace maya
