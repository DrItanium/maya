//
// Created by jwscoggins on 7/26/20.
//

#include "Entity.h"

namespace maya {

    bool
    Entity::evaluate(std::shared_ptr<UDFValue> returnValue) {
        return false;
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
