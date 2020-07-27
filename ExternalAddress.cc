//
// Created by jwscoggins on 7/26/20.
//

#include "ExternalAddress.h"

namespace maya {
    void
    ExternalAddress::write(const std::string& logicalName) {
        longPrint(logicalName);
    }

    bool
    ExternalAddress::evaluate(UDFValue::Ptr retVal) {
        retVal->setContents(this->shared_from_this());
        return true;
    }

}