//
// Created by jwscoggins on 7/26/20.
//

#include "ExternalAddress.h"

namespace maya {
    void
    ExternalAddress::write(const std::string& logicalName) {
        longPrint(logicalName);
    }
}