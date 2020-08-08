//
// Created by jwscoggins on 7/26/20.
//

#include "ExternalAddress.h"
#include "Environment.h"
#include <sstream>

namespace maya {
    void
    ExternalAddress::write(const std::string& logicalName) {
        longPrint(logicalName);
    }

    UDFValue::Ptr
    ExternalAddress::evaluate() {
        return std::make_shared<UDFValue>(getParent(), shared_from_this(), nullptr);
    }

    void
    ExternalAddressPointer::shortPrint(const std::string &logicalName) {
        defaultPrint(logicalName);
    }

    void
    ExternalAddressPointer::longPrint(const std::string &logicalName) {
        defaultPrint(logicalName);
    }

    void
    ExternalAddressPointer::defaultPrint(const std::string &logicalName) {
        std::ostringstream contents;
        contents << "<Pointer-" << getPointerAddress() << ">";
        auto str = contents.str();
        getParent().writeStringRouter(logicalName, str);

    }

    bool
    ExternalAddress::call(UDFContext &context, UDFValue::Ptr returnValue) {
        returnValue->setContents(getParent().getFalseSymbol());
        return false;
    }
    bool
    ExternalAddressStaticRecord::call(UDFContext &context, UDFValue::Ptr returnValue) {
        if (_symcall) {
            return _symcall(context, returnValue);
        } else {
            returnValue->setContents(getParent().getFalseSymbol());
            return false;
        }
    }

    ExternalAddress::Ptr
    ExternalAddressStaticRecord::makeNewInstance(UDFContext &context) {
        if (_newFunc) {
            return _newFunc(context, _externalId);
        } else {
            return nullptr;
        }
    }
}