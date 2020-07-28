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

    bool
    ExternalAddress::evaluate(UDFValue::Ptr retVal) {
        retVal->setContents(this->shared_from_this());
        return true;
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
        _parent.writeStringRouter(logicalName, str);
    }

    bool
    ExternalAddress::call(UDFContext &context, UDFValue::Ptr returnValue) {
        returnValue->setContents(_parent.getFalseSymbol());
        return false;
    }
    bool
    ExternalAddressStaticRecord::call(UDFContext &context, UDFValue::Ptr returnValue) {
        if (_symcall) {
            return _symcall(context, returnValue);
        } else {
            returnValue->setContents(_parent.getFalseSymbol());
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