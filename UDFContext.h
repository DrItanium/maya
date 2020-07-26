//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_UDFCONTEXT_H
#define MAYA_UDFCONTEXT_H
#include <memory>
#include "Value.h"
#include "HoldsEnvironmentCallback.h"
#include <list>
namespace maya {
    struct ExternalFunction;
    struct Expression;

    struct UDFContext : public HoldsEnvironmentCallback {
    public:
        using Self = UDFContext;
        using Ptr = std::shared_ptr<Self>;
    public:
        UDFContext(maya::Environment &parent,
                   std::shared_ptr<ExternalFunction> func,
                   std::list<std::shared_ptr<Expression>> &argList,
                   maya::UDFValue::Ptr retVal);
        std::shared_ptr<ExternalFunction> theFunction;
        std::list<std::shared_ptr<Expression>> &_args;
        maya::UDFValue::Ptr returnValue;
    };
}
#endif //MAYA_UDFCONTEXT_H
