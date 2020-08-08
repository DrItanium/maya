//
// Created by jwscoggins on 7/26/20.
//

#include "UDFContext.h"
#include "Expression.h"
#include <memory>
namespace maya {
    UDFContext::UDFContext(Environment &parent, std::shared_ptr<ExternalFunction> func, std::list<Expression::Ptr>& argList) : HoldsEnvironmentCallback(parent), theFunction(func), _args(argList){

    }
}