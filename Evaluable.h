//
// Created by jwscoggins on 7/15/20.
//

#ifndef MAYA_EVALUABLE_H
#define MAYA_EVALUABLE_H
#include "HoldsEnvironmentCallback.h"
namespace maya {
    struct UDFValue;
    struct Environment;

/**
 * @brief A class interface that states the given type can be evaluated
 */
    class Evaluable : public HoldsEnvironmentCallback {
    public:
        Evaluable(Environment &parent) : HoldsEnvironmentCallback(parent) {}
        virtual ~Evaluable() = default;
        virtual bool evaluate(std::shared_ptr<UDFValue> returnValue) = 0;
    };
}
#endif //MAYA_EVALUABLE_H
