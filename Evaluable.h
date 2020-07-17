//
// Created by jwscoggins on 7/15/20.
//

#ifndef MAYA_EVALUABLE_H
#define MAYA_EVALUABLE_H
#include <memory>
namespace maya {
    struct UDFValue;

/**
 * @brief A class interface that states the given type can be evaluated
 */
    class Evaluable {
    public:
        using Self = Evaluable;
        using Ptr = std::shared_ptr<Self>;
    public:
        virtual ~Evaluable() = default;
        virtual bool evaluate(std::shared_ptr<UDFValue> returnValue) = 0;
    };
}
#endif //MAYA_EVALUABLE_H
