//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_CALLABLE_H
#define MAYA_CALLABLE_H
#include <memory>
namespace maya {
/**
 * @brief Provides a callable interface that when implemented on a ExternalAddress child will provide a "call" method
 */
    class Callable {
    public:
        virtual ~Callable() = default;
        virtual bool call(struct UDFContext &context, std::shared_ptr<struct maya::UDFValue> returnValue) = 0;
    };
}

#endif //MAYA_CALLABLE_H
