//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_CALLABLE_H
#define MAYA_CALLABLE_H
#include <memory>
#include <functional>
namespace maya {
/**
 * @brief Provides a callable interface used by ExternalAddresses which operate on an instance of ExternalAddress
 */
    class InstanceCallable {
    public:
        virtual ~InstanceCallable() = default;
        virtual bool call(struct UDFContext &context, std::shared_ptr<struct UDFValue> returnValue) = 0;
    };
} // end namespace maya

#endif //MAYA_CALLABLE_H
