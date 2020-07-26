//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_EXTERNALADDRESS_H
#define MAYA_EXTERNALADDRESS_H
#include "Environment.h"
#include "Atom.h"
#include "TransferEvaluable.h"
#include "UDFContext.h"
namespace maya {

    struct ExternalAddress : public Atom, public TransferEvaluable<ExternalAddress> {
    public:
        using Self = ExternalAddress;
        //using Ptr = std::shared_ptr<Self>;
        using ObserverPtr = std::experimental::fundamentals_v2::observer_ptr<Self>;
        using WeakPtr = std::weak_ptr<Self>;
        using SharedPtr = std::shared_ptr<Self>;
        using Ptr = std::variant<SharedPtr, ObserverPtr>;
    public:
        using Atom::Atom;
        ~ExternalAddress() override = default;
        [[nodiscard]] constexpr bool isCallable() const noexcept { return maya::IsCallable<decltype(*this)>; }
        void write(const std::string &logicalName) override;
        virtual void shortPrint(const std::string &logicalName) = 0;
        virtual void longPrint(const std::string &logicalName) = 0;
    };

    template<typename T>
    constexpr auto IsExternalAddressType = std::is_base_of_v<ExternalAddress, T>;
    template<typename T>
    constexpr auto IsConstructibleExternalAddressType = IsExternalAddressType<T> && std::is_constructible_v<T, Environment &, UDFContext &>;


/**
 * @brief Make a newFunction if such a thing can be generated from an ExternalAddress constructor
 * @tparam T The type to make (must inherit from ExternalAddress)
 * @return If the proper ctor is there then an actual std::function otherwise nullptr
 */
    template<typename T, std::enable_if_t<std::is_base_of_v<ExternalAddress, T>, int> = 0>
    std::function<ExternalAddress::Ptr(Environment &, struct UDFContext &)> makeNewFunction() noexcept {
        if (IsConstructibleExternalAddressType<T>) {
            return [](Environment &env, struct UDFContext &ctx) { return std::make_shared<T>(env, ctx); };
        } else {
            return nullptr;
        }
    }
} // end namespace maya
#endif //MAYA_EXTERNALADDRESS_H
