//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_EXTERNALADDRESS_H
#define MAYA_EXTERNALADDRESS_H
#include "Environment.h"
#include "Atom.h"
#include "UDFContext.h"
#include "Value.h"
#include "Constants.h"
#include "Callable.h"
#include <cstddef>
namespace maya {
    /**
     * @brief An external address is a kind of atom which wraps an external type.
     * An implementation is meant to inherit from this and implement the desired methods as needed.
     * When registering the external address type with the environment is where the magic happens. Generally, you want to
     * stay away from having the external types you wish to manipulate being destructible by maya unless it was created by
     * maya itself. At that point, there is extra logic that must be provided to make sure that maya does not destroy your objects
     * without your knowledge.
     */
    struct ExternalAddress : public Atom, std::enable_shared_from_this<ExternalAddress> {
    public:
        using Self = ExternalAddress;
        //using Ptr = std::shared_ptr<Self>;
        using ObserverPtr = std::experimental::fundamentals_v2::observer_ptr<Self>;
        using WeakPtr = std::weak_ptr<Self>;
        using SharedPtr = std::shared_ptr<Self>;
        using Ptr = std::variant<SharedPtr, ObserverPtr>;
    public:
        ExternalAddress(Environment& env, uint16_t externalType) : Atom(env, EXTERNAL_ADDRESS_TYPE), _externalType(externalType) { }
        /**
         * @brief Override this destructor to do something when destroying this container
         */
        ~ExternalAddress() override = default;
        [[nodiscard]] constexpr bool isCallable() const noexcept { return maya::IsCallable<decltype(*this)>; }
        void write(const std::string &logicalName) override;
        virtual void shortPrint(const std::string &logicalName) = 0;
        virtual void longPrint(const std::string &logicalName) = 0;
        /**
         * @brief Transfer this instance as
         * @param retVal The Value to store this result in
         * @return boolean value signifying if success happened or not
         */
        bool evaluate(UDFValue::Ptr retVal) override final;
    private:
        uint16_t _externalType;
    };

    /**
     * @brief An external address type which (new) works with, in this case you should override ctor (Environment&, UDFContext&, uint16_t). This version of ExternalAddress tracks if the
     * instance was made within maya or without.
     */
    class NewConstructibleExternalAddress : public ExternalAddress {
    public:
        using Self = NewConstructibleExternalAddress;
        using Ptr = std::shared_ptr<Self>;
    public:
        NewConstructibleExternalAddress(Environment& env, UDFContext& context, uint16_t typeCode) : ExternalAddress(env, typeCode), _mayaConstructed(true) { }
        NewConstructibleExternalAddress(Environment& env, uint16_t typeCode) : ExternalAddress(env, typeCode), _mayaConstructed(false) { }
        ~NewConstructibleExternalAddress() override = default;
        /**
         * @brief Is this instance constructed internally by maya?
         * @return True if the (new) method was used to make this instance.
         */
        [[nodiscard]] constexpr auto internallyConstructed() const noexcept { return _mayaConstructed; }
    private:
        bool _mayaConstructed;
    };

    template<typename T>
    constexpr auto IsExternalAddressType = std::is_base_of_v<ExternalAddress, T>;
    template<typename T>
    constexpr auto IsConstructibleExternalAddressType = IsExternalAddressType<T> && std::is_constructible_v<T, Environment &, UDFContext &, uint16_t>;

    static_assert(IsExternalAddressType<NewConstructibleExternalAddress>);
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
