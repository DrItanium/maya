//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_EXTERNALADDRESS_H
#define MAYA_EXTERNALADDRESS_H
#include <cstddef>
#include <memory>
#include <functional>
#include <any>
#include "Environment.h"
#include "Atom.h"
#include "UDFContext.h"
#include "Value.h"
#include "Constants.h"
namespace maya {

    /**
     * When the call method passes a symbol as it's first argument this kind of method is used
     */
    using StaticExternalAddressCallable = std::function<bool(struct UDFContext&, std::shared_ptr<struct UDFValue>)>;
    /**
     * @brief An external address is a kind of atom which wraps an external type.
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
        /**
         * @brief Construct an external address with the given type
         * @param env
         * @param externalType
         */
        ExternalAddress(Environment& env, uint16_t externalType) : Atom(env, EXTERNAL_ADDRESS_TYPE), _externalType(externalType) { }
        /**
         * @brief Used by the new method to construct a given type
         * @param context The context that describes the environment this external address will be a part of
         * @param externalType The type id which specifies the proper type
         */
        ExternalAddress(UDFContext& context, uint16_t externalType ) : ExternalAddress(context.getParent(), externalType) { }
        /**
         * @brief Override this destructor to do something when destroying this container
         */
        ~ExternalAddress() override = default;
        void write(const std::string &logicalName) override;
        virtual void shortPrint(const std::string &logicalName) = 0;
        virtual void longPrint(const std::string &logicalName) = 0;
        /**
         * @brief Used by the call UDF when an ExternalAddress is the first argument to that UDF
         * @param context The environment context
         * @param returnValue Where to store the returned value
         * @return boolean value signifying if we were successful at all
         */
        virtual bool call(UDFContext& context, UDFValue::Ptr returnValue);
    public:
        /**
         * @brief Transfer this instance into the UDFValue
         * @param retVal The Value to store this result in
         * @return boolean value signifying if success happened or not
         */
        bool evaluate(UDFValue::Ptr retVal) override final;
    private:
        uint16_t _externalType;
    };

    class ExternalAddressPointer : public ExternalAddress {
    public:
        ExternalAddressPointer(Environment& parent, uint16_t externalType) : ExternalAddress(parent, externalType) { }
        ExternalAddressPointer(UDFContext& context, uint16_t externalType) : ExternalAddress(context, externalType) { }
        ~ExternalAddressPointer() override = default;
        void shortPrint(const std::string& logicalName) override;
        void longPrint(const std::string& logicalName) override;
    private:
        void defaultPrint(const std::string& logicalName);
    protected:
        virtual size_t getPointerAddress() const noexcept = 0;
    };
    template<typename T>
    constexpr auto IsConstructibleExternalAddress = std::is_base_of_v<ExternalAddress, T> && std::is_constructible_v<T, UDFContext&, uint16_t>;
    /**
     * @brief Invoke the call method associated with a given ExternalAddress
     * @param context The data about the invoking context
     * @param returnValue The result of the call operation
     */
    void CallFunction(UDFContext& context, UDFValue::Ptr returnValue);
    /**
     * @brief Constructs a new external address of a given type
     * @param context The data about the invoking context
     * @param returnValue The external address or false
     */
    void NewFunction(UDFContext& context, UDFValue::Ptr returnValue);
} // end namespace maya
#endif //MAYA_EXTERNALADDRESS_H
