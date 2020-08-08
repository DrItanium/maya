//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_ENTITY_H
#define MAYA_ENTITY_H
#include "HoldsEnvironmentCallback.h"
#include "Evaluable.h"
#include "Hashable.h"
#include "IORouterAware.h"
#include "Value.h"
namespace maya {
    struct Entity : public HoldsEnvironmentCallback, public Evaluable, public Hashable, public IORouterAware {
    public:
        using Self = Entity;
        using Ptr = std::shared_ptr<Self>;
    public:
        Entity(Environment &parent);
        virtual ~Entity() = default;
        void write(const std::string &logicalName) override;
        virtual void shortPrint(const std::string &logicalName);
        virtual void longPrint(const std::string &logicalName);
        std::shared_ptr<UDFValue> evaluate() override;
    };

    template<typename T>
    struct EntityMetadata final {
        EntityMetadata() = delete;
        ~EntityMetadata() = delete;
        EntityMetadata(const EntityMetadata &) = delete;
        EntityMetadata(EntityMetadata &&) = delete;
        EntityMetadata &operator=(const EntityMetadata &) = delete;
        EntityMetadata &operator=(EntityMetadata &&) = delete;
        static constexpr bool isComplex() noexcept { return false; }
    };

#define DefEntityMetadata(T, TypeName, TypeID, IsComplex) \
template<> \
    struct EntityMetadata<T> final { \
        EntityMetadata() = delete; \
        ~EntityMetadata() = delete; \
        EntityMetadata(const EntityMetadata&) = delete; \
        EntityMetadata(EntityMetadata&&) = delete; \
        EntityMetadata& operator=(const EntityMetadata&) = delete; \
        EntityMetadata& operator=(EntityMetadata&&) = delete; \
        static const std::string& getName() noexcept { \
            static std::string _value = TypeName ; \
            return _value; \
        } \
        static constexpr unsigned short getId() noexcept { return TypeID; } \
        static constexpr bool isComplex() noexcept { return IsComplex; } \
    }

    template<typename T>
    constexpr auto AddsToRuleComplexity = EntityMetadata<T>::isComplex();
#if 0
    /****************/
/* EntityRecord */
/****************/
    using EntityPrintFunction = EnvironmentPtrNoReturnFunction<const std::string &, std::any>;
    using EntityEvaluationFunction = EnvironmentPtrFunction<bool, std::any, std::shared_ptr<UDFValue>>;
    using EntityBusyCountFunction = std::function<void(Environment &, std::any)>;

    struct EntityRecord : public HoldsEnvironmentCallback {
    public:
        using Self = EntityRecord;
        using Ptr = std::shared_ptr<Self>;
    public:
        EntityRecord(Environment &parent) : HoldsEnvironmentCallback(parent) {}
        std::string name;
        unsigned int type: 13;
        bool copyToEvaluate: 1;
        bool bitMap: 1;
        bool addsToRuleComplexity: 1;
        std::shared_ptr<struct userData> usrData;
        EntityPrintFunction shortPrintFunction;
        EntityPrintFunction longPrintFunction;
        EntityEvaluationFunction evaluateFunction;
        EntityBusyCountFunction decrementBusyCount;
        EntityBusyCountFunction incrementBusyCount;
    };

    using PatternEntityRecordDecrementBasisCountFunction = EnvironmentPtrNoReturnFunction<std::any>;
    using PatternEntityRecordIncrementBasisCountFunction = EnvironmentPtrNoReturnFunction<std::any>;
    using PatternEntityRecordMatchFunction = EnvironmentPtrNoReturnFunction<std::any>;
    using PatternEntityRecordSynchronizedFunction = EnvironmentPtrFunction<bool, std::any>;
    using PatternEntityRecordIsDeletedFunction = EnvironmentPtrFunction<bool, std::any>;
#endif
} // end namespace maya
#endif //MAYA_ENTITY_H
