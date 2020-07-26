/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  11/01/16            */
/*                                                     */
/*                ENTITIES HEADER FILE                 */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.40: Created to store key data structures.          */
/*                                                           */
/*************************************************************/

#ifndef _H_entities
#define _H_entities

#include <memory>
#include <variant>
#include <any>
#include <vector>
#include <functional>
#include <list>
#include <iostream>
#include <experimental/memory>

#include "Constants.h"
#include "Hashable.h"
#include "HoldsEnvironmentCallback.h"
#include "IORouterAware.h"
#include "Evaluable.h"
#include "Callable.h"
#include "ExternalAddress.h"
#include "TypeHeader.h"
#include "Atom.h"
namespace maya {
    class Void final : public Atom {
    public:
        using Self = Void;
        using Ptr = std::shared_ptr<Self>;
    public:
        Void(Environment& parent);
        ~Void() override = default;
        size_t hash(size_t range) const override { return 0; }
        void write(const std::string& logicalName) override { }
    };

    class TreatAsString {
    };

    class TreatAsSymbol {
    };

    class TreatAsInstanceName {
    };
/***************/
/* CLIPSLexeme */
/***************/
    class Lexeme : public EphemeralAtom, public TransferEvaluable<Lexeme> {
    public:
        using Self = Lexeme;
        using Ptr = std::shared_ptr<Self>;
    public:
        Lexeme(Environment& parent, TreatAsSymbol) : EphemeralAtom(parent, SYMBOL_TYPE) { }
        Lexeme(Environment& parent, TreatAsString) : EphemeralAtom(parent, STRING_TYPE) { }
        Lexeme(Environment& parent, TreatAsInstanceName) : EphemeralAtom(parent, INSTANCE_NAME_TYPE) { }
        Lexeme(Environment& parent, const std::string& contents, TreatAsSymbol) : EphemeralAtom(parent, SYMBOL_TYPE), _contents(contents) { }
        Lexeme(Environment& parent, const std::string& contents, TreatAsString) : EphemeralAtom(parent, STRING_TYPE), _contents(contents) { }
        Lexeme(Environment& parent, const std::string& contents, TreatAsInstanceName) : EphemeralAtom(parent, INSTANCE_NAME_TYPE), _contents(contents) { }
        Lexeme(Environment& parent, const std::string& contents, unsigned short type) : EphemeralAtom(parent, type), _contents(contents) { }
        std::string getContents() const noexcept { return _contents; }
        ~Lexeme() override = default;
        size_t hash(size_t range) const override;
        void write(const std::string& logicalName) override;
    private:
        std::string _contents;
    };


/**************/
/* Float */
/**************/
    class Float : public EphemeralAtom, public TransferEvaluable<Float> {
    public:
        using Self = Float;
        using Ptr = std::shared_ptr<Self>;
        using BackingType = double;
    public:
        Float(Environment& parent, BackingType value = 0.0) : EphemeralAtom(parent, FLOAT_TYPE), _contents(value) {}
        ~Float() override = default;
        size_t hash(size_t range) const override;
        void write(const std::string &logicalName) override;
        constexpr auto getContents() const noexcept { return _contents; }
    private:
        BackingType _contents;
    };

/****************/
/* Integer */
/****************/
    class Integer : public EphemeralAtom, public TransferEvaluable<Integer> {
    public:
        using Self = Integer;
        using Ptr = std::shared_ptr<Self>;
        using BackingType = long long;
    public:
        Integer(Environment& parent, BackingType value = 0) : EphemeralAtom(parent, INTEGER_TYPE), _contents(value) {}
        ~Integer() override = default;
        size_t hash(size_t range) const override;
        void write(const std::string &logicalName) override;
        constexpr auto getContents() const noexcept { return _contents; }
    private:
        BackingType _contents;
    };
    /**
     * @brief A generic entity which holds onto a binary view of some kind of bits. At this level, the act of preventing duplicates
     * is provided through getByte and numBytes.
     */
    class BitMap : public Atom, public Evaluable {
    public:
        using Self = BitMap;
        using Ptr = std::shared_ptr<Self>;
    public:
        BitMap(Environment& parent, unsigned short type) : Atom(parent, type) { }
        ~BitMap() override = default;
        [[nodiscard]] bool contentsEqual(Ptr other) const;
        [[nodiscard]] size_t getTypeHashCode() const noexcept { return typeid(*this).hash_code(); }
    protected:
        /**
         * @brief Compares the internals of a given BitMap when it is already known that the other is of the same polymorphic type as this instance
         * @param other the other BitMap that has already been polymorphically verified to be of the right type
         * @return boolean signifying if the internals are equal to one another provided that they are of the same type
         */
        [[nodiscard]] virtual bool compareInternals(Ptr other) const = 0;
    };


    /**************/
/* multifield */
/**************/
    struct Multifield : public HoldsEnvironmentCallback, public TypeHeader, public Evaluable {
    public:
        using Self = Multifield;
        using Ptr = std::shared_ptr<Self>;
    public:
        Multifield(Environment& parent) : HoldsEnvironmentCallback(parent), TypeHeader(MULTIFIELD_TYPE) {}
        ~Multifield() override = default;
        bool evaluate(std::shared_ptr<UDFValue> retVal) override;
    public:
        auto length() const noexcept { return contents.size(); }
        std::vector<std::shared_ptr<struct CLIPSValue>> contents;
    };

    struct Entity : public HoldsEnvironmentCallback, public Evaluable, public Hashable, public IORouterAware {
    public:
        using Self = Entity;
        using Ptr = std::shared_ptr<Self>;
    public:
        Entity(Environment &parent);
        virtual ~Entity() = default;
        void write(const std::string& logicalName) override;
        virtual void shortPrint(const std::string &logicalName);
        virtual void longPrint(const std::string &logicalName);
        bool evaluate(std::shared_ptr<UDFValue> returnValue) override;
    };
    template<typename T>
    struct EntityMetadata final {
        EntityMetadata() = delete;
        ~EntityMetadata() = delete;
        EntityMetadata(const EntityMetadata&) = delete;
        EntityMetadata(EntityMetadata&&) = delete;
        EntityMetadata& operator=(const EntityMetadata&) = delete;
        EntityMetadata& operator=(EntityMetadata&&) = delete;
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
#if 0
    /***********************/
/* PatternEntityRecord */
/***********************/
    struct PatternEntityRecord : public EntityRecord {
        PatternEntityRecordDecrementBasisCountFunction decrementBasisCount;
        PatternEntityRecordIncrementBasisCountFunction incrementBasisCount;
        PatternEntityRecordMatchFunction matchFunction;
        PatternEntityRecordSynchronizedFunction synchronized;
        PatternEntityRecordIsDeletedFunction isDeleted;
    };
/*****************/
/* PatternEntity */
/*****************/
    struct PatternEntity : public TypeHeader, public ReferenceCountable {
    public:
        PatternEntity(unsigned short type) : TypeHeader(type) {}
        ~PatternEntity() override = default;
    public:
        std::shared_ptr<struct PatternEntityRecord> theInfo;
        std::any dependents;
    private:
        unsigned _busyCount = 0;
        unsigned long long _timeTag = 0;
    public:
        void retain() override;
        void release() override;
        bool canRelease() const noexcept override;
        constexpr auto getBusyCount() const noexcept { return _busyCount; }
        constexpr auto getTimeTag() const noexcept { return _timeTag; }
        void setTimetag(unsigned long long value) noexcept { _timeTag = value; }
    };
#endif
    class PatternEntity : public Entity {
    public:
        using Self = PatternEntity;
        using Ptr = std::shared_ptr<Self>;
    public:
        using Entity::Entity;
        ~PatternEntity() override = default;
        virtual void onMatch();
        virtual bool synchronized();
        [[nodiscard]] constexpr auto getTimeTag() const noexcept { return _timeTag; }
        void setTimeTag(unsigned long long value) noexcept { _timeTag = value; }
        auto getDependents() const noexcept { return _dependents; }
        void setDependents(std::any value) { _dependents = value; }
    private:
        unsigned long long _timeTag = 0;
        std::any _dependents;
    };
} // end namespace maya

std::ostream& operator<<(std::ostream& os, const maya::Lexeme& lexeme);
std::ostream& operator<<(std::ostream& os, const maya::Float& value);
std::ostream& operator<<(std::ostream& os, const maya::Integer& value);
#endif /* _H_entities */


