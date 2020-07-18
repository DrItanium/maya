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

#include "Constants.h"
#include "ReferenceCounted.h"
#include "Hashable.h"
#include "HoldsEnvironmentCallback.h"
#include "IORouterAware.h"
#include "BusyCountable.h"
#include "Evaluable.h"
namespace maya {
    class Environment;
/**************/
/* typeHeader */
/**************/
    struct TypeHeader {
        TypeHeader(unsigned short t = 0) : _type(t) {}
        constexpr auto getType() const noexcept { return _type; }
        constexpr auto isConstantType() const noexcept { return maya::isConstantType(_type); }
    private:
        unsigned short _type;
    };

    class Atom : public HoldsEnvironmentCallback, public TypeHeader, public IORouterAware, public ReferenceCounted, public Hashable {
    public:
        Atom(Environment& parent, unsigned short type = 0) : HoldsEnvironmentCallback(parent), TypeHeader(type) {}
        ~Atom() override = default;
    };
    template<typename T>
    class TransferEvaluable : public Evaluable, std::enable_shared_from_this<T> {
    public:
        ~TransferEvaluable() override = default;
        bool evaluate(std::shared_ptr<UDFValue> retVal) override;

    };
/*************/
/* clipsVoid */
/*************/
    class Void final : public Atom {
    public:
        using Self = Void;
        using Ptr = std::shared_ptr<Self>;
    public:
        Void(Environment& parent) : Atom(parent, VOID_TYPE) {}
        ~Void() override = default;
        size_t hash(size_t) override { return 0; }
        void retain() override { }
        void release() override { }
        bool canRelease() const noexcept override { return false; }
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
    class Lexeme : public Atom, public TransferEvaluable<Lexeme> {
    public:
        using Self = Lexeme;
        using Ptr = std::shared_ptr<Self>;
    public:
        Lexeme(Environment& parent, TreatAsSymbol) : Atom(parent, SYMBOL_TYPE) { }
        Lexeme(Environment& parent, TreatAsString) : Atom(parent, STRING_TYPE) { }
        Lexeme(Environment& parent, TreatAsInstanceName) : Atom(parent, INSTANCE_NAME_TYPE) { }
        Lexeme(Environment& parent, const std::string& contents, TreatAsSymbol) : Atom(parent, SYMBOL_TYPE), _contents(contents) { }
        Lexeme(Environment& parent, const std::string& contents, TreatAsString) : Atom(parent, STRING_TYPE), _contents(contents) { }
        Lexeme(Environment& parent, const std::string& contents, TreatAsInstanceName) : Atom(parent, INSTANCE_NAME_TYPE), _contents(contents) { }
        Lexeme(Environment& parent, const std::string& contents, unsigned short type) : Atom(parent, type), _contents(contents) { }
        std::string getContents() const noexcept { return _contents; }
        ~Lexeme() override = default;
        size_t hash(size_t range) override;
        void write(const std::string& logicalName) override;
    private:
        std::string _contents;
    };


/**************/
/* Float */
/**************/
    class Float : public Atom, public TransferEvaluable<Float> {
    public:
        using Self = Float;
        using Ptr = std::shared_ptr<Self>;
    public:
        Float(Environment& parent, double value = 0.0) : Atom(parent, FLOAT_TYPE), _contents(value) {}
        ~Float() override = default;
        size_t hash(size_t range) override;
        void write(const std::string &logicalName) override;
        constexpr auto getContents() const noexcept { return _contents; }
        bool evaluate(std::shared_ptr<UDFValue> retVal) override;
    private:
        double _contents;
    };

/****************/
/* Integer */
/****************/
    struct Integer : public Atom, public TransferEvaluable<Integer> {
    public:
        using Self = Integer;
        using Ptr = std::shared_ptr<Self>;
        using BackingType = long long;
    public:
        Integer(Environment& parent, BackingType value = 0) : Atom(parent, INTEGER_TYPE), _contents(value) {}
        ~Integer() override = default;
        size_t hash(size_t range) override;
        void write(const std::string &logicalName) override;
        constexpr auto getContents() const noexcept { return _contents; }
        bool evaluate(std::shared_ptr<UDFValue> retVal) override;
    private:
        BackingType _contents;
    };

/***************/
/* BitMap */
/***************/
    struct BitMap : public Atom {
    public:
        using Self = BitMap;
        using Ptr = std::shared_ptr<Self>;
    public:
        BitMap(Environment& parent, unsigned short type) : Atom(parent, type) {}
        size_t hash(size_t range) override;
        void write(const std::string& logicalName) override;
    public:
        std::vector<bool> contents;
    };

/************************/
/* ExternalAddress */
/************************/
    struct ExternalAddress : public Atom, public TransferEvaluable<ExternalAddress> {
    public:
        using Self = ExternalAddress;
        using Ptr = std::shared_ptr<Self>;
    public:
        ExternalAddress(Environment& parent, unsigned short externalType) : Atom(parent, EXTERNAL_ADDRESS_TYPE), _externalAddressType(externalType) {}
        size_t hash(size_t range) override;
        constexpr auto getExternalType() const noexcept { return _externalAddressType; }
        void write(const std::string& logicalName) override;
    public:
        std::any contents;
    private:
        unsigned short _externalAddressType;
    };

/**************/
/* multifield */
/**************/
    struct Multifield : public HoldsEnvironmentCallback, public TypeHeader, public BusyCountable, public Evaluable {
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

    struct Fact;
    struct Instance;
    using ValueContainer = std::variant<std::monostate,
            Lexeme::Ptr,
            Float::Ptr,
            Integer::Ptr,
            Void::Ptr,
            Multifield::Ptr,
            std::shared_ptr<Fact>,
            std::shared_ptr<Instance>,
            ExternalAddress::Ptr>;

    struct HoldsOntoGenericValue : public ReferenceCountable {
        HoldsOntoGenericValue() = default;
        ~HoldsOntoGenericValue() override = default;
        ValueContainer contents;
        void retain() override;
        void release() override;
        bool canRelease() const noexcept override;
        unsigned short getType() const noexcept;
    };

/**************/
/* CLIPSValue */
/**************/
    struct CLIPSValue : public HoldsOntoGenericValue {
    public:
        using Self = CLIPSValue;
        using Ptr = std::shared_ptr<Self>;
    };

    using ExpressionPtr = std::shared_ptr<struct Expression>;
/************/
/* UDFValue */
/************/
    struct UDFValue : public HoldsOntoGenericValue {
    public:
        using Self = UDFValue;
        using Ptr = std::shared_ptr<Self>;
    public:
        UDFValue() = default;
        ~UDFValue() override = default;

        std::any supplementalInfo;
        size_t begin;
        size_t range;
        ExpressionPtr toExpression(const EnvironmentPtr &theEnv);
    };

    bool operator==(const UDFValue &a, const UDFValue &b);
    struct ExternalFunction;
    struct Expression;
/**************/
/* udfContext */
/**************/
    struct UDFContext : public HoldsEnvironmentCallback {
    public:
        using Self = UDFContext;
        using Ptr = std::shared_ptr<Self>;
    public:
        UDFContext(Environment &parent, std::shared_ptr<ExternalFunction> func, std::list<std::shared_ptr<struct Expression>>& argList, UDFValue::Ptr retVal);
        std::shared_ptr<ExternalFunction> theFunction;
        std::list<std::shared_ptr<struct Expression>>& _args;
        UDFValue::Ptr returnValue;
    };
    // So in the original code the EntityRecord was used as a sort of "class wrapper" to provide
    // common functionality to a given C type (think a virtual abstract class of sorts). This
    // design required that one register each type to a given primitive type through an EntityRecord
    // data structure which described a set of common actions and properties. While this functionaity could
    // be ported over to the C++ rewrite, I feel that the C++ language provides quite a bit of functionality which
    // not only allows for more type safety but also expandability.
    //
    // For example, each entity record has an associated type and a few boolean properties associated with it.
    // With the boolean properties we are storing constant data into a unsigned short along with the associated type.
    // This information does _not_ change at runtime and is better served as compile time constants.
    //
    // An entity also has an associated name with it as well. This is also the same for all instances of the thing that
    // is tied to the entity record. Thus we also have another candidate for template parameters.
    //
    // The entity record itself may stay as an independent thing but it will be purely informational and the same for each environment.
    // The downside to this approach is that the data could be non-thread safe.
    //
    // Regardless, the EntityRecord and Entity separation concept needs to be abolished because we have objects.

    template<typename T>
    constexpr auto addsToRuleComplexity = false;

    struct Entity : public HoldsEnvironmentCallback, public BusyCountable, public Evaluable {
    public:
        using Self = Entity;
        using Ptr = std::shared_ptr<Self>;
    public:
        Entity(Environment &parent, const std::string &name, unsigned int type);
        virtual ~Entity() = default;
        [[nodiscard]] std::string getName() const noexcept { return _name; }
        [[nodiscard]] constexpr auto getType() const noexcept { return _type; }
        //[[nodiscard]] constexpr auto copyToEvaluate() const noexcept { return _copyToEvaluate; }
        //[[nodiscard]] constexpr auto addsToRuleComplexity() const noexcept { return _addsToRuleComplexity; }
        virtual void shortPrint(const std::string &logicalName);
        virtual void longPrint(const std::string &logicalName);
        bool evaluate(std::shared_ptr<UDFValue> returnValue) override;
        // retain and release are provided by BusyCountable
    private:
        std::string _name;
        unsigned int _type: 13;
        // this is handled by special forms of Evaluable and a special inheritance of entity that is used
        //bool _copyToEvaluate: 1;
        //bool _addsToRuleComplexity: 1;
    };

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
        virtual void decrementBasisCount();
        virtual void incrementBasisCount();
        virtual void onMatch();
        virtual bool synchronized();
        [[nodiscard]] constexpr auto getTimeTag() const noexcept { return _timeTag; }
        void setTimeTag(unsigned long long value) noexcept { _timeTag = value; }
        auto getDependents() const noexcept { return _dependents; }
        void setDependents(std::any value) { _dependents = value; }
    private:
        unsigned long long _timeTag = 0;
        std::any _dependents;
        // virtual bool isDeleted();
    };
    template<typename T>
    bool
    TransferEvaluable<T>::evaluate(UDFValue::Ptr retVal) {
        retVal->contents = this->shared_from_this();
        return true;
    }
} // end namespace clips
#endif /* _H_entities */


