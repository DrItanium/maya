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

#include "Constants.h"
#include "ReferenceCounted.h"
#include "Hashable.h"
#include "HoldsEnvironmentCallback.h"
#include "IORouterAware.h"
namespace maya {
    class Environment;
/**************/
/* typeHeader */
/**************/
    struct TypeHeader {
        TypeHeader(unsigned short t = 0) : _type(t) {}
        constexpr auto getType() const noexcept { return _type; }
        constexpr auto isConstantType() const noexcept { return ::isConstantType(_type); }
    private:
        unsigned short _type;
    };

    class Atom : public HoldsEnvironmentCallback, public TypeHeader, public IORouterAware, public ReferenceCountable, public Hashable {
    public:
        Atom(Environment& parent, unsigned short type = 0) : HoldsEnvironmentCallback(parent), TypeHeader(type) {}
        ~Atom() override = default;
    };
    class ReferenceCountedAtom : public Atom, public ReferenceCounted {
    public:
       ReferenceCountedAtom(Environment& parent, unsigned short type) : Atom(parent, type) { }
       ~ReferenceCountedAtom() override = default;
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
    class Lexeme : public ReferenceCountedAtom {
    public:
        using Self = Lexeme;
        using Ptr = std::shared_ptr<Self>;
    public:
        Lexeme(Environment& parent, TreatAsSymbol) : ReferenceCountedAtom(parent, SYMBOL_TYPE) { }
        Lexeme(Environment& parent, TreatAsString) : ReferenceCountedAtom(parent, STRING_TYPE) { }
        Lexeme(Environment& parent, TreatAsInstanceName) : ReferenceCountedAtom(parent, INSTANCE_NAME_TYPE) { }
        Lexeme(Environment& parent, const std::string& contents, TreatAsSymbol) : ReferenceCountedAtom(parent, SYMBOL_TYPE), _contents(contents) { }
        Lexeme(Environment& parent, const std::string& contents, TreatAsString) : ReferenceCountedAtom(parent, STRING_TYPE), _contents(contents) { }
        Lexeme(Environment& parent, const std::string& contents, TreatAsInstanceName) : ReferenceCountedAtom(parent, INSTANCE_NAME_TYPE), _contents(contents) { }
        Lexeme(Environment& parent, const std::string& contents, unsigned short type) : ReferenceCountedAtom(parent, type), _contents(contents) { }
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
class Float : public ReferenceCountedAtom {
public:
    using Self = Float;
    using Ptr = std::shared_ptr<Self>;
    public:
        Float(Environment& parent, double value = 0.0) : ReferenceCountedAtom(parent, FLOAT_TYPE), _contents(value) {}
        ~Float() override = default;
        size_t hash(size_t range) override;
        void write(const std::string &logicalName) override;
        constexpr auto getContents() const noexcept { return _contents; }
    private:
        double _contents;
    };

/****************/
/* Integer */
/****************/
    struct Integer : public ReferenceCountedAtom {
    public:
        using Self = Integer;
        using Ptr = std::shared_ptr<Self>;
        using BackingType = long long;
    public:
        Integer(Environment& parent, BackingType value = 0) : ReferenceCountedAtom(parent, INTEGER_TYPE), _contents(value) {}
        ~Integer() override = default;
        size_t hash(size_t range) override;
        void write(const std::string &logicalName) override;
        constexpr auto getContents() const noexcept { return _contents; }
    private:
        BackingType _contents;
    };

/***************/
/* BitMap */
/***************/
    struct BitMap : public ReferenceCountedAtom {
    public:
        using Self = BitMap;
        using Ptr = std::shared_ptr<Self>;
    public:
        BitMap(Environment& parent, unsigned short type) : ReferenceCountedAtom(parent, type) {}
        size_t hash(size_t range) override;
        void write(const std::string& logicalName) override;
    public:
        std::vector<bool> contents;
    };

/************************/
/* ExternalAddress */
/************************/
    struct ExternalAddress : public ReferenceCountedAtom {
    public:
        using Self = ExternalAddress;
        using Ptr = std::shared_ptr<Self>;
    public:
        ExternalAddress(Environment& parent, unsigned short externalType) : ReferenceCountedAtom(parent, EXTERNAL_ADDRESS_TYPE), _externalAddressType(externalType) {}
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
    struct Multifield : public TypeHeader, public ReferenceCountable {
    public:
        using Self = Multifield;
        using Ptr = std::shared_ptr<Self>;
    public:
        Multifield() : TypeHeader(MULTIFIELD_TYPE) {}
        ~Multifield() override = default;
    private:
        unsigned _busyCount = 0;
    public:
        auto length() const noexcept { return contents.size(); }
        std::vector<struct CLIPSValue> contents;
        void retain() override;
        void release() override;
        bool canRelease() const noexcept override;
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
    struct FunctionDefinition;
    struct Expression;
/**************/
/* udfContext */
/**************/
    struct UDFContext : public HoldsEnvironmentCallback {
    public:
        using Self = UDFContext;
        using Ptr = std::shared_ptr<Self>;
    public:
        UDFContext(Environment &parent);
        std::shared_ptr<FunctionDefinition> theFunction;
        unsigned int lastPosition = 0;
        std::shared_ptr<struct Expression> lastArg;
        UDFValue::Ptr returnValue;
    };

/**
 * @brief A description of a given type
 */
    struct Entity : public HoldsEnvironmentCallback {
    public:
        using Self = Entity;
        using Ptr = std::shared_ptr<Self>;
    public:
        Entity(Environment &parent, const std::string &name, unsigned int type, bool copyToEvaluate, bool isBitmap,
               bool addsToRuleComplexity);
        virtual ~Entity() = default;
        std::string getName() const noexcept { return _name; }
        constexpr auto getType() const noexcept { return _type; }
        constexpr auto copyToEvaluate() const noexcept { return _copyToEvaluate; }
        constexpr auto isBitmap() const noexcept { return _bitMap; }
        constexpr auto addsToRuleComplexity() const noexcept { return _addsToRuleComplexity; }
        virtual void shortPrint(const std::string &logicalName) {}
        virtual void longPrint(const std::string &logicalName) {}
        virtual bool evaluate(std::shared_ptr<UDFValue> returnValue) { return false; }
        virtual void incrementBusyCount() {}
        virtual void decrementBusyCount() {}
    private:
        std::string _name;
        unsigned int _type: 13;
        bool _copyToEvaluate: 1;
        bool _bitMap: 1;
        bool _addsToRuleComplexity: 1;
    };

#if 0
/****************/
/* EntityRecord */
/****************/
    using EntityRecordDeleteFunction = std::function<bool(std::any, const EnvironmentPtr &)>;
    using EntityRecordGetNextFunction = std::function<std::any(std::any, std::any)>;
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
        EntityRecordDeleteFunction deleteFunction;
        EntityEvaluationFunction evaluateFunction;
        EntityRecordGetNextFunction getNextFunction;
        EntityBusyCountFunction decrementBusyCount;
        EntityBusyCountFunction incrementBusyCount;
    };

    using PatternEntityRecordDecrementBasisCountFunction = EnvironmentPtrNoReturnFunction<std::any>;
    using PatternEntityRecordIncrementBasisCountFunction = EnvironmentPtrNoReturnFunction<std::any>;
    using PatternEntityRecordMatchFunction = EnvironmentPtrNoReturnFunction<std::any>;
    using PatternEntityRecordSynchronizedFunction = EnvironmentPtrFunction<bool, std::any>;
    using PatternEntityRecordIsDeletedFunction = EnvironmentPtrFunction<bool, std::any>;
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
#endif
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
} // end namespace clips
#endif /* _H_entities */


