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
struct UDFValue;
class Environment;
using EnvironmentPtr = std::shared_ptr<Environment>;
template<typename R, typename ... Ts>
using EnvironmentPtrFunction = std::function<R(const EnvironmentPtr&, Ts...)>;
template<typename ... Ts>
using EnvironmentPtrNoReturnFunction = EnvironmentPtrFunction<void, Ts...>;
using EntityPrintFunction = EnvironmentPtrNoReturnFunction<const std::string&, std::any>;
using EntityEvaluationFunction = EnvironmentPtrFunction<bool, std::any, std::shared_ptr<UDFValue>>;
using EntityBusyCountFunction = EnvironmentPtrNoReturnFunction<std::any>;

using BoolCallFunction = EnvironmentPtrFunction<bool, std::any>;
using VoidCallFunction = EnvironmentPtrNoReturnFunction<std::any>;
using VoidCallFunctionWithArg = EnvironmentPtrNoReturnFunction<std::any, std::any>;
/**************/
/* typeHeader */
/**************/
struct TypeHeader {
    TypeHeader(unsigned short t = 0) : _type(t) { }
    constexpr auto getType() const noexcept { return _type; }
private:
    unsigned short _type;
};

/*************/
/* clipsVoid */
/*************/
struct CLIPSVoid : public TypeHeader {
public:
    using Self = CLIPSVoid;
    using Ptr = std::shared_ptr<Self>;
public:
    CLIPSVoid() : TypeHeader(VOID_TYPE) { }
};

/***************/
/* CLIPSLexeme */
/***************/
struct CLIPSLexeme : public TypeHeader, public ReferenceCounted, public Hashable {
public:
    using Self = CLIPSLexeme;
    using Ptr = std::shared_ptr<Self>;
public:
    CLIPSLexeme(unsigned short type) : TypeHeader(type) { }
    size_t hash(size_t range) override;
public:
    std::string contents;
};

/**************/
/* CLIPSFloat */
/**************/
struct CLIPSFloat : public TypeHeader, public ReferenceCounted, public Hashable {
public:
    using Self = CLIPSFloat;
    using Ptr = std::shared_ptr<Self>;
public:
    CLIPSFloat() : TypeHeader(FLOAT_TYPE) { }
    size_t hash(size_t range) override;
public:
    double contents;
};

/****************/
/* CLIPSInteger */
/****************/
struct CLIPSInteger : public TypeHeader, public ReferenceCounted, public Hashable {
public:
    using Self = CLIPSInteger;
    using Ptr = std::shared_ptr<Self>;
public:
    CLIPSInteger() : TypeHeader(INTEGER_TYPE) { }
    size_t hash(size_t range) override;
public:
    long long contents;
};

/***************/
/* CLIPSBitMap */
/***************/
struct CLIPSBitMap : public TypeHeader, public ReferenceCounted, public Hashable {
public:
    using Self = CLIPSBitMap;
    using Ptr = std::shared_ptr<Self>;
public:
    CLIPSBitMap(unsigned short type) : TypeHeader(type) { }
    size_t hash(size_t range) override;
public:
    std::vector<bool> contents;
};

/************************/
/* CLIPSExternalAddress */
/************************/
struct CLIPSExternalAddress : public TypeHeader, public ReferenceCounted, public Hashable {
public:
    using Self = CLIPSExternalAddress;
    using Ptr = std::shared_ptr<Self>;
public:
    CLIPSExternalAddress(unsigned short type) : TypeHeader(type) { }
    size_t hash(size_t range) override;
public:
    std::any contents;
    unsigned short type;
};

/**************/
/* multifield */
/**************/
struct Multifield : public TypeHeader, public ReferenceCountable {
public:
    using Self = Multifield;
    using Ptr = std::shared_ptr<Self>;
public:
    Multifield() : TypeHeader(MULTIFIELD_TYPE) { }
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
            CLIPSLexeme::Ptr,
            CLIPSFloat::Ptr,
            CLIPSInteger::Ptr,
            CLIPSVoid::Ptr,
            Multifield::Ptr,
            std::shared_ptr<Fact>,
            std::shared_ptr<Instance>,
            CLIPSExternalAddress::Ptr>;

struct HoldsOntoGenericValue : public ReferenceCountable {
    ValueContainer contents;
    void retain() override;
    void release() override;
    bool canRelease() const noexcept override;
};

/**************/
/* CLIPSValue */
/**************/
struct CLIPSValue : public HoldsOntoGenericValue {
public:
    using Self = CLIPSValue;
    using Ptr = std::shared_ptr<Self>;
};


/************/
/* UDFValue */
/************/
struct UDFValue : public HoldsOntoGenericValue {
public:
    using Self = UDFValue;
    using Ptr = std::shared_ptr<Self>;
public:
    std::any supplementalInfo;
    size_t begin;
    size_t range;
};
struct FunctionDefinition;
struct Expression;
/**************/
/* udfContext */
/**************/
struct UDFContext : public HoldsEnvironmentCallback {
    UDFContext(Environment& parent);
    std::shared_ptr<FunctionDefinition> theFunction;
    unsigned int lastPosition = 0;
    std::shared_ptr<struct Expression> lastArg;
    UDFValue::Ptr returnValue;
};
using GenericEntityRecordFunction = std::function<void(std::any, std::any)>;
using EntityRecordPropagateDepthFunction = GenericEntityRecordFunction ;
using EntityRecordMarkNeededFunction = GenericEntityRecordFunction ;
using EntityRecordInstallFunction = GenericEntityRecordFunction ;
using EntityRecordDeinstallFunction = GenericEntityRecordFunction ;
using EntityRecordDeleteFunction = std::function<bool(std::any, const EnvironmentPtr&)>;
using EntityRecordGetNextFunction = std::function<std::any(std::any, std::any)>;
/****************/
/* EntityRecord */
/****************/
struct EntityRecord {
    std::string name;
    unsigned int type: 13;
    bool copyToEvaluate: 1;
    bool bitMap: 1;
    bool addsToRuleComplexity: 1;
    EntityPrintFunction shortPrintFunction;
    EntityPrintFunction longPrintFunction;
    EntityRecordDeleteFunction deleteFunction;
    EntityEvaluationFunction evaluateFunction;
    EntityRecordGetNextFunction getNextFunction;
    EntityBusyCountFunction decrementBusyCount;
    EntityBusyCountFunction incrementBusyCount;
    EntityRecordPropagateDepthFunction propagateDepth;
    EntityRecordMarkNeededFunction markNeeded;
    EntityRecordInstallFunction install;
    EntityRecordDeinstallFunction deinstall;
    std::shared_ptr<struct userData> usrData;
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

/*****************/
/* PatternEntity */
/*****************/
struct PatternEntity : public TypeHeader, public ReferenceCountable {
public:
    PatternEntity(unsigned short type) : TypeHeader(type) { }
    ~PatternEntity() override = default;
public:
    std::shared_ptr<struct PatternEntityRecord> theInfo;
    std::any dependents;
private:
    unsigned _busyCount = 0;
public:
    unsigned long long timeTag = 0;
    void retain() override;
    void release() override;
    bool canRelease() const noexcept override;
};

#endif /* _H_entities */


