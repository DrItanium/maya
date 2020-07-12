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

#pragma once

#define _H_entities
#include <memory>
#include <variant>

#include "ReferenceCounted.h"
using Environment = std::shared_ptr<struct EnvironmentData>;
struct UDFValue;
typedef void EntityPrintFunction(const Environment&, const char *, void *);
typedef bool EntityEvaluationFunction(const Environment&, void *, std::shared_ptr<UDFValue>);
typedef void EntityBusyCountFunction(const Environment&, void *);


typedef bool BoolCallFunction(const Environment&, void *);
typedef void VoidCallFunction(const Environment&, void *);
typedef void VoidCallFunctionWithArg(const Environment&, void *, void *);

/**************/
/* typeHeader */
/**************/
struct TypeHeader {
    unsigned short type;
};

/*************/
/* clipsVoid */
/*************/
struct CLIPSVoid {
public:
    using Self = CLIPSVoid;
    using Ptr = std::shared_ptr<Self>;
public:
    TypeHeader header;
};

/***************/
/* CLIPSLexeme */
/***************/
struct CLIPSLexeme : public ReferenceCounted {
public:
    using Self = CLIPSLexeme;
    using Ptr = std::shared_ptr<Self>;
public:
    TypeHeader header;
    Ptr next;
    const char *contents;
};

/**************/
/* CLIPSFloat */
/**************/
struct CLIPSFloat : public ReferenceCounted {
public:
    using Self = CLIPSFloat;
    using Ptr = std::shared_ptr<Self>;
public:
    TypeHeader header;
    Ptr next;
    double contents;
};

/****************/
/* CLIPSInteger */
/****************/
struct CLIPSInteger : public ReferenceCounted {
public:
    using Self = CLIPSInteger;
    using Ptr = std::shared_ptr<Self>;
public:
    TypeHeader header;
    Ptr next;
    long long contents;
};

/***************/
/* CLIPSBitMap */
/***************/
struct CLIPSBitMap : public ReferenceCounted {
public:
    using Self = CLIPSBitMap;
    using Ptr = std::shared_ptr<Self>;
public:
    TypeHeader header;
    Ptr next;
    const char *contents;
    unsigned short size;
};

/************************/
/* CLIPSExternalAddress */
/************************/
struct CLIPSExternalAddress : public ReferenceCounted {
public:
    using Self = CLIPSExternalAddress;
    using Ptr = std::shared_ptr<Self>;
public:
    TypeHeader header;
    Ptr next;
    std::any contents;
    unsigned short type;
};

/**************/
/* multifield */
/**************/
struct Multifield {
public:
    using Self = Multifield;
    using Ptr = std::shared_ptr<Self>;
public:
    TypeHeader header;
    unsigned busyCount;
    size_t length;
    Ptr next;
    std::vector<struct CLIPSValue> contents;
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

/**************/
/* CLIPSValue */
/**************/
struct CLIPSValue {
public:
    using Self = CLIPSValue;
    using Ptr = std::shared_ptr<Self>;
public:
    ValueContainer contents;
#if STUBBING_INACTIVE
    union {
        std::shared_ptr<TypeHeader> header; // does this work o_O????
        CLIPSLexeme::Ptr lexemeValue;
        CLIPSFloat::Ptr floatValue;
        CLIPSInteger::Ptr integerValue;
        CLIPSVoid::Ptr voidValue;
        Multifield::Ptr multifieldValue;
        std::shared_ptr<Fact> factValue;
        std::shared_ptr<Instance> instanceValue;
        CLIPSExternalAddress::Ptr externalAddressValue;
    };
#endif
};


/************/
/* UDFValue */
/************/
struct UDFValue {
public:
    using Self = UDFValue;
    using Ptr = std::shared_ptr<Self>;
public:
    std::any supplementalInfo;
    ValueContainer contents;
#if STUBBING_INACTIVE
    union {
        std::shared_ptr<TypeHeader> header; // does this work o_O????
        CLIPSLexeme::Ptr lexemeValue;
        CLIPSFloat::Ptr floatValue;
        CLIPSInteger::Ptr integerValue;
        CLIPSVoid::Ptr voidValue;
        Multifield::Ptr multifieldValue;
        std::shared_ptr<Fact> factValue;
        std::shared_ptr<Instance> instanceValue;
        CLIPSExternalAddress::Ptr externalAddressValue;
    };
#endif
    size_t begin;
    size_t range;
    Ptr next;
};
struct FunctionDefinition;
struct Expression;
/**************/
/* udfContext */
/**************/
struct UDFContext {
    Environment environment;
    std::shared_ptr<FunctionDefinition> theFunction;
    unsigned int lastPosition;
    std::shared_ptr<struct Expression> lastArg;
    UDFValue::Ptr returnValue;
};

typedef void EntityRecordPropagateDepthFunction(void*, void*);
typedef void EntityRecordMarkNeededFunction(void*, void*);
typedef void EntityRecordInstallFunction(void*, void*);
typedef void EntityRecordDeinstallFunction(void*, void*);
typedef bool EntityRecordDeleteFunction(void*, const Environment&);
typedef void* EntityRecordGetNextFunction(void*, void*);
/****************/
/* EntityRecord */
/****************/
struct EntityRecord {
    const char *name;
    unsigned int type: 13;
    bool copyToEvaluate: 1;
    bool bitMap: 1;
    bool addsToRuleComplexity: 1;
    std::shared_ptr<EntityPrintFunction> shortPrintFunction;
    std::shared_ptr<EntityPrintFunction> longPrintFunction;
    std::shared_ptr<EntityRecordDeleteFunction> deleteFunction;
    std::shared_ptr<EntityEvaluationFunction> evaluateFunction;
    std::shared_ptr<EntityRecordGetNextFunction> getNextFunction;
    std::shared_ptr<EntityBusyCountFunction> decrementBusyCount;
    std::shared_ptr<EntityBusyCountFunction> incrementBusyCount;
    std::shared_ptr<EntityRecordPropagateDepthFunction> propagateDepth;
    std::shared_ptr<EntityRecordMarkNeededFunction> markNeeded;
    std::shared_ptr<EntityRecordInstallFunction> install;
    std::shared_ptr<EntityRecordDeinstallFunction> deinstall;
    std::shared_ptr<struct userData> usrData;
};

typedef void PatternEntityRecordDecrementBasisCountFunction(const Environment&, void*);
typedef void PatternEntityRecordIncrementBasisCountFunction(const Environment&, void*);
typedef void PatternEntityRecordMatchFunction(const Environment&, void*);
typedef bool PatternEntityRecordSynchronizedFunction(const Environment&, void*);
typedef bool PatternEntityRecordIsDeletedFunction(const Environment&, void*);
/***********************/
/* PatternEntityRecord */
/***********************/
struct PatternEntityRecord {
    struct EntityRecord base;
    PatternEntityRecordDecrementBasisCountFunction* decrementBasisCount;
    PatternEntityRecordIncrementBasisCountFunction* incrementBasisCount;
    PatternEntityRecordMatchFunction* matchFunction;
    PatternEntityRecordSynchronizedFunction* synchronized;
    PatternEntityRecordIsDeletedFunction* isDeleted;
};

/*****************/
/* PatternEntity */
/*****************/
struct PatternEntity {
    TypeHeader header;
    std::shared_ptr<struct PatternEntityRecord> theInfo;
    std::any dependents;
    unsigned busyCount;
    unsigned long long timeTag;
};

#endif /* _H_entities */


