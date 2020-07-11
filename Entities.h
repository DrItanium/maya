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
struct UDFValue;
typedef void EntityPrintFunction(const Environment&, const char *, void *);
typedef bool EntityEvaluationFunction(const Environment&, void *, UDFValue *);
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
    TypeHeader header;
};

/***************/
/* clipsLexeme */
/***************/
struct CLIPSLexeme {
    TypeHeader header;
    CLIPSLexeme *next;
    long count;
    bool permanent: 1;
    bool markedEphemeral: 1;
    bool neededSymbol: 1;
    unsigned int bucket: 29;
    const char *contents;
};

/**************/
/* clipsFloat */
/**************/
struct CLIPSFloat {
    TypeHeader header;
    CLIPSFloat *next;
    long count;
    bool permanent: 1;
    bool markedEphemeral: 1;
    bool neededFloat: 1;
    unsigned int bucket: 29;
    double contents;
};

/****************/
/* clipsInteger */
/****************/
struct CLIPSInteger {
    TypeHeader header;
    CLIPSInteger *next;
    long count;
    bool permanent: 1;
    bool markedEphemeral: 1;
    bool neededInteger: 1;
    unsigned int bucket: 29;
    long long contents;
};

/***************/
/* clipsBitMap */
/***************/
struct CLIPSBitMap {
    TypeHeader header;
    CLIPSBitMap *next;
    long count;
    bool permanent: 1;
    bool markedEphemeral: 1;
    bool neededBitMap: 1;
    unsigned int bucket: 29;
    const char *contents;
    unsigned short size;
};

/************************/
/* clipsExternalAddress */
/************************/
struct CLIPSExternalAddress{
    TypeHeader header;
    CLIPSExternalAddress *next;
    long count;
    bool permanent: 1;
    bool markedEphemeral: 1;
    bool neededPointer: 1;
    unsigned int bucket: 29;
    void *contents;
    unsigned short type;
};
struct Multifield;
struct Fact;
struct Instance;
/**************/
/* clipsValue */
/**************/
struct CLIPSValue {
    union {
        void *value;
        TypeHeader *header;
        CLIPSLexeme *lexemeValue;
        CLIPSFloat *floatValue;
        CLIPSInteger *integerValue;
        CLIPSVoid *voidValue;
        Multifield *multifieldValue;
        Fact *factValue;
        Instance *instanceValue;
        CLIPSExternalAddress *externalAddressValue;
    };
};

/**************/
/* multifield */
/**************/
struct Multifield {
    TypeHeader header;
    unsigned busyCount;
    size_t length;
    Multifield *next;
    CLIPSValue contents[1];
};

/************/
/* UDFValue */
/************/
struct UDFValue {
    void *supplementalInfo;
    union {
        void *value;
        TypeHeader *header;
        CLIPSLexeme *lexemeValue;
        CLIPSFloat *floatValue;
        CLIPSInteger *integerValue;
        CLIPSVoid *voidValue;
        Multifield *multifieldValue;
        Fact *factValue;
        Instance *instanceValue;
        CLIPSExternalAddress *externalAddressValue;
    };
    size_t begin;
    size_t range;
    UDFValue *next;
};
struct FunctionDefinition;
struct Expression;
/**************/
/* udfContext */
/**************/
struct UDFContext {
    Environment environment;
    void *context;
    FunctionDefinition *theFunction;
    unsigned int lastPosition;
    Expression *lastArg;
    UDFValue *returnValue;
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
    EntityPrintFunction *shortPrintFunction;
    EntityPrintFunction *longPrintFunction;
    EntityRecordDeleteFunction* deleteFunction;
    EntityEvaluationFunction *evaluateFunction;
    EntityRecordGetNextFunction* getNextFunction;
    EntityBusyCountFunction *decrementBusyCount;
    EntityBusyCountFunction *incrementBusyCount;
    EntityRecordPropagateDepthFunction* propagateDepth;
    EntityRecordMarkNeededFunction* markNeeded;
    EntityRecordInstallFunction* install;
    EntityRecordDeinstallFunction* deinstall;
    struct userData *usrData;
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
    PatternEntityRecord *theInfo;
    void *dependents;
    unsigned busyCount;
    unsigned long long timeTag;
};

#endif /* _H_entities */


