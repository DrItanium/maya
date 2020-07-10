/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*               CLIPS Version 6.40  02/20/20          */
/*                                                     */
/*                OBJECT SYSTEM DEFINITIONS            */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*      6.31: Optimization for marking relevant alpha nodes  */
/*            in the object pattern network.                 */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_object

#pragma once

#define _H_object

typedef struct defclassModule DEFCLASS_MODULE;
typedef struct defclass Defclass;
typedef struct packedClassLinks PACKED_CLASS_LINKS;
typedef struct classLink CLASS_LINK;
typedef struct slotName SLOT_NAME;
typedef struct slotDescriptor SlotDescriptor;
typedef struct defmessageHandler DefmessageHandler;

typedef struct instanceSlot InstanceSlot;

typedef struct instanceBuilder InstanceBuilder;
typedef struct instanceModifier InstanceModifier;

/* Maximum # of simultaneous class hierarchy traversals
   should be a multiple of BITS_PER_BYTE and less than MAX_INT      */

constexpr auto MAX_TRAVERSALS  = 256;
constexpr auto TRAVERSAL_BYTES = 32; /* (MAX_TRAVERSALS / BITS_PER_BYTE) */

constexpr auto VALUE_REQUIRED     = 0;
constexpr auto VALUE_PROHIBITED   = 1;
constexpr auto VALUE_NOT_REQUIRED = 2;

#include "Entities.h"
#include "Construct.h"
#include "Constraint.h"
#include "Defmodule.h"
#include "Evaluation.h"
#include "Expression.h"
#include "Multifield.h"
#include "Symbol.h"
#include "Match.h"

#include "ObjectReteMatch.h"

struct packedClassLinks {
    unsigned long classCount;
    Defclass **classArray;
};

struct defclassModule {
    struct defmoduleItemHeader header;
};

struct defclass {
    ConstructHeader header;
    bool installed: 1;
    bool system: 1;
    bool abstract: 1;
    bool reactive: 1;
    bool traceInstances: 1;
    bool traceSlots: 1;
    unsigned short id;
    unsigned busy;
    unsigned hashTableIndex;
    PACKED_CLASS_LINKS directSuperclasses;
    PACKED_CLASS_LINKS directSubclasses;
    PACKED_CLASS_LINKS allSuperclasses;
    SlotDescriptor *slots;
    SlotDescriptor **instanceTemplate;
    unsigned *slotNameMap;
    unsigned short slotCount;
    unsigned short localInstanceSlotCount;
    unsigned short instanceSlotCount;
    unsigned short maxSlotNameID;
    Instance *instanceList;
    Instance *instanceListBottom;
    DefmessageHandler *handlers;
    unsigned *handlerOrderMap;
    unsigned short handlerCount;
    Defclass *nxtHash;
    CLIPSBitMap *scopeMap;

    /*
     * Links this defclass to each of the terminal alpha nodes which could be
     * affected by a modification to an instance of it. This saves having to
     * iterate through every single terminal alpha for every single modification
     * to an instance of a defclass.
     */
    CLASS_ALPHA_LINK *relevant_terminal_alpha_nodes;

    char traversalRecord[TRAVERSAL_BYTES];
};

struct classLink {
    Defclass *cls;
    struct classLink *nxt;
};

struct slotName {
    unsigned hashTableIndex;
    unsigned use;
    unsigned short id;
    CLIPSLexeme *name;
    CLIPSLexeme *putHandlerName;
    struct slotName *nxt;
    unsigned long bsaveIndex;
};

struct instanceSlot {
    SlotDescriptor *desc;
    bool valueRequired: 1;
    bool override: 1;
    unsigned short type;
    union {
        void *value;
        TypeHeader *header;
        CLIPSLexeme *lexemeValue;
        CLIPSFloat *floatValue;
        CLIPSInteger *integerValue;
        CLIPSVoid *voidValue;
        Fact *factValue;
        Instance *instanceValue;
        Multifield *multifieldValue;
        CLIPSExternalAddress *externalAddressValue;
    };
};

struct slotDescriptor {
    bool shared: 1;
    bool multiple: 1;
    bool composite: 1;
    bool noInherit: 1;
    bool noWrite: 1;
    bool initializeOnly: 1;
    bool dynamicDefault: 1;
    bool defaultSpecified: 1;
    bool noDefault: 1;
    bool reactive: 1;
    bool publicVisibility: 1;
    bool createReadAccessor: 1;
    bool createWriteAccessor: 1;
    bool overrideMessageSpecified: 1;
    Defclass *cls;
    SLOT_NAME *slotName;
    CLIPSLexeme *overrideMessage;
    void *defaultValue;
    CONSTRAINT_RECORD *constraint;
    unsigned sharedCount;
    unsigned long bsaveIndex;
    InstanceSlot sharedValue;
};

struct instance {
    union {
        struct patternEntity patternHeader;
        TypeHeader header;
    };
    void *partialMatchList;
    InstanceSlot *basisSlots;
    bool installed: 1;
    bool garbage: 1;
    bool initSlotsCalled: 1;
    bool initializeInProgress: 1;
    bool reteSynchronized: 1;
    CLIPSLexeme *name;
    unsigned hashTableIndex;
    unsigned busy;
    Defclass *cls;
    Instance *prvClass, *nxtClass,
            *prvHash, *nxtHash,
            *prvList, *nxtList;
    InstanceSlot **slotAddresses,
            *slots;
};

struct defmessageHandler {
    ConstructHeader header;
    bool system: 1;
    unsigned type: 2;
    bool mark: 1;
    bool trace: 1;
    unsigned busy;
    Defclass *cls;
    unsigned short minParams;
    unsigned short maxParams;
    unsigned short localVarCount;
    Expression *actions;
};

struct instanceBuilder {
    Environment ibEnv;
    Defclass *ibDefclass;
    CLIPSValue *ibValueArray;
};

struct instanceModifier {
    Environment imEnv;
    Instance *imOldInstance;
    CLIPSValue *imValueArray;
    char *changeMap;
};

#endif /* _H_object */





