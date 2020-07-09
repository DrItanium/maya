/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  08/25/16            */
/*                                                     */
/*                                                     */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added support for hashed alpha memories.       */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_objrtfnx

#pragma once

#define _H_objrtfnx


#include "Evaluation.h"
#include "Match.h"
#include "Object.h"
#include "ObjectReteMatch.h"

struct ObjectMatchVar1 {
    unsigned short whichSlot;
    unsigned short whichPattern;
    unsigned short whichField;
    bool objectAddress: 1;
    bool allFields: 1;
    bool lhs: 1;
    bool rhs: 1;
};

struct ObjectMatchVar2 {
    unsigned short whichSlot;
    unsigned short whichPattern;
    unsigned short beginningOffset;
    unsigned short endOffset;
    bool fromBeginning: 1;
    bool fromEnd: 1;
    bool lhs: 1;
    bool rhs: 1;
};

struct ObjectMatchLength {
    unsigned minLength: 15;
    bool exactly: 1;
};

struct ObjectCmpPNConstant {
    unsigned short offset;
    bool pass: 1;
    bool fail: 1;
    bool general: 1;
    bool fromBeginning: 1;
};

struct ObjectCmpPNSingleSlotVars1 {
    unsigned short firstSlot;
    unsigned short secondSlot;
    bool pass: 1;
    bool fail: 1;
};

struct ObjectCmpPNSingleSlotVars2 {
    unsigned short firstSlot;
    unsigned short secondSlot;
    unsigned short offset;
    bool pass: 1;
    bool fail: 1;
    bool fromBeginning: 1;
};

struct ObjectCmpPNSingleSlotVars3 {
    unsigned short firstSlot;
    unsigned short secondSlot;
    unsigned short firstOffset;
    unsigned short secondOffset;
    bool pass: 1;
    bool fail: 1;
    bool firstFromBeginning: 1;
    bool secondFromBeginning: 1;
};

struct ObjectCmpJoinSingleSlotVars1 {
    unsigned short firstSlot;
    unsigned short secondSlot;
    unsigned short firstPattern;
    unsigned short secondPattern;
    bool pass: 1;
    bool fail: 1;
    bool firstPatternLHS: 1;
    bool firstPatternRHS: 1;
    bool secondPatternLHS: 1;
    bool secondPatternRHS: 1;
};

struct ObjectCmpJoinSingleSlotVars2 {
    unsigned short firstSlot;
    unsigned short secondSlot;
    unsigned short firstPattern;
    unsigned short secondPattern;
    unsigned short offset;
    bool pass: 1;
    bool fromBeginning: 1;
    bool fail: 1;
    bool firstPatternLHS: 1;
    bool firstPatternRHS: 1;
    bool secondPatternLHS: 1;
    bool secondPatternRHS: 1;
};

struct ObjectCmpJoinSingleSlotVars3 {
    unsigned short firstSlot;
    unsigned short secondSlot;
    unsigned short firstPattern;
    unsigned short secondPattern;
    unsigned short firstOffset;
    unsigned short secondOffset;
    bool pass: 1;
    bool fail: 1;
    bool firstFromBeginning: 1;
    bool secondFromBeginning: 1;
    bool firstPatternLHS: 1;
    bool firstPatternRHS: 1;
    bool secondPatternLHS: 1;
    bool secondPatternRHS: 1;
};

constexpr auto OBJECT_RETE_DATA = 35;

struct objectReteData {
    Instance *CurrentPatternObject;
    InstanceSlot *CurrentPatternObjectSlot;
    size_t CurrentObjectSlotLength;
    struct multifieldMarker *CurrentPatternObjectMarks;
    struct entityRecord ObjectGVInfo1;
    struct entityRecord ObjectGVInfo2;
    struct entityRecord ObjectGVPNInfo1;
    struct entityRecord ObjectGVPNInfo2;
    struct entityRecord ObjectCmpConstantInfo;
    struct entityRecord LengthTestInfo;
    struct entityRecord PNSimpleCompareInfo1;
    struct entityRecord PNSimpleCompareInfo2;
    struct entityRecord PNSimpleCompareInfo3;
    struct entityRecord JNSimpleCompareInfo1;
    struct entityRecord JNSimpleCompareInfo2;
    struct entityRecord JNSimpleCompareInfo3;
    OBJECT_MATCH_ACTION *ObjectMatchActionQueue;
    OBJECT_PATTERN_NODE *ObjectPatternNetworkPointer;
    OBJECT_ALPHA_NODE *ObjectPatternNetworkTerminalPointer;
    bool DelayObjectPatternMatching;
    unsigned long long CurrentObjectMatchTimeTag;
    unsigned long long UseEntityTimeTag;
};

#define ObjectReteData(theEnv) ((objectReteData *) GetEnvironmentData(theEnv,OBJECT_RETE_DATA))

void InstallObjectPrimitives(Environment *);
bool ObjectCmpConstantFunction(Environment *, void *, UDFValue *);

#endif /* _H_objrtfnx */






