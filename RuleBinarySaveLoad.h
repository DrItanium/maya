/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/30/16            */
/*                                                     */
/*           DEFRULE BSAVE/BLOAD HEADER FILE           */
/*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    defrule construct.                                     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed CONFLICT_RESOLUTION_STRATEGIES,        */
/*            DYNAMIC_SALIENCE, and LOGICAL_DEPENDENCIES     */
/*            compilation flags.                             */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Added support for alpha memories.              */
/*                                                           */
/*            Added salience groups to improve performance   */
/*            with large numbers of activations of different */
/*            saliences.                                     */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_rulebin

#pragma once

#define _H_rulebin

#include "Construct.h"
#include "DefmoduleBinarySaveLoad.h"
#include "Network.h"

struct bsaveDefrule {
    struct bsaveConstructHeader header;
    int salience;
    unsigned short localVarCnt;
    unsigned int complexity: 12;
    bool autoFocus: 1;
    unsigned long dynamicSalience;
    unsigned long actions;
    unsigned long logicalJoin;
    unsigned long lastJoin;
    unsigned long disjunct;
};

struct bsavePatternNodeHeader {
    unsigned long entryJoin;
    unsigned long rightHash;
    bool singlefieldNode: 1;
    bool multifieldNode: 1;
    bool stopNode: 1;
    bool blocked: 1;
    bool initialize: 1;
    bool marked: 1;
    bool beginSlot: 1;
    bool endSlot: 1;
    bool selector: 1;
};

struct bsaveDefruleModule {
    struct bsaveDefmoduleItemHeader header;
};

struct bsaveJoinLink {
    char enterDirection;
    unsigned long join;
    unsigned long next;
};

struct bsaveJoinNode {
    bool firstJoin: 1;
    bool logicalJoin: 1;
    bool joinFromTheRight: 1;
    bool patternIsNegated: 1;
    bool patternIsExists: 1;
    unsigned int rhsType: 3;
    unsigned int depth: 7;
    unsigned long networkTest;
    unsigned long secondaryNetworkTest;
    unsigned long leftHash;
    unsigned long rightHash;
    unsigned long rightSideEntryStructure;
    unsigned long nextLinks;
    unsigned long lastLevel;
    unsigned long rightMatchNode;
    unsigned long ruleToActivate;
};

constexpr auto RULEBIN_DATA = 20;

struct defruleBinaryData {
    unsigned long NumberOfDefruleModules;
    unsigned long NumberOfDefrules;
    unsigned long NumberOfJoins;
    unsigned long NumberOfLinks;
    unsigned long RightPrimeIndex;
    unsigned long LeftPrimeIndex;
    struct defruleModule *ModuleArray;
    Defrule *DefruleArray;
    struct joinNode *JoinArray;
    struct joinLink *LinkArray;
};

#define DefruleBinaryData(theEnv) ((defruleBinaryData *) GetEnvironmentData(theEnv,RULEBIN_DATA))

#define BloadDefrulePointer(x, i) ((Defrule *) ((i == ULONG_MAX) ? nullptr : &x[i]))
#define BsaveJoinIndex(joinPtr) ((joinPtr == nullptr) ? ULONG_MAX :  ((joinNode *) joinPtr)->bsaveID)
#define BloadJoinPointer(i) ((joinNode *) ((i == ULONG_MAX) ? nullptr : &DefruleBinaryData(theEnv)->JoinArray[i]))
#define BsaveJoinLinkIndex(linkPtr) ((linkPtr == nullptr) ? ULONG_MAX :  ((joinLink *) linkPtr)->bsaveID)
#define BloadJoinLinkPointer(i) ((joinLink *) ((i == ULONG_MAX) ? nullptr : &DefruleBinaryData(theEnv)->LinkArray[i]))

void DefruleBinarySetup(const Environment&);
void UpdatePatternNodeHeader(const Environment&, PatternNodeHeader *,
                             struct bsavePatternNodeHeader *);
void AssignBsavePatternHeaderValues(const Environment&, struct bsavePatternNodeHeader *,
                                    PatternNodeHeader *);
void *BloadDefruleModuleReference(const Environment&, unsigned long);

#endif /* _H_rulebin */





