/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  11/01/16             */
/*                                                     */
/*                 NETWORK HEADER FILE                 */
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
/*      6.30: Added support for hashed memories.             */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_network

#pragma once

#define _H_network

struct alphaMemoryHash;
struct betaMemory;
struct joinLink;
struct joinNode;
struct patternNodeHashEntry;
typedef struct patternNodeHeader PatternNodeHeader;

#include "Entities.h"

struct patternNodeHeader {
    struct alphaMemoryHash *firstHash;
    struct alphaMemoryHash *lastHash;
    struct joinNode *entryJoin;
    Expression *rightHash;
    bool singlefieldNode: 1;
    bool multifieldNode: 1;
    bool stopNode: 1;
    bool initialize: 1;
    bool marked: 1;
    bool beginSlot: 1;
    bool endSlot: 1;
    bool selector: 1;
};

#include "Match.h"

struct patternNodeHashEntry {
    void *parent;
    void *child;
    int type;
    void *value;
    struct patternNodeHashEntry *next;
};

constexpr auto SIZE_PATTERN_HASH = 16231;

struct alphaMemoryHash {
    unsigned long bucket;
    struct patternNodeHeader *owner;
    PartialMatch *alphaMemory;
    PartialMatch *endOfQueue;
    struct alphaMemoryHash *nextHash;
    struct alphaMemoryHash *prevHash;
    struct alphaMemoryHash *next;
    struct alphaMemoryHash *prev;
};

typedef struct alphaMemoryHash ALPHA_MEMORY_HASH;

#include "Defrule.h"

constexpr auto INITIAL_BETA_HASH_SIZE = 17;

struct betaMemory {
    unsigned long size;
    unsigned long count;
    struct partialMatch **beta;
    struct partialMatch **last;
};

struct joinLink {
    char enterDirection;
    struct joinNode *join;
    struct joinLink *next;
    unsigned long bsaveID;
};

struct joinNode {
    bool firstJoin: 1;
    bool logicalJoin: 1;
    bool joinFromTheRight: 1;
    bool patternIsNegated: 1;
    bool patternIsExists: 1;
    bool initialize: 1;
    bool marked: 1;
    unsigned int rhsType: 3;
    unsigned int depth: 16;
    unsigned long bsaveID;
    long long memoryLeftAdds;
    long long memoryRightAdds;
    long long memoryLeftDeletes;
    long long memoryRightDeletes;
    long long memoryCompares;
    struct betaMemory *leftMemory;
    struct betaMemory *rightMemory;
    Expression *networkTest;
    Expression *secondaryNetworkTest;
    Expression *leftHash;
    Expression *rightHash;
    void *rightSideEntryStructure;
    struct joinLink *nextLinks;
    struct joinNode *lastLevel;
    struct joinNode *rightMatchNode;
    Defrule *ruleToActivate;
};

#endif /* _H_network */




