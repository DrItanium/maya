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


#include "Entities.hxx"
#include "Match.h"
#include "Defrule.h"

struct alphaMemoryHash;
struct betaMemory;
struct joinLink;
struct joinNode;
struct patternNodeHashEntry;

struct PatternNodeHeader {
    std::shared_ptr<alphaMemoryHash> firstHash;
    std::shared_ptr<alphaMemoryHash> lastHash;
    std::shared_ptr<joinNode> entryJoin;
    std::shared_ptr<Expression> rightHash;
    bool singlefieldNode: 1;
    bool multifieldNode: 1;
    bool stopNode: 1;
    bool initialize: 1;
    bool marked: 1;
    bool beginSlot: 1;
    bool endSlot: 1;
    bool selector: 1;
};


struct patternNodeHashEntry {
    std::any parent;
    std::any child;
    int type;
    std::any value;
    std::shared_ptr<patternNodeHashEntry> next;
};

constexpr auto SIZE_PATTERN_HASH = 16231;
struct PartialMatch;
struct alphaMemoryHash {
    unsigned long bucket;
    std::shared_ptr<PatternNodeHeader> owner;
    std::shared_ptr<PartialMatch> alphaMemory;
    std::shared_ptr<PartialMatch> endOfQueue;
    std::shared_ptr<alphaMemoryHash> nextHash;
    std::shared_ptr<alphaMemoryHash> prevHash;
    std::shared_ptr<alphaMemoryHash> next;
    std::shared_ptr<alphaMemoryHash> prev;
};

typedef struct alphaMemoryHash ALPHA_MEMORY_HASH;


constexpr auto INITIAL_BETA_HASH_SIZE = 17;

struct betaMemory {
    unsigned long size;
    unsigned long count;
    std::shared_ptr<PartialMatch>*beta;
    std::shared_ptr<PartialMatch>*last;
};

struct joinLink {
    char enterDirection;
    std::shared_ptr<joinNode> join;
    std::shared_ptr<joinLink> next;
    unsigned long bsaveID;
};
struct Defrule;
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
    std::shared_ptr<betaMemory > leftMemory;
    std::shared_ptr<betaMemory > rightMemory;
    std::shared_ptr<Expression > networkTest;
    std::shared_ptr<Expression > secondaryNetworkTest;
    std::shared_ptr<Expression > leftHash;
    std::shared_ptr<Expression > rightHash;
    std::any rightSideEntryStructure;
    std::shared_ptr<joinLink> nextLinks;
    std::shared_ptr<joinNode> lastLevel;
    std::shared_ptr<joinNode> rightMatchNode;
    std::shared_ptr<Defrule> ruleToActivate;
};

#endif /* _H_network */




