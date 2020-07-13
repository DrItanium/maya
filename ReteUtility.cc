/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  10/03/19             */
/*                                                     */
/*                 RETE UTILITY MODULE                 */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of utility functions useful to    */
/*   other modules.                                          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed INCREMENTAL_RESET compilation flag.    */
/*                                                           */
/*            Rule with exists CE has incorrect activation.  */
/*            DR0867                                         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Support for join network changes.              */
/*                                                           */
/*            Support for using an asterick (*) to indicate  */
/*            that existential patterns are matched.         */
/*                                                           */
/*            Support for partial match changes.             */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added support for hashed memories.             */
/*                                                           */
/*            Removed pseudo-facts used in not CEs.          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.31: Bug fix to prevent rule activations for        */
/*            partial matches being deleted.                 */
/*                                                           */
/*      6.40: Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            Incremental reset is always enabled.           */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#include <cstdio>

#include "Setup.h"


#include "Drive.h"
#include "Engine.h"
#include "Environment.h"
#include "IncrementalReset.h"
#include "Match.h"
#include "MemoryAllocation.h"
#include "Defmodule.h"
#include "Pattern.h"
#include "PrintUtility.h"
#include "Retract.h"
#include "Router.h"
#include "RuleCommands.h"

#include "ReteUtility.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void TraceErrorToRuleDriver(const Environment::Ptr&, struct joinNode *, const char *, int, bool);
static struct alphaMemoryHash *FindAlphaMemory(const Environment::Ptr&, PatternNodeHeader *, unsigned long);
static unsigned long AlphaMemoryHashValue(PatternNodeHeader *, unsigned long);
static void UnlinkAlphaMemory(const Environment::Ptr&, PatternNodeHeader *, struct alphaMemoryHash *);
static void UnlinkAlphaMemoryBucketSiblings(const Environment::Ptr&, struct alphaMemoryHash *);
static void InitializePMLinks(PartialMatch *);
static void UnlinkBetaPartialMatchfromAlphaAndBetaLineage(PartialMatch *);
static int CountPriorPatterns(joinNode *);
static void ResizeBetaMemory(const Environment::Ptr&, struct betaMemory *);
static void ResetBetaMemory(const Environment::Ptr&, struct betaMemory *);
#if (BLOAD_AND_BSAVE)
static void TagNetworkTraverseJoins(const Environment::Ptr&, unsigned long *, unsigned long *, struct joinNode *);
#endif

/***********************************************************/
/* PrintPartialMatch: Prints out the list of fact indices  */
/*   and/or instance names associated with a partial match */
/*   or rule instantiation.                                */
/***********************************************************/
void PrintPartialMatch(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        PartialMatch::Ptr list) {
    PatternEntity *matchingItem;
    unsigned short i;

    for (i = 0; i < list->bcount;) {
        if ((get_nth_pm_match(list, i) != nullptr) &&
            (get_nth_pm_match(list, i)->matchingItem != nullptr)) {
            matchingItem = get_nth_pm_match(list, i)->matchingItem;
            (*matchingItem->theInfo->base.shortPrintFunction)(theEnv, logicalName, matchingItem);
        } else { WriteString(theEnv, logicalName, "*"); }
        i++;
        if (i < list->bcount) WriteString(theEnv, logicalName, ",");
    }
}

/**********************************************/
/* CopyPartialMatch:  Copies a partial match. */
/**********************************************/
PartialMatch *CopyPartialMatch(
        const Environment::Ptr&theEnv,
        PartialMatch *list) {
    PartialMatch *linker;
    unsigned short i;

    linker = get_var_struct(theEnv, PartialMatch, sizeof(genericMatch) *
                                                  (list->bcount - 1));

    InitializePMLinks(linker);
    linker->betaMemory = true;
    linker->busy = false;
    linker->rhsMemory = false;
    linker->deleting = false;
    linker->bcount = list->bcount;
    linker->hashValue = 0;

    for (i = 0; i < linker->bcount; i++) linker->binds[i] = list->binds[i];

    return (linker);
}

/****************************/
/* CreateEmptyPartialMatch: */
/****************************/
PartialMatch *CreateEmptyPartialMatch(
        const Environment::Ptr&theEnv) {
    PartialMatch *linker;

    linker = get_struct(theEnv, PartialMatch);

    InitializePMLinks(linker);
    linker->betaMemory = true;
    linker->busy = false;
    linker->rhsMemory = false;
    linker->deleting = false;
    linker->bcount = 1;
    linker->hashValue = 0;
    linker->binds[0].gm.theValue = nullptr;

    return (linker);
}

/**********************/
/* InitializePMLinks: */
/**********************/
static void InitializePMLinks(
        PartialMatch *theMatch) {
    theMatch->nextInMemory = nullptr;
    theMatch->prevInMemory = nullptr;
    theMatch->nextRightChild = nullptr;
    theMatch->prevRightChild = nullptr;
    theMatch->nextLeftChild = nullptr;
    theMatch->prevLeftChild = nullptr;
    theMatch->children = nullptr;
    theMatch->rightParent = nullptr;
    theMatch->leftParent = nullptr;
    theMatch->blockList = nullptr;
    theMatch->nextBlocked = nullptr;
    theMatch->prevBlocked = nullptr;
    theMatch->marker = nullptr;
    theMatch->dependents = nullptr;
}

/**********************/
/* UpdateBetaPMLinks: */
/**********************/
void UpdateBetaPMLinks(
        const Environment::Ptr&theEnv,
        PartialMatch *thePM,
        PartialMatch *lhsBinds,
        PartialMatch *rhsBinds,
        struct joinNode *join,
        unsigned long hashValue,
        int side) {
    unsigned long betaLocation;
    struct betaMemory *theMemory;

    if (side == CLIPS_LHS) {
        theMemory = join->leftMemory;
        thePM->rhsMemory = false;
    } else {
        theMemory = join->rightMemory;
        thePM->rhsMemory = true;
    }

    thePM->hashValue = hashValue;

    /*================================*/
    /* Update the node's linked list. */
    /*================================*/

    betaLocation = hashValue % theMemory->size;

    if (side == CLIPS_LHS) {
        thePM->nextInMemory = theMemory->beta[betaLocation];
        if (theMemory->beta[betaLocation] != nullptr) { theMemory->beta[betaLocation]->prevInMemory = thePM; }
        theMemory->beta[betaLocation] = thePM;
    } else {
        if (theMemory->last[betaLocation] != nullptr) {
            theMemory->last[betaLocation]->nextInMemory = thePM;
            thePM->prevInMemory = theMemory->last[betaLocation];
        } else { theMemory->beta[betaLocation] = thePM; }

        theMemory->last[betaLocation] = thePM;
    }

    theMemory->count++;
    if (side == CLIPS_LHS) { join->memoryLeftAdds++; }
    else { join->memoryRightAdds++; }

    thePM->owner = join;

    /*======================================*/
    /* Update the alpha memory linked list. */
    /*======================================*/

    if (rhsBinds != nullptr) {
        thePM->nextRightChild = rhsBinds->children;
        if (rhsBinds->children != nullptr) { rhsBinds->children->prevRightChild = thePM; }
        rhsBinds->children = thePM;
        thePM->rightParent = rhsBinds;
    }

    /*=====================================*/
    /* Update the beta memory linked list. */
    /*=====================================*/

    if (lhsBinds != nullptr) {
        thePM->nextLeftChild = lhsBinds->children;
        if (lhsBinds->children != nullptr) { lhsBinds->children->prevLeftChild = thePM; }
        lhsBinds->children = thePM;
        thePM->leftParent = lhsBinds;
    }

    if (!DefruleData(theEnv)->BetaMemoryResizingFlag) { return; }

    if ((theMemory->size > 1) &&
        (theMemory->count > (theMemory->size * 11))) { ResizeBetaMemory(theEnv, theMemory); }
}

/**********************************************************/
/* AddBlockedLink: Adds a link between a partial match in */
/*   the beta memory of a join (with a negated RHS) and a */
/*   partial match in its right memory that prevents the  */
/*   partial match from being satisfied and propagated to */
/*   the next join in the rule.                           */
/**********************************************************/
void AddBlockedLink(
        PartialMatch *thePM,
        PartialMatch *rhsBinds) {
    thePM->marker = rhsBinds;
    thePM->nextBlocked = rhsBinds->blockList;
    if (rhsBinds->blockList != nullptr) { rhsBinds->blockList->prevBlocked = thePM; }
    rhsBinds->blockList = thePM;
}

/*************************************************************/
/* RemoveBlockedLink: Removes a link between a partial match */
/*   in the beta memory of a join (with a negated RHS) and a */
/*   partial match in its right memory that prevents the     */
/*   partial match from being satisfied and propagated to    */
/*   the next join in the rule.                              */
/*************************************************************/
void RemoveBlockedLink(
        PartialMatch *thePM) {
    PartialMatch *blocker;

    if (thePM->prevBlocked == nullptr) {
        blocker = (PartialMatch *) thePM->marker;
        blocker->blockList = thePM->nextBlocked;
    } else { thePM->prevBlocked->nextBlocked = thePM->nextBlocked; }

    if (thePM->nextBlocked != nullptr) { thePM->nextBlocked->prevBlocked = thePM->prevBlocked; }

    thePM->nextBlocked = nullptr;
    thePM->prevBlocked = nullptr;
    thePM->marker = nullptr;
}

/***********************************/
/* UnlinkBetaPMFromNodeAndLineage: */
/***********************************/
void UnlinkBetaPMFromNodeAndLineage(
        const Environment::Ptr&theEnv,
        struct joinNode *join,
        PartialMatch *thePM,
        int side) {
    unsigned long betaLocation;
    struct betaMemory *theMemory;

    if (side == CLIPS_LHS) { theMemory = join->leftMemory; }
    else { theMemory = join->rightMemory; }

    /*=============================================*/
    /* Update the nextInMemory/prevInMemory links. */
    /*=============================================*/

    theMemory->count--;

    if (side == CLIPS_LHS) { join->memoryLeftDeletes++; }
    else { join->memoryRightDeletes++; }

    betaLocation = thePM->hashValue % theMemory->size;

    if ((side == CLIPS_RHS) &&
        (theMemory->last[betaLocation] == thePM)) { theMemory->last[betaLocation] = thePM->prevInMemory; }

    if (thePM->prevInMemory == nullptr) {
        betaLocation = thePM->hashValue % theMemory->size;
        theMemory->beta[betaLocation] = thePM->nextInMemory;
    } else { thePM->prevInMemory->nextInMemory = thePM->nextInMemory; }

    if (thePM->nextInMemory != nullptr) { thePM->nextInMemory->prevInMemory = thePM->prevInMemory; }

    thePM->nextInMemory = nullptr;
    thePM->prevInMemory = nullptr;

    UnlinkBetaPartialMatchfromAlphaAndBetaLineage(thePM);

    if (!DefruleData(theEnv)->BetaMemoryResizingFlag) { return; }

    if ((theMemory->count == 0) && (theMemory->size > 1)) { ResetBetaMemory(theEnv, theMemory); }
}

/*************************/
/* UnlinkNonLeftLineage: */
/*************************/
void UnlinkNonLeftLineage(
        const Environment::Ptr&theEnv,
        struct joinNode *join,
        PartialMatch *thePM,
        int side) {
    unsigned long betaLocation;
    struct betaMemory *theMemory;
    PartialMatch *tempPM;

    if (side == CLIPS_LHS) { theMemory = join->leftMemory; }
    else { theMemory = join->rightMemory; }

    /*=============================================*/
    /* Update the nextInMemory/prevInMemory links. */
    /*=============================================*/

    theMemory->count--;

    if (side == CLIPS_LHS) { join->memoryLeftDeletes++; }
    else { join->memoryRightDeletes++; }

    betaLocation = thePM->hashValue % theMemory->size;

    if ((side == CLIPS_RHS) &&
        (theMemory->last[betaLocation] == thePM)) { theMemory->last[betaLocation] = thePM->prevInMemory; }

    if (thePM->prevInMemory == nullptr) {
        betaLocation = thePM->hashValue % theMemory->size;
        theMemory->beta[betaLocation] = thePM->nextInMemory;
    } else { thePM->prevInMemory->nextInMemory = thePM->nextInMemory; }

    if (thePM->nextInMemory != nullptr) { thePM->nextInMemory->prevInMemory = thePM->prevInMemory; }

    /*=========================*/
    /* Update the alpha lists. */
    /*=========================*/

    if (thePM->prevRightChild == nullptr) {
        if (thePM->rightParent != nullptr) {
            thePM->rightParent->children = thePM->nextRightChild;
            if (thePM->nextRightChild != nullptr) {
                thePM->rightParent->children = thePM->nextRightChild;
                thePM->nextRightChild->rightParent = thePM->rightParent;
            }
        }
    } else { thePM->prevRightChild->nextRightChild = thePM->nextRightChild; }

    if (thePM->nextRightChild != nullptr) { thePM->nextRightChild->prevRightChild = thePM->prevRightChild; }

    /*===========================*/
    /* Update the blocked lists. */
    /*===========================*/

    if (thePM->prevBlocked == nullptr) {
        tempPM = (PartialMatch *) thePM->marker;

        if (tempPM != nullptr) { tempPM->blockList = thePM->nextBlocked; }
    } else { thePM->prevBlocked->nextBlocked = thePM->nextBlocked; }

    if (thePM->nextBlocked != nullptr) { thePM->nextBlocked->prevBlocked = thePM->prevBlocked; }

    if (!DefruleData(theEnv)->BetaMemoryResizingFlag) { return; }

    if ((theMemory->count == 0) && (theMemory->size > 1)) { ResetBetaMemory(theEnv, theMemory); }
}

/*******************************************************************/
/* UnlinkBetaPartialMatchfromAlphaAndBetaLineage: Removes the      */
/*   lineage links from a beta memory partial match. This removes  */
/*   the links between this partial match and its left and right   */
/*   memory parents. It also removes the links between this        */
/*   partial match and any of its children in other beta memories. */
/*******************************************************************/
static void UnlinkBetaPartialMatchfromAlphaAndBetaLineage(
        PartialMatch *thePM) {
    PartialMatch *tempPM;

    /*=========================*/
    /* Update the alpha lists. */
    /*=========================*/

    if (thePM->prevRightChild == nullptr) {
        if (thePM->rightParent != nullptr) { thePM->rightParent->children = thePM->nextRightChild; }
    } else { thePM->prevRightChild->nextRightChild = thePM->nextRightChild; }

    if (thePM->nextRightChild != nullptr) { thePM->nextRightChild->prevRightChild = thePM->prevRightChild; }

    thePM->rightParent = nullptr;
    thePM->nextRightChild = nullptr;
    thePM->prevRightChild = nullptr;

    /*========================*/
    /* Update the beta lists. */
    /*========================*/

    if (thePM->prevLeftChild == nullptr) {
        if (thePM->leftParent != nullptr) { thePM->leftParent->children = thePM->nextLeftChild; }
    } else { thePM->prevLeftChild->nextLeftChild = thePM->nextLeftChild; }

    if (thePM->nextLeftChild != nullptr) { thePM->nextLeftChild->prevLeftChild = thePM->prevLeftChild; }

    thePM->leftParent = nullptr;
    thePM->nextLeftChild = nullptr;
    thePM->prevLeftChild = nullptr;

    /*===========================*/
    /* Update the blocked lists. */
    /*===========================*/

    if (thePM->prevBlocked == nullptr) {
        tempPM = (PartialMatch *) thePM->marker;

        if (tempPM != nullptr) { tempPM->blockList = thePM->nextBlocked; }
    } else { thePM->prevBlocked->nextBlocked = thePM->nextBlocked; }

    if (thePM->nextBlocked != nullptr) { thePM->nextBlocked->prevBlocked = thePM->prevBlocked; }

    thePM->marker = nullptr;
    thePM->nextBlocked = nullptr;
    thePM->prevBlocked = nullptr;

    /*===============================================*/
    /* Remove parent reference from the child links. */
    /*===============================================*/

    if (thePM->children != nullptr) {
        if (thePM->rhsMemory) {
            for (tempPM = thePM->children; tempPM != nullptr; tempPM = tempPM->nextRightChild) { tempPM->rightParent = nullptr; }
        } else {
            for (tempPM = thePM->children; tempPM != nullptr; tempPM = tempPM->nextLeftChild) { tempPM->leftParent = nullptr; }
        }

        thePM->children = nullptr;
    }
}

/********************************************************/
/* MergePartialMatches: Merges two partial matches. The */
/*   second match should either be nullptr (indicating a   */
/*   negated CE) or contain a single match.             */
/********************************************************/
PartialMatch *MergePartialMatches(
        const Environment::Ptr&theEnv,
        PartialMatch *lhsBind,
        PartialMatch *rhsBind) {
    PartialMatch *linker;
    static PartialMatch mergeTemplate = {1}; /* betaMemory is true, remainder are 0 or nullptr */

    /*=================================*/
    /* Allocate the new partial match. */
    /*=================================*/

    linker = get_var_struct(theEnv, PartialMatch, sizeof(genericMatch) * lhsBind->bcount);

    /*============================================*/
    /* Set the flags to their appropriate values. */
    /*============================================*/
    memcpy(linker, &mergeTemplate, sizeof(PartialMatch) - sizeof(genericMatch));

    linker->deleting = false;
    linker->bcount = lhsBind->bcount + 1;

    /*========================================================*/
    /* Copy the bindings of the partial match being extended. */
    /*========================================================*/

    memcpy(linker->binds, lhsBind->binds, sizeof(genericMatch) * lhsBind->bcount);

    /*===================================*/
    /* Add the binding of the rhs match. */
    /*===================================*/

    if (rhsBind == nullptr) { linker->binds[lhsBind->bcount].gm.theValue = nullptr; }
    else { linker->binds[lhsBind->bcount].gm.theValue = rhsBind->binds[0].gm.theValue; }

    return linker;
}

/*******************************************************************/
/* InitializePatternHeader: Initializes a pattern header structure */
/*   (used by the fact and instance pattern matchers).             */
/*******************************************************************/
void InitializePatternHeader(
        const Environment::Ptr&theEnv,
        PatternNodeHeader *theHeader) {
#if MAC_XCD
#pragma unused(theEnv)
#endif
    theHeader->firstHash = nullptr;
    theHeader->lastHash = nullptr;
    theHeader->entryJoin = nullptr;
    theHeader->rightHash = nullptr;
    theHeader->singlefieldNode = false;
    theHeader->multifieldNode = false;
    theHeader->stopNode = false;
    theHeader->initialize = true;
    theHeader->marked = false;
    theHeader->beginSlot = false;
    theHeader->endSlot = false;
    theHeader->selector = false;
}

/******************************************************************/
/* CreateAlphaMatch: Given a pointer to an entity (such as a fact */
/*   or instance) which matched a pattern, this function creates  */
/*   a partial match suitable for storing in the alpha memory of  */
/*   the pattern network. Note that the multifield markers which  */
/*   are passed as a calling argument are copied (thus the caller */
/*   is still responsible for freeing these data structures).     */
/******************************************************************/
PartialMatch *CreateAlphaMatch(
        const Environment::Ptr&theEnv,
        void *theEntity,
        struct multifieldMarker *markers,
        PatternNodeHeader *theHeader,
        unsigned long hashOffset) {
    PartialMatch *theMatch;
    struct alphaMatch *afbtemp;
    unsigned long hashValue;
    struct alphaMemoryHash *theAlphaMemory;

    /*==================================================*/
    /* Create the alpha match and intialize its values. */
    /*==================================================*/

    theMatch = get_struct(theEnv, PartialMatch);
    InitializePMLinks(theMatch);
    theMatch->betaMemory = false;
    theMatch->busy = false;
    theMatch->deleting = false;
    theMatch->bcount = 1;
    theMatch->hashValue = hashOffset;

    afbtemp = get_struct(theEnv, alphaMatch);
    afbtemp->next = nullptr;
    afbtemp->matchingItem = (PatternEntity *) theEntity;

    if (markers != nullptr) { afbtemp->markers = CopyMultifieldMarkers(theEnv, markers); }
    else { afbtemp->markers = nullptr; }

    theMatch->binds[0].gm.theMatch = afbtemp;

    /*============================================*/
    /* Find the alpha memory of the pattern node. */
    /*============================================*/

    hashValue = AlphaMemoryHashValue(theHeader, hashOffset);
    theAlphaMemory = FindAlphaMemory(theEnv, theHeader, hashValue);
    afbtemp->bucket = hashValue;

    /*============================================*/
    /* Create an alpha memory if it wasn't found. */
    /*============================================*/

    if (theAlphaMemory == nullptr) {
        theAlphaMemory = get_struct(theEnv, alphaMemoryHash);
        theAlphaMemory->bucket = hashValue;
        theAlphaMemory->owner = theHeader;
        theAlphaMemory->alphaMemory = nullptr;
        theAlphaMemory->endOfQueue = nullptr;
        theAlphaMemory->nextHash = nullptr;

        theAlphaMemory->next = DefruleData(theEnv)->AlphaMemoryTable[hashValue];
        if (theAlphaMemory->next != nullptr) { theAlphaMemory->next->prev = theAlphaMemory; }

        theAlphaMemory->prev = nullptr;
        DefruleData(theEnv)->AlphaMemoryTable[hashValue] = theAlphaMemory;

        if (theHeader->firstHash == nullptr) {
            theHeader->firstHash = theAlphaMemory;
            theHeader->lastHash = theAlphaMemory;
            theAlphaMemory->prevHash = nullptr;
        } else {
            theHeader->lastHash->nextHash = theAlphaMemory;
            theAlphaMemory->prevHash = theHeader->lastHash;
            theHeader->lastHash = theAlphaMemory;
        }
    }

    /*====================================*/
    /* Store the alpha match in the alpha */
    /* memory of the pattern node.        */
    /*====================================*/

    theMatch->prevInMemory = theAlphaMemory->endOfQueue;
    if (theAlphaMemory->endOfQueue == nullptr) {
        theAlphaMemory->alphaMemory = theMatch;
        theAlphaMemory->endOfQueue = theMatch;
    } else {
        theAlphaMemory->endOfQueue->nextInMemory = theMatch;
        theAlphaMemory->endOfQueue = theMatch;
    }

    /*===================================================*/
    /* Return a pointer to the newly create alpha match. */
    /*===================================================*/

    return (theMatch);
}

/*******************************************/
/* CopyMultifieldMarkers: Copies a list of */
/*   multifieldMarker data structures.     */
/*******************************************/
struct multifieldMarker *CopyMultifieldMarkers(
        const Environment::Ptr&theEnv,
        struct multifieldMarker *theMarkers) {
    struct multifieldMarker *head = nullptr, *lastMark = nullptr, *newMark;

    while (theMarkers != nullptr) {
        newMark = get_struct(theEnv, multifieldMarker);
        newMark->next = nullptr;
        newMark->whichField = theMarkers->whichField;
        newMark->where = theMarkers->where;
        newMark->startPosition = theMarkers->startPosition;
        newMark->range = theMarkers->range;

        if (lastMark == nullptr) { head = newMark; }
        else { lastMark->next = newMark; }
        lastMark = newMark;

        theMarkers = theMarkers->next;
    }

    return (head);
}

/***************************************************************/
/* FlushAlphaBetaMemory: Returns all partial matches in a list */
/*   of partial matches either directly to the pool of free    */
/*   memory or to the list of GarbagePartialMatches. Partial   */
/*   matches stored in alpha memories must be placed on the    */
/*   list of GarbagePartialMatches.                            */
/***************************************************************/
void FlushAlphaBetaMemory(
        const Environment::Ptr&theEnv,
        PartialMatch *pfl) {
    PartialMatch *pfltemp;

    while (pfl != nullptr) {
        pfltemp = pfl->nextInMemory;

        UnlinkBetaPartialMatchfromAlphaAndBetaLineage(pfl);
        ReturnPartialMatch(theEnv, pfl);

        pfl = pfltemp;
    }
}

/*****************************************************************/
/* DestroyAlphaBetaMemory: Returns all partial matches in a list */
/*   of partial matches directly to the pool of free memory.     */
/*****************************************************************/
void DestroyAlphaBetaMemory(
        const Environment::Ptr&theEnv,
        PartialMatch *pfl) {
    PartialMatch *pfltemp;

    while (pfl != nullptr) {
        pfltemp = pfl->nextInMemory;
        DestroyPartialMatch(theEnv, pfl);
        pfl = pfltemp;
    }
}

/******************************************************/
/* FindEntityInPartialMatch: Searches for a specified */
/*   data entity in a partial match.                  */
/******************************************************/
bool FindEntityInPartialMatch(
        PatternEntity *theEntity,
        PartialMatch *thePartialMatch) {
    unsigned short i;

    for (i = 0; i < thePartialMatch->bcount; i++) {
        if (thePartialMatch->binds[i].gm.theMatch == nullptr) continue;
        if (thePartialMatch->binds[i].gm.theMatch->matchingItem == theEntity) { return true; }
    }

    return false;
}

/***********************************************************************/
/* GetPatternNumberFromJoin: Given a pointer to a join associated with */
/*   a pattern CE, returns an integer representing the position of the */
/*   pattern CE in the rule (e.g. first, second, third).               */
/***********************************************************************/
int GetPatternNumberFromJoin(
        struct joinNode *joinPtr) {
    int whichOne = 0;

    while (joinPtr != nullptr) {
        if (joinPtr->joinFromTheRight) { joinPtr = (joinNode *) joinPtr->rightSideEntryStructure; }
        else {
            whichOne++;
            joinPtr = joinPtr->lastLevel;
        }
    }

    return (whichOne);
}

/************************************************************************/
/* TraceErrorToRule: Prints an error message when a error occurs as the */
/*   result of evaluating an expression in the pattern network. Used to */
/*   indicate which rule caused the problem.                            */
/************************************************************************/
void TraceErrorToRule(
        const Environment::Ptr&theEnv,
        struct joinNode *joinPtr,
        const char *indentSpaces) {
    int patternCount;

    MarkRuleNetwork(theEnv, 0);

    patternCount = CountPriorPatterns(joinPtr->lastLevel) + 1;

    TraceErrorToRuleDriver(theEnv, joinPtr, indentSpaces, patternCount, false);

    MarkRuleNetwork(theEnv, 0);
}

/**************************************************************/
/* TraceErrorToRuleDriver: Driver code for printing out which */
/*   rule caused a pattern or join network error.             */
/**************************************************************/
static void TraceErrorToRuleDriver(
        const Environment::Ptr&theEnv,
        struct joinNode *joinPtr,
        const char *indentSpaces,
        int priorRightJoinPatterns,
        bool enteredJoinFromRight) {
    const char *name;
    int priorPatternCount;
    struct joinLink *theLinks;

    if ((joinPtr->joinFromTheRight) && enteredJoinFromRight) { priorPatternCount = CountPriorPatterns(joinPtr->lastLevel); }
    else { priorPatternCount = 0; }

    if (joinPtr->marked) { /* Do Nothing */ }
    else if (joinPtr->ruleToActivate != nullptr) {
        joinPtr->marked = 1;
        name = DefruleName(joinPtr->ruleToActivate);
        WriteString(theEnv, STDERR, indentSpaces);

        WriteString(theEnv, STDERR, "Of pattern #");
        WriteInteger(theEnv, STDERR, priorRightJoinPatterns + priorPatternCount);
        WriteString(theEnv, STDERR, " in rule ");
        WriteString(theEnv, STDERR, name);
        WriteString(theEnv, STDERR, "\n");
    } else {
        joinPtr->marked = 1;

        theLinks = joinPtr->nextLinks;
        while (theLinks != nullptr) {
            TraceErrorToRuleDriver(theEnv, theLinks->join, indentSpaces,
                                   priorRightJoinPatterns + priorPatternCount,
                                   (theLinks->enterDirection == CLIPS_RHS));
            theLinks = theLinks->next;
        }
    }
}

/***********************/
/* CountPriorPatterns: */
/***********************/
static int CountPriorPatterns(
        struct joinNode *joinPtr) {
    int count = 0;

    while (joinPtr != nullptr) {
        if (joinPtr->joinFromTheRight) { count += CountPriorPatterns((joinNode *) joinPtr->rightSideEntryStructure); }
        else { count++; }

        joinPtr = joinPtr->lastLevel;
    }

    return (count);
}

/********************************************************/
/* MarkRuleNetwork: Sets the marked flag in each of the */
/*   joins in the join network to the specified value.  */
/********************************************************/
void MarkRuleNetwork(
        const Environment::Ptr&theEnv,
        bool value) {
    Defrule::Ptr rulePtr, *disjunctPtr;
    struct joinNode *joinPtr;
    Defmodule *modulePtr;

    /*===========================*/
    /* Loop through each module. */
    /*===========================*/

    SaveCurrentModule(theEnv);
    for (modulePtr = GetNextDefmodule(theEnv, nullptr);
         modulePtr != nullptr;
         modulePtr = GetNextDefmodule(theEnv, modulePtr)) {
        SetCurrentModule(theEnv, modulePtr);

        /*=========================*/
        /* Loop through each rule. */
        /*=========================*/

        rulePtr = GetNextDefrule(theEnv, nullptr);
        while (rulePtr != nullptr) {
            /*=============================*/
            /* Mark each join for the rule */
            /* with the specified value.   */
            /*=============================*/

            for (disjunctPtr = rulePtr; disjunctPtr != nullptr; disjunctPtr = disjunctPtr->disjunct) {
                joinPtr = disjunctPtr->lastJoin;
                MarkRuleJoins(joinPtr, value);
            }

            /*===========================*/
            /* Move on to the next rule. */
            /*===========================*/

            rulePtr = GetNextDefrule(theEnv, rulePtr);
        }

    }

    RestoreCurrentModule(theEnv);
}

/******************/
/* MarkRuleJoins: */
/******************/
void MarkRuleJoins(
        struct joinNode *joinPtr,
        bool value) {
    while (joinPtr != nullptr) {
        if (joinPtr->joinFromTheRight) { MarkRuleJoins((joinNode *) joinPtr->rightSideEntryStructure, value); }

        joinPtr->marked = value;
        joinPtr = joinPtr->lastLevel;
    }
}

/*****************************************/
/* GetAlphaMemory: Retrieves the list of */
/*   matches from an alpha memory.       */
/*****************************************/
PartialMatch *GetAlphaMemory(
        const Environment::Ptr&theEnv,
        PatternNodeHeader *theHeader,
        unsigned long hashOffset) {
    struct alphaMemoryHash *theAlphaMemory;
    unsigned long hashValue;

    hashValue = AlphaMemoryHashValue(theHeader, hashOffset);
    theAlphaMemory = FindAlphaMemory(theEnv, theHeader, hashValue);

    if (theAlphaMemory == nullptr) { return nullptr; }

    return theAlphaMemory->alphaMemory;
}

/*****************************************/
/* GetLeftBetaMemory: Retrieves the list */
/*   of matches from a beta memory.      */
/*****************************************/
PartialMatch *GetLeftBetaMemory(
        struct joinNode *theJoin,
        unsigned long hashValue) {
    unsigned long betaLocation;

    betaLocation = hashValue % theJoin->leftMemory->size;

    return theJoin->leftMemory->beta[betaLocation];
}

/******************************************/
/* GetRightBetaMemory: Retrieves the list */
/*   of matches from a beta memory.       */
/******************************************/
PartialMatch *GetRightBetaMemory(
        struct joinNode *theJoin,
        unsigned long hashValue) {
    unsigned long betaLocation;

    betaLocation = hashValue % theJoin->rightMemory->size;

    return theJoin->rightMemory->beta[betaLocation];
}

/***************************************/
/* ReturnLeftMemory: Sets the contents */
/*   of a beta memory to nullptr.         */
/***************************************/
void ReturnLeftMemory(
        const Environment::Ptr&theEnv,
        struct joinNode *theJoin) {
    if (theJoin->leftMemory == nullptr) return;
    genfree(theEnv, theJoin->leftMemory->beta, sizeof(PartialMatch *) * theJoin->leftMemory->size);
    rtn_struct(theEnv, betaMemory, theJoin->leftMemory);
    theJoin->leftMemory = nullptr;
}

/***************************************/
/* ReturnRightMemory: Sets the contents */
/*   of a beta memory to nullptr.         */
/***************************************/
void ReturnRightMemory(
        const Environment::Ptr&theEnv,
        struct joinNode *theJoin) {
    if (theJoin->rightMemory == nullptr) return;
    genfree(theEnv, theJoin->rightMemory->beta, sizeof(PartialMatch *) * theJoin->rightMemory->size);
    genfree(theEnv, theJoin->rightMemory->last, sizeof(PartialMatch *) * theJoin->rightMemory->size);
    rtn_struct(theEnv, betaMemory, theJoin->rightMemory);
    theJoin->rightMemory = nullptr;
}

/****************************************************************/
/* DestroyBetaMemory: Destroys the contents of a beta memory in */
/*   preperation for the deallocation of a join. Destroying is  */
/*   performed when the environment is being deallocated and it */
/*   is not necessary to leave the environment in a consistent  */
/*   state (as it would be if just a single rule were being     */
/*   deleted).                                                  */
/****************************************************************/
void DestroyBetaMemory(
        const Environment::Ptr&theEnv,
        struct joinNode *theJoin,
        int side) {
    unsigned long i;

    if (side == CLIPS_LHS) {
        if (theJoin->leftMemory == nullptr) return;

        for (i = 0; i < theJoin->leftMemory->size; i++) { DestroyAlphaBetaMemory(theEnv, theJoin->leftMemory->beta[i]); }
    } else {
        if (theJoin->rightMemory == nullptr) return;

        for (i = 0; i < theJoin->rightMemory->size; i++) { DestroyAlphaBetaMemory(theEnv, theJoin->rightMemory->beta[i]); }
    }
}

/*************************************************************/
/* FlushBetaMemory: Flushes the contents of a beta memory in */
/*   preperation for the deallocation of a join. Flushing    */
/*   is performed when the partial matches in the beta       */
/*   memory may still be in use because the environment will */
/*   remain active.                                          */
/*************************************************************/
void FlushBetaMemory(
        const Environment::Ptr&theEnv,
        struct joinNode *theJoin,
        int side) {
    unsigned long i;

    if (side == CLIPS_LHS) {
        if (theJoin->leftMemory == nullptr) return;

        for (i = 0; i < theJoin->leftMemory->size; i++) { FlushAlphaBetaMemory(theEnv, theJoin->leftMemory->beta[i]); }
    } else {
        if (theJoin->rightMemory == nullptr) return;

        for (i = 0; i < theJoin->rightMemory->size; i++) { FlushAlphaBetaMemory(theEnv, theJoin->rightMemory->beta[i]); }
    }
}

/***********************/
/* BetaMemoryNotEmpty: */
/***********************/
bool BetaMemoryNotEmpty(
        struct joinNode *theJoin) {
    if (theJoin->leftMemory != nullptr) {
        if (theJoin->leftMemory->count > 0) { return true; }
    }

    if (theJoin->rightMemory != nullptr) {
        if (theJoin->rightMemory->count > 0) { return true; }
    }

    return false;
}

/*********************************************/
/* RemoveAlphaMemoryMatches: Removes matches */
/*   from an alpha memory.                   */
/*********************************************/
void RemoveAlphaMemoryMatches(
        const Environment::Ptr&theEnv,
        PatternNodeHeader *theHeader,
        PartialMatch *theMatch,
        struct alphaMatch *theAlphaMatch) {
    struct alphaMemoryHash *theAlphaMemory = nullptr;
    unsigned long hashValue;

    if ((theMatch->prevInMemory == nullptr) || (theMatch->nextInMemory == nullptr)) {
        hashValue = theAlphaMatch->bucket;
        theAlphaMemory = FindAlphaMemory(theEnv, theHeader, hashValue);
    }

    if (theMatch->prevInMemory != nullptr) { theMatch->prevInMemory->nextInMemory = theMatch->nextInMemory; }
    else { theAlphaMemory->alphaMemory = theMatch->nextInMemory; }

    if (theMatch->nextInMemory != nullptr) { theMatch->nextInMemory->prevInMemory = theMatch->prevInMemory; }
    else { theAlphaMemory->endOfQueue = theMatch->prevInMemory; }

    /*====================================*/
    /* Add the match to the garbage list. */
    /*====================================*/

    theMatch->nextInMemory = EngineData(theEnv)->GarbagePartialMatches;
    EngineData(theEnv)->GarbagePartialMatches = theMatch;

    if ((theAlphaMemory != nullptr) && (theAlphaMemory->alphaMemory == nullptr)) { UnlinkAlphaMemory(theEnv, theHeader, theAlphaMemory); }
}

/***********************/
/* DestroyAlphaMemory: */
/***********************/
void DestroyAlphaMemory(
        const Environment::Ptr&theEnv,
        PatternNodeHeader *theHeader,
        bool unlink) {
    struct alphaMemoryHash *theAlphaMemory, *tempMemory;

    theAlphaMemory = theHeader->firstHash;

    while (theAlphaMemory != nullptr) {
        tempMemory = theAlphaMemory->nextHash;
        DestroyAlphaBetaMemory(theEnv, theAlphaMemory->alphaMemory);
        if (unlink) { UnlinkAlphaMemoryBucketSiblings(theEnv, theAlphaMemory); }
        rtn_struct(theEnv, alphaMemoryHash, theAlphaMemory);
        theAlphaMemory = tempMemory;
    }

    theHeader->firstHash = nullptr;
    theHeader->lastHash = nullptr;
}

/*********************/
/* FlushAlphaMemory: */
/*********************/
void FlushAlphaMemory(
        const Environment::Ptr&theEnv,
        PatternNodeHeader *theHeader) {
    struct alphaMemoryHash *theAlphaMemory, *tempMemory;

    theAlphaMemory = theHeader->firstHash;

    while (theAlphaMemory != nullptr) {
        tempMemory = theAlphaMemory->nextHash;
        FlushAlphaBetaMemory(theEnv, theAlphaMemory->alphaMemory);
        UnlinkAlphaMemoryBucketSiblings(theEnv, theAlphaMemory);
        rtn_struct(theEnv, alphaMemoryHash, theAlphaMemory);
        theAlphaMemory = tempMemory;
    }

    theHeader->firstHash = nullptr;
    theHeader->lastHash = nullptr;
}

/********************/
/* FindAlphaMemory: */
/********************/
static struct alphaMemoryHash *FindAlphaMemory(
        const Environment::Ptr&theEnv,
        PatternNodeHeader *theHeader,
        unsigned long hashValue) {
    struct alphaMemoryHash *theAlphaMemory;

    theAlphaMemory = DefruleData(theEnv)->AlphaMemoryTable[hashValue];

    if (theAlphaMemory != nullptr) {
        while ((theAlphaMemory != nullptr) && (theAlphaMemory->owner != theHeader)) { theAlphaMemory = theAlphaMemory->next; }
    }

    return theAlphaMemory;
}

/*************************/
/* AlphaMemoryHashValue: */
/*************************/
static unsigned long AlphaMemoryHashValue(
        PatternNodeHeader *theHeader,
        unsigned long hashOffset) {
    unsigned long hashValue;
    union {
        void *vv;
        unsigned uv;
    } fis;

    fis.uv = 0;
    fis.vv = theHeader;

    hashValue = fis.uv + hashOffset;
    hashValue = hashValue % ALPHA_MEMORY_HASH_SIZE;

    return hashValue;
}

/**********************/
/* UnlinkAlphaMemory: */
/**********************/
static void UnlinkAlphaMemory(
        const Environment::Ptr&theEnv,
        PatternNodeHeader *theHeader,
        struct alphaMemoryHash *theAlphaMemory) {
    /*======================*/
    /* Unlink the siblings. */
    /*======================*/

    UnlinkAlphaMemoryBucketSiblings(theEnv, theAlphaMemory);

    /*================================*/
    /* Update firstHash and lastHash. */
    /*================================*/

    if (theAlphaMemory == theHeader->firstHash) { theHeader->firstHash = theAlphaMemory->nextHash; }

    if (theAlphaMemory == theHeader->lastHash) { theHeader->lastHash = theAlphaMemory->prevHash; }

    /*===============================*/
    /* Update nextHash and prevHash. */
    /*===============================*/

    if (theAlphaMemory->prevHash != nullptr) { theAlphaMemory->prevHash->nextHash = theAlphaMemory->nextHash; }

    if (theAlphaMemory->nextHash != nullptr) { theAlphaMemory->nextHash->prevHash = theAlphaMemory->prevHash; }

    rtn_struct(theEnv, alphaMemoryHash, theAlphaMemory);
}

/************************************/
/* UnlinkAlphaMemoryBucketSiblings: */
/************************************/
static void UnlinkAlphaMemoryBucketSiblings(
        const Environment::Ptr&theEnv,
        struct alphaMemoryHash *theAlphaMemory) {
    if (theAlphaMemory->prev == nullptr) { DefruleData(theEnv)->AlphaMemoryTable[theAlphaMemory->bucket] = theAlphaMemory->next; }
    else { theAlphaMemory->prev->next = theAlphaMemory->next; }

    if (theAlphaMemory->next != nullptr) { theAlphaMemory->next->prev = theAlphaMemory->prev; }
}

/**************************/
/* ComputeRightHashValue: */
/**************************/
unsigned long ComputeRightHashValue(
        const Environment::Ptr&theEnv,
        PatternNodeHeader *theHeader) {
    Expression *tempExpr;
    unsigned long hashValue = 0;
    unsigned long multiplier = 1;
    union {
        void *vv;
        unsigned long liv;
    } fis;

    if (theHeader->rightHash == nullptr) { return hashValue; }

    for (tempExpr = theHeader->rightHash;
         tempExpr != nullptr;
         tempExpr = tempExpr->nextArg, multiplier = multiplier * 509) {
        UDFValue theResult;
        Expression *oldArgument;

        oldArgument = EvaluationData(theEnv)->CurrentExpression;
        EvaluationData(theEnv)->CurrentExpression = tempExpr;
        (*EvaluationData(theEnv)->PrimitivesArray[tempExpr->type]->evaluateFunction)(theEnv, tempExpr->value, &theResult);
        EvaluationData(theEnv)->CurrentExpression = oldArgument;

        switch (theResult.header->type) {
            case STRING_TYPE:
            case SYMBOL_TYPE:
            case INSTANCE_NAME_TYPE:
                hashValue += (theResult.lexemeValue->bucket * multiplier);
                break;

            case INTEGER_TYPE:
                hashValue += (theResult.integerValue->bucket * multiplier);
                break;

            case FLOAT_TYPE:
                hashValue += (theResult.floatValue->bucket * multiplier);
                break;

            case FACT_ADDRESS_TYPE:
            case INSTANCE_ADDRESS_TYPE:
                fis.liv = 0;
                fis.vv = theResult.value;
                hashValue += fis.liv * multiplier;
                break;

            case EXTERNAL_ADDRESS_TYPE:
                fis.liv = 0;
                fis.vv = theResult.externalAddressValue->contents;
                hashValue += fis.liv * multiplier;
                break;
        }
    }

    return hashValue;
}

/*********************/
/* ResizeBetaMemory: */
/*********************/
void ResizeBetaMemory(
        const Environment::Ptr&theEnv,
        struct betaMemory *theMemory) {
    PartialMatch **oldArray, **lastAdd, *thePM, *nextPM;
    unsigned long i, oldSize, betaLocation;

    oldSize = theMemory->size;
    oldArray = theMemory->beta;

    theMemory->size = oldSize * 11;
    theMemory->beta = (PartialMatch **) genalloc(theEnv, sizeof(PartialMatch *) * theMemory->size);

    lastAdd = (PartialMatch **) genalloc(theEnv, sizeof(PartialMatch *) * theMemory->size);
    memset(theMemory->beta, 0, sizeof(PartialMatch *) * theMemory->size);
    memset(lastAdd, 0, sizeof(PartialMatch *) * theMemory->size);

    for (i = 0; i < oldSize; i++) {
        thePM = oldArray[i];
        while (thePM != nullptr) {
            nextPM = thePM->nextInMemory;

            thePM->nextInMemory = nullptr;

            betaLocation = thePM->hashValue % theMemory->size;
            thePM->prevInMemory = lastAdd[betaLocation];

            if (lastAdd[betaLocation] != nullptr) { lastAdd[betaLocation]->nextInMemory = thePM; }
            else { theMemory->beta[betaLocation] = thePM; }

            lastAdd[betaLocation] = thePM;

            thePM = nextPM;
        }
    }

    if (theMemory->last != nullptr) {
        genfree(theEnv, theMemory->last, sizeof(PartialMatch *) * oldSize);
        theMemory->last = lastAdd;
    } else { genfree(theEnv, lastAdd, sizeof(PartialMatch *) * theMemory->size); }

    genfree(theEnv, oldArray, sizeof(PartialMatch *) * oldSize);
}

/********************/
/* ResetBetaMemory: */
/********************/
static void ResetBetaMemory(
        const Environment::Ptr&theEnv,
        struct betaMemory *theMemory) {
    PartialMatch **oldArray, **lastAdd;
    unsigned long oldSize;

    if ((theMemory->size == 1) ||
        (theMemory->size == INITIAL_BETA_HASH_SIZE)) { return; }

    oldSize = theMemory->size;
    oldArray = theMemory->beta;

    theMemory->size = INITIAL_BETA_HASH_SIZE;
    theMemory->beta = (PartialMatch **) genalloc(theEnv, sizeof(PartialMatch *) * theMemory->size);
    memset(theMemory->beta, 0, sizeof(PartialMatch *) * theMemory->size);
    genfree(theEnv, oldArray, sizeof(PartialMatch *) * oldSize);

    if (theMemory->last != nullptr) {
        lastAdd = (PartialMatch **) genalloc(theEnv, sizeof(PartialMatch *) * theMemory->size);
        memset(lastAdd, 0, sizeof(PartialMatch *) * theMemory->size);
        genfree(theEnv, theMemory->last, sizeof(PartialMatch *) * oldSize);
        theMemory->last = lastAdd;
    }
}

/********************/
/* PrintBetaMemory: */
/********************/
unsigned long PrintBetaMemory(
        const Environment::Ptr&theEnv,
        const char *logName,
        struct betaMemory *theMemory,
        bool indentFirst,
        const char *indentString,
        Verbosity output) {
    PartialMatch *listOfMatches;
    unsigned long b, count = 0;

    if (GetHaltExecution(theEnv)) { return count; }

    for (b = 0; b < theMemory->size; b++) {
        listOfMatches = theMemory->beta[b];

        while (listOfMatches != nullptr) {
            /*=========================================*/
            /* Check to see if the user is attempting  */
            /* to stop the display of partial matches. */
            /*=========================================*/

            if (GetHaltExecution(theEnv)) { return count; }

            /*=========================================================*/
            /* The first partial match may have already been indented. */
            /* Subsequent partial matches will always be indented with */
            /* the indentation string.                                 */
            /*=========================================================*/

            if (output == VERBOSE) {
                if (indentFirst) { WriteString(theEnv, logName, indentString); }
                else { indentFirst = true; }
            }

            /*==========================*/
            /* Print the partial match. */
            /*==========================*/

            if (output == VERBOSE) {
                PrintPartialMatch(theEnv, logName, listOfMatches);
                WriteString(theEnv, logName, "\n");
            }

            count++;

            /*============================*/
            /* Move on to the next match. */
            /*============================*/

            listOfMatches = listOfMatches->nextInMemory;
        }
    }

    return count;
}

#if (BLOAD_AND_BSAVE)

/*************************************************************/
/* TagRuleNetwork: Assigns each join in the join network and */
/*   each defrule data structure with a unique integer ID.   */
/*   Also counts the number of defrule and joinNode data     */
/*   structures currently in use.                            */
/*************************************************************/
void TagRuleNetwork(
        const Environment::Ptr&theEnv,
        unsigned long *moduleCount,
        unsigned long *ruleCount,
        unsigned long *joinCount,
        unsigned long *linkCount) {
    Defmodule *modulePtr;
    Defrule::Ptr rulePtr, *disjunctPtr;
    struct joinLink *theLink;

    *moduleCount = 0;
    *ruleCount = 0;
    *joinCount = 0;
    *linkCount = 0;

    MarkRuleNetwork(theEnv, 0);

    for (theLink = DefruleData(theEnv)->LeftPrimeJoins;
         theLink != nullptr;
         theLink = theLink->next) {
        theLink->bsaveID = *linkCount;
        (*linkCount)++;
    }

    for (theLink = DefruleData(theEnv)->RightPrimeJoins;
         theLink != nullptr;
         theLink = theLink->next) {
        theLink->bsaveID = *linkCount;
        (*linkCount)++;
    }

    /*===========================*/
    /* Loop through each module. */
    /*===========================*/

    for (modulePtr = GetNextDefmodule(theEnv, nullptr);
         modulePtr != nullptr;
         modulePtr = GetNextDefmodule(theEnv, modulePtr)) {
        (*moduleCount)++;
        SetCurrentModule(theEnv, modulePtr);

        /*=========================*/
        /* Loop through each rule. */
        /*=========================*/

        rulePtr = GetNextDefrule(theEnv, nullptr);

        while (rulePtr != nullptr) {
            /*=============================*/
            /* Loop through each disjunct. */
            /*=============================*/

            for (disjunctPtr = rulePtr; disjunctPtr != nullptr; disjunctPtr = disjunctPtr->disjunct) {
                disjunctPtr->header.bsaveID = *ruleCount;
                (*ruleCount)++;
                TagNetworkTraverseJoins(theEnv, joinCount, linkCount, disjunctPtr->lastJoin);
            }

            rulePtr = GetNextDefrule(theEnv, rulePtr);
        }
    }
}

/*******************************************************************/
/* TagNetworkTraverseJoins: Traverses the join network for a rule. */
/*******************************************************************/
static void TagNetworkTraverseJoins(
        const Environment::Ptr&theEnv,
        unsigned long *joinCount,
        unsigned long *linkCount,
        struct joinNode *joinPtr) {
    struct joinLink *theLink;
    for (;
            joinPtr != nullptr;
            joinPtr = joinPtr->lastLevel) {
        if (joinPtr->marked == 0) {
            joinPtr->marked = 1;
            joinPtr->bsaveID = *joinCount;
            (*joinCount)++;
            for (theLink = joinPtr->nextLinks;
                 theLink != nullptr;
                 theLink = theLink->next) {
                theLink->bsaveID = *linkCount;
                (*linkCount)++;
            }
        }

        if (joinPtr->joinFromTheRight) {
            TagNetworkTraverseJoins(theEnv, joinCount, linkCount, (joinNode *) joinPtr->rightSideEntryStructure);
        }
    }
}

#endif /* (BLOAD_AND_BSAVE) */






