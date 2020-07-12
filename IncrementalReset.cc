/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  08/11/16             */
/*                                                     */
/*              INCREMENTAL RESET MODULE               */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides functionality for the incremental       */
/*   reset of the pattern and join networks when a new       */
/*   rule is added.                                          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Removed INCREMENTAL_RESET compilation flag.    */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Added support for hashed alpha memories and    */
/*            other join network changes.                    */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Modified EnvSetIncrementalReset to check for   */
/*            the existance of rules.                        */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.31: Fix for slow incremental reset of rule with    */
/*            several dozen nand joins.                      */
/*                                                           */
/*      6.40: Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            Incremental reset is always enabled.           */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#include <cstdio>


#include "Agenda.h"
#include "ArgumentAccess.h"
#include "Constants.h"
#include "Drive.h"
#include "Engine.h"
#include "Environment.h"
#include "Evaluation.h"
#include "Pattern.h"
#include "Router.h"
#include "ReteUtility.h"

#include "IncrementalReset.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void MarkNetworkForIncrementalReset(const Environment&, Defrule::Ptr , bool);
static void MarkJoinsForIncrementalReset(const Environment&, struct joinNode *, bool);
static void CheckForPrimableJoins(const Environment&, Defrule::Ptr , struct joinNode *);
static void PrimeJoinFromLeftMemory(const Environment&, struct joinNode *);
static void PrimeJoinFromRightMemory(const Environment&, struct joinNode *);
static void MarkPatternForIncrementalReset(const Environment&, unsigned short,
                                           PatternNodeHeader *, bool);

/**************************************************************/
/* IncrementalReset: Incrementally resets the specified rule. */
/**************************************************************/
void IncrementalReset(
        const Environment&theEnv,
        Defrule::Ptr tempRule) {
    Defrule::Ptr tempPtr;
    struct patternParser *theParser;

    /*=====================================================*/
    /* Mark the pattern and join network data structures   */
    /* associated with the rule being incrementally reset. */
    /*=====================================================*/

    MarkNetworkForIncrementalReset(theEnv, tempRule, true);

    /*==========================*/
    /* Begin incremental reset. */
    /*==========================*/

    EngineData(theEnv)->IncrementalResetInProgress = true;

    /*============================================================*/
    /* If the new rule shares patterns or joins with other rules, */
    /* then it is necessary to update its join network based on   */
    /* existing partial matches it shares with other rules.       */
    /*============================================================*/

    for (tempPtr = tempRule;
         tempPtr != nullptr;
         tempPtr = tempPtr->disjunct) { CheckForPrimableJoins(theEnv, tempPtr, tempPtr->lastJoin); }

    /*===============================================*/
    /* Filter existing data entities through the new */
    /* portions of the pattern and join networks.    */
    /*===============================================*/

    for (theParser = PatternData(theEnv)->ListOfPatternParsers;
         theParser != nullptr;
         theParser = theParser->next) {
        if (theParser->incrementalResetFunction != nullptr) { (*theParser->incrementalResetFunction)(theEnv); }
    }

    /*========================*/
    /* End incremental reset. */
    /*========================*/

    EngineData(theEnv)->IncrementalResetInProgress = false;

    /*====================================================*/
    /* Remove the marks in the pattern and join networks. */
    /*====================================================*/

    MarkNetworkForIncrementalReset(theEnv, tempRule, false);
}


/**********************************************************************/
/* MarkNetworkForIncrementalReset: Coordinates marking the initialize */
/*   flags in the pattern and join networks both before and after an  */
/*   incremental reset.                                               */
/**********************************************************************/
static void MarkNetworkForIncrementalReset(
        const Environment&theEnv,
        Defrule::Ptr tempRule,
        bool value) {
    /*============================================*/
    /* Loop through each of the rule's disjuncts. */
    /*============================================*/

    for (;
            tempRule != nullptr;
            tempRule = tempRule->disjunct) { MarkJoinsForIncrementalReset(theEnv, tempRule->lastJoin, value); }
}

/**********************************************************************/
/* MarkJoinsForIncrementalReset: Coordinates marking the initialize */
/*   flags in the pattern and join networks both before and after an  */
/*   incremental reset.                                               */
/**********************************************************************/
static void MarkJoinsForIncrementalReset(
        const Environment&theEnv,
        struct joinNode *joinPtr,
        bool value) {
    PatternNodeHeader *patternPtr;

    for (;
            joinPtr != nullptr;
            joinPtr = GetPreviousJoin(joinPtr)) {
        if (joinPtr->ruleToActivate != nullptr) {
            joinPtr->marked = false;
            joinPtr->initialize = value;
            continue;
        }

        //if (joinPtr->joinFromTheRight)
        //  { MarkJoinsForIncrementalReset(theEnv,(struct joinNode *) joinPtr->rightSideEntryStructure,value); }

        /*================*/
        /* Mark the join. */
        /*================*/

        joinPtr->marked = false; /* GDR 6.05 */

        if (joinPtr->initialize) {
            joinPtr->initialize = value;
            if (joinPtr->joinFromTheRight == false) {
                patternPtr = (PatternNodeHeader *) GetPatternForJoin(joinPtr);
                if (patternPtr != nullptr) { MarkPatternForIncrementalReset(theEnv, joinPtr->rhsType, patternPtr, value); }
            }
        }
    }
}

/*******************************************************************************/
/* CheckForPrimableJoins: Updates the joins of a rule for an incremental reset */
/*   if portions of that rule are shared with other rules that have already    */
/*   been incrementally reset. A join for a new rule will be updated if it is  */
/*   marked for initialization and either its parent join or its associated    */
/*   entry pattern node has not been marked for initialization. The function   */
/*   PrimeJoin is used to update joins which meet these criteria.              */
/*******************************************************************************/
static void CheckForPrimableJoins(
        const Environment&theEnv,
        Defrule::Ptr tempRule,
        struct joinNode *joinPtr) {
    /*========================================*/
    /* Loop through each of the rule's joins. */
    /*========================================*/

    for (;
            joinPtr != nullptr;
            joinPtr = GetPreviousJoin(joinPtr)) {
        /*===============================*/
        /* Update the join if necessary. */
        /*===============================*/

        if ((joinPtr->initialize) && (!joinPtr->marked)) {
            if (joinPtr->firstJoin == true) {
                if (joinPtr->joinFromTheRight == false) {
                    if ((joinPtr->rightSideEntryStructure == nullptr) ||
                        (joinPtr->patternIsNegated) ||
                        (((PatternNodeHeader *) joinPtr->rightSideEntryStructure)->initialize == false)) {
                        PrimeJoinFromLeftMemory(theEnv, joinPtr);
                        joinPtr->marked = true;
                    }
                } else {
                    PrimeJoinFromRightMemory(theEnv, joinPtr);
                    joinPtr->marked = true;
                }
            } else if (joinPtr->lastLevel->initialize == false) {
                PrimeJoinFromLeftMemory(theEnv, joinPtr);
                joinPtr->marked = true;
            } else if ((joinPtr->joinFromTheRight) &&
                       (((joinNode *) joinPtr->rightSideEntryStructure)->initialize == false)) {
                PrimeJoinFromRightMemory(theEnv, joinPtr);
                joinPtr->marked = true;
            }
        }

        //if (joinPtr->joinFromTheRight)
        //  { CheckForPrimableJoins(theEnv,tempRule,(struct joinNode *) joinPtr->rightSideEntryStructure); }
    }
}

/****************************************************************************/
/* PrimeJoinFromLeftMemory: Updates a join in a rule for an incremental     */
/*   reset. Joins are updated by "priming" them only if the join (or its    */
/*   associated pattern) is shared with other rules that have already been  */
/*   incrementally reset. A join for a new rule will be updated if it is    */
/*   marked for initialization and either its parent join or its associated */
/*   entry pattern node has not been marked for initialization.             */
/****************************************************************************/
static void PrimeJoinFromLeftMemory(
        const Environment&theEnv,
        struct joinNode *joinPtr) {
    PartialMatch *theList, *linker;
    struct alphaMemoryHash *listOfHashNodes;
    unsigned long b;
    unsigned long hashValue;
    struct betaMemory *theMemory;
    PartialMatch *notParent;
    struct joinLink *tempLink;

    /*===========================================================*/
    /* If the join is the first join of a rule, then send all of */
    /* the partial matches from the alpha memory of the pattern  */
    /* associated with this join to the join for processing and  */
    /* the priming process is then complete.                     */
    /*===========================================================*/

    if (joinPtr->firstJoin == true) {
        if (joinPtr->rightSideEntryStructure == nullptr) { NetworkAssert(theEnv, joinPtr->rightMemory->beta[0], joinPtr); }
        else if (joinPtr->patternIsNegated) {
            notParent = joinPtr->leftMemory->beta[0];

            if (joinPtr->secondaryNetworkTest != nullptr) {
                if (!EvaluateSecondaryNetworkTest(theEnv, notParent, joinPtr)) { return; }
            }

            for (listOfHashNodes = ((PatternNodeHeader *) joinPtr->rightSideEntryStructure)->firstHash;
                 listOfHashNodes != nullptr;
                 listOfHashNodes = listOfHashNodes->nextHash) {
                if (listOfHashNodes->alphaMemory != nullptr) {
                    AddBlockedLink(notParent, listOfHashNodes->alphaMemory);
                    return;
                }
            }

            EPMDrive(theEnv, notParent, joinPtr, NETWORK_ASSERT);
        } else {
            for (listOfHashNodes = ((PatternNodeHeader *) joinPtr->rightSideEntryStructure)->firstHash;
                 listOfHashNodes != nullptr;
                 listOfHashNodes = listOfHashNodes->nextHash) {
                for (theList = listOfHashNodes->alphaMemory;
                     theList != nullptr;
                     theList = theList->nextInMemory) { NetworkAssert(theEnv, theList, joinPtr); }
            }
        }
        return;
    }

    /*========================================*/
    /* Find another beta memory from which we */
    /* can retrieve the partial matches.      */
    /*========================================*/

    tempLink = joinPtr->lastLevel->nextLinks;

    while (tempLink != nullptr) {
        if ((tempLink->join != joinPtr) &&
            (tempLink->join->initialize == false)) { break; }

        tempLink = tempLink->next;
    }

    if (tempLink == nullptr) return;

    if (tempLink->enterDirection == CLIPS_LHS) { theMemory = tempLink->join->leftMemory; }
    else { theMemory = tempLink->join->rightMemory; }

    /*============================================*/
    /* Send all partial matches from the selected */
    /* beta memory to the new join.               */
    /*============================================*/

    for (b = 0; b < theMemory->size; b++) {
        for (theList = theMemory->beta[b];
             theList != nullptr;
             theList = theList->nextInMemory) {
            linker = CopyPartialMatch(theEnv, theList);

            if (joinPtr->leftHash != nullptr) { hashValue = BetaMemoryHashValue(theEnv, joinPtr->leftHash, linker, nullptr, joinPtr); }
            else { hashValue = 0; }

            UpdateBetaPMLinks(theEnv, linker, theList->leftParent, theList->rightParent, joinPtr, hashValue, CLIPS_LHS);

            NetworkAssertLeft(theEnv, linker, joinPtr, NETWORK_ASSERT);
        }
    }
}

/****************************************************************************/
/* PrimeJoinFromRightMemory: Updates a join in a rule for an incremental    */
/*   reset. Joins are updated by "priming" them only if the join (or its    */
/*   associated pattern) is shared with other rules that have already been  */
/*   incrementally reset. A join for a new rule will be updated if it is    */
/*   marked for initialization and either its parent join or its associated */
/*   entry pattern node has not been marked for initialization.             */
/****************************************************************************/
static void PrimeJoinFromRightMemory(
        const Environment&theEnv,
        struct joinNode *joinPtr) {
    PartialMatch *theList, *linker;
    unsigned long b;
    struct betaMemory *theMemory;
    unsigned long hashValue;
    struct joinLink *tempLink;
    PartialMatch *notParent;

    /*=======================================*/
    /* This should be a join from the right. */
    /*=======================================*/

    if (joinPtr->joinFromTheRight == false) { return; }

    /*========================================*/
    /* Find another beta memory from which we */
    /* can retrieve the partial matches.      */
    /*========================================*/

    tempLink = ((joinNode *) joinPtr->rightSideEntryStructure)->nextLinks;
    while (tempLink != nullptr) {
        if ((tempLink->join != joinPtr) &&
            (tempLink->join->initialize == false)) { break; }

        tempLink = tempLink->next;
    }

    if (tempLink == nullptr) {
        if (joinPtr->firstJoin &&
            (joinPtr->rightMemory->beta[0] == nullptr) &&
            (!joinPtr->patternIsExists)) {
            notParent = joinPtr->leftMemory->beta[0];

            if (joinPtr->secondaryNetworkTest != nullptr) {
                if (!EvaluateSecondaryNetworkTest(theEnv, notParent, joinPtr)) { return; }
            }

            EPMDrive(theEnv, notParent, joinPtr, NETWORK_ASSERT);
        }

        return;
    }

    if (tempLink->enterDirection == CLIPS_LHS) { theMemory = tempLink->join->leftMemory; }
    else { theMemory = tempLink->join->rightMemory; }

    /*============================================*/
    /* Send all partial matches from the selected */
    /* beta memory to the new join.               */
    /*============================================*/

    for (b = 0; b < theMemory->size; b++) {
        for (theList = theMemory->beta[b];
             theList != nullptr;
             theList = theList->nextInMemory) {
            linker = CopyPartialMatch(theEnv, theList);

            if (joinPtr->rightHash != nullptr) { hashValue = BetaMemoryHashValue(theEnv, joinPtr->rightHash, linker, nullptr, joinPtr); }
            else { hashValue = 0; }

            UpdateBetaPMLinks(theEnv, linker, theList->leftParent, theList->rightParent, joinPtr, hashValue, CLIPS_RHS);
            NetworkAssert(theEnv, linker, joinPtr);
        }
    }

    if (joinPtr->firstJoin &&
        (joinPtr->rightMemory->beta[0] == nullptr) &&
        (!joinPtr->patternIsExists)) {
        notParent = joinPtr->leftMemory->beta[0];

        if (joinPtr->secondaryNetworkTest != nullptr) {
            if (!EvaluateSecondaryNetworkTest(theEnv, notParent, joinPtr)) { return; }
        }

        EPMDrive(theEnv, notParent, joinPtr, NETWORK_ASSERT);
    }
}

/*********************************************************************/
/* MarkPatternForIncrementalReset: Given a pattern node and its type */
/*   (fact, instance, etc.), calls the appropriate function to mark  */
/*   the pattern for an incremental reset. Used to mark the pattern  */
/*   nodes both before and after an incremental reset.               */
/*********************************************************************/
static void MarkPatternForIncrementalReset(
        const Environment&theEnv,
        unsigned short rhsType,
        PatternNodeHeader *theHeader,
        bool value) {
    struct patternParser *tempParser;

    tempParser = GetPatternParser(theEnv, rhsType);

    if (tempParser != nullptr) {
        if (tempParser->markIRPatternFunction != nullptr) { (*tempParser->markIRPatternFunction)(theEnv, theHeader, value); }
    }
}

