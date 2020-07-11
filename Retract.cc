/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  08/28/17             */
/*                                                     */
/*                   RETRACT MODULE                    */
/*******************************************************/

/*************************************************************/
/* Purpose:  Handles join network activity associated with   */
/*   with the removal of a data entity such as a fact or     */
/*   instance.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed LOGICAL_DEPENDENCIES compilation flag. */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Rule with exists CE has incorrect activation.  */
/*            DR0867                                         */
/*                                                           */
/*      6.30: Added support for hashed memories.             */
/*                                                           */
/*            Added additional developer statistics to help  */
/*            analyze join network performance.              */
/*                                                           */
/*            Removed pseudo-facts used in not CEs.          */
/*                                                           */
/*      6.31: Bug fix to prevent rule activations for        */
/*            partial matches being deleted.                 */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#include <cstdio>
#include <cstdlib>

#include "Setup.h"


#include "Agenda.h"
#include "ArgumentAccess.h"
#include "Constants.h"
#include "Drive.h"
#include "Engine.h"
#include "Environment.h"
#include "LogicalDependencies.h"
#include "Match.h"
#include "MemoryAllocation.h"
#include "Network.h"
#include "PrintUtility.h"
#include "ReteUtility.h"
#include "Router.h"
#include "Symbol.h"

#include "Retract.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void ReturnMarkers(const Environment&, struct multifieldMarker *);
static bool FindNextConflictingMatch(const Environment&, PartialMatch *,
                                     PartialMatch *,
                                     struct joinNode *, PartialMatch *, int);
static bool PartialMatchDefunct(const Environment&, PartialMatch *);
static void NegEntryRetractAlpha(const Environment&, PartialMatch *, int);
static void NegEntryRetractBeta(const Environment&, struct joinNode *, PartialMatch *,
                                PartialMatch *, int);

/************************************************************/
/* NetworkRetract:  Retracts a data entity (such as a fact  */
/*   or instance) from the pattern and join networks given  */
/*   a pointer to the list of patterns which the data       */
/*   entity matched.                                        */
/************************************************************/
void NetworkRetract(
        const Environment&theEnv,
        struct patternMatch *listOfMatchedPatterns) {
    struct patternMatch *tempMatch, *nextMatch;

    tempMatch = listOfMatchedPatterns;
    while (tempMatch != nullptr) {
        nextMatch = tempMatch->next;

        tempMatch->theMatch->deleting = true;

        if (tempMatch->theMatch->children != nullptr) { PosEntryRetractAlpha(theEnv, tempMatch->theMatch, NETWORK_RETRACT); }

        if (tempMatch->theMatch->blockList != nullptr) { NegEntryRetractAlpha(theEnv, tempMatch->theMatch, NETWORK_RETRACT); }

        /*===================================================*/
        /* Remove from the alpha memory of the pattern node. */
        /*===================================================*/

        RemoveAlphaMemoryMatches(theEnv, tempMatch->matchingPattern,
                                 tempMatch->theMatch,
                                 tempMatch->theMatch->binds[0].gm.theMatch);

        rtn_struct(theEnv, patternMatch, tempMatch);

        tempMatch = nextMatch;
    }
}

/*************************/
/* PosEntryRetractAlpha: */
/*************************/
void PosEntryRetractAlpha(
        const Environment&theEnv,
        PartialMatch *alphaMatch,
        int operation) {
    PartialMatch *betaMatch, *tempMatch;
    struct joinNode *joinPtr;

    betaMatch = alphaMatch->children;
    while (betaMatch != nullptr) {
        joinPtr = (joinNode *) betaMatch->owner;

        if (betaMatch->children != nullptr) { PosEntryRetractBeta(theEnv, betaMatch, betaMatch->children, operation); }

        if (betaMatch->rhsMemory) { NegEntryRetractAlpha(theEnv, betaMatch, operation); }

        /* Remove the beta match. */

        if ((joinPtr->ruleToActivate != nullptr) ?
            (betaMatch->marker != nullptr) : false) { RemoveActivation(theEnv, (activation *) betaMatch->marker, true, true); }

        tempMatch = betaMatch->nextRightChild;

        if (betaMatch->rhsMemory) { UnlinkBetaPMFromNodeAndLineage(theEnv, joinPtr, betaMatch, CLIPS_RHS); }
        else { UnlinkBetaPMFromNodeAndLineage(theEnv, joinPtr, betaMatch, CLIPS_LHS); }

        DeletePartialMatches(theEnv, betaMatch);

        betaMatch = tempMatch;
    }
}

/*************************/
/* NegEntryRetractAlpha: */
/*************************/
static void NegEntryRetractAlpha(
        const Environment&theEnv,
        PartialMatch *alphaMatch,
        int operation) {
    PartialMatch *betaMatch;
    struct joinNode *joinPtr;

    betaMatch = alphaMatch->blockList;
    while (betaMatch != nullptr) {
        joinPtr = (joinNode *) betaMatch->owner;

        if ((!joinPtr->patternIsNegated) &&
            (!joinPtr->patternIsExists) &&
            (!joinPtr->joinFromTheRight)) {
            SystemError(theEnv, "RETRACT", 117);
            betaMatch = betaMatch->nextBlocked;
            continue;
        }

        NegEntryRetractBeta(theEnv, joinPtr, alphaMatch, betaMatch, operation);
        betaMatch = alphaMatch->blockList;
    }
}

/************************/
/* NegEntryRetractBeta: */
/************************/
static void NegEntryRetractBeta(
        const Environment&theEnv,
        struct joinNode *joinPtr,
        PartialMatch *alphaMatch,
        PartialMatch *betaMatch,
        int operation) {
    /*======================================================*/
    /* Try to find another RHS partial match which prevents */
    /* the LHS partial match from being satisifed.          */
    /*======================================================*/

    RemoveBlockedLink(betaMatch);

    if (FindNextConflictingMatch(theEnv, betaMatch, alphaMatch->nextInMemory, joinPtr, alphaMatch, operation)) { return; }
    else if (joinPtr->patternIsExists) {
        if (betaMatch->children != nullptr) { PosEntryRetractBeta(theEnv, betaMatch, betaMatch->children, operation); }
        return;
    } else if (joinPtr->firstJoin && (joinPtr->patternIsNegated || joinPtr->joinFromTheRight) && (!joinPtr->patternIsExists)) {
        if (joinPtr->secondaryNetworkTest != nullptr) {
            if (!EvaluateSecondaryNetworkTest(theEnv, betaMatch, joinPtr)) { return; }
        }

        EPMDrive(theEnv, betaMatch, joinPtr, operation);

        return;
    }

    if (joinPtr->secondaryNetworkTest != nullptr) {
        if (!EvaluateSecondaryNetworkTest(theEnv, betaMatch, joinPtr)) { return; }
    }

    /*=========================================================*/
    /* If the LHS partial match now has no RHS partial matches */
    /* that conflict with it, then it satisfies the conditions */
    /* of the RHS not CE. Create a partial match and send it   */
    /* to the joins below.                                     */
    /*=========================================================*/

    /*===============================*/
    /* Create the new partial match. */
    /*===============================*/

    if ((operation == NETWORK_RETRACT) && PartialMatchWillBeDeleted(theEnv, betaMatch)) { return; }

    PPDrive(theEnv, betaMatch, nullptr, joinPtr, operation);
}

/************************/
/* PosEntryRetractBeta: */
/************************/
void PosEntryRetractBeta(
        const Environment&theEnv,
        PartialMatch *parentMatch,
        PartialMatch *betaMatch,
        int operation) {
    PartialMatch *tempMatch;

    while (betaMatch != nullptr) {
        if (betaMatch->children != nullptr) {
            betaMatch = betaMatch->children;
            continue;
        }

        if (betaMatch->nextLeftChild != nullptr) { tempMatch = betaMatch->nextLeftChild; }
        else {
            tempMatch = betaMatch->leftParent;
            betaMatch->leftParent->children = nullptr;
        }

        if (betaMatch->blockList != nullptr) { NegEntryRetractAlpha(theEnv, betaMatch, operation); }
        else if ((((joinNode *) betaMatch->owner)->ruleToActivate != nullptr) ?
                 (betaMatch->marker != nullptr) : false) { RemoveActivation(theEnv, (activation *) betaMatch->marker, true, true); }

        if (betaMatch->rhsMemory) { UnlinkNonLeftLineage(theEnv, (joinNode *) betaMatch->owner, betaMatch, CLIPS_RHS); }
        else { UnlinkNonLeftLineage(theEnv, (joinNode *) betaMatch->owner, betaMatch, CLIPS_LHS); }

        if (betaMatch->dependents != nullptr) RemoveLogicalSupport(theEnv, betaMatch);
        ReturnPartialMatch(theEnv, betaMatch);

        if (tempMatch == parentMatch) return;
        betaMatch = tempMatch;
    }
}

/******************************************************************/
/* FindNextConflictingMatch: Finds the next conflicting partial   */
/*    match in the right memory of a join that prevents a partial */
/*    match in the beta memory of the join from being satisfied.  */
/******************************************************************/
static bool FindNextConflictingMatch(
        const Environment&theEnv,
        PartialMatch *theBind,
        PartialMatch *possibleConflicts,
        struct joinNode *theJoin,
        PartialMatch *skipMatch,
        int operation) {
    bool result, restore = false;
    PartialMatch *oldLHSBinds = nullptr;
    PartialMatch *oldRHSBinds = nullptr;
    struct joinNode *oldJoin = nullptr;

    /*====================================*/
    /* Check each of the possible partial */
    /* matches which could conflict.      */
    /*====================================*/

#if DEVELOPER
    if (possibleConflicts != nullptr)
      { EngineData(theEnv)->leftToRightLoops++; }
#endif
    /*====================================*/
    /* Set up the evaluation environment. */
    /*====================================*/

    if (possibleConflicts != nullptr) {
        oldLHSBinds = EngineData(theEnv)->GlobalLHSBinds;
        oldRHSBinds = EngineData(theEnv)->GlobalRHSBinds;
        oldJoin = EngineData(theEnv)->GlobalJoin;
        EngineData(theEnv)->GlobalLHSBinds = theBind;
        EngineData(theEnv)->GlobalJoin = theJoin;
        restore = true;
    }

    for (;
            possibleConflicts != nullptr;
            possibleConflicts = possibleConflicts->nextInMemory) {
        theJoin->memoryCompares++;

        /*=====================================*/
        /* Initially indicate that the partial */
        /* match doesn't conflict.             */
        /*=====================================*/

        result = false;

        if (skipMatch == possibleConflicts) { /* Do Nothing */ }

            /*======================================================*/
            /* 6.05 Bug Fix. It is possible that a pattern entity   */
            /* (e.g. instance) in a partial match is 'out of date'  */
            /* with respect to the lazy evaluation scheme use by    */
            /* negated patterns. In other words, the object may     */
            /* have changed since it was last pushed through the    */
            /* network, and thus the partial match may be invalid.  */
            /* If so, the partial match must be ignored here.       */
            /*======================================================*/

        else if (PartialMatchDefunct(theEnv, possibleConflicts)) { /* Do Nothing */ }

        else if ((operation == NETWORK_RETRACT) && PartialMatchWillBeDeleted(theEnv, possibleConflicts)) { /* Do Nothing */ }

            /*================================================*/
            /* If the join doesn't have a network expression  */
            /* to be evaluated, then partial match conflicts. */
            /*================================================*/

        else if (theJoin->networkTest == nullptr) { result = true; }

            /*=================================================*/
            /* Otherwise, if the join has a network expression */
            /* to evaluate, then evaluate it.                  */
            /*=================================================*/

        else {
#if DEVELOPER
            if (theJoin->networkTest)
              {
               EngineData(theEnv)->leftToRightComparisons++;
               EngineData(theEnv)->findNextConflictingComparisons++;
              }
#endif
            EngineData(theEnv)->GlobalRHSBinds = possibleConflicts;

            result = EvaluateJoinExpression(theEnv, theJoin->networkTest, theJoin);
            if (EvaluationData(theEnv)->EvaluationError) {
                result = true;
                EvaluationData(theEnv)->EvaluationError = false;
            }

#if DEVELOPER
            if (result != false)
             { EngineData(theEnv)->leftToRightSucceeds++; }
#endif
        }

        /*==============================================*/
        /* If the network expression evaluated to true, */
        /* then partial match being examined conflicts. */
        /* Point the beta memory partial match to the   */
        /* conflicting partial match and return true to */
        /* indicate a conflict was found.               */
        /*==============================================*/

        if (result) {
            AddBlockedLink(theBind, possibleConflicts);
            EngineData(theEnv)->GlobalLHSBinds = oldLHSBinds;
            EngineData(theEnv)->GlobalRHSBinds = oldRHSBinds;
            EngineData(theEnv)->GlobalJoin = oldJoin;
            return true;
        }
    }

    if (restore) {
        EngineData(theEnv)->GlobalLHSBinds = oldLHSBinds;
        EngineData(theEnv)->GlobalRHSBinds = oldRHSBinds;
        EngineData(theEnv)->GlobalJoin = oldJoin;
    }

    /*========================*/
    /* No conflict was found. */
    /*========================*/

    return false;
}

/***********************************************************/
/* PartialMatchDefunct: Determines if any pattern entities */
/*   contained within the partial match have changed since */
/*   this partial match was generated. Assumes counterf is */
/*   false.                                                */
/***********************************************************/
static bool PartialMatchDefunct(
        const Environment&theEnv,
        PartialMatch *thePM) {
    unsigned short i;
    struct patternEntity *thePE;

    if (thePM->deleting) return true;

    for (i = 0; i < thePM->bcount; i++) {
        if (thePM->binds[i].gm.theMatch == nullptr) continue;
        thePE = thePM->binds[i].gm.theMatch->matchingItem;
        if (thePE && thePE->theInfo->synchronized &&
            !(*thePE->theInfo->synchronized)(theEnv, thePE))
            return true;
    }
    return false;
}

/*****************************************************************/
/* PartialMatchWillBeDeleted: Determines if any pattern entities */
/*   contained within the partial match were deleted as part of  */
/*   a retraction/deletion. When rules have multiple patterns    */
/*   that can be matched by the same fact it's possible that a   */
/*   partial match encountered in the join network has not yet   */
/*   deleted and so should not be considered as valid.           */
/*****************************************************************/
bool PartialMatchWillBeDeleted(
        const Environment&theEnv,
        PartialMatch *thePM) {
    unsigned short i;
    struct patternEntity *thePE;

    if (thePM == nullptr) return false;

    if (thePM->deleting) return true;

    for (i = 0; i < thePM->bcount; i++) {
        if (thePM->binds[i].gm.theMatch == nullptr) continue;
        thePE = thePM->binds[i].gm.theMatch->matchingItem;
        if (thePE && thePE->theInfo->isDeleted &&
            (*thePE->theInfo->isDeleted)(theEnv, thePE))
            return true;
    }

    return false;
}

/***************************************************/
/* DeletePartialMatches: Returns a list of partial */
/*   matches to the pool of free memory.           */
/***************************************************/
void DeletePartialMatches(
        const Environment&theEnv,
        PartialMatch *listOfPMs) {
    PartialMatch *nextPM;

    while (listOfPMs != nullptr) {
        /*============================================*/
        /* Remember the next partial match to delete. */
        /*============================================*/

        nextPM = listOfPMs->nextInMemory;

        /*================================================*/
        /* Remove the links between the partial match and */
        /* any data entities that it is attached to as a  */
        /* result of a logical CE.                        */
        /*================================================*/

        if (listOfPMs->dependents != nullptr) RemoveLogicalSupport(theEnv, listOfPMs);

        /*==========================================================*/
        /* If the partial match is being deleted from a beta memory */
        /* and the partial match isn't associated with a satisfied  */
        /* not CE, then it can be immediately returned to the pool  */
        /* of free memory. Otherwise, it's could be in use (either  */
        /* to retrieve variables from the LHS or by the activation  */
        /* of the rule). Since a not CE creates a "pseudo" data     */
        /* entity, the beta partial match which stores this pseudo  */
        /* data entity can not be deleted immediately (for the same */
        /* reason an alpha memory partial match can't be deleted    */
        /* immediately).                                            */
        /*==========================================================*/

        ReturnPartialMatch(theEnv, listOfPMs);

        /*====================================*/
        /* Move on to the next partial match. */
        /*====================================*/

        listOfPMs = nextPM;
    }
}

/**************************************************************/
/* ReturnPartialMatch: Returns the data structures associated */
/*   with a partial match to the pool of free memory.         */
/**************************************************************/
void ReturnPartialMatch(
        const Environment&theEnv,
        PartialMatch *waste) {
    /*==============================================*/
    /* If the partial match is in use, then put it  */
    /* on a garbage list to be processed later when */
    /* the partial match is not in use.             */
    /*==============================================*/

    if (waste->busy) {
        waste->nextInMemory = EngineData(theEnv)->GarbagePartialMatches;
        EngineData(theEnv)->GarbagePartialMatches = waste;
        return;
    }

    /*======================================================*/
    /* If we're dealing with an alpha memory partial match, */
    /* then return the multifield markers associated with   */
    /* the partial match (if any) along with the alphaMatch */
    /* data structure.                                      */
    /*======================================================*/

    if (waste->betaMemory == false) {
        if (waste->binds[0].gm.theMatch->markers != nullptr) { ReturnMarkers(theEnv, waste->binds[0].gm.theMatch->markers); }
        rm(theEnv, waste->binds[0].gm.theMatch, sizeof(alphaMatch));
    }

    /*=================================================*/
    /* Remove any links between the partial match and  */
    /* a data entity that were created with the use of */
    /* the logical CE.                                 */
    /*=================================================*/

    if (waste->dependents != nullptr) RemovePMDependencies(theEnv, waste);

    /*======================================================*/
    /* Return the partial match to the pool of free memory. */
    /*======================================================*/

    rtn_var_struct(theEnv, partialMatch, sizeof(genericMatch *) *
                                         (waste->bcount - 1),
                   waste);
}

/***************************************************************/
/* DestroyPartialMatch: Returns the data structures associated */
/*   with a partial match to the pool of free memory.          */
/***************************************************************/
void DestroyPartialMatch(
        const Environment&theEnv,
        PartialMatch *waste) {
    /*======================================================*/
    /* If we're dealing with an alpha memory partial match, */
    /* then return the multifield markers associated with   */
    /* the partial match (if any) along with the alphaMatch */
    /* data structure.                                      */
    /*======================================================*/

    if (waste->betaMemory == false) {
        if (waste->binds[0].gm.theMatch->markers != nullptr) { ReturnMarkers(theEnv, waste->binds[0].gm.theMatch->markers); }
        rm(theEnv, waste->binds[0].gm.theMatch, sizeof(alphaMatch));
    }

    /*=================================================*/
    /* Remove any links between the partial match and  */
    /* a data entity that were created with the use of */
    /* the logical CE.                                 */
    /*=================================================*/

    if (waste->dependents != nullptr) DestroyPMDependencies(theEnv, waste);

    /*======================================================*/
    /* Return the partial match to the pool of free memory. */
    /*======================================================*/

    rtn_var_struct(theEnv, partialMatch, sizeof(genericMatch *) *
                                         (waste->bcount - 1),
                   waste);
}

/******************************************************/
/* ReturnMarkers: Returns a linked list of multifield */
/*   markers associated with a data entity matching a */
/*   pattern to the pool of free memory.              */
/******************************************************/
static void ReturnMarkers(
        const Environment&theEnv,
        struct multifieldMarker *waste) {
    struct multifieldMarker *temp;

    while (waste != nullptr) {
        temp = waste->next;
        rtn_struct(theEnv, multifieldMarker, waste);
        waste = temp;
    }
}

/*************************************************************/
/* FlushGarbagePartialMatches:  Returns partial matches and  */
/*   associated structures that were removed as part of a    */
/*   retraction. It is necessary to postpone returning these */
/*   structures to memory because RHS actions retrieve their */
/*   variable bindings directly from the fact and instance   */
/*   data structures through the alpha memory bindings.      */
/*************************************************************/
void FlushGarbagePartialMatches(
        const Environment&theEnv) {
    PartialMatch *pmPtr;
    struct alphaMatch *amPtr;

    /*===================================================*/
    /* Return the garbage partial matches collected from */
    /* the alpha memories of the pattern networks.       */
    /*===================================================*/

    while (EngineData(theEnv)->GarbageAlphaMatches != nullptr) {
        amPtr = EngineData(theEnv)->GarbageAlphaMatches->next;
        rtn_struct(theEnv, alphaMatch, EngineData(theEnv)->GarbageAlphaMatches);
        EngineData(theEnv)->GarbageAlphaMatches = amPtr;
    }

    /*==============================================*/
    /* Return the garbage partial matches collected */
    /* from the beta memories of the join networks. */
    /*==============================================*/

    while (EngineData(theEnv)->GarbagePartialMatches != nullptr) {
        /*=====================================================*/
        /* Remember the next garbage partial match to process. */
        /*=====================================================*/

        pmPtr = EngineData(theEnv)->GarbagePartialMatches->nextInMemory;

        /*============================================*/
        /* Dispose of the garbage partial match being */
        /* examined and move on to the next one.      */
        /*============================================*/

        EngineData(theEnv)->GarbagePartialMatches->busy = false;
        ReturnPartialMatch(theEnv, EngineData(theEnv)->GarbagePartialMatches);
        EngineData(theEnv)->GarbagePartialMatches = pmPtr;
    }
}

