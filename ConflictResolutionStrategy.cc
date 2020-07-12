/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  08/25/16             */
/*                                                     */
/*         CONFLICT RESOLUTION STRATEGY MODULE         */
/*******************************************************/

/*************************************************************/
/* Purpose: Used to determine where a new activation is      */
/*   placed on the agenda based on the current conflict      */
/*   resolution strategy (depth, breadth, mea, lex,          */
/*   simplicity, or complexity). Also provides the           */
/*   set-strategy and get-strategy commands.                 */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*      6.24: Removed CONFLICT_RESOLUTION_STRATEGIES         */
/*            compilation flag.                              */
/*                                                           */
/*      6.30: Added salience groups to improve performance   */
/*            with large numbers of activations of different */
/*            saliences.                                     */
/*                                                           */
/*            Removed pseudo-facts used for not CEs.         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#include <cstdio>
#include <cstring>

#include "Setup.h"


#include "Agenda.h"
#include "ArgumentAccess.h"
#include "Constants.h"
#include "Environment.h"
#include "MemoryAllocation.h"
#include "Pattern.h"
#include "ReteUtility.h"

#include "ConflictResolutionStrategy.h"

#define GetMatchingItem(x, i) ((x->getBasis()->binds[i].gm.theMatch != nullptr) ? \
                              (x->getBasis()->binds[i].gm.theMatch->matchingItem) : nullptr)

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static Activation::Ptr PlaceDepthActivation(Activation::Ptr , struct SalienceGroup *);
static Activation::Ptr PlaceBreadthActivation(Activation::Ptr , struct SalienceGroup *);
static Activation::Ptr PlaceLEXActivation(const Environment&, Activation::Ptr , struct SalienceGroup *);
static Activation::Ptr PlaceMEAActivation(const Environment&, Activation::Ptr , struct SalienceGroup *);
static Activation::Ptr PlaceComplexityActivation(Activation::Ptr , struct SalienceGroup *);
static Activation::Ptr PlaceSimplicityActivation(Activation::Ptr , struct SalienceGroup *);
static Activation::Ptr PlaceRandomActivation(Activation::Ptr , struct SalienceGroup *);
static int ComparePartialMatches(const Environment&, Activation::Ptr , Activation::Ptr );
static const char *GetStrategyName(StrategyType);
static unsigned long long *SortPartialMatch(const Environment&, PartialMatch *);

/******************************************************************/
/* PlaceActivation: Coordinates placement of an activation on the */
/*   Agenda based on the current conflict resolution strategy.    */
/******************************************************************/
void PlaceActivation(
        const Environment&theEnv,
        Activation::Ptr *whichAgenda,
        Activation::Ptr newActivation,
        struct SalienceGroup *theGroup) {
    Activation::Ptr placeAfter = nullptr;

    /*================================================*/
    /* Set the flag which indicates that a change has */
    /* been made to the agenda.                       */
    /*================================================*/
    AgendaData(theEnv)->markAgendaHasChanged(true);

    /*=============================================*/
    /* Determine the location where the activation */
    /* should be placed in the agenda based on the */
    /* current conflict resolution strategy.       */
    /*==============================================*/

    if (*whichAgenda != nullptr) {
        switch (AgendaData(theEnv)->getStrategy()) {
            case DEPTH_STRATEGY:
                placeAfter = PlaceDepthActivation(newActivation, theGroup);
                break;

            case BREADTH_STRATEGY:
                placeAfter = PlaceBreadthActivation(newActivation, theGroup);
                break;

            case LEX_STRATEGY:
                placeAfter = PlaceLEXActivation(theEnv, newActivation, theGroup);
                break;

            case MEA_STRATEGY:
                placeAfter = PlaceMEAActivation(theEnv, newActivation, theGroup);
                break;

            case COMPLEXITY_STRATEGY:
                placeAfter = PlaceComplexityActivation(newActivation, theGroup);
                break;

            case SIMPLICITY_STRATEGY:
                placeAfter = PlaceSimplicityActivation(newActivation, theGroup);
                break;

            case RANDOM_STRATEGY:
                placeAfter = PlaceRandomActivation(newActivation, theGroup);
                break;
        }
    } else {
        theGroup->setFirst(newActivation);
        theGroup->setLast(newActivation);
    }

    /*==============================================================*/
    /* Place the activation at the appropriate place in the agenda. */
    /*==============================================================*/

    if (placeAfter == nullptr) /* then place it at the beginning of then agenda. */
    {
        newActivation->setNext(*whichAgenda);
        *whichAgenda = newActivation;
        if (newActivation->getNext()!= nullptr) newActivation->getNext()->setPrevious(newActivation);
    } else /* insert it in the agenda. */
    {
        newActivation->setNext(placeAfter->getNext());
        newActivation->setPrevious(placeAfter);
        placeAfter->setNext(newActivation);
        if (newActivation->getNext() != nullptr) {
            newActivation->getNext()->setPrevious(newActivation);
        }
    }
}

/*******************************************************************/
/* PlaceDepthActivation: Determines the location in the agenda     */
/*    where a new activation should be placed for the depth        */
/*    strategy. Returns a pointer to the activation after which    */
/*    the new activation should be placed (or nullptr if the          */
/*    activation should be placed at the beginning of the agenda). */
/*******************************************************************/
static Activation::Ptr PlaceDepthActivation(
        Activation::Ptr newActivation,
        struct SalienceGroup *theGroup) {
    Activation::Ptr lastAct, *actPtr;
    unsigned long long timetag;

    /*============================================*/
    /* Set up initial information for the search. */
    /*============================================*/

    timetag = newActivation->getTimetag();
    if (theGroup->getPrevious() == nullptr) { lastAct = nullptr; }
    else { lastAct = theGroup->getPrevious()->getLast(); }

    /*=========================================================*/
    /* Find the insertion point in the agenda. The activation  */
    /* is placed before activations of lower salience and      */
    /* after activations of higher salience. Among activations */
    /* of equal salience, the activation is placed before      */
    /* activations with an equal or lower timetag (yielding    */
    /* depth first traversal).                                 */
    /*=========================================================*/

    actPtr = theGroup->getFirst();
    while (actPtr != nullptr) {
        if (timetag < actPtr->getTimetag()) {
            lastAct = actPtr;
            if (actPtr == theGroup->getLast()) { break; }
            else { actPtr = actPtr->getNext(); }
        } else { break; }
    }

    /*========================================*/
    /* Update the salience group information. */
    /*========================================*/

    if ((lastAct == nullptr) ||
        ((theGroup->getPrevious() != nullptr) && (theGroup->getPrevious()->getLast() == lastAct))) { theGroup->setFirst(newActivation); }

    if ((theGroup->getLast() == nullptr) || (theGroup->getLast() == lastAct)) { theGroup->setLast(newActivation); }

    /*===========================================*/
    /* Return the insertion point in the agenda. */
    /*===========================================*/

    return (lastAct);
}

/*******************************************************************/
/* PlaceBreadthActivation: Determines the location in the agenda   */
/*    where a new activation should be placed for the breadth      */
/*    strategy. Returns a pointer to the activation after which    */
/*    the new activation should be placed (or nullptr if the          */
/*    activation should be placed at the beginning of the agenda). */
/*******************************************************************/
static Activation::Ptr PlaceBreadthActivation(
        Activation::Ptr newActivation,
        struct SalienceGroup *theGroup) {
    unsigned long long timetag;
    Activation::Ptr lastAct, actPtr;

    /*============================================*/
    /* Set up initial information for the search. */
    /*============================================*/

    timetag = newActivation->getTimetag();
    if (theGroup->getLast() == nullptr) {
        if (theGroup->getPrevious() == nullptr) { lastAct = nullptr; }
        else { lastAct = theGroup->getPrevious()->getLast(); }
    } else { lastAct = theGroup->getLast(); }

    /*=========================================================*/
    /* Find the insertion point in the agenda. The activation  */
    /* is placed before activations of lower salience and      */
    /* after activations of higher salience. Among activations */
    /* of equal salience, the activation is placed after       */
    /* activations with a lessor timetag (yielding breadth     */
    /* first traversal).                                       */
    /*=========================================================*/

    actPtr = theGroup->getLast();
    while (actPtr != nullptr) {
        if (timetag < actPtr->getTimetag()) {
            if (actPtr == theGroup->getFirst()) {
                if (theGroup->getPrevious() == nullptr) { lastAct = nullptr; }
                else { lastAct = theGroup->getPrevious()->getLast(); }
                break;
            } else { actPtr = actPtr->getPrevious(); }
        } else {
            lastAct = actPtr;
            break;
        }
    }

    /*========================================*/
    /* Update the salience group information. */
    /*========================================*/

    if ((lastAct == nullptr) ||
        ((theGroup->getPrevious() != nullptr) && (theGroup->getPrevious()->getLast() == lastAct))) { theGroup->setFirst(newActivation); }

    if ((theGroup->getLast() == nullptr) || (theGroup->getLast() == lastAct)) { theGroup->setLast(newActivation); }

    /*===========================================*/
    /* Return the insertion point in the agenda. */
    /*===========================================*/

    return lastAct;
}

/*******************************************************************/
/* PlaceLEXActivation: Determines the location in the agenda       */
/*    where a new activation should be placed for the lex          */
/*    strategy. Returns a pointer to the activation after which    */
/*    the new activation should be placed (or nullptr if the          */
/*    activation should be placed at the beginning of the agenda). */
/*******************************************************************/
static Activation::Ptr PlaceLEXActivation(
        const Environment&theEnv,
        Activation::Ptr newActivation,
        struct SalienceGroup *theGroup) {
    unsigned long long timetag;
    Activation::Ptr lastAct;
    int flag;

    /*============================================*/
    /* Set up initial information for the search. */
    /*============================================*/

    timetag = newActivation->getTimetag();
    if (theGroup->getPrevious() == nullptr) { lastAct = nullptr; }
    else { lastAct = theGroup->getPrevious()->getLast(); }

    /*================================================*/
    /* Look first at the very end of the group to see */
    /* if the activation should be placed there.      */
    /*================================================*/

    auto actPtr = theGroup->getLast();
    if (actPtr != nullptr) {
        flag = ComparePartialMatches(theEnv, actPtr, newActivation);

        if ((flag == LESS_THAN) ||
            ((flag == EQUAL) && (timetag > actPtr->getTimetag()))) {
            theGroup->setLast(newActivation);

            return actPtr;
        }
    }

    /*=========================================================*/
    /* Find the insertion point in the agenda. The activation  */
    /* is placed before activations of lower salience and      */
    /* after activations of higher salience. Among activations */
    /* of equal salience, the OPS5 lex strategy is used for    */
    /* determining placement.                                  */
    /*=========================================================*/

    actPtr = theGroup->getFirst();
    while (actPtr != nullptr) {
        flag = ComparePartialMatches(theEnv, actPtr, newActivation);

        if (flag == LESS_THAN) {
            lastAct = actPtr;
            if (actPtr == theGroup->getLast()) { break; }
            else { actPtr = actPtr->getNext(); }
        } else if (flag == GREATER_THAN) { break; }
        else /* flag == EQUAL */
        {
            if (timetag > actPtr->getTimetag()) {
                lastAct = actPtr;
                if (actPtr == theGroup->getLast()) { break; }
                else { actPtr = actPtr->getNext(); }
            } else { break; }
        }
    }

    /*========================================*/
    /* Update the salience group information. */
    /*========================================*/

    if ((lastAct == nullptr) ||
        ((theGroup->getPrevious() != nullptr) && (theGroup->getPrevious()->getLast() == lastAct))) { theGroup->setFirst(newActivation); }

    if ((theGroup->getLast() == nullptr) || (theGroup->getLast() == lastAct)) { theGroup->setLast(newActivation); }

    /*===========================================*/
    /* Return the insertion point in the agenda. */
    /*===========================================*/

    return lastAct;
}

/*******************************************************************/
/* PlaceMEAActivation: Determines the location in the agenda       */
/*    where a new activation should be placed for the mea          */
/*    strategy. Returns a pointer to the activation after which    */
/*    the new activation should be placed (or nullptr if the          */
/*    activation should be placed at the beginning of the agenda). */
/*******************************************************************/
static Activation::Ptr PlaceMEAActivation(
        const Environment&theEnv,
        Activation::Ptr newActivation,
        struct SalienceGroup *theGroup) {
    unsigned long long timetag;
    Activation::Ptr lastAct, *actPtr;
    int flag;
    unsigned long long cWhoset = 0, oWhoset = 0;
    bool cSet, oSet;

    /*============================================*/
    /* Set up initial information for the search. */
    /*============================================*/

    timetag = newActivation->getTimetag();
    if (theGroup->getPrevious() == nullptr) { lastAct = nullptr; }
    else { lastAct = theGroup->getPrevious()->getLast(); }

    /*================================================*/
    /* Look first at the very end of the group to see */
    /* if the activation should be placed there.      */
    /*================================================*/

    actPtr = theGroup->getLast();
    if (actPtr != nullptr) {
        if (GetMatchingItem(newActivation, 0) != nullptr) {
            cWhoset = GetMatchingItem(newActivation, 0)->timeTag;
            cSet = true;
        } else { cSet = false; }

        if (GetMatchingItem(actPtr, 0) != nullptr) {
            oWhoset = GetMatchingItem(actPtr, 0)->timeTag;
            oSet = true;
        } else { oSet = false; }

        if (!cSet && !oSet) { flag = ComparePartialMatches(theEnv, actPtr, newActivation); }
        else if (cSet && !oSet) { flag = GREATER_THAN; }
        else if (!cSet && oSet) { flag = LESS_THAN; }
        else if (oWhoset < cWhoset) { flag = GREATER_THAN; }
        else if (oWhoset > cWhoset) { flag = LESS_THAN; }
        else { flag = ComparePartialMatches(theEnv, actPtr, newActivation); }

        if ((flag == LESS_THAN) ||
            ((flag == EQUAL) && (timetag > actPtr->getTimetag()))) {
            theGroup->setLast(newActivation);

            return actPtr;
        }
    }

    /*=========================================================*/
    /* Find the insertion point in the agenda. The activation  */
    /* is placed before activations of lower salience and      */
    /* after activations of higher salience. Among activations */
    /* of equal salience, the OPS5 mea strategy is used for    */
    /* determining placement.                                  */
    /*=========================================================*/

    actPtr = theGroup->getFirst();
    while (actPtr != nullptr) {
        cWhoset = 0;
        oWhoset = 0;
        if (GetMatchingItem(newActivation, 0) != nullptr) { cWhoset = GetMatchingItem(newActivation, 0)->timeTag; }

        if (GetMatchingItem(actPtr, 0) != nullptr) { oWhoset = GetMatchingItem(actPtr, 0)->timeTag; }

        if (oWhoset < cWhoset) {
            if (cWhoset > 0) flag = GREATER_THAN;
            else flag = LESS_THAN;
        } else if (oWhoset > cWhoset) {
            if (oWhoset > 0) flag = LESS_THAN;
            else flag = GREATER_THAN;
        } else { flag = ComparePartialMatches(theEnv, actPtr, newActivation); }

        if (flag == LESS_THAN) {
            lastAct = actPtr;
            if (actPtr == theGroup->getLast()) { break; }
            else { actPtr = actPtr->getNext(); }
        } else if (flag == GREATER_THAN) { break; }
        else /* flag == EQUAL */
        {
            if (timetag > actPtr->getTimetag()) {
                lastAct = actPtr;
                if (actPtr == theGroup->getLast()) { break; }
                else { actPtr = actPtr->getNext(); }
            } else { break; }
        }
    }

    /*========================================*/
    /* Update the salience group information. */
    /*========================================*/

    if ((lastAct == nullptr) ||
        ((theGroup->getPrevious() != nullptr) && (theGroup->getPrevious()->getLast() == lastAct))) { theGroup->setFirst(newActivation); }

    if ((theGroup->getLast() == nullptr) || (theGroup->getLast() == lastAct)) { theGroup->setLast(newActivation); }

    /*===========================================*/
    /* Return the insertion point in the agenda. */
    /*===========================================*/

    return lastAct;
}

/*********************************************************************/
/* PlaceComplexityActivation: Determines the location in the agenda  */
/*    where a new activation should be placed for the complexity     */
/*    strategy. Returns a pointer to the activation  after which the */
/*    new activation should be placed (or nullptr if the activation     */
/*    should be placed at the beginning of the agenda).              */
/*********************************************************************/
static Activation::Ptr PlaceComplexityActivation(
        Activation::Ptr newActivation,
        struct SalienceGroup *theGroup) {
    unsigned int complexity;
    unsigned long long timetag;
    Activation::Ptr lastAct, *actPtr;

    /*========================================*/
    /* Set up initial information for search. */
    /*========================================*/

    timetag = newActivation->getTimetag();
    complexity = newActivation->getRule()->complexity;
    if (theGroup->getPrevious() == nullptr) { lastAct = nullptr; }
    else { lastAct = theGroup->getPrevious()->getLast(); }

    /*=========================================================*/
    /* Find the insertion point in the agenda. The activation  */
    /* is placed before activations of lower salience and      */
    /* after activations of higher salience. Among activations */
    /* of equal salience, the activation is placed before      */
    /* activations of equal or lessor complexity.              */
    /*=========================================================*/

    actPtr = theGroup->getFirst();
    while (actPtr != nullptr) {
        if (complexity < actPtr->getRule()->complexity) {
            lastAct = actPtr;
            if (actPtr == theGroup->getLast()) { break; }
            else { actPtr = actPtr->getNext(); }
        } else if (complexity > actPtr->getRule()->complexity) { break; }
        else if (timetag > actPtr->getTimetag()) {
            lastAct = actPtr;
            if (actPtr == theGroup->getLast()) { break; }
            else { actPtr = actPtr->getNext(); }
        } else { break; }
    }

    /*========================================*/
    /* Update the salience group information. */
    /*========================================*/

    if ((lastAct == nullptr) ||
        ((theGroup->getPrevious() != nullptr) && (theGroup->getPrevious()->getLast() == lastAct))) { theGroup->setFirst(newActivation); }

    if ((theGroup->getLast() == nullptr) || (theGroup->getLast() == lastAct)) { theGroup->setLast(newActivation); }

    /*===========================================*/
    /* Return the insertion point in the agenda. */
    /*===========================================*/

    return lastAct;
}

/*********************************************************************/
/* PlaceSimplicityActivation: Determines the location in the agenda  */
/*    where a new activation should be placed for the simplicity     */
/*    strategy. Returns a pointer to the activation  after which the */
/*    new activation should be placed (or nullptr if the activation     */
/*    should be placed at the beginning of the agenda).              */
/*********************************************************************/
static Activation::Ptr PlaceSimplicityActivation(
        Activation::Ptr newActivation,
        struct SalienceGroup *theGroup) {
    unsigned int complexity;
    unsigned long long timetag;
    Activation::Ptr lastAct, *actPtr;

    /*============================================*/
    /* Set up initial information for the search. */
    /*============================================*/

    timetag = newActivation->getTimetag();
    complexity = newActivation->getRule()->complexity;
    if (theGroup->getPrevious() == nullptr) { lastAct = nullptr; }
    else { lastAct = theGroup->getPrevious()->getLast(); }

    /*=========================================================*/
    /* Find the insertion point in the agenda. The activation  */
    /* is placed before activations of lower salience and      */
    /* after activations of higher salience. Among activations */
    /* of equal salience, the activation is placed after       */
    /* activations of equal or greater complexity.             */
    /*=========================================================*/

    actPtr = theGroup->getFirst();
    while (actPtr != nullptr) {
        if (complexity > actPtr->getRule()->complexity) {
            lastAct = actPtr;
            if (actPtr == theGroup->getLast()) { break; }
            else { actPtr = actPtr->getNext(); }
        } else if (complexity < actPtr->getRule()->complexity) { break; }
        else if (timetag > actPtr->getTimetag()) {
            lastAct = actPtr;
            if (actPtr == theGroup->getLast()) { break; }
            else { actPtr = actPtr->getNext(); }
        } else { break; }
    }

    /*========================================*/
    /* Update the salience group information. */
    /*========================================*/

    if ((lastAct == nullptr) ||
        ((theGroup->getPrevious() != nullptr) && (theGroup->getPrevious()->getLast() == lastAct))) { theGroup->setFirst(newActivation); }

    if ((theGroup->getLast() == nullptr) || (theGroup->getLast() == lastAct)) { theGroup->setLast(newActivation); }

    /*===========================================*/
    /* Return the insertion point in the agenda. */
    /*===========================================*/

    return (lastAct);
}

/*******************************************************************/
/* PlaceRandomActivation: Determines the location in the agenda    */
/*    where a new activation should be placed for the random       */
/*    strategy. Returns a pointer to the activation  after which   */
/*    the new activation should be placed (or nullptr if the          */
/*    activation should be placed at the beginning of the agenda). */
/*******************************************************************/
static Activation::Ptr PlaceRandomActivation(
        Activation::Ptr newActivation,
        struct SalienceGroup *theGroup) {
    int randomID;
    unsigned long long timetag;
    Activation::Ptr lastAct, *actPtr;

    /*============================================*/
    /* Set up initial information for the search. */
    /*============================================*/

    timetag = newActivation->getTimetag();
    randomID = newActivation->getRandomID();
    if (theGroup->getPrevious() == nullptr) { lastAct = nullptr; }
    else { lastAct = theGroup->getPrevious()->getLast(); }

    /*=========================================================*/
    /* Find the insertion point in the agenda. The activation  */
    /* is placed before activations of lower salience and      */
    /* after activations of higher salience. Among activations */
    /* of equal salience, the placement of the activation is   */
    /* determined through the generation of a random number.   */
    /*=========================================================*/

    actPtr = theGroup->getFirst();
    while (actPtr != nullptr) {
        if (randomID > actPtr->getRandomID()) {
            lastAct = actPtr;
            if (actPtr == theGroup->getLast()) { break; }
            else { actPtr = actPtr->getNext(); }
        } else if (randomID < actPtr->getRandomID()) { break; }
        else if (timetag > actPtr->getTimetag()) {
            lastAct = actPtr;
            if (actPtr == theGroup->getLast()) { break; }
            else { actPtr = actPtr->getNext(); }
        } else { break; }
    }

    /*========================================*/
    /* Update the salience group information. */
    /*========================================*/

    if ((lastAct == nullptr) ||
        ((theGroup->getPrevious() != nullptr) && (theGroup->getPrevious()->getLast() == lastAct))) { theGroup->setFirst(newActivation); }

    if ((theGroup->getLast() == nullptr) || (theGroup->getLast() == lastAct)) { theGroup->setLast(newActivation); }

    /*===========================================*/
    /* Return the insertion point in the agenda. */
    /*===========================================*/

    return (lastAct);
}

/*********************************************************/
/* SortPartialMatch: Creates an array of sorted timetags */
/*    in ascending order from a partial match.           */
/*********************************************************/
static unsigned long long *SortPartialMatch(
        const Environment&theEnv,
        PartialMatch *binds) {
    unsigned long long *nbinds;
    unsigned long long temp;
    bool flag;
    unsigned short j, k;

    /*====================================================*/
    /* Copy the array. Use 0 to represent the timetags of */
    /* negated patterns. Patterns matching fact/instances */
    /* should have timetags greater than 0.               */
    /*====================================================*/

    nbinds = (unsigned long long *) get_mem(theEnv, sizeof(long long) * binds->bcount);

    for (j = 0; j < binds->bcount; j++) {
        if ((binds->binds[j].gm.theMatch != nullptr) &&
            (binds->binds[j].gm.theMatch->matchingItem != nullptr)) { nbinds[j] = binds->binds[j].gm.theMatch->matchingItem->timeTag; }
        else { nbinds[j] = 0; }
    }

    /*=================*/
    /* Sort the array. */
    /*=================*/

    for (flag = true, k = binds->bcount - 1;
         flag;
         k--) {
        flag = false;
        for (j = 0; j < k; j++) {
            if (nbinds[j] < nbinds[j + 1]) {
                temp = nbinds[j];
                nbinds[j] = nbinds[j + 1];
                nbinds[j + 1] = temp;
                flag = true;
            }
        }
    }

    /*===================*/
    /* Return the array. */
    /*===================*/

    return nbinds;
}

/**************************************************************************/
/* ComparePartialMatches: Compares two activations using the lex conflict */
/*   resolution strategy to determine which activation should be placed   */
/*   first on the agenda. This lexicographic comparison function is used  */
/*   for both the lex and mea strategies.                                 */
/**************************************************************************/
static int ComparePartialMatches(
        const Environment&theEnv,
        Activation::Ptr actPtr,
        Activation::Ptr newActivation) {
    unsigned cCount, oCount, mCount, i;
    unsigned long long *basis1, *basis2;

    /*=================================================*/
    /* If the activation already on the agenda doesn't */
    /* have a set of sorted timetags, then create one. */
    /*=================================================*/

    basis1 = SortPartialMatch(theEnv, newActivation->getBasis());
    basis2 = SortPartialMatch(theEnv, actPtr->getBasis());

    /*==============================================================*/
    /* Determine the number of timetags in each of the activations. */
    /* The number of timetags to be compared is the lessor of these */
    /* two numbers.                                                 */
    /*==============================================================*/

    cCount = newActivation->getBasis()->bcount;
    oCount = actPtr->getBasis()->bcount;

    if (oCount > cCount) mCount = cCount;
    else mCount = oCount;

    /*===========================================================*/
    /* Compare the sorted timetags one by one until there are no */
    /* more timetags to compare or the timetags being compared   */
    /* are not equal. If the timetags aren't equal, then the     */
    /* activation containing the larger timetag is placed before */
    /* the activation containing the smaller timetag.            */
    /*===========================================================*/

    for (i = 0; i < mCount; i++) {
        if (basis1[i] < basis2[i]) {
            rtn_mem(theEnv, sizeof(long long) * cCount, basis1);
            rtn_mem(theEnv, sizeof(long long) * oCount, basis2);
            return (LESS_THAN);
        } else if (basis1[i] > basis2[i]) {
            rtn_mem(theEnv, sizeof(long long) * cCount, basis1);
            rtn_mem(theEnv, sizeof(long long) * oCount, basis2);
            return (GREATER_THAN);
        }
    }

    rtn_mem(theEnv, sizeof(long long) * cCount, basis1);
    rtn_mem(theEnv, sizeof(long long) * oCount, basis2);

    /*==========================================================*/
    /* If the sorted timetags are identical up to the number of */
    /* timetags contained in the smaller partial match, then    */
    /* the activation containing more timetags should be        */
    /* placed before the activation containing fewer timetags.  */
    /*==========================================================*/

    if (cCount < oCount) return (LESS_THAN);
    else if (cCount > oCount) return (GREATER_THAN);

    /*=========================================================*/
    /* If the sorted partial matches for both activations are  */
    /* identical (containing the same number and values of     */
    /* timetags), then the activation associated with the rule */
    /* having the highest complexity is placed before the      */
    /* other partial match.                                    */
    /*=========================================================*/

    if (newActivation->getRule()->complexity < actPtr->getRule()->complexity) { return (LESS_THAN); }
    else if (newActivation->getRule()->complexity > actPtr->getRule()->complexity) { return (GREATER_THAN); }

    /*================================================*/
    /* The two partial matches are equal for purposes */
    /* of placement on the agenda for the lex and mea */
    /* conflict resolution strategies.                */
    /*================================================*/

    return (EQUAL);
}

/***********************************/
/* SetStrategy: C access routine   */
/*   for the set-strategy command. */
/***********************************/
StrategyType SetStrategy(
        const Environment&theEnv,
        StrategyType value) {
    auto oldStrategy = AgendaData(theEnv)->getStrategy();
    AgendaData(theEnv)->setStrategy(value);

    if (oldStrategy != AgendaData(theEnv)->getStrategy()) { ReorderAllAgendas(theEnv); }

    return oldStrategy;
}

/***********************************/
/* GetStrategy: C access routine   */
/*   for the get-strategy command. */
/***********************************/
StrategyType GetStrategy(
        const Environment&theEnv) {
    return AgendaData(theEnv)->getStrategy();
}

/********************************************/
/* GetStrategyCommand: H/L access routine   */
/*   for the get-strategy command.          */
/********************************************/
void GetStrategyCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    returnValue->lexemeValue = CreateSymbol(theEnv, GetStrategyName(GetStrategy(theEnv)));
}

/********************************************/
/* SetStrategyCommand: H/L access routine   */
/*   for the set-strategy command.          */
/********************************************/
void SetStrategyCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UDFValue theArg;
    const char *argument;
    StrategyType oldStrategy;

    /*=======================*/
    /* Set the return value. */
    /*=======================*/

    oldStrategy = GetStrategy(theEnv);
    returnValue->lexemeValue = CreateSymbol(theEnv, GetStrategyName(oldStrategy));

    /*=========================================*/
    /* Check for the correct type of argument. */
    /*=========================================*/

    if (!UDFFirstArgument(context, SYMBOL_BIT, &theArg)) { return; }

    /*=============================================*/
    /* Set the strategy to the specified strategy. */
    /*=============================================*/

    argument = theArg.lexemeValue->contents;

    if (strcmp(argument, "depth") == 0) { SetStrategy(theEnv, DEPTH_STRATEGY); }
    else if (strcmp(argument, "breadth") == 0) { SetStrategy(theEnv, BREADTH_STRATEGY); }
    else if (strcmp(argument, "lex") == 0) { SetStrategy(theEnv, LEX_STRATEGY); }
    else if (strcmp(argument, "mea") == 0) { SetStrategy(theEnv, MEA_STRATEGY); }
    else if (strcmp(argument, "complexity") == 0) { SetStrategy(theEnv, COMPLEXITY_STRATEGY); }
    else if (strcmp(argument, "simplicity") == 0) { SetStrategy(theEnv, SIMPLICITY_STRATEGY); }
    else if (strcmp(argument, "random") == 0) { SetStrategy(theEnv, RANDOM_STRATEGY); }
    else {
        UDFInvalidArgumentMessage(context,
                                  "symbol with value depth, breadth, lex, mea, complexity, simplicity, or random");
    }
}

/**********************************************************/
/* GetStrategyName: Given the integer value corresponding */
/*   to a specified strategy, return a character string   */
/*   of the strategy's name.                              */
/**********************************************************/
static const char *GetStrategyName(
        StrategyType strategy) {
    const char *sname;

    switch (strategy) {
        case DEPTH_STRATEGY:
            sname = "depth";
            break;
        case BREADTH_STRATEGY:
            sname = "breadth";
            break;
        case LEX_STRATEGY:
            sname = "lex";
            break;
        case MEA_STRATEGY:
            sname = "mea";
            break;
        case COMPLEXITY_STRATEGY:
            sname = "complexity";
            break;
        case SIMPLICITY_STRATEGY:
            sname = "simplicity";
            break;
        case RANDOM_STRATEGY:
            sname = "random";
            break;
        default:
            sname = "unknown";
            break;
    }

    return (sname);
}

