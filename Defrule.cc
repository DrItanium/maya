/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  08/06/16             */
/*                                                     */
/*                   DEFRULE MODULE                    */
/*******************************************************/

/*************************************************************/
/* Purpose: Defines basic defrule primitive functions such   */
/*   as allocating and deallocating, traversing, and finding */
/*   defrule data structures.                                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed DYNAMIC_SALIENCE and                   */
/*            LOGICAL_DEPENDENCIES compilation flags.        */
/*                                                           */
/*            Removed CONFLICT_RESOLUTION_STRATEGIES         */
/*            compilation flag.                              */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warnings.                             */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added support for hashed memories.             */
/*                                                           */
/*            Added additional developer statistics to help  */
/*            analyze join network performance.              */
/*                                                           */
/*            Added salience groups to improve performance   */
/*            with large numbers of activations of different */
/*            saliences.                                     */
/*                                                           */
/*            Added EnvGetDisjunctCount and                  */
/*            EnvGetNthDisjunct functions.                   */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Changed find construct functionality so that    */
/*            imported modules are search when locating a    */
/*            named construct.                               */
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
/*************************************************************/

#include "Setup.h"


#include <cstdio>

#include "Agenda.h"
#include "Drive.h"
#include "Engine.h"
#include "Environment.h"
#include "MemoryAllocation.h"
#include "Pattern.h"
#include "Retract.h"
#include "ReteUtility.h"
#include "BasicRuleCommands.h"
#include "RuleCommands.h"
#include "RuleParser.h"
#include "RuleDelete.h"

#if BLOAD_AND_BSAVE
#include "BinaryLoad.h"
#include "RuleBinarySaveLoad.h"
#endif

#include "Defrule.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void *AllocateModule(const Environment&);
static void ReturnModule(const Environment&, void *);
static void InitializeDefruleModules(const Environment&);
static void DeallocateDefruleData(const Environment&);
static void DestroyDefruleAction(const Environment&, ConstructHeader *, void *);

/**********************************************************/
/* InitializeDefrules: Initializes the defrule construct. */
/**********************************************************/
void InitializeDefrules(
        const Environment&theEnv) {
    unsigned long i;
    //AllocateEnvironmentData(theEnv, DEFRULE_DATA, sizeof(defruleData), DeallocateDefruleData);
    theEnv->allocateEnvironmentModule<defruleData>();

    InitializeEngine(theEnv);
    InitializeAgenda(theEnv);
    InitializePatterns(theEnv);
    InitializeDefruleModules(theEnv);

    AddReservedPatternSymbol(theEnv, "and", nullptr);
    AddReservedPatternSymbol(theEnv, "not", nullptr);
    AddReservedPatternSymbol(theEnv, "or", nullptr);
    AddReservedPatternSymbol(theEnv, "test", nullptr);
    AddReservedPatternSymbol(theEnv, "logical", nullptr);
    AddReservedPatternSymbol(theEnv, "exists", nullptr);
    AddReservedPatternSymbol(theEnv, "forall", nullptr);

    DefruleBasicCommands(theEnv);

    DefruleCommands(theEnv);

    DefruleData(theEnv)->DefruleConstruct =
            AddConstruct(theEnv, "defrule", "defrules",
                         ParseDefrule,
                         (FindConstructFunction *) FindDefrule,
                         GetConstructNamePointer, GetConstructPPForm,
                         GetConstructModuleItem,
                         (GetNextConstructFunction *) GetNextDefrule,
                         SetNextConstruct,
                         (IsConstructDeletableFunction *) DefruleIsDeletable,
                         (DeleteConstructFunction *) Undefrule,
                         (FreeConstructFunction *) ReturnDefrule);

    DefruleData(theEnv)->AlphaMemoryTable = (ALPHA_MEMORY_HASH **)
            gm2(theEnv, sizeof(ALPHA_MEMORY_HASH *) * ALPHA_MEMORY_HASH_SIZE);

    for (i = 0; i < ALPHA_MEMORY_HASH_SIZE; i++) DefruleData(theEnv)->AlphaMemoryTable[i] = nullptr;

    DefruleData(theEnv)->BetaMemoryResizingFlag = true;

    DefruleData(theEnv)->RightPrimeJoins = nullptr;
    DefruleData(theEnv)->LeftPrimeJoins = nullptr;
}

/**************************************************/
/* DeallocateDefruleData: Deallocates environment */
/*    data for the defrule construct.             */
/**************************************************/
static void DeallocateDefruleData(
        const Environment&theEnv) {
    struct defruleModule *theModuleItem;
    Defmodule *theModule;
    Activation::Ptr theActivation, *tmpActivation;
    struct SalienceGroup *theGroup, *tmpGroup;

#if BLOAD_AND_BSAVE
    if (Bloaded(theEnv)) { return; }
#endif

    DoForAllConstructs(theEnv, DestroyDefruleAction,
                       DefruleData(theEnv)->DefruleModuleIndex, false, nullptr);

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        theModuleItem = (defruleModule *)
                GetModuleItem(theEnv, theModule,
                              DefruleData(theEnv)->DefruleModuleIndex);

        theActivation = theModuleItem->agenda;
        while (theActivation != nullptr) {
            tmpActivation = theActivation->getNext();

            rtn_struct(theEnv, Activation, theActivation);

            theActivation = tmpActivation;
        }

        theGroup = theModuleItem->groupings;
        while (theGroup != nullptr) {
            tmpGroup = theGroup->getNext();

            rtn_struct(theEnv, SalienceGroup, theGroup);

            theGroup = tmpGroup;
        }

        rtn_struct(theEnv, defruleModule, theModuleItem);
    }

    rm(theEnv, DefruleData(theEnv)->AlphaMemoryTable, sizeof(ALPHA_MEMORY_HASH *) * ALPHA_MEMORY_HASH_SIZE);
}

/********************************************************/
/* DestroyDefruleAction: Action used to remove defrules */
/*   as a result of DestroyEnvironment.                 */
/********************************************************/
static void DestroyDefruleAction(
        const Environment&theEnv,
        ConstructHeader *theConstruct,
        void *buffer) {
#if MAC_XCD
#pragma unused(buffer)
#endif
    Defrule *theDefrule = (Defrule *) theConstruct;

    DestroyDefrule(theEnv, theDefrule);
}

/*****************************************************/
/* InitializeDefruleModules: Initializes the defrule */
/*   construct for use with the defmodule construct. */
/*****************************************************/
static void InitializeDefruleModules(
        const Environment&theEnv) {
    DefruleData(theEnv)->DefruleModuleIndex = RegisterModuleItem(theEnv, "defrule",
                                                                 AllocateModule,
                                                                 ReturnModule,
#if BLOAD_AND_BSAVE
                                                                 BloadDefruleModuleReference,
#else
            nullptr,
#endif
                                                                 (FindConstructFunction *) FindDefruleInModule);
}

/***********************************************/
/* AllocateModule: Allocates a defrule module. */
/***********************************************/
static void *AllocateModule(
        const Environment&theEnv) {
    struct defruleModule *theItem;

    theItem = get_struct(theEnv, defruleModule);
    theItem->agenda = nullptr;
    theItem->groupings = nullptr;
    return ((void *) theItem);
}

/***********************************************/
/* ReturnModule: Deallocates a defrule module. */
/***********************************************/
static void ReturnModule(
        const Environment&theEnv,
        void *theItem) {
    FreeConstructHeaderModule(theEnv, (defmoduleItemHeader *) theItem, DefruleData(theEnv)->DefruleConstruct);
    rtn_struct(theEnv, defruleModule, theItem);
}

/************************************************************/
/* GetDefruleModuleItem: Returns a pointer to the defmodule */
/*  item for the specified defrule or defmodule.            */
/************************************************************/
struct defruleModule *GetDefruleModuleItem(
        const Environment&theEnv,
        Defmodule *theModule) {
    return ((defruleModule *) GetConstructModuleItemByIndex(theEnv, theModule, DefruleData(theEnv)->DefruleModuleIndex));
}

/****************************************************************/
/* FindDefrule: Searches for a defrule in the list of defrules. */
/*   Returns a pointer to the defrule if found, otherwise nullptr. */
/****************************************************************/
Defrule *FindDefrule(
        const Environment&theEnv,
        const char *defruleName) {
    return (Defrule *) FindNamedConstructInModuleOrImports(theEnv, defruleName, DefruleData(theEnv)->DefruleConstruct);
}

/************************************************************************/
/* FindDefruleInModule: Searches for a defrule in the list of defrules. */
/*   Returns a pointer to the defrule if found, otherwise nullptr.         */
/************************************************************************/
Defrule *FindDefruleInModule(
        const Environment&theEnv,
        const char *defruleName) {
    return (Defrule *) FindNamedConstructInModule(theEnv, defruleName, DefruleData(theEnv)->DefruleConstruct);
}

/************************************************************/
/* GetNextDefrule: If passed a nullptr pointer, returns the    */
/*   first defrule in the ListOfDefrules. Otherwise returns */
/*   the next defrule following the defrule passed as an    */
/*   argument.                                              */
/************************************************************/
Defrule *GetNextDefrule(
        const Environment&theEnv,
        Defrule *defrulePtr) {
    return (Defrule *) GetNextConstructItem(theEnv, &defrulePtr->header, DefruleData(theEnv)->DefruleModuleIndex);
}

/******************************************************/
/* DefruleIsDeletable: Returns true if a particular   */
/*   defrule can be deleted, otherwise returns false. */
/******************************************************/
bool DefruleIsDeletable(
        Defrule *theDefrule) {
    const Environment&theEnv = theDefrule->header.env;

    if (!ConstructsDeletable(theEnv)) { return false; }

    for (;
            theDefrule != nullptr;
            theDefrule = theDefrule->disjunct) { if (theDefrule->executing) return false; }

    return !EngineData(theEnv)->JoinOperationInProgress;

}

/********************************************************/
/* GetDisjunctCount: Returns the number of disjuncts of */
/*   a rule (permutations caused by the use of or CEs). */
/********************************************************/
long GetDisjunctCount(
        const Environment&theEnv,
        Defrule *theDefrule) {
    long count = 0;

    for (;
            theDefrule != nullptr;
            theDefrule = theDefrule->disjunct) { count++; }

    return (count);
}

/*******************************************************/
/* GetNthDisjunct: Returns the nth disjunct of a rule. */
/*   The disjunct indices run from 1 to N rather than  */
/*   0 to N - 1.                                       */
/*******************************************************/
Defrule *GetNthDisjunct(
        const Environment&theEnv,
        Defrule *theDefrule,
        long index) {
    long count = 0;

    for (;
            theDefrule != nullptr;
            theDefrule = theDefrule->disjunct) {
        count++;
        if (count == index) { return theDefrule; }
    }

    return nullptr;
}

#if BLOAD_AND_BSAVE

/**************************/
/* AddBetaMemoriesToJoin: */
/**************************/
void AddBetaMemoriesToJoin(
        const Environment&theEnv,
        struct joinNode *theNode) {
    if ((theNode->leftMemory != nullptr) || (theNode->rightMemory != nullptr)) { return; }

    if ((!theNode->firstJoin) || theNode->patternIsExists || theNode->patternIsNegated || theNode->joinFromTheRight) {
        if (theNode->leftHash == nullptr) {
            theNode->leftMemory = get_struct(theEnv, betaMemory);
            theNode->leftMemory->beta = (PartialMatch **) genalloc(theEnv, sizeof(PartialMatch *));
            theNode->leftMemory->beta[0] = nullptr;
            theNode->leftMemory->size = 1;
            theNode->leftMemory->count = 0;
            theNode->leftMemory->last = nullptr;
        } else {
            theNode->leftMemory = get_struct(theEnv, betaMemory);
            theNode->leftMemory->beta = (PartialMatch **) genalloc(theEnv, sizeof(PartialMatch *) * INITIAL_BETA_HASH_SIZE);
            memset(theNode->leftMemory->beta, 0, sizeof(PartialMatch *) * INITIAL_BETA_HASH_SIZE);
            theNode->leftMemory->size = INITIAL_BETA_HASH_SIZE;
            theNode->leftMemory->count = 0;
            theNode->leftMemory->last = nullptr;
        }

        if (theNode->firstJoin && (theNode->patternIsExists || theNode->patternIsNegated || theNode->joinFromTheRight)) {
            theNode->leftMemory->beta[0] = CreateEmptyPartialMatch(theEnv);
            theNode->leftMemory->beta[0]->owner = theNode;
        }
    } else { theNode->leftMemory = nullptr; }

    if (theNode->joinFromTheRight) {
        if (theNode->leftHash == nullptr) {
            theNode->rightMemory = get_struct(theEnv, betaMemory);
            theNode->rightMemory->beta = (PartialMatch **) genalloc(theEnv, sizeof(PartialMatch *));
            theNode->rightMemory->last = (PartialMatch **) genalloc(theEnv, sizeof(PartialMatch *));
            theNode->rightMemory->beta[0] = nullptr;
            theNode->rightMemory->last[0] = nullptr;
            theNode->rightMemory->size = 1;
            theNode->rightMemory->count = 0;
        } else {
            theNode->rightMemory = get_struct(theEnv, betaMemory);
            theNode->rightMemory->beta = (PartialMatch **) genalloc(theEnv, sizeof(PartialMatch *) * INITIAL_BETA_HASH_SIZE);
            theNode->rightMemory->last = (PartialMatch **) genalloc(theEnv, sizeof(PartialMatch *) * INITIAL_BETA_HASH_SIZE);
            memset(theNode->rightMemory->beta, 0, sizeof(PartialMatch **) * INITIAL_BETA_HASH_SIZE);
            memset(theNode->rightMemory->last, 0, sizeof(PartialMatch **) * INITIAL_BETA_HASH_SIZE);
            theNode->rightMemory->size = INITIAL_BETA_HASH_SIZE;
            theNode->rightMemory->count = 0;
        }
    } else if (theNode->rightSideEntryStructure == nullptr) {
        theNode->rightMemory = get_struct(theEnv, betaMemory);
        theNode->rightMemory->beta = (PartialMatch **) genalloc(theEnv, sizeof(PartialMatch *));
        theNode->rightMemory->last = (PartialMatch **) genalloc(theEnv, sizeof(PartialMatch *));
        theNode->rightMemory->beta[0] = CreateEmptyPartialMatch(theEnv);
        theNode->rightMemory->beta[0]->owner = theNode;
        theNode->rightMemory->beta[0]->rhsMemory = true;
        theNode->rightMemory->last[0] = theNode->rightMemory->beta[0];
        theNode->rightMemory->size = 1;
        theNode->rightMemory->count = 1;
    } else { theNode->rightMemory = nullptr; }
}

#endif /* BLOAD_AND_BSAVE */

/*##################################*/
/* Additional Environment Functions */
/*##################################*/

const char *DefruleModule(
        Defrule *theDefrule) {
    return GetConstructModuleName(&theDefrule->header);
}

const char *DefruleName(
        Defrule *theDefrule) {
    return GetConstructNameString(&theDefrule->header);
}

const char *DefrulePPForm(
        Defrule *theDefrule) {
    return GetConstructPPForm(&theDefrule->header);
}


