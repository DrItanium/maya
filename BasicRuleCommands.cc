/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  07/02/18             */
/*                                                     */
/*          DEFRULE BASIC COMMANDS HEADER FILE         */
/*******************************************************/

/*************************************************************/
/* Purpose: Implements core commands for the defrule         */
/*   construct such as clear, reset, save, undefrule,        */
/*   ppdefrule, list-defrules, and                           */
/*   get-defrule-list.                                       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*            Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Support for join network changes.              */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Added code to prevent a clear command from     */
/*            being executed during fact assertions via      */
/*            JoinOperationInProgress mechanism.             */
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
/*            Pretty print functions accept optional logical */
/*            name argument.                                 */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#include <cstdio>

#include "ArgumentAccess.h"
#include "Construct.h"
#include "Drive.h"
#include "Engine.h"
#include "Environment.h"
#include "ExternalFunctions.hxx"
#include "Multifield.h"
#include "ReteUtility.h"
#include "Router.h"
#include "Defrule.h"
#include "Watch.h"

#if BLOAD_AND_BSAVE
#include "RuleBinarySaveLoad.h"
#endif

#include "BasicRuleCommands.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void ResetDefrules(const Environment::Ptr&, void *);
static void ResetDefrulesPrime(const Environment::Ptr&, void *);
static void SaveDefrules(const Environment::Ptr&, Defmodule *, const char *, void *);
static bool ClearDefrulesReady(const Environment::Ptr&, void *);
static void ClearDefrules(const Environment::Ptr&, void *);

/*************************************************************/
/* DefruleBasicCommands: Initializes basic defrule commands. */
/*************************************************************/
void DefruleBasicCommands(
        const Environment::Ptr&theEnv) {
    AddResetFunction(theEnv, "defrule", ResetDefrules, 70);
    AddResetFunction(theEnv, "defrule", ResetDefrulesPrime, 10);
    AddSaveFunction(theEnv, "defrule", SaveDefrules, 0);
    AddClearReadyFunction(theEnv, "defrule", ClearDefrulesReady, 0);
    AddClearFunction(theEnv, "defrule", ClearDefrules, 0);

#if DEBUGGING_FUNCTIONS
    AddWatchItem(theEnv, "rules", 0, &DefruleData(theEnv)->WatchRules, 70, DefruleWatchAccess, DefruleWatchPrint);
#endif

    AddUDF(theEnv, "get-defrule-list", "m", 0, 1, "y", GetDefruleListFunction);
    AddUDF(theEnv, "undefrule", "v", 1, 1, "y", UndefruleCommand);
    AddUDF(theEnv, "defrule-module", "y", 1, 1, "y", DefruleModuleFunction);

#if DEBUGGING_FUNCTIONS
    AddUDF(theEnv, "rules", "v", 0, 1, "y", ListDefrulesCommand);
    AddUDF(theEnv, "list-defrules", "v", 0, 1, "y", ListDefrulesCommand);
    AddUDF(theEnv, "ppdefrule", "vs", 1, 2, ";y;ldsyn", PPDefruleCommand);
#endif

#if BLOAD_AND_BSAVE
    DefruleBinarySetup(theEnv);
#endif

}

/*****************************************************/
/* ResetDefrules: Defrule reset routine for use with */
/*   the reset command. Sets the current entity time */
/*   tag (used by the conflict resolution strategies */
/*   for recency) to zero. The focus stack is also   */
/*   cleared.                                        */
/*****************************************************/
static void ResetDefrules(
        const Environment::Ptr&theEnv,
        void *context) {
    Defmodule *theModule;
    struct joinLink *theLink;
    PartialMatch *notParent;

    DefruleData(theEnv)->CurrentEntityTimeTag = 1L;
    ClearFocusStack(theEnv);
    theModule = FindDefmodule(theEnv, "MAIN");
    Focus(theModule);

    for (theLink = DefruleData(theEnv)->RightPrimeJoins;
         theLink != nullptr;
         theLink = theLink->next) { PosEntryRetractAlpha(theEnv, theLink->join->rightMemory->beta[0], NETWORK_ASSERT); }

    for (theLink = DefruleData(theEnv)->LeftPrimeJoins;
         theLink != nullptr;
         theLink = theLink->next) {
        if ((theLink->join->patternIsNegated || theLink->join->joinFromTheRight) &&
            (!theLink->join->patternIsExists)) {
            notParent = theLink->join->leftMemory->beta[0];

            if (notParent->marker) { RemoveBlockedLink(notParent); }

            /*==========================================================*/
            /* Prevent any retractions from generating partial matches. */
            /*==========================================================*/

            notParent->marker = notParent;

            if (notParent->children != nullptr) { PosEntryRetractBeta(theEnv, notParent, notParent->children, NETWORK_ASSERT); }
            /*
          if (notParent->dependents != nullptr)
            { RemoveLogicalSupport(theEnv,notParent); } */
        }
    }
}

/***********************/
/* ResetDefrulesPrime: */
/***********************/
static void ResetDefrulesPrime(
        const Environment::Ptr&theEnv,
        void *context) {
    struct joinLink *theLink;
    PartialMatch *notParent;

    for (theLink = DefruleData(theEnv)->RightPrimeJoins;
         theLink != nullptr;
         theLink = theLink->next) { NetworkAssert(theEnv, theLink->join->rightMemory->beta[0], theLink->join); }

    for (theLink = DefruleData(theEnv)->LeftPrimeJoins;
         theLink != nullptr;
         theLink = theLink->next) {
        if ((theLink->join->patternIsNegated || theLink->join->joinFromTheRight) &&
            (!theLink->join->patternIsExists)) {
            notParent = theLink->join->leftMemory->beta[0];

            if (theLink->join->secondaryNetworkTest != nullptr) {
                if (!EvaluateSecondaryNetworkTest(theEnv, notParent, theLink->join)) { continue; }
            }

            notParent->marker = nullptr;

            EPMDrive(theEnv, notParent, theLink->join, NETWORK_ASSERT);
        }
    }

}


/******************************************************************/
/* ClearDefrulesReady: Indicates whether defrules can be cleared. */
/******************************************************************/
static bool ClearDefrulesReady(
        const Environment::Ptr&theEnv,
        void *context) {
    if (EngineData(theEnv)->ExecutingRule != nullptr) return false;

    if (EngineData(theEnv)->JoinOperationInProgress) return false;

    ClearFocusStack(theEnv);
    if (GetCurrentModule(theEnv) == nullptr) return false;

    DefruleData(theEnv)->CurrentEntityTimeTag = 1L;

    return true;
}

/***************************************************************/
/* ClearDefrules: Pushes the MAIN module as the current focus. */
/***************************************************************/
static void ClearDefrules(
        const Environment::Ptr&theEnv,
        void *context) {
    Defmodule *theModule;

    theModule = FindDefmodule(theEnv, "MAIN");
    Focus(theModule);
}


/**************************************/
/* SaveDefrules: Defrule save routine */
/*   for use with the save command.   */
/**************************************/
static void SaveDefrules(
        const Environment::Ptr&theEnv,
        Defmodule *theModule,
        const char *logicalName,
        void *context) {
    SaveConstruct(theEnv, theModule, logicalName, DefruleData(theEnv)->DefruleConstruct);
}

/******************************************/
/* UndefruleCommand: H/L access routine   */
/*   for the undefrule command.           */
/******************************************/
void UndefruleCommand(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UndefconstructCommand(context, "undefrule", DefruleData(theEnv)->DefruleConstruct);
}

/********************************/
/* Undefrule: C access routine  */
/*   for the undefrule command. */
/********************************/
bool Undefrule(
        Defrule::Ptr theDefrule,
        const Environment::Ptr&allEnv) {
    Environment theEnv;

    if (theDefrule == nullptr) {
        theEnv = allEnv;
        return Undefconstruct(theEnv, nullptr, DefruleData(theEnv)->DefruleConstruct);
    } else {
        theEnv = theDefrule->header.env;
        return Undefconstruct(theEnv, &theDefrule->header, DefruleData(theEnv)->DefruleConstruct);
    }
}

/************************************************/
/* GetDefruleListFunction: H/L access routine   */
/*   for the get-defrule-list function.         */
/************************************************/
void GetDefruleListFunction(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    GetConstructListFunction(context, returnValue, DefruleData(theEnv)->DefruleConstruct);
}

/****************************************/
/* GetDefruleList: C access routine     */
/*   for the get-defrule-list function. */
/****************************************/
void GetDefruleList(
        const Environment::Ptr&theEnv,
        CLIPSValue *returnValue,
        Defmodule *theModule) {
    UDFValue result;

    GetConstructList(theEnv, &result, DefruleData(theEnv)->DefruleConstruct, theModule);
    NormalizeMultifield(theEnv, &result);
    returnValue->value = result.value;
}

/*********************************************/
/* DefruleModuleFunction: H/L access routine */
/*   for the defrule-module function.        */
/*********************************************/
void DefruleModuleFunction(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    returnValue->value = GetConstructModuleCommand(context, "defrule-module", DefruleData(theEnv)->DefruleConstruct);
}

#if DEBUGGING_FUNCTIONS

/******************************************/
/* PPDefruleCommand: H/L access routine   */
/*   for the ppdefrule command.           */
/******************************************/
void PPDefruleCommand(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    PPConstructCommand(context, "ppdefrule", DefruleData(theEnv)->DefruleConstruct, returnValue);
}

/***********************************/
/* PPDefrule: C access routine for */
/*   the ppdefrule command.        */
/***********************************/
bool PPDefrule(
        const Environment::Ptr&theEnv,
        const char *defruleName,
        const char *logicalName) {
    return (PPConstruct(theEnv, defruleName, logicalName, DefruleData(theEnv)->DefruleConstruct));
}

/*********************************************/
/* ListDefrulesCommand: H/L access routine   */
/*   for the list-defrules command.          */
/*********************************************/
void ListDefrulesCommand(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    ListConstructCommand(context, DefruleData(theEnv)->DefruleConstruct);
}

/************************************/
/* ListDefrules: C access routine   */
/*   for the list-defrules command. */
/************************************/
void ListDefrules(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        Defmodule *theModule) {
    ListConstruct(theEnv, DefruleData(theEnv)->DefruleConstruct, logicalName, theModule);
}

/*******************************************************/
/* DefruleGetWatchActivations: C access routine for    */
/*   retrieving the current watch value of a defrule's */
/*   activations.                                      */
/*******************************************************/
bool DefruleGetWatchActivations(
        Defrule::Ptr rulePtr) {
    Defrule::Ptr thePtr;

    for (thePtr = rulePtr;
         thePtr != nullptr;
         thePtr = thePtr->disjunct) { if (thePtr->watchActivation) return true; }

    return false;
}

/********************************************/
/* DefruleGetWatchFirings: C access routine */
/*   for retrieving the current watch value */
/*   of a defrule's firings.                */
/********************************************/
bool DefruleGetWatchFirings(
        Defrule::Ptr rulePtr) {
    Defrule::Ptr thePtr;

    for (thePtr = rulePtr;
         thePtr != nullptr;
         thePtr = thePtr->disjunct) { if (thePtr->watchFiring) return true; }

    return false;
}

/************************************************/
/* DefruleSetWatchActivations: C access routine */
/*   for setting the current watch value of a   */
/*   defrule's activations.                     */
/************************************************/
void DefruleSetWatchActivations(
        Defrule::Ptr rulePtr,
        bool newState) {
    Defrule::Ptr thePtr;

    for (thePtr = rulePtr;
         thePtr != nullptr;
         thePtr = thePtr->disjunct) { thePtr->watchActivation = newState; }
}

/********************************************/
/* DefruleSetWatchFirings: C access routine */
/*   for setting the current watch value of */
/*   a defrule's firings.                   */
/********************************************/
void DefruleSetWatchFirings(
        Defrule::Ptr rulePtr,
        bool newState) {
    Defrule::Ptr thePtr;

    for (thePtr = rulePtr;
         thePtr != nullptr;
         thePtr = thePtr->disjunct) { thePtr->watchFiring = newState; }
}

/*******************************************************************/
/* DefruleWatchAccess: Access function for setting the watch flags */
/*   associated with rules (activations and rule firings).         */
/*******************************************************************/
bool DefruleWatchAccess(
        const Environment::Ptr&theEnv,
        int code,
        bool newState,
        Expression *argExprs) {
    if (code)
        return (ConstructSetWatchAccess(theEnv, DefruleData(theEnv)->DefruleConstruct, newState, argExprs,
                                        (ConstructGetWatchFunction *) DefruleGetWatchActivations,
                                        (ConstructSetWatchFunction *) DefruleSetWatchActivations));
    else
        return (ConstructSetWatchAccess(theEnv, DefruleData(theEnv)->DefruleConstruct, newState, argExprs,
                                        (ConstructGetWatchFunction *) DefruleGetWatchFirings,
                                        (ConstructSetWatchFunction *) DefruleSetWatchFirings));
}

/*****************************************************************/
/* DefruleWatchPrint: Access routine for printing which defrules */
/*   have their watch flag set via the list-watch-items command. */
/*****************************************************************/
bool DefruleWatchPrint(
        const Environment::Ptr&theEnv,
        const char *logName,
        int code,
        Expression *argExprs) {
    if (code)
        return (ConstructPrintWatchAccess(theEnv, DefruleData(theEnv)->DefruleConstruct, logName, argExprs,
                                          (ConstructGetWatchFunction *) DefruleGetWatchActivations,
                                          (ConstructSetWatchFunction *) DefruleSetWatchActivations));
    else
        return (ConstructPrintWatchAccess(theEnv, DefruleData(theEnv)->DefruleConstruct, logName, argExprs,
                                          (ConstructGetWatchFunction *) DefruleGetWatchActivations,
                                          (ConstructSetWatchFunction *) DefruleGetWatchActivations));
}

#endif /* DEBUGGING_FUNCTIONS */

