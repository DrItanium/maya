/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  08/25/16            */
/*                                                     */
/*                 ENGINE HEADER FILE                  */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides functionality primarily associated with */
/*   the run and focus commands.                             */
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
/*            Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*      6.24: Removed DYNAMIC_SALIENCE, INCREMENTAL_RESET,   */
/*            and LOGICAL_DEPENDENCIES compilation flags.    */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added access functions to the HaltRules flag.  */
/*                                                           */
/*            Added EnvGetNextFocus, EnvGetFocusChanged, and */
/*            EnvSetFocusChanged functions.                  */
/*                                                           */
/*      6.30: Added additional developer statistics to help  */
/*            analyze join network performance.              */
/*                                                           */
/*            Removed pseudo-facts used in not CEs.          */
/*                                                           */
/*            Added context information for run functions.   */
/*                                                           */
/*            Added before rule firing callback function.    */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added EnvHalt function.                        */
/*                                                           */
/*            Used gensprintf instead of sprintf.            */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            Incremental reset is always enabled.           */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_engine

#pragma once

#define _H_engine

typedef struct focalModule FocalModule;

#include "LogicalDependencies.h"
#include "Defrule.h"
#include "Network.h"
#include "Defmodule.h"
#include "Retract.h"

struct focalModule {
    Defmodule *theModule;
    struct defruleModule *theDefruleModule;
    FocalModule *next;
};

typedef struct ruleFiredFunctionItem RuleFiredFunctionItem;
typedef void RuleFiredFunction(Environment *, Activation *, void *);

struct ruleFiredFunctionItem {
    const char *name;
    RuleFiredFunction *func;
    int priority;
    RuleFiredFunctionItem *next;
    void *context;
};

#define ENGINE_DATA 18

struct engineData {
    Defrule *ExecutingRule;
    bool HaltRules;
    struct joinNode *TheLogicalJoin;
    struct partialMatch *TheLogicalBind;
    struct dependency *UnsupportedDataEntities;
    bool alreadyEntered;
    RuleFiredFunctionItem *ListOfAfterRuleFiresFunctions;
    RuleFiredFunctionItem *ListOfBeforeRuleFiresFunctions;
    FocalModule *CurrentFocus;
    bool FocusChanged;
#if DEBUGGING_FUNCTIONS
    bool WatchStatistics;
    bool WatchFocus;
#endif
    bool IncrementalResetInProgress;
    bool JoinOperationInProgress;
    struct partialMatch *GlobalLHSBinds;
    struct partialMatch *GlobalRHSBinds;
    struct joinNode *GlobalJoin;
    struct partialMatch *GarbagePartialMatches;
    struct alphaMatch *GarbageAlphaMatches;
    bool AlreadyRunning;
#if DEVELOPER
    long leftToRightComparisons;
    long rightToLeftComparisons;
    long leftToRightSucceeds;
    long rightToLeftSucceeds;
    long leftToRightLoops;
    long rightToLeftLoops;
    long findNextConflictingComparisons;
    long betaHashHTSkips;
    long betaHashListSkips;
    long unneededMarkerCompare;
#endif
};

#define EngineData(theEnv) ((struct engineData *) GetEnvironmentData(theEnv,ENGINE_DATA))

#define MAX_PATTERNS_CHECKED 64

long long Run(Environment *, long long);
bool AddAfterRuleFiresFunction(Environment *, const char *,
                               RuleFiredFunction *, int, void *);
bool RemoveAfterRuleFiresFunction(Environment *, const char *);
bool AddBeforeRuleFiresFunction(Environment *, const char *,
                                RuleFiredFunction *, int, void *);
bool RemoveBeforeRuleFiresFunction(Environment *, const char *);
RuleFiredFunctionItem *AddRuleFiredFunctionToCallList(Environment *, const char *, int, RuleFiredFunction *,
                                                      RuleFiredFunctionItem *, void *);
RuleFiredFunctionItem *RemoveRuleFiredFunctionFromCallList(Environment *, const char *,
                                                           RuleFiredFunctionItem *, bool *);
void DeallocateRuleFiredCallList(Environment *, RuleFiredFunctionItem *);
void InitializeEngine(Environment *);
void SetBreak(Defrule *);
void Halt(Environment *);
bool RemoveBreak(Defrule *);
void RemoveAllBreakpoints(Environment *);
void ShowBreaks(Environment *, const char *, Defmodule *);
bool DefruleHasBreakpoint(Defrule *);
void RunCommand(Environment *, UDFContext *, UDFValue *);
void SetBreakCommand(Environment *, UDFContext *, UDFValue *);
void RemoveBreakCommand(Environment *, UDFContext *, UDFValue *);
void ShowBreaksCommand(Environment *, UDFContext *, UDFValue *);
void HaltCommand(Environment *, UDFContext *, UDFValue *);
void FocusCommand(Environment *, UDFContext *, UDFValue *);
void ClearFocusStackCommand(Environment *, UDFContext *, UDFValue *);
void ClearFocusStack(Environment *);
FocalModule *GetNextFocus(Environment *, FocalModule *);
const char *FocalModuleName(FocalModule *);
Defmodule *FocalModuleModule(FocalModule *);
void Focus(Defmodule *);
bool GetFocusChanged(Environment *);
void SetFocusChanged(Environment *, bool);
void ListFocusStackCommand(Environment *, UDFContext *, UDFValue *);
void ListFocusStack(Environment *, const char *);
void GetFocusStackFunction(Environment *, UDFContext *, UDFValue *);
void GetFocusStack(Environment *, CLIPSValue *);
void PopFocusFunction(Environment *, UDFContext *, UDFValue *);
Defmodule *PopFocus(Environment *);
bool GetHaltRules(Environment *);
void SetHaltRules(Environment *, bool);
Activation *NextActivationToFire(Environment *);

#endif /* _H_engine */






