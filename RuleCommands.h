/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  08/25/16            */
/*                                                     */
/*             DEFRULE COMMANDS HEADER FILE            */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides the matches command. Also provides the  */
/*   the developer commands show-joins and rule-complexity.  */
/*   Also provides the initialization routine which          */
/*   registers rule commands found in other modules.         */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed CONFLICT_RESOLUTION_STRATEGIES         */
/*            INCREMENTAL_RESET, and LOGICAL_DEPENDENCIES    */
/*            compilation flags.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added support for hashed memories.             */
/*                                                           */
/*            Improvements to matches command.               */
/*                                                           */
/*            Add join-activity and join-activity-reset      */
/*            commands.                                      */
/*                                                           */
/*            Added get-beta-memory-resizing and             */
/*            set-beta-memory-resizing functions.            */
/*                                                           */
/*            Added timetag function.                        */
/*                                                           */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_rulecom

#pragma once

#define _H_rulecom

#include "Evaluation.h"

struct joinInformation {
    unsigned short whichCE;
    struct joinNode *theJoin;
    int patternBegin;
    int patternEnd;
    int marked;
    struct betaMemory *theMemory;
    struct joinNode *nextJoin;
};

enum Verbosity {
    VERBOSE,
    SUCCINCT,
    TERSE
};

bool GetBetaMemoryResizing(const Environment::Ptr&);
bool SetBetaMemoryResizing(const Environment::Ptr&, bool);
void GetBetaMemoryResizingCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SetBetaMemoryResizingCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void Matches(Defrule::Ptr , Verbosity, CLIPSValue *);
void JoinActivity(const Environment::Ptr&, Defrule::Ptr , int, UDFValue *);
void DefruleCommands(const Environment::Ptr&);
void MatchesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void JoinActivityCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void TimetagFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
unsigned short AlphaJoinCount(const Environment::Ptr&, Defrule::Ptr );
unsigned short BetaJoinCount(const Environment::Ptr&, Defrule::Ptr );
struct joinInformation *CreateJoinArray(const Environment::Ptr&, unsigned short);
void FreeJoinArray(const Environment::Ptr&, struct joinInformation *, unsigned short);
void AlphaJoins(const Environment::Ptr&, Defrule::Ptr , unsigned short, struct joinInformation *);
void BetaJoins(const Environment::Ptr&, Defrule::Ptr , unsigned short, struct joinInformation *);
void JoinActivityResetCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GetFocusFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
Defmodule *GetFocus(const Environment::Ptr&);
#if DEVELOPER
void                           ShowJoinsCommand(const Environment::Ptr&,UDFContext *,UDFValue *);
void                           RuleComplexityCommand(const Environment::Ptr&,UDFContext *,UDFValue *);
void                           ShowAlphaHashTable(const Environment::Ptr&,UDFContext *,UDFValue *);
#endif

#endif /* _H_rulecom */
