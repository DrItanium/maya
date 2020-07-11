/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/30/16            */
/*                                                     */
/*              COMMAND LINE HEADER FILE               */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of routines for processing        */
/*   commands entered at the top level prompt.               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Refactored several functions and added         */
/*            additional functions for use by an interface   */
/*            layered on top of CLIPS.                       */
/*                                                           */
/*      6.30: Local variables set with the bind function     */
/*            persist until a reset/clear command is issued. */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Metrowerks CodeWarrior (MAC_MCW, IBM_MCW) is   */
/*            no longer supported.                           */
/*                                                           */
/*            UTF-8 support.                                 */
/*                                                           */
/*            Command history and editing support            */
/*                                                           */
/*            Used genstrcpy instead of strcpy.              */
/*                                                           */
/*            Added before command execution callback        */
/*            function.                                      */
/*                                                           */
/*            Fixed RouteCommand return value.               */
/*                                                           */
/*            Added AwaitingInput flag.                      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Refactored code to reduce header dependencies  */
/*            in sysdep.c.                                   */
/*                                                           */
/*            Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_commline

#pragma once

#define _H_commline

constexpr auto COMMANDLINE_DATA = 40;

#include <cstdlib>

typedef void AfterPromptFunction(const Environment&);
typedef bool BeforeCommandExecutionFunction(const Environment&);
typedef void EventFunction(const Environment&);

struct commandLineData {
    bool EvaluatingTopLevelCommand;
    bool HaltCommandLoopBatch;
    Expression *CurrentCommand;
    char *CommandString;
    size_t MaximumCharacters;
    bool ParsingTopLevelCommand;
    const char *BannerString;
    EventFunction *EventCallback;
    AfterPromptFunction *AfterPromptCallback;
    BeforeCommandExecutionFunction *BeforeCommandExecutionCallback;
};
RegisterEnvironmentModule(commandLineData, COMMANDLINE_DATA);
#define CommandLineData(theEnv) (GetEnvironmentData(theEnv,COMMANDLINE_DATA))

void InitializeCommandLineData(const Environment&);
bool ExpandCommandString(const Environment&, int);
void FlushCommandString(const Environment&);
void SetCommandString(const Environment&, const char *);
void AppendCommandString(const Environment&, const char *);
void InsertCommandString(const Environment&, const char *, unsigned);
char *GetCommandString(const Environment&);
int CompleteCommand(const char *);
void CommandLoop(const Environment&);
void CommandLoopBatch(const Environment&);
void CommandLoopBatchDriver(const Environment&);
void PrintPrompt(const Environment&);
void PrintBanner(const Environment&);
void SetAfterPromptFunction(const Environment&, AfterPromptFunction *);
void SetBeforeCommandExecutionFunction(const Environment&, BeforeCommandExecutionFunction *);
bool RouteCommand(const Environment&, const char *, bool);
EventFunction *SetEventFunction(const Environment&, EventFunction *);
bool TopLevelCommand(const Environment&);
void AppendNCommandString(const Environment&, const char *, unsigned);
void SetNCommandString(const Environment&, const char *, unsigned);
const char *GetCommandCompletionString(const Environment&, const char *, size_t);
bool ExecuteIfCommandComplete(const Environment&);
void CommandLoopOnceThenBatch(const Environment&);
bool CommandCompleteAndNotEmpty(const Environment&);
void SetHaltCommandLoopBatch(const Environment&, bool);
bool GetHaltCommandLoopBatch(const Environment&);
void RerouteStdin(const Environment&, int, char *[]);

#endif





