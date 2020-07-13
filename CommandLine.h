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
#include <string>
#include "Environment.h"
#include "Expression.h"
using EventFunction = std::function<void(const Environment::Ptr&)>;
using BeforeCommandExecutionFunction = std::function<bool(const Environment::Ptr&)>;
using AfterPromptFunction = std::function<void(const Environment::Ptr&)>;

struct CommandLineModule : public EnvironmentModule {
public:
    CommandLineModule(Environment& envCallback, const std::string& banner, EventFunction callback);
    bool EvaluatingTopLevelCommand = false;
    bool HaltCommandLoopBatch = false;
    Expression::Ptr CurrentCommand;
    std::string CommandString;
    size_t MaximumCharacters = 0;
    bool ParsingTopLevelCommand = false;
    std::string BannerString;
    EventFunction EventCallback;
    AfterPromptFunction AfterPromptCallback;
    BeforeCommandExecutionFunction BeforeCommandExecutionCallback;
    std::string getCommandString() const noexcept { return CommandString; }
};
RegisterEnvironmentModule(CommandLineModule, COMMANDLINE_DATA, CommandLine);

void InitializeCommandLineData(const Environment::Ptr&);
bool ExpandCommandString(const Environment::Ptr&, int);
void FlushCommandString(const Environment::Ptr&);
void SetCommandString(const Environment::Ptr&, const char *);
void AppendCommandString(const Environment::Ptr&, const char *);
void InsertCommandString(const Environment::Ptr&, const char *, unsigned);
std::string GetCommandString(const Environment::Ptr&);
int CompleteCommand(const char *);
void CommandLoop(const Environment::Ptr&);
void CommandLoopBatch(const Environment::Ptr&);
void CommandLoopBatchDriver(const Environment::Ptr&);
void PrintPrompt(const Environment::Ptr&);
void PrintBanner(const Environment::Ptr&);
void SetAfterPromptFunction(const Environment::Ptr&, AfterPromptFunction *);
void SetBeforeCommandExecutionFunction(const Environment::Ptr&, BeforeCommandExecutionFunction *);
bool RouteCommand(const Environment::Ptr&, const char *, bool);
EventFunction *SetEventFunction(const Environment::Ptr&, EventFunction *);
bool TopLevelCommand(const Environment::Ptr&);
void AppendNCommandString(const Environment::Ptr&, const char *, unsigned);
void SetNCommandString(const Environment::Ptr&, const char *, unsigned);
const char *GetCommandCompletionString(const Environment::Ptr&, const char *, size_t);
bool ExecuteIfCommandComplete(const Environment::Ptr&);
void CommandLoopOnceThenBatch(const Environment::Ptr&);
bool CommandCompleteAndNotEmpty(const Environment::Ptr&);
void SetHaltCommandLoopBatch(const Environment::Ptr&, bool);
bool GetHaltCommandLoopBatch(const Environment::Ptr&);
void RerouteStdin(const Environment::Ptr&, int, char *[]);

#endif





