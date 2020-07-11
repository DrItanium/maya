/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  08/25/16            */
/*                                                     */
/*                  WATCH HEADER FILE                  */
/*******************************************************/

/*************************************************************/
/* Purpose: Support functions for the watch and unwatch      */
/*   commands.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added EnvSetWatchItem function.                */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
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

#ifndef _H_watch

#pragma once

#define _H_watch

#include "Expression.h"

constexpr auto WATCH_DATA = 54;

typedef struct watchItemRecord WatchItemRecord;

enum WatchItem {
    ALL,
    FACTS,
    INSTANCES,
    SLOTS,
    RULES,
    ACTIVATIONS,
    MESSAGES,
    MESSAGE_HANDLERS,
    GENERIC_FUNCTIONS,
    METHODS,
    DEFFUNCTIONS,
    COMPILATIONS,
    STATISTICS,
    GLOBALS,
    FOCUS
};

typedef bool WatchAccessFunction(const Environment&, int, bool, Expression*);
typedef bool WatchPrintFunction(const Environment&, const char*, int, Expression*);
struct watchItemRecord {
    const char *name;
    bool *flag;
    int code, priority;
    WatchAccessFunction* accessFunc;
    WatchPrintFunction* printFunc;
    WatchItemRecord *next;
};

struct watchData {
    WatchItemRecord *ListOfWatchItems;
};
RegisterEnvironmentModule(watchData, WATCH_DATA);
#define WatchData(theEnv) (GetEnvironmentData(theEnv,WATCH_DATA))

void Watch(const Environment&, WatchItem);
void Unwatch(const Environment&, WatchItem);

bool WatchString(const Environment&, const char *);
bool UnwatchString(const Environment&, const char *);
void InitializeWatchData(const Environment&);
bool SetWatchItem(const Environment&, const char *, bool, Expression *);
int GetWatchItem(const Environment&, const char *);
bool AddWatchItem(const Environment& theEnv,
                  const char* name,
                  int code,
                  bool* flag,
                  int priority,
                  WatchAccessFunction accessFunc,
                  WatchPrintFunction printFunc);
const char *GetNthWatchName(const Environment&, int);
int GetNthWatchValue(const Environment&, int);
void WatchCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void UnwatchCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ListWatchItemsCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void WatchFunctionDefinitions(const Environment&);
void GetWatchItemCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool GetWatchState(const Environment&, WatchItem);
void SetWatchState(const Environment&, WatchItem, bool);

#endif /* _H_watch */



