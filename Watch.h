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

#define WATCH_DATA 54

typedef struct watchItemRecord WatchItemRecord;

typedef enum {
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
} WatchItem;

typedef bool WatchAccessFunction(Environment*, int, bool, struct expr*);
typedef bool WatchPrintFunction(Environment*, const char*, int, struct expr*);
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

#define WatchData(theEnv) ((struct watchData *) GetEnvironmentData(theEnv,WATCH_DATA))

void Watch(Environment *, WatchItem);
void Unwatch(Environment *, WatchItem);

bool WatchString(Environment *, const char *);
bool UnwatchString(Environment *, const char *);
void InitializeWatchData(Environment *);
bool SetWatchItem(Environment *, const char *, bool, struct expr *);
int GetWatchItem(Environment *, const char *);
bool AddWatchItem(Environment* theEnv,
                  const char* name,
                  int code,
                  bool* flag,
                  int priority,
                  WatchAccessFunction accessFunc,
                  WatchPrintFunction printFunc);
const char *GetNthWatchName(Environment *, int);
int GetNthWatchValue(Environment *, int);
void WatchCommand(Environment *theEnv, UDFContext *context, UDFValue *ret);
void UnwatchCommand(Environment *theEnv, UDFContext *context, UDFValue *ret);
void ListWatchItemsCommand(Environment *theEnv, UDFContext *context, UDFValue *ret);
void WatchFunctionDefinitions(Environment *);
void GetWatchItemCommand(Environment *theEnv, UDFContext *context, UDFValue *ret);
bool GetWatchState(Environment *, WatchItem);
void SetWatchState(Environment *, WatchItem, bool);

#endif /* _H_watch */



