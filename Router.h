/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  04/04/19            */
/*                                                     */
/*                 ROUTER HEADER FILE                  */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides a centralized mechanism for handling    */
/*   input and output requests.                              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed conversion of '\r' to '\n' from the    */
/*            EnvGetcRouter function.                        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added support for passing context information  */
/*            to the router functions.                       */
/*                                                           */
/*      6.30: Fixed issues with passing context to routers.  */
/*                                                           */
/*            Added AwaitingInput flag.                      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Added STDOUT and STDIN logical name            */
/*            definitions.                                   */
/*                                                           */
/*      6.31: Compiler warning fix.                          */
/*                                                           */
/*      6.40: Added InputBufferCount function.               */
/*                                                           */
/*            Added check for reuse of existing router name. */
/*                                                           */
/*            Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Changed return values for router functions.    */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            Callbacks must be environment aware.           */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Removed WPROMPT, WDISPLAY, WTRACE, and WDIALOG */
/*            logical names.                                 */
/*                                                           */
/*************************************************************/

#ifndef _H_router

#pragma once

#define _H_router

#include <cstdio>

typedef struct router Router;
typedef bool RouterQueryFunction(const Environment&, const char *, void *);
typedef void RouterWriteFunction(const Environment&, const char *, const char *, void *);
typedef void RouterExitFunction(const Environment&, int, void *);
typedef int RouterReadFunction(const Environment&, const char *, void *);
typedef int RouterUnreadFunction(const Environment&, const char *, int, void *);

extern const char *STDOUT;
extern const char *STDIN;
extern const char *STDERR;
extern const char *STDWRN;

constexpr auto ROUTER_DATA = 46;

struct router {
    const char *name;
    bool active;
    int priority;
    RouterQueryFunction *queryCallback;
    RouterWriteFunction *writeCallback;
    RouterExitFunction *exitCallback;
    RouterReadFunction *readCallback;
    RouterUnreadFunction *unreadCallback;
    Router *next;
};

struct routerData : public EnvironmentModule {
    size_t CommandBufferInputCount;
    size_t InputUngets;
    bool AwaitingInput;
    const char *LineCountRouter;
    const char *FastCharGetRouter;
    const char *FastCharGetString;
    long FastCharGetIndex;
    struct router *ListOfRouters;
    FILE *FastLoadFilePtr;
    FILE *FastSaveFilePtr;
    bool Abort;
};
RegisterEnvironmentModule(routerData, ROUTER_DATA, Router);

void InitializeDefaultRouters(const Environment&);
void WriteString(const Environment&, const std::string&, const std::string&);
void Write(const Environment&, const char *);
void Writeln(const Environment&, const char *);
int ReadRouter(const Environment&, const char *);
int UnreadRouter(const Environment&, const char *, int);
void ExitRouter(const Environment&, int);
void AbortExit(const Environment&);
bool AddRouter(const Environment&, const char *, int,
               RouterQueryFunction *, RouterWriteFunction *,
               RouterReadFunction *, RouterUnreadFunction *,
               RouterExitFunction *, void *);
bool DeleteRouter(const Environment&, const char *);
bool QueryRouters(const Environment&, const char *);
bool DeactivateRouter(const Environment&, const char *);
bool ActivateRouter(const Environment&, const char *);
void SetFastLoad(const Environment&, FILE *);
void SetFastSave(const Environment&, FILE *);
FILE *GetFastLoad(const Environment&);
FILE *GetFastSave(const Environment&);
void UnrecognizedRouterMessage(const Environment&, const char *);
void PrintNRouter(const Environment&, const char *, const char *, unsigned long);
size_t InputBufferCount(const Environment&);
Router *FindRouter(const Environment&, const char *);
bool PrintRouterExists(const Environment&, const char *);

#endif /* _H_router */
