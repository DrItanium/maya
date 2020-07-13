/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/30/16            */
/*                                                     */
/*            STRING_TYPE I/O ROUTER HEADER FILE            */
/*******************************************************/

/*************************************************************/
/* Purpose: I/O Router routines which allow strings to be    */
/*   used as input and output sources.                       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Used genstrcpy instead of strcpy.              */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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
/*************************************************************/

#ifndef _H_strngrtr

#pragma once

#define _H_strngrtr

typedef struct stringRouter StringRouter;
typedef struct stringBuilderRouter StringBuilderRouter;

#include <cstdio>
#include "Utility.h"

constexpr auto STRING_ROUTER_DATA = 48;

struct stringRouter {
    const char *name;
    const char *readString;
    char *writeString;
    size_t currentPosition;
    size_t maximumPosition;
    int readWriteType;
    StringRouter *next;
};

struct stringBuilderRouter {
    const char *name;
    StringBuilder *SBR;
    StringBuilderRouter *next;
};

struct stringRouterData : public EnvironmentModule {
    StringRouter *ListOfStringRouters;
    StringBuilderRouter *ListOfStringBuilderRouters;
};
RegisterEnvironmentModule(stringRouterData, STRING_ROUTER_DATA, StringRouter);

/**************************/
/* I/O ROUTER DEFINITIONS */
/**************************/

void InitializeStringRouter(const Environment::Ptr&);
bool OpenStringSource(const Environment::Ptr&, const char *, const char *, size_t);
bool OpenTextSource(const Environment::Ptr&, const char *, const char *, size_t, size_t);
bool CloseStringSource(const Environment::Ptr&, const char *);
bool OpenStringDestination(const Environment::Ptr&, const char *, char *, size_t);
bool CloseStringDestination(const Environment::Ptr&, const char *);
bool OpenStringBuilderDestination(const Environment::Ptr&, const char *, StringBuilder *);
bool CloseStringBuilderDestination(const Environment::Ptr&, const char *);

#endif /* _H_strngrtr */


