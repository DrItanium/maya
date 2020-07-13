/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  07/30/16             */
/*                                                     */
/*              STRING_TYPE I/O ROUTER MODULE               */
/*******************************************************/

/*************************************************************/
/* Purpose: I/O Router routines which allow strings to be    */
/*   used as input and output sources.                       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
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
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Changed return values for router functions.    */
/*                                                           */
/*************************************************************/

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "Setup.h"

#include "Constants.h"
#include "Environment.h"
#include "MemoryAllocation.h"
#include "PrintUtility.h"
#include "Router.h"
#include "SystemDependency.h"

#include "StringRouter.h"

#define READ_STRING 0
#define WRITE_STRING 1

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static bool QueryStringCallback(const Environment::Ptr&, const char *, void *);
static void WriteStringCallback(const Environment::Ptr&, const char *, const char *, void *);
static int ReadStringCallback(const Environment::Ptr&, const char *, void *);
static int UnreadStringCallback(const Environment::Ptr&, const char *, int, void *);
static StringRouter *FindStringRouter(const Environment::Ptr&, const char *);
static bool CreateReadStringSource(const Environment::Ptr&, const char *, const char *, size_t, size_t);
static void DeallocateStringRouterData(const Environment::Ptr&);
static StringBuilderRouter *FindStringBuilderRouter(const Environment::Ptr&, const char *);
static bool QueryStringBuilderCallback(const Environment::Ptr&, const char *, void *);
static void WriteStringBuilderCallback(const Environment::Ptr&, const char *, const char *, void *);

/**********************************************************/
/* InitializeStringRouter: Initializes string I/O router. */
/**********************************************************/
void InitializeStringRouter(
        const Environment::Ptr&theEnv) {
    //AllocateEnvironmentData(theEnv, STRING_ROUTER_DATA, sizeof(stringRouterData), DeallocateStringRouterData);
    theEnv->allocateEnvironmentModule<stringRouterData>();

    AddRouter(theEnv, "string", 0, QueryStringCallback, WriteStringCallback, ReadStringCallback, UnreadStringCallback, nullptr, nullptr);
    AddRouter(theEnv, "stringBuilder", 0, QueryStringBuilderCallback, WriteStringBuilderCallback, nullptr, nullptr, nullptr, nullptr);
}

/*******************************************/
/* DeallocateStringRouterData: Deallocates */
/*    environment data for string routers. */
/*******************************************/
static void DeallocateStringRouterData(
        const Environment::Ptr&theEnv) {
    StringRouter *tmpPtr, *nextPtr;
    StringBuilderRouter *tmpSBPtr, *nextSBPtr;

    tmpPtr = StringRouterData(theEnv)->ListOfStringRouters;
    while (tmpPtr != nullptr) {
        nextPtr = tmpPtr->next;
        rm(theEnv, (void *) tmpPtr->name, strlen(tmpPtr->name) + 1);
        rtn_struct(theEnv, stringRouter, tmpPtr);
        tmpPtr = nextPtr;
    }

    tmpSBPtr = StringRouterData(theEnv)->ListOfStringBuilderRouters;
    while (tmpSBPtr != nullptr) {
        nextSBPtr = tmpSBPtr->next;
        rm(theEnv, (void *) tmpSBPtr->name, strlen(tmpSBPtr->name) + 1);
        rtn_struct(theEnv, stringBuilderRouter, tmpSBPtr);
        tmpSBPtr = nextSBPtr;
    }
}

/*********************************************************************/
/* QueryStringCallback: Find routine for string router logical names. */
/*********************************************************************/
static bool QueryStringCallback(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        void *context) {
    struct stringRouter *head;

    head = StringRouterData(theEnv)->ListOfStringRouters;
    while (head != nullptr) {
        if (strcmp(head->name, logicalName) == 0) { return true; }
        head = head->next;
    }

    return false;
}

/**********************************************************/
/* WriteStringCallback: Print routine for string routers. */
/**********************************************************/
static void WriteStringCallback(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        const char *str,
        void *context) {
    struct stringRouter *head;

    head = FindStringRouter(theEnv, logicalName);
    if (head == nullptr) {
        SystemError(theEnv, "ROUTER", 3);
        ExitRouter(theEnv, EXIT_FAILURE);
        return;
    }

    if (head->readWriteType != WRITE_STRING) return;

    if (head->maximumPosition == 0) return;

    if ((head->currentPosition + 1) >= head->maximumPosition) return;

    genstrncpy(&head->writeString[head->currentPosition],
               str, (STD_SIZE) (head->maximumPosition - head->currentPosition) - 1);

    head->currentPosition += strlen(str);
}

/********************************************************/
/* ReadStringCallback: Getc routine for string routers. */
/********************************************************/
static int ReadStringCallback(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        void *context) {
    struct stringRouter *head;
    int rc;

    head = FindStringRouter(theEnv, logicalName);
    if (head == nullptr) {
        SystemError(theEnv, "ROUTER", 1);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    if (head->readWriteType != READ_STRING) return (EOF);
    if (head->currentPosition >= head->maximumPosition) {
        head->currentPosition++;
        return (EOF);
    }

    rc = (unsigned char) head->readString[head->currentPosition];
    head->currentPosition++;

    return (rc);
}

/************************************************************/
/* UnreadStringCallback: Ungetc routine for string routers. */
/************************************************************/
static int UnreadStringCallback(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        int ch,
        void *context) {
    struct stringRouter *head;
#if MAC_XCD
#pragma unused(ch)
#endif

    head = FindStringRouter(theEnv, logicalName);

    if (head == nullptr) {
        SystemError(theEnv, "ROUTER", 2);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    if (head->readWriteType != READ_STRING) return 0;
    if (head->currentPosition > 0) { head->currentPosition--; }

    return 1;
}

/************************************************/
/* OpenStringSource: Opens a new string router. */
/************************************************/
bool OpenStringSource(
        const Environment::Ptr&theEnv,
        const char *name,
        const char *str,
        size_t currentPosition) {
    size_t maximumPosition;

    if (str == nullptr) {
        currentPosition = 0;
        maximumPosition = 0;
    } else { maximumPosition = strlen(str); }

    return (CreateReadStringSource(theEnv, name, str, currentPosition, maximumPosition));
}

/******************************************************/
/* OpenTextSource: Opens a new string router for text */
/*   (which is not nullptr terminated).                  */
/******************************************************/
bool OpenTextSource(
        const Environment::Ptr&theEnv,
        const char *name,
        const char *str,
        size_t currentPosition,
        size_t maximumPosition) {
    if (str == nullptr) {
        currentPosition = 0;
        maximumPosition = 0;
    }

    return CreateReadStringSource(theEnv, name, str, currentPosition, maximumPosition);
}

/******************************************************************/
/* CreateReadStringSource: Creates a new string router for input. */
/******************************************************************/
static bool CreateReadStringSource(
        const Environment::Ptr&theEnv,
        const char *name,
        const char *str,
        size_t currentPosition,
        size_t maximumPosition) {
#if STUBBING_INACTIVE
    struct stringRouter *newStringRouter;
    char *theName;

    if (FindStringRouter(theEnv, name) != nullptr) return false;

    newStringRouter = get_struct(theEnv, stringRouter);
    theName = (char *) gm1(theEnv, strlen(name) + 1);
    genstrcpy(theName, name);
    newStringRouter->name = theName;
    newStringRouter->writeString = nullptr;
    newStringRouter->readString = str;
    newStringRouter->currentPosition = currentPosition;
    newStringRouter->readWriteType = READ_STRING;
    newStringRouter->maximumPosition = maximumPosition;
    newStringRouter->next = StringRouterData(theEnv)->ListOfStringRouters;
    StringRouterData(theEnv)->ListOfStringRouters = newStringRouter;

    return true;
#endif
    return false;
}

/**********************************************/
/* CloseStringSource: Closes a string router. */
/**********************************************/
bool CloseStringSource(
        const Environment::Ptr&theEnv,
        const char *name) {
    struct stringRouter *head, *last;

    last = nullptr;
    head = StringRouterData(theEnv)->ListOfStringRouters;
    while (head != nullptr) {
        if (strcmp(head->name, name) == 0) {
            if (last == nullptr) {
                StringRouterData(theEnv)->ListOfStringRouters = head->next;
                rm(theEnv, (void *) head->name, strlen(head->name) + 1);
                rtn_struct(theEnv, stringRouter, head);
                return true;
            } else {
                last->next = head->next;
                rm(theEnv, (void *) head->name, strlen(head->name) + 1);
                rtn_struct(theEnv, stringRouter, head);
                return true;
            }
        }
        last = head;
        head = head->next;
    }

    return false;
}

/******************************************************************/
/* OpenStringDestination: Opens a new string router for printing. */
/******************************************************************/
bool OpenStringDestination(
        const Environment::Ptr&theEnv,
        const char *name,
        char *str,
        size_t maximumPosition) {
#if STUBBING_INACTIVE
    struct stringRouter *newStringRouter;
    char *theName;

    if (FindStringRouter(theEnv, name) != nullptr) return false;

    newStringRouter = get_struct(theEnv, stringRouter);
    theName = (char *) gm1(theEnv, strlen(name) + 1);
    genstrcpy(theName, name);
    newStringRouter->name = theName;
    newStringRouter->readString = nullptr;
    newStringRouter->writeString = str;
    newStringRouter->currentPosition = 0;
    newStringRouter->readWriteType = WRITE_STRING;
    newStringRouter->maximumPosition = maximumPosition;
    newStringRouter->next = StringRouterData(theEnv)->ListOfStringRouters;
    StringRouterData(theEnv)->ListOfStringRouters = newStringRouter;

    return true;
#endif
    return false;
}

/***************************************************/
/* CloseStringDestination: Closes a string router. */
/***************************************************/
bool CloseStringDestination(
        const Environment::Ptr&theEnv,
        const char *name) {
    return CloseStringSource(theEnv, name);
}

/*******************************************************************/
/* FindStringRouter: Returns a pointer to the named string router. */
/*******************************************************************/
static struct stringRouter *FindStringRouter(
        const Environment::Ptr&theEnv,
        const char *name) {
    struct stringRouter *head;

    head = StringRouterData(theEnv)->ListOfStringRouters;
    while (head != nullptr) {
        if (strcmp(head->name, name) == 0) { return (head); }
        head = head->next;
    }

    return nullptr;
}

/*********************************************/
/* OpenStringBuilderDestination: Opens a new */
/*   StringBuilder router for printing.      */
/*********************************************/
bool OpenStringBuilderDestination(
        const Environment::Ptr&theEnv,
        const char *name,
        StringBuilder *theSB) {
#if STUBBING_INACTIVE
    StringBuilderRouter *newStringRouter;
    char *theName;

    if (FindStringBuilderRouter(theEnv, name) != nullptr) return false;

    newStringRouter = get_struct(theEnv, stringBuilderRouter);
    theName = (char *) gm1(theEnv, strlen(name) + 1);
    genstrcpy(theName, name);
    newStringRouter->name = theName;
    newStringRouter->SBR = theSB;
    newStringRouter->next = StringRouterData(theEnv)->ListOfStringBuilderRouters;
    StringRouterData(theEnv)->ListOfStringBuilderRouters = newStringRouter;

    return true;
#endif
    return false;
}

/*****************************************/
/* CloseStringBuilderDestination: Closes */
/*   a StringBuilder router.             */
/*****************************************/
bool CloseStringBuilderDestination(
        const Environment::Ptr&theEnv,
        const char *name) {
    StringBuilderRouter *head, *last;

    last = nullptr;
    head = StringRouterData(theEnv)->ListOfStringBuilderRouters;
    while (head != nullptr) {
        if (strcmp(head->name, name) == 0) {
            if (last == nullptr) {
                StringRouterData(theEnv)->ListOfStringBuilderRouters = head->next;
                rm(theEnv, (void *) head->name, strlen(head->name) + 1);
                rtn_struct(theEnv, stringBuilderRouter, head);
                return true;
            } else {
                last->next = head->next;
                rm(theEnv, (void *) head->name, strlen(head->name) + 1);
                rtn_struct(theEnv, stringBuilderRouter, head);
                return true;
            }
        }
        last = head;
        head = head->next;
    }

    return false;
}

/**********************************************/
/* FindStringBuilderRouter: Returns a pointer */
/*   to the named StringBuilder router.       */
/**********************************************/
static struct stringBuilderRouter *FindStringBuilderRouter(
        const Environment::Ptr&theEnv,
        const char *name) {
    StringBuilderRouter *head;

    head = StringRouterData(theEnv)->ListOfStringBuilderRouters;
    while (head != nullptr) {
        if (strcmp(head->name, name) == 0) { return head; }
        head = head->next;
    }

    return nullptr;
}

/*********************************************/
/* QueryStringBuilderCallback: Query routine */
/*   for stringBuilder router logical names. */
/*********************************************/
static bool QueryStringBuilderCallback(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        void *context) {
    StringBuilderRouter *head;

    head = StringRouterData(theEnv)->ListOfStringBuilderRouters;
    while (head != nullptr) {
        if (strcmp(head->name, logicalName) == 0) { return true; }
        head = head->next;
    }

    return false;
}

/*********************************************/
/* WriteStringBuilderCallback: Print routine */
/*    for stringBuilder routers.             */
/*********************************************/
static void WriteStringBuilderCallback(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        const char *str,
        void *context) {
    StringBuilderRouter *head;

    head = FindStringBuilderRouter(theEnv, logicalName);
    if (head == nullptr) {
        SystemError(theEnv, "ROUTER", 3);
        ExitRouter(theEnv, EXIT_FAILURE);
        return;
    }

    SBAppend(head->SBR, str);
}


