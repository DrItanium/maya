/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  03/20/19             */
/*                                                     */
/*                  I/O ROUTER MODULE                  */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides a centralized mechanism for handling    */
/*   input and output requests.                              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
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
/*      6.31: Fixed line count issue when using Windows      */
/*            line endings in Unix.                          */
/*                                                           */
/*      6.40: Added InputBufferCount function.               */
/*                                                           */
/*            Added check for reuse of existing router name. */
/*                                                           */
/*            Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
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
/*************************************************************/

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "Setup.h"

#include "ArgumentAccess.h"
#include "Constants.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "File.h"
#include "MemoryAllocation.h"
#include "PrintUtility.h"
#include "Scanner.h"
#include "StringRouter.h"
#include "SystemDependency.h"

#include "Router.h"

/**********************/
/* STRING DEFINITIONS */
/**********************/

const char *STDIN = "stdin";
const char *STDOUT = "stdout";
const char *STDERR = "stderr";
const char *STDWRN = "stdwrn";

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static bool QueryRouter(const Environment::Ptr&, const char *, struct router *);
static void DeallocateRouterData(const Environment::Ptr&);

/*********************************************************/
/* InitializeDefaultRouters: Initializes output streams. */
/*********************************************************/
void InitializeDefaultRouters(
        const Environment::Ptr&theEnv) {
    //AllocateEnvironmentData(theEnv, ROUTER_DATA, sizeof(routerData), DeallocateRouterData);
    theEnv->allocateEnvironmentModule<routerData>();

    RouterData(theEnv)->CommandBufferInputCount = 0;
    RouterData(theEnv)->InputUngets = 0;
    RouterData(theEnv)->AwaitingInput = true;
#if STUBBING_INACTIVE
    InitializeFileRouter(theEnv);
    InitializeStringRouter(theEnv);
#endif
}

/*************************************************/
/* DeallocateRouterData: Deallocates environment */
/*    data for I/O routers.                      */
/*************************************************/
static void DeallocateRouterData(
        const Environment::Ptr&theEnv) {
    struct router *tmpPtr, *nextPtr;

    tmpPtr = RouterData(theEnv)->ListOfRouters;
    while (tmpPtr != nullptr) {
        nextPtr = tmpPtr->next;
        genfree(theEnv, (void *) tmpPtr->name, strlen(tmpPtr->name) + 1);
        rtn_struct(theEnv, router, tmpPtr);
        tmpPtr = nextPtr;
    }
}

/*********************/
/* PrintRouterExists */
/*********************/
bool PrintRouterExists(
        const Environment::Ptr&theEnv,
        const char *logicalName) {
    struct router *currentPtr;

    if (((char *) RouterData(theEnv)->FastSaveFilePtr) == logicalName) { return true; }

    currentPtr = RouterData(theEnv)->ListOfRouters;
    while (currentPtr != nullptr) {
        if ((currentPtr->writeCallback != nullptr) ? QueryRouter(theEnv, logicalName, currentPtr) : false) { return true; }
        currentPtr = currentPtr->next;
    }

    return false;
}

/**********************************/
/* Write: Generic print function. */
/**********************************/
void Write(
        const Environment::Ptr&theEnv,
        const char *str) {
    WriteString(theEnv, STDOUT, str);
}

/************************************/
/* Writeln: Generic print function. */
/************************************/
void Writeln(
        const Environment::Ptr&theEnv,
        const char *str) {
    WriteString(theEnv, STDOUT, str);
    WriteString(theEnv, STDOUT, "\n");
}

/****************************************/
/* WriteString: Generic print function. */
/****************************************/
void WriteString(
        const Environment::Ptr&theEnv,
        const std::string& logicalName,
        const std::string& str) {
#if STUBBING_INACTIVE
    router *currentPtr;
    if (str == nullptr) return;

    /*===================================================*/
    /* If the "fast save" option is being used, then the */
    /* logical name is actually a pointer to a file and  */
    /* fprintf can be called directly to bypass querying */
    /* all of the routers.                               */
    /*===================================================*/

    if (((char *) RouterData(theEnv)->FastSaveFilePtr) == logicalName) {
        fprintf(RouterData(theEnv)->FastSaveFilePtr, "%s", str);
        return;
    }

    /*==============================================*/
    /* Search through the list of routers until one */
    /* is found that will handle the print request. */
    /*==============================================*/

    currentPtr = RouterData(theEnv)->ListOfRouters;
    while (currentPtr != nullptr) {
        if ((currentPtr->writeCallback != nullptr) ? QueryRouter(theEnv, logicalName, currentPtr) : false) {
            (*currentPtr->writeCallback)(theEnv, logicalName, str, nullptr);
            return;
        }
        currentPtr = currentPtr->next;
    }

    /*=====================================================*/
    /* The logical name was not recognized by any routers. */
    /*=====================================================*/

    if (strcmp(STDERR, logicalName) != 0) { UnrecognizedRouterMessage(theEnv, logicalName); }
#endif
}

/***********************************************/
/* ReadRouter: Generic get character function. */
/***********************************************/
int ReadRouter(
        const Environment::Ptr&theEnv,
        const char *logicalName) {
    struct router *currentPtr;
    int inchar;

    /*===================================================*/
    /* If the "fast load" option is being used, then the */
    /* logical name is actually a pointer to a file and  */
    /* getc can be called directly to bypass querying    */
    /* all of the routers.                               */
    /*===================================================*/

    if (((char *) RouterData(theEnv)->FastLoadFilePtr) == logicalName) {
        inchar = getc(RouterData(theEnv)->FastLoadFilePtr);

        if (inchar == '\n') {
            if (((char *) RouterData(theEnv)->FastLoadFilePtr) == RouterData(theEnv)->LineCountRouter) { IncrementLineCount(theEnv); }
        }

        /* if (inchar == '\r') return('\n'); */

        return (inchar);
    }

    /*===============================================*/
    /* If the "fast string get" option is being used */
    /* for the specified logical name, then bypass   */
    /* the router system and extract the character   */
    /* directly from the fast get string.            */
    /*===============================================*/

    if (RouterData(theEnv)->FastCharGetRouter == logicalName) {
        inchar = (unsigned char) RouterData(theEnv)->FastCharGetString[RouterData(theEnv)->FastCharGetIndex];

        RouterData(theEnv)->FastCharGetIndex++;

        if (inchar == '\0') return (EOF);

        if (inchar == '\n') {
            if (RouterData(theEnv)->FastCharGetRouter == RouterData(theEnv)->LineCountRouter) { IncrementLineCount(theEnv); }
        }

        return (inchar);
    }

    /*==============================================*/
    /* Search through the list of routers until one */
    /* is found that will handle the getc request.  */
    /*==============================================*/

    currentPtr = RouterData(theEnv)->ListOfRouters;
    while (currentPtr != nullptr) {
        if ((currentPtr->readCallback != nullptr) ? QueryRouter(theEnv, logicalName, currentPtr) : false) {
            inchar = (*currentPtr->readCallback)(theEnv, logicalName, nullptr);

            if (inchar == '\n') {
                if ((RouterData(theEnv)->LineCountRouter != nullptr) &&
                    (strcmp(logicalName, RouterData(theEnv)->LineCountRouter) == 0)) { IncrementLineCount(theEnv); }
            }

            return (inchar);
        }
        currentPtr = currentPtr->next;
    }

    /*=====================================================*/
    /* The logical name was not recognized by any routers. */
    /*=====================================================*/

    UnrecognizedRouterMessage(theEnv, logicalName);
    return (-1);
}

/***************************************************/
/* UnreadRouter: Generic unget character function. */
/***************************************************/
int UnreadRouter(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        int ch) {
    struct router *currentPtr;

    /*===================================================*/
    /* If the "fast load" option is being used, then the */
    /* logical name is actually a pointer to a file and  */
    /* ungetc can be called directly to bypass querying  */
    /* all of the routers.                               */
    /*===================================================*/

    if (((char *) RouterData(theEnv)->FastLoadFilePtr) == logicalName) {
        if (ch == '\n') {
            if (((char *) RouterData(theEnv)->FastLoadFilePtr) == RouterData(theEnv)->LineCountRouter) { DecrementLineCount(theEnv); }
        }

        return ungetc(ch, RouterData(theEnv)->FastLoadFilePtr);
    }

    /*===============================================*/
    /* If the "fast string get" option is being used */
    /* for the specified logical name, then bypass   */
    /* the router system and unget the character     */
    /* directly from the fast get string.            */
    /*===============================================*/

    if (RouterData(theEnv)->FastCharGetRouter == logicalName) {
        if (ch == '\n') {
            if (RouterData(theEnv)->FastCharGetRouter == RouterData(theEnv)->LineCountRouter) { DecrementLineCount(theEnv); }
        }

        if (RouterData(theEnv)->FastCharGetIndex > 0) RouterData(theEnv)->FastCharGetIndex--;
        return ch;
    }

    /*===============================================*/
    /* Search through the list of routers until one  */
    /* is found that will handle the ungetc request. */
    /*===============================================*/

    currentPtr = RouterData(theEnv)->ListOfRouters;
    while (currentPtr != nullptr) {
        if ((currentPtr->unreadCallback != nullptr) ? QueryRouter(theEnv, logicalName, currentPtr) : false) {
            if (ch == '\n') {
                if ((RouterData(theEnv)->LineCountRouter != nullptr) &&
                    (strcmp(logicalName, RouterData(theEnv)->LineCountRouter) == 0)) { DecrementLineCount(theEnv); }
            }

            return (*currentPtr->unreadCallback)(theEnv, logicalName, ch, nullptr);
        }

        currentPtr = currentPtr->next;
    }

    /*=====================================================*/
    /* The logical name was not recognized by any routers. */
    /*=====================================================*/

    UnrecognizedRouterMessage(theEnv, logicalName);
    return -1;
}

/********************************************/
/* ExitRouter: Generic exit function. Calls */
/*   all of the router exit functions.      */
/********************************************/
void ExitRouter(
        const Environment::Ptr&theEnv,
        int num) {
    struct router *currentPtr, *nextPtr;

    RouterData(theEnv)->Abort = false;
    currentPtr = RouterData(theEnv)->ListOfRouters;
    while (currentPtr != nullptr) {
        nextPtr = currentPtr->next;
        if (currentPtr->active) {
            if (currentPtr->exitCallback != nullptr) {
                (*currentPtr->exitCallback)(theEnv, num, nullptr);
            }
        }
        currentPtr = nextPtr;
    }

    if (RouterData(theEnv)->Abort) return;
    genexit(theEnv, num);
}

/********************************************/
/* AbortExit: Forces ExitRouter to terminate */
/*   after calling all closing routers.     */
/********************************************/
void AbortExit(
        const Environment::Ptr&theEnv) {
    RouterData(theEnv)->Abort = true;
}
/*********************************************************/
/* AddRouter: Adds an I/O router to the list of routers. */
/*********************************************************/
bool AddRouter(
        const Environment::Ptr&theEnv,
        const char *routerName,
        int priority,
        RouterQueryFunction *queryFunction,
        RouterWriteFunction *writeFunction,
        RouterReadFunction *readFunction,
        RouterUnreadFunction *unreadFunction,
        RouterExitFunction *exitFunction,
        void *context) {
#if STUBBING_INACTIVE
    struct router *newPtr, *lastPtr, *currentPtr;
    char *nameCopy;

    /*==================================================*/
    /* Reject the router if the name is already in use. */
    /*==================================================*/

    for (currentPtr = RouterData(theEnv)->ListOfRouters;
         currentPtr != nullptr;
         currentPtr = currentPtr->next) {
        if (strcmp(currentPtr->name, routerName) == 0) { return false; }
    }

    newPtr = get_struct(theEnv, router);

    nameCopy = (char *) genalloc(theEnv, strlen(routerName) + 1);
    genstrcpy(nameCopy, routerName);
    newPtr->name = nameCopy;

    newPtr->active = true;
    newPtr->context = context;
    newPtr->priority = priority;
    newPtr->queryCallback = queryFunction;
    newPtr->writeCallback = writeFunction;
    newPtr->exitCallback = exitFunction;
    newPtr->readCallback = readFunction;
    newPtr->unreadCallback = unreadFunction;
    newPtr->next = nullptr;

    if (RouterData(theEnv)->ListOfRouters == nullptr) {
        RouterData(theEnv)->ListOfRouters = newPtr;
        return true;
    }

    lastPtr = nullptr;
    currentPtr = RouterData(theEnv)->ListOfRouters;
    while ((currentPtr != nullptr) ? (priority < currentPtr->priority) : false) {
        lastPtr = currentPtr;
        currentPtr = currentPtr->next;
    }

    if (lastPtr == nullptr) {
        newPtr->next = RouterData(theEnv)->ListOfRouters;
        RouterData(theEnv)->ListOfRouters = newPtr;
    } else {
        newPtr->next = currentPtr;
        lastPtr->next = newPtr;
    }

    return true;
#endif
    return false;
}

/*****************************************************************/
/* DeleteRouter: Removes an I/O router from the list of routers. */
/*****************************************************************/
bool DeleteRouter(
        const Environment::Ptr&theEnv,
        const char *routerName) {
    struct router *currentPtr, *lastPtr;

    currentPtr = RouterData(theEnv)->ListOfRouters;
    lastPtr = nullptr;

    while (currentPtr != nullptr) {
        if (strcmp(currentPtr->name, routerName) == 0) {
            genfree(theEnv, (void *) currentPtr->name, strlen(currentPtr->name) + 1);
            if (lastPtr == nullptr) {
                RouterData(theEnv)->ListOfRouters = currentPtr->next;
                rm(theEnv, currentPtr, sizeof(router));
                return true;
            }
            lastPtr->next = currentPtr->next;
            rm(theEnv, currentPtr, sizeof(router));
            return true;
        }
        lastPtr = currentPtr;
        currentPtr = currentPtr->next;
    }

    return false;
}

/*********************************************************************/
/* QueryRouters: Determines if any router recognizes a logical name. */
/*********************************************************************/
bool QueryRouters(
        const Environment::Ptr&theEnv,
        const char *logicalName) {
    struct router *currentPtr;

    currentPtr = RouterData(theEnv)->ListOfRouters;
    while (currentPtr != nullptr) {
        if (QueryRouter(theEnv, logicalName, currentPtr)) return true;
        currentPtr = currentPtr->next;
    }

    return false;
}

/************************************************/
/* QueryRouter: Determines if a specific router */
/*    recognizes a logical name.                */
/************************************************/
static bool QueryRouter(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        struct router *currentPtr) {
    /*===================================================*/
    /* If the router is inactive, then it can't respond. */
    /*===================================================*/

    if (!currentPtr->active) { return false; }

    /*=============================================================*/
    /* If the router has no query function, then it can't respond. */
    /*=============================================================*/

    if (currentPtr->queryCallback == nullptr) return false;

    /*=========================================*/
    /* Call the router's query function to see */
    /* if it recognizes the logical name.      */
    /*=========================================*/

    return (*currentPtr->queryCallback)(theEnv, logicalName, nullptr);

}

/*******************************************************/
/* DeactivateRouter: Deactivates a specific router. */
/*******************************************************/
bool DeactivateRouter(
        const Environment::Ptr&theEnv,
        const char *routerName) {
    struct router *currentPtr;

    currentPtr = RouterData(theEnv)->ListOfRouters;

    while (currentPtr != nullptr) {
        if (strcmp(currentPtr->name, routerName) == 0) {
            currentPtr->active = false;
            return true;
        }
        currentPtr = currentPtr->next;
    }

    return false;
}

/************************************************/
/* ActivateRouter: Activates a specific router. */
/************************************************/
bool ActivateRouter(
        const Environment::Ptr&theEnv,
        const char *routerName) {
    struct router *currentPtr;

    currentPtr = RouterData(theEnv)->ListOfRouters;

    while (currentPtr != nullptr) {
        if (strcmp(currentPtr->name, routerName) == 0) {
            currentPtr->active = true;
            return true;
        }
        currentPtr = currentPtr->next;
    }

    return false;
}

/*****************************************/
/* FindRouter: Locates the named router. */
/*****************************************/
Router *FindRouter(
        const Environment::Ptr&theEnv,
        const char *routerName) {
    Router *currentPtr;

    for (currentPtr = RouterData(theEnv)->ListOfRouters;
         currentPtr != nullptr;
         currentPtr = currentPtr->next) {
        if (strcmp(currentPtr->name, routerName) == 0) { return currentPtr; }
    }

    return nullptr;
}

/********************************************************/
/* SetFastLoad: Used to bypass router system for loads. */
/********************************************************/
void SetFastLoad(
        const Environment::Ptr&theEnv,
        FILE *filePtr) {
    RouterData(theEnv)->FastLoadFilePtr = filePtr;
}

/********************************************************/
/* SetFastSave: Used to bypass router system for saves. */
/********************************************************/
void SetFastSave(
        const Environment::Ptr&theEnv,
        FILE *filePtr) {
    RouterData(theEnv)->FastSaveFilePtr = filePtr;
}

/******************************************************/
/* GetFastLoad: Returns the "fast load" file pointer. */
/******************************************************/
FILE *GetFastLoad(
        const Environment::Ptr&theEnv) {
    return (RouterData(theEnv)->FastLoadFilePtr);
}

/******************************************************/
/* GetFastSave: Returns the "fast save" file pointer. */
/******************************************************/
FILE *GetFastSave(
        const Environment::Ptr&theEnv) {
    return (RouterData(theEnv)->FastSaveFilePtr);
}

/*****************************************************/
/* UnrecognizedRouterMessage: Standard error message */
/*   for an unrecognized router name.                */
/*****************************************************/
void UnrecognizedRouterMessage(
        const Environment::Ptr&theEnv,
        const char *logicalName) {
    PrintErrorID(theEnv, "ROUTER", 1, false);
    WriteString(theEnv, STDERR, "Logical name '");
    WriteString(theEnv, STDERR, logicalName);
    WriteString(theEnv, STDERR, "' was not recognized by any routers.\n");
}

/*****************************************/
/* PrintNRouter: Generic print function. */
/*****************************************/
void PrintNRouter(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        const char *str,
        unsigned long length) {
    char *tempStr;

    tempStr = (char *) genalloc(theEnv, length + 1);
    genstrncpy(tempStr, str, length);
    tempStr[length] = 0;
    WriteString(theEnv, logicalName, tempStr);
    genfree(theEnv, tempStr, length + 1);
}

/*********************/
/* InputBufferCount: */
/*********************/
size_t InputBufferCount(
        const Environment::Ptr&theEnv) {
    return RouterData(theEnv)->CommandBufferInputCount;
}
