/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  08/25/16             */
/*                                                     */
/*                    WATCH MODULE                     */
/*******************************************************/


/*************************************************************/
/* Purpose: Support functions for the watch and unwatch      */
/*   commands.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian Dantes                                         */
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
/*      6.40: Added Env prefix to GetEvaluationError and     */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#if DEBUGGING_FUNCTIONS


#include <cstdio>
#include <cstring>

#include "ArgumentAccess.h"
#include "Constants.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "MemoryAllocation.h"
#include "Router.h"

#include "Watch.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static WatchItemRecord *ValidWatchItem(const Environment&, const char *, bool *);
static void DeallocateWatchData(const Environment&);

/**********************************************/
/* InitializeWatchData: Allocates environment */
/*    data for watch items.                   */
/**********************************************/
void InitializeWatchData(
        const Environment&theEnv) {
    AllocateEnvironmentData(theEnv, WATCH_DATA, sizeof(watchData), DeallocateWatchData);
}

/************************************************/
/* DeallocateWatchData: Deallocates environment */
/*    data for watch items.                     */
/************************************************/
static void DeallocateWatchData(
        const Environment&theEnv) {
    WatchItemRecord *tmpPtr, *nextPtr;

    tmpPtr = WatchData(theEnv)->ListOfWatchItems;
    while (tmpPtr != nullptr) {
        nextPtr = tmpPtr->next;
        rtn_struct(theEnv, watchItemRecord, tmpPtr);
        tmpPtr = nextPtr;
    }
}

/*************************************************************/
/* AddWatchItem: Adds an item to the list of watchable items */
/*   that can be set using the watch and unwatch commands.   */
/*   Returns false if the item is already in the list,       */
/*   otherwise returns true.                                 */
/*************************************************************/
bool AddWatchItem(
        const Environment&theEnv,
        const char *name,
        int code,
        bool *flag,
        int priority,
        WatchAccessFunction accessFunc,
        WatchPrintFunction printFunc) {
    WatchItemRecord *newPtr, *currentPtr, *lastPtr;

    /*================================================================*/
    /* Find the insertion point in the watchable items list to place  */
    /* the new item. If the item is already in the list return false. */
    /*================================================================*/

    for (currentPtr = WatchData(theEnv)->ListOfWatchItems, lastPtr = nullptr;
         currentPtr != nullptr;
         currentPtr = currentPtr->next) {
        if (strcmp(currentPtr->name, name) == 0) return false;
        if (priority < currentPtr->priority) lastPtr = currentPtr;
    }

    /*============================*/
    /* Create the new watch item. */
    /*============================*/

    newPtr = get_struct(theEnv, watchItemRecord);
    newPtr->name = name;
    newPtr->flag = flag;
    newPtr->code = code;
    newPtr->priority = priority;
    newPtr->accessFunc = accessFunc;
    newPtr->printFunc = printFunc;

    /*=================================================*/
    /* Insert the new item in the list of watch items. */
    /*=================================================*/

    if (lastPtr == nullptr) {
        newPtr->next = WatchData(theEnv)->ListOfWatchItems;
        WatchData(theEnv)->ListOfWatchItems = newPtr;
    } else {
        newPtr->next = lastPtr->next;
        lastPtr->next = newPtr;
    }

    /*==================================================*/
    /* Return true to indicate the item has been added. */
    /*==================================================*/

    return true;
}

/**************************************************/
/* Watch: C access routine for the watch command. */
/**************************************************/
void Watch(
        const Environment&theEnv,
        WatchItem item) {
    switch (item) {
        case ALL:
            SetWatchItem(theEnv, "all", true, nullptr);
            break;

        case FACTS:
            SetWatchItem(theEnv, "facts", true, nullptr);
            break;

        case INSTANCES:
            SetWatchItem(theEnv, "instances", true, nullptr);
            break;

        case SLOTS:
            SetWatchItem(theEnv, "slots", true, nullptr);
            break;

        case RULES:
            SetWatchItem(theEnv, "rules", true, nullptr);
            break;

        case ACTIVATIONS:
            SetWatchItem(theEnv, "activations", true, nullptr);
            break;

        case MESSAGES:
            SetWatchItem(theEnv, "messages", true, nullptr);
            break;

        case MESSAGE_HANDLERS:
            SetWatchItem(theEnv, "message-handlers", true, nullptr);
            break;

        case GENERIC_FUNCTIONS:
            SetWatchItem(theEnv, "generic-functions", true, nullptr);
            break;

        case METHODS:
            SetWatchItem(theEnv, "methods", true, nullptr);
            break;

        case DEFFUNCTIONS:
            SetWatchItem(theEnv, "deffunctions", true, nullptr);
            break;

        case COMPILATIONS:
            SetWatchItem(theEnv, "compilations", true, nullptr);
            break;

        case STATISTICS:
            SetWatchItem(theEnv, "statistics", true, nullptr);
            break;

        case GLOBALS:
            SetWatchItem(theEnv, "globals", true, nullptr);
            break;

        case FOCUS:
            SetWatchItem(theEnv, "focus", true, nullptr);
            break;
    }
}

/****************************************************/
/* Unwatch: C access routine for the watch command. */
/****************************************************/
void Unwatch(
        const Environment&theEnv,
        WatchItem item) {
    switch (item) {
        case ALL:
            SetWatchItem(theEnv, "all", false, nullptr);
            break;

        case FACTS:
            SetWatchItem(theEnv, "facts", false, nullptr);
            break;

        case INSTANCES:
            SetWatchItem(theEnv, "instances", false, nullptr);
            break;

        case SLOTS:
            SetWatchItem(theEnv, "slots", false, nullptr);
            break;

        case RULES:
            SetWatchItem(theEnv, "rules", false, nullptr);
            break;

        case ACTIVATIONS:
            SetWatchItem(theEnv, "activations", false, nullptr);
            break;

        case MESSAGES:
            SetWatchItem(theEnv, "messages", false, nullptr);
            break;

        case MESSAGE_HANDLERS:
            SetWatchItem(theEnv, "message-handlers", false, nullptr);
            break;

        case GENERIC_FUNCTIONS:
            SetWatchItem(theEnv, "generic-functions", false, nullptr);
            break;

        case METHODS:
            SetWatchItem(theEnv, "methods", false, nullptr);
            break;

        case DEFFUNCTIONS:
            SetWatchItem(theEnv, "deffunctions", false, nullptr);
            break;

        case COMPILATIONS:
            SetWatchItem(theEnv, "compilations", false, nullptr);
            break;

        case STATISTICS:
            SetWatchItem(theEnv, "statistics", false, nullptr);
            break;

        case GLOBALS:
            SetWatchItem(theEnv, "globals", false, nullptr);
            break;

        case FOCUS:
            SetWatchItem(theEnv, "focus", false, nullptr);
            break;
    }
}

/******************************************/
/* GetWatchState: Returns the watch state */
/*   for the specified watch item.        */
/******************************************/
bool GetWatchState(
        const Environment&theEnv,
        WatchItem item) {
    switch (item) {
        case ALL:
            return false;

        case FACTS:
            return (GetWatchItem(theEnv, "facts") == 1);

        case INSTANCES:
            return (GetWatchItem(theEnv, "instances") == 1);

        case SLOTS:
            return (GetWatchItem(theEnv, "slots") == 1);

        case RULES:
            return (GetWatchItem(theEnv, "rules") == 1);

        case ACTIVATIONS:
            return (GetWatchItem(theEnv, "activations") == 1);

        case MESSAGES:
            return (GetWatchItem(theEnv, "messages") == 1);

        case MESSAGE_HANDLERS:
            return (GetWatchItem(theEnv, "message-handlers") == 1);

        case GENERIC_FUNCTIONS:
            return (GetWatchItem(theEnv, "generic-functions") == 1);

        case METHODS:
            return (GetWatchItem(theEnv, "methods") == 1);

        case DEFFUNCTIONS:
            return (GetWatchItem(theEnv, "deffunctions") == 1);

        case COMPILATIONS:
            return (GetWatchItem(theEnv, "compilations") == 1);

        case STATISTICS:
            return (GetWatchItem(theEnv, "statistics") == 1);

        case GLOBALS:
            return (GetWatchItem(theEnv, "globals") == 1);

        case FOCUS:
            return (GetWatchItem(theEnv, "focus") == 1);
    }

    return false;
}

/******************************************/
/* SetWatchState: Returns the watch state */
/*   for the specified watch item.        */
/******************************************/
void SetWatchState(
        const Environment&theEnv,
        WatchItem item,
        bool newState) {
    switch (item) {
        case ALL:
            SetWatchItem(theEnv, "all", newState, nullptr);
            return;

        case FACTS:
            SetWatchItem(theEnv, "facts", newState, nullptr);
            return;

        case INSTANCES:
            SetWatchItem(theEnv, "instances", newState, nullptr);
            return;

        case SLOTS:
            SetWatchItem(theEnv, "slots", newState, nullptr);
            return;

        case RULES:
            SetWatchItem(theEnv, "rules", newState, nullptr);
            return;

        case ACTIVATIONS:
            SetWatchItem(theEnv, "activations", newState, nullptr);
            return;

        case MESSAGES:
            SetWatchItem(theEnv, "messages", newState, nullptr);
            return;

        case MESSAGE_HANDLERS:
            SetWatchItem(theEnv, "message-handlers", newState, nullptr);
            return;

        case GENERIC_FUNCTIONS:
            SetWatchItem(theEnv, "generic-functions", newState, nullptr);
            return;

        case METHODS:
            SetWatchItem(theEnv, "methods", newState, nullptr);
            return;

        case DEFFUNCTIONS:
            SetWatchItem(theEnv, "deffunctions", newState, nullptr);
            return;

        case COMPILATIONS:
            SetWatchItem(theEnv, "compilations", newState, nullptr);
            return;

        case STATISTICS:
            SetWatchItem(theEnv, "statistics", newState, nullptr);
            return;

        case GLOBALS:
            SetWatchItem(theEnv, "globals", newState, nullptr);
            return;

        case FOCUS:
            SetWatchItem(theEnv, "focus", newState, nullptr);
            return;
    }
}

/********************************************************/
/* WatchString: C access routine for the watch command. */
/********************************************************/
bool WatchString(
        const Environment&theEnv,
        const char *itemName) {
    return SetWatchItem(theEnv, itemName, true, nullptr);
}

/************************************************************/
/* UnwatchString: C access routine for the unwatch command. */
/************************************************************/
bool UnwatchString(
        const Environment&theEnv,
        const char *itemName) {
    return SetWatchItem(theEnv, itemName, false, nullptr);
}

/********************************************************************/
/* SetWatchItem: Sets the state of a specified watch item to either */
/*   on or off. Returns true if the item was set, otherwise false.  */
/********************************************************************/
bool SetWatchItem(
        const Environment&theEnv,
        const char *itemName,
        bool newState,
        Expression *argExprs) {
    WatchItemRecord *wPtr;

    /*===================================================*/
    /* If the name of the watch item to set is all, then */
    /* all watch items are set to the new state and true */
    /* is returned.                                      */
    /*===================================================*/

    if (strcmp(itemName, "all") == 0) {
        for (wPtr = WatchData(theEnv)->ListOfWatchItems; wPtr != nullptr; wPtr = wPtr->next) {
            /*==============================================*/
            /* If no specific arguments are specified, then */
            /* set the global flag for the watch item.      */
            /*==============================================*/

            if (argExprs == nullptr) *(wPtr->flag) = newState;

            /*=======================================*/
            /* Set flags for individual watch items. */
            /*=======================================*/

            if ((wPtr->accessFunc == nullptr) ? false :
                !(*wPtr->accessFunc)(theEnv, wPtr->code, newState, argExprs)) {
                SetEvaluationError(theEnv, true);
                return false;
            }
        }
        return true;
    }

    /*=================================================*/
    /* Search for the watch item to be set in the list */
    /* of watch items. If found, set the watch item to */
    /* its new state and return true.                  */
    /*=================================================*/

    for (wPtr = WatchData(theEnv)->ListOfWatchItems; wPtr != nullptr; wPtr = wPtr->next) {
        if (strcmp(itemName, wPtr->name) == 0) {
            /*==============================================*/
            /* If no specific arguments are specified, then */
            /* set the global flag for the watch item.      */
            /*==============================================*/

            if (argExprs == nullptr) *(wPtr->flag) = newState;

            /*=======================================*/
            /* Set flags for individual watch items. */
            /*=======================================*/

            if ((wPtr->accessFunc == nullptr) ? false :
                !(*wPtr->accessFunc)(theEnv, wPtr->code, newState, argExprs)) {
                SetEvaluationError(theEnv, true);
                return false;
            }

            return true;
        }
    }

    /*=================================================*/
    /* If the specified item was not found in the list */
    /* of watchable items then return false.           */
    /*=================================================*/

    return false;
}

/****************************************************************/
/* GetWatchItem: Gets the current state of the specified watch  */
/*   item. Returns the state of the watch item (0 for off and 1 */
/*   for on) if the watch item is found in the list of watch    */
/*   items, otherwise -1 is returned.                           */
/****************************************************************/
int GetWatchItem(
        const Environment&theEnv,
        const char *itemName) {
    WatchItemRecord *wPtr;

    for (wPtr = WatchData(theEnv)->ListOfWatchItems; wPtr != nullptr; wPtr = wPtr->next) {
        if (strcmp(itemName, wPtr->name) == 0) {
            if (*(wPtr->flag)) { return 1; }
            else { return 0; }
        }
    }

    return -1;
}

/***************************************************************/
/* ValidWatchItem: Returns true if the specified name is found */
/*   in the list of watch items, otherwise returns false.      */
/***************************************************************/
static WatchItemRecord *ValidWatchItem(
        const Environment&theEnv,
        const char *itemName,
        bool *recognized) {
    WatchItemRecord *wPtr;

    *recognized = true;
    if (strcmp(itemName, "all") == 0)
        return nullptr;

    for (wPtr = WatchData(theEnv)->ListOfWatchItems; wPtr != nullptr; wPtr = wPtr->next) {
        if (strcmp(itemName, wPtr->name) == 0)
            return (wPtr);
    }

    *recognized = false;
    return nullptr;
}

/*************************************************************/
/* GetNthWatchName: Returns the name associated with the nth */
/*   item in the list of watchable items. If the nth item    */
/*   does not exist, then nullptr is returned.                  */
/*************************************************************/
const char *GetNthWatchName(
        const Environment&theEnv,
        int whichItem) {
    int i;
    WatchItemRecord *wPtr;

    for (wPtr = WatchData(theEnv)->ListOfWatchItems, i = 1;
         wPtr != nullptr;
         wPtr = wPtr->next, i++) { if (i == whichItem) return (wPtr->name); }

    return nullptr;
}

/***************************************************************/
/* GetNthWatchValue: Returns the current state associated with */
/*   the nth item in the list of watchable items. If the nth   */
/*   item does not exist, then -1 is returned.                 */
/***************************************************************/
int GetNthWatchValue(
        const Environment&theEnv,
        int whichItem) {
    int i;
    WatchItemRecord *wPtr;

    for (wPtr = WatchData(theEnv)->ListOfWatchItems, i = 1;
         wPtr != nullptr;
         wPtr = wPtr->next, i++) { if (i == whichItem) return ((int) *(wPtr->flag)); }

    return (-1);
}

/**************************************/
/* WatchCommand: H/L access routine   */
/*   for the watch command.           */
/**************************************/
void WatchCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UDFValue theValue;
    const char *argument;
    bool recognized;
    WatchItemRecord *wPtr;

    /*========================================*/
    /* Determine which item is to be watched. */
    /*========================================*/

    if (!UDFFirstArgument(context, SYMBOL_BIT, &theValue)) return;

    argument = theValue.lexemeValue->contents;
    wPtr = ValidWatchItem(theEnv, argument, &recognized);
    if (!recognized) {
        SetEvaluationError(theEnv, true);
        UDFInvalidArgumentMessage(context, "watchable symbol");
        return;
    }

    /*=================================================*/
    /* Check to make sure extra arguments are allowed. */
    /*=================================================*/

    if (GetNextArgument(GetFirstArgument()) != nullptr) {
        if ((wPtr == nullptr) ? true : (wPtr->accessFunc == nullptr)) {
            SetEvaluationError(theEnv, true);
            ExpectedCountError(theEnv, "watch", EXACTLY, 1);
            return;
        }
    }

    /*=====================*/
    /* Set the watch item. */
    /*=====================*/

    SetWatchItem(theEnv, argument, true, GetNextArgument(GetFirstArgument()));
}

/****************************************/
/* UnwatchCommand: H/L access routine   */
/*   for the unwatch command.           */
/****************************************/
void UnwatchCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UDFValue theValue;
    const char *argument;
    bool recognized;
    WatchItemRecord *wPtr;

    /*==========================================*/
    /* Determine which item is to be unwatched. */
    /*==========================================*/

    if (!UDFFirstArgument(context, SYMBOL_BIT, &theValue)) return;

    argument = theValue.lexemeValue->contents;
    wPtr = ValidWatchItem(theEnv, argument, &recognized);
    if (!recognized) {
        SetEvaluationError(theEnv, true);
        UDFInvalidArgumentMessage(context, "watchable symbol");
        return;
    }

    /*=================================================*/
    /* Check to make sure extra arguments are allowed. */
    /*=================================================*/

    if (GetNextArgument(GetFirstArgument()) != nullptr) {
        if ((wPtr == nullptr) ? true : (wPtr->accessFunc == nullptr)) {
            SetEvaluationError(theEnv, true);
            ExpectedCountError(theEnv, "unwatch", EXACTLY, 1);
            return;
        }
    }

    /*=====================*/
    /* Set the watch item. */
    /*=====================*/

    SetWatchItem(theEnv, argument, false, GetNextArgument(GetFirstArgument()));
}

/************************************************/
/* ListWatchItemsCommand: H/L access routines   */
/*   for the list-watch-items command.          */
/************************************************/
void ListWatchItemsCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    WatchItemRecord *wPtr;
    UDFValue theValue;
    bool recognized;

    /*=======================*/
    /* List the watch items. */
    /*=======================*/

    if (GetFirstArgument() == nullptr) {
        for (wPtr = WatchData(theEnv)->ListOfWatchItems; wPtr != nullptr; wPtr = wPtr->next) {
            WriteString(theEnv, STDOUT, wPtr->name);
            if (*(wPtr->flag)) WriteString(theEnv, STDOUT, " = on\n");
            else WriteString(theEnv, STDOUT, " = off\n");
        }
        return;
    }

    /*=======================================*/
    /* Determine which item is to be listed. */
    /*=======================================*/

    if (!UDFFirstArgument(context, SYMBOL_BIT, &theValue)) return;
    wPtr = ValidWatchItem(theEnv, theValue.lexemeValue->contents, &recognized);
    if (!recognized || (wPtr == nullptr)) {
        SetEvaluationError(theEnv, true);
        ExpectedTypeError1(theEnv, "list-watch-items", 1, "'watchable symbol'");
        return;
    }

    /*=================================================*/
    /* Check to make sure extra arguments are allowed. */
    /*=================================================*/

    if ((wPtr->printFunc == nullptr) &&
        (GetNextArgument(GetFirstArgument()) != nullptr)) {
        SetEvaluationError(theEnv, true);
        ExpectedCountError(theEnv, "list-watch-items", EXACTLY, 1);
        return;
    }

    /*====================================*/
    /* List the status of the watch item. */
    /*====================================*/

    WriteString(theEnv, STDOUT, wPtr->name);
    if (*(wPtr->flag)) WriteString(theEnv, STDOUT, " = on\n");
    else WriteString(theEnv, STDOUT, " = off\n");

    /*============================================*/
    /* List the status of individual watch items. */
    /*============================================*/

    if (wPtr->printFunc != nullptr) {
        if (!(*wPtr->printFunc)(theEnv, STDOUT, wPtr->code,
                                GetNextArgument(GetFirstArgument()))) { SetEvaluationError(theEnv, true); }
    }
}

/*******************************************/
/* GetWatchItemCommand: H/L access routine */
/*   for the get-watch-item command.       */
/*******************************************/
void GetWatchItemCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UDFValue theValue;
    const char *argument;
    bool recognized;

    /*========================================*/
    /* Determine which item is to be watched. */
    /*========================================*/

    if (!UDFFirstArgument(context, SYMBOL_BIT, &theValue)) { return; }

    argument = theValue.lexemeValue->contents;
    ValidWatchItem(theEnv, argument, &recognized);
    if (!recognized) {
        SetEvaluationError(theEnv, true);
        ExpectedTypeError1(theEnv, "get-watch-item", 1, "'watchable symbol'");
        returnValue->lexemeValue = FalseSymbol(theEnv);
        return;
    }

    /*===========================*/
    /* Get the watch item value. */
    /*===========================*/

    if (GetWatchItem(theEnv, argument) == 1) { returnValue->lexemeValue = TrueSymbol(theEnv); }
    else { returnValue->lexemeValue = FalseSymbol(theEnv); }
}

/*************************************************************/
/* WatchFunctionDefinitions: Initializes the watch commands. */
/*************************************************************/
void WatchFunctionDefinitions(
        const Environment&theEnv) {
    AddUDF(theEnv, "watch", "v", 1, UNBOUNDED, "*;y", WatchCommand);
    AddUDF(theEnv, "unwatch", "v", 1, UNBOUNDED, "*;y", UnwatchCommand);
    AddUDF(theEnv, "get-watch-item", "b", 1, 1, "y", GetWatchItemCommand);
    AddUDF(theEnv, "list-watch-items", "v", 0, UNBOUNDED, "*;y", ListWatchItemsCommand);
}

#endif /* DEBUGGING_FUNCTIONS */

