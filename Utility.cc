/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  05/03/19             */
/*                                                     */
/*                   UTILITY MODULE                    */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of utility functions useful to    */
/*   other modules. Primarily these are the functions for    */
/*   handling periodic garbage collection and appending      */
/*   string data.                                            */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian Dantes                                         */
/*      Jeff Bezanson                                        */
/*         www.cprogramming.com/tutorial/unicode.html        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Added CopyString, DeleteString,                */
/*            InsertInString,and EnlargeString functions.    */
/*                                                           */
/*            Used genstrncpy function instead of strncpy    */
/*            function.                                      */
/*                                                           */
/*            Support for typed EXTERNAL_ADDRESS.            */
/*                                                           */
/*            Support for tracked memory (allows memory to   */
/*            be freed if CLIPS is exited while executing).  */
/*                                                           */
/*            Added UTF-8 routines.                          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.31: Added debugging code for checking the garbage  */
/*            frame.                                         */
/*                                                           */
/*      6.40: Fix for memory used discrepancy.               */
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
/*            Callbacks must be environment aware.           */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Added GCBlockStart and GCBlockEnd functions    */
/*            for garbage collection blocks.                 */
/*                                                           */
/*            Added StringBuilder functions.                 */
/*                                                           */
/*            Moved BufferedRead and FreeReadBuffer from     */
/*            insfile.c to utility.c                         */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#include <ctype.h>
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include "CommandLine.h"
#include "Environment.h"
#include "Evaluation.h"
#include "Fact.h"
#include "MemoryAllocation.h"
#include "Multifield.h"
#include "PrintUtility.h"
#include "Router.h"
#include "Symbol.h"
#include "SystemDependency.h"

#include "Utility.h"

constexpr auto MAX_EPHEMERAL_COUNT = 1000L;
constexpr auto MAX_EPHEMERAL_SIZE = 10240L;
constexpr auto COUNT_INCREMENT = 1000L;
constexpr auto SIZE_INCREMENT = 10240L;

constexpr auto MAX_BLOCK_SIZE = 10240;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void DeallocateUtilityData(const Environment&);

/************************************************/
/* InitializeUtilityData: Allocates environment */
/*    data for utility routines.                */
/************************************************/
void InitializeUtilityData(
        const Environment&theEnv) {
    //AllocateEnvironmentData(theEnv, UTILITY_DATA, sizeof(utilityData), DeallocateUtilityData);
    theEnv->allocateEnvironmentModule<utilityData>();

    UtilityData(theEnv)->CurrentGarbageFrame = &UtilityData(theEnv)->MasterGarbageFrame;

    UtilityData(theEnv)->PeriodicFunctionsEnabled = true;
    UtilityData(theEnv)->YieldFunctionEnabled = true;
}
#if 0
/**************************************************/
/* DeallocateUtilityData: Deallocates environment */
/*    data for utility routines.                  */
/**************************************************/
static void DeallocateUtilityData(
        const Environment&theEnv) {
    struct trackedMemory *tmpTM, *nextTM;
    struct garbageFrame *theGarbageFrame;
    struct ephemeron *edPtr, *nextEDPtr;
    Multifield *tmpMFPtr, *nextMFPtr;

    /*======================*/
    /* Free tracked memory. */
    /*======================*/

    tmpTM = UtilityData(theEnv)->trackList;
    while (tmpTM != nullptr) {
        nextTM = tmpTM->next;
        genfree(theEnv, tmpTM->theMemory, tmpTM->memSize);
        rtn_struct(theEnv, trackedMemory, tmpTM);
        tmpTM = nextTM;
    }

    /*==========================*/
    /* Free callback functions. */
    /*==========================*/

    DeallocateVoidCallList(theEnv, UtilityData(theEnv)->ListOfPeriodicFunctions);
    DeallocateVoidCallList(theEnv, UtilityData(theEnv)->ListOfCleanupFunctions);

    /*=========================================*/
    /* Free the ephemerons tracking data which */
    /* needs to be garbage collected.          */
    /*=========================================*/

    while (UtilityData(theEnv)->CurrentGarbageFrame != nullptr) {
        theGarbageFrame = UtilityData(theEnv)->CurrentGarbageFrame;

        edPtr = theGarbageFrame->ephemeralSymbolList;

        while (edPtr != nullptr) {
            nextEDPtr = edPtr->next;
            rtn_struct(theEnv, ephemeron, edPtr);
            edPtr = nextEDPtr;
        }

        edPtr = theGarbageFrame->ephemeralFloatList;

        while (edPtr != nullptr) {
            nextEDPtr = edPtr->next;
            rtn_struct(theEnv, ephemeron, edPtr);
            edPtr = nextEDPtr;
        }

        edPtr = theGarbageFrame->ephemeralIntegerList;

        while (edPtr != nullptr) {
            nextEDPtr = edPtr->next;
            rtn_struct(theEnv, ephemeron, edPtr);
            edPtr = nextEDPtr;
        }

        edPtr = theGarbageFrame->ephemeralBitMapList;

        while (edPtr != nullptr) {
            nextEDPtr = edPtr->next;
            rtn_struct(theEnv, ephemeron, edPtr);
            edPtr = nextEDPtr;
        }

        edPtr = theGarbageFrame->ephemeralExternalAddressList;

        while (edPtr != nullptr) {
            nextEDPtr = edPtr->next;
            rtn_struct(theEnv, ephemeron, edPtr);
            edPtr = nextEDPtr;
        }

        /*==========================*/
        /* Free up multifield data. */
        /*==========================*/

        tmpMFPtr = theGarbageFrame->ListOfMultifields;
        while (tmpMFPtr != nullptr) {
            nextMFPtr = tmpMFPtr->next;
            ReturnMultifield(theEnv, tmpMFPtr);
            tmpMFPtr = nextMFPtr;
        }

        UtilityData(theEnv)->CurrentGarbageFrame = UtilityData(theEnv)->CurrentGarbageFrame->priorFrame;
    }
}
#endif
/*****************************/
/* CleanCurrentGarbageFrame: */
/*****************************/
void CleanCurrentGarbageFrame(
        const Environment&theEnv,
        UDFValue *returnValue) {
#if 0
    struct garbageFrame *currentGarbageFrame;

    currentGarbageFrame = UtilityData(theEnv)->CurrentGarbageFrame;

    if (!currentGarbageFrame->dirty) return;

    if (returnValue != nullptr) { RetainUDFV(theEnv, returnValue); }

    CallCleanupFunctions(theEnv);
    RemoveEphemeralAtoms(theEnv);
    FlushMultifields(theEnv);

    if (returnValue != nullptr) { ReleaseUDFV(theEnv, returnValue); }

    if ((currentGarbageFrame->ephemeralFloatList == nullptr) &&
        (currentGarbageFrame->ephemeralIntegerList == nullptr) &&
        (currentGarbageFrame->ephemeralSymbolList == nullptr) &&
        (currentGarbageFrame->ephemeralBitMapList == nullptr) &&
        (currentGarbageFrame->ephemeralExternalAddressList == nullptr) &&
        (currentGarbageFrame->LastMultifield == nullptr)) { currentGarbageFrame->dirty = false; }
#endif
}
#if 0

/*****************************/
/* RestorePriorGarbageFrame: */
/*****************************/
void RestorePriorGarbageFrame(
        const Environment&theEnv,
        struct garbageFrame *newGarbageFrame,
        struct garbageFrame *oldGarbageFrame,
        UDFValue *returnValue) {
    if (newGarbageFrame->dirty) {
        if (returnValue != nullptr) RetainUDFV(theEnv, returnValue);
        CallCleanupFunctions(theEnv);
        RemoveEphemeralAtoms(theEnv);
        FlushMultifields(theEnv);
    }

    UtilityData(theEnv)->CurrentGarbageFrame = oldGarbageFrame;

    if (newGarbageFrame->dirty) {
        if (newGarbageFrame->ListOfMultifields != nullptr) {
            if (oldGarbageFrame->ListOfMultifields == nullptr) { oldGarbageFrame->ListOfMultifields = newGarbageFrame->ListOfMultifields; }
            else { oldGarbageFrame->LastMultifield->next = newGarbageFrame->ListOfMultifields; }

            oldGarbageFrame->LastMultifield = newGarbageFrame->LastMultifield;
            oldGarbageFrame->dirty = true;
        }

        if (returnValue != nullptr) ReleaseUDFV(theEnv, returnValue);
    }

    if (returnValue != nullptr) { EphemerateValue(theEnv, returnValue->value); }
}
#endif
/*****************/
/* GCBlockStart: */
/*****************/
void GCBlockStart(
        const Environment&theEnv,
        GCBlock *theBlock) {
#if 0
    theBlock->oldGarbageFrame = UtilityData(theEnv)->CurrentGarbageFrame;
    memset(&theBlock->newGarbageFrame, 0, sizeof(garbageFrame));
    theBlock->newGarbageFrame.priorFrame = theBlock->oldGarbageFrame;
    UtilityData(theEnv)->CurrentGarbageFrame = &theBlock->newGarbageFrame;
#endif
}

/***************/
/* GCBlockEnd: */
/***************/
void GCBlockEnd(
        const Environment&theEnv,
        GCBlock *theBlock) {
#if 0
    RestorePriorGarbageFrame(theEnv, &theBlock->newGarbageFrame, theBlock->oldGarbageFrame, nullptr);
#endif
}
#if 0
/******************/
/* GCBlockEndUDF: */
/******************/
void GCBlockEndUDF(
        const Environment&theEnv,
        GCBlock *theBlock,
        UDFValue *rv) {
    RestorePriorGarbageFrame(theEnv, &theBlock->newGarbageFrame, theBlock->oldGarbageFrame, rv);
}

/*******************************/
/* CurrentGarbageFrameIsDirty: */
/*******************************/
bool CurrentGarbageFrameIsDirty(
        const Environment&theEnv) {
    struct garbageFrame *cgf;

    cgf = UtilityData(theEnv)->CurrentGarbageFrame;

    return cgf->dirty;
}

/*************************/
/* CallCleanupFunctions: */
/*************************/
void CallCleanupFunctions(
        const Environment&theEnv) {
    struct voidCallFunctionItem *cleanupPtr;

    for (cleanupPtr = UtilityData(theEnv)->ListOfCleanupFunctions;
         cleanupPtr != nullptr;
         cleanupPtr = cleanupPtr->next) { (*cleanupPtr->func)(theEnv, nullptr); }
}

#endif
/**************************************************/
/* CallPeriodicTasks: Calls the list of functions */
/*   for handling periodic tasks.                 */
/**************************************************/
void CallPeriodicTasks(
        const Environment&theEnv) {
#if 0
    struct voidCallFunctionItem *periodPtr;

    if (UtilityData(theEnv)->PeriodicFunctionsEnabled) {
        for (periodPtr = UtilityData(theEnv)->ListOfPeriodicFunctions;
             periodPtr != nullptr;
             periodPtr = periodPtr->next) { (*periodPtr->func)(theEnv, nullptr); }
    }
#endif
}
#if 0

/***************************************************/
/* AddCleanupFunction: Adds a function to the list */
/*   of functions called to perform cleanup such   */
/*   as returning free memory to the memory pool.  */
/***************************************************/
bool AddCleanupFunction(
        const Environment&theEnv,
        const char *name,
        VoidCallFunction *theFunction,
        int priority,
        void *context) {
    UtilityData(theEnv)->ListOfCleanupFunctions =
            AddVoidFunctionToCallList(theEnv, name, priority, theFunction,
                                      UtilityData(theEnv)->ListOfCleanupFunctions, context);
    return true;
}

/****************************************************/
/* AddPeriodicFunction: Adds a function to the list */
/*   of functions called to handle periodic tasks.  */
/****************************************************/
bool AddPeriodicFunction(
        const Environment&theEnv,
        const char *name,
        VoidCallFunction *theFunction,
        int priority,
        void *context) {
    UtilityData(theEnv)->ListOfPeriodicFunctions =
            AddVoidFunctionToCallList(theEnv, name, priority, theFunction,
                                      UtilityData(theEnv)->ListOfPeriodicFunctions, context);
    return true;
}

/***************************************************/
/* GetPeriodicFunctionContext: Returns the context */
/*   associated with a periodic function.          */
/***************************************************/
void *GetPeriodicFunctionContext(
        const Environment&theEnv,
        const char *name) {
    return nullptr;
}

/*******************************************************/
/* RemoveCleanupFunction: Removes a function from the  */
/*   list of functions called to perform cleanup such  */
/*   as returning free memory to the memory pool.      */
/*******************************************************/
bool RemoveCleanupFunction(
        const Environment&theEnv,
        const char *name) {
    bool found;

    UtilityData(theEnv)->ListOfCleanupFunctions =
            RemoveVoidFunctionFromCallList(theEnv, name, UtilityData(theEnv)->ListOfCleanupFunctions, &found);

    return found;
}

/********************************************************/
/* RemovePeriodicFunction: Removes a function from the  */
/*   list of functions called to handle periodic tasks. */
/********************************************************/
bool RemovePeriodicFunction(
        const Environment&theEnv,
        const char *name) {
    bool found;

    UtilityData(theEnv)->ListOfPeriodicFunctions =
            RemoveVoidFunctionFromCallList(theEnv, name, UtilityData(theEnv)->ListOfPeriodicFunctions, &found);

    return found;
}

#endif
#if 0
/*****************************************************/
/* StringPrintForm: Generates printed representation */
/*   of a string. Replaces / with // and " with /".  */
/*****************************************************/
const char *StringPrintForm(
        const Environment&theEnv,
        const char *str) {
    int i = 0;
    size_t pos = 0;
    size_t max = 0;
    char *theString = nullptr;
    CLIPSLexeme *thePtr;

    theString = ExpandStringWithChar(theEnv, '"', theString, &pos, &max, max + 80);
    while (str[i] != EOS) {
        if ((str[i] == '"') || (str[i] == '\\')) {
            theString = ExpandStringWithChar(theEnv, '\\', theString, &pos, &max, max + 80);
            theString = ExpandStringWithChar(theEnv, str[i], theString, &pos, &max, max + 80);
        } else { theString = ExpandStringWithChar(theEnv, str[i], theString, &pos, &max, max + 80); }
        i++;
    }

    theString = ExpandStringWithChar(theEnv, '"', theString, &pos, &max, max + 80);

    thePtr = CreateString(theEnv, theString);
    rm(theEnv, theString, max);

    return thePtr->contents;
}
#endif
/**************************************************************/
/* CopyString: Copies a string using CLIPS memory management. */
/**************************************************************/
char *CopyString(
        const Environment&theEnv,
        const char *theString) {
#if 0
    char *stringCopy = nullptr;

    if (theString != nullptr) {
        stringCopy = (char *) genalloc(theEnv, strlen(theString) + 1);
        genstrcpy(stringCopy, theString);
    }

    return stringCopy;
#endif
    return nullptr;
}
/*****************************************************************/
/* DeleteString: Deletes a string using CLIPS memory management. */
/*****************************************************************/
void DeleteString(
        const Environment&theEnv,
        char *theString) {
    //if (theString != nullptr) { genfree(theEnv, theString, strlen(theString) + 1); }
}
#if 0

/***********************************************************/
/* AppendStrings: Appends two strings together. The string */
/*   created is added to the SymbolTable, so it is not     */
/*   necessary to deallocate the string returned.          */
/***********************************************************/
const char *AppendStrings(
        const Environment&theEnv,
        const char *str1,
        const char *str2) {
    size_t pos = 0;
    size_t max = 0;
    char *theString = nullptr;
    CLIPSLexeme *thePtr;

    theString = AppendToString(theEnv, str1, theString, &pos, &max);
    theString = AppendToString(theEnv, str2, theString, &pos, &max);

    thePtr = CreateString(theEnv, theString);
    rm(theEnv, theString, max);
    return thePtr->contents;
}
#endif
/******************************************************/
/* AppendToString: Appends a string to another string */
/*   (expanding the other string if necessary).       */
/******************************************************/
char *AppendToString(
        const Environment&theEnv,
        const char *appendStr,
        char *oldStr,
        size_t *oldPos,
        size_t *oldMax) {
#if 0
    size_t length;

    /*=========================================*/
    /* Expand the old string so it can contain */
    /* the new string (if necessary).          */
    /*=========================================*/

    length = strlen(appendStr);

    /*==============================================================*/
    /* Return nullptr if the old string was not successfully expanded. */
    /*==============================================================*/

    if ((oldStr = EnlargeString(theEnv, length, oldStr, oldPos, oldMax)) == nullptr) { return nullptr; }

    /*===============================================*/
    /* Append the new string to the expanded string. */
    /*===============================================*/

    genstrcpy(&oldStr[*oldPos], appendStr);
    *oldPos += length;

    /*============================================================*/
    /* Return the expanded string containing the appended string. */
    /*============================================================*/

    return oldStr;
#endif
    return nullptr;
}
/**********************************************************/
/* InsertInString: Inserts a string within another string */
/*   (expanding the other string if necessary).           */
/**********************************************************/
char *InsertInString(
        const Environment&theEnv,
        const char *insertStr,
        size_t position,
        char *oldStr,
        size_t *oldPos,
        size_t *oldMax) {
#if 0
    size_t length;

    /*=========================================*/
    /* Expand the old string so it can contain */
    /* the new string (if necessary).          */
    /*=========================================*/

    length = strlen(insertStr);

    /*==============================================================*/
    /* Return nullptr if the old string was not successfully expanded. */
    /*==============================================================*/

    if ((oldStr = EnlargeString(theEnv, length, oldStr, oldPos, oldMax)) == nullptr) { return nullptr; }

    /*================================================================*/
    /* Shift the contents to the right of insertion point so that the */
    /* new text does not overwrite what is currently in the string.   */
    /*================================================================*/

    memmove(&oldStr[position], &oldStr[position + length], *oldPos - position);

    /*===============================================*/
    /* Insert the new string in the expanded string. */
    /*===============================================*/

    genstrncpy(&oldStr[*oldPos], insertStr, length);
    *oldPos += length;

    /*============================================================*/
    /* Return the expanded string containing the appended string. */
    /*============================================================*/

    return (oldStr);
#endif
    return nullptr;
}

#if 0
/*******************************************************************/
/* EnlargeString: Enlarges a string by the specified amount.       */
/*******************************************************************/
char *EnlargeString(
        const Environment&theEnv,
        size_t length,
        char *oldStr,
        size_t *oldPos,
        size_t *oldMax) {
    size_t newMax;

    /*=========================================*/
    /* Expand the old string so it can contain */
    /* the new string (if necessary).          */
    /*=========================================*/

    if (length + *oldPos + 1 > *oldMax) {
        newMax = length + *oldPos + 1;
        if (newMax < sizeof(char *)) { newMax = sizeof(char *); }

        oldStr = (char *) genrealloc(theEnv, oldStr, *oldMax, newMax);

        *oldMax = newMax;
    }

    /*==============================================================*/
    /* Return nullptr if the old string was not successfully expanded. */
    /*==============================================================*/

    if (oldStr == nullptr) { return nullptr; }

    return (oldStr);
}
#endif
/*******************************************************/
/* AppendNToString: Appends a string to another string */
/*   (expanding the other string if necessary). Only a */
/*   specified number of characters are appended from  */
/*   the string.                                       */
/*******************************************************/
char *AppendNToString(
        const Environment&theEnv,
        const char *appendStr,
        char *oldStr,
        size_t length,
        size_t *oldPos,
        size_t *oldMax) {
#if 0
    size_t lengthWithEOS;
    size_t newSize;

    /*====================================*/
    /* Determine the number of characters */
    /* to be appended from the string.    */
    /*====================================*/

    if (appendStr[length - 1] != '\0') lengthWithEOS = length + 1;
    else lengthWithEOS = length;

    /*=========================================*/
    /* Expand the old string so it can contain */
    /* the new string (if necessary).          */
    /*=========================================*/

    if (lengthWithEOS + *oldPos > *oldMax) {
        newSize = *oldPos + lengthWithEOS;
        if (newSize < sizeof(char *)) { newSize = sizeof(char *); }

        oldStr = (char *) genrealloc(theEnv, oldStr, *oldMax, newSize);
        *oldMax = newSize;
    }

    /*==============================================================*/
    /* Return nullptr if the old string was not successfully expanded. */
    /*==============================================================*/

    if (oldStr == nullptr) { return nullptr; }

    /*==================================*/
    /* Append N characters from the new */
    /* string to the expanded string.   */
    /*==================================*/

    genstrncpy(&oldStr[*oldPos], appendStr, length);
    *oldPos += (lengthWithEOS - 1);
    oldStr[*oldPos] = '\0';

    /*============================================================*/
    /* Return the expanded string containing the appended string. */
    /*============================================================*/

    return (oldStr);
#endif
    return nullptr;
}
/*******************************************************/
/* ExpandStringWithChar: Adds a character to a string, */
/*   reallocating space for the string if it needs to  */
/*   be enlarged. The backspace character causes the   */
/*   size of the string to reduced if it is "added" to */
/*   the string.                                       */
/*******************************************************/
char *ExpandStringWithChar(
        const Environment&theEnv,
        int inchar,
        char *str,
        size_t *pos,
        size_t *max,
        size_t newSize) {
#if 0
    if ((*pos + 1) >= *max) {
        if (newSize < sizeof(char *)) { newSize = sizeof(char *); }
        str = (char *) genrealloc(theEnv, str, *max, newSize);
        *max = newSize;
    }

    if (inchar != '\b') {
        str[*pos] = (char) inchar;
        (*pos)++;
        str[*pos] = '\0';
    } else {
        /*===========================================================*/
        /* First delete any UTF-8 multibyte continuation characters. */
        /*===========================================================*/

        while ((*pos > 1) && IsUTF8MultiByteContinuation(str[*pos - 1])) { (*pos)--; }

        /*===================================================*/
        /* Now delete the first byte of the UTF-8 character. */
        /*===================================================*/

        if (*pos > 0) (*pos)--;
        str[*pos] = '\0';
    }

    return (str);
#endif
    return nullptr;
}
#if 0

/**********************************************************/
/* AddVoidFunctionToCallList: Adds a function to a list   */
/*   of functions which are called to perform certain     */
/*   operations (e.g. clear, reset, and bload functions). */
/**********************************************************/
struct voidCallFunctionItem *AddVoidFunctionToCallList(
        const Environment&theEnv,
        const char *name,
        int priority,
        VoidCallFunction *func,
        struct voidCallFunctionItem *head,
        void *context) {
    struct voidCallFunctionItem *newPtr, *currentPtr, *lastPtr = nullptr;
    char *nameCopy;

    newPtr = get_struct(theEnv, voidCallFunctionItem);

    nameCopy = (char *) genalloc(theEnv, strlen(name) + 1);
    genstrcpy(nameCopy, name);
    newPtr->name = nameCopy;

    newPtr->func = func;
    newPtr->priority = priority;
    newPtr->context = context;

    if (head == nullptr) {
        newPtr->next = nullptr;
        return (newPtr);
    }

    currentPtr = head;
    while ((currentPtr != nullptr) ? (priority < currentPtr->priority) : false) {
        lastPtr = currentPtr;
        currentPtr = currentPtr->next;
    }

    if (lastPtr == nullptr) {
        newPtr->next = head;
        head = newPtr;
    } else {
        newPtr->next = currentPtr;
        lastPtr->next = newPtr;
    }

    return (head);
}

/**********************************************************/
/* AddBoolFunctionToCallList: Adds a function to a list   */
/*   of functions which are called to perform certain     */
/*   operations (e.g. clear, reset, and bload functions). */
/**********************************************************/
BoolCallFunctionItem *AddBoolFunctionToCallList(
        const Environment&theEnv,
        const char *name,
        int priority,
        BoolCallFunction *func,
        BoolCallFunctionItem *head,
        void *context) {
    struct boolCallFunctionItem *newPtr, *currentPtr, *lastPtr = nullptr;
    char *nameCopy;

    newPtr = get_struct(theEnv, boolCallFunctionItem);

    nameCopy = (char *) genalloc(theEnv, strlen(name) + 1);
    genstrcpy(nameCopy, name);
    newPtr->name = nameCopy;

    newPtr->func = func;
    newPtr->priority = priority;
    newPtr->context = context;

    if (head == nullptr) {
        newPtr->next = nullptr;
        return (newPtr);
    }

    currentPtr = head;
    while ((currentPtr != nullptr) ? (priority < currentPtr->priority) : false) {
        lastPtr = currentPtr;
        currentPtr = currentPtr->next;
    }

    if (lastPtr == nullptr) {
        newPtr->next = head;
        head = newPtr;
    } else {
        newPtr->next = currentPtr;
        lastPtr->next = newPtr;
    }

    return (head);
}

/****************************************************************/
/* GetVoidFunctionFromCallList: Retrieves a function from a list of */
/*   functions which are called to perform certain operations   */
/*   (e.g. clear, reset, and bload functions).                  */
/****************************************************************/
struct voidCallFunctionItem *GetVoidFunctionFromCallList(
        const Environment&theEnv,
        const char *name,
        struct voidCallFunctionItem *head) {
    struct voidCallFunctionItem *currentPtr;

    for (currentPtr = head; currentPtr != nullptr; currentPtr = currentPtr->next) {
        if (strcmp(name, currentPtr->name) == 0) { return currentPtr; }
    }

    return nullptr;
}

/****************************************************************/
/* GetBoolFunctionFromCallList: Retrieves a function from a list of */
/*   functions which are called to perform certain operations   */
/*   (e.g. clear, reset, and bload functions).                  */
/****************************************************************/
struct boolCallFunctionItem *GetBoolFunctionFromCallList(
        const Environment&theEnv,
        const char *name,
        struct boolCallFunctionItem *head) {
    struct boolCallFunctionItem *currentPtr;

    for (currentPtr = head; currentPtr != nullptr; currentPtr = currentPtr->next) {
        if (strcmp(name, currentPtr->name) == 0) { return currentPtr; }
    }

    return nullptr;
}

/******************************************************************/
/* RemoveVoidFunctionFromCallList: Removes a function from a list */
/*   of functions which are called to perform certain operations  */
/*   (e.g. clear, reset, and bload functions).                    */
/******************************************************************/
struct voidCallFunctionItem *RemoveVoidFunctionFromCallList(
        const Environment&theEnv,
        const char *name,
        struct voidCallFunctionItem *head,
        bool *found) {
    struct voidCallFunctionItem *currentPtr, *lastPtr;

    *found = false;
    lastPtr = nullptr;
    currentPtr = head;

    while (currentPtr != nullptr) {
        if (strcmp(name, currentPtr->name) == 0) {
            *found = true;
            if (lastPtr == nullptr) { head = currentPtr->next; }
            else { lastPtr->next = currentPtr->next; }

            genfree(theEnv, (void *) currentPtr->name, strlen(currentPtr->name) + 1);
            rtn_struct(theEnv, voidCallFunctionItem, currentPtr);
            return head;
        }

        lastPtr = currentPtr;
        currentPtr = currentPtr->next;
    }

    return head;
}

/******************************************************************/
/* RemoveBoolFunctionFromCallList: Removes a function from a list */
/*   of functions which are called to perform certain operations  */
/*   (e.g. clear, reset, and bload functions).                    */
/******************************************************************/
struct boolCallFunctionItem *RemoveBoolFunctionFromCallList(
        const Environment&theEnv,
        const char *name,
        struct boolCallFunctionItem *head,
        bool *found) {
    struct boolCallFunctionItem *currentPtr, *lastPtr;

    *found = false;
    lastPtr = nullptr;
    currentPtr = head;

    while (currentPtr != nullptr) {
        if (strcmp(name, currentPtr->name) == 0) {
            *found = true;
            if (lastPtr == nullptr) { head = currentPtr->next; }
            else { lastPtr->next = currentPtr->next; }

            genfree(theEnv, (void *) currentPtr->name, strlen(currentPtr->name) + 1);
            rtn_struct(theEnv, boolCallFunctionItem, currentPtr);
            return head;
        }

        lastPtr = currentPtr;
        currentPtr = currentPtr->next;
    }

    return head;
}

/*************************************************************/
/* DeallocateVoidCallList: Removes all functions from a list */
/*   of functions which are called to perform certain        */
/*   operations (e.g. clear, reset, and bload functions).    */
/*************************************************************/
void DeallocateVoidCallList(
        const Environment&theEnv,
        struct voidCallFunctionItem *theList) {
    struct voidCallFunctionItem *tmpPtr, *nextPtr;

    tmpPtr = theList;
    while (tmpPtr != nullptr) {
        nextPtr = tmpPtr->next;
        genfree(theEnv, (void *) tmpPtr->name, strlen(tmpPtr->name) + 1);
        rtn_struct(theEnv, voidCallFunctionItem, tmpPtr);
        tmpPtr = nextPtr;
    }
}

/*************************************************************/
/* DeallocateBoolCallList: Removes all functions from a list */
/*   of functions which are called to perform certain        */
/*   operations (e.g. clear, reset, and bload functions).    */
/*************************************************************/
void DeallocateBoolCallList(
        const Environment&theEnv,
        struct boolCallFunctionItem *theList) {
    struct boolCallFunctionItem *tmpPtr, *nextPtr;

    tmpPtr = theList;
    while (tmpPtr != nullptr) {
        nextPtr = tmpPtr->next;
        genfree(theEnv, (void *) tmpPtr->name, strlen(tmpPtr->name) + 1);
        rtn_struct(theEnv, boolCallFunctionItem, tmpPtr);
        tmpPtr = nextPtr;
    }
}

/***********************************************************/
/* AddFunctionToCallListWithArg: Adds a function to a list */
/*   of functions which are called to perform certain      */
/*   operations (e.g. clear, reset, and bload functions).  */
/***********************************************************/
struct callFunctionItemWithArg *AddFunctionToCallListWithArg(
        const Environment&theEnv,
        const char *name,
        int priority,
        VoidCallFunctionWithArg *func,
        struct callFunctionItemWithArg *head,
        void *context) {
    struct callFunctionItemWithArg *newPtr, *currentPtr, *lastPtr = nullptr;

    newPtr = get_struct(theEnv, callFunctionItemWithArg);

    newPtr->name = name;
    newPtr->func = func;
    newPtr->priority = priority;
    newPtr->context = context;

    if (head == nullptr) {
        newPtr->next = nullptr;
        return (newPtr);
    }

    currentPtr = head;
    while ((currentPtr != nullptr) ? (priority < currentPtr->priority) : false) {
        lastPtr = currentPtr;
        currentPtr = currentPtr->next;
    }

    if (lastPtr == nullptr) {
        newPtr->next = head;
        head = newPtr;
    } else {
        newPtr->next = currentPtr;
        lastPtr->next = newPtr;
    }

    return (head);
}

/**************************************************************/
/* RemoveFunctionFromCallListWithArg: Removes a function from */
/*   a list of functions which are called to perform certain  */
/*   operations (e.g. clear, reset, and bload functions).     */
/**************************************************************/
struct callFunctionItemWithArg *RemoveFunctionFromCallListWithArg(
        const Environment&theEnv,
        const char *name,
        struct callFunctionItemWithArg *head,
        bool *found) {
    struct callFunctionItemWithArg *currentPtr, *lastPtr;

    *found = false;
    lastPtr = nullptr;
    currentPtr = head;

    while (currentPtr != nullptr) {
        if (strcmp(name, currentPtr->name) == 0) {
            *found = true;
            if (lastPtr == nullptr) { head = currentPtr->next; }
            else { lastPtr->next = currentPtr->next; }

            rtn_struct(theEnv, callFunctionItemWithArg, currentPtr);
            return (head);
        }

        lastPtr = currentPtr;
        currentPtr = currentPtr->next;
    }

    return (head);
}

/****************************************************************/
/* DeallocateCallListWithArg: Removes all functions from a list */
/*   of functions which are called to perform certain           */
/*   operations (e.g. clear, reset, and bload functions).       */
/****************************************************************/
void DeallocateCallListWithArg(
        const Environment&theEnv,
        struct callFunctionItemWithArg *theList) {
    struct callFunctionItemWithArg *tmpPtr, *nextPtr;

    tmpPtr = theList;
    while (tmpPtr != nullptr) {
        nextPtr = tmpPtr->next;
        rtn_struct(theEnv, callFunctionItemWithArg, tmpPtr);
        tmpPtr = nextPtr;
    }
}

/*****************************************/
/* ItemHashValue: Returns the hash value */
/*   for the specified value.            */
/*****************************************/
size_t ItemHashValue(
        const Environment&theEnv,
        unsigned short theType,
        void *theValue,
        size_t theRange) {
    union {
        void *vv;
        unsigned uv;
    } fis;

    switch (theType) {
        case FLOAT_TYPE:
            return HashFloat(((CLIPSFloat *) theValue)->contents, theRange);

        case INTEGER_TYPE:
            return HashInteger(((CLIPSInteger *) theValue)->contents, theRange);

        case SYMBOL_TYPE:
        case STRING_TYPE:
        case INSTANCE_NAME_TYPE:
            return HashSymbol(((CLIPSLexeme *) theValue)->contents, theRange);

        case MULTIFIELD_TYPE:
            return HashMultifield((Multifield *) theValue, theRange);

#if DEFTEMPLATE_CONSTRUCT
        case FACT_ADDRESS_TYPE:
            return (((Fact *) theValue)->hashValue % theRange);
#endif

        case EXTERNAL_ADDRESS_TYPE:
            return HashExternalAddress(((CLIPSExternalAddress *) theValue)->contents, theRange);

        case INSTANCE_ADDRESS_TYPE:
            fis.uv = 0;
            fis.vv = theValue;
            return (fis.uv % theRange);
    }

    SystemError(theEnv, "UTILITY", 1);
    return 0;
}

/********************************************/
/* YieldTime: Yields time to a user-defined */
/*   function. Intended to allow foreground */
/*   application responsiveness when CLIPS  */
/*   is running in the background.          */
/********************************************/
void YieldTime(
        const Environment&theEnv) {
    if ((UtilityData(theEnv)->YieldTimeFunction != nullptr) && UtilityData(theEnv)->YieldFunctionEnabled) {
        (*UtilityData(theEnv)->YieldTimeFunction)();
    }
}

/****************************/
/* EnablePeriodicFunctions: */
/****************************/
bool EnablePeriodicFunctions(
        const Environment&theEnv,
        bool value) {
    bool oldValue;

    oldValue = UtilityData(theEnv)->PeriodicFunctionsEnabled;

    UtilityData(theEnv)->PeriodicFunctionsEnabled = value;

    return oldValue;
}

/************************/
/* EnableYieldFunction: */
/************************/
bool EnableYieldFunction(
        const Environment&theEnv,
        bool value) {
    bool oldValue;

    oldValue = UtilityData(theEnv)->YieldFunctionEnabled;

    UtilityData(theEnv)->YieldFunctionEnabled = value;

    return oldValue;
}

/*************************************************************************/
/* AddTrackedMemory: Tracked memory is memory allocated by CLIPS that's  */
/*   referenced by a variable on the stack, but not by any environment   */
/*   data structure. An example would be the storage for local variables */
/*   allocated when a deffunction is executed. Tracking this memory      */
/*   allows it to be removed later when using longjmp as the code that   */
/*   would normally deallocate the memory would be bypassed.             */
/*************************************************************************/
struct trackedMemory *AddTrackedMemory(
        const Environment&theEnv,
        void *theMemory,
        size_t theSize) {
    struct trackedMemory *newPtr;

    newPtr = get_struct(theEnv, trackedMemory);

    newPtr->prev = nullptr;
    newPtr->theMemory = theMemory;
    newPtr->memSize = theSize;
    newPtr->next = UtilityData(theEnv)->trackList;
    UtilityData(theEnv)->trackList = newPtr;

    return newPtr;
}

/************************/
/* RemoveTrackedMemory: */
/************************/
void RemoveTrackedMemory(
        const Environment&theEnv,
        struct trackedMemory *theTracker) {
    if (theTracker->prev == nullptr) { UtilityData(theEnv)->trackList = theTracker->next; }
    else { theTracker->prev->next = theTracker->next; }

    if (theTracker->next != nullptr) { theTracker->next->prev = theTracker->prev; }

    rtn_struct(theEnv, trackedMemory, theTracker);
}

/******************************************/
/* UTF8Length: Returns the logical number */
/*   of characters in a UTF8 string.      */
/******************************************/
size_t UTF8Length(
        const char *s) {
    size_t i = 0, length = 0;

    while (s[i] != '\0') {
        UTF8Increment(s, &i);
        length++;
    }

    return (length);
}

/*********************************************/
/* UTF8Increment: Finds the beginning of the */
/*   next character in a UTF8 string.        */
/*********************************************/
void UTF8Increment(
        const char *s,
        size_t *i) {
    (void) (IsUTF8Start(s[++(*i)]) ||
            IsUTF8Start(s[++(*i)]) ||
            IsUTF8Start(s[++(*i)]) ||
            ++(*i));
}

/****************************************************/
/* UTF8Offset: Converts the logical character index */
/*   in a UTF8 string to the actual byte offset.    */
/****************************************************/
size_t UTF8Offset(
        const char *str,
        size_t charnum) {
    size_t offs = 0;

    while ((charnum > 0) && (str[offs])) {
        (void) (IsUTF8Start(str[++offs]) ||
                IsUTF8Start(str[++offs]) ||
                IsUTF8Start(str[++offs]) ||
                ++offs);

        charnum--;
    }

    return offs;
}

/*************************************************/
/* UTF8CharNum: Converts the UTF8 character byte */
/*   offset to the logical character index.      */
/*************************************************/
size_t UTF8CharNum(
        const char *s,
        size_t offset) {
    size_t charnum = 0, offs = 0;

    while ((offs < offset) && (s[offs])) {
        (void) (IsUTF8Start(s[++offs]) ||
                IsUTF8Start(s[++offs]) ||
                IsUTF8Start(s[++offs]) ||
                ++offs);

        charnum++;
    }

    return charnum;
}

#endif
/************************/
/* CreateStringBuilder: */
/************************/
stringBuilder::stringBuilder(const Environment& theEnv) : _env (theEnv) { }
std::string
stringBuilder::contents() const noexcept {
    // inefficient but fine for now
    auto str = _internal.str();
    return str + EOS;
}
StringBuilder *CreateStringBuilder(const Environment&theEnv, size_t) {
    return new StringBuilder(theEnv);
}
void
stringBuilder::append(const char* str) {
    std::string tmp(str);
    _internal << tmp;
}
/*************/
/* SBAppend: */
/*************/
void SBAppend(StringBuilder *theSB, const char *appendString) { theSB->append(appendString); }

/********************/
/* SBAppendInteger: */
/********************/
void
stringBuilder::append(long long value) {
    _internal << value;
}
void SBAppendInteger( StringBuilder *theSB, long long value) { theSB->append(value); }

/******************/
/* SBAppendFloat: */
/******************/
void
stringBuilder::append(double value) {
    _internal << value;
}
void SBAppendFloat( StringBuilder *theSB, double value) { theSB->append(value); }

/**************/
/* SBAddChar: */
/**************/
void
stringBuilder::appendChar(int value) {
    _internal << char(value);
}

void SBAddChar( StringBuilder *theSB, int theChar) { theSB->appendChar(theChar); }

/***********/
/* SBReset */
/***********/
void
stringBuilder::reset()
{
    _internal.str("");
}
void SBReset(StringBuilder *theSB) { theSB->reset(); }

/**********/
/* SBCopy */
/**********/
stringBuilder::stringBuilder(const stringBuilder & other) : _env(other._env), _internal(other._internal.str()){ }
/**************/
/* SBDispose: */
/**************/
void SBDispose(StringBuilder *theSB) {
    delete theSB;
}
#if 0
/***************************************************
  NAME         : BufferedRead
  DESCRIPTION  : Reads data from binary file
                 (Larger blocks than requested size
                  may be read and buffered)
  INPUTS       : 1) The buffer
                 2) The buffer size
  RETURNS      : Nothing useful
  SIDE EFFECTS : Data stored in buffer
  NOTES        : None
 ***************************************************/
void BufferedRead(
        const Environment&theEnv,
        void *buf,
        size_t bufsz) {
    size_t i, amountLeftToRead;

    if (UtilityData(theEnv)->CurrentReadBuffer != nullptr) {
        amountLeftToRead = UtilityData(theEnv)->CurrentReadBufferSize - UtilityData(theEnv)->CurrentReadBufferOffset;
        if (bufsz <= amountLeftToRead) {
            for (i = 0L; i < bufsz; i++)
                ((char *) buf)[i] = UtilityData(theEnv)->CurrentReadBuffer[i + UtilityData(theEnv)->CurrentReadBufferOffset];
            UtilityData(theEnv)->CurrentReadBufferOffset += bufsz;
            if (UtilityData(theEnv)->CurrentReadBufferOffset == UtilityData(theEnv)->CurrentReadBufferSize)
                FreeReadBuffer(theEnv);
        } else {
            if (UtilityData(theEnv)->CurrentReadBufferOffset < UtilityData(theEnv)->CurrentReadBufferSize) {
                for (i = 0; i < amountLeftToRead; i++)
                    ((char *) buf)[i] = UtilityData(theEnv)->CurrentReadBuffer[i + UtilityData(theEnv)->CurrentReadBufferOffset];
                bufsz -= amountLeftToRead;
                buf = (void *) (((char *) buf) + amountLeftToRead);
            }
            FreeReadBuffer(theEnv);
            BufferedRead(theEnv, buf, bufsz);
        }
    } else {
        if (bufsz > MAX_BLOCK_SIZE) {
            UtilityData(theEnv)->CurrentReadBufferSize = bufsz;
            if (bufsz > (UtilityData(theEnv)->BinaryFileSize - UtilityData(theEnv)->BinaryFileOffset)) {
                SystemError(theEnv, "INSFILE", 2);
                ExitRouter(theEnv, EXIT_FAILURE);
            }
        } else if (MAX_BLOCK_SIZE >
                   (UtilityData(theEnv)->BinaryFileSize - UtilityData(theEnv)->BinaryFileOffset)) {
            UtilityData(theEnv)->CurrentReadBufferSize = UtilityData(theEnv)->BinaryFileSize - UtilityData(theEnv)->BinaryFileOffset;
        } else { UtilityData(theEnv)->CurrentReadBufferSize = (unsigned long) MAX_BLOCK_SIZE; }

        UtilityData(theEnv)->CurrentReadBuffer = (char *) genalloc(theEnv, UtilityData(theEnv)->CurrentReadBufferSize);
        GenReadBinary(theEnv, UtilityData(theEnv)->CurrentReadBuffer, UtilityData(theEnv)->CurrentReadBufferSize);
        for (i = 0L; i < bufsz; i++)
            ((char *) buf)[i] = UtilityData(theEnv)->CurrentReadBuffer[i];
        UtilityData(theEnv)->CurrentReadBufferOffset = bufsz;
        UtilityData(theEnv)->BinaryFileOffset += UtilityData(theEnv)->CurrentReadBufferSize;
    }
}

/*****************************************************
  NAME         : FreeReadBuffer
  DESCRIPTION  : Deallocates buffer for binary reads
  INPUTS       : None
  RETURNS      : Nothing usefu
  SIDE EFFECTS : Binary global read buffer deallocated
  NOTES        : None
 *****************************************************/
void FreeReadBuffer(
        const Environment&theEnv) {
    if (UtilityData(theEnv)->CurrentReadBufferSize != 0L) {
        genfree(theEnv, UtilityData(theEnv)->CurrentReadBuffer, UtilityData(theEnv)->CurrentReadBufferSize);
        UtilityData(theEnv)->CurrentReadBuffer = nullptr;
        UtilityData(theEnv)->CurrentReadBufferSize = 0L;
        UtilityData(theEnv)->CurrentReadBufferOffset = 0L; // TBD Added
    }
}
#endif