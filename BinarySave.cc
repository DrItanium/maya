/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  10/04/17             */
/*                                                     */
/*                     BSAVE MODULE                    */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides core routines for saving constructs to  */
/*   a binary file.                                          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Used genstrncpy instead of strncpy.            */
/*                                                           */
/*            Borland C (IBM_TBC) and Metrowerks CodeWarrior */
/*            (MAC_MCW, IBM_MCW) are no longer supported.    */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.31: Data sizes written to binary files for         */
/*            validation when loaded.                        */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
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

#include "Setup.h"

#include "ArgumentAccess.h"
#include "BinaryLoad.h"
#include "ConstraintBinaryLoadSave.h"
#include "Environment.h"
#include "Expression.h"
#include "MemoryAllocation.h"
#include "Defmodule.h"
#include "PrintUtility.h"
#include "Router.h"
#include "Symbol.h"

#include "BinarySave.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
static void FindNeededItems(const Environment&);
static void InitializeFunctionNeededFlags(const Environment&);
static void WriteNeededFunctions(const Environment&, FILE *);
static size_t FunctionBinarySize(const Environment&);
static void WriteBinaryHeader(const Environment&, FILE *);
static void WriteBinaryFooter(const Environment&, FILE *);
#endif
static void DeallocateBsaveData(const Environment&);

/**********************************************/
/* InitializeBsaveData: Allocates environment */
/*    data for the bsave command.             */
/**********************************************/
void InitializeBsaveData(
        const Environment&theEnv) {
    AllocateEnvironmentData(theEnv, BSAVE_DATA, sizeof(bsaveData), DeallocateBsaveData);
}

/************************************************/
/* DeallocateBsaveData: Deallocates environment */
/*    data for the bsave command.               */
/************************************************/
static void DeallocateBsaveData(
        const Environment&theEnv) {
    struct BinaryItem *tmpPtr, *nextPtr;

    tmpPtr = BsaveData(theEnv)->ListOfBinaryItems;
    while (tmpPtr != nullptr) {
        nextPtr = tmpPtr->next;
        rtn_struct(theEnv, BinaryItem, tmpPtr);
        tmpPtr = nextPtr;
    }
}

/**************************************/
/* BsaveCommand: H/L access routine   */
/*   for the bsave command.           */
/**************************************/
void BsaveCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
#if BLOAD_AND_BSAVE
    const char *fileName;

    fileName = GetFileName(context);
    if (fileName != nullptr) {
        if (Bsave(theEnv, fileName)) {
            returnValue->lexemeValue = TrueSymbol(theEnv);
            return;
        }
    }
#else
#if MAC_XCD
#pragma unused(theEnv,context)
#endif
#endif
    returnValue->lexemeValue = FalseSymbol(theEnv);
}

#if BLOAD_AND_BSAVE

/****************************/
/* Bsave: C access routine  */
/*   for the bsave command. */
/****************************/
bool Bsave(
        const Environment&theEnv,
        const char *fileName) {
    FILE *fp;
    struct BinaryItem *biPtr;
    char constructBuffer[CONSTRUCT_HEADER_SIZE];
    unsigned long saveExpressionCount;

    /*=====================================*/
    /* If embedded, clear the error flags. */
    /*=====================================*/

    if (EvaluationData(theEnv)->CurrentExpression == nullptr) { ResetErrorFlags(theEnv); }

    /*===================================*/
    /* A bsave can't occur when a binary */
    /* image is already loaded.          */
    /*===================================*/

    if (Bloaded(theEnv)) {
        PrintErrorID(theEnv, "BSAVE", 1, false);
        WriteString(theEnv, STDERR,
                    "Cannot perform a binary save while a binary load is in effect.\n");
        return false;
    }

    /*================*/
    /* Open the file. */
    /*================*/

    if ((fp = GenOpen(theEnv, fileName, "wb")) == nullptr) {
        OpenErrorMessage(theEnv, "bsave", fileName);
        return false;
    }

    /*==============================*/
    /* Remember the current module. */
    /*==============================*/

    SaveCurrentModule(theEnv);

    /*==================================*/
    /* Write binary header to the file. */
    /*==================================*/

    WriteBinaryHeader(theEnv, fp);

    /*===========================================*/
    /* Initialize count variables, index values, */
    /* and determine some of the data structures */
    /* which need to be saved.                   */
    /*===========================================*/

    ExpressionData(theEnv)->ExpressionCount = 0;
    InitializeFunctionNeededFlags(theEnv);
    InitAtomicValueNeededFlags(theEnv);
    FindHashedExpressions(theEnv);
    FindNeededItems(theEnv);
    SetAtomicValueIndices(theEnv, false);

    /*===============================*/
    /* Save the functions and atoms. */
    /*===============================*/

    WriteNeededFunctions(theEnv, fp);
    WriteNeededAtomicValues(theEnv, fp);

    /*=========================================*/
    /* Write out the number of expression data */
    /* structures in the binary image.         */
    /*=========================================*/

    GenWrite(&ExpressionData(theEnv)->ExpressionCount, sizeof(unsigned long), fp);

    /*===========================================*/
    /* Save the numbers indicating the amount of */
    /* memory needed to bload the constructs.    */
    /*===========================================*/

    for (biPtr = BsaveData(theEnv)->ListOfBinaryItems;
         biPtr != nullptr;
         biPtr = biPtr->next) {
        if (biPtr->bsaveStorageFunction != nullptr) {
            genstrncpy(constructBuffer, biPtr->name, CONSTRUCT_HEADER_SIZE);
            GenWrite(constructBuffer, CONSTRUCT_HEADER_SIZE, fp);
            (*biPtr->bsaveStorageFunction)(theEnv, fp);
        }
    }

    /*====================================*/
    /* Write a binary footer to the file. */
    /*====================================*/

    WriteBinaryFooter(theEnv, fp);

    /*===================*/
    /* Save expressions. */
    /*===================*/

    ExpressionData(theEnv)->ExpressionCount = 0;
    BsaveHashedExpressions(theEnv, fp);
    saveExpressionCount = ExpressionData(theEnv)->ExpressionCount;
    BsaveConstructExpressions(theEnv, fp);
    ExpressionData(theEnv)->ExpressionCount = saveExpressionCount;

    /*===================*/
    /* Save constraints. */
    /*===================*/

    WriteNeededConstraints(theEnv, fp);

    /*==================*/
    /* Save constructs. */
    /*==================*/

    for (biPtr = BsaveData(theEnv)->ListOfBinaryItems;
         biPtr != nullptr;
         biPtr = biPtr->next) {
        if (biPtr->bsaveFunction != nullptr) {
            genstrncpy(constructBuffer, biPtr->name, CONSTRUCT_HEADER_SIZE);
            GenWrite(constructBuffer, CONSTRUCT_HEADER_SIZE, fp);
            (*biPtr->bsaveFunction)(theEnv, fp);
        }
    }

    /*===================================*/
    /* Save a binary footer to the file. */
    /*===================================*/

    WriteBinaryFooter(theEnv, fp);

    /*===========*/
    /* Clean up. */
    /*===========*/

    RestoreAtomicValueBuckets(theEnv);

    /*=================*/
    /* Close the file. */
    /*=================*/

    GenClose(theEnv, fp);

    /*=============================*/
    /* Restore the current module. */
    /*=============================*/

    RestoreCurrentModule(theEnv);

    /*==================================*/
    /* Return true to indicate success. */
    /*==================================*/

    return true;
}

/*********************************************/
/* InitializeFunctionNeededFlags: Marks each */
/*   function in the list of functions as    */
/*   being unneeded by this binary image.    */
/*********************************************/
static void InitializeFunctionNeededFlags(
        const Environment&theEnv) {
    struct functionDefinition *functionList;

    for (functionList = GetFunctionList(theEnv);
         functionList != nullptr;
         functionList = functionList->next) { functionList->neededFunction = false; }
}

/**********************************************************/
/* FindNeededItems: Searches through the constructs for   */
/*   the functions, constraints, or atoms that are needed */
/*   by that construct. This routine also counts the      */
/*   number of expressions in use (through a global).     */
/**********************************************************/
static void FindNeededItems(
        const Environment&theEnv) {
    struct BinaryItem *biPtr;

    for (biPtr = BsaveData(theEnv)->ListOfBinaryItems;
         biPtr != nullptr;
         biPtr = biPtr->next) { if (biPtr->findFunction != nullptr) (*biPtr->findFunction)(theEnv); }
}

/****************************************************/
/* WriteNeededFunctions: Writes the names of needed */
/*   functions to the binary save file.             */
/****************************************************/
static void WriteNeededFunctions(
        const Environment&theEnv,
        FILE *fp) {
    unsigned long count = 0;
    size_t space, length;
    struct functionDefinition *functionList;

    /*================================================*/
    /* Assign each function an index if it is needed. */
    /*================================================*/

    for (functionList = GetFunctionList(theEnv);
         functionList != nullptr;
         functionList = functionList->next) {
        if (functionList->neededFunction) { functionList->bsaveIndex = count++; }
        else { functionList->bsaveIndex = ULONG_MAX; }
    }

    /*===================================================*/
    /* Write the number of function names to be written. */
    /*===================================================*/

    GenWrite(&count, sizeof(unsigned long), fp);
    if (count == 0) {
        GenWrite(&count, sizeof(unsigned long), fp);
        return;
    }

    /*================================*/
    /* Determine the amount of space  */
    /* needed for the function names. */
    /*================================*/

    space = FunctionBinarySize(theEnv);
    GenWrite(&space, sizeof(unsigned long), fp);

    /*===============================*/
    /* Write out the function names. */
    /*===============================*/

    for (functionList = GetFunctionList(theEnv);
         functionList != nullptr;
         functionList = functionList->next) {
        if (functionList->neededFunction) {
            length = strlen(functionList->callFunctionName->contents) + 1;
            GenWrite((void *) functionList->callFunctionName->contents, length, fp);
        }
    }
}

/*********************************************/
/* FunctionBinarySize: Determines the number */
/*   of bytes needed to save all of the      */
/*   function names in the binary save file. */
/*********************************************/
static size_t FunctionBinarySize(
        const Environment&theEnv) {
    size_t size = 0;
    struct functionDefinition *functionList;

    for (functionList = GetFunctionList(theEnv);
         functionList != nullptr;
         functionList = functionList->next) {
        if (functionList->neededFunction) { size += strlen(functionList->callFunctionName->contents) + 1; }
    }

    return (size);
}

/***************************************************/
/* SaveBloadCount: Used to save the data structure */
/*   count values when a binary save command is    */
/*   issued when a binary image is loaded.         */
/***************************************************/
void SaveBloadCount(
        const Environment&theEnv,
        unsigned long cnt) {
    BLOADCNTSV *tmp, *prv;

    tmp = get_struct(theEnv, bloadcntsv);
    tmp->val = cnt;
    tmp->nxt = nullptr;

    if (BsaveData(theEnv)->BloadCountSaveTop == nullptr) { BsaveData(theEnv)->BloadCountSaveTop = tmp; }
    else {
        prv = BsaveData(theEnv)->BloadCountSaveTop;
        while (prv->nxt != nullptr) { prv = prv->nxt; }
        prv->nxt = tmp;
    }
}

/**************************************************/
/* RestoreBloadCount: Restores the data structure */
/*   count values after a binary save command is  */
/*   completed when a binary image is loaded.     */
/**************************************************/
void RestoreBloadCount(
        const Environment&theEnv,
        unsigned long *cnt) {
    BLOADCNTSV *tmp;

    *cnt = BsaveData(theEnv)->BloadCountSaveTop->val;
    tmp = BsaveData(theEnv)->BloadCountSaveTop;
    BsaveData(theEnv)->BloadCountSaveTop = BsaveData(theEnv)->BloadCountSaveTop->nxt;
    rtn_struct(theEnv, bloadcntsv, tmp);
}

/**********************************************/
/* MarkNeededItems: Examines an expression to */
/*   determine which items are needed to save */
/*   an expression as part of a binary image. */
/**********************************************/
void MarkNeededItems(
        const Environment&theEnv,
        struct expr *testPtr) {
    while (testPtr != nullptr) {
        switch (testPtr->type) {
            case SYMBOL_TYPE:
            case STRING_TYPE:
            case GBL_VARIABLE:
            case INSTANCE_NAME_TYPE:
                testPtr->lexemeValue->neededSymbol = true;
                break;

            case FLOAT_TYPE:
                testPtr->floatValue->neededFloat = true;
                break;

            case INTEGER_TYPE:
                testPtr->integerValue->neededInteger = true;
                break;

            case FCALL:
                testPtr->functionValue->neededFunction = true;
                break;

            case VOID_TYPE:
                break;

            default:
                if (EvaluationData(theEnv)->PrimitivesArray[testPtr->type] == nullptr) break;
                if (EvaluationData(
                        theEnv)->PrimitivesArray[testPtr->type]->bitMap) { ((CLIPSBitMap *) testPtr->value)->neededBitMap = true; }
                break;

        }

        if (testPtr->argList != nullptr) { MarkNeededItems(theEnv, testPtr->argList); }

        testPtr = testPtr->nextArg;
    }
}

/******************************************************/
/* WriteBinaryHeader: Writes a binary header used for */
/*   verification when a binary image is loaded.      */
/******************************************************/
static void WriteBinaryHeader(
        const Environment&theEnv,
        FILE *fp) {
    GenWrite((void *) BloadData(theEnv)->BinaryPrefixID, strlen(BloadData(theEnv)->BinaryPrefixID) + 1, fp);
    GenWrite((void *) BloadData(theEnv)->BinaryVersionID, strlen(BloadData(theEnv)->BinaryVersionID) + 1, fp);
    GenWrite((void *) BloadData(theEnv)->BinarySizes, strlen(BloadData(theEnv)->BinarySizes) + 1, fp);
}

/******************************************************/
/* WriteBinaryFooter: Writes a binary footer used for */
/*   verification when a binary image is loaded.      */
/******************************************************/
static void WriteBinaryFooter(
        const Environment&theEnv,
        FILE *fp) {
    char footerBuffer[CONSTRUCT_HEADER_SIZE];

    genstrncpy(footerBuffer, BloadData(theEnv)->BinaryPrefixID, CONSTRUCT_HEADER_SIZE);
    GenWrite(footerBuffer, CONSTRUCT_HEADER_SIZE, fp);
}

#endif /* BLOAD_AND_BSAVE */

#if BLOAD_AND_BSAVE

/**********************************************************/
/* AddBinaryItem: Informs the bload/bsave commands of the */
/*   appropriate access functions needed to save/load the */
/*   data structures of a construct or other "item" to a  */
/*   binary file.                                         */
/**********************************************************/
bool AddBinaryItem(
        const Environment&theEnv,
        const char *name,
        int priority,
        void (*findFunction)(const Environment&),
        void (*expressionFunction)(const Environment&, FILE *),
        void (*bsaveStorageFunction)(const Environment&, FILE *),
        void (*bsaveFunction)(const Environment&, FILE *),
        void (*bloadStorageFunction)(const Environment&),
        void (*bloadFunction)(const Environment&),
        void (*clearFunction)(const Environment&)) {
    struct BinaryItem *newPtr, *currentPtr, *lastPtr = nullptr;

    /*========================================*/
    /* Create the binary item data structure. */
    /*========================================*/

    newPtr = get_struct(theEnv, BinaryItem);

    newPtr->name = name;
    newPtr->findFunction = findFunction;
    newPtr->expressionFunction = expressionFunction;
    newPtr->bsaveStorageFunction = bsaveStorageFunction;
    newPtr->bsaveFunction = bsaveFunction;
    newPtr->bloadStorageFunction = bloadStorageFunction;
    newPtr->bloadFunction = bloadFunction;
    newPtr->clearFunction = clearFunction;
    newPtr->priority = priority;

    /*=================================*/
    /* If no binary items are defined, */
    /* just put the item on the list.  */
    /*=================================*/

    if (BsaveData(theEnv)->ListOfBinaryItems == nullptr) {
        newPtr->next = nullptr;
        BsaveData(theEnv)->ListOfBinaryItems = newPtr;
        return true;
    }

    /*=========================================*/
    /* Otherwise, place the binary item at the */
    /* appropriate place in the list of binary */
    /* items based on its priority.            */
    /*=========================================*/

    currentPtr = BsaveData(theEnv)->ListOfBinaryItems;
    while ((currentPtr != nullptr) ? (priority < currentPtr->priority) : false) {
        lastPtr = currentPtr;
        currentPtr = currentPtr->next;
    }

    if (lastPtr == nullptr) {
        newPtr->next = BsaveData(theEnv)->ListOfBinaryItems;
        BsaveData(theEnv)->ListOfBinaryItems = newPtr;
    } else {
        newPtr->next = currentPtr;
        lastPtr->next = newPtr;
    }

    /*==================================*/
    /* Return true to indicate the item */
    /* was successfully added.          */
    /*==================================*/

    return true;
}

#endif /* BLOAD_AND_BSAVE */




