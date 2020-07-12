/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  05/03/19             */
/*                                                     */
/*                    BLOAD MODULE                     */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides core routines for loading constructs    */
/*   from a binary file.                                     */
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
/*      6.30: Borland C (IBM_TBC) and Metrowerks CodeWarrior */
/*            (MAC_MCW, IBM_MCW) are no longer supported.    */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.31: Data sizes written to binary files for         */
/*            validation when loaded.                        */
/*                                                           */
/*      6.40: Refactored code to reduce header dependencies  */
/*            in sysdep.c.                                   */
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
/*************************************************************/

#include "Setup.h"

#include "ArgumentAccess.h"
#include "BinarySave.h"
#include "Construct.h"
#include "ConstraintBinaryLoadSave.h"
#include "Environment.h"
#include "Expression.h"
#include "MemoryAllocation.h"
#include "PrintUtility.h"
#include "Router.h"
#include "Utility.h"

#include "BinaryLoad.h"

#if (BLOAD_AND_BSAVE)

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static FunctionDefinition **ReadNeededFunctions(const Environment&, unsigned long *, bool *);
static FunctionDefinition *FastFindFunction(const Environment&, const char *, FunctionDefinition *);
static bool ClearBload(const Environment&);
static void ClearBloadCallback(const Environment&, void *);
static void AbortBload(const Environment&);
static bool BloadOutOfMemoryFunction(const Environment&, size_t);
static void DeallocateBloadData(const Environment&);

/**********************************************/
/* InitializeBloadData: Allocates environment */
/*    data for the bload command.             */
/**********************************************/
void InitializeBloadData(
        const Environment&theEnv) {
    char sizeBuffer[20];
    sprintf(sizeBuffer, "%2zu%2zu%2zu%2zu%2zu", sizeof(void *), sizeof(double),
            sizeof(int), sizeof(long), sizeof(long long));
    theEnv->allocateEnvironmentModule<bloadData>();
    /// @todo DeallocateBLoadData is the dtor for bloadData
    //AddEnvironmentCleanupFunction(theEnv, "bload", DeallocateBloadData, -1500);
    AddClearFunction(theEnv, "bload", ClearBloadCallback, 10000, nullptr);

    BloadData(theEnv)->BinaryPrefixID = "\1\2\3\4CLIPS";
    BloadData(theEnv)->BinaryVersionID = "V6.40";
    BloadData(theEnv)->BinarySizes = (char *) genalloc(theEnv, strlen(sizeBuffer) + 1);
    genstrcpy(BloadData(theEnv)->BinarySizes, sizeBuffer);
}

/************************************************/
/* DeallocateBloadData: Deallocates environment */
/*    data for the bload command.               */
/************************************************/
static void DeallocateBloadData(
        const Environment&theEnv) {
    DeallocateVoidCallList(theEnv, BloadData(theEnv)->BeforeBloadFunctions);
    DeallocateVoidCallList(theEnv, BloadData(theEnv)->AfterBloadFunctions);
    DeallocateBoolCallList(theEnv, BloadData(theEnv)->ClearBloadReadyFunctions);
    DeallocateVoidCallList(theEnv, BloadData(theEnv)->AbortBloadFunctions);
    genfree(theEnv, BloadData(theEnv)->BinarySizes, strlen(BloadData(theEnv)->BinarySizes) + 1);
}

/****************************/
/* Bload: C access routine  */
/*   for the bload command. */
/****************************/
bool Bload(
        const Environment&theEnv,
        const char *fileName) {
    unsigned long numberOfFunctions;
    unsigned long space;
    bool error;
    char IDbuffer[20];
    char sizesBuffer[20];
    char constructBuffer[CONSTRUCT_HEADER_SIZE];
    struct BinaryItem *biPtr;
    struct voidCallFunctionItem *bfPtr;

    /*=====================================*/
    /* If embedded, clear the error flags. */
    /*=====================================*/

    if (EvaluationData(theEnv)->CurrentExpression == nullptr) { ResetErrorFlags(theEnv); }

    /*================*/
    /* Open the file. */
    /*================*/

    if (!GenOpenReadBinary(theEnv, "bload", fileName)) {
        OpenErrorMessage(theEnv, "bload", fileName);
        return false;
    }

    /*=====================================*/
    /* Determine if this is a binary file. */
    /*=====================================*/

    GenReadBinary(theEnv, IDbuffer, strlen(BloadData(theEnv)->BinaryPrefixID) + 1);
    if (strcmp(IDbuffer, BloadData(theEnv)->BinaryPrefixID) != 0) {
        PrintErrorID(theEnv, "BLOAD", 2, false);
        WriteString(theEnv, STDERR, "File '");
        WriteString(theEnv, STDERR, fileName);
        WriteString(theEnv, STDERR, "' is not a binary construct file.\n");
        GenCloseBinary(theEnv);
        return false;
    }

    /*=======================================*/
    /* Determine if it's a binary file using */
    /* a format from a different version.    */
    /*=======================================*/

    GenReadBinary(theEnv, IDbuffer, strlen(BloadData(theEnv)->BinaryVersionID) + 1);
    if (strcmp(IDbuffer, BloadData(theEnv)->BinaryVersionID) != 0) {
        PrintErrorID(theEnv, "BLOAD", 3, false);
        WriteString(theEnv, STDERR, "File '");
        WriteString(theEnv, STDERR, fileName);
        WriteString(theEnv, STDERR, "' is an incompatible binary construct file.\n");
        GenCloseBinary(theEnv);
        return false;
    }

    /*===========================================*/
    /* Determine if it's a binary file using the */
    /* correct size for pointers and numbers.    */
    /*===========================================*/

    GenReadBinary(theEnv, sizesBuffer, strlen(BloadData(theEnv)->BinarySizes) + 1);
    if (strcmp(sizesBuffer, BloadData(theEnv)->BinarySizes) != 0) {
        PrintErrorID(theEnv, "BLOAD", 3, false);
        WriteString(theEnv, STDERR, "File '");
        WriteString(theEnv, STDERR, fileName);
        WriteString(theEnv, STDERR, "' is an incompatible binary construct file.\n");
        GenCloseBinary(theEnv);
        return false;
    }

    /*====================*/
    /* Clear environment. */
    /*====================*/

    if (BloadData(theEnv)->BloadActive) {
        if (!ClearBload(theEnv)) {
            GenCloseBinary(theEnv);
            return false;
        }
    }

    /*=================================*/
    /* Determine if the KB environment */
    /* was successfully cleared.       */
    /*=================================*/

    if (!ClearReady(theEnv)) {
        GenCloseBinary(theEnv);
        PrintErrorID(theEnv, "BLOAD", 4, false);
        WriteString(theEnv, STDERR, "The ");
        WriteString(theEnv, STDERR, APPLICATION_NAME);
        WriteString(theEnv, STDERR, " environment could not be cleared.\n");
        WriteString(theEnv, STDERR, "Binary load cannot continue.\n");
        return false;
    }

    /*==================================*/
    /* Call the list of functions to be */
    /* executed before a bload occurs.  */
    /*==================================*/

    ConstructData(theEnv)->ClearInProgress = true;
    for (bfPtr = BloadData(theEnv)->BeforeBloadFunctions;
         bfPtr != nullptr;
         bfPtr = bfPtr->next) { (*bfPtr->func)(theEnv, nullptr); }

    ConstructData(theEnv)->ClearInProgress = false;

    /*====================================================*/
    /* Read in the functions needed by this binary image. */
    /*====================================================*/

    BloadData(theEnv)->FunctionArray = ReadNeededFunctions(theEnv, &numberOfFunctions, &error);
    if (error) {
        GenCloseBinary(theEnv);
        AbortBload(theEnv);
        return false;
    }

    /*================================================*/
    /* Read in the atoms needed by this binary image. */
    /*================================================*/

    ReadNeededAtomicValues(theEnv);

    /*===========================================*/
    /* Determine the number of expressions to be */
    /* read and allocate the appropriate space   */
    /*===========================================*/

    AllocateExpressions(theEnv);

    /*==========================================================*/
    /* Read in the memory requirements of the constructs stored */
    /* in this binary image and allocate the necessary space    */
    /*==========================================================*/

    for (GenReadBinary(theEnv, constructBuffer, CONSTRUCT_HEADER_SIZE);
         strncmp(constructBuffer, BloadData(theEnv)->BinaryPrefixID, CONSTRUCT_HEADER_SIZE) != 0;
         GenReadBinary(theEnv, constructBuffer, CONSTRUCT_HEADER_SIZE)) {
        bool found;

        /*================================================*/
        /* Search for the construct type in the list of   */
        /* binary items. If found, allocate the storage   */
        /* needed by the construct for this binary image. */
        /*================================================*/

        found = false;
        for (biPtr = BsaveData(theEnv)->ListOfBinaryItems;
             biPtr != nullptr;
             biPtr = biPtr->next) {
            if (strncmp(biPtr->name, constructBuffer, CONSTRUCT_HEADER_SIZE) == 0) {
                if (biPtr->bloadStorageFunction != nullptr) {
                    (*biPtr->bloadStorageFunction)(theEnv);
                    found = true;
                }
                break;
            }
        }

        /*==========================================*/
        /* If the construct type wasn't found, skip */
        /* the storage binary load information for  */
        /* this construct.                          */
        /*==========================================*/

        if (!found) {
            GenReadBinary(theEnv, &space, sizeof(unsigned long));
            GetSeekCurBinary(theEnv, (long) space);
            if (space != 0) {
                WriteString(theEnv, STDOUT, "\nSkipping ");
                WriteString(theEnv, STDOUT, constructBuffer);
                WriteString(theEnv, STDOUT, " constructs because of unavailability\n");
            }
        }
    }

    /*======================================*/
    /* Refresh the pointers in expressions. */
    /*======================================*/

    RefreshExpressions(theEnv);

    /*==========================*/
    /* Read in the constraints. */
    /*==========================*/

    ReadNeededConstraints(theEnv);

    /*======================================================*/
    /* Read in the constructs stored in this binary image.  */
    /*======================================================*/

    for (GenReadBinary(theEnv, constructBuffer, CONSTRUCT_HEADER_SIZE);
         strncmp(constructBuffer, BloadData(theEnv)->BinaryPrefixID, CONSTRUCT_HEADER_SIZE) != 0;
         GenReadBinary(theEnv, constructBuffer, CONSTRUCT_HEADER_SIZE)) {
        bool found;

        /*==================================================*/
        /* Search for the function to load the construct    */
        /* into the previously allocated storage. If found, */
        /* call the function to load the construct.         */
        /*==================================================*/

        found = false;
        for (biPtr = BsaveData(theEnv)->ListOfBinaryItems;
             biPtr != nullptr;
             biPtr = biPtr->next) {
            if (strncmp(biPtr->name, constructBuffer, CONSTRUCT_HEADER_SIZE) == 0) {
                if (biPtr->bloadFunction != nullptr) {
                    (*biPtr->bloadFunction)(theEnv);
                    found = true;
                }
                break;
            }
        }

        /*==========================================*/
        /* If the construct type wasn't found, skip */
        /* the binary data for this construct.      */
        /*==========================================*/

        if (!found) {
            GenReadBinary(theEnv, &space, sizeof(unsigned long));
            GetSeekCurBinary(theEnv, (long) space);
        }
    }

    /*=================*/
    /* Close the file. */
    /*=================*/

    GenCloseBinary(theEnv);

    /*========================================*/
    /* Free up temporary storage used for the */
    /* function and atomic value information. */
    /*========================================*/

    if (BloadData(theEnv)->FunctionArray != nullptr) {
        genfree(theEnv, BloadData(theEnv)->FunctionArray,
                sizeof(FunctionDefinition *) * numberOfFunctions);
    }
    FreeAtomicValueStorage(theEnv);

    /*==================================*/
    /* Call the list of functions to be */
    /* executed after a bload occurs.   */
    /*==================================*/

    for (bfPtr = BloadData(theEnv)->AfterBloadFunctions;
         bfPtr != nullptr;
         bfPtr = bfPtr->next) { (*bfPtr->func)(theEnv, nullptr); }

    /*=======================================*/
    /* Add a clear function to remove binary */
    /* load when a clear command is issued.  */
    /*=======================================*/

    BloadData(theEnv)->BloadActive = true;

    /*=============================*/
    /* Return true to indicate the */
    /* binary load was successful. */
    /*=============================*/

    return true;
}

/************************************************************
  NAME         : BloadandRefresh
  DESCRIPTION  : Loads and refreshes objects - will bload
                 all objects at once, if possible, but
                 will aslo work in increments if memory is
                 restricted
  INPUTS       : 1) the number of objects to bload and update
                 2) the size of one object
                 3) An update function which takes a bloaded
                    object buffer and the index of the object
                    to refresh as arguments
  RETURNS      : Nothing useful
  SIDE EFFECTS : Objects bloaded and updated
  NOTES        : Assumes binary file pointer is positioned
                 for bloads of the objects
 ************************************************************/
void BloadandRefresh(
        const Environment&theEnv,
        unsigned long objcnt,
        size_t objsz,
        void (*objupdate)(const Environment&, void *, unsigned long)) {
    unsigned long i, bi;
    char *buf;
    unsigned long objsmaxread, objsread;
    size_t space;

    return;
#if 0
    if (objcnt == 0L) return;

    //oldOutOfMemoryFunction = SetOutOfMemoryFunction(theEnv, BloadOutOfMemoryFunction);
    objsmaxread = objcnt;
    do {
        space = objsmaxread * objsz;
        buf = (char *) genalloc(theEnv, space);
        if (buf == nullptr) {
            if ((objsmaxread / 2) == 0) {
                if ((*oldOutOfMemoryFunction)(theEnv, space)) {
                    SetOutOfMemoryFunction(theEnv, oldOutOfMemoryFunction);
                    return;
                }
            } else
                objsmaxread /= 2;
        }
    } while (buf == nullptr);

    SetOutOfMemoryFunction(theEnv, oldOutOfMemoryFunction);

    i = 0L;
    do {
        objsread = (objsmaxread > (objcnt - i)) ? (objcnt - i) : objsmaxread;
        GenReadBinary(theEnv, buf, objsread * objsz);
        for (bi = 0L; bi < objsread; bi++, i++)
            (*objupdate)(theEnv, buf + objsz * bi, i);
    } while (i < objcnt);
    genfree(theEnv, buf, space);
#endif
}

/**********************************************/
/* ReadNeededFunctions: Reads in the names of */
/*   functions needed by the binary image.    */
/**********************************************/
static FunctionDefinition **ReadNeededFunctions(
        const Environment&theEnv,
        unsigned long *numberOfFunctions,
        bool *error) {
    char *functionNames, *namePtr;
    unsigned long space;
    size_t temp;
    unsigned long i;
    FunctionDefinition **newFunctionArray, *functionPtr;
    bool functionsNotFound = false;

    /*===================================================*/
    /* Determine the number of function names to be read */
    /* and the space required for them.                  */
    /*===================================================*/

    GenReadBinary(theEnv, numberOfFunctions, sizeof(long));
    GenReadBinary(theEnv, &space, sizeof(unsigned long));
    if (*numberOfFunctions == 0) {
        *error = false;
        return nullptr;
    }

    /*=======================================*/
    /* Allocate area for strings to be read. */
    /*=======================================*/

    functionNames = (char *) genalloc(theEnv, space);
    GenReadBinary(theEnv, functionNames, space);

    /*====================================================*/
    /* Store the function pointers in the function array. */
    /*====================================================*/

    temp = sizeof(FunctionDefinition *) * *numberOfFunctions;
    newFunctionArray = (FunctionDefinition **) genalloc(theEnv, temp);
    namePtr = functionNames;
    functionPtr = nullptr;
    for (i = 0; i < *numberOfFunctions; i++) {
        if ((functionPtr = FastFindFunction(theEnv, namePtr, functionPtr)) == nullptr) {
            if (!functionsNotFound) {
                PrintErrorID(theEnv, "BLOAD", 6, false);
                WriteString(theEnv, STDERR, "The following undefined functions are ");
                WriteString(theEnv, STDERR, "referenced by this binary image:\n");
            }

            WriteString(theEnv, STDERR, "   ");
            WriteString(theEnv, STDERR, namePtr);
            WriteString(theEnv, STDERR, "\n");
            functionsNotFound = true;
        }

        newFunctionArray[i] = functionPtr;
        namePtr += strlen(namePtr) + 1;
    }

    /*==========================================*/
    /* Free the memory used by the name buffer. */
    /*==========================================*/

    genfree(theEnv, functionNames, space);

    /*==================================================*/
    /* If any of the required functions were not found, */
    /* then free the memory used by the function array. */
    /*==================================================*/

    if (functionsNotFound) {
        genfree(theEnv, newFunctionArray, temp);
        newFunctionArray = nullptr;
    }

    /*===================================*/
    /* Set globals to appropriate values */
    /* and return the function array.    */
    /*===================================*/

    *error = functionsNotFound;
    return newFunctionArray;
}

/*****************************************/
/* FastFindFunction: Search the function */
/*   list for a specific function.       */
/*****************************************/
static FunctionDefinition *FastFindFunction(
        const Environment&theEnv,
        const char *functionName,
        FunctionDefinition *lastFunction) {
    FunctionDefinition *theList, *theFunction;

    /*========================*/
    /* Get the function list. */
    /*========================*/

    theList = GetFunctionList(theEnv);
    if (theList == nullptr) { return nullptr; }

    /*=======================================*/
    /* If we completed a previous function   */
    /* search, start where we last left off. */
    /*=======================================*/

    if (lastFunction != nullptr) { theFunction = lastFunction->next; }
    else { theFunction = theList; }

    /*======================================================*/
    /* Traverse the rest of the function list searching for */
    /* the named function wrapping around if necessary.     */
    /*======================================================*/

    while (strcmp(functionName, theFunction->callFunctionName->contents) != 0) {
        theFunction = theFunction->next;
        if (theFunction == lastFunction) return nullptr;
        if (theFunction == nullptr) theFunction = theList;
    }

    /*=======================*/
    /* Return the pointer to */
    /* the found function.   */
    /*=======================*/

    return (theFunction);
}

/******************************************/
/* Bloaded: Returns true if the current   */
/*   environment is the result of a bload */
/*   command, otherwise returns false.    */
/******************************************/
bool Bloaded(
        const Environment&theEnv) {
    return BloadData(theEnv)->BloadActive;
}

/***************************************/
/* ClearBloadCallback: Clears a binary */
/*   image from the KB environment.    */
/***************************************/
static void ClearBloadCallback(
        const Environment&theEnv,
        void *context) {
    ClearBload(theEnv);
}

/*************************************/
/* ClearBload: Clears a binary image */
/*   from the KB environment.        */
/*************************************/
static bool ClearBload(
        const Environment&theEnv) {
    struct BinaryItem *biPtr;
    struct boolCallFunctionItem *bfPtr;
    bool ready, error;

    /*======================================*/
    /* If bload is not active, then there's */
    /* no need to clear bload data.         */
    /*======================================*/

    if (!BloadData(theEnv)->BloadActive) { return true; }

    /*=================================================*/
    /* Make sure it's safe to clear the bloaded image. */
    /*=================================================*/

    error = false;
    for (bfPtr = BloadData(theEnv)->ClearBloadReadyFunctions;
         bfPtr != nullptr;
         bfPtr = bfPtr->next) {
        ready = (bfPtr->func)(theEnv, nullptr);

        if (!ready) {
            if (!error) {
                PrintErrorID(theEnv, "BLOAD", 5, false);
                WriteString(theEnv, STDERR,
                            "Some constructs are still in use by the current binary image:\n");
            }
            WriteString(theEnv, STDERR, "   ");
            WriteString(theEnv, STDERR, bfPtr->name);
            WriteString(theEnv, STDERR, "\n");
            error = true;
        }
    }

    /*==================================================*/
    /* If some constructs are still in use and can't be */
    /* cleared, indicate the binary load can't continue */
    /* and return false to indicate this condition.     */
    /*==================================================*/

    if (error) {
        WriteString(theEnv, STDERR, "Binary clear cannot continue.\n");
        return false;
    }

    /*=============================*/
    /* Call bload clear functions. */
    /*=============================*/

    for (biPtr = BsaveData(theEnv)->ListOfBinaryItems;
         biPtr != nullptr;
         biPtr = biPtr->next) { if (biPtr->clearFunction != nullptr) (*biPtr->clearFunction)(theEnv); }

    /*===========================*/
    /* Free bloaded expressions. */
    /*===========================*/

    ClearBloadedExpressions(theEnv);

    /*===========================*/
    /* Free bloaded constraints. */
    /*===========================*/

    ClearBloadedConstraints(theEnv);

    /*==================================*/
    /* Remove the bload clear function. */
    /*==================================*/

    BloadData(theEnv)->BloadActive = false;

    /*====================================*/
    /* Return true to indicate the binary */
    /* image was successfully cleared.    */
    /*====================================*/

    return true;
}

/*************************************************/
/* AbortBload: Cleans up effects of before-bload */
/*   functions in event of failure.              */
/*************************************************/
static void AbortBload(
        const Environment&theEnv) {
    struct voidCallFunctionItem *bfPtr;

    for (bfPtr = BloadData(theEnv)->AbortBloadFunctions;
         bfPtr != nullptr;
         bfPtr = bfPtr->next) { (*bfPtr->func)(theEnv, nullptr); }
}

/********************************************/
/* AddBeforeBloadFunction: Adds a function  */
/*   to the list of functions called before */
/*   a binary load occurs.                  */
/********************************************/
void AddBeforeBloadFunction(
        const Environment&theEnv,
        const char *name,
        VoidCallFunction *func,
        int priority,
        void *context) {
    BloadData(theEnv)->BeforeBloadFunctions =
            AddVoidFunctionToCallList(theEnv, name, priority, func, BloadData(theEnv)->BeforeBloadFunctions, context);
}

/*******************************************/
/* AddAfterBloadFunction: Adds a function  */
/*   to the list of functions called after */
/*   a binary load occurs.                 */
/*******************************************/
void AddAfterBloadFunction(
        const Environment&theEnv,
        const char *name,
        VoidCallFunction *func,
        int priority,
        void *context) {
    BloadData(theEnv)->AfterBloadFunctions =
            AddVoidFunctionToCallList(theEnv, name, priority, func, BloadData(theEnv)->AfterBloadFunctions, context);
}

/**************************************************/
/* AddClearBloadReadyFunction: Adds a function to */
/*   the list of functions called to determine if */
/*   a binary image can be cleared.               */
/**************************************************/
void AddClearBloadReadyFunction(
        const Environment&theEnv,
        const char *name,
        BoolCallFunction *func,
        int priority,
        void *context) {
    BloadData(theEnv)->ClearBloadReadyFunctions =
            AddBoolFunctionToCallList(theEnv, name, priority, func,
                                      BloadData(theEnv)->ClearBloadReadyFunctions, context);
}

/*********************************************/
/* AddAbortBloadFunction: Adds a function to */
/*   the list of functions called if a bload */
/*   has to be aborted.                      */
/*********************************************/
void AddAbortBloadFunction(
        const Environment&theEnv,
        const char *name,
        VoidCallFunction *func,
        int priority,
        void *context) {
    BloadData(theEnv)->AbortBloadFunctions =
            AddVoidFunctionToCallList(theEnv, name, priority, func,
                                      BloadData(theEnv)->AbortBloadFunctions, context);
}

/*******************************************************
  NAME         : BloadOutOfMemoryFunction
  DESCRIPTION  : Memory function used by bload to
                   prevent exiting when out of
                   memory - used by BloadandRefresh
  INPUTS       : The memory request size (unused)
  RETURNS      : True (indicates a failure and for
                 the memory functions to simply
                 return a nullptr pointer)
  SIDE EFFECTS : None
  NOTES        : None
 *******************************************************/
static bool BloadOutOfMemoryFunction(
        const Environment&theEnv,
        size_t size) {
#if MAC_XCD
#pragma unused(size,theEnv)
#endif
    return true;
}

/*****************************************************/
/* CannotLoadWithBloadMessage: Generic error message */
/*   for indicating that a construct can't be loaded */
/*   when a binary image is active.                  */
/*****************************************************/
void CannotLoadWithBloadMessage(
        const Environment&theEnv,
        const char *constructName) {
    PrintErrorID(theEnv, "BLOAD", 1, true);
    WriteString(theEnv, STDERR, "Cannot load ");
    WriteString(theEnv, STDERR, constructName);
    WriteString(theEnv, STDERR, " construct with binary load in effect.\n");
}

#endif /* (BLOAD_AND_BSAVE) */

/**************************************/
/* BloadCommand: H/L access routine   */
/*   for the bload command.           */
/**************************************/
void BloadCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
#if (BLOAD_AND_BSAVE)
    const char *fileName;

    fileName = GetFileName(context);
    if (fileName != nullptr) {
        returnValue->lexemeValue = CreateBoolean(theEnv, Bload(theEnv, fileName));
        return;
    }
#else
#if MAC_XCD
#pragma unused(theEnv,context)
#endif
#endif
    returnValue->lexemeValue = FalseSymbol(theEnv);
}
