/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  02/19/20             */
/*                                                     */
/*              CONSTRUCT COMMANDS MODULE              */
/*******************************************************/

/*************************************************************/
/* Purpose: Contains generic routines for deleting, pretty   */
/*   printing, finding, obtaining module information,        */
/*   obtaining lists of constructs, listing constructs, and  */
/*   manipulation routines.                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Modified GetConstructList to remove buffer     */
/*            overflow problem with large construct/module   */
/*            names. DR0858                                  */
/*                                                           */
/*            Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*            Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Corrected an error when compiling as a C++     */
/*            file. DR0868                                   */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added ConstructsDeletable function.            */
/*                                                           */
/*      6.30: Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Change find construct functionality so that    */
/*            imported modules are search when locating a    */
/*            named construct.                               */
/*                                                           */
/*      6.31: Fixed use after free issue for deallocation    */
/*            functions passed to DoForAllConstructs.        */
/*                                                           */
/*      6.40: Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Pretty print functions accept optional logical */
/*            name argument.                                 */
/*                                                           */
/*************************************************************/

#include <cstring>

#include "Setup.h"

#include "Constants.h"
#include "Environment.h"
#include "ExternalFunctions.hxx"
#include "MemoryAllocation.h"
#include "Defmodule.h"
#include "ArgumentAccess.h"
#include "Multifield.h"
#include "DefmoduleUtility.h"
#include "PrintUtility.h"
#include "Router.h"
#include "Utility.h"
#include "CommandLine.h"
#include "SystemDependency.h"

#if BLOAD_AND_BSAVE
#include "BinaryLoad.h"
#endif

#include "Construct.h"
#include "Construct.h"
#include "ReferenceCounted.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if DEBUGGING_FUNCTIONS
static void ConstructPrintWatch(const Environment::Ptr&, const char *, Construct *,
                                ConstructHeader *,
                                ConstructGetWatchFunction *);
static bool ConstructWatchSupport(const Environment::Ptr&, Construct *, const char *,
                                  const char *, Expression *, bool,
                                  bool, ConstructGetWatchFunction *,
                                  ConstructSetWatchFunction *);
#endif


/************************************/
/* AddConstructToModule: Adds a     */
/* construct to the current module. */
/************************************/
void AddConstructToModule(
        ConstructHeader *theConstruct) {
    if (theConstruct->whichModule->lastItem == nullptr) { theConstruct->whichModule->firstItem = theConstruct; }
    else { theConstruct->whichModule->lastItem->next = theConstruct; }

    theConstruct->whichModule->lastItem = theConstruct;
    theConstruct->next = nullptr;
}


/****************************************************/
/* DeleteNamedConstruct: Generic driver routine for */
/*   deleting a specific construct from a module.   */
/****************************************************/
bool DeleteNamedConstruct(
        const Environment::Ptr&theEnv,
        const char *constructName,
        Construct *constructClass) {
    ConstructHeader *constructPtr;

    /*=============================*/
    /* Constructs can't be deleted */
    /* while a bload is in effect. */
    /*=============================*/

#if BLOAD_AND_BSAVE
    if (Bloaded(theEnv)) return false;
#endif

    /*===============================*/
    /* Look for the named construct. */
    /*===============================*/

    constructPtr = (*constructClass->findFunction)(theEnv, constructName);

    /*========================================*/
    /* If the construct was found, delete it. */
    /*========================================*/

    if (constructPtr != nullptr) { return (*constructClass->deleteFunction)(constructPtr, theEnv); }

    /*========================================*/
    /* If the construct wasn't found, but the */
    /* special symbol * was used, then delete */
    /* all constructs of the specified type.  */
    /*========================================*/

    if (strcmp("*", constructName) == 0) {
        (*constructClass->deleteFunction)(nullptr, theEnv);
        return true;
    }

    /*===============================*/
    /* Otherwise, return false to    */
    /* indicate no deletion occured. */
    /*===============================*/

    return false;
}

/********************************************************/
/* FindNamedConstructInModuleOrImports: Generic routine */
/*   for searching for a specified construct.           */
/********************************************************/
ConstructHeader *FindNamedConstructInModuleOrImports(
        const Environment::Ptr&theEnv,
        const char *constructName,
        Construct *constructClass) {
    ConstructHeader *theConstruct;
    unsigned int count;

    /*================================================*/
    /* First look in the current or specified module. */
    /*================================================*/

    theConstruct = FindNamedConstructInModule(theEnv, constructName, constructClass);
    if (theConstruct != nullptr) return theConstruct;

    /*=====================================*/
    /* If there's a module specifier, then */
    /* the construct does not exist.       */
    /*=====================================*/

    if (FindModuleSeparator(constructName)) { return nullptr; }

    /*========================================*/
    /* Otherwise, search in imported modules. */
    /*========================================*/

    theConstruct = FindImportedConstruct(theEnv, constructClass->constructName, nullptr,
                                         constructName, &count, true, nullptr);

    if (count > 1) {
        AmbiguousReferenceErrorMessage(theEnv, constructClass->constructName, constructName);
        return nullptr;
    }

    return theConstruct;
}

/***********************************************/
/* FindNamedConstructInModule: Generic routine */
/*   for searching for a specified construct.  */
/***********************************************/
ConstructHeader *FindNamedConstructInModule(
        const Environment::Ptr&theEnv,
        const char *constructName,
        Construct *constructClass) {
    ConstructHeader *theConstruct;
    CLIPSLexeme *findValue = nullptr;

    /*==========================*/
    /* Save the current module. */
    /*==========================*/

    SaveCurrentModule(theEnv);

    /*=========================================================*/
    /* Extract the construct name. If a module was specified,  */
    /* then ExtractModuleAndConstructName will set the current */
    /* module to the module specified in the name.             */
    /*=========================================================*/

    constructName = ExtractModuleAndConstructName(theEnv, constructName);

    /*=================================================*/
    /* If a valid construct name couldn't be extracted */
    /* or the construct name isn't in the symbol table */
    /* (which means the construct doesn't exist), then */
    /* return nullptr to indicate the specified construct */
    /* couldn't be found.                              */
    /*=================================================*/

    if ((constructName == nullptr) ?
        true :
        ((findValue = FindSymbolHN(theEnv, constructName, SYMBOL_BIT)) == nullptr)) {
        RestoreCurrentModule(theEnv);
        return nullptr;
    }

    /*===============================================*/
    /* If we find the symbol for the construct name, */
    /* but it has a count of 0, then it can't be for */
    /* a construct that's currently defined.         */
    /*===============================================*/

    if (findValue->getCount() == 0) {
        RestoreCurrentModule(theEnv);
        return nullptr;
    }

    /*===============================================*/
    /* Loop through every construct of the specified */
    /* class in the current module checking to see   */
    /* if the construct's name matches the construct */
    /* being sought. If found, restore the current   */
    /* module and return a pointer to the construct. */
    /*===============================================*/

    for (theConstruct = (*constructClass->getNextItemFunction)(theEnv, nullptr);
         theConstruct != nullptr;
         theConstruct = (*constructClass->getNextItemFunction)(theEnv, theConstruct)) {
        if (findValue == (*constructClass->getConstructNameFunction)(theConstruct)) {
            RestoreCurrentModule(theEnv);
            return theConstruct;
        }
    }

    /*=============================*/
    /* Restore the current module. */
    /*=============================*/

    RestoreCurrentModule(theEnv);

    /*====================================*/
    /* Return nullptr to indicated the named */
    /* construct was not found.           */
    /*====================================*/

    return nullptr;
}

/*****************************************/
/* UndefconstructCommand: Driver routine */
/*   for the undef<construct> commands.  */
/*****************************************/
void UndefconstructCommand(
        UDFContext *context,
        const char *command,
        Construct *constructClass) {
    const Environment::Ptr&theEnv = context->environment;
    const char *constructName;
    char buffer[80];

    /*==============================================*/
    /* Get the name of the construct to be deleted. */
    /*==============================================*/

    gensprintf(buffer, "%s name", constructClass->constructName);

    constructName = GetConstructName(context, command, buffer);
    if (constructName == nullptr) return;


    /*=============================================*/
    /* Check to see if the named construct exists. */
    /*=============================================*/

    if (((*constructClass->findFunction)(theEnv, constructName) == nullptr) &&
        (strcmp("*", constructName) != 0)) {
        CantFindItemErrorMessage(theEnv, constructClass->constructName, constructName, true);
        return;
    }

        /*===============================================*/
        /* If the construct does exist, try deleting it. */
        /*===============================================*/

    else if (!DeleteNamedConstruct(theEnv, constructName, constructClass)) {
        CantDeleteItemErrorMessage(theEnv, constructClass->constructName, constructName);
        return;
    }

    return;
}

/******************************************/
/* PPConstructCommand: Driver routine for */
/*   the ppdef<construct> commands.       */
/******************************************/
void PPConstructCommand(
        UDFContext *context,
        const char *command,
        Construct *constructClass,
        UDFValue *returnValue) {
    const Environment::Ptr&theEnv = context->environment;
    const char *constructName;
    const char *logicalName;
    const char *ppForm;
    char buffer[80];

    /*===============================*/
    /* Get the name of the construct */
    /* to be "pretty printed."       */
    /*===============================*/

    gensprintf(buffer, "%s name", constructClass->constructName);

    constructName = GetConstructName(context, command, buffer);
    if (constructName == nullptr) return;

    if (UDFHasNextArgument(context)) {
        logicalName = GetLogicalName(context, STDOUT);
        if (logicalName == nullptr) {
            IllegalLogicalNameMessage(theEnv, command);
            SetHaltExecution(theEnv, true);
            SetEvaluationError(theEnv, true);
            return;
        }
    } else { logicalName = STDOUT; }

    /*================================*/
    /* Call the driver routine for    */
    /* pretty printing the construct. */
    /*================================*/

    if (strcmp(logicalName, "nil") == 0) {
        ppForm = PPConstructNil(theEnv, constructName, constructClass);

        if (ppForm == nullptr) { CantFindItemErrorMessage(theEnv, constructClass->constructName, constructName, true); }

        returnValue->contents = CreateString(theEnv, ppForm);

        return;
    }

    if (!PPConstruct(theEnv, constructName, logicalName, constructClass)) {
        CantFindItemErrorMessage(theEnv, constructClass->constructName, constructName, true);
    }
}

/******************************************************/
/* PPConstructNil: Driver routine for pretty printing */
/*   a construct using the logical name nil.          */
/******************************************************/
const char *PPConstructNil(
        const Environment::Ptr&theEnv,
        const char *constructName,
        Construct *constructClass) {
    ConstructHeader *constructPtr;

    /*==================================*/
    /* Use the construct's name to find */
    /* a pointer to actual construct.   */
    /*==================================*/

    constructPtr = (*constructClass->findFunction)(theEnv, constructName);
    if (constructPtr == nullptr) return nullptr;

    /*==============================================*/
    /* If the pretty print form is nullptr (because of */
    /* conserve-mem), return "" (which indicates    */
    /* the construct was found).                    */
    /*==============================================*/

    if ((*constructClass->getPPFormFunction)(constructPtr) == nullptr) { return ""; }

    /*=================================*/
    /* Return the pretty print string. */
    /*=================================*/

    return (*constructClass->getPPFormFunction)(constructPtr);
}

/***********************************/
/* PPConstruct: Driver routine for */
/*   pretty printing a construct.  */
/***********************************/
bool PPConstruct(
        const Environment::Ptr&theEnv,
        const char *constructName,
        const char *logicalName,
        Construct *constructClass) {
    ConstructHeader *constructPtr;

    /*==================================*/
    /* Use the construct's name to find */
    /* a pointer to actual construct.   */
    /*==================================*/

    constructPtr = (*constructClass->findFunction)(theEnv, constructName);
    if (constructPtr == nullptr) return false;

    /*==============================================*/
    /* If the pretty print form is nullptr (because of */
    /* conserve-mem), return true (which indicates  */
    /* the construct was found).                    */
    /*==============================================*/

    if ((*constructClass->getPPFormFunction)(constructPtr) == nullptr) { return true; }

    /*================================*/
    /* Print the pretty print string. */
    /*================================*/

    WriteString(theEnv, logicalName, (*constructClass->getPPFormFunction)(constructPtr));

    /*=======================================*/
    /* Return true to indicate the construct */
    /* was found and pretty printed.         */
    /*=======================================*/

    return true;
}

/*********************************************/
/* GetConstructModuleCommand: Driver routine */
/*   for def<construct>-module routines      */
/*********************************************/
CLIPSLexeme::Ptr GetConstructModuleCommand(
        UDFContext *context,
        const char *command,
        Construct *constructClass) {
#if STUBBING_INACTIVE
    const Environment::Ptr&theEnv = context->environment;
    const char *constructName;
    char buffer[80];
    Defmodule *constructModule;

    /*=========================================*/
    /* Get the name of the construct for which */
    /* we want to determine its module.        */
    /*=========================================*/

    gensprintf(buffer, "%s name", constructClass->constructName);

    constructName = GetConstructName(context, command, buffer);
    if (constructName == nullptr) return FalseSymbol(theEnv);

    /*==========================================*/
    /* Get a pointer to the construct's module. */
    /*==========================================*/

    constructModule = GetConstructModule(theEnv, constructName, constructClass);
    if (constructModule == nullptr) {
        CantFindItemErrorMessage(theEnv, constructClass->constructName, constructName, true);
        return FalseSymbol(theEnv);
    }

    /*============================================*/
    /* Return the name of the construct's module. */
    /*============================================*/

    return constructModule->header.name;
#endif
    return nullptr;
}

/******************************************/
/* GetConstructModule: Driver routine for */
/*   getting the module for a construct   */
/******************************************/
Defmodule *GetConstructModule(
        const Environment::Ptr&theEnv,
        const char *constructName,
        Construct *constructClass) {
    ConstructHeader *constructPtr;
    unsigned int count;
    unsigned position;
    CLIPSLexeme *theName;

    /*====================================================*/
    /* If the construct name contains a module specifier, */
    /* then get a pointer to the defmodule associated     */
    /* with the specified name.                           */
    /*====================================================*/

    if ((position = FindModuleSeparator(constructName)) != 0) {
        theName = ExtractModuleName(theEnv, position, constructName);
        if (theName != nullptr) { return FindDefmodule(theEnv, theName->contents); }
    }

    /*============================================*/
    /* No module was specified, so search for the */
    /* named construct in the current module and  */
    /* modules from which it imports.             */
    /*============================================*/

    constructPtr = FindImportedConstruct(theEnv, constructClass->constructName, nullptr, constructName,
                                         &count, true, nullptr);
    if (constructPtr == nullptr) return nullptr;

    return (constructPtr->whichModule->theModule);
}

/****************************************/
/* UndefconstructAll: Generic C routine */
/*   for deleting all constructs.       */
/****************************************/
bool UndefconstructAll(
        const Environment::Ptr&theEnv,
        Construct *constructClass) {
    ConstructHeader *currentConstruct, *nextConstruct;
    bool success = true;
    GCBlock gcb;

    /*===================================================*/
    /* Loop through all of the constructs in the module. */
    /*===================================================*/

    GCBlockStart(theEnv, &gcb);

    currentConstruct = (*constructClass->getNextItemFunction)(theEnv, nullptr);
    while (currentConstruct != nullptr) {
        /*==============================*/
        /* Remember the next construct. */
        /*==============================*/

        nextConstruct = (*constructClass->getNextItemFunction)(theEnv, currentConstruct);

        /*=============================*/
        /* Try deleting the construct. */
        /*=============================*/

        if ((*constructClass->isConstructDeletableFunction)(currentConstruct)) {
            RemoveConstructFromModule(theEnv, currentConstruct);
            (*constructClass->freeFunction)(theEnv, currentConstruct);
        } else {
            CantDeleteItemErrorMessage(theEnv, constructClass->constructName,
                                       (*constructClass->getConstructNameFunction)(currentConstruct)->contents);
            success = false;
        }

        /*================================*/
        /* Move on to the next construct. */
        /*================================*/

        currentConstruct = nextConstruct;
    }

    GCBlockEnd(theEnv, &gcb);
    CallPeriodicTasks(theEnv);

    /*============================================*/
    /* Return true if all constructs successfully */
    /* deleted, otherwise false.                  */
    /*============================================*/

    return success;
}

/*************************************/
/* Undefconstruct: Generic C routine */
/*   for deleting a construct.       */
/*************************************/
bool Undefconstruct(
        const Environment::Ptr&theEnv,
        ConstructHeader *theConstruct,
        Construct *constructClass) {
    GCBlock gcb;

    /*================================================*/
    /* Delete all constructs of the specified type if */
    /* the construct pointer is the nullptr pointer.     */
    /*================================================*/

    if (theConstruct == nullptr) { return UndefconstructAll(theEnv, constructClass); }

    /*==================================================*/
    /* Return false if the construct cannot be deleted. */
    /*==================================================*/

    if (!(*constructClass->isConstructDeletableFunction)(theConstruct)) { return false; }

    /*===================================*/
    /* Start a garbage collection block. */
    /*===================================*/

    GCBlockStart(theEnv, &gcb);

    /*===========================*/
    /* Remove the construct from */
    /* the list in its module.   */
    /*===========================*/

    RemoveConstructFromModule(theEnv, theConstruct);

    /*=======================*/
    /* Delete the construct. */
    /*=======================*/

    (*constructClass->freeFunction)(theEnv, theConstruct);

    /*===================================*/
    /* End the garbage collection block. */
    /*===================================*/

    GCBlockEnd(theEnv, &gcb);

    /*=============================*/
    /* Return true to indicate the */
    /* construct was deleted.      */
    /*=============================*/

    return true;
}

/***********************************/
/* SaveConstruct: Generic routine  */
/*   for saving a construct class. */
/***********************************/
void SaveConstruct(
        const Environment::Ptr&theEnv,
        Defmodule *theModule,
        const char *logicalName,
        Construct *constructClass) {
    const char *ppform;
    ConstructHeader *theConstruct;

    /*==========================*/
    /* Save the current module. */
    /*==========================*/

    SaveCurrentModule(theEnv);

    /*===========================*/
    /* Set the current module to */
    /* the one we're examining.  */
    /*===========================*/

    SetCurrentModule(theEnv, theModule);

    /*==============================================*/
    /* Loop through each construct of the specified */
    /* construct class in the module.               */
    /*==============================================*/

    for (theConstruct = (*constructClass->getNextItemFunction)(theEnv, nullptr);
         theConstruct != nullptr;
         theConstruct = (*constructClass->getNextItemFunction)(theEnv, theConstruct)) {
        /*==========================================*/
        /* Print the construct's pretty print form. */
        /*==========================================*/

        ppform = (*constructClass->getPPFormFunction)(theConstruct);
        if (ppform != nullptr) {
            WriteString(theEnv, logicalName, ppform);
            WriteString(theEnv, logicalName, "\n");
        }
    }

    /*=============================*/
    /* Restore the current module. */
    /*=============================*/

    RestoreCurrentModule(theEnv);
}

/*********************************************************/
/* GetConstructModuleName: Generic routine for returning */
/*   the name of the module to which a construct belongs */
/*********************************************************/
const char *GetConstructModuleName(
        ConstructHeader *theConstruct) { return DefmoduleName(theConstruct->whichModule->theModule); }

/*********************************************************/
/* GetConstructNameString: Generic routine for returning */
/*   the name string of a construct.                     */
/*********************************************************/
const char *GetConstructNameString(
        ConstructHeader *theConstruct) { return theConstruct->name->contents; }

/**********************************************************/
/* GetConstructNamePointer: Generic routine for returning */
/*   the name pointer of a construct.                     */
/**********************************************************/
CLIPSLexeme *GetConstructNamePointer(
        ConstructHeader *theConstruct) { return theConstruct->name; }

/************************************************/
/* GetConstructListFunction: Generic Routine    */
/*   for retrieving the constructs in a module. */
/************************************************/
void GetConstructListFunction(
        UDFContext *context,
        UDFValue *returnValue,
        Construct *constructClass) {
    Defmodule *theModule;
    UDFValue result;
    unsigned int numArgs;
    const Environment::Ptr&theEnv = context->environment;

    /*====================================*/
    /* If an argument was given, check to */
    /* see that it's a valid module name. */
    /*====================================*/

    numArgs = UDFArgumentCount(context);
    if (numArgs == 1) {
        /*======================================*/
        /* Only symbols are valid module names. */
        /*======================================*/

        if (!UDFFirstArgument(context, SYMBOL_BIT, &result)) { return; }

        /*===========================================*/
        /* Verify that the named module exists or is */
        /* the symbol * (for obtaining the construct */
        /* list for all modules).                    */
        /*===========================================*/

        if (auto lexValue = std::get<CLIPSLexeme::Ptr>(result.contents); (theModule = FindDefmodule(theEnv, lexValue->contents)) == nullptr) {
            if (strcmp("*", lexValue->contents) != 0) {
                SetMultifieldErrorValue(theEnv, returnValue);
                ExpectedTypeError1(theEnv, UDFContextFunctionName(context), 1, "'defmodule name'");
                return;
            }

            theModule = nullptr;
        }
    }

        /*=====================================*/
        /* Otherwise use the current module to */
        /* generate the construct list.        */
        /*=====================================*/

    else { theModule = GetCurrentModule(theEnv); }

    /*=============================*/
    /* Call the driver routine to  */
    /* get the list of constructs. */
    /*=============================*/

    GetConstructList(theEnv, returnValue, constructClass, theModule);
}

/********************************************/
/* GetConstructList: Generic C Routine for  */
/*   retrieving the constructs in a module. */
/********************************************/
void GetConstructList(
        const Environment::Ptr&theEnv,
        UDFValue *returnValue,
        Construct *constructClass,
        Defmodule *theModule) {
#if STUBBING_INACTIVE
    ConstructHeader *theConstruct;
    unsigned long count = 0;
    Multifield *theList;
    CLIPSLexeme *theName;
    Defmodule *loopModule;
    bool allModules = false;
    size_t largestConstructNameSize = 0, bufferSize = 80;  /* prevents warning */
    char *buffer;

    /*==========================*/
    /* Save the current module. */
    /*==========================*/

    SaveCurrentModule(theEnv);

    /*=======================================*/
    /* If the module specified is nullptr, then */
    /* get all constructs in all modules.    */
    /*=======================================*/

    if (theModule == nullptr) {
        theModule = GetNextDefmodule(theEnv, nullptr);
        allModules = true;
    }

    /*======================================================*/
    /* Count the number of constructs to  be retrieved and  */
    /* determine the buffer size needed to store the        */
    /* module-name::construct-names that will be generated. */
    /*======================================================*/

    loopModule = theModule;
    while (loopModule != nullptr) {
        size_t tempSize;

        /*======================================================*/
        /* Set the current module to the module being examined. */
        /*======================================================*/

        SetCurrentModule(theEnv, loopModule);

        /*===========================================*/
        /* Loop over every construct in the  module. */
        /*===========================================*/

        theConstruct = nullptr;
        largestConstructNameSize = 0;

        while ((theConstruct = (*constructClass->getNextItemFunction)(theEnv, theConstruct)) != nullptr) {
            /*================================*/
            /* Increment the construct count. */
            /*================================*/

            count++;

            /*=================================================*/
            /* Is this the largest construct name encountered? */
            /*=================================================*/

            tempSize = strlen((*constructClass->getConstructNameFunction)(theConstruct)->contents);
            if (tempSize > largestConstructNameSize) { largestConstructNameSize = tempSize; }
        }

        /*========================================*/
        /* Determine the size of the module name. */
        /*========================================*/

        tempSize = strlen(DefmoduleName(loopModule));

        /*======================================================*/
        /* The buffer must be large enough for the module name, */
        /* the largest name of all the constructs, and the ::.  */
        /*======================================================*/

        if ((tempSize + largestConstructNameSize + 5) > bufferSize) { bufferSize = tempSize + largestConstructNameSize + 5; }

        /*=============================*/
        /* Move on to the next module. */
        /*=============================*/

        if (allModules) loopModule = GetNextDefmodule(theEnv, loopModule);
        else loopModule = nullptr;
    }

    /*===========================*/
    /* Allocate the name buffer. */
    /*===========================*/

    buffer = (char *) genalloc(theEnv, bufferSize);

    /*================================*/
    /* Create the multifield value to */
    /* store the construct names.     */
    /*================================*/

    returnValue->begin = 0;
    returnValue->range = count;
    theList = CreateMultifield(theEnv, count);
    returnValue->contents = theList;

    /*===========================*/
    /* Store the construct names */
    /* in the multifield value.  */
    /*===========================*/

    loopModule = theModule;
    count = 0;
    while (loopModule != nullptr) {
        /*============================*/
        /* Set the current module to  */
        /* the module being examined. */
        /*============================*/

        SetCurrentModule(theEnv, loopModule);

        /*===============================*/
        /* Add each construct name found */
        /* in the module to the list.    */
        /*===============================*/

        theConstruct = nullptr;
        while ((theConstruct = (*constructClass->getNextItemFunction)(theEnv, theConstruct)) != nullptr) {
            theName = (*constructClass->getConstructNameFunction)(theConstruct);
            if (allModules) {
                genstrcpy(buffer, DefmoduleName(loopModule));
                genstrcat(buffer, "::");
                genstrcat(buffer, theName->contents);
                theList->contents[count].value = CreateSymbol(theEnv, buffer);
            } else { theList->contents[count].value = CreateSymbol(theEnv, theName->contents); }
            count++;
        }

        /*==================================*/
        /* Move on to the next module (if   */
        /* the list is to contain the names */
        /* of constructs from all modules). */
        /*==================================*/

        if (allModules) loopModule = GetNextDefmodule(theEnv, loopModule);
        else loopModule = nullptr;
    }

    /*=========================*/
    /* Return the name buffer. */
    /*=========================*/

    genfree(theEnv, buffer, bufferSize);

    /*=============================*/
    /* Restore the current module. */
    /*=============================*/

    RestoreCurrentModule(theEnv);
#endif
}

/*********************************************/
/* ListConstructCommand: Generic Routine for */
/*   listing the constructs in a module.     */
/*********************************************/
void ListConstructCommand(
        UDFContext *context,
        Construct *constructClass) {
    Defmodule *theModule;
    UDFValue result;
    unsigned int numArgs;
    const Environment::Ptr&theEnv = context->environment;

    /*====================================*/
    /* If an argument was given, check to */
    /* see that it's a valid module name. */
    /*====================================*/

    numArgs = UDFArgumentCount(context);
    if (numArgs == 1) {
        /*======================================*/
        /* Only symbols are valid module names. */
        /*======================================*/

        if (!UDFFirstArgument(context, SYMBOL_BIT, &result)) { return; }

        /*===========================================*/
        /* Verify that the named module exists or is */
        /* the symbol * (for obtaining the construct */
        /* list for all modules).                    */
        /*===========================================*/

        if (auto lexValue = std::get<CLIPSLexeme::Ptr>(result.contents); (theModule = FindDefmodule(theEnv, lexValue->contents)) == nullptr) {
            if (strcmp("*", lexValue->contents) != 0) {
                ExpectedTypeError1(theEnv, UDFContextFunctionName(context), 1, "'defmodule name'");
                return;
            }

            theModule = nullptr;
        }
    }

        /*=====================================*/
        /* Otherwise use the current module to */
        /* generate the construct list.        */
        /*=====================================*/

    else { theModule = GetCurrentModule(theEnv); }

    /*=========================*/
    /* Call the driver routine */
    /* to list the constructs. */
    /*=========================*/

    ListConstruct(theEnv, constructClass, STDOUT, theModule);
}

/*****************************************/
/* ListConstruct: Generic C Routine for  */
/*   listing the constructs in a module. */
/*****************************************/
void ListConstruct(
        const Environment::Ptr&theEnv,
        Construct *constructClass,
        const char *logicalName,
        Defmodule *theModule) {
    ConstructHeader *constructPtr;
    CLIPSLexeme *constructName;
    unsigned long count = 0;
    bool allModules = false;

    /*==========================*/
    /* Save the current module. */
    /*==========================*/

    SaveCurrentModule(theEnv);

    /*=======================================*/
    /* If the module specified is nullptr, then */
    /* list all constructs in all modules.   */
    /*=======================================*/

    if (theModule == nullptr) {
        theModule = GetNextDefmodule(theEnv, nullptr);
        allModules = true;
    }

    /*==================================*/
    /* Loop through all of the modules. */
    /*==================================*/

    while (theModule != nullptr) {
        /*========================================*/
        /* If we're printing the construct in all */
        /* modules, then preface each module      */
        /* listing with the name of the module.   */
        /*========================================*/

        if (allModules) {
            WriteString(theEnv, logicalName, DefmoduleName(theModule));
            WriteString(theEnv, logicalName, ":\n");
        }

        /*===============================*/
        /* Set the current module to the */
        /* module we're examining.       */
        /*===============================*/

        SetCurrentModule(theEnv, theModule);

        /*===========================================*/
        /* List all of the constructs in the module. */
        /*===========================================*/

        for (constructPtr = (*constructClass->getNextItemFunction)(theEnv, nullptr);
             constructPtr != nullptr;
             constructPtr = (*constructClass->getNextItemFunction)(theEnv, constructPtr)) {
            if (EvaluationData(theEnv)->HaltExecution) return;

            constructName = (*constructClass->getConstructNameFunction)(constructPtr);

            if (constructName != nullptr) {
                if (allModules) WriteString(theEnv, STDOUT, "   ");
                WriteString(theEnv, logicalName, constructName->contents);
                WriteString(theEnv, logicalName, "\n");
            }

            count++;
        }

        /*====================================*/
        /* Move on to the next module (if the */
        /* listing is to contain the names of */
        /* constructs from all modules).      */
        /*====================================*/

        if (allModules) theModule = GetNextDefmodule(theEnv, theModule);
        else theModule = nullptr;
    }

    /*=================================================*/
    /* Print the tally and restore the current module. */
    /*=================================================*/

    PrintTally(theEnv, STDOUT, count, constructClass->constructName,
               constructClass->pluralName);

    RestoreCurrentModule(theEnv);
}

/**********************************************************/
/* SetNextConstruct: Sets the next field of one construct */
/*   to point to another construct of the same type.      */
/**********************************************************/
void SetNextConstruct(
        ConstructHeader *theConstruct,
        ConstructHeader *targetConstruct) { theConstruct->next = targetConstruct; }

/********************************************************************/
/* GetConstructModuleItem: Returns the construct module for a given */
/*   construct (note that this is a pointer to a data structure     */
/*   like the deffactsModule, not a pointer to an environment       */
/*   module which contains a number of types of constructs.         */
/********************************************************************/
struct defmoduleItemHeader *GetConstructModuleItem(
        ConstructHeader *theConstruct) { return (theConstruct->whichModule); }

/*************************************************/
/* GetConstructPPForm: Returns the pretty print  */
/*   representation for the specified construct. */
/*************************************************/
const char *GetConstructPPForm(
        ConstructHeader *theConstruct) {
    return theConstruct->ppForm;
}

/****************************************************/
/* GetNextConstructItem: Returns the next construct */
/*   items from a list of constructs.               */
/****************************************************/
ConstructHeader *GetNextConstructItem(
        const Environment::Ptr&theEnv,
        ConstructHeader *theConstruct,
        unsigned moduleIndex) {
    struct defmoduleItemHeader *theModuleItem;

    if (theConstruct == nullptr) {
        theModuleItem = (defmoduleItemHeader *)
                GetModuleItem(theEnv, nullptr, moduleIndex);
        if (theModuleItem == nullptr) return nullptr;
        return (theModuleItem->firstItem);
    }

    return (theConstruct->next);
}

/*******************************************************/
/* GetConstructModuleItemByIndex: Returns a pointer to */
/*  the defmodule item for the specified construct. If */
/*  theModule is nullptr, then the construct module item  */
/*  for the current module is returned, otherwise the  */
/*  construct module item for the specified construct  */
/*  is returned.                                       */
/*******************************************************/
struct defmoduleItemHeader *GetConstructModuleItemByIndex(
        const Environment::Ptr&theEnv,
        Defmodule *theModule,
        unsigned moduleIndex) {
    if (theModule != nullptr) {
        return ((defmoduleItemHeader *)
                GetModuleItem(theEnv, theModule, moduleIndex));
    }

    return ((defmoduleItemHeader *)
            GetModuleItem(theEnv, GetCurrentModule(theEnv), moduleIndex));
}

/******************************************/
/* FreeConstructHeaderModule: Deallocates */
/*   the data structures associated with  */
/*   the construct module item header.    */
/******************************************/
void FreeConstructHeaderModule(
        const Environment::Ptr&theEnv,
        struct defmoduleItemHeader *theModuleItem,
        Construct *constructClass) {
    ConstructHeader *thisOne, *nextOne;

    thisOne = theModuleItem->firstItem;

    while (thisOne != nullptr) {
        nextOne = thisOne->next;
        (*constructClass->freeFunction)(theEnv, thisOne);
        thisOne = nextOne;
    }
}

/**********************************************/
/* DoForAllConstructs: Executes an action for */
/*   all constructs of a specified type.      */
/**********************************************/
void DoForAllConstructs(
        const Environment::Ptr&theEnv,
        void (*actionFunction)(const Environment::Ptr&, ConstructHeader *, void *),
        unsigned moduleItemIndex,
        bool interruptable,
        void *userBuffer) {
    ConstructHeader *theConstruct, *next = nullptr;
    struct defmoduleItemHeader *theModuleItem;
    Defmodule *theModule;
    long moduleCount = 0L;

    /*==========================*/
    /* Save the current module. */
    /*==========================*/

    SaveCurrentModule(theEnv);

    /*==================================*/
    /* Loop through all of the modules. */
    /*==================================*/

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule), moduleCount++) {
        /*=============================*/
        /* Set the current module to   */
        /* the module we're examining. */
        /*=============================*/

        SetCurrentModule(theEnv, theModule);

        /*================================================*/
        /* Perform the action for each of the constructs. */
        /*================================================*/

        theModuleItem = (defmoduleItemHeader *)
                GetModuleItem(theEnv, theModule, moduleItemIndex);

        for (theConstruct = theModuleItem->firstItem;
             theConstruct != nullptr;
             theConstruct = next) {
            /*==========================================*/
            /* Check to see iteration should be halted. */
            /*==========================================*/

            if (interruptable) {
                if (GetHaltExecution(theEnv)) {
                    RestoreCurrentModule(theEnv);
                    return;
                }
            }

            /*===============================================*/
            /* Determine the next construct since the action */
            /* could delete the current construct.           */
            /*===============================================*/

            next = theConstruct->next;

            /*===============================================*/
            /* Perform the action for the current construct. */
            /*===============================================*/

            (*actionFunction)(theEnv, theConstruct, userBuffer);
        }
    }

    /*=============================*/
    /* Restore the current module. */
    /*=============================*/

    RestoreCurrentModule(theEnv);
}

/******************************************************/
/* DoForAllConstructsInModule: Executes an action for */
/*   all constructs of a specified type in a module.  */
/******************************************************/
void DoForAllConstructsInModule(
        const Environment::Ptr&theEnv,
        Defmodule *theModule,
        ConstructActionFunction *actionFunction,
        unsigned moduleItemIndex,
        bool interruptable,
        void *userBuffer) {
    ConstructHeader *theConstruct;
    struct defmoduleItemHeader *theModuleItem;

    /*==========================*/
    /* Save the current module. */
    /*==========================*/

    SaveCurrentModule(theEnv);

    /*=============================*/
    /* Set the current module to   */
    /* the module we're examining. */
    /*=============================*/

    SetCurrentModule(theEnv, theModule);

    /*================================================*/
    /* Perform the action for each of the constructs. */
    /*================================================*/

    theModuleItem = (defmoduleItemHeader *)
            GetModuleItem(theEnv, theModule, moduleItemIndex);

    for (theConstruct = theModuleItem->firstItem;
         theConstruct != nullptr;
         theConstruct = theConstruct->next) {
        if (interruptable) {
            if (GetHaltExecution(theEnv)) {
                RestoreCurrentModule(theEnv);
                return;
            }
        }

        (*actionFunction)(theEnv, theConstruct, userBuffer);
    }

    /*=============================*/
    /* Restore the current module. */
    /*=============================*/

    RestoreCurrentModule(theEnv);
}

/*****************************************************/
/* InitializeConstructHeader: Initializes construct  */
/*   header info, including to which module item the */
/*   new construct belongs                           */
/*****************************************************/
void InitializeConstructHeader(
        const Environment::Ptr&theEnv,
        const char *constructNameString,
        ConstructType theType,
        ConstructHeader *theConstruct,
        CLIPSLexeme *theConstructName) {
    struct moduleItem *theModuleItem;
    struct defmoduleItemHeader *theItemHeader;

    theModuleItem = FindModuleItem(theEnv, constructNameString);
    theItemHeader = (defmoduleItemHeader *)
            GetModuleItem(theEnv, nullptr, theModuleItem->moduleIndex);

    theConstruct->whichModule = theItemHeader;
    theConstruct->name = theConstructName;
    theConstruct->ppForm = nullptr;
    theConstruct->bsaveID = 0L;
    theConstruct->next = nullptr;
    theConstruct->usrData = nullptr;
    theConstruct->constructType = theType;
    theConstruct->env = theEnv;
}

/*************************************************/
/* SetConstructPPForm: Sets a construct's pretty */
/*   print form and deletes the old one.         */
/*************************************************/
void SetConstructPPForm(
        const Environment::Ptr&theEnv,
        ConstructHeader *theConstruct,
        const char *ppForm) {
    if (theConstruct->ppForm != nullptr) {
        rm(theEnv, (void *) theConstruct->ppForm,
           ((strlen(theConstruct->ppForm) + 1) * sizeof(char)));
    }
    theConstruct->ppForm = ppForm;
}

#if DEBUGGING_FUNCTIONS

/******************************************************/
/* ConstructPrintWatchAccess: Provides an interface   */
/*   to the list-watch-items function for a construct */
/******************************************************/
bool ConstructPrintWatchAccess(
        const Environment::Ptr&theEnv,
        Construct *constructClass,
        const char *logName,
        Expression *argExprs,
        ConstructGetWatchFunction *getWatchFunc,
        ConstructSetWatchFunction *setWatchFunc) {
    return (ConstructWatchSupport(theEnv, constructClass, "list-watch-items", logName, argExprs,
                                  false, false, getWatchFunc, setWatchFunc));
}

/**************************************************/
/* ConstructSetWatchAccess: Provides an interface */
/*   to the watch function for a construct        */
/**************************************************/
bool ConstructSetWatchAccess(
        const Environment::Ptr&theEnv,
        Construct *constructClass,
        bool newState,
        Expression *argExprs,
        ConstructGetWatchFunction *getWatchFunc,
        ConstructSetWatchFunction *setWatchFunc) {
    return (ConstructWatchSupport(theEnv, constructClass, "watch", STDERR, argExprs,
                                  true, newState, getWatchFunc, setWatchFunc));
}

/******************************************************/
/* ConstructWatchSupport: Generic construct interface */
/*   into watch and list-watch-items.                 */
/******************************************************/
static bool ConstructWatchSupport(
        const Environment::Ptr&theEnv,
        Construct *constructClass,
        const char *funcName,
        const char *logName,
        Expression *argExprs,
        bool setFlag,
        bool newState,
        ConstructGetWatchFunction *getWatchFunc,
        ConstructSetWatchFunction *setWatchFunc) {
#if STUBBING_INACTIVE
    Defmodule *theModule;
    ConstructHeader *theConstruct = nullptr;
    UDFValue constructName;
    unsigned int argIndex = 2;

    /*========================================*/
    /* If no constructs are specified, then   */
    /* show/set the trace for all constructs. */
    /*========================================*/

    if (argExprs == nullptr) {
        /*==========================*/
        /* Save the current module. */
        /*==========================*/

        SaveCurrentModule(theEnv);

        /*===========================*/
        /* Loop through each module. */
        /*===========================*/

        for (theModule = GetNextDefmodule(theEnv, nullptr);
             theModule != nullptr;
             theModule = GetNextDefmodule(theEnv, theModule)) {
            /*============================*/
            /* Set the current module to  */
            /* the module being examined. */
            /*============================*/

            SetCurrentModule(theEnv, theModule);

            /*====================================================*/
            /* If we're displaying the names of constructs with   */
            /* watch flags enabled, then preface each module      */
            /* listing of constructs with the name of the module. */
            /*====================================================*/

            if (!setFlag) {
                WriteString(theEnv, logName, DefmoduleName(theModule));
                WriteString(theEnv, logName, ":\n");
            }

            /*============================================*/
            /* Loop through each construct in the module. */
            /*============================================*/

            for (theConstruct = (*constructClass->getNextItemFunction)(theEnv, nullptr);
                 theConstruct != nullptr;
                 theConstruct = (*constructClass->getNextItemFunction)(theEnv, theConstruct)) {
                /*=============================================*/
                /* Either set the watch flag for the construct */
                /* or display its current state.               */
                /*=============================================*/

                if (setFlag) { (*setWatchFunc)(theConstruct, newState); }
                else {
                    WriteString(theEnv, logName, "   ");
                    ConstructPrintWatch(theEnv, logName, constructClass, theConstruct, getWatchFunc);
                }
            }
        }

        /*=============================*/
        /* Restore the current module. */
        /*=============================*/

        RestoreCurrentModule(theEnv);

        /*====================================*/
        /* Return true to indicate successful */
        /* completion of the command.         */
        /*====================================*/

        return true;
    }

    /*==================================================*/
    /* Show/set the trace for each specified construct. */
    /*==================================================*/

    while (argExprs != nullptr) {
        /*==========================================*/
        /* Evaluate the argument that should be a   */
        /* construct name. Return false is an error */
        /* occurs when evaluating the argument.     */
        /*==========================================*/

        if (EvaluateExpression(theEnv, argExprs, &constructName)) { return false; }

        /*================================================*/
        /* Check to see that it's a valid construct name. */
        /*================================================*/

        if ((constructName.header->type != SYMBOL_TYPE) ? true :
            ((theConstruct = LookupConstruct(theEnv, constructClass,
                                             constructName.lexemeValue->contents, true)) == nullptr)) {
            ExpectedTypeError1(theEnv, funcName, argIndex, constructClass->constructName);
            return false;
        }

        /*=============================================*/
        /* Either set the watch flag for the construct */
        /* or display its current state.               */
        /*=============================================*/

        if (setFlag) { (*setWatchFunc)(theConstruct, newState); }
        else { ConstructPrintWatch(theEnv, logName, constructClass, theConstruct, getWatchFunc); }

        /*===============================*/
        /* Move on to the next argument. */
        /*===============================*/

        argIndex++;
        argExprs = GetNextArgument(argExprs);
    }

    /*====================================*/
    /* Return true to indicate successful */
    /* completion of the command.         */
    /*====================================*/

    return true;
#endif
    return false;
}

/*************************************************/
/* ConstructPrintWatch: Displays the trace value */
/*   of a construct for list-watch-items         */
/*************************************************/
static void ConstructPrintWatch(
        const Environment::Ptr&theEnv,
        const char *logName,
        Construct *constructClass,
        ConstructHeader *theConstruct,
        ConstructGetWatchFunction *getWatchFunc) {
    WriteString(theEnv, logName, (*constructClass->getConstructNameFunction)(theConstruct)->contents);
    if ((*getWatchFunc)(theConstruct))
        WriteString(theEnv, logName, " = on\n");
    else
        WriteString(theEnv, logName, " = off\n");
}

#endif /* DEBUGGING_FUNCTIONS */

/*****************************************************/
/* LookupConstruct: Finds a construct in the current */
/*   or imported modules. If specified, will also    */
/*   look for construct in a non-imported module.    */
/*****************************************************/
ConstructHeader *LookupConstruct(
        const Environment::Ptr&theEnv,
        Construct *constructClass,
        const char *constructName,
        bool moduleNameAllowed) {
    ConstructHeader *theConstruct;
    const char *constructType;
    unsigned int moduleCount;

    /*============================================*/
    /* Look for the specified construct in the    */
    /* current module or in any imported modules. */
    /*============================================*/

    constructType = constructClass->constructName;
    theConstruct = FindImportedConstruct(theEnv, constructType, nullptr, constructName,
                                         &moduleCount, true, nullptr);

    /*===========================================*/
    /* Return nullptr if the reference is ambiguous */
    /* (it was found in more than one module).   */
    /*===========================================*/

    if (theConstruct != nullptr) {
        if (moduleCount > 1) {
            AmbiguousReferenceErrorMessage(theEnv, constructType, constructName);
            return nullptr;
        }
        return (theConstruct);
    }

    /*=============================================*/
    /* If specified, check to see if the construct */
    /* is in a non-imported module.                */
    /*=============================================*/

    if (moduleNameAllowed && FindModuleSeparator(constructName)) { theConstruct = (*constructClass->findFunction)(theEnv, constructName); }

    /*====================================*/
    /* Return a pointer to the construct. */
    /*====================================*/

    return (theConstruct);
}

/***********************************************************/
/* ConstructsDeletable: Returns a boolean value indicating */
/*   whether constructs in general can be deleted.         */
/***********************************************************/
bool ConstructsDeletable(
        const Environment::Ptr&theEnv) {
#if (!BLOAD_AND_BSAVE)
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif

#if BLOAD_AND_BSAVE
    return !Bloaded(theEnv);
#endif
}
