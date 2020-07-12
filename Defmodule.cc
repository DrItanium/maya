/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  10/01/16             */
/*                                                     */
/*                  DEFMODULE MODULE                   */
/*******************************************************/

/*************************************************************/
/* Purpose: Defines basic defmodule primitive functions such */
/*   as allocating and deallocating, traversing, and finding */
/*   defmodule data structures.                              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
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
/*            Callbacks must be environment aware.           */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#include <cstdio>
#include <cstring>

#include "ArgumentAccess.h"
#if BLOAD_AND_BSAVE
#include "BinaryLoad.h"
#include "DefmoduleBinarySaveLoad.h"
#endif
#include "Constants.h"
#include "Construct.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "MemoryAllocation.h"
#include "DefModuleBasicCommands.h"
#include "DefmoduleParser.h"
#include "PrintUtility.h"
#include "Router.h"
#include "Utility.h"

#include "Defmodule.h"
#include "ReferenceCounted.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void ReturnDefmodule(const Environment&, Defmodule *, bool);
static void DeallocateDefmoduleData(const Environment&);

/************************************************/
/* AllocateDefmoduleGlobals: Initializes global */
/*   variables used by the defmodule construct. */
/************************************************/
void AllocateDefmoduleGlobals(
        const Environment&theEnv) {
    //AllocateEnvironmentData(theEnv, DEFMODULE_DATA, sizeof(defmoduleData));
    theEnv->allocateEnvironmentModule<defmoduleData>();
    /// @todo DeallocateDefmoduleData is the dtor for defmoduleData
    //AddEnvironmentCleanupFunction(theEnv, "defmodules", DeallocateDefmoduleData, -1000);
    DefmoduleData(theEnv)->CallModuleChangeFunctions = true;
    DefmoduleData(theEnv)->MainModuleRedefinable = true;
}

#if STUBBING_INACTIVE
/****************************************************/
/* DeallocateDefmoduleData: Deallocates environment */
/*    data for the defmodule construct.             */
/****************************************************/
static void DeallocateDefmoduleData(
        const Environment&theEnv) {
    struct moduleStackItem *tmpMSPtr, *nextMSPtr;
    struct moduleItem *tmpMIPtr, *nextMIPtr;
    Defmodule *tmpDMPtr, *nextDMPtr;
    struct portConstructItem *tmpPCPtr, *nextPCPtr;
#if (BLOAD_AND_BSAVE)
    unsigned int i;
    size_t space;
#endif

#if (BLOAD_AND_BSAVE)
    for (i = 0; i < DefmoduleData(theEnv)->BNumberOfDefmodules; i++) {
        if (DefmoduleData(theEnv)->DefmoduleArray[i].itemsArray != nullptr) {
            rm(theEnv, DefmoduleData(theEnv)->DefmoduleArray[i].itemsArray,
               sizeof(void *) * GetNumberOfModuleItems(theEnv));
        }
    }

    space = DefmoduleData(theEnv)->BNumberOfDefmodules * sizeof(Defmodule);
    if (space != 0) {
        genfree(theEnv, DefmoduleData(theEnv)->DefmoduleArray, space);
        DefmoduleData(theEnv)->ListOfDefmodules = nullptr;
    }

    space = DefmoduleData(theEnv)->NumberOfPortItems * sizeof(portItem);
    if (space != 0) genfree(theEnv, DefmoduleData(theEnv)->PortItemArray, space);
#endif

    tmpDMPtr = DefmoduleData(theEnv)->ListOfDefmodules;
    while (tmpDMPtr != nullptr) {
        nextDMPtr = (Defmodule *) tmpDMPtr->header.next;
        ReturnDefmodule(theEnv, tmpDMPtr, true);
        tmpDMPtr = nextDMPtr;
    }

    tmpPCPtr = DefmoduleData(theEnv)->ListOfPortConstructItems;
    while (tmpPCPtr != nullptr) {
        nextPCPtr = tmpPCPtr->next;
        rtn_struct(theEnv, portConstructItem, tmpPCPtr);
        tmpPCPtr = nextPCPtr;
    }

    tmpMSPtr = DefmoduleData(theEnv)->ModuleStack;
    while (tmpMSPtr != nullptr) {
        nextMSPtr = tmpMSPtr->next;
        rtn_struct(theEnv, moduleStackItem, tmpMSPtr);
        tmpMSPtr = nextMSPtr;
    }

    tmpMIPtr = DefmoduleData(theEnv)->ListOfModuleItems;
    while (tmpMIPtr != nullptr) {
        nextMIPtr = tmpMIPtr->next;
        rtn_struct(theEnv, moduleItem, tmpMIPtr);
        tmpMIPtr = nextMIPtr;
    }

    DeallocateVoidCallList(theEnv, DefmoduleData(theEnv)->AfterModuleDefinedFunctions);
    DeallocateVoidCallList(theEnv, DefmoduleData(theEnv)->AfterModuleChangeFunctions);
}
/**************************************************************/
/* InitializeDefmodules: Initializes the defmodule construct. */
/**************************************************************/
void InitializeDefmodules(
        const Environment&theEnv) {
    DefmoduleBasicCommands(theEnv);

    CreateMainModule(theEnv, nullptr);

#if DEFMODULE_CONSTRUCT
    AddConstruct(theEnv, "defmodule", "defmodules", ParseDefmodule, nullptr, nullptr, nullptr, nullptr,
                 nullptr, nullptr, nullptr, nullptr, nullptr);
#endif

#if DEFMODULE_CONSTRUCT
    AddUDF(theEnv, "get-current-module", "y", 0, 0, nullptr, GetCurrentModuleCommand);

    AddUDF(theEnv, "set-current-module", "y", 1, 1, "y", SetCurrentModuleCommand);
#endif
}
#endif
#if STUBBING_INACTIVE
/******************************************************/
/* RegisterModuleItem: Called to register a construct */
/*   which can be placed within a module.             */
/******************************************************/
unsigned
RegisterModuleItem(const Environment&theEnv,
                   const char *theItem,
                   AllocateModuleFunction *allocateFunction,
                   FreeModuleFunction *freeFunction,
                   void *(*bloadModuleReference)(const Environment&, unsigned long),
                   FindConstructFunction *findFunction) {
    struct moduleItem *newModuleItem;

    newModuleItem = get_struct(theEnv, moduleItem);
    newModuleItem->name = theItem;
    newModuleItem->allocateFunction = allocateFunction;
    newModuleItem->freeFunction = freeFunction;
    newModuleItem->bloadModuleReference = bloadModuleReference;
    newModuleItem->findFunction = findFunction;
    newModuleItem->moduleIndex = DefmoduleData(theEnv)->NumberOfModuleItems++;
    newModuleItem->next = nullptr;

    if (DefmoduleData(theEnv)->LastModuleItem == nullptr) {
        DefmoduleData(theEnv)->ListOfModuleItems = newModuleItem;
        DefmoduleData(theEnv)->LastModuleItem = newModuleItem;
    } else {
        DefmoduleData(theEnv)->LastModuleItem->next = newModuleItem;
        DefmoduleData(theEnv)->LastModuleItem = newModuleItem;
    }

    return newModuleItem->moduleIndex;
}
#endif
/***********************************************************/
/* GetListOfModuleItems: Returns the list of module items. */
/***********************************************************/
struct moduleItem *GetListOfModuleItems(
        const Environment&theEnv) {
    return (DefmoduleData(theEnv)->ListOfModuleItems);
}

/***************************************************************/
/* GetNumberOfModuleItems: Returns the number of module items. */
/***************************************************************/
unsigned GetNumberOfModuleItems(
        const Environment&theEnv) {
    return DefmoduleData(theEnv)->NumberOfModuleItems;
}
#if STUBBING_INACTIVE
/********************************************************/
/* FindModuleItem: Finds the module item data structure */
/*   corresponding to the specified name.               */
/********************************************************/
struct moduleItem *FindModuleItem(
        const Environment&theEnv,
        const char *theName) {
    struct moduleItem *theModuleItem;

    for (theModuleItem = DefmoduleData(theEnv)->ListOfModuleItems;
         theModuleItem != nullptr;
         theModuleItem = theModuleItem->next) { if (strcmp(theModuleItem->name, theName) == 0) return (theModuleItem); }

    return nullptr;
}

/***************************************/
/* GetCurrentModule: Returns a pointer */
/*   to the current module.            */
/***************************************/
Defmodule *GetCurrentModule(
        const Environment&theEnv) {
    return DefmoduleData(theEnv)->CurrentModule;
}
#endif

/***********************************************************/
/* SetCurrentModule: Sets the value of the current module. */
/***********************************************************/
Defmodule *SetCurrentModule(
        const Environment&theEnv,
        Defmodule *newModule) {
#if STUBBING_INACTIVE
    struct voidCallFunctionItem *changeFunctions;

    /*=============================================*/
    /* Change the current module to the specified  */
    /* module and save the previous current module */
    /* for the return value.                       */
    /*=============================================*/

    auto oldModule = DefmoduleData(theEnv)->CurrentModule;
    DefmoduleData(theEnv)->CurrentModule = newModule;

    /*==========================================================*/
    /* Call the list of registered functions that need to know  */
    /* when the module has changed. The module change functions */
    /* should only be called if this is a "real" module change. */
    /* Many routines temporarily change the module to look for  */
    /* constructs, etc. The SaveCurrentModule function will     */
    /* disable the change functions from being called.          */
    /*==========================================================*/

    if (DefmoduleData(theEnv)->CallModuleChangeFunctions) {
        DefmoduleData(theEnv)->ModuleChangeIndex++;
        changeFunctions = DefmoduleData(theEnv)->AfterModuleChangeFunctions;
        while (changeFunctions != nullptr) {
            (*changeFunctions->func)(theEnv, nullptr);
            changeFunctions = changeFunctions->next;
        }
    }

    /*=====================================*/
    /* Return the previous current module. */
    /*=====================================*/

    return oldModule;
#endif
    return nullptr;
}
#if STUBBING_INACTIVE
/********************************************************/
/* SaveCurrentModule: Saves current module on stack and */
/*   prevents SetCurrentModule() from calling change    */
/*   functions                                          */
/********************************************************/
void SaveCurrentModule(
        const Environment&theEnv) {
    auto tmp = get_struct(theEnv, moduleStackItem);
    tmp->changeFlag = DefmoduleData(theEnv)->CallModuleChangeFunctions;
    DefmoduleData(theEnv)->CallModuleChangeFunctions = false;
    tmp->theModule = DefmoduleData(theEnv)->CurrentModule;
    tmp->next = DefmoduleData(theEnv)->ModuleStack;
    DefmoduleData(theEnv)->ModuleStack = tmp;
}

/**********************************************************/
/* RestoreCurrentModule: Restores saved module and resets */
/*   ability of SetCurrentModule() to call changed        */
/*   functions to previous state                          */
/**********************************************************/
void RestoreCurrentModule(const Environment&theEnv) {
    auto tmp = DefmoduleData(theEnv)->ModuleStack;
    DefmoduleData(theEnv)->ModuleStack = tmp->next;
    DefmoduleData(theEnv)->CallModuleChangeFunctions = tmp->changeFlag;
    DefmoduleData(theEnv)->CurrentModule = tmp->theModule;
    rtn_struct(theEnv, moduleStackItem, tmp);
}

/*************************************************************/
/* GetModuleItem: Returns the data pointer for the specified */
/*   module item in the specified module. If no module is    */
/*   indicated, then the module item for the current module  */
/*   is returned.                                            */
/*************************************************************/
void *GetModuleItem(
        const Environment&theEnv,
        Defmodule *theModule,
        unsigned moduleItemIndex) {
    if (theModule == nullptr) {
        if (DefmoduleData(theEnv)->CurrentModule == nullptr) return nullptr;
        theModule = DefmoduleData(theEnv)->CurrentModule;
    }

    if (theModule->itemsArray == nullptr) return (nullptr);

    return ((void *) theModule->itemsArray[moduleItemIndex]);
}

/************************************************************/
/* SetModuleItem: Sets the data pointer for the specified   */
/*   module item in the specified module. If no module is   */
/*   indicated, then the module item for the current module */
/*   is returned.                                           */
/************************************************************/
void SetModuleItem(
        const Environment&theEnv,
        Defmodule *theModule,
        unsigned moduleItemIndex,
        void *newValue) {
    if (theModule == nullptr) {
        if (DefmoduleData(theEnv)->CurrentModule == nullptr) return;
        theModule = DefmoduleData(theEnv)->CurrentModule;
    }

    if (theModule->itemsArray == nullptr) return;
    theModule->itemsArray[moduleItemIndex] = (defmoduleItemHeader *) newValue;
}

/******************************************************/
/* CreateMainModule: Creates the default MAIN module. */
/******************************************************/
void CreateMainModule(
        const Environment&theEnv,
        void *context) {
    Defmodule *newDefmodule;
    struct moduleItem *theItem;
    unsigned int i;
    struct defmoduleItemHeader *theHeader;

    /*=======================================*/
    /* Allocate the defmodule data structure */
    /* and name it the MAIN module.          */
    /*=======================================*/

    newDefmodule = get_struct(theEnv, defmodule);
    zeroMemory(newDefmodule);
    newDefmodule->header.name = CreateSymbol(theEnv, "MAIN");
    IncrementLexemeCount(newDefmodule->header.name);
    newDefmodule->header.whichModule = nullptr;
    newDefmodule->header.next = nullptr;
    newDefmodule->header.ppForm = nullptr;
    newDefmodule->importList = nullptr;
    newDefmodule->exportList = nullptr;
    newDefmodule->header.bsaveID = 0L;
    newDefmodule->header.usrData = nullptr;
    newDefmodule->header.constructType = DEFMODULE;
    newDefmodule->header.env = theEnv;

    /*==================================*/
    /* Initialize the array for storing */
    /* the module's construct lists.    */
    /*==================================*/

    if (DefmoduleData(theEnv)->NumberOfModuleItems == 0) newDefmodule->itemsArray = nullptr;
    else {
        newDefmodule->itemsArray = (defmoduleItemHeader **)
                gm2(theEnv, sizeof(void *) * DefmoduleData(theEnv)->NumberOfModuleItems);
        for (i = 0, theItem = DefmoduleData(theEnv)->ListOfModuleItems;
             (i < DefmoduleData(theEnv)->NumberOfModuleItems) && (theItem != nullptr);
             i++, theItem = theItem->next) {
            if (theItem->allocateFunction == nullptr) { newDefmodule->itemsArray[i] = nullptr; }
            else {
                newDefmodule->itemsArray[i] = (defmoduleItemHeader *)
                        (*theItem->allocateFunction)(theEnv);
                theHeader = (defmoduleItemHeader *) newDefmodule->itemsArray[i];
                theHeader->theModule = newDefmodule;
                theHeader->firstItem = nullptr;
                theHeader->lastItem = nullptr;
            }
        }
    }

    /*=======================================*/
    /* Add the module to the list of modules */
    /* and make it the current module.       */
    /*=======================================*/

#if DEFMODULE_CONSTRUCT
    SetNumberOfDefmodules(theEnv, 1);
#endif

    DefmoduleData(theEnv)->LastDefmodule = newDefmodule;
    DefmoduleData(theEnv)->ListOfDefmodules = newDefmodule;
    SetCurrentModule(theEnv, newDefmodule);
}

/*********************************************************************/
/* SetListOfDefmodules: Sets the list of defmodules to the specified */
/*   value. Normally used when initializing a run-time module or     */
/*   when bloading a binary file to install the list of defmodules.  */
/*********************************************************************/
void SetListOfDefmodules(
        const Environment&theEnv,
        Defmodule *defmodulePtr) {
    DefmoduleData(theEnv)->ListOfDefmodules = defmodulePtr;
    DefmoduleData(theEnv)->LastDefmodule = DefmoduleData(theEnv)->ListOfDefmodules;

    if (DefmoduleData(theEnv)->LastDefmodule == nullptr) return;
    DefmoduleData(theEnv)->LastDefmodule->header.env = theEnv;

    while (DefmoduleData(theEnv)->LastDefmodule->header.next != nullptr) {
        DefmoduleData(theEnv)->LastDefmodule = (Defmodule *) DefmoduleData(theEnv)->LastDefmodule->header.next;
        DefmoduleData(theEnv)->LastDefmodule->header.env = theEnv;
    }
}
#endif
/*******************************************************************/
/* GetNextDefmodule: If passed a nullptr pointer, returns the first   */
/*   defmodule in the ListOfDefmodules. Otherwise returns the next */
/*   defmodule following the defmodule passed as an argument.      */
/*******************************************************************/
Defmodule *GetNextDefmodule(
        const Environment&theEnv,
        Defmodule *defmodulePtr) {
#if STUBBING_INACTIVE
    if (defmodulePtr == nullptr) { return DefmoduleData(theEnv)->ListOfDefmodules; }
    else { return (Defmodule *) defmodulePtr->header.next; }
#endif
    return nullptr;
}
#if STUBBING_INACTIVE
/***********************************/
/* DefmoduleName: Returns the name */
/*   of the specified defmodule.   */
/***********************************/
const char *DefmoduleName(
        Defmodule *defmodulePtr) {
    return defmodulePtr->header.name->contents;
}

/************************************************/
/* DefmodulePPForm: Returns the pretty print    */
/*   representation of the specified defmodule. */
/************************************************/
const char *DefmodulePPForm(
        Defmodule *defmodulePtr) {
    return defmodulePtr->header.ppForm;
}


/***********************************************/
/* RemoveAllDefmodules: Removes all defmodules */
/*   from the current environment.             */
/***********************************************/
void RemoveAllDefmodules(
        const Environment&theEnv,
        void *context) {
    Defmodule *nextDefmodule;

    while (DefmoduleData(theEnv)->ListOfDefmodules != nullptr) {
        nextDefmodule = (Defmodule *) DefmoduleData(theEnv)->ListOfDefmodules->header.next;
        ReturnDefmodule(theEnv, DefmoduleData(theEnv)->ListOfDefmodules, false);
        DefmoduleData(theEnv)->ListOfDefmodules = nextDefmodule;
    }

    DefmoduleData(theEnv)->CurrentModule = nullptr;
    DefmoduleData(theEnv)->LastDefmodule = nullptr;
}

/************************************************************/
/* ReturnDefmodule: Returns the data structures associated  */
/*   with a defmodule construct to the pool of free memory. */
/************************************************************/
static void ReturnDefmodule(
        const Environment&theEnv,
        Defmodule *theDefmodule,
        bool environmentClear) {
    unsigned int i;
    struct moduleItem *theItem;
    struct portItem *theSpec, *nextSpec;

    /*=====================================================*/
    /* Set the current module to the module being deleted. */
    /*=====================================================*/

    if (theDefmodule == nullptr) return;

    if (!environmentClear) { SetCurrentModule(theEnv, theDefmodule); }

    /*============================================*/
    /* Call the free functions for the constructs */
    /* belonging to this module.                  */
    /*============================================*/

    if (theDefmodule->itemsArray != nullptr) {
        if (!environmentClear) {
            for (i = 0, theItem = DefmoduleData(theEnv)->ListOfModuleItems;
                 (i < DefmoduleData(theEnv)->NumberOfModuleItems) && (theItem != nullptr);
                 i++, theItem = theItem->next) {
                if (theItem->freeFunction != nullptr) { (*theItem->freeFunction)(theEnv, theDefmodule->itemsArray[i]); }
            }
        }

        rm(theEnv, theDefmodule->itemsArray, sizeof(void *) * DefmoduleData(theEnv)->NumberOfModuleItems);
    }

    /*======================================================*/
    /* Decrement the symbol count for the defmodule's name. */
    /*======================================================*/

    if (!environmentClear) { ReleaseLexeme(theEnv, theDefmodule->header.name); }

    /*====================================*/
    /* Free the items in the import list. */
    /*====================================*/

    theSpec = theDefmodule->importList;
    while (theSpec != nullptr) {
        nextSpec = theSpec->next;
        if (!environmentClear) {
            if (theSpec->moduleName != nullptr) ReleaseLexeme(theEnv, theSpec->moduleName);
            if (theSpec->constructType != nullptr) ReleaseLexeme(theEnv, theSpec->constructType);
            if (theSpec->constructName != nullptr) ReleaseLexeme(theEnv, theSpec->constructName);
        }
        rtn_struct(theEnv, portItem, theSpec);
        theSpec = nextSpec;
    }

    /*====================================*/
    /* Free the items in the export list. */
    /*====================================*/

    theSpec = theDefmodule->exportList;
    while (theSpec != nullptr) {
        nextSpec = theSpec->next;
        if (!environmentClear) {
            if (theSpec->moduleName != nullptr) ReleaseLexeme(theEnv, theSpec->moduleName);
            if (theSpec->constructType != nullptr) ReleaseLexeme(theEnv, theSpec->constructType);
            if (theSpec->constructName != nullptr) ReleaseLexeme(theEnv, theSpec->constructName);
        }
        rtn_struct(theEnv, portItem, theSpec);
        theSpec = nextSpec;
    }

    /*=========================================*/
    /* Free the defmodule pretty print string. */
    /*=========================================*/

    if (theDefmodule->header.ppForm != nullptr) {
        rm(theEnv, (void *) theDefmodule->header.ppForm,
           sizeof(char) * (strlen(theDefmodule->header.ppForm) + 1));
    }

    /*=======================*/
    /* Return the user data. */
    /*=======================*/

    ClearUserDataList(theEnv, theDefmodule->header.usrData);

    /*======================================*/
    /* Return the defmodule data structure. */
    /*======================================*/

    rtn_struct(theEnv, defmodule, theDefmodule);
}


#endif
/************************************************/
/* FindDefmodule: Searches for a defmodule in   */
/*   the list of defmodules. Returns a pointer  */
/*   to the defmodule if found, otherwise nullptr. */
/************************************************/
Defmodule *FindDefmodule(
        const Environment&theEnv,
        const char *defmoduleName) {
#if STUBBING_INACTIVE
    Defmodule *defmodulePtr;
    CLIPSLexeme *findValue;

    if ((findValue = FindSymbolHN(theEnv, defmoduleName, SYMBOL_BIT)) == nullptr) return nullptr;

    defmodulePtr = DefmoduleData(theEnv)->ListOfDefmodules;
    while (defmodulePtr != nullptr) {
        if (defmodulePtr->header.name == findValue) { return defmodulePtr; }

        defmodulePtr = (Defmodule *) defmodulePtr->header.next;
    }
#endif

    return nullptr;
}
/*************************************************/
/* GetCurrentModuleCommand: H/L access routine   */
/*   for the get-current-module command.         */
/*************************************************/
void GetCurrentModuleCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
#if STUBBING_INACTIVE
    Defmodule *theModule;

    theModule = GetCurrentModule(theEnv);

    if (theModule == nullptr) {
        returnValue->lexemeValue = FalseSymbol(theEnv);
        return;
    }

    returnValue->value = theModule->header.name;
#endif
}
#if STUBBING_INACTIVE

/*************************************************/
/* SetCurrentModuleCommand: H/L access routine   */
/*   for the set-current-module command.         */
/*************************************************/
void SetCurrentModuleCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UDFValue theArg;
    const char *argument;
    Defmodule *theModule;
    CLIPSLexeme *oldModuleName;

    /*=======================*/
    /* Set the return value. */
    /*=======================*/

    theModule = GetCurrentModule(theEnv);
    if (theModule == nullptr) {
        returnValue->lexemeValue = FalseSymbol(theEnv);
        return;
    }

    oldModuleName = theModule->header.name;
    returnValue->value = oldModuleName;

    /*=====================================================*/
    /* Check for the correct number and type of arguments. */
    /*=====================================================*/

    if (!UDFFirstArgument(context, SYMBOL_BIT, &theArg)) { return; }

    argument = theArg.lexemeValue->contents;

    /*================================================*/
    /* Set the current module to the specified value. */
    /*================================================*/

    theModule = FindDefmodule(theEnv, argument);

    if (theModule == nullptr) {
        CantFindItemErrorMessage(theEnv, "defmodule", argument, true);
        return;
    }

    SetCurrentModule(theEnv, theModule);
}

/*************************************************/
/* AddAfterModuleChangeFunction: Adds a function */
/*   to the list of functions to be called after */
/*   a module change occurs.                     */
/*************************************************/
void AddAfterModuleChangeFunction(
        const Environment&theEnv,
        const char *name,
        VoidCallFunction *func,
        int priority,
        void *context) {
    DefmoduleData(theEnv)->AfterModuleChangeFunctions =
            AddVoidFunctionToCallList(theEnv, name, priority, func, DefmoduleData(theEnv)->AfterModuleChangeFunctions, context);
}

/************************************************/
/* IllegalModuleSpecifierMessage: Error message */
/*   for the illegal use of a module specifier. */
/************************************************/
void IllegalModuleSpecifierMessage(
        const Environment&theEnv) {
    PrintErrorID(theEnv, "MODULDEF", 1, true);
    WriteString(theEnv, STDERR, "Illegal use of the module specifier.\n");
}

/*********************************************/
/* GetNumberOfDefmodules: Returns the number */
/*   of defmodules currently defined.        */
/*********************************************/
unsigned short GetNumberOfDefmodules(
        const Environment&theEnv) {
#if DEFMODULE_CONSTRUCT
    return DefmoduleData(theEnv)->NumberOfDefmodules;
#else
    return 1;
#endif
}

#endif
