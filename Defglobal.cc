/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  09/03/18             */
/*                                                     */
/*                  DEFGLOBAL MODULE                   */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides core routines for the creation and      */
/*   maintenance of the defglobal construct.                 */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warning.                              */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
/*                                                           */
/*            Changed find construct functionality so that   */
/*            imported modules are search when locating a    */
/*            named construct.                               */
/*                                                           */
/*      6.40: Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
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
/*            UDF redesign.                                  */
/*                                                           */
/*            Use of ?<var>, $?<var>, ?*<var>, and  $?*var*  */
/*            by itself at the command prompt and within     */
/*            the eval function now consistently returns the */
/*            value of  the variable.                        */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#if DEFGLOBAL_CONSTRUCT

#include <cstdio>

#if BLOAD_AND_BSAVE
#include "BinaryLoad.h"
#include "DefglobalBinary.h"
#endif
#include "CommandLine.h"
#include "Environment.h"
#include "DefglobalBasicCommands.h"
#include "DefglobalCommands.h"
#include "DefglobalParser.h"
#include "MemoryAllocation.h"
#include "DefmoduleParser.h"
#include "DefmoduleUtility.h"
#include "Multifield.h"
#include "PrintUtility.h"
#include "Router.h"
#include "StringRouter.h"
#include "Utility.h"

#include "Defglobal.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void *AllocateModule(const Environment&);
static void ReturnModule(const Environment&, void *);
static void ReturnDefglobal(const Environment&, Defglobal *);
static void InitializeDefglobalModules(const Environment&);
static bool EntityGetDefglobalValue(const Environment&, void *, UDFValue *);
static void IncrementDefglobalBusyCount(const Environment&, Defglobal *);
static void DecrementDefglobalBusyCount(const Environment&, Defglobal *);
static void DeallocateDefglobalData(const Environment&);
static void DestroyDefglobalAction(const Environment&, ConstructHeader *, void *);
static void DestroyDefglobal(const Environment&, Defglobal *);

/**************************************************************/
/* InitializeDefglobals: Initializes the defglobal construct. */
/**************************************************************/
void InitializeDefglobals(
        const Environment&theEnv) {
    struct entityRecord globalInfo = {"GBL_VARIABLE", GBL_VARIABLE, 0, 0, 0,
                                      nullptr,
                                      nullptr,
                                      nullptr,
                                      (EntityEvaluationFunction *) EntityGetDefglobalValue,
                                      nullptr, nullptr,
                                      nullptr, nullptr, nullptr, nullptr, nullptr, nullptr};

    struct entityRecord defglobalPtrRecord = {"DEFGLOBAL_PTR", DEFGLOBAL_PTR, 0, 0, 0,
                                              nullptr, nullptr, nullptr,
                                              (EntityEvaluationFunction *) QGetDefglobalUDFValue,
                                              nullptr,
                                              (EntityBusyCountFunction *) DecrementDefglobalBusyCount,
                                              (EntityBusyCountFunction *) IncrementDefglobalBusyCount,
                                              nullptr, nullptr, nullptr, nullptr, nullptr};

    AllocateEnvironmentData(theEnv, DEFGLOBAL_DATA, sizeof(defglobalData), DeallocateDefglobalData);

    memcpy(&DefglobalData(theEnv)->GlobalInfo, &globalInfo, sizeof(entityRecord));
    memcpy(&DefglobalData(theEnv)->DefglobalPtrRecord, &defglobalPtrRecord, sizeof(entityRecord));

    DefglobalData(theEnv)->ResetGlobals = true;
    DefglobalData(theEnv)->LastModuleIndex = -1;

    InstallPrimitive(theEnv, &DefglobalData(theEnv)->GlobalInfo, GBL_VARIABLE);
    InstallPrimitive(theEnv, &DefglobalData(theEnv)->GlobalInfo, MF_GBL_VARIABLE);
    InstallPrimitive(theEnv, &DefglobalData(theEnv)->DefglobalPtrRecord, DEFGLOBAL_PTR);

    InitializeDefglobalModules(theEnv);

    DefglobalBasicCommands(theEnv);
    DefglobalCommandDefinitions(theEnv);

    DefglobalData(theEnv)->DefglobalConstruct =
            AddConstruct(theEnv, "defglobal", "defglobals", ParseDefglobal,
                         (FindConstructFunction *) FindDefglobal,
                         GetConstructNamePointer, GetConstructPPForm,
                         GetConstructModuleItem,
                         (GetNextConstructFunction *) GetNextDefglobal,
                         SetNextConstruct,
                         (IsConstructDeletableFunction *) DefglobalIsDeletable,
                         (DeleteConstructFunction *) Undefglobal,
                         (FreeConstructFunction *) ReturnDefglobal);
}

/****************************************************/
/* DeallocateDefglobalData: Deallocates environment */
/*    data for the defglobal construct.             */
/****************************************************/
static void DeallocateDefglobalData(
        const Environment&theEnv) {
    struct defglobalModule *theModuleItem;
    Defmodule *theModule;

#if BLOAD_AND_BSAVE
    if (Bloaded(theEnv)) return;
#endif

    DoForAllConstructs(theEnv, DestroyDefglobalAction,
                       DefglobalData(theEnv)->DefglobalModuleIndex, false, nullptr);

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        theModuleItem = (defglobalModule *)
                GetModuleItem(theEnv, theModule,
                              DefglobalData(theEnv)->DefglobalModuleIndex);
        rtn_struct(theEnv, defglobalModule, theModuleItem);
    }
}

/***************************************************/
/* DestroyDefglobalAction: Action used to remove   */
/*   defglobals as a result of DestroyEnvironment. */
/***************************************************/
static void DestroyDefglobalAction(
        const Environment&theEnv,
        ConstructHeader *theConstruct,
        void *buffer) {
#if MAC_XCD
#pragma unused(buffer)
#endif
    Defglobal *theDefglobal = (Defglobal *) theConstruct;

    if (theDefglobal == nullptr) return;

    DestroyDefglobal(theEnv, theDefglobal);
}

/*********************************************************/
/* InitializeDefglobalModules: Initializes the defglobal */
/*   construct for use with the defmodule construct.     */
/*********************************************************/
static void InitializeDefglobalModules(
        const Environment&theEnv) {
    DefglobalData(theEnv)->DefglobalModuleIndex = RegisterModuleItem(theEnv, "defglobal",
                                                                     AllocateModule,
                                                                     ReturnModule,
#if BLOAD_AND_BSAVE
                                                                     BloadDefglobalModuleReference,
#else
            nullptr,
#endif
                                                                     (FindConstructFunction *) FindDefglobalInModule);

#if DEFMODULE_CONSTRUCT
    AddPortConstructItem(theEnv, "defglobal", SYMBOL_TOKEN);
#endif
}

/*************************************************/
/* AllocateModule: Allocates a defglobal module. */
/*************************************************/
static void *AllocateModule(
        const Environment&theEnv) {
    return (void *) get_struct(theEnv, defglobalModule);
}

/*************************************************/
/* ReturnModule: Deallocates a defglobal module. */
/*************************************************/
static void ReturnModule(
        const Environment&theEnv,
        void *theItem) {
    FreeConstructHeaderModule(theEnv, (defmoduleItemHeader *) theItem, DefglobalData(theEnv)->DefglobalConstruct);
    rtn_struct(theEnv, defglobalModule, theItem);
}

/**************************************************************/
/* GetDefglobalModuleItem: Returns a pointer to the defmodule */
/*  item for the specified defglobal or defmodule.            */
/**************************************************************/
struct defglobalModule *GetDefglobalModuleItem(
        const Environment&theEnv,
        Defmodule *theModule) {
    return ((defglobalModule *) GetConstructModuleItemByIndex(theEnv, theModule, DefglobalData(theEnv)->DefglobalModuleIndex));
}

/**************************************************/
/* FindDefglobal: Searches for a defglobal in the */
/*   list of defglobals. Returns a pointer to the */
/*   defglobal if found, otherwise nullptr.          */
/**************************************************/
Defglobal *FindDefglobal(
        const Environment&theEnv,
        const char *defglobalName) {
    return (Defglobal *) FindNamedConstructInModuleOrImports(theEnv, defglobalName, DefglobalData(theEnv)->DefglobalConstruct);
}

/******************************************************/
/* FindDefglobalInModule: Searches for a defglobal in */
/*   the list of defglobals. Returns a pointer to the */
/*   defglobal if found, otherwise nullptr.              */
/******************************************************/
Defglobal *FindDefglobalInModule(
        const Environment&theEnv,
        const char *defglobalName) {
    return (Defglobal *) FindNamedConstructInModule(theEnv, defglobalName, DefglobalData(theEnv)->DefglobalConstruct);
}

/*****************************************************************/
/* GetNextDefglobal: If passed a nullptr pointer, returns the first */
/*   defglobal in the defglobal list. Otherwise returns the next */
/*   defglobal following the defglobal passed as an argument.    */
/*****************************************************************/
Defglobal *GetNextDefglobal(
        const Environment&theEnv,
        Defglobal *defglobalPtr) {
    return (Defglobal *) GetNextConstructItem(theEnv, &defglobalPtr->header, DefglobalData(theEnv)->DefglobalModuleIndex);
}

/********************************************************/
/* DefglobalIsDeletable: Returns true if a particular   */
/*   defglobal can be deleted, otherwise returns false. */
/********************************************************/
bool DefglobalIsDeletable(
        Defglobal *theDefglobal) {
    const Environment&theEnv = theDefglobal->header.env;

    if (!ConstructsDeletable(theEnv)) { return false; }

    return theDefglobal->busyCount == 0;

}

/************************************************************/
/* ReturnDefglobal: Returns the data structures associated  */
/*   with a defglobal construct to the pool of free memory. */
/************************************************************/
static void ReturnDefglobal(
        const Environment&theEnv,
        Defglobal *theDefglobal) {
    if (theDefglobal == nullptr) return;

    /*====================================*/
    /* Return the global's current value. */
    /*====================================*/

    Release(theEnv, theDefglobal->current.header);
    if (theDefglobal->current.header->type == MULTIFIELD_TYPE) {
        if (theDefglobal->current.multifieldValue->busyCount == 0) { ReturnMultifield(theEnv, theDefglobal->current.multifieldValue); }
        else { AddToMultifieldList(theEnv, theDefglobal->current.multifieldValue); }
    }

    /*================================================*/
    /* Return the expression representing the initial */
    /* value of the defglobal when it was defined.    */
    /*================================================*/

    RemoveHashedExpression(theEnv, theDefglobal->initial);

    /*===============================*/
    /* Release items stored in the   */
    /* defglobal's construct header. */
    /*===============================*/

    DeinstallConstructHeader(theEnv, &theDefglobal->header);

    /*======================================*/
    /* Return the defglobal data structure. */
    /*======================================*/

    rtn_struct(theEnv, defglobal, theDefglobal);

    /*===========================================*/
    /* Set the variable indicating that a change */
    /* has been made to a global variable.       */
    /*===========================================*/

    DefglobalData(theEnv)->ChangeToGlobals = true;
}

/************************************************************/
/* DestroyDefglobal: Returns the data structures associated  */
/*   with a defglobal construct to the pool of free memory. */
/************************************************************/
static void DestroyDefglobal(
        const Environment&theEnv,
        Defglobal *theDefglobal) {
    if (theDefglobal == nullptr) return;

    /*====================================*/
    /* Return the global's current value. */
    /*====================================*/

    if (theDefglobal->current.header->type == MULTIFIELD_TYPE) {
        if (theDefglobal->current.multifieldValue->busyCount == 0) { ReturnMultifield(theEnv, theDefglobal->current.multifieldValue); }
        else { AddToMultifieldList(theEnv, theDefglobal->current.multifieldValue); }
    }


    /*===============================*/
    /* Release items stored in the   */
    /* defglobal's construct header. */
    /*===============================*/

    DeinstallConstructHeader(theEnv, &theDefglobal->header);

    /*======================================*/
    /* Return the defglobal data structure. */
    /*======================================*/

    rtn_struct(theEnv, defglobal, theDefglobal);
}

/************************************************/
/* QSetDefglobalValue: Lowest level routine for */
/*   setting a defglobal's value.               */
/************************************************/
void QSetDefglobalValue(
        const Environment&theEnv,
        Defglobal *theGlobal,
        UDFValue *vPtr,
        bool resetVar) {
    CLIPSValue newValue;

    /*====================================================*/
    /* If the new value passed for the defglobal is nullptr, */
    /* then reset the defglobal to the initial value it   */
    /* had when it was defined.                           */
    /*====================================================*/

    if (resetVar) {
        EvaluateExpression(theEnv, theGlobal->initial, vPtr);
        if (EvaluationData(theEnv)->EvaluationError) { vPtr->value = FalseSymbol(theEnv); }
    }

    /*==========================================*/
    /* If globals are being watch, then display */
    /* the change to the global variable.       */
    /*==========================================*/

#if DEBUGGING_FUNCTIONS
    if (theGlobal->watch &&
        (!ConstructData(theEnv)->ClearReadyInProgress) &&
        (!ConstructData(theEnv)->ClearInProgress)) {
        WriteString(theEnv, STDOUT, ":== ?*");
        WriteString(theEnv, STDOUT, theGlobal->header.name->contents);
        WriteString(theEnv, STDOUT, "* ==> ");
        WriteUDFValue(theEnv, STDOUT, vPtr);
        WriteString(theEnv, STDOUT, " <== ");
        WriteCLIPSValue(theEnv, STDOUT, &theGlobal->current);
        WriteString(theEnv, STDOUT, "\n");
    }
#endif

    /*==============================================*/
    /* Retain the new value of the global variable. */
    /*==============================================*/

    NormalizeMultifield(theEnv, vPtr);
    if (vPtr->header->type != MULTIFIELD_TYPE) { newValue.value = vPtr->value; }
    else { newValue.value = CopyMultifield(theEnv, vPtr->multifieldValue); }
    Retain(theEnv, newValue.header);

    /*==============================================*/
    /* Remove the old value of the global variable. */
    /*==============================================*/

    Release(theEnv, theGlobal->current.header);
    if (theGlobal->current.header->type == MULTIFIELD_TYPE) {
        if (theGlobal->current.multifieldValue->busyCount == 0) { ReturnMultifield(theEnv, theGlobal->current.multifieldValue); }
        else { AddToMultifieldList(theEnv, theGlobal->current.multifieldValue); }
    }

    /*===========================================*/
    /* Set the new value of the global variable. */
    /*===========================================*/

    theGlobal->current.value = newValue.value;

    /*===========================================*/
    /* Set the variable indicating that a change */
    /* has been made to a global variable.       */
    /*===========================================*/

    DefglobalData(theEnv)->ChangeToGlobals = true;

    if (EvaluationData(theEnv)->CurrentExpression == nullptr) {
        CleanCurrentGarbageFrame(theEnv, nullptr);
        CallPeriodicTasks(theEnv);
    }
}

/**************************************************************/
/* QFindDefglobal: Searches for a defglobal in the list of    */
/*   defglobals. Returns a pointer to the defglobal if found, */
/*   otherwise nullptr.                                          */
/**************************************************************/
Defglobal *QFindDefglobal(
        const Environment&theEnv,
        CLIPSLexeme *defglobalName) {
    Defglobal *theDefglobal;

    for (theDefglobal = GetNextDefglobal(theEnv, nullptr);
         theDefglobal != nullptr;
         theDefglobal = GetNextDefglobal(theEnv, theDefglobal)) { if (defglobalName == theDefglobal->header.name) return theDefglobal; }

    return nullptr;
}

/*******************************************************************/
/* DefglobalValueForm: Returns the pretty print representation of  */
/*   the current value of the specified defglobal. For example, if */
/*   the current value of ?*x* is 5, the string "?*x* = 5" would   */
/*   be returned.                                                  */
/*******************************************************************/
void DefglobalValueForm(
        Defglobal *theGlobal,
        StringBuilder *theSB) {
    const Environment&theEnv = theGlobal->header.env;

    OpenStringBuilderDestination(theEnv, "GlobalValueForm", theSB);
    WriteString(theEnv, "GlobalValueForm", "?*");
    WriteString(theEnv, "GlobalValueForm", theGlobal->header.name->contents);
    WriteString(theEnv, "GlobalValueForm", "* = ");
    WriteCLIPSValue(theEnv, "GlobalValueForm", &theGlobal->current);
    CloseStringBuilderDestination(theEnv, "GlobalValueForm");
}

/*********************************************************/
/* GetGlobalsChanged: Returns the defglobal change flag. */
/*********************************************************/
bool GetGlobalsChanged(
        const Environment&theEnv) {
    return DefglobalData(theEnv)->ChangeToGlobals;
}

/******************************************************/
/* SetGlobalsChanged: Sets the defglobal change flag. */
/******************************************************/
void SetGlobalsChanged(
        const Environment&theEnv,
        bool value) {
    DefglobalData(theEnv)->ChangeToGlobals = value;
}

/*********************************************************/
/* EntityGetDefglobalValue: Returns the value of the     */
/*   specified global variable in the supplied UDFValue. */
/*********************************************************/
static bool EntityGetDefglobalValue(
        const Environment&theEnv,
        void *theValue,
        UDFValue *vPtr) {
    Defglobal *theGlobal;
    unsigned int count;

    /*===========================================*/
    /* Search for the specified defglobal in the */
    /* modules visible to the current module.    */
    /*===========================================*/

    theGlobal = (Defglobal *)
            FindImportedConstruct(theEnv, "defglobal", nullptr, ((CLIPSLexeme *) theValue)->contents,
                                  &count, true, nullptr);

    /*=============================================*/
    /* If it wasn't found, print an error message. */
    /*=============================================*/

    if (theGlobal == nullptr) {
        PrintErrorID(theEnv, "GLOBLDEF", 1, false);
        WriteString(theEnv, STDERR, "Global variable ?*");
        WriteString(theEnv, STDERR, ((CLIPSLexeme *) theValue)->contents);
        WriteString(theEnv, STDERR, "* is unbound.\n");
        vPtr->value = FalseSymbol(theEnv);
        SetEvaluationError(theEnv, true);
        return false;
    }

    /*========================================================*/
    /* The current implementation of the defmodules shouldn't */
    /* allow a construct to be defined which would cause an   */
    /* ambiguous reference, but we'll check for it anyway.    */
    /*========================================================*/

    if (count > 1) {
        AmbiguousReferenceErrorMessage(theEnv, "defglobal", ((CLIPSLexeme *) theValue)->contents);
        vPtr->value = FalseSymbol(theEnv);
        SetEvaluationError(theEnv, true);
        return false;
    }

    /*=================================*/
    /* Get the value of the defglobal. */
    /*=================================*/

    CLIPSToUDFValue(&theGlobal->current, vPtr);

    return true;
}

/******************************************************************/
/* QGetDefglobalUDFValue: Returns the value of a global variable. */
/******************************************************************/
bool QGetDefglobalUDFValue(
        const Environment&theEnv,
        Defglobal *theGlobal,
        UDFValue *vPtr) {
    vPtr->value = theGlobal->current.value;

    /*===========================================================*/
    /* If the global contains a multifield value, return a copy  */
    /* of the value so that routines which use this value are    */
    /* not affected if the value of the global is later changed. */
    /*===========================================================*/

    if (theGlobal->current.header->type == MULTIFIELD_TYPE) {
        vPtr->begin = 0;
        vPtr->range = theGlobal->current.multifieldValue->length;
    }

    return true;
}

/*********************************************************/
/* DefglobalGetValue: Returns the value of the specified */
/*   global variable in the supplied UDFValue.           */
/*********************************************************/
void DefglobalGetValue(
        Defglobal *theDefglobal,
        CLIPSValue *vPtr) {
    vPtr->value = theDefglobal->current.value;
}

/*************************************************************/
/* DefglobalSetValue: Sets the value of the specified global */
/*   variable to the value stored in the supplied UDFValue.  */
/*************************************************************/
void DefglobalSetValue(
        Defglobal *theDefglobal,
        CLIPSValue *vPtr) {
    UDFValue temp;
    GCBlock gcb;
    const Environment&theEnv = theDefglobal->header.env;

    /*=====================================*/
    /* If embedded, clear the error flags. */
    /*=====================================*/

    if (EvaluationData(theEnv)->CurrentExpression == nullptr) { ResetErrorFlags(theEnv); }

    GCBlockStart(theEnv, &gcb);
    CLIPSToUDFValue(vPtr, &temp);
    QSetDefglobalValue(theEnv, theDefglobal, &temp, false);
    GCBlockEnd(theEnv, &gcb);
}

/************************/
/* DefglobalSetInteger: */
/************************/
void DefglobalSetInteger(
        Defglobal *theDefglobal,
        long long value) {
    CLIPSValue cv;

    cv.integerValue = CreateInteger(theDefglobal->header.env, value);

    DefglobalSetValue(theDefglobal, &cv);
}

/**********************/
/* DefglobalSetFloat: */
/**********************/
void DefglobalSetFloat(
        Defglobal *theDefglobal,
        double value) {
    CLIPSValue cv;

    cv.floatValue = CreateFloat(theDefglobal->header.env, value);

    DefglobalSetValue(theDefglobal, &cv);
}

/***********************/
/* DefglobalSetSymbol: */
/***********************/
void DefglobalSetSymbol(
        Defglobal *theDefglobal,
        const char *value) {
    CLIPSValue cv;

    cv.lexemeValue = CreateSymbol(theDefglobal->header.env, value);

    DefglobalSetValue(theDefglobal, &cv);
}

/***********************/
/* DefglobalSetString: */
/***********************/
void DefglobalSetString(
        Defglobal *theDefglobal,
        const char *value) {
    CLIPSValue cv;

    cv.lexemeValue = CreateString(theDefglobal->header.env, value);

    DefglobalSetValue(theDefglobal, &cv);
}

/*****************************/
/* DefglobalSetInstanceName: */
/*****************************/
void DefglobalSetInstanceName(
        Defglobal *theDefglobal,
        const char *value) {
    CLIPSValue cv;

    cv.lexemeValue = CreateInstanceName(theDefglobal->header.env, value);

    DefglobalSetValue(theDefglobal, &cv);
}

/*****************************/
/* DefglobalSetCLIPSInteger: */
/*****************************/
void DefglobalSetCLIPSInteger(
        Defglobal *theDefglobal,
        CLIPSInteger *value) {
    CLIPSValue cv;

    cv.integerValue = value;

    DefglobalSetValue(theDefglobal, &cv);
}

/***************************/
/* DefglobalSetCLIPSFloat: */
/***************************/
void DefglobalSetCLIPSFloat(
        Defglobal *theDefglobal,
        CLIPSFloat *value) {
    CLIPSValue cv;

    cv.floatValue = value;

    DefglobalSetValue(theDefglobal, &cv);
}

/****************************/
/* DefglobalSetCLIPSLexeme: */
/****************************/
void DefglobalSetCLIPSLexeme(
        Defglobal *theDefglobal,
        CLIPSLexeme *value) {
    CLIPSValue cv;

    cv.lexemeValue = value;

    DefglobalSetValue(theDefglobal, &cv);
}

/*********************/
/* DefglobalSetFact: */
/*********************/
void DefglobalSetFact(
        Defglobal *theDefglobal,
        Fact *value) {
    CLIPSValue cv;

    cv.factValue = value;

    DefglobalSetValue(theDefglobal, &cv);
}

/*************************/
/* DefglobalSetInstance: */
/*************************/
void DefglobalSetInstance(
        Defglobal *theDefglobal,
        Instance *value) {
    CLIPSValue cv;

    cv.instanceValue = value;

    DefglobalSetValue(theDefglobal, &cv);
}

/***************************/
/* DefglobalSetMultifield: */
/***************************/
void DefglobalSetMultifield(
        Defglobal *theDefglobal,
        Multifield *value) {
    CLIPSValue cv;

    cv.multifieldValue = value;

    DefglobalSetValue(theDefglobal, &cv);
}

/*************************************/
/* DefglobalSetCLIPSExternalAddress: */
/*************************************/
void DefglobalSetCLIPSExternalAddress(
        Defglobal *theDefglobal,
        CLIPSExternalAddress *value) {
    CLIPSValue cv;

    cv.externalAddressValue = value;

    DefglobalSetValue(theDefglobal, &cv);
}

/**********************************************************/
/* DecrementDefglobalBusyCount: Decrements the busy count */
/*   of a defglobal data structure.                       */
/**********************************************************/
static void DecrementDefglobalBusyCount(
        const Environment&theEnv,
        Defglobal *theGlobal) {
    if (!ConstructData(theEnv)->ClearInProgress) theGlobal->busyCount--;
}

/**********************************************************/
/* IncrementDefglobalBusyCount: Increments the busy count */
/*   of a defglobal data structure.                       */
/**********************************************************/
static void IncrementDefglobalBusyCount(
        const Environment&theEnv,
        Defglobal *theGlobal) {
#if MAC_XCD
#pragma unused(theEnv)
#endif

    theGlobal->busyCount++;
}

/***********************************************************************/
/* UpdateDefglobalScope: Updates the scope flag of all the defglobals. */
/***********************************************************************/
void UpdateDefglobalScope(
        const Environment&theEnv) {
    Defglobal *theDefglobal;
    unsigned int moduleCount;
    Defmodule *theModule;
    struct defmoduleItemHeader *theItem;

    /*============================*/
    /* Loop through every module. */
    /*============================*/

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        /*============================================================*/
        /* Loop through every defglobal in the module being examined. */
        /*============================================================*/

        theItem = (defmoduleItemHeader *)
                GetModuleItem(theEnv, theModule, DefglobalData(theEnv)->DefglobalModuleIndex);

        for (theDefglobal = (Defglobal *) theItem->firstItem;
             theDefglobal != nullptr;
             theDefglobal = GetNextDefglobal(theEnv, theDefglobal)) {
            /*====================================================*/
            /* If the defglobal is visible to the current module, */
            /* then mark it as being in scope, otherwise mark it  */
            /* as being out of scope.                             */
            /*====================================================*/

            theDefglobal->inScope = FindImportedConstruct(theEnv, "defglobal", theModule,
                                                          theDefglobal->header.name->contents,
                                                          &moduleCount, true, nullptr) != nullptr;
        }
    }
}

/*******************************************************/
/* GetNextDefglobalInScope: Returns the next defglobal */
/*   that is scope of the current module. Works in a   */
/*   similar fashion to GetNextDefglobal, but skips    */
/*   defglobals that are out of scope.                 */
/*******************************************************/
Defglobal *GetNextDefglobalInScope(
        const Environment&theEnv,
        Defglobal *theGlobal) {
    struct defmoduleItemHeader *theItem;

    /*=======================================*/
    /* If we're beginning the search for the */
    /* first defglobal in scope, then ...    */
    /*=======================================*/

    if (theGlobal == nullptr) {
        /*==============================================*/
        /* If the current module has been changed since */
        /* the last time the scopes were computed, then */
        /* recompute the scopes.                        */
        /*==============================================*/

        if (DefglobalData(theEnv)->LastModuleIndex != DefmoduleData(theEnv)->ModuleChangeIndex) {
            UpdateDefglobalScope(theEnv);
            DefglobalData(theEnv)->LastModuleIndex = DefmoduleData(theEnv)->ModuleChangeIndex;
        }

        /*==========================================*/
        /* Get the first module and first defglobal */
        /* to start the search with.                */
        /*==========================================*/

        DefglobalData(theEnv)->TheDefmodule = GetNextDefmodule(theEnv, nullptr);
        theItem = (defmoduleItemHeader *)
                GetModuleItem(theEnv, DefglobalData(theEnv)->TheDefmodule, DefglobalData(theEnv)->DefglobalModuleIndex);
        theGlobal = (Defglobal *) theItem->firstItem;
    }

        /*==================================================*/
        /* Otherwise, see if the last defglobal returned by */
        /* this function has a defglobal following it.      */
        /*==================================================*/

    else { theGlobal = GetNextDefglobal(theEnv, theGlobal); }

    /*======================================*/
    /* Continue looping through the modules */
    /* until a defglobal in scope is found. */
    /*======================================*/

    while (DefglobalData(theEnv)->TheDefmodule != nullptr) {
        /*=====================================================*/
        /* Loop through the defglobals in the module currently */
        /* being examined to see if one is in scope.           */
        /*=====================================================*/

        for (;
                theGlobal != nullptr;
                theGlobal = GetNextDefglobal(theEnv, theGlobal)) { if (theGlobal->inScope) return theGlobal; }

        /*================================================*/
        /* If a global in scope couldn't be found in this */
        /* module, then move on to the next module.       */
        /*================================================*/

        DefglobalData(theEnv)->TheDefmodule = GetNextDefmodule(theEnv, DefglobalData(theEnv)->TheDefmodule);
        theItem = (defmoduleItemHeader *)
                GetModuleItem(theEnv, DefglobalData(theEnv)->TheDefmodule, DefglobalData(theEnv)->DefglobalModuleIndex);
        theGlobal = (Defglobal *) theItem->firstItem;
    }

    /*====================================*/
    /* All the globals in scope have been */
    /* traversed and there are none left. */
    /*====================================*/

    return nullptr;
}

/*##################################*/
/* Additional Environment Functions */
/*##################################*/

const char *DefglobalModule(
        Defglobal *theDefglobal) {
    return GetConstructModuleName(&theDefglobal->header);
}

const char *DefglobalName(
        Defglobal *theDefglobal) {
    return GetConstructNameString(&theDefglobal->header);
}

const char *DefglobalPPForm(
        Defglobal *theDefglobal) {
    return GetConstructPPForm(&theDefglobal->header);
}

#endif /* DEFGLOBAL_CONSTRUCT */


