/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  11/01/16             */
/*                                                     */
/*                 DEFTEMPLATE MODULE                  */
/*******************************************************/

/*************************************************************/
/* Purpose: Defines basic deftemplate primitive functions    */
/*   such as allocating and deallocating, traversing, and    */
/*   finding deftemplate data structures.                    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Added support for templates maintaining their  */
/*            own list of facts.                             */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warnings.                             */
/*                                                           */
/*      6.30: Added code for deftemplate run time            */
/*            initialization of hashed comparisons to        */
/*            constants.                                     */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Support for deftemplate slot facets.           */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Changed find construct functionality so that   */
/*            imported modules are search when locating a    */
/*            named construct.                               */
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
/*************************************************************/

#include "Setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include <cstdio>

#include "Construct.h"
#include "ConstraintChecking.h"
#include "Environment.h"
#include "Expression.h"
#include "MemoryAllocation.h"
#include "DefmoduleParser.h"
#include "DefmoduleUtility.h"
#include "Network.h"
#include "Pattern.h"
#include "Router.h"
#include "DeftemplateBasicCommands.h"
#include "DeftemplateFunctions.h"
#include "DeftemplateParser.h"
#include "DeftemplateUtilities.h"

#if BLOAD_AND_BSAVE
#include "BinaryLoad.h"
#include "DeftemplateBinarySaveLoad.h"
#endif

#include "Deftemplate.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void *AllocateModule(const Environment&);
static void ReturnModule(const Environment&, void *);
static void ReturnDeftemplate(const Environment&, Deftemplate *);
static void InitializeDeftemplateModules(const Environment&);
static void DeallocateDeftemplateData(const Environment&);
static void DestroyDeftemplateAction(const Environment&, ConstructHeader *, void *);
static void DestroyDeftemplate(const Environment&, Deftemplate *);

/******************************************************************/
/* InitializeDeftemplates: Initializes the deftemplate construct. */
/******************************************************************/
void InitializeDeftemplates(
        const Environment&theEnv) {
#if STUBBING_INACTIVE
    EntityRecord deftemplatePtrRecord =
            {"DEFTEMPLATE_PTR",
             DEFTEMPLATE_PTR, 1, 0, 0,
             nullptr,
             nullptr, nullptr,
             nullptr,
             nullptr,
             (EntityBusyCountFunction *) DecrementDeftemplateBusyCount,
             (EntityBusyCountFunction *) IncrementDeftemplateBusyCount,
             nullptr, nullptr, nullptr, nullptr, nullptr};
    //AllocateEnvironmentData(theEnv, DEFTEMPLATE_DATA, sizeof(deftemplateData), DeallocateDeftemplateData);
#endif
    theEnv->allocateEnvironmentModule<deftemplateData>();

#if STUBBING_INACTIVE
    DeftemplateData(theEnv)->DeftemplatePtrRecord = deftemplatePtrRecord;
#endif

#if STUBBING_INACTIVE
    InitializeFacts(theEnv);

    InitializeDeftemplateModules(theEnv);

    DeftemplateBasicCommands(theEnv);

    DeftemplateFunctions(theEnv);
#endif
#if STUBBING_INACTIVE
    DeftemplateData(theEnv)->DeftemplateConstruct =
            AddConstruct(theEnv, "deftemplate", "deftemplates", ParseDeftemplate,
                         (FindConstructFunction *) FindDeftemplate,
                         GetConstructNamePointer, GetConstructPPForm,
                         GetConstructModuleItem,
                         (GetNextConstructFunction *) GetNextDeftemplate,
                         SetNextConstruct,
                         (IsConstructDeletableFunction *) DeftemplateIsDeletable,
                         (DeleteConstructFunction *) Undeftemplate,
                         (FreeConstructFunction *) ReturnDeftemplate);

    InstallPrimitive(theEnv, (EntityRecord *) &DeftemplateData(theEnv)->DeftemplatePtrRecord, DEFTEMPLATE_PTR);
#endif
}
#if STUBBING_INACTIVE
/******************************************************/
/* DeallocateDeftemplateData: Deallocates environment */
/*    data for the deftemplate construct.             */
/******************************************************/
static void DeallocateDeftemplateData(
        const Environment&theEnv) {
    struct deftemplateModule *theModuleItem;
    Defmodule *theModule;
#if BLOAD_AND_BSAVE
    if (Bloaded(theEnv)) return;
#endif

    DoForAllConstructs(theEnv, DestroyDeftemplateAction, DeftemplateData(theEnv)->DeftemplateModuleIndex, false, nullptr);

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        theModuleItem = (deftemplateModule *)
                GetModuleItem(theEnv, theModule,
                              DeftemplateData(theEnv)->DeftemplateModuleIndex);
        rtn_struct(theEnv, deftemplateModule, theModuleItem);
    }
}

/*****************************************************/
/* DestroyDeftemplateAction: Action used to remove   */
/*   deftemplates as a result of DestroyEnvironment. */
/*****************************************************/
static void DestroyDeftemplateAction(
        const Environment&theEnv,
        ConstructHeader *theConstruct,
        void *buffer) {
#if MAC_XCD
#pragma unused(buffer)
#endif
    Deftemplate *theDeftemplate = (Deftemplate *) theConstruct;

    if (theDeftemplate == nullptr) return;

    DestroyDeftemplate(theEnv, theDeftemplate);
}


/*************************************************************/
/* InitializeDeftemplateModules: Initializes the deftemplate */
/*   construct for use with the defmodule construct.         */
/*************************************************************/
static void InitializeDeftemplateModules(
        const Environment&theEnv) {
    DeftemplateData(theEnv)->DeftemplateModuleIndex = RegisterModuleItem(theEnv, "deftemplate",
                                                                         AllocateModule,
                                                                         ReturnModule,
#if BLOAD_AND_BSAVE
                                                                         BloadDeftemplateModuleReference,
#else
            nullptr,
#endif
                                                                         (FindConstructFunction *) FindDeftemplateInModule);

#if DEFMODULE_CONSTRUCT
    AddPortConstructItem(theEnv, "deftemplate", SYMBOL_TOKEN);
#endif
}

/***************************************************/
/* AllocateModule: Allocates a deftemplate module. */
/***************************************************/
static void *AllocateModule(
        const Environment&theEnv) {
    return ((void *) get_struct(theEnv, deftemplateModule));
}

/*************************************************/
/* ReturnModule: Deallocates a deftemplate module. */
/*************************************************/
static void ReturnModule(
        const Environment&theEnv,
        void *theItem) {
    FreeConstructHeaderModule(theEnv, (defmoduleItemHeader *) theItem, DeftemplateData(theEnv)->DeftemplateConstruct);
    rtn_struct(theEnv, deftemplateModule, theItem);
}

/****************************************************************/
/* GetDeftemplateModuleItem: Returns a pointer to the defmodule */
/*  item for the specified deftemplate or defmodule.            */
/****************************************************************/
struct deftemplateModule *GetDeftemplateModuleItem(
        const Environment&theEnv,
        Defmodule *theModule) {
    return ((deftemplateModule *) GetConstructModuleItemByIndex(theEnv, theModule, DeftemplateData(theEnv)->DeftemplateModuleIndex));
}

/***************************************************/
/* FindDeftemplate: Searches for a deftemplate in  */
/*   the list of deftemplates. Returns a pointer   */
/*   to the deftemplate if  found, otherwise nullptr. */
/***************************************************/
Deftemplate *FindDeftemplate(
        const Environment&theEnv,
        const char *deftemplateName) {
    return (Deftemplate *) FindNamedConstructInModuleOrImports(theEnv, deftemplateName, DeftemplateData(theEnv)->DeftemplateConstruct);
}

/*******************************************************/
/* FindDeftemplateInModule: Searches for a deftemplate */
/*   in the list of deftemplates. Returns a pointer    */
/*   to the deftemplate if  found, otherwise nullptr.     */
/*******************************************************/
Deftemplate *FindDeftemplateInModule(
        const Environment&theEnv,
        const char *deftemplateName) {
    return (Deftemplate *) FindNamedConstructInModule(theEnv, deftemplateName, DeftemplateData(theEnv)->DeftemplateConstruct);
}

/***********************************************************************/
/* GetNextDeftemplate: If passed a nullptr pointer, returns the first     */
/*   deftemplate in the ListOfDeftemplates. Otherwise returns the next */
/*   deftemplate following the deftemplate passed as an argument.      */
/***********************************************************************/
Deftemplate *GetNextDeftemplate(
        const Environment&theEnv,
        Deftemplate *deftemplatePtr) {
    return (Deftemplate *) GetNextConstructItem(theEnv, &deftemplatePtr->header, DeftemplateData(theEnv)->DeftemplateModuleIndex);
}

/**********************************************************/
/* DeftemplateIsDeletable: Returns true if a particular   */
/*   deftemplate can be deleted, otherwise returns false. */
/**********************************************************/
bool DeftemplateIsDeletable(
        Deftemplate *theDeftemplate) {
    const Environment&theEnv = theDeftemplate->header.env;

    if (!ConstructsDeletable(theEnv)) { return false; }

    if (theDeftemplate->busyCount > 0) return false;
    return theDeftemplate->patternNetwork == nullptr;

}

/**************************************************************/
/* ReturnDeftemplate: Returns the data structures associated  */
/*   with a deftemplate construct to the pool of free memory. */
/**************************************************************/
static void ReturnDeftemplate(
        const Environment&theEnv,
        Deftemplate *theDeftemplate) {
    struct templateSlot *slotPtr;

    if (theDeftemplate == nullptr) return;

        /*====================================================================*/
        /* If a template is redefined, then we want to save its debug status. */
        /*====================================================================*/

#if DEBUGGING_FUNCTIONS
    DeftemplateData(theEnv)->DeletedTemplateDebugFlags = 0;
    if (theDeftemplate->watch) BitwiseSet(DeftemplateData(theEnv)->DeletedTemplateDebugFlags, 0);
#endif

    /*===========================================*/
    /* Free storage used by the templates slots. */
    /*===========================================*/

    slotPtr = theDeftemplate->slotList;
    while (slotPtr != nullptr) {
        ReleaseLexeme(theEnv, slotPtr->slotName);
        RemoveHashedExpression(theEnv, slotPtr->defaultList);
        slotPtr->defaultList = nullptr;
        RemoveHashedExpression(theEnv, slotPtr->facetList);
        slotPtr->facetList = nullptr;
        RemoveConstraint(theEnv, slotPtr->constraints);
        slotPtr->constraints = nullptr;
        slotPtr = slotPtr->next;
    }

    ReturnSlots(theEnv, theDeftemplate->slotList);

    /*==================================*/
    /* Free storage used by the header. */
    /*==================================*/

    DeinstallConstructHeader(theEnv, &theDeftemplate->header);

    rtn_struct(theEnv, deftemplate, theDeftemplate);
}

/**************************************************************/
/* DestroyDeftemplate: Returns the data structures associated */
/*   with a deftemplate construct to the pool of free memory. */
/**************************************************************/
static void DestroyDeftemplate(
        const Environment&theEnv,
        Deftemplate *theDeftemplate) {
    struct templateSlot *slotPtr, *nextSlot;
    if (theDeftemplate == nullptr) return;

    slotPtr = theDeftemplate->slotList;

    while (slotPtr != nullptr) {
        nextSlot = slotPtr->next;
        rtn_struct(theEnv, templateSlot, slotPtr);
        slotPtr = nextSlot;
    }

    DestroyFactPatternNetwork(theEnv, theDeftemplate->patternNetwork);

    /*==================================*/
    /* Free storage used by the header. */
    /*==================================*/

    DeinstallConstructHeader(theEnv, &theDeftemplate->header);

    rtn_struct(theEnv, deftemplate, theDeftemplate);
}

/***********************************************/
/* ReturnSlots: Returns the slot structures of */
/*   a deftemplate to free memory.             */
/***********************************************/
void ReturnSlots(
        const Environment&theEnv,
        struct templateSlot *slotPtr) {
    struct templateSlot *nextSlot;

    while (slotPtr != nullptr) {
        nextSlot = slotPtr->next;
        ReturnExpression(theEnv, slotPtr->defaultList);
        ReturnExpression(theEnv, slotPtr->facetList);
        RemoveConstraint(theEnv, slotPtr->constraints);
        rtn_struct(theEnv, templateSlot, slotPtr);
        slotPtr = nextSlot;
    }
}

/*************************************************/
/* DecrementDeftemplateBusyCount: Decrements the */
/*   busy count of a deftemplate data structure. */
/*************************************************/
void DecrementDeftemplateBusyCount(
        const Environment&theEnv,
        Deftemplate *theTemplate) {
    if (!ConstructData(theEnv)->ClearInProgress) theTemplate->busyCount--;
}

/*************************************************/
/* IncrementDeftemplateBusyCount: Increments the */
/*   busy count of a deftemplate data structure. */
/*************************************************/
void IncrementDeftemplateBusyCount(
        const Environment&theEnv,
        Deftemplate *theTemplate) {
#if MAC_XCD
#pragma unused(theEnv)
#endif

    theTemplate->busyCount++;
}

/*******************************************************************/
/* GetNextFactInTemplate: If passed a nullptr pointer, returns the    */
/*   first fact in the template's fact-list. Otherwise returns the */
/*   next template fact following the fact passed as an argument.  */
/*******************************************************************/
Fact *GetNextFactInTemplate(
        Deftemplate *theTemplate,
        Fact *factPtr) {
    if (factPtr == nullptr) { return (theTemplate->factList); }

    if (factPtr->garbage) return nullptr;

    return (factPtr->nextTemplateFact);
}


/******************************/
/* CreateDeftemplateScopeMap: */
/******************************/
void *CreateDeftemplateScopeMap(
        const Environment&theEnv,
        Deftemplate *theDeftemplate) {
    unsigned short scopeMapSize;
    char *scopeMap;
    const char *templateName;
    Defmodule *matchModule, *theModule;
    unsigned long moduleID;
    unsigned int count;
    void *theBitMap;

    templateName = theDeftemplate->header.name->contents;
    matchModule = theDeftemplate->header.whichModule->theModule;

    scopeMapSize = (sizeof(char) * ((GetNumberOfDefmodules(theEnv) / BITS_PER_BYTE) + 1));
    scopeMap = (char *) gm2(theEnv, scopeMapSize);

    ClearBitString((void *) scopeMap, scopeMapSize);
    SaveCurrentModule(theEnv);
    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        SetCurrentModule(theEnv, theModule);
        moduleID = theModule->header.bsaveID;
        if (FindImportedConstruct(theEnv, "deftemplate", matchModule,
                                  templateName, &count, true, nullptr) != nullptr)
            SetBitMap(scopeMap, moduleID);
    }
    RestoreCurrentModule(theEnv);
    theBitMap = AddBitMap(theEnv, scopeMap, scopeMapSize);
    IncrementBitMapCount(theBitMap);
    rm(theEnv, scopeMap, scopeMapSize);
    return (theBitMap);
}


/*##################################*/
/* Additional Environment Functions */
/*##################################*/

const char *DeftemplateModule(
        Deftemplate *theDeftemplate) {
    return GetConstructModuleName(&theDeftemplate->header);
}

const char *DeftemplateName(
        Deftemplate *theDeftemplate) {
    return GetConstructNameString(&theDeftemplate->header);
}

const char *DeftemplatePPForm(
        Deftemplate *theDeftemplate) {
    return GetConstructPPForm(&theDeftemplate->header);
}
#endif
#endif /* DEFTEMPLATE_CONSTRUCT */


