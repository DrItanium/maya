/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  08/06/16             */
/*                                                     */
/*              DEFFACTS DEFINITION MODULE             */
/*******************************************************/

/*************************************************************/
/* Purpose: Defines basic deffacts primitive functions such  */
/*   as allocating and deallocating, traversing, and finding */
/*   deffacts data structures.                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
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

#if DEFFACTS_CONSTRUCT

#include <stdio.h>

#include "DeffactsBasicCommands.h"
#include "DeffactsParser.h"
#include "Environment.h"
#include "MemoryAllocation.h"

#if BLOAD_AND_BSAVE
#include "BinaryLoad.h"
#include "DeffactsBinarySaveLoad.h"
#endif

#include "Deffacts.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void *AllocateModule(Environment *);
static void ReturnModule(Environment *, void *);
static void ReturnDeffacts(Environment *, Deffacts *);
static void InitializeDeffactsModules(Environment *);
static void DeallocateDeffactsData(Environment *);
static void DestroyDeffactsAction(Environment *, ConstructHeader *, void *);

/***********************************************************/
/* InitializeDeffacts: Initializes the deffacts construct. */
/***********************************************************/
void InitializeDeffacts(
        Environment *theEnv) {
    AllocateEnvironmentData(theEnv, DEFFACTS_DATA, sizeof(struct deffactsData), DeallocateDeffactsData);

    InitializeDeffactsModules(theEnv);

    DeffactsBasicCommands(theEnv);

    DeffactsData(theEnv)->DeffactsConstruct =
            AddConstruct(theEnv, "deffacts", "deffacts", ParseDeffacts,
                         (FindConstructFunction *) FindDeffacts,
                         GetConstructNamePointer, GetConstructPPForm,
                         GetConstructModuleItem,
                         (GetNextConstructFunction *) GetNextDeffacts,
                         SetNextConstruct,
                         (IsConstructDeletableFunction *) DeffactsIsDeletable,
                         (DeleteConstructFunction *) Undeffacts,
                         (FreeConstructFunction *) ReturnDeffacts);
}

/***************************************************/
/* DeallocateDeffactsData: Deallocates environment */
/*    data for the deffacts construct.             */
/***************************************************/
static void DeallocateDeffactsData(
        Environment *theEnv) {
    struct deffactsModule *theModuleItem;
    Defmodule *theModule;

#if BLOAD_AND_BSAVE
    if (Bloaded(theEnv)) return;
#endif

    DoForAllConstructs(theEnv,
                       DestroyDeffactsAction,
                       DeffactsData(theEnv)->DeffactsModuleIndex,
                       false, NULL);

    for (theModule = GetNextDefmodule(theEnv, NULL);
         theModule != NULL;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        theModuleItem = (struct deffactsModule *)
                GetModuleItem(theEnv, theModule,
                              DeffactsData(theEnv)->DeffactsModuleIndex);
        rtn_struct(theEnv, deffactsModule, theModuleItem);
    }
}

/*********************************************************/
/* DestroyDeffactsAction: Action used to remove deffacts */
/*   as a result of DestroyEnvironment.                  */
/*********************************************************/
static void DestroyDeffactsAction(
        Environment *theEnv,
        ConstructHeader *theConstruct,
        void *buffer) {
#if MAC_XCD
#pragma unused(buffer)
#endif
    Deffacts *theDeffacts = (Deffacts *) theConstruct;

    if (theDeffacts == NULL) return;

    ReturnPackedExpression(theEnv, theDeffacts->assertList);

    DestroyConstructHeader(theEnv, &theDeffacts->header);

    rtn_struct(theEnv, deffacts, theDeffacts);
}

/*******************************************************/
/* InitializeDeffactsModules: Initializes the deffacts */
/*   construct for use with the defmodule construct.   */
/*******************************************************/
static void InitializeDeffactsModules(
        Environment *theEnv) {
    DeffactsData(theEnv)->DeffactsModuleIndex =
            RegisterModuleItem(theEnv, "deffacts",
                               AllocateModule,
                               ReturnModule,
#if BLOAD_AND_BSAVE
                               BloadDeffactsModuleReference,
#else
                    NULL,
#endif
                               (FindConstructFunction *) FindDeffactsInModule);
}

/************************************************/
/* AllocateModule: Allocates a deffacts module. */
/************************************************/
static void *AllocateModule(
        Environment *theEnv) {
    return ((void *) get_struct(theEnv, deffactsModule));
}

/************************************************/
/* ReturnModule: Deallocates a deffacts module. */
/************************************************/
static void ReturnModule(
        Environment *theEnv,
        void *theItem) {
    FreeConstructHeaderModule(theEnv, (struct defmoduleItemHeader *) theItem, DeffactsData(theEnv)->DeffactsConstruct);
    rtn_struct(theEnv, deffactsModule, theItem);
}

/*************************************************************/
/* GetDeffactsModuleItem: Returns a pointer to the defmodule */
/*  item for the specified deffacts or defmodule.            */
/*************************************************************/
struct deffactsModule *GetDeffactsModuleItem(
        Environment *theEnv,
        Defmodule *theModule) {
    return ((struct deffactsModule *) GetConstructModuleItemByIndex(theEnv, theModule, DeffactsData(theEnv)->DeffactsModuleIndex));
}

/************************************************/
/* FindDeffacts: Searches for a deffact in the  */
/*   list of deffacts. Returns a pointer to the */
/*   deffact if found, otherwise NULL.          */
/************************************************/
Deffacts *FindDeffacts(
        Environment *theEnv,
        const char *deffactsName) {
    return (Deffacts *) FindNamedConstructInModuleOrImports(theEnv, deffactsName, DeffactsData(theEnv)->DeffactsConstruct);
}

/************************************************/
/* FindDeffactsInModule: Searches for a deffact */
/*   in the list of deffacts. Returns a pointer */
/*   to the deffact if found, otherwise NULL.   */
/************************************************/
Deffacts *FindDeffactsInModule(
        Environment *theEnv,
        const char *deffactsName) {
    return (Deffacts *) FindNamedConstructInModule(theEnv, deffactsName, DeffactsData(theEnv)->DeffactsConstruct);
}

/*********************************************************/
/* GetNextDeffacts: If passed a NULL pointer, returns    */
/*   the first deffacts in the ListOfDeffacts. Otherwise */
/*   returns the next deffacts following the deffacts    */
/*   passed as an argument.                              */
/*********************************************************/
Deffacts *GetNextDeffacts(
        Environment *theEnv,
        Deffacts *deffactsPtr) {
    return (Deffacts *) GetNextConstructItem(theEnv, &deffactsPtr->header, DeffactsData(theEnv)->DeffactsModuleIndex);
}

/*******************************************************/
/* DeffactsIsDeletable: Returns true if a particular   */
/*   deffacts can be deleted, otherwise returns false. */
/*******************************************************/
bool DeffactsIsDeletable(
        Deffacts *theDeffacts) {
    Environment *theEnv = theDeffacts->header.env;

    if (!ConstructsDeletable(theEnv)) { return false; }

    if (ConstructData(theEnv)->ResetInProgress) return false;

    return true;
}

/***********************************************************/
/* ReturnDeffacts: Returns the data structures associated  */
/*   with a deffacts construct to the pool of free memory. */
/***********************************************************/
static void ReturnDeffacts(
        Environment *theEnv,
        Deffacts *theDeffacts) {
    if (theDeffacts == NULL) return;

    ExpressionDeinstall(theEnv, theDeffacts->assertList);
    ReturnPackedExpression(theEnv, theDeffacts->assertList);

    DeinstallConstructHeader(theEnv, &theDeffacts->header);

    rtn_struct(theEnv, deffacts, theDeffacts);
}

/*##################################*/
/* Additional Environment Functions */
/*##################################*/

const char *DeffactsModule(
        Deffacts *theDeffacts) {
    return GetConstructModuleName(&theDeffacts->header);
}

const char *DeffactsName(
        Deffacts *theDeffacts) {
    return GetConstructNameString(&theDeffacts->header);
}

const char *DeffactsPPForm(
        Deffacts *theDeffacts) {
    return GetConstructPPForm(&theDeffacts->header);
}

#endif /* DEFFACTS_CONSTRUCT */


