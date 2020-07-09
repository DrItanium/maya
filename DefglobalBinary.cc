/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  07/30/16             */
/*                                                     */
/*            DEFGLOBAL BSAVE/BLOAD MODULE             */
/*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    defglobal construct.                                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Moved WatchGlobals global to defglobalData.    */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#if DEFGLOBAL_CONSTRUCT && (BLOAD_AND_BSAVE)

#include <cstdio>

#include "BinaryLoad.h"
#include "BinarySave.h"
#include "Environment.h"
#include "DefglobalBasicCommands.h"
#include "Defglobal.h"
#include "MemoryAllocation.h"
#include "Defmodule.h"
#include "Multifield.h"

#include "DefglobalBinary.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
static void BsaveFind(Environment *);
static void BsaveStorage(Environment *, FILE *);
static void BsaveBinaryItem(Environment *, FILE *);
#endif
static void BloadStorageDefglobals(Environment *);
static void BloadBinaryItem(Environment *);
static void UpdateDefglobalModule(Environment *, void *, unsigned long);
static void UpdateDefglobal(Environment *, void *, unsigned long);
static void ClearBload(Environment *);
static void DeallocateDefglobalBloadData(Environment *);

/*********************************************/
/* DefglobalBinarySetup: Installs the binary */
/*   save/load feature for the defglobals.   */
/*********************************************/
void DefglobalBinarySetup(
        Environment *theEnv) {
    AllocateEnvironmentData(theEnv, GLOBLBIN_DATA, sizeof(defglobalBinaryData), DeallocateDefglobalBloadData);
#if (BLOAD_AND_BSAVE)
    AddAfterBloadFunction(theEnv, "defglobal", ResetDefglobals, 50, nullptr);
#endif

#if BLOAD_AND_BSAVE
    AddBinaryItem(theEnv, "defglobal", 0, BsaveFind, nullptr,
                  BsaveStorage, BsaveBinaryItem,
                  BloadStorageDefglobals, BloadBinaryItem,
                  ClearBload);
#endif

}

/*********************************************************/
/* DeallocateDefglobalBloadData: Deallocates environment */
/*    data for the defglobal bsave functionality.        */
/*********************************************************/
static void DeallocateDefglobalBloadData(
        Environment *theEnv) {
#if (BLOAD_AND_BSAVE)
    size_t space;
    unsigned long i;

    for (i = 0; i < DefglobalBinaryData(theEnv)->NumberOfDefglobals; i++) {
        if (DefglobalBinaryData(theEnv)->DefglobalArray[i].current.header->type == MULTIFIELD_TYPE) {
            ReturnMultifield(theEnv,
                             DefglobalBinaryData(
                                     theEnv)->DefglobalArray[i].current.multifieldValue);
        }
    }

    space = DefglobalBinaryData(theEnv)->NumberOfDefglobals * sizeof(Defglobal);
    if (space != 0) { genfree(theEnv, DefglobalBinaryData(theEnv)->DefglobalArray, space); }

    space = DefglobalBinaryData(theEnv)->NumberOfDefglobalModules * sizeof(defglobalModule);
    if (space != 0) { genfree(theEnv, DefglobalBinaryData(theEnv)->ModuleArray, space); }
#endif
}

#if BLOAD_AND_BSAVE

/****************************************************/
/* BsaveFind:  Counts the number of data structures */
/*   which must be saved in the binary image for    */
/*   the defglobals in the current environment.     */
/****************************************************/
static void BsaveFind(
        Environment *theEnv) {
    Defglobal *defglobalPtr;
    Defmodule *theModule;

    /*=======================================================*/
    /* If a binary image is already loaded, then temporarily */
    /* save the count values since these will be overwritten */
    /* in the process of saving the binary image.            */
    /*=======================================================*/

    SaveBloadCount(theEnv, DefglobalBinaryData(theEnv)->NumberOfDefglobalModules);
    SaveBloadCount(theEnv, DefglobalBinaryData(theEnv)->NumberOfDefglobals);

    /*============================================*/
    /* Set the count of defglobals and defglobals */
    /* module data structures to zero.            */
    /*============================================*/

    DefglobalBinaryData(theEnv)->NumberOfDefglobals = 0;
    DefglobalBinaryData(theEnv)->NumberOfDefglobalModules = 0;

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        /*================================================*/
        /* Set the current module to the module being     */
        /* examined and increment the number of defglobal */
        /* modules encountered.                           */
        /*================================================*/

        SetCurrentModule(theEnv, theModule);
        DefglobalBinaryData(theEnv)->NumberOfDefglobalModules++;

        /*====================================================*/
        /* Loop through each defglobal in the current module. */
        /*====================================================*/

        for (defglobalPtr = GetNextDefglobal(theEnv, nullptr);
             defglobalPtr != nullptr;
             defglobalPtr = GetNextDefglobal(theEnv, defglobalPtr)) {
            /*======================================================*/
            /* Initialize the construct header for the binary save. */
            /*======================================================*/

            MarkConstructHeaderNeededItems(&defglobalPtr->header, DefglobalBinaryData(theEnv)->NumberOfDefglobals++);
        }
    }
}

/*****************************************************/
/* BsaveStorage: Writes out storage requirements for */
/*   all defglobal structures to the binary file     */
/*****************************************************/
static void BsaveStorage(
        Environment *theEnv,
        FILE *fp) {
    size_t space;

    /*===========================================================*/
    /* Only two data structures are saved as part of a defglobal */
    /* binary image: the defglobal data structure and the        */
    /* defglobalModule data structure.                           */
    /*===========================================================*/

    space = sizeof(long) * 2;
    GenWrite(&space, sizeof(size_t), fp);
    GenWrite(&DefglobalBinaryData(theEnv)->NumberOfDefglobals, sizeof(long), fp);
    GenWrite(&DefglobalBinaryData(theEnv)->NumberOfDefglobalModules, sizeof(long), fp);
}

/*********************************************/
/* BsaveBinaryItem: Writes out all defglobal */
/*   structures to the binary file           */
/*********************************************/
static void BsaveBinaryItem(
        Environment *theEnv,
        FILE *fp) {
    size_t space;
    Defglobal *theDefglobal;
    struct bsaveDefglobal newDefglobal;
    Defmodule *theModule;
    struct bsaveDefglobalModule tempDefglobalModule;
    struct defglobalModule *theModuleItem;

    /*==========================================================*/
    /* Write out the amount of space taken up by the defglobal  */
    /* and defglobalModule data structures in the binary image. */
    /*==========================================================*/

    space = DefglobalBinaryData(theEnv)->NumberOfDefglobals * sizeof(bsaveDefglobal) +
            (DefglobalBinaryData(theEnv)->NumberOfDefglobalModules * sizeof(bsaveDefglobalModule));
    GenWrite(&space, sizeof(size_t), fp);

    /*=================================================*/
    /* Write out each defglobal module data structure. */
    /*=================================================*/

    DefglobalBinaryData(theEnv)->NumberOfDefglobals = 0;
    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        SetCurrentModule(theEnv, theModule);

        theModuleItem = (defglobalModule *)
                GetModuleItem(theEnv, nullptr, FindModuleItem(theEnv, "defglobal")->moduleIndex);
        AssignBsaveDefmdlItemHdrVals(&tempDefglobalModule.header,
                                     &theModuleItem->header);
        GenWrite(&tempDefglobalModule, sizeof(bsaveDefglobalModule), fp);
    }

    /*===========================*/
    /* Write out each defglobal. */
    /*===========================*/

    DefglobalBinaryData(theEnv)->NumberOfDefglobals = 0;
    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        SetCurrentModule(theEnv, theModule);

        for (theDefglobal = GetNextDefglobal(theEnv, nullptr);
             theDefglobal != nullptr;
             theDefglobal = GetNextDefglobal(theEnv, theDefglobal)) {
            AssignBsaveConstructHeaderVals(&newDefglobal.header,
                                           &theDefglobal->header);
            newDefglobal.initial = HashedExpressionIndex(theEnv, theDefglobal->initial);

            GenWrite(&newDefglobal, sizeof(bsaveDefglobal), fp);
        }
    }

    /*=============================================================*/
    /* If a binary image was already loaded when the bsave command */
    /* was issued, then restore the counts indicating the number   */
    /* of defglobals and defglobal modules in the binary image     */
    /* (these were overwritten by the binary save).                */
    /*=============================================================*/

    RestoreBloadCount(theEnv, &DefglobalBinaryData(theEnv)->NumberOfDefglobalModules);
    RestoreBloadCount(theEnv, &DefglobalBinaryData(theEnv)->NumberOfDefglobals);
}

#endif /* BLOAD_AND_BSAVE */

/***********************************************/
/* BloadStorageDefglobals: Allocates space for */
/*   the defglobals used by this binary image. */
/***********************************************/
static void BloadStorageDefglobals(
        Environment *theEnv) {
    size_t space;

    /*=======================================================*/
    /* Determine the number of defglobal and defglobalModule */
    /* data structures to be read.                           */
    /*=======================================================*/

    GenReadBinary(theEnv, &space, sizeof(size_t));
    GenReadBinary(theEnv, &DefglobalBinaryData(theEnv)->NumberOfDefglobals, sizeof(long));
    GenReadBinary(theEnv, &DefglobalBinaryData(theEnv)->NumberOfDefglobalModules, sizeof(long));

    /*===================================*/
    /* Allocate the space needed for the */
    /* defglobalModule data structures.  */
    /*===================================*/

    if (DefglobalBinaryData(theEnv)->NumberOfDefglobalModules == 0) {
        DefglobalBinaryData(theEnv)->DefglobalArray = nullptr;
        DefglobalBinaryData(theEnv)->ModuleArray = nullptr;
    }

    space = DefglobalBinaryData(theEnv)->NumberOfDefglobalModules * sizeof(defglobalModule);
    DefglobalBinaryData(theEnv)->ModuleArray = (defglobalModule *) genalloc(theEnv, space);

    /*===================================*/
    /* Allocate the space needed for the */
    /* defglobal data structures.        */
    /*===================================*/

    if (DefglobalBinaryData(theEnv)->NumberOfDefglobals == 0) {
        DefglobalBinaryData(theEnv)->DefglobalArray = nullptr;
        return;
    }

    space = (DefglobalBinaryData(theEnv)->NumberOfDefglobals * sizeof(Defglobal));
    DefglobalBinaryData(theEnv)->DefglobalArray = (Defglobal *) genalloc(theEnv, space);
}

/******************************************************/
/* BloadBinaryItem: Loads and refreshes the defglobal */
/*   constructs used by this binary image.            */
/******************************************************/
static void BloadBinaryItem(
        Environment *theEnv) {
    size_t space;

    /*======================================================*/
    /* Read in the amount of space used by the binary image */
    /* (this is used to skip the construct in the event it  */
    /* is not available in the version being run).          */
    /*======================================================*/

    GenReadBinary(theEnv, &space, sizeof(size_t));

    /*=============================================*/
    /* Read in the defglobalModule data structures */
    /* and refresh the pointers.                   */
    /*=============================================*/

    BloadandRefresh(theEnv, DefglobalBinaryData(theEnv)->NumberOfDefglobalModules,
                    sizeof(bsaveDefglobalModule),
                    UpdateDefglobalModule);

    /*=======================================*/
    /* Read in the defglobal data structures */
    /* and refresh the pointers.             */
    /*=======================================*/

    BloadandRefresh(theEnv, DefglobalBinaryData(theEnv)->NumberOfDefglobals,
                    sizeof(bsaveDefglobal),
                    UpdateDefglobal);
}

/************************************************/
/* UpdateDefglobalModule: Bload refresh routine */
/*   for defglobal module data structures.      */
/************************************************/
static void UpdateDefglobalModule(
        Environment *theEnv,
        void *buf,
        unsigned long obji) {
    struct bsaveDefglobalModule *bdmPtr;

    bdmPtr = (bsaveDefglobalModule *) buf;

    UpdateDefmoduleItemHeader(theEnv, &bdmPtr->header, &DefglobalBinaryData(theEnv)->ModuleArray[obji].header,
                              sizeof(Defglobal),
                              DefglobalBinaryData(theEnv)->DefglobalArray);
}

/******************************************/
/* UpdateDefglobal: Bload refresh routine */
/*   for defglobal data structures.       */
/******************************************/
static void UpdateDefglobal(
        Environment *theEnv,
        void *buf,
        unsigned long obji) {
    struct bsaveDefglobal *bdp;

    bdp = (bsaveDefglobal *) buf;
    UpdateConstructHeader(theEnv, &bdp->header, &DefglobalBinaryData(theEnv)->DefglobalArray[obji].header, DEFGLOBAL,
                          sizeof(defglobalModule), DefglobalBinaryData(theEnv)->ModuleArray,
                          sizeof(Defglobal), DefglobalBinaryData(theEnv)->DefglobalArray);

#if DEBUGGING_FUNCTIONS
    DefglobalBinaryData(theEnv)->DefglobalArray[obji].watch = DefglobalData(theEnv)->WatchGlobals;
#endif
    DefglobalBinaryData(theEnv)->DefglobalArray[obji].initial = HashedExpressionPointer(bdp->initial);
    DefglobalBinaryData(theEnv)->DefglobalArray[obji].current.voidValue = VoidConstant(theEnv);
}

/***************************************/
/* ClearBload: Defglobal clear routine */
/*   when a binary load is in effect.  */
/***************************************/
static void ClearBload(
        Environment *theEnv) {
    unsigned long i;
    size_t space;

    /*=======================================================*/
    /* Decrement in use counters for atomic values contained */
    /* in the construct headers. Also decrement data         */
    /* structures used to store the defglobal's value.       */
    /*=======================================================*/

    for (i = 0; i < DefglobalBinaryData(theEnv)->NumberOfDefglobals; i++) {
        UnmarkConstructHeader(theEnv, &DefglobalBinaryData(theEnv)->DefglobalArray[i].header);

        Release(theEnv, DefglobalBinaryData(theEnv)->DefglobalArray[i].current.header);
        if (DefglobalBinaryData(theEnv)->DefglobalArray[i].current.header->type == MULTIFIELD_TYPE) {
            ReturnMultifield(theEnv,
                             DefglobalBinaryData(
                                     theEnv)->DefglobalArray[i].current.multifieldValue);
        }
    }

    /*==============================================================*/
    /* Deallocate the space used for the defglobal data structures. */
    /*==============================================================*/

    space = DefglobalBinaryData(theEnv)->NumberOfDefglobals * sizeof(Defglobal);
    if (space != 0) genfree(theEnv, DefglobalBinaryData(theEnv)->DefglobalArray, space);
    DefglobalBinaryData(theEnv)->NumberOfDefglobals = 0;

    /*=====================================================================*/
    /* Deallocate the space used for the defglobal module data structures. */
    /*=====================================================================*/

    space = DefglobalBinaryData(theEnv)->NumberOfDefglobalModules * sizeof(defglobalModule);
    if (space != 0) genfree(theEnv, DefglobalBinaryData(theEnv)->ModuleArray, space);
    DefglobalBinaryData(theEnv)->NumberOfDefglobalModules = 0;
}

/********************************************************/
/* BloadDefglobalModuleReference: Returns the defglobal */
/*   module pointer for using with the bload function.  */
/********************************************************/
void *BloadDefglobalModuleReference(
        Environment *theEnv,
        unsigned long theIndex) {
    return (void *) &DefglobalBinaryData(theEnv)->ModuleArray[theIndex];
}

#endif /* DEFGLOBAL_CONSTRUCT && (BLOAD_AND_BSAVE) */



