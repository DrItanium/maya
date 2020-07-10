/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  07/30/16             */
/*                                                     */
/*             DEFMODULE BSAVE/BLOAD MODULE            */
/*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    defmodule construct.                                   */
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
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#if (BLOAD_AND_BSAVE)

#include <cstdio>

#include "BinaryLoad.h"
#include "BinarySave.h"
#include "Construct.h"
#include "Environment.h"
#include "MemoryAllocation.h"
#include "Defmodule.h"

#include "DefmoduleBinarySaveLoad.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
static void BsaveFind(const Environment&);
static void BsaveStorage(const Environment&, FILE *);
static void BsaveBinaryItem(const Environment&, FILE *);
#endif
static void BloadStorage(const Environment&);
static void BloadBinaryItem(const Environment&);
static void UpdateDefmodule(const Environment&, void *, unsigned long);
static void UpdatePortItem(const Environment&, void *, unsigned long);
static void ClearBload(const Environment&);

/*********************************************/
/* DefmoduleBinarySetup: Installs the binary */
/*   save/load feature for defmodules.       */
/*********************************************/
void DefmoduleBinarySetup(
        const Environment&theEnv) {
    AddBeforeBloadFunction(theEnv, "defmodule", RemoveAllDefmodules, 2000, nullptr);

#if BLOAD_AND_BSAVE
    AddBinaryItem(theEnv, "defmodule", 0, BsaveFind, nullptr,
                  BsaveStorage, BsaveBinaryItem,
                  BloadStorage, BloadBinaryItem,
                  ClearBload);
#endif

    AddAbortBloadFunction(theEnv, "defmodule", CreateMainModule, 0, nullptr);

}

/**************************************************************/
/* UpdateDefmoduleItemHeader: Updates the values in defmodule */
/*   item headers for the loaded binary image.                */
/**************************************************************/
void UpdateDefmoduleItemHeader(
        const Environment&theEnv,
        struct bsaveDefmoduleItemHeader *theBsaveHeader,
        struct defmoduleItemHeader *theHeader,
        size_t itemSize,
        void *itemArray) {
    size_t firstOffset, lastOffset;

    theHeader->theModule = ModulePointer(theBsaveHeader->theModule);
    if (theBsaveHeader->firstItem == ULONG_MAX) {
        theHeader->firstItem = nullptr;
        theHeader->lastItem = nullptr;
    } else {
        firstOffset = itemSize * theBsaveHeader->firstItem;
        lastOffset = itemSize * theBsaveHeader->lastItem;
        theHeader->firstItem =
                (ConstructHeader *) &((char *) itemArray)[firstOffset];
        theHeader->lastItem =
                (ConstructHeader *) &((char *) itemArray)[lastOffset];
    }
}

#if BLOAD_AND_BSAVE

/*********************************************************/
/* AssignBsaveDefmdlItemHdrVals: Assigns the appropriate */
/*   values to a bsave defmodule item header record.     */
/*********************************************************/
void AssignBsaveDefmdlItemHdrVals(
        struct bsaveDefmoduleItemHeader *theBsaveHeader,
        struct defmoduleItemHeader *theHeader) {
    theBsaveHeader->theModule = theHeader->theModule->header.bsaveID;
    if (theHeader->firstItem == nullptr) {
        theBsaveHeader->firstItem = ULONG_MAX;
        theBsaveHeader->lastItem = ULONG_MAX;
    } else {
        theBsaveHeader->firstItem = theHeader->firstItem->bsaveID;
        theBsaveHeader->lastItem = theHeader->lastItem->bsaveID;
    }
}

/**********************************************************/
/* BsaveFind: Counts the number of data structures which  */
/*   must be saved in the binary image for the defmodules */
/*   in the current environment.                          */
/**********************************************************/
static void BsaveFind(
        const Environment&theEnv) {
    Defmodule *defmodulePtr;
    struct portItem *theList;

    /*=======================================================*/
    /* If a binary image is already loaded, then temporarily */
    /* save the count values since these will be overwritten */
    /* in the process of saving the binary image.            */
    /*=======================================================*/

    SaveBloadCount(theEnv, DefmoduleData(theEnv)->BNumberOfDefmodules);
    SaveBloadCount(theEnv, DefmoduleData(theEnv)->NumberOfPortItems);

    /*==========================================*/
    /* Set the count of defmodule and defmodule */
    /* port items data structures to zero.      */
    /*==========================================*/

    DefmoduleData(theEnv)->BNumberOfDefmodules = 0;
    DefmoduleData(theEnv)->NumberOfPortItems = 0;

    /*===========================*/
    /* Loop through each module. */
    /*===========================*/

    for (defmodulePtr = GetNextDefmodule(theEnv, nullptr);
         defmodulePtr != nullptr;
         defmodulePtr = GetNextDefmodule(theEnv, defmodulePtr)) {
        /*==============================================*/
        /* Increment the number of modules encountered. */
        /*==============================================*/

        DefmoduleData(theEnv)->BNumberOfDefmodules++;

        /*===========================*/
        /* Mark the defmodule's name */
        /* as being a needed symbol. */
        /*===========================*/

        MarkConstructHeaderNeededItems(&defmodulePtr->header, defmodulePtr->header.bsaveID);

        /*==============================================*/
        /* Loop through each of the port items in the   */
        /* defmodule's import list incrementing the     */
        /* number of port items encountered and marking */
        /* needed symbols.                              */
        /*==============================================*/

        for (theList = defmodulePtr->importList;
             theList != nullptr;
             theList = theList->next) {
            DefmoduleData(theEnv)->NumberOfPortItems++;
            if (theList->moduleName != nullptr) { theList->moduleName->neededSymbol = true; }
            if (theList->constructType != nullptr) { theList->constructType->neededSymbol = true; }
            if (theList->constructName != nullptr) { theList->constructName->neededSymbol = true; }
        }

        /*==============================================*/
        /* Loop through each of the port items in the   */
        /* defmodule's export list incrementing the     */
        /* number of port items encountered and marking */
        /* needed symbols.                              */
        /*==============================================*/

        for (theList = defmodulePtr->exportList;
             theList != nullptr;
             theList = theList->next) {
            DefmoduleData(theEnv)->NumberOfPortItems++;
            if (theList->moduleName != nullptr) { theList->moduleName->neededSymbol = true; }
            if (theList->constructType != nullptr) { theList->constructType->neededSymbol = true; }
            if (theList->constructName != nullptr) { theList->constructName->neededSymbol = true; }
        }
    }
}

/*********************************************************/
/* BsaveStorage: Writes out the storage requirements for */
/*    all defmodule structures to the binary file.       */
/*********************************************************/
static void BsaveStorage(
        const Environment&theEnv,
        FILE *fp) {
    size_t space;

    space = sizeof(long) * 2;
    GenWrite(&space, sizeof(size_t), fp);
    GenWrite(&DefmoduleData(theEnv)->BNumberOfDefmodules, sizeof(long), fp);
    GenWrite(&DefmoduleData(theEnv)->NumberOfPortItems, sizeof(long), fp);
}

/*********************************************/
/* BsaveBinaryItem: Writes out all defmodule */
/*   structures to the binary file.          */
/*********************************************/
static void BsaveBinaryItem(
        const Environment&theEnv,
        FILE *fp) {
    size_t space;
    Defmodule *defmodulePtr;
    struct bsaveDefmodule newDefmodule;
    struct bsavePortItem newPortItem;
    struct portItem *theList;

    /*=========================================================*/
    /* Write out the amount of space taken up by the defmodule */
    /* and port item data structures in the binary image.      */
    /*=========================================================*/

    space = DefmoduleData(theEnv)->BNumberOfDefmodules * sizeof(bsaveDefmodule);
    space += DefmoduleData(theEnv)->NumberOfPortItems * sizeof(bsavePortItem);
    GenWrite(&space, sizeof(size_t), fp);

    /*==========================================*/
    /* Write out each defmodule data structure. */
    /*==========================================*/

    DefmoduleData(theEnv)->BNumberOfDefmodules = 0;
    DefmoduleData(theEnv)->NumberOfPortItems = 0;
    for (defmodulePtr = GetNextDefmodule(theEnv, nullptr);
         defmodulePtr != nullptr;
         defmodulePtr = GetNextDefmodule(theEnv, defmodulePtr)) {
        AssignBsaveConstructHeaderVals(&newDefmodule.header, &defmodulePtr->header);

        DefmoduleData(theEnv)->BNumberOfDefmodules++;

        if (defmodulePtr->importList == nullptr) { newDefmodule.importList = ULONG_MAX; }
        else {
            newDefmodule.importList = DefmoduleData(theEnv)->NumberOfPortItems;
            for (theList = defmodulePtr->importList;
                 theList != nullptr;
                 theList = theList->next) { DefmoduleData(theEnv)->NumberOfPortItems++; }
        }

        if (defmodulePtr->exportList == nullptr) { newDefmodule.exportList = ULONG_MAX; }
        else {
            newDefmodule.exportList = DefmoduleData(theEnv)->NumberOfPortItems;
            for (theList = defmodulePtr->exportList;
                 theList != nullptr;
                 theList = theList->next) { DefmoduleData(theEnv)->NumberOfPortItems++; }
        }

        newDefmodule.bsaveID = defmodulePtr->header.bsaveID;
        GenWrite(&newDefmodule, sizeof(bsaveDefmodule), fp);
    }

    /*==========================================*/
    /* Write out each port item data structure. */
    /*==========================================*/

    DefmoduleData(theEnv)->NumberOfPortItems = 0;
    defmodulePtr = GetNextDefmodule(theEnv, nullptr);
    while (defmodulePtr != nullptr) {
        for (theList = defmodulePtr->importList;
             theList != nullptr;
             theList = theList->next) {
            DefmoduleData(theEnv)->NumberOfPortItems++;
            if (theList->moduleName == nullptr) newPortItem.moduleName = ULONG_MAX;
            else newPortItem.moduleName = theList->moduleName->bucket;

            if (theList->constructType == nullptr) newPortItem.constructType = ULONG_MAX;
            else newPortItem.constructType = theList->constructType->bucket;

            if (theList->constructName == nullptr) newPortItem.constructName = ULONG_MAX;
            else newPortItem.constructName = theList->constructName->bucket;

            if (theList->next == nullptr) newPortItem.next = ULONG_MAX;
            else newPortItem.next = DefmoduleData(theEnv)->NumberOfPortItems;

            GenWrite(&newPortItem, sizeof(bsavePortItem), fp);
        }

        for (theList = defmodulePtr->exportList;
             theList != nullptr;
             theList = theList->next) {
            DefmoduleData(theEnv)->NumberOfPortItems++;
            if (theList->moduleName == nullptr) newPortItem.moduleName = ULONG_MAX;
            else newPortItem.moduleName = theList->moduleName->bucket;

            if (theList->constructType == nullptr) newPortItem.constructType = ULONG_MAX;
            else newPortItem.constructType = theList->constructType->bucket;

            if (theList->constructName == nullptr) newPortItem.constructName = ULONG_MAX;
            else newPortItem.constructName = theList->constructName->bucket;

            if (theList->next == nullptr) newPortItem.next = ULONG_MAX;
            else newPortItem.next = DefmoduleData(theEnv)->NumberOfPortItems;

            GenWrite(&newPortItem, sizeof(bsavePortItem), fp);
        }

        defmodulePtr = GetNextDefmodule(theEnv, defmodulePtr);
    }

    /*=============================================================*/
    /* If a binary image was already loaded when the bsave command */
    /* was issued, then restore the counts indicating the number   */
    /* of defmodule and port items in the binary image (these were */
    /* overwritten by the binary save).                            */
    /*=============================================================*/

    RestoreBloadCount(theEnv, &DefmoduleData(theEnv)->BNumberOfDefmodules);
    RestoreBloadCount(theEnv, &DefmoduleData(theEnv)->NumberOfPortItems);
}

#endif /* BLOAD_AND_BSAVE */

/**********************************************************/
/* BloadStorage: Allocates storage requirements for the   */
/*   defmodules and port items used by this binary image. */
/**********************************************************/
static void BloadStorage(
        const Environment&theEnv) {
    size_t space;

    /*=======================================*/
    /* Determine the number of defmodule and */
    /* port item data structures to be read. */
    /*=======================================*/

    GenReadBinary(theEnv, &space, sizeof(size_t));
    GenReadBinary(theEnv, &DefmoduleData(theEnv)->BNumberOfDefmodules, sizeof(unsigned long));
    GenReadBinary(theEnv, &DefmoduleData(theEnv)->NumberOfPortItems, sizeof(unsigned long));

    /*================================*/
    /* Allocate the space needed for  */
    /* the defmodule data structures. */
    /*================================*/

    if (DefmoduleData(theEnv)->BNumberOfDefmodules == 0) {
        DefmoduleData(theEnv)->DefmoduleArray = nullptr;
        return;
    }

    space = (DefmoduleData(theEnv)->BNumberOfDefmodules * sizeof(Defmodule));
    DefmoduleData(theEnv)->DefmoduleArray = (Defmodule *) genalloc(theEnv, space);

    /*================================*/
    /* Allocate the space needed for  */
    /* the port item data structures. */
    /*================================*/

    if (DefmoduleData(theEnv)->NumberOfPortItems == 0) {
        DefmoduleData(theEnv)->PortItemArray = nullptr;
        return;
    }

    space = (DefmoduleData(theEnv)->NumberOfPortItems * sizeof(portItem));
    DefmoduleData(theEnv)->PortItemArray = (portItem *) genalloc(theEnv, space);
}

/********************************************/
/* BloadBinaryItem: Loads and refreshes the */
/*   defmodules used by this binary image.  */
/********************************************/
static void BloadBinaryItem(
        const Environment&theEnv) {
    size_t space;

    GenReadBinary(theEnv, &space, sizeof(size_t));
    if (DefmoduleData(theEnv)->BNumberOfDefmodules == 0) return;

    BloadandRefresh(theEnv, DefmoduleData(theEnv)->BNumberOfDefmodules, sizeof(bsaveDefmodule), UpdateDefmodule);
    BloadandRefresh(theEnv, DefmoduleData(theEnv)->NumberOfPortItems, sizeof(bsavePortItem), UpdatePortItem);

    SetListOfDefmodules(theEnv, DefmoduleData(theEnv)->DefmoduleArray);
    SetCurrentModule(theEnv, GetNextDefmodule(theEnv, nullptr));
}

/******************************************/
/* UpdateDefmodule: Bload refresh routine */
/*   for defmodule data structure.        */
/******************************************/
static void UpdateDefmodule(
        const Environment&theEnv,
        void *buf,
        unsigned long obji) {
    struct bsaveDefmodule *bdp;
    struct moduleItem *theItem;
    unsigned int i;

    bdp = (bsaveDefmodule *) buf;

    UpdateConstructHeader(theEnv, &bdp->header, &DefmoduleData(theEnv)->DefmoduleArray[obji].header, DEFMODULE,
                          0, nullptr, sizeof(Defmodule), DefmoduleData(theEnv)->DefmoduleArray);

    if (GetNumberOfModuleItems(theEnv) == 0) { DefmoduleData(theEnv)->DefmoduleArray[obji].itemsArray = nullptr; }
    else {
        DefmoduleData(theEnv)->DefmoduleArray[obji].itemsArray =
                (defmoduleItemHeader **) gm2(theEnv, sizeof(void *) * GetNumberOfModuleItems(theEnv));
    }

    for (i = 0, theItem = GetListOfModuleItems(theEnv);
         (i < GetNumberOfModuleItems(theEnv)) && (theItem != nullptr);
         i++, theItem = theItem->next) {
        if (theItem->bloadModuleReference == nullptr) { DefmoduleData(theEnv)->DefmoduleArray[obji].itemsArray[i] = nullptr; }
        else {
            DefmoduleData(theEnv)->DefmoduleArray[obji].itemsArray[i] =
                    (defmoduleItemHeader *)
                            (*theItem->bloadModuleReference)(theEnv, obji);
        }
    }

    DefmoduleData(theEnv)->DefmoduleArray[obji].header.ppForm = nullptr;

    if (bdp->importList != ULONG_MAX) {
        DefmoduleData(theEnv)->DefmoduleArray[obji].importList = (portItem *) &DefmoduleData(theEnv)->PortItemArray[bdp->importList];
    } else { DefmoduleData(theEnv)->DefmoduleArray[obji].importList = nullptr; }

    if (bdp->exportList != ULONG_MAX) {
        DefmoduleData(theEnv)->DefmoduleArray[obji].exportList = (portItem *) &DefmoduleData(theEnv)->PortItemArray[bdp->exportList];
    } else { DefmoduleData(theEnv)->DefmoduleArray[obji].exportList = nullptr; }
    DefmoduleData(theEnv)->DefmoduleArray[obji].header.bsaveID = bdp->bsaveID;
}

/*****************************************/
/* UpdatePortItem: Bload refresh routine */
/*   for port item data structure.       */
/*****************************************/
static void UpdatePortItem(
        const Environment&theEnv,
        void *buf,
        unsigned long obji) {
    struct bsavePortItem *bdp;

    bdp = (bsavePortItem *) buf;

    if (bdp->moduleName != ULONG_MAX) {
        DefmoduleData(theEnv)->PortItemArray[obji].moduleName = SymbolPointer(bdp->moduleName);
        IncrementLexemeCount(DefmoduleData(theEnv)->PortItemArray[obji].moduleName);
    } else { DefmoduleData(theEnv)->PortItemArray[obji].moduleName = nullptr; }

    if (bdp->constructType != ULONG_MAX) {
        DefmoduleData(theEnv)->PortItemArray[obji].constructType = SymbolPointer(bdp->constructType);
        IncrementLexemeCount(DefmoduleData(theEnv)->PortItemArray[obji].constructType);
    } else { DefmoduleData(theEnv)->PortItemArray[obji].constructType = nullptr; }

    if (bdp->constructName != ULONG_MAX) {
        DefmoduleData(theEnv)->PortItemArray[obji].constructName = SymbolPointer(bdp->constructName);
        IncrementLexemeCount(DefmoduleData(theEnv)->PortItemArray[obji].constructName);
    } else { DefmoduleData(theEnv)->PortItemArray[obji].constructName = nullptr; }

    if (bdp->next != ULONG_MAX) {
        DefmoduleData(theEnv)->PortItemArray[obji].next = (portItem *) &DefmoduleData(theEnv)->PortItemArray[bdp->next];
    } else { DefmoduleData(theEnv)->PortItemArray[obji].next = nullptr; }
}

/***************************************/
/* ClearBload: Defmodule clear routine */
/*   when a binary load is in effect.  */
/***************************************/
static void ClearBload(
        const Environment&theEnv) {
    unsigned long i;
    size_t space;
    struct portItem *theList;

    /*===========================*/
    /* Decrement in use counters */
    /* used by the binary image. */
    /*===========================*/

    for (i = 0; i < DefmoduleData(theEnv)->BNumberOfDefmodules; i++) {
        ReleaseLexeme(theEnv, DefmoduleData(theEnv)->DefmoduleArray[i].header.name);
        for (theList = DefmoduleData(theEnv)->DefmoduleArray[i].importList;
             theList != nullptr;
             theList = theList->next) {
            if (theList->moduleName != nullptr) ReleaseLexeme(theEnv, theList->moduleName);
            if (theList->constructType != nullptr) ReleaseLexeme(theEnv, theList->constructType);
            if (theList->constructName != nullptr) ReleaseLexeme(theEnv, theList->constructName);
        }

        for (theList = DefmoduleData(theEnv)->DefmoduleArray[i].exportList;
             theList != nullptr;
             theList = theList->next) {
            if (theList->moduleName != nullptr) ReleaseLexeme(theEnv, theList->moduleName);
            if (theList->constructType != nullptr) ReleaseLexeme(theEnv, theList->constructType);
            if (theList->constructName != nullptr) ReleaseLexeme(theEnv, theList->constructName);
        }

        rm(theEnv, DefmoduleData(theEnv)->DefmoduleArray[i].itemsArray, sizeof(void *) * GetNumberOfModuleItems(theEnv));
    }

    /*================================*/
    /* Deallocate the space used for  */
    /* the defmodule data structures. */
    /*================================*/

    space = DefmoduleData(theEnv)->BNumberOfDefmodules * sizeof(Defmodule);
    if (space != 0) genfree(theEnv, DefmoduleData(theEnv)->DefmoduleArray, space);
    DefmoduleData(theEnv)->BNumberOfDefmodules = 0;

    /*================================*/
    /* Deallocate the space used for  */
    /* the port item data structures. */
    /*================================*/

    space = DefmoduleData(theEnv)->NumberOfPortItems * sizeof(portItem);
    if (space != 0) genfree(theEnv, DefmoduleData(theEnv)->PortItemArray, space);
    DefmoduleData(theEnv)->NumberOfPortItems = 0;

    /*===========================*/
    /* Reset module information. */
    /*===========================*/

    SetListOfDefmodules(theEnv, nullptr);
    CreateMainModule(theEnv, nullptr);
    DefmoduleData(theEnv)->MainModuleRedefinable = true;
}

#endif /*  (BLOAD_AND_BSAVE) */


