/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  07/30/16             */
/*                                                     */
/*             DEFFACTS BSAVE/BLOAD MODULE             */
/*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    deffacts construct.                                    */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#if DEFFACTS_CONSTRUCT && (BLOAD_AND_BSAVE)

#include <cstdio>

#include "BinaryLoad.h"
#include "BinarySave.h"
#include "Deffacts.h"
#include "Environment.h"
#include "MemoryAllocation.h"
#include "Defmodule.h"

#include "DeffactsBinarySaveLoad.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
static void BsaveFind(const Environment&);
static void BsaveExpressions(const Environment&, FILE *);
static void BsaveStorage(const Environment&, FILE *);
static void BsaveBinaryItem(const Environment&, FILE *);
#endif
static void BloadStorage(const Environment&);
static void BloadBinaryItem(const Environment&);
static void UpdateDeffactsModule(const Environment&, void *, unsigned long);
static void UpdateDeffacts(const Environment&, void *, unsigned long);
static void ClearBload(const Environment&);
static void DeallocateDeffactsBloadData(const Environment&);

/********************************************/
/* DeffactsBinarySetup: Installs the binary */
/*   save/load feature for deffacts.        */
/********************************************/
void DeffactsBinarySetup(
        const Environment&theEnv) {
    AllocateEnvironmentData(theEnv, DFFCTBIN_DATA, sizeof(deffactsBinaryData), DeallocateDeffactsBloadData);
#if BLOAD_AND_BSAVE
    AddBinaryItem(theEnv, "deffacts", 0, BsaveFind, BsaveExpressions,
                  BsaveStorage, BsaveBinaryItem,
                  BloadStorage, BloadBinaryItem,
                  ClearBload);
#endif

}

/********************************************************/
/* DeallocateDeffactsBloadData: Deallocates environment */
/*    data for the deffacts bsave functionality.        */
/********************************************************/
static void DeallocateDeffactsBloadData(
        const Environment&theEnv) {
    size_t space;

    space = DeffactsBinaryData(theEnv)->NumberOfDeffacts * sizeof(Deffacts);
    if (space != 0) genfree(theEnv, DeffactsBinaryData(theEnv)->DeffactsArray, space);

    space = DeffactsBinaryData(theEnv)->NumberOfDeffactsModules * sizeof(deffactsModule);
    if (space != 0) genfree(theEnv, DeffactsBinaryData(theEnv)->ModuleArray, space);
}

#if BLOAD_AND_BSAVE

/*********************************************************/
/* BsaveFind: Counts the number of data structures which */
/*   must be saved in the binary image for the deffacts  */
/*   in the current environment.                         */
/*********************************************************/
static void BsaveFind(
        const Environment&theEnv) {
    Deffacts *theDeffacts;
    Defmodule *theModule;

    /*=======================================================*/
    /* If a binary image is already loaded, then temporarily */
    /* save the count values since these will be overwritten */
    /* in the process of saving the binary image.            */
    /*=======================================================*/

    SaveBloadCount(theEnv, DeffactsBinaryData(theEnv)->NumberOfDeffactsModules);
    SaveBloadCount(theEnv, DeffactsBinaryData(theEnv)->NumberOfDeffacts);

    /*========================================*/
    /* Set the count of deffacts and deffacts */
    /* module data structures to zero.        */
    /*========================================*/

    DeffactsBinaryData(theEnv)->NumberOfDeffacts = 0;
    DeffactsBinaryData(theEnv)->NumberOfDeffactsModules = 0;

    /*===========================*/
    /* Loop through each module. */
    /*===========================*/

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        /*===============================================*/
        /* Set the current module to the module being    */
        /* examined and increment the number of deffacts */
        /* modules encountered.                          */
        /*===============================================*/

        SetCurrentModule(theEnv, theModule);
        DeffactsBinaryData(theEnv)->NumberOfDeffactsModules++;

        /*===================================================*/
        /* Loop through each deffacts in the current module. */
        /*===================================================*/

        for (theDeffacts = GetNextDeffacts(theEnv, nullptr);
             theDeffacts != nullptr;
             theDeffacts = GetNextDeffacts(theEnv, theDeffacts)) {
            /*======================================================*/
            /* Initialize the construct header for the binary save. */
            /*======================================================*/

            MarkConstructHeaderNeededItems(&theDeffacts->header, DeffactsBinaryData(theEnv)->NumberOfDeffacts++);

            /*============================================================*/
            /* Count the number of expressions contained in the deffacts' */
            /* assertion list and mark any atomic values contained there  */
            /* as in use.                                                 */
            /*============================================================*/

            ExpressionData(theEnv)->ExpressionCount += ExpressionSize(theDeffacts->assertList);
            MarkNeededItems(theEnv, theDeffacts->assertList);
        }
    }
}

/************************************************/
/* BsaveExpressions: Saves the expressions used */
/*   by deffacts to the binary save file.       */
/************************************************/
static void BsaveExpressions(
        const Environment&theEnv,
        FILE *fp) {
    Deffacts *theDeffacts;
    Defmodule *theModule;

    /*===========================*/
    /* Loop through each module. */
    /*===========================*/

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        /*======================================================*/
        /* Set the current module to the module being examined. */
        /*======================================================*/

        SetCurrentModule(theEnv, theModule);

        /*==================================================*/
        /* Loop through each deffacts in the current module */
        /* and save the assertion list expression.          */
        /*==================================================*/

        for (theDeffacts = GetNextDeffacts(theEnv, nullptr);
             theDeffacts != nullptr;
             theDeffacts = GetNextDeffacts(theEnv, theDeffacts)) { BsaveExpression(theEnv, theDeffacts->assertList, fp); }
    }
}

/******************************************************/
/* BsaveStorage: Writes out the storage requirements  */
/*    for all deffacts structures to the binary file. */
/******************************************************/
static void BsaveStorage(
        const Environment&theEnv,
        FILE *fp) {
    size_t space;

    /*=================================================================*/
    /* Only two data structures are saved as part of a deffacts binary */
    /* image: the deffacts data structure and the deffactsModule data  */
    /* structure. The assertion list expressions are not save with the */
    /* deffacts portion of the binary image.                           */
    /*=================================================================*/

    space = sizeof(long) * 2;
    GenWrite(&space, sizeof(size_t), fp);
    GenWrite(&DeffactsBinaryData(theEnv)->NumberOfDeffacts, sizeof(long), fp);
    GenWrite(&DeffactsBinaryData(theEnv)->NumberOfDeffactsModules, sizeof(long), fp);
}

/********************************************/
/* BsaveBinaryItem: Writes out all deffacts */
/*   structures to the binary file.         */
/********************************************/
static void BsaveBinaryItem(
        const Environment&theEnv,
        FILE *fp) {
    size_t space;
    Deffacts *theDeffacts;
    struct bsaveDeffacts newDeffacts;
    Defmodule *theModule;
    struct bsaveDeffactsModule tempDeffactsModule;
    struct deffactsModule *theModuleItem;

    /*=========================================================*/
    /* Write out the amount of space taken up by the deffacts  */
    /* and deffactsModule data structures in the binary image. */
    /*=========================================================*/

    space = DeffactsBinaryData(theEnv)->NumberOfDeffacts * sizeof(bsaveDeffacts) +
            (DeffactsBinaryData(theEnv)->NumberOfDeffactsModules * sizeof(bsaveDeffactsModule));
    GenWrite(&space, sizeof(size_t), fp);

    /*================================================*/
    /* Write out each deffacts module data structure. */
    /*================================================*/

    DeffactsBinaryData(theEnv)->NumberOfDeffacts = 0;
    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        SetCurrentModule(theEnv, theModule);

        theModuleItem = (deffactsModule *) GetModuleItem(theEnv, nullptr, DeffactsData(theEnv)->DeffactsModuleIndex);
        AssignBsaveDefmdlItemHdrVals(&tempDeffactsModule.header, &theModuleItem->header);
        GenWrite(&tempDeffactsModule, sizeof(bsaveDeffactsModule), fp);
    }

    /*==========================*/
    /* Write out each deffacts. */
    /*==========================*/

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        SetCurrentModule(theEnv, theModule);

        for (theDeffacts = GetNextDeffacts(theEnv, nullptr);
             theDeffacts != nullptr;
             theDeffacts = GetNextDeffacts(theEnv, theDeffacts)) {
            AssignBsaveConstructHeaderVals(&newDeffacts.header, &theDeffacts->header);
            if (theDeffacts->assertList != nullptr) {
                newDeffacts.assertList = ExpressionData(theEnv)->ExpressionCount;
                ExpressionData(theEnv)->ExpressionCount += ExpressionSize(theDeffacts->assertList);
            } else { newDeffacts.assertList = ULONG_MAX; }

            GenWrite(&newDeffacts, sizeof(bsaveDeffacts), fp);
        }
    }

    /*=============================================================*/
    /* If a binary image was already loaded when the bsave command */
    /* was issued, then restore the counts indicating the number   */
    /* of deffacts and deffacts modules in the binary image (these */
    /* were overwritten by the binary save).                       */
    /*=============================================================*/

    RestoreBloadCount(theEnv, &DeffactsBinaryData(theEnv)->NumberOfDeffactsModules);
    RestoreBloadCount(theEnv, &DeffactsBinaryData(theEnv)->NumberOfDeffacts);
}

#endif /* BLOAD_AND_BSAVE */

/****************************************************/
/* BloadStorage: Allocates storage requirements for */
/*   the deffacts used by this binary image.        */
/****************************************************/
static void BloadStorage(
        const Environment&theEnv) {
    size_t space;

    /*=====================================================*/
    /* Determine the number of deffacts and deffactsModule */
    /* data structures to be read.                         */
    /*=====================================================*/

    GenReadBinary(theEnv, &space, sizeof(size_t));
    GenReadBinary(theEnv, &DeffactsBinaryData(theEnv)->NumberOfDeffacts, sizeof(long));
    GenReadBinary(theEnv, &DeffactsBinaryData(theEnv)->NumberOfDeffactsModules, sizeof(long));

    /*===================================*/
    /* Allocate the space needed for the */
    /* deffactsModule data structures.   */
    /*===================================*/

    if (DeffactsBinaryData(theEnv)->NumberOfDeffactsModules == 0) {
        DeffactsBinaryData(theEnv)->DeffactsArray = nullptr;
        DeffactsBinaryData(theEnv)->ModuleArray = nullptr;
        return;
    }

    space = DeffactsBinaryData(theEnv)->NumberOfDeffactsModules * sizeof(deffactsModule);
    DeffactsBinaryData(theEnv)->ModuleArray = (deffactsModule *) genalloc(theEnv, space);

    /*===================================*/
    /* Allocate the space needed for the */
    /* deffacts data structures.         */
    /*===================================*/

    if (DeffactsBinaryData(theEnv)->NumberOfDeffacts == 0) {
        DeffactsBinaryData(theEnv)->DeffactsArray = nullptr;
        return;
    }

    space = (DeffactsBinaryData(theEnv)->NumberOfDeffacts * sizeof(Deffacts));
    DeffactsBinaryData(theEnv)->DeffactsArray = (Deffacts *) genalloc(theEnv, space);
}

/*****************************************************/
/* BloadBinaryItem: Loads and refreshes the deffacts */
/*   constructs used by this binary image.           */
/*****************************************************/
static void BloadBinaryItem(
        const Environment&theEnv) {
    size_t space;

    /*======================================================*/
    /* Read in the amount of space used by the binary image */
    /* (this is used to skip the construct in the event it  */
    /* is not available in the version being run).          */
    /*======================================================*/

    GenReadBinary(theEnv, &space, sizeof(size_t));

    /*============================================*/
    /* Read in the deffactsModule data structures */
    /* and refresh the pointers.                  */
    /*============================================*/

    BloadandRefresh(theEnv, DeffactsBinaryData(theEnv)->NumberOfDeffactsModules,
                    sizeof(bsaveDeffactsModule), UpdateDeffactsModule);

    /*======================================*/
    /* Read in the deffacts data structures */
    /* and refresh the pointers.            */
    /*======================================*/

    BloadandRefresh(theEnv, DeffactsBinaryData(theEnv)->NumberOfDeffacts,
                    sizeof(bsaveDeffacts), UpdateDeffacts);
}

/***************************************************/
/* UpdateDeffactsModule: Bload refresh routine for */
/*   deffacts module data structures.              */
/***************************************************/
static void UpdateDeffactsModule(
        const Environment&theEnv,
        void *buf,
        unsigned long obji) {
    struct bsaveDeffactsModule *bdmPtr;

    bdmPtr = (bsaveDeffactsModule *) buf;
    UpdateDefmoduleItemHeader(theEnv, &bdmPtr->header, &DeffactsBinaryData(theEnv)->ModuleArray[obji].header,
                              sizeof(Deffacts), DeffactsBinaryData(theEnv)->DeffactsArray);
}

/*********************************************/
/* UpdateDeffacts: Bload refresh routine for */
/*   deffacts data structures.               */
/*********************************************/
static void UpdateDeffacts(
        const Environment&theEnv,
        void *buf,
        unsigned long obji) {
    struct bsaveDeffacts *bdp;

    bdp = (bsaveDeffacts *) buf;
    UpdateConstructHeader(theEnv, &bdp->header, &DeffactsBinaryData(theEnv)->DeffactsArray[obji].header,
                          DEFFACTS, sizeof(deffactsModule), DeffactsBinaryData(theEnv)->ModuleArray,
                          sizeof(Deffacts), DeffactsBinaryData(theEnv)->DeffactsArray);
    DeffactsBinaryData(theEnv)->DeffactsArray[obji].assertList = ExpressionPointer(bdp->assertList);
}

/**************************************/
/* ClearBload: Deffacts clear routine */
/*   when a binary load is in effect. */
/**************************************/
static void ClearBload(
        const Environment&theEnv) {
    unsigned long i;
    size_t space;

    /*=============================================*/
    /* Decrement in use counters for atomic values */
    /* contained in the construct headers.         */
    /*=============================================*/

    for (i = 0; i < DeffactsBinaryData(theEnv)->NumberOfDeffacts; i++) {
        UnmarkConstructHeader(theEnv, &DeffactsBinaryData(
                theEnv)->DeffactsArray[i].header);
    }

    /*=============================================================*/
    /* Deallocate the space used for the deffacts data structures. */
    /*=============================================================*/

    space = DeffactsBinaryData(theEnv)->NumberOfDeffacts * sizeof(Deffacts);
    if (space != 0) genfree(theEnv, DeffactsBinaryData(theEnv)->DeffactsArray, space);
    DeffactsBinaryData(theEnv)->NumberOfDeffacts = 0;

    /*====================================================================*/
    /* Deallocate the space used for the deffacts module data structures. */
    /*====================================================================*/

    space = DeffactsBinaryData(theEnv)->NumberOfDeffactsModules * sizeof(deffactsModule);
    if (space != 0) genfree(theEnv, DeffactsBinaryData(theEnv)->ModuleArray, space);
    DeffactsBinaryData(theEnv)->NumberOfDeffactsModules = 0;
}

/******************************************************/
/* BloadDeffactsModuleReference: Returns the deffacts */
/*   module pointer for use with the bload function.  */
/******************************************************/
void *BloadDeffactsModuleReference(
        const Environment&theEnv,
        unsigned long theIndex) {
    return (void *) &DeffactsBinaryData(theEnv)->ModuleArray[theIndex];
}

#endif /* DEFFACTS_CONSTRUCT && (BLOAD_AND_BSAVE) */


