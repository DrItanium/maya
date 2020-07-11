/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  11/01/16             */
/*                                                     */
/*                FACT BSAVE/BLOAD MODULE              */
/*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    fact pattern network.                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
/*                                                           */
/*            Changed integer type/precision.                */
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

#if DEFTEMPLATE_CONSTRUCT && (BLOAD_AND_BSAVE)

#include <cstdio>

#include "Fact.h"
#include "BinaryLoad.h"
#include "BinarySave.h"
#include "Environment.h"
#include "MemoryAllocation.h"
#include "Defmodule.h"
#include "Pattern.h"
#include "ReteUtility.h"
#include "RuleBinarySaveLoad.h"
#include "Deftemplate.h"


/********************************************/
/* INTERNAL DATA STRUCTURES AND DEFINITIONS */
/********************************************/

struct bsaveFactPatternNode {
    struct bsavePatternNodeHeader header;
    unsigned short whichSlot;
    unsigned short whichField;
    unsigned short leaveFields;
    unsigned long networkTest;
    unsigned long nextLevel;
    unsigned long lastLevel;
    unsigned long leftNode;
    unsigned long rightNode;
};

constexpr auto BSAVE_FIND         = 0;
constexpr auto BSAVE_PATTERNS     = 1;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
static void BsaveDriver(const Environment&, int, FILE *, FactPatternNode *);
static void BsaveFind(const Environment&);
static void BsaveStorage(const Environment&, FILE *);
static void BsaveFactPatterns(const Environment&, FILE *);
static void BsavePatternNode(const Environment&, FactPatternNode *, FILE *);
#endif
static void BloadStorage(const Environment&);
static void BloadBinaryItem(const Environment&);
static void UpdateFactPatterns(const Environment&, void *, unsigned long);
static void ClearBload(const Environment&);
static void DeallocateFactBloadData(const Environment&);

/*****************************************************/
/* FactBinarySetup: Initializes the binary load/save */
/*   feature for the fact pattern network.           */
/*****************************************************/
void FactBinarySetup(
        const Environment&theEnv) {
    theEnv->allocateEnvironmentModule<factBinaryData>();
    //AllocateEnvironmentData(theEnv, FACTBIN_DATA, sizeof(factBinaryData), DeallocateFactBloadData);

#if BLOAD_AND_BSAVE
    AddBinaryItem(theEnv, "facts", 0, BsaveFind, nullptr,
                  BsaveStorage, BsaveFactPatterns,
                  BloadStorage, BloadBinaryItem,
                  ClearBload);
#endif
}

/****************************************************/
/* DeallocateFactBloadData: Deallocates environment */
/*    data for the fact bsave functionality.        */
/****************************************************/
static void DeallocateFactBloadData(
        const Environment&theEnv) {
    size_t space;
    unsigned long i;

    for (i = 0; i < FactBinaryData(theEnv)->NumberOfPatterns; i++) {
        DestroyAlphaMemory(theEnv, &FactBinaryData(theEnv)->FactPatternArray[i].header, false);
    }

    space = FactBinaryData(theEnv)->NumberOfPatterns * sizeof(FactPatternNode);
    if (space != 0) genfree(theEnv, FactBinaryData(theEnv)->FactPatternArray, space);
}

#if BLOAD_AND_BSAVE

/*********************************************************/
/* BsaveFind: Counts the number of data structures which */
/*   must be saved in the binary image for the fact      */
/*   pattern network in the current environment.         */
/*********************************************************/
static void BsaveFind(
        const Environment&theEnv) {
    Deftemplate *theDeftemplate;
    Defmodule *theModule;

    /*=======================================================*/
    /* If a binary image is already loaded, then temporarily */
    /* save the count values since these will be overwritten */
    /* in the process of saving the binary image.            */
    /*=======================================================*/

    SaveBloadCount(theEnv, FactBinaryData(theEnv)->NumberOfPatterns);

    /*=======================================*/
    /* Set the count of fact pattern network */
    /* data structures to zero.              */
    /*=======================================*/

    FactBinaryData(theEnv)->NumberOfPatterns = 0L;

    /*===========================*/
    /* Loop through each module. */
    /*===========================*/

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        /*===============================*/
        /* Set the current module to the */
        /* module being examined.        */
        /*===============================*/

        SetCurrentModule(theEnv, theModule);

        /*=====================================================*/
        /* Loop through each deftemplate in the current module */
        /* and count the number of data structures which must  */
        /* be saved for its pattern network.                   */
        /*=====================================================*/

        for (theDeftemplate = GetNextDeftemplate(theEnv, nullptr);
             theDeftemplate != nullptr;
             theDeftemplate = GetNextDeftemplate(theEnv, theDeftemplate)) {
            BsaveDriver(theEnv, BSAVE_FIND, nullptr, theDeftemplate->patternNetwork);
        }
    }
}

/**********************************************************/
/* BsaveDriver: Binary save driver routine which handles  */
/*   both finding/marking the data structures to be saved */
/*   and saving the data structures to a file.            */
/**********************************************************/
static void BsaveDriver(
        const Environment&theEnv,
        int action,
        FILE *fp,
        FactPatternNode *thePattern) {
    while (thePattern != nullptr) {
        switch (action) {
            case BSAVE_FIND:
                thePattern->bsaveID = FactBinaryData(theEnv)->NumberOfPatterns++;
                break;

            case BSAVE_PATTERNS:
                BsavePatternNode(theEnv, thePattern, fp);
                break;

            default:
                break;
        }

        if (thePattern->nextLevel == nullptr) {
            while (thePattern->rightNode == nullptr) {
                thePattern = thePattern->lastLevel;
                if (thePattern == nullptr) return;
            }
            thePattern = thePattern->rightNode;
        } else { thePattern = thePattern->nextLevel; }
    }
}

/*********************************************************/
/* BsaveStorage: Writes out storage requirements for all */
/*   FactPatternNode data structures to the binary file  */
/*********************************************************/
static void BsaveStorage(
        const Environment&theEnv,
        FILE *fp) {
    size_t space;

    space = sizeof(long);
    GenWrite(&space, sizeof(size_t), fp);
    GenWrite(&FactBinaryData(theEnv)->NumberOfPatterns, sizeof(long), fp);
}

/*****************************************************/
/* BsaveFactPatterns: Writes out all FactPatternNode */
/*    data structures to the binary file.            */
/*****************************************************/
static void BsaveFactPatterns(
        const Environment&theEnv,
        FILE *fp) {
    size_t space;
    Deftemplate *theDeftemplate;
    Defmodule *theModule;

    /*========================================*/
    /* Write out the amount of space taken up */
    /* by the FactPatternNode data structures */
    /* in the binary image.                   */
    /*========================================*/

    space = FactBinaryData(theEnv)->NumberOfPatterns * sizeof(bsaveFactPatternNode);
    GenWrite(&space, sizeof(size_t), fp);

    /*===========================*/
    /* Loop through each module. */
    /*===========================*/

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        /*=====================================================*/
        /* Loop through each deftemplate in the current module */
        /* and save its fact pattern network to the file.      */
        /*=====================================================*/

        SetCurrentModule(theEnv, theModule);
        for (theDeftemplate = GetNextDeftemplate(theEnv, nullptr);
             theDeftemplate != nullptr;
             theDeftemplate = GetNextDeftemplate(theEnv, theDeftemplate)) {
            BsaveDriver(theEnv, BSAVE_PATTERNS, fp, theDeftemplate->patternNetwork);
        }
    }

    /*=============================================================*/
    /* If a binary image was already loaded when the bsave command */
    /* was issued, then restore the counts indicating the number   */
    /* of FactPatternNode data structures in the binary image      */
    /* (these were overwritten by the binary save).                */
    /*=============================================================*/

    RestoreBloadCount(theEnv, &FactBinaryData(theEnv)->NumberOfPatterns);
}

/******************************************************/
/* BsavePatternNode: Writes out a single fact pattern */
/*   node to the binary image save file.              */
/******************************************************/
static void BsavePatternNode(
        const Environment&theEnv,
        FactPatternNode *thePattern,
        FILE *fp) {
    struct bsaveFactPatternNode tempNode;

    AssignBsavePatternHeaderValues(theEnv, &tempNode.header, &thePattern->header);

    tempNode.whichField = thePattern->whichField;
    tempNode.leaveFields = thePattern->leaveFields;
    tempNode.whichSlot = thePattern->whichSlot;
    tempNode.networkTest = HashedExpressionIndex(theEnv, thePattern->networkTest);
    tempNode.nextLevel = BsaveFactPatternIndex(thePattern->nextLevel);
    tempNode.lastLevel = BsaveFactPatternIndex(thePattern->lastLevel);
    tempNode.leftNode = BsaveFactPatternIndex(thePattern->leftNode);
    tempNode.rightNode = BsaveFactPatternIndex(thePattern->rightNode);

    GenWrite(&tempNode, sizeof(bsaveFactPatternNode), fp);
}

#endif /* BLOAD_AND_BSAVE */

/*****************************************************/
/* BloadStorage: Allocates storage requirements for  */
/*   the FactPatternNodes used by this binary image. */
/*****************************************************/
static void BloadStorage(
        const Environment&theEnv) {
    size_t space;

    /*=========================================*/
    /* Determine the number of FactPatternNode */
    /* data structures to be read.             */
    /*=========================================*/

    GenReadBinary(theEnv, &space, sizeof(size_t));
    GenReadBinary(theEnv, &FactBinaryData(theEnv)->NumberOfPatterns, sizeof(long));

    /*===================================*/
    /* Allocate the space needed for the */
    /* FactPatternNode data structures.  */
    /*===================================*/

    if (FactBinaryData(theEnv)->NumberOfPatterns == 0) {
        FactBinaryData(theEnv)->FactPatternArray = nullptr;
        return;
    }

    space = FactBinaryData(theEnv)->NumberOfPatterns * sizeof(FactPatternNode);
    FactBinaryData(theEnv)->FactPatternArray = (FactPatternNode *) genalloc(theEnv, space);
}

/************************************************************/
/* BloadBinaryItem: Loads and refreshes the FactPatternNode */
/*   data structures used by this binary image.             */
/************************************************************/
static void BloadBinaryItem(
        const Environment&theEnv) {
    size_t space;
    unsigned long i;

    /*======================================================*/
    /* Read in the amount of space used by the binary image */
    /* (this is used to skip the construct in the event it  */
    /* is not available in the version being run).          */
    /*======================================================*/

    GenReadBinary(theEnv, &space, sizeof(size_t));

    /*=============================================*/
    /* Read in the FactPatternNode data structures */
    /* and refresh the pointers.                   */
    /*=============================================*/

    BloadandRefresh(theEnv, FactBinaryData(theEnv)->NumberOfPatterns, sizeof(bsaveFactPatternNode),
                    UpdateFactPatterns);

    for (i = 0; i < FactBinaryData(theEnv)->NumberOfPatterns; i++) {
        if ((FactBinaryData(theEnv)->FactPatternArray[i].lastLevel != nullptr) &&
            (FactBinaryData(theEnv)->FactPatternArray[i].lastLevel->header.selector)) {
            AddHashedPatternNode(theEnv, FactBinaryData(theEnv)->FactPatternArray[i].lastLevel,
                                 &FactBinaryData(theEnv)->FactPatternArray[i],
                                 FactBinaryData(theEnv)->FactPatternArray[i].networkTest->type,
                                 FactBinaryData(theEnv)->FactPatternArray[i].networkTest->value);
        }
    }
}

/*************************************************/
/* UpdateFactPatterns: Bload refresh routine for */
/*   the FactPatternNode structure.              */
/*************************************************/
static void UpdateFactPatterns(
        const Environment&theEnv,
        void *buf,
        unsigned long obji) {
    struct bsaveFactPatternNode *bp;

    bp = (bsaveFactPatternNode *) buf;

    UpdatePatternNodeHeader(theEnv, &FactBinaryData(theEnv)->FactPatternArray[obji].header, &bp->header);

    FactBinaryData(theEnv)->FactPatternArray[obji].bsaveID = 0L;
    FactBinaryData(theEnv)->FactPatternArray[obji].whichField = bp->whichField;
    FactBinaryData(theEnv)->FactPatternArray[obji].leaveFields = bp->leaveFields;
    FactBinaryData(theEnv)->FactPatternArray[obji].whichSlot = bp->whichSlot;

    FactBinaryData(theEnv)->FactPatternArray[obji].networkTest = HashedExpressionPointer(bp->networkTest);
    FactBinaryData(theEnv)->FactPatternArray[obji].rightNode = BloadFactPatternPointer(bp->rightNode);
    FactBinaryData(theEnv)->FactPatternArray[obji].nextLevel = BloadFactPatternPointer(bp->nextLevel);
    FactBinaryData(theEnv)->FactPatternArray[obji].lastLevel = BloadFactPatternPointer(bp->lastLevel);
    FactBinaryData(theEnv)->FactPatternArray[obji].leftNode = BloadFactPatternPointer(bp->leftNode);
}

/***************************************************/
/* ClearBload:  Fact pattern network clear routine */
/*   when a binary load is in effect.              */
/***************************************************/
static void ClearBload(
        const Environment&theEnv) {
    size_t space;
    unsigned long i;

    for (i = 0; i < FactBinaryData(theEnv)->NumberOfPatterns; i++) {
        if ((FactBinaryData(theEnv)->FactPatternArray[i].lastLevel != nullptr) &&
            (FactBinaryData(theEnv)->FactPatternArray[i].lastLevel->header.selector)) {
            RemoveHashedPatternNode(theEnv, FactBinaryData(theEnv)->FactPatternArray[i].lastLevel,
                                    &FactBinaryData(theEnv)->FactPatternArray[i],
                                    FactBinaryData(theEnv)->FactPatternArray[i].networkTest->type,
                                    FactBinaryData(theEnv)->FactPatternArray[i].networkTest->value);
        }
    }

    space = FactBinaryData(theEnv)->NumberOfPatterns * sizeof(FactPatternNode);
    if (space != 0) genfree(theEnv, FactBinaryData(theEnv)->FactPatternArray, space);
    FactBinaryData(theEnv)->NumberOfPatterns = 0;
}

#endif /* DEFTEMPLATE_CONSTRUCT && (BLOAD_AND_BSAVE) */


