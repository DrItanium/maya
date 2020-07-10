/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  12/02/19             */
/*                                                     */
/*            CONSTRAINT BLOAD/BSAVE MODULE            */
/*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for      */
/*    constraint records.                                    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added allowed-classes slot facet.              */
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

#include "Constants.h"
#include "Environment.h"
#include "MemoryAllocation.h"
#include "PrintUtility.h"
#include "Router.h"
#include "BinaryLoad.h"

#if BLOAD_AND_BSAVE
#include "BinarySave.h"
#endif

#include "ConstraintBinaryLoadSave.h"

/*******************/
/* DATA STRUCTURES */
/*******************/

struct bsaveConstraintRecord {
    unsigned int anyAllowed: 1;
    unsigned int symbolsAllowed: 1;
    unsigned int stringsAllowed: 1;
    unsigned int floatsAllowed: 1;
    unsigned int integersAllowed: 1;
    unsigned int instanceNamesAllowed: 1;
    unsigned int instanceAddressesAllowed: 1;
    unsigned int externalAddressesAllowed: 1;
    unsigned int factAddressesAllowed: 1;
    unsigned int anyRestriction: 1;
    unsigned int symbolRestriction: 1;
    unsigned int stringRestriction: 1;
    unsigned int numberRestriction: 1;
    unsigned int floatRestriction: 1;
    unsigned int integerRestriction: 1;
    unsigned int classRestriction: 1;
    unsigned int instanceNameRestriction: 1;
    unsigned int multifieldsAllowed: 1;
    unsigned int singlefieldsAllowed: 1;
    unsigned long classList;
    unsigned long restrictionList;
    unsigned long minValue;
    unsigned long maxValue;
    unsigned long minFields;
    unsigned long maxFields;
};

typedef struct bsaveConstraintRecord BSAVE_CONSTRAINT_RECORD;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
static void CopyToBsaveConstraintRecord(Environment *, CONSTRAINT_RECORD *, BSAVE_CONSTRAINT_RECORD *);
#endif
static void CopyFromBsaveConstraintRecord(Environment *, void *, unsigned long);

#if BLOAD_AND_BSAVE

/**************************************************/
/* WriteNeededConstraints: Writes the constraints */
/*   in the constraint table to the binary image  */
/*   currently being saved.                       */
/**************************************************/
void WriteNeededConstraints(
        Environment *theEnv,
        FILE *fp) {
    int i;
    unsigned long theIndex = 0;
    unsigned long numberOfUsedConstraints = 0;
    CONSTRAINT_RECORD *tmpPtr;
    BSAVE_CONSTRAINT_RECORD bsaveConstraints;

    /*================================*/
    /* Get the number of constraints. */
    /*================================*/

    for (i = 0; i < SIZE_CONSTRAINT_HASH; i++) {
        for (tmpPtr = ConstraintData(theEnv)->ConstraintHashtable[i];
             tmpPtr != nullptr;
             tmpPtr = tmpPtr->getNext()) {
            tmpPtr->setBSaveID(theIndex++);
            numberOfUsedConstraints++;
        }
    }

    /*=============================================*/
    /* If dynamic constraint checking is disabled, */
    /* then no constraints are saved.              */
    /*=============================================*/

    if ((!GetDynamicConstraintChecking(theEnv)) && (numberOfUsedConstraints != 0)) {
        numberOfUsedConstraints = 0;
        PrintWarningID(theEnv, "CSTRNBIN", 1, false);
        WriteString(theEnv, STDWRN, "Constraints are not saved with a binary image\n");
        WriteString(theEnv, STDWRN, "  when dynamic constraint checking is disabled.\n");
    }

    /*============================================*/
    /* Write out the number of constraints in the */
    /* constraint table followed by each of the   */
    /* constraints in the constraint table.       */
    /*============================================*/

    GenWrite(&numberOfUsedConstraints, sizeof(unsigned long), fp);
    if (numberOfUsedConstraints == 0) return;

    for (i = 0; i < SIZE_CONSTRAINT_HASH; i++) {
        for (tmpPtr = ConstraintData(theEnv)->ConstraintHashtable[i];
             tmpPtr != nullptr;
             tmpPtr = tmpPtr->getNext()) {
            CopyToBsaveConstraintRecord(theEnv, tmpPtr, &bsaveConstraints);
            GenWrite(&bsaveConstraints, sizeof(BSAVE_CONSTRAINT_RECORD), fp);
        }
    }
}

/****************************************************/
/* CopyToBsaveConstraintRecord: Copies a constraint */
/*   record to the data structure used for storing  */
/*   constraints in a binary image.                 */
/****************************************************/
static void CopyToBsaveConstraintRecord(
        Environment *theEnv,
        CONSTRAINT_RECORD *constraints,
        BSAVE_CONSTRAINT_RECORD *bsaveConstraints) {
    bsaveConstraints->anyAllowed = constraints->getAnyAllowed();
    bsaveConstraints->symbolsAllowed = constraints->getSymbolsAllowed();
    bsaveConstraints->stringsAllowed = constraints->getStringsAllowed();
    bsaveConstraints->floatsAllowed = constraints->getFloatsAllowed();
    bsaveConstraints->integersAllowed = constraints->getIntegersAllowed();
    bsaveConstraints->instanceNamesAllowed = constraints->getInstanceNamesAllowed();
    bsaveConstraints->instanceAddressesAllowed = constraints->getInstanceAddressesAllowed();
    bsaveConstraints->externalAddressesAllowed = constraints->getExternalAddressesAllowed();
    bsaveConstraints->multifieldsAllowed = constraints->getMultifieldsAllowed();
    bsaveConstraints->singlefieldsAllowed = constraints->getSinglefieldsAllowed();
    bsaveConstraints->factAddressesAllowed = constraints->getFactAddressesAllowed();
    bsaveConstraints->anyRestriction = constraints->getAnyRestriction();
    bsaveConstraints->symbolRestriction = constraints->getSymbolRestriction();
    bsaveConstraints->stringRestriction = constraints->getStringRestriction();
    bsaveConstraints->floatRestriction = constraints->getFloatRestriction();
    bsaveConstraints->integerRestriction = constraints->getIntegerRestriction();
    bsaveConstraints->classRestriction = constraints->getClassRestriction();
    bsaveConstraints->instanceNameRestriction = constraints->getInstanceNameRestriction();

    bsaveConstraints->restrictionList = HashedExpressionIndex(theEnv, constraints->getRestrictionList());
    bsaveConstraints->classList = HashedExpressionIndex(theEnv, constraints->getClassList());
    bsaveConstraints->minValue = HashedExpressionIndex(theEnv, constraints->getMinValue());
    bsaveConstraints->maxValue = HashedExpressionIndex(theEnv, constraints->getMaxValue());
    bsaveConstraints->minFields = HashedExpressionIndex(theEnv, constraints->getMinFields());
    bsaveConstraints->maxFields = HashedExpressionIndex(theEnv, constraints->getMaxFields());
}

#endif /* BLOAD_AND_BSAVE */

/********************************************************/
/* ReadNeededConstraints: Reads in the constraints used */
/*   by the binary image currently being loaded.        */
/********************************************************/
void ReadNeededConstraints(
        Environment *theEnv) {
    GenReadBinary(theEnv, &ConstraintData(theEnv)->NumberOfConstraints, sizeof(unsigned long));
    if (ConstraintData(theEnv)->NumberOfConstraints == 0) return;

    ConstraintData(theEnv)->ConstraintArray = (CONSTRAINT_RECORD *)
            genalloc(theEnv, (sizeof(CONSTRAINT_RECORD) * ConstraintData(theEnv)->NumberOfConstraints));

    BloadandRefresh(theEnv, ConstraintData(theEnv)->NumberOfConstraints, sizeof(BSAVE_CONSTRAINT_RECORD),
                    CopyFromBsaveConstraintRecord);
}

/*****************************************************/
/* CopyFromBsaveConstraintRecord: Copies values to a */
/*   constraint record from the data structure used  */
/*   for storing constraints in a binary image.      */
/*****************************************************/
static void CopyFromBsaveConstraintRecord(
        Environment *theEnv,
        void *buf,
        unsigned long theIndex) {
    BSAVE_CONSTRAINT_RECORD *bsaveConstraints;
    CONSTRAINT_RECORD *constraints;

    bsaveConstraints = (BSAVE_CONSTRAINT_RECORD *) buf;
    constraints = (CONSTRAINT_RECORD *) &ConstraintData(theEnv)->ConstraintArray[theIndex];

    constraints->setAnyAllowed(bsaveConstraints->anyAllowed);
    constraints->setSymbolsAllowed ( bsaveConstraints->symbolsAllowed);
    constraints->setStringsAllowed ( bsaveConstraints->stringsAllowed);
    constraints->setFloatsAllowed ( bsaveConstraints->floatsAllowed);
    constraints->setIntegersAllowed ( bsaveConstraints->integersAllowed);
    constraints->setInstanceNamesAllowed ( bsaveConstraints->instanceNamesAllowed);
    constraints->setInstanceAddressesAllowed ( bsaveConstraints->instanceAddressesAllowed);
    constraints->setExternalAddressesAllowed ( bsaveConstraints->externalAddressesAllowed);
    constraints->setVoidAllowed ( false);
    constraints->setMultifieldsAllowed ( bsaveConstraints->multifieldsAllowed);
    constraints->setSinglefieldsAllowed ( bsaveConstraints->singlefieldsAllowed);
    constraints->setFactAddressesAllowed ( bsaveConstraints->factAddressesAllowed);
    constraints->setAnyRestriction ( bsaveConstraints->anyRestriction);
    constraints->setSymbolRestriction ( bsaveConstraints->symbolRestriction);
    constraints->setStringRestriction ( bsaveConstraints->stringRestriction);
    constraints->setFloatRestriction ( bsaveConstraints->floatRestriction);
    constraints->setIntegerRestriction ( bsaveConstraints->integerRestriction);
    constraints->setClassRestriction ( bsaveConstraints->classRestriction);
    constraints->setInstanceNameRestriction ( bsaveConstraints->instanceNameRestriction);

    constraints->setRestrictionList ( HashedExpressionPointer(bsaveConstraints->restrictionList));
    constraints->setClassList ( HashedExpressionPointer(bsaveConstraints->classList));
    constraints->setMinValue ( HashedExpressionPointer(bsaveConstraints->minValue));
    constraints->setMaxValue ( HashedExpressionPointer(bsaveConstraints->maxValue));
    constraints->setMinFields ( HashedExpressionPointer(bsaveConstraints->minFields));
    constraints->setMaxFields ( HashedExpressionPointer(bsaveConstraints->maxFields));
    constraints->setMultifield(nullptr);
}

/********************************************************/
/* ClearBloadedConstraints: Releases memory associated  */
/*   with constraints loaded from binary image          */
/********************************************************/
void ClearBloadedConstraints(
        Environment *theEnv) {
    if (ConstraintData(theEnv)->NumberOfConstraints != 0) {
        genfree(theEnv, ConstraintData(theEnv)->ConstraintArray,
                (sizeof(CONSTRAINT_RECORD) * ConstraintData(theEnv)->NumberOfConstraints));
        ConstraintData(theEnv)->NumberOfConstraints = 0;
    }
}

#endif /* (BLOAD_AND_BSAVE)  */





