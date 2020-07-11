/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  10/18/16             */
/*                                                     */
/*                 CONSTRAINT MODULE                   */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for creating and removing     */
/*   constraint records, adding them to the contraint hash   */
/*   table, and enabling and disabling static and dynamic    */
/*   constraint checking.                                    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian Dantes                                         */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Added allowed-classes slot facet.              */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Changed integer type/precision.                */
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
/*            Static constraint checking is always enabled.  */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Eval support for run time and bload only.      */
/*                                                           */
/*************************************************************/

#include <cstdio>
#include <cstdlib>

#include "Setup.h"

#include "ArgumentAccess.h"
#include "Constants.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "MemoryAllocation.h"
#include "Multifield.h"
#include "Router.h"
#include "Scanner.h"

#include "Constraint.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void InstallConstraintRecord(const Environment&, CONSTRAINT_RECORD *);
static bool ConstraintCompare(constraintRecord *, struct constraintRecord *);
static void ReturnConstraintRecord(const Environment&, CONSTRAINT_RECORD *);
static void DeinstallConstraintRecord(const Environment&, CONSTRAINT_RECORD *);
static void DeallocateConstraintData(const Environment&);

/*****************************************************/
/* InitializeConstraints: Initializes the constraint */
/*   hash table to nullptr and defines the static and   */
/*   dynamic constraint access functions.            */
/*****************************************************/
void InitializeConstraints(
        const Environment&theEnv) {
    int i;

    //AllocateEnvironmentData(theEnv, CONSTRAINT_DATA, sizeof(constraintData), DeallocateConstraintData);

    ConstraintData(theEnv)->ConstraintHashtable = (constraintRecord **)
            gm2(theEnv, sizeof(constraintRecord *) *
                        SIZE_CONSTRAINT_HASH);

    if (ConstraintData(theEnv)->ConstraintHashtable == nullptr) ExitRouter(theEnv, EXIT_FAILURE);

    for (i = 0; i < SIZE_CONSTRAINT_HASH; i++) ConstraintData(theEnv)->ConstraintHashtable[i] = nullptr;

    AddUDF(theEnv, "get-dynamic-constraint-checking", "b", 0, 0, nullptr, GDCCommand);
    AddUDF(theEnv, "set-dynamic-constraint-checking", "b", 1, 1, nullptr, SDCCommand);
}

/*****************************************************/
/* DeallocateConstraintData: Deallocates environment */
/*    data for constraints.                          */
/*****************************************************/
static void DeallocateConstraintData(
        const Environment&theEnv) {
    struct constraintRecord *tmpPtr, *nextPtr;
    int i;

    for (i = 0; i < SIZE_CONSTRAINT_HASH; i++) {
        tmpPtr = ConstraintData(theEnv)->ConstraintHashtable[i];
        while (tmpPtr != nullptr) {
            nextPtr = tmpPtr->getNext();
            ReturnConstraintRecord(theEnv, tmpPtr);
            tmpPtr = nextPtr;
        }
    }

    rm(theEnv, ConstraintData(theEnv)->ConstraintHashtable,
       sizeof(constraintRecord *) * SIZE_CONSTRAINT_HASH);

#if (BLOAD_AND_BSAVE)
    if (ConstraintData(theEnv)->NumberOfConstraints != 0) {
        genfree(theEnv, ConstraintData(theEnv)->ConstraintArray,
                (sizeof(CONSTRAINT_RECORD) * ConstraintData(theEnv)->NumberOfConstraints));
    }
#endif
}

/*************************************************************/
/* ReturnConstraintRecord: Frees the data structures used by */
/*   a constraint record. If the returnOnlyFields argument   */
/*   is false, then the constraint record is also freed.     */
/*************************************************************/
static void ReturnConstraintRecord(
        const Environment&theEnv,
        CONSTRAINT_RECORD *constraints) {
    if (constraints == nullptr) return;

    if (!constraints->installed) {
        ReturnExpression(theEnv, constraints->getClassList());
        ReturnExpression(theEnv, constraints->getRestrictionList());
        ReturnExpression(theEnv, constraints->getMaxValue());
        ReturnExpression(theEnv, constraints->getMinValue());
        ReturnExpression(theEnv, constraints->getMinFields());
        ReturnExpression(theEnv, constraints->getMaxFields());
    }

    ReturnConstraintRecord(theEnv, constraints->getMultifield());

    rtn_struct(theEnv, constraintRecord, constraints);
}

/***************************************************/
/* DeinstallConstraintRecord: Decrements the count */
/*   values of all occurrences of primitive data   */
/*   types found in a constraint record.           */
/***************************************************/
static void DeinstallConstraintRecord(
        const Environment&theEnv,
        CONSTRAINT_RECORD *constraints) {
    if (constraints->installed) {
        RemoveHashedExpression(theEnv, constraints->getClassList());
        RemoveHashedExpression(theEnv, constraints->getRestrictionList());
        RemoveHashedExpression(theEnv, constraints->getMaxValue());
        RemoveHashedExpression(theEnv, constraints->getMinValue());
        RemoveHashedExpression(theEnv, constraints->getMinFields());
        RemoveHashedExpression(theEnv, constraints->getMaxFields());
    } else {
        ExpressionDeinstall(theEnv, constraints->getClassList());
        ExpressionDeinstall(theEnv, constraints->getRestrictionList());
        ExpressionDeinstall(theEnv, constraints->getMaxValue());
        ExpressionDeinstall(theEnv, constraints->getMinValue());
        ExpressionDeinstall(theEnv, constraints->getMinFields());
        ExpressionDeinstall(theEnv, constraints->getMaxFields());
    }

    if (constraints->getMultifield() != nullptr) { DeinstallConstraintRecord(theEnv, constraints->getMultifield()); }
}

/******************************************/
/* RemoveConstraint: Removes a constraint */
/*   from the constraint hash table.      */
/******************************************/
void RemoveConstraint(
        const Environment&theEnv,
        struct constraintRecord *theConstraint) {
    struct constraintRecord *tmpPtr, *prevPtr = nullptr;

    if (theConstraint == nullptr) return;

    /*========================================*/
    /* If the bucket value is less than zero, */
    /* then the constraint wasn't stored in   */
    /* the hash table.                        */
    /*========================================*/

    if (!theConstraint->installed) {
        ReturnConstraintRecord(theEnv, theConstraint);
        return;
    }

    /*================================*/
    /* Find and remove the constraint */
    /* from the contraint hash table. */
    /*================================*/

    tmpPtr = ConstraintData(theEnv)->ConstraintHashtable[theConstraint->getBucket()];
    while (tmpPtr != nullptr) {
        if (tmpPtr == theConstraint) {
            theConstraint->decrementCount();
            if (theConstraint->getCount() == 0) {
                if (prevPtr == nullptr) { ConstraintData(theEnv)->ConstraintHashtable[theConstraint->getBucket()] = theConstraint->getNext(); }
                else { prevPtr->setNext(theConstraint->getNext()); }
                DeinstallConstraintRecord(theEnv, theConstraint);
                ReturnConstraintRecord(theEnv, theConstraint);
            }
            return;
        }

        prevPtr = tmpPtr;
        tmpPtr = tmpPtr->getNext();
    }

    return;
}


/***********************************/
/* HashConstraint: Returns a hash  */
/*   value for a given constraint. */
/***********************************/
unsigned long HashConstraint(
        struct constraintRecord *theConstraint) {
    unsigned short i = 0;
    unsigned long count = 0;
    unsigned long hashValue;
    Expression *tmpPtr;

    count +=
            (theConstraint->getAnyAllowed() * 17) +
            (theConstraint->getSymbolsAllowed()* 5) +
            (theConstraint->getStringsAllowed() * 23) +
            (theConstraint->getFloatsAllowed() * 19) +
            (theConstraint->getIntegersAllowed() * 29) +
            (theConstraint->getInstanceNamesAllowed() * 31) +
            (theConstraint->getInstanceAddressesAllowed() * 17);

    count +=
            (theConstraint->getExternalAddressesAllowed() * 29) +
            (theConstraint->getVoidAllowed() * 29) +
            (theConstraint->getMultifieldsAllowed() * 29) +
            (theConstraint->getFactAddressesAllowed() * 79) +
            (theConstraint->getAnyRestriction() * 59) +
            (theConstraint->getSymbolRestriction() * 61);

    count +=
            (theConstraint->getStringRestriction() * 3) +
            (theConstraint->getFloatRestriction() * 37) +
            (theConstraint->getIntegerRestriction() * 9) +
            (theConstraint->getClassRestriction() * 11) +
            (theConstraint->getInstanceNameRestriction() * 7);

    for (tmpPtr = theConstraint->getClassList(); tmpPtr != nullptr; tmpPtr = tmpPtr->nextArg) {
        count += GetAtomicHashValue(tmpPtr->type, tmpPtr->value, i++);
    }

    for (tmpPtr = theConstraint->getRestrictionList(); tmpPtr != nullptr; tmpPtr = tmpPtr->nextArg) {
        count += GetAtomicHashValue(tmpPtr->type, tmpPtr->value, i++);
    }

    for (tmpPtr = theConstraint->getMinValue(); tmpPtr != nullptr; tmpPtr = tmpPtr->nextArg) {
        count += GetAtomicHashValue(tmpPtr->type, tmpPtr->value, i++);
    }

    for (tmpPtr = theConstraint->getMaxValue(); tmpPtr != nullptr; tmpPtr = tmpPtr->nextArg) {
        count += GetAtomicHashValue(tmpPtr->type, tmpPtr->value, i++);
    }

    for (tmpPtr = theConstraint->getMinFields(); tmpPtr != nullptr; tmpPtr = tmpPtr->nextArg) {
        count += GetAtomicHashValue(tmpPtr->type, tmpPtr->value, i++);
    }

    for (tmpPtr = theConstraint->getMaxFields(); tmpPtr != nullptr; tmpPtr = tmpPtr->nextArg) {
        count += GetAtomicHashValue(tmpPtr->type, tmpPtr->value, i++);
    }

    if (theConstraint->getMultifield() != nullptr) { count += HashConstraint(theConstraint->getMultifield()); }

    hashValue = count % SIZE_CONSTRAINT_HASH;

    return hashValue;
}

/**********************************************/
/* ConstraintCompare: Compares two constraint */
/*   records and returns true if they are     */
/*   identical, otherwise false.              */
/**********************************************/
static bool ConstraintCompare(
        struct constraintRecord *constraint1,
        struct constraintRecord *constraint2) {
    Expression *tmpPtr1, *tmpPtr2;

    if ((constraint1->getAnyAllowed() != constraint2->getAnyAllowed()) ||
        (constraint1->getSymbolsAllowed()!= constraint2->getSymbolsAllowed()) ||
        (constraint1->getStringsAllowed() != constraint2->getStringsAllowed()) ||
        (constraint1->floatsAllowed != constraint2->floatsAllowed) ||
        (constraint1->integersAllowed != constraint2->integersAllowed) ||
        (constraint1->instanceNamesAllowed != constraint2->instanceNamesAllowed) ||
        (constraint1->instanceAddressesAllowed != constraint2->instanceAddressesAllowed) ||
        (constraint1->externalAddressesAllowed != constraint2->externalAddressesAllowed) ||
        (constraint1->voidAllowed != constraint2->voidAllowed) ||
        (constraint1->multifieldsAllowed != constraint2->multifieldsAllowed) ||
        (constraint1->singlefieldsAllowed != constraint2->singlefieldsAllowed) ||
        (constraint1->factAddressesAllowed != constraint2->factAddressesAllowed) ||
        (constraint1->anyRestriction != constraint2->anyRestriction) ||
        (constraint1->symbolRestriction != constraint2->symbolRestriction) ||
        (constraint1->stringRestriction != constraint2->stringRestriction) ||
        (constraint1->floatRestriction != constraint2->floatRestriction) ||
        (constraint1->integerRestriction != constraint2->integerRestriction) ||
        (constraint1->classRestriction != constraint2->classRestriction) ||
        (constraint1->instanceNameRestriction != constraint2->instanceNameRestriction)) { return false; }

    for (tmpPtr1 = constraint1->getClassList(), tmpPtr2 = constraint2->getClassList();
         (tmpPtr1 != nullptr) && (tmpPtr2 != nullptr);
         tmpPtr1 = tmpPtr1->nextArg, tmpPtr2 = tmpPtr2->nextArg) {
        if ((tmpPtr1->type != tmpPtr2->type) || (tmpPtr1->value != tmpPtr2->value)) { return false; }
    }
    if (tmpPtr1 != tmpPtr2) return false;

    for (tmpPtr1 = constraint1->getRestrictionList(), tmpPtr2 = constraint2->getRestrictionList();
         (tmpPtr1 != nullptr) && (tmpPtr2 != nullptr);
         tmpPtr1 = tmpPtr1->nextArg, tmpPtr2 = tmpPtr2->nextArg) {
        if ((tmpPtr1->type != tmpPtr2->type) || (tmpPtr1->value != tmpPtr2->value)) { return false; }
    }
    if (tmpPtr1 != tmpPtr2) return false;

    for (tmpPtr1 = constraint1->getMinValue(), tmpPtr2 = constraint2->getMinValue();
         (tmpPtr1 != nullptr) && (tmpPtr2 != nullptr);
         tmpPtr1 = tmpPtr1->nextArg, tmpPtr2 = tmpPtr2->nextArg) {
        if ((tmpPtr1->type != tmpPtr2->type) || (tmpPtr1->value != tmpPtr2->value)) { return false; }
    }
    if (tmpPtr1 != tmpPtr2) return false;

    for (tmpPtr1 = constraint1->getMaxValue(), tmpPtr2 = constraint2->getMaxValue();
         (tmpPtr1 != nullptr) && (tmpPtr2 != nullptr);
         tmpPtr1 = tmpPtr1->nextArg, tmpPtr2 = tmpPtr2->nextArg) {
        if ((tmpPtr1->type != tmpPtr2->type) || (tmpPtr1->value != tmpPtr2->value)) { return false; }
    }
    if (tmpPtr1 != tmpPtr2) return false;

    for (tmpPtr1 = constraint1->getMinFields(), tmpPtr2 = constraint2->getMinFields();
         (tmpPtr1 != nullptr) && (tmpPtr2 != nullptr);
         tmpPtr1 = tmpPtr1->nextArg, tmpPtr2 = tmpPtr2->nextArg) {
        if ((tmpPtr1->type != tmpPtr2->type) || (tmpPtr1->value != tmpPtr2->value)) { return false; }
    }
    if (tmpPtr1 != tmpPtr2) return false;

    for (tmpPtr1 = constraint1->getMaxFields(), tmpPtr2 = constraint2->getMaxFields();
         (tmpPtr1 != nullptr) && (tmpPtr2 != nullptr);
         tmpPtr1 = tmpPtr1->nextArg, tmpPtr2 = tmpPtr2->nextArg) {
        if ((tmpPtr1->type != tmpPtr2->type) || (tmpPtr1->value != tmpPtr2->value)) { return false; }
    }
    if (tmpPtr1 != tmpPtr2) return false;

    if (((constraint1->getMultifield() == nullptr) && (constraint2->getMultifield() != nullptr)) ||
        ((constraint1->getMultifield() != nullptr) && (constraint2->getMultifield() == nullptr))) { return false; }
    else if (constraint1->getMultifield() == constraint2->getMultifield()) { return true; }

    return (ConstraintCompare(constraint1->getMultifield(), constraint2->getMultifield()));
}

/************************************/
/* AddConstraint: Adds a constraint */
/*   to the constraint hash table.  */
/************************************/
struct constraintRecord *AddConstraint(
        const Environment&theEnv,
        struct constraintRecord *theConstraint) {
    struct constraintRecord *tmpPtr;
    unsigned long hashValue;

    if (theConstraint == nullptr) return nullptr;

    hashValue = HashConstraint(theConstraint);

    for (tmpPtr = ConstraintData(theEnv)->ConstraintHashtable[hashValue];
         tmpPtr != nullptr;
         tmpPtr = tmpPtr->getNext()) {
        if (ConstraintCompare(theConstraint, tmpPtr)) {
            tmpPtr->incrementCount();
            ReturnConstraintRecord(theEnv, theConstraint);
            return tmpPtr;
        }
    }

    InstallConstraintRecord(theEnv, theConstraint);
    theConstraint->setCount(1);
    theConstraint->setBucket((unsigned int) hashValue);
    theConstraint->installed = true;
    theConstraint->setNext(ConstraintData(theEnv)->ConstraintHashtable[hashValue]);
    ConstraintData(theEnv)->ConstraintHashtable[hashValue] = theConstraint;
    return theConstraint;
}

/*************************************************/
/* InstallConstraintRecord: Increments the count */
/*   values of all occurrences of primitive data */
/*   types found in a constraint record.         */
/*************************************************/
static void InstallConstraintRecord(
        const Environment&theEnv,
        CONSTRAINT_RECORD *constraints) {
    Expression *tempExpr;

    tempExpr = AddHashedExpression(theEnv, constraints->getClassList());
    ReturnExpression(theEnv, constraints->getClassList());
    constraints->setClassList(tempExpr);

    tempExpr = AddHashedExpression(theEnv, constraints->getRestrictionList());
    ReturnExpression(theEnv, constraints->getRestrictionList());
    constraints->setRestrictionList(tempExpr);

    tempExpr = AddHashedExpression(theEnv, constraints->getMaxValue());
    ReturnExpression(theEnv, constraints->getMaxValue());
    constraints->setMaxValue(tempExpr);

    tempExpr = AddHashedExpression(theEnv, constraints->getMinValue());
    ReturnExpression(theEnv, constraints->getMinValue());
    constraints->setMinValue( tempExpr);

    tempExpr = AddHashedExpression(theEnv, constraints->getMinFields());
    ReturnExpression(theEnv, constraints->getMinFields());
    constraints->setMinFields(tempExpr);

    tempExpr = AddHashedExpression(theEnv, constraints->getMaxFields());
    ReturnExpression(theEnv, constraints->getMaxFields());
    constraints->setMaxFields(tempExpr);

    if (constraints->getMultifield() != nullptr) { InstallConstraintRecord(theEnv, constraints->getMultifield()); }
}


/**********************************************/
/* SDCCommand: H/L access routine for the     */
/*   set-dynamic-constraint-checking command. */
/**********************************************/
void SDCCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UDFValue theArg;

    returnValue->lexemeValue = CreateBoolean(theEnv, GetDynamicConstraintChecking(theEnv));

    if (!UDFFirstArgument(context, ANY_TYPE_BITS, &theArg)) { return; }

    SetDynamicConstraintChecking(theEnv, theArg.value != FalseSymbol(theEnv));
}

/**********************************************/
/* GDCCommand: H/L access routine for the     */
/*   get-dynamic-constraint-checking command. */
/**********************************************/
void GDCCommand(
        const Environment&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    returnValue->lexemeValue = CreateBoolean(theEnv, GetDynamicConstraintChecking(theEnv));
}

/******************************************************/
/* SetDynamicConstraintChecking: C access routine     */
/*   for the set-dynamic-constraint-checking command. */
/******************************************************/
bool SetDynamicConstraintChecking(
        const Environment&theEnv,
        bool value) {
    bool ov;
    ov = ConstraintData(theEnv)->DynamicConstraintChecking;
    ConstraintData(theEnv)->DynamicConstraintChecking = value;
    return (ov);
}

/******************************************************/
/* GetDynamicConstraintChecking: C access routine     */
/*   for the get-dynamic-constraint-checking command. */
/******************************************************/
bool GetDynamicConstraintChecking(
        const Environment&theEnv) {
    return (ConstraintData(theEnv)->DynamicConstraintChecking);
}

