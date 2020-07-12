/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  10/18/16             */
/*                                                     */
/*             CONSTRAINT UTILITY MODULE               */
/*******************************************************/

/*************************************************************/
/* Purpose: Utility routines for manipulating, initializing, */
/*   creating, copying, and comparing constraint records.    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian Dantes                                         */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added allowed-classes slot facet.              */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
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

#include "ConstraintUtilities.h"

/************************************************/
/* GetConstraintRecord: Creates and initializes */
/*   the values of a constraint record.         */
/************************************************/
struct constraintRecord *GetConstraintRecord(
        const Environment&theEnv) {
#if STUBBING_INACTIVE
    unsigned i;

    auto constraints = get_struct(theEnv, constraintRecord);

    for (i = 0; i < sizeof(CONSTRAINT_RECORD); i++) { ((char *) constraints)[i] = '\0'; }

    SetAnyAllowedFlags(constraints, true);

    constraints->multifieldsAllowed = false;
    constraints->singlefieldsAllowed = true;

    constraints->anyRestriction = false;
    constraints->symbolRestriction = false;
    constraints->stringRestriction = false;
    constraints->floatRestriction = false;
    constraints->integerRestriction = false;
    constraints->classRestriction = false;
    constraints->instanceNameRestriction = false;
    constraints->setClassList ( nullptr);
    constraints->setRestrictionList ( nullptr);
    constraints->setMinValue ( GenConstant(theEnv, SYMBOL_TYPE, SymbolData(theEnv)->NegativeInfinity));
    constraints->setMaxValue ( GenConstant(theEnv, SYMBOL_TYPE, SymbolData(theEnv)->PositiveInfinity));
    constraints->setMinFields ( GenConstant(theEnv, INTEGER_TYPE, SymbolData(theEnv)->Zero));
    constraints->setMaxFields ( GenConstant(theEnv, SYMBOL_TYPE, SymbolData(theEnv)->PositiveInfinity));
    constraints->installed = false;
    constraints->setBucket(0);
    constraints->setCount(0);
    constraints->setMultifield(nullptr);
    constraints->setNext(nullptr);

    return constraints;
#endif
    return nullptr;
}

/********************************************************/
/* SetAnyAllowedFlags: Sets the allowed type flags of a */
/*   constraint record to allow all types. If passed an */
/*   argument of true, just the "any allowed" flag is   */
/*   set to true. If passed an argument of false, then  */
/*   all of the individual type flags are set to true.  */
/********************************************************/
void SetAnyAllowedFlags(
        CONSTRAINT_RECORD *theConstraint,
        bool justOne) {
    bool flag1, flag2;

    if (justOne) {
        flag1 = true;
        flag2 = false;
    } else {
        flag1 = false;
        flag2 = true;
    }

    theConstraint->setAnyAllowed(flag1);
    theConstraint->setSymbolsAllowed( flag2);
    theConstraint->setStringsAllowed(flag2);
    theConstraint->setFloatsAllowed ( flag2);
    theConstraint->setIntegersAllowed ( flag2);
    theConstraint->setInstanceNamesAllowed ( flag2);
    theConstraint->setInstanceAddressesAllowed ( flag2);
    theConstraint->setExternalAddressesAllowed ( flag2);
    theConstraint->setVoidAllowed ( flag2);
    theConstraint->setFactAddressesAllowed ( flag2);
}

/*****************************************************/
/* CopyConstraintRecord: Copies a constraint record. */
/*****************************************************/
struct constraintRecord *CopyConstraintRecord(
        const Environment&theEnv,
        CONSTRAINT_RECORD *sourceConstraint) {
    CONSTRAINT_RECORD *theConstraint;

    if (sourceConstraint == nullptr) return nullptr;

    theConstraint = get_struct(theEnv, constraintRecord);

    theConstraint->setAnyAllowed(sourceConstraint->getAnyAllowed());
    theConstraint->setSymbolsAllowed(sourceConstraint->getSymbolsAllowed());
    theConstraint->setStringsAllowed ( sourceConstraint-> getStringsAllowed());
    theConstraint->setFloatsAllowed ( sourceConstraint->  getFloatsAllowed());
    theConstraint->setIntegersAllowed ( sourceConstraint->getIntegersAllowed());
    theConstraint->setInstanceNamesAllowed ( sourceConstraint->getInstanceNamesAllowed());
    theConstraint->setInstanceAddressesAllowed ( sourceConstraint->getInstanceAddressesAllowed());
    theConstraint->setExternalAddressesAllowed ( sourceConstraint->getExternalAddressesAllowed());
    theConstraint->setVoidAllowed ( sourceConstraint->getVoidAllowed());
    theConstraint->setMultifieldsAllowed ( sourceConstraint->getMultifieldsAllowed());
    theConstraint->setSinglefieldsAllowed ( sourceConstraint->getSinglefieldsAllowed());
    theConstraint->setFactAddressesAllowed ( sourceConstraint->getFactAddressesAllowed());
    theConstraint->setAnyRestriction ( sourceConstraint->getAnyRestriction());
    theConstraint->setSymbolRestriction ( sourceConstraint->getSymbolRestriction());
    theConstraint->setStringRestriction ( sourceConstraint->getStringRestriction());
    theConstraint->setFloatRestriction ( sourceConstraint->getFloatRestriction());
    theConstraint->setIntegerRestriction ( sourceConstraint->getIntegerRestriction());
    theConstraint->setClassRestriction ( sourceConstraint->getClassRestriction());
    theConstraint->setInstanceNameRestriction ( sourceConstraint->getInstanceNameRestriction());
    theConstraint->setClassList ( CopyExpression(theEnv, sourceConstraint->getClassList()));
    theConstraint->setRestrictionList ( CopyExpression(theEnv, sourceConstraint->getRestrictionList()));
    theConstraint->setMinValue ( CopyExpression(theEnv, sourceConstraint->getMinValue()));
    theConstraint->setMaxValue ( CopyExpression(theEnv, sourceConstraint->getMaxValue()));
    theConstraint->setMinFields ( CopyExpression(theEnv, sourceConstraint->getMinFields()));
    theConstraint->setMaxFields ( CopyExpression(theEnv, sourceConstraint->getMaxFields()));
    theConstraint->setBucket(0);
    theConstraint->installed = false;
    theConstraint->setCount(0);
    theConstraint->setMultifield(CopyConstraintRecord(theEnv, sourceConstraint->getMultifield()));
    theConstraint->setNext(nullptr);

    return (theConstraint);
}

/**************************************************************/
/* SetAnyRestrictionFlags: Sets the restriction type flags of */
/*   a constraint record to indicate there are restriction on */
/*   all types. If passed an argument of true, just the       */
/*   "any restriction" flag is set to true. If passed an      */
/*   argument of false, then all of the individual type       */
/*   restriction flags are set to true.                       */
/**************************************************************/
void SetAnyRestrictionFlags(
        CONSTRAINT_RECORD *theConstraint,
        bool justOne) {
    bool flag1, flag2;

    if (justOne) {
        flag1 = true;
        flag2 = false;
    } else {
        flag1 = false;
        flag2 = true;
    }

    theConstraint->anyRestriction = flag1;
    theConstraint->symbolRestriction = flag2;
    theConstraint->stringRestriction = flag2;
    theConstraint->floatRestriction = flag2;
    theConstraint->integerRestriction = flag2;
    theConstraint->instanceNameRestriction = flag2;
}


/*****************************************************/
/* SetConstraintType: Given a constraint type and a  */
/*   constraint, sets the allowed type flags for the */
/*   specified type in the constraint to true.       */
/*****************************************************/
bool SetConstraintType(
        int theType,
        CONSTRAINT_RECORD *constraints) {
    bool rv = true;

    switch (theType) {
        case UNKNOWN_VALUE:
            rv = constraints->getAnyAllowed();
            constraints->setAnyAllowed(true);
            break;

        case SYMBOL_TYPE:
            rv = constraints->getSymbolsAllowed();
            constraints->setSymbolsAllowed(true);
            break;

        case STRING_TYPE:
            rv = constraints->getStringsAllowed();
            constraints->setStringsAllowed(true);
            break;

        case SYMBOL_OR_STRING:
            rv = (constraints->getStringsAllowed() || constraints->getSymbolsAllowed());
            constraints->setSymbolsAllowed(true);
            constraints->setStringsAllowed(true);
            break;

        case INTEGER_TYPE:
            rv = constraints->integersAllowed;
            constraints->integersAllowed = true;
            break;

        case FLOAT_TYPE:
            rv = constraints->floatsAllowed;
            constraints->floatsAllowed = true;
            break;

        case INTEGER_OR_FLOAT:
            rv = (constraints->integersAllowed || constraints->floatsAllowed);
            constraints->integersAllowed = true;
            constraints->floatsAllowed = true;
            break;

        case INSTANCE_ADDRESS_TYPE:
            rv = constraints->instanceAddressesAllowed;
            constraints->instanceAddressesAllowed = true;
            break;

        case INSTANCE_NAME_TYPE:
            rv = constraints->instanceNamesAllowed;
            constraints->instanceNamesAllowed = true;
            break;

        case INSTANCE_OR_INSTANCE_NAME:
            rv = (constraints->instanceNamesAllowed || constraints->instanceAddressesAllowed);
            constraints->instanceNamesAllowed = true;
            constraints->instanceAddressesAllowed = true;
            break;

        case EXTERNAL_ADDRESS_TYPE:
            rv = constraints->externalAddressesAllowed;
            constraints->externalAddressesAllowed = true;
            break;

        case VOID_TYPE:
            rv = constraints->voidAllowed;
            constraints->voidAllowed = true;
            break;

        case FACT_ADDRESS_TYPE:
            rv = constraints->factAddressesAllowed;
            constraints->factAddressesAllowed = true;
            break;

        case MULTIFIELD_TYPE:
            rv = constraints->multifieldsAllowed;
            constraints->multifieldsAllowed = true;
            break;
    }

    if (theType != UNKNOWN_VALUE) constraints->setAnyAllowed(false);
    return (rv);
}


/*************************************************************/
/* CompareNumbers: Given two numbers (which can be integers, */
/*   floats, or the symbols for positive/negative infinity)  */
/*   returns the relationship between the numbers (greater   */
/*   than, less than or equal).                              */
/*************************************************************/
int CompareNumbers(
        const Environment&theEnv,
        int type1,
        void *vptr1,
        int type2,
        void *vptr2) {
    /*============================================*/
    /* Handle the situation in which the values   */
    /* are exactly equal (same type, same value). */
    /*============================================*/

    if (vptr1 == vptr2) return (EQUAL);

    /*=======================================*/
    /* Handle the special cases for positive */
    /* and negative infinity.                */
    /*=======================================*/

    if (vptr1 == SymbolData(theEnv)->PositiveInfinity) return (GREATER_THAN);

    if (vptr1 == SymbolData(theEnv)->NegativeInfinity) return (LESS_THAN);

    if (vptr2 == SymbolData(theEnv)->PositiveInfinity) return (LESS_THAN);

    if (vptr2 == SymbolData(theEnv)->NegativeInfinity) return (GREATER_THAN);

    /*=======================*/
    /* Compare two integers. */
    /*=======================*/

    if ((type1 == INTEGER_TYPE) && (type2 == INTEGER_TYPE)) {
        if (((CLIPSInteger *) vptr1)->contents < ((CLIPSInteger *) vptr2)->contents) { return (LESS_THAN); }
        else if (((CLIPSInteger *) vptr1)->contents > ((CLIPSInteger *) vptr2)->contents) { return (GREATER_THAN); }

        return (EQUAL);
    }

    /*=====================*/
    /* Compare two floats. */
    /*=====================*/

    if ((type1 == FLOAT_TYPE) && (type2 == FLOAT_TYPE)) {
        if (((CLIPSFloat *) vptr1)->contents < ((CLIPSFloat *) vptr2)->contents) { return (LESS_THAN); }
        else if (((CLIPSFloat *) vptr1)->contents > ((CLIPSFloat *) vptr2)->contents) { return (GREATER_THAN); }

        return (EQUAL);
    }

    /*================================*/
    /* Compare an integer to a float. */
    /*================================*/

    if ((type1 == INTEGER_TYPE) && (type2 == FLOAT_TYPE)) {
        if (((double) ((CLIPSInteger *) vptr1)->contents) < ((CLIPSFloat *) vptr2)->contents) { return (LESS_THAN); }
        else if (((double) ((CLIPSInteger *) vptr1)->contents) > ((CLIPSFloat *) vptr2)->contents) { return (GREATER_THAN); }

        return (EQUAL);
    }

    /*================================*/
    /* Compare a float to an integer. */
    /*================================*/

    if ((type1 == FLOAT_TYPE) && (type2 == INTEGER_TYPE)) {
        if (((CLIPSFloat *) vptr1)->contents < ((double) ((CLIPSInteger *) vptr2)->contents)) { return (LESS_THAN); }
        else if (((CLIPSFloat *) vptr1)->contents > ((double) ((CLIPSInteger *) vptr2)->contents)) { return (GREATER_THAN); }

        return (EQUAL);
    }

    /*===================================*/
    /* One of the arguments was invalid. */
    /* Return -1 to indicate an error.   */
    /*===================================*/

    return (-1);
}

/****************************************************************/
/* ExpressionToConstraintRecord: Converts an expression into a  */
/*   constraint record. For example, an expression representing */
/*   the symbol BLUE would be converted to a  record with       */
/*   allowed types SYMBOL_TYPE and allow-values BLUE.                */
/****************************************************************/
CONSTRAINT_RECORD *ExpressionToConstraintRecord(
        const Environment&theEnv,
        Expression *theExpression) {
    CONSTRAINT_RECORD *rv;

    /*================================================*/
    /* A nullptr expression is converted to a constraint */
    /* record with no values allowed.                 */
    /*================================================*/

    if (theExpression == nullptr) {
        rv = GetConstraintRecord(theEnv);
        rv->setAnyAllowed(false);
        return (rv);
    }

    /*=============================================================*/
    /* Convert variables and function calls to constraint records. */
    /*=============================================================*/

    if ((theExpression->type == SF_VARIABLE) ||
        (theExpression->type == MF_VARIABLE) ||
        #if DEFGENERIC_CONSTRUCT
        (theExpression->type == GCALL) ||
        #endif
        #if DEFFUNCTION_CONSTRUCT
        (theExpression->type == PCALL) ||
        #endif
        (theExpression->type == GBL_VARIABLE) ||
        (theExpression->type == MF_GBL_VARIABLE)) {
        rv = GetConstraintRecord(theEnv);
        rv->multifieldsAllowed = true;
        return (rv);
    } else if (theExpression->type == FCALL) { return (FunctionCallToConstraintRecord(theEnv, theExpression->value)); }

    /*============================================*/
    /* Convert a constant to a constraint record. */
    /*============================================*/

    rv = GetConstraintRecord(theEnv);
    rv->setAnyAllowed(false);

    switch (theExpression->type) {
        case FLOAT_TYPE:
            rv->floatRestriction = true;
            rv->floatsAllowed = true;
            break;

        case INTEGER_TYPE:
            rv->integerRestriction = true;
            rv->integersAllowed = true;
            break;

        case SYMBOL_TYPE:
            rv->setSymbolRestriction(true);
            rv->setSymbolsAllowed(true);
            break;

        case STRING_TYPE:
            rv->setStringRestriction(true);
            rv->setStringsAllowed(true);
            break;

        case INSTANCE_NAME_TYPE:
            rv->instanceNameRestriction = true;
            rv->instanceNamesAllowed = true;
            break;

        case INSTANCE_ADDRESS_TYPE:
            rv->instanceAddressesAllowed = true;
            break;

        default:
            break;
    }

    if (rv->getFloatsAllowed() || rv->getIntegersAllowed() || rv->getSymbolsAllowed() ||
        rv->getStringsAllowed()|| rv->getInstanceNamesAllowed()) {
        rv->setRestrictionList(GenConstant(theEnv, theExpression->type, theExpression->value));
    }

    return rv;
}

/*******************************************************/
/* FunctionCallToConstraintRecord: Converts a function */
/*   call to a constraint record. For example, the +   */
/*   function when converted would be a constraint     */
/*   record with allowed types INTEGER_TYPE and FLOAT_TYPE.      */
/*******************************************************/
CONSTRAINT_RECORD *FunctionCallToConstraintRecord(
        const Environment&theEnv,
        void *theFunction) {
    return ArgumentTypeToConstraintRecord(theEnv, UnknownFunctionType(theFunction));
}

/*********************************************/
/* ArgumentTypeToConstraintRecord2: Uses the */
/*   new argument type codes for 6.4.        */
/*********************************************/
CONSTRAINT_RECORD *ArgumentTypeToConstraintRecord(
        const Environment&theEnv,
        unsigned bitTypes) {
    CONSTRAINT_RECORD *rv;

    rv = GetConstraintRecord(theEnv);
    rv->setAnyAllowed(false);

    if (bitTypes & VOID_BIT) { rv->voidAllowed = true; }
    if (bitTypes & FLOAT_BIT) { rv->floatsAllowed = true; }
    if (bitTypes & INTEGER_BIT) { rv->integersAllowed = true; }
    if (bitTypes & SYMBOL_BIT) { rv->setSymbolsAllowed(true); }
    if (bitTypes & STRING_BIT) { rv->setStringsAllowed(true); }
    if (bitTypes & MULTIFIELD_BIT) { rv->multifieldsAllowed = true; }
    if (bitTypes & EXTERNAL_ADDRESS_BIT) { rv->externalAddressesAllowed = true; }
    if (bitTypes & FACT_ADDRESS_BIT) { rv->factAddressesAllowed = true; }
    if (bitTypes & INSTANCE_ADDRESS_BIT) { rv->instanceAddressesAllowed = true; }
    if (bitTypes & INSTANCE_NAME_BIT) { rv->instanceNamesAllowed = true; }
    if (bitTypes & BOOLEAN_BIT) { rv->setSymbolsAllowed(true); }

    if (bitTypes == ANY_TYPE_BITS) { rv->setAnyAllowed(true); }

    return (rv);
}

