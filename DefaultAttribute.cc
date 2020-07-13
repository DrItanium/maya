/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  08/25/16             */
/*                                                     */
/*               DEFAULT ATTRIBUTE MODULE              */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for parsing the default       */
/*   attribute and determining default values based on       */
/*   slot constraints.                                       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian Dantes                                         */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Support for deftemplate-slot-default-value     */
/*            function.                                      */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "Constants.h"
#include "Constraint.h"
#include "Environment.h"
#include "Expression.h"
#include "Fact.h"
#include "InstanceCommand.h"
#include "Multifield.h"
#include "PrettyPrint.h"
#include "PrintUtility.h"
#include "Router.h"
#include "Scanner.h"

#include "DefaultAttribute.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void *FindDefaultValue(const Environment&, int, CONSTRAINT_RECORD *, void *);

/********************************************************/
/* DeriveDefaultFromConstraints: Returns an appropriate */
/*   default value for the supplied constraints.        */
/********************************************************/
void DeriveDefaultFromConstraints(
        const Environment&theEnv,
        CONSTRAINT_RECORD *constraints,
        UDFValue *theDefault,
        bool multifield,
        bool garbageMultifield) {
    unsigned long minFields;
    void *theValue;

    /*=============================================================*/
    /* If no constraints are specified, then use the symbol nil as */
    /* a default for single field slots and a multifield of length */
    /* 0 as a default for multifield slots.                        */
    /*=============================================================*/

    if (constraints == nullptr) {
        if (multifield) {
            theDefault->begin = 0;
            theDefault->range = 0;
            if (garbageMultifield) theDefault->value = CreateMultifield(theEnv, 0L);
            else theDefault->value = CreateUnmanagedMultifield(theEnv, 0L);
        } else { theDefault->value = CreateSymbol(theEnv, "nil"); }

        return;
    }

    /*=========================================*/
    /* Determine the default's type and value. */
    /*=========================================*/

    if (constraints->getAnyAllowed()|| constraints->getSymbolsAllowed()) {
        theValue = FindDefaultValue(theEnv, SYMBOL_TYPE, constraints, CreateSymbol(theEnv, "nil"));
    } else if (constraints->getStringsAllowed()) { theValue = FindDefaultValue(theEnv, STRING_TYPE, constraints, CreateString(theEnv, "")); }

    else if (constraints->integersAllowed) { theValue = FindDefaultValue(theEnv, INTEGER_TYPE, constraints, CreateInteger(theEnv, 0LL)); }

    else if (constraints->floatsAllowed) { theValue = FindDefaultValue(theEnv, FLOAT_TYPE, constraints, CreateFloat(theEnv, 0.0)); }
    else if (constraints->instanceNamesAllowed) {
        theValue = FindDefaultValue(theEnv, INSTANCE_NAME_TYPE, constraints, CreateInstanceName(theEnv, "nil"));
    } else if (constraints->instanceAddressesAllowed) { theValue = &InstanceData(theEnv)->DummyInstance; }
#if DEFTEMPLATE_CONSTRUCT
    else if (constraints->factAddressesAllowed) { theValue = &FactData(theEnv)->DummyFact; }
#endif
    else if (constraints->externalAddressesAllowed) { theValue = CreateExternalAddress(theEnv, nullptr, 0); }

    else { theValue = CreateSymbol(theEnv, "nil"); }

    /*=========================================================*/
    /* If the default is for a multifield slot, then create a  */
    /* multifield default value that satisfies the cardinality */
    /* constraints for the slot. The default value for a       */
    /* multifield slot is a multifield of length 0.            */
    /*=========================================================*/

    if (multifield) {
        if (constraints->getMinFields() == nullptr) minFields = 0;
        else if (constraints->getMinFields()->value == SymbolData(theEnv)->NegativeInfinity) minFields = 0;
        else minFields = (unsigned long) constraints->getMinFields()->integerValue->contents;

        theDefault->begin = 0;
        theDefault->range = minFields;
        if (garbageMultifield) theDefault->value = CreateMultifield(theEnv, minFields);
        else theDefault->value = CreateUnmanagedMultifield(theEnv, minFields);

        for (; minFields > 0; minFields--) { theDefault->multifieldValue->contents[minFields - 1].value = theValue; }
    } else {
        theDefault->value = theValue;
    }
}

/***********************************************************************/
/* FindDefaultValue: Searches the list of restriction values for a     */
/*   constraint to find a default value of the specified type. For     */
/*   example, if the attribute (allowed-symbols on off) was specified, */
/*   then the symbol "on" would be used as a default value rather than */
/*   the symbol "nil". For integers and floats, the range attribute is */
/*   also used to select a suitable default value. If a minimum value  */
/*   was specified, then this value is used first followed by the      */
/*   maximum value.                                                    */
/************************************************************************/
static void *FindDefaultValue(
        const Environment&theEnv,
        int theType,
        CONSTRAINT_RECORD *theConstraints,
        void *standardDefault) {
    /*=====================================================*/
    /* Look on the the allowed values list to see if there */
    /* is a value of the requested type. Return the first  */
    /* value found of the requested type.                  */
    /*=====================================================*/

    auto theList = theConstraints->getRestrictionList();
    while (theList != nullptr) {
        if (theList->type == theType) return (theList->value);
        theList = theList->nextArg;
    }

    /*=============================================================*/
    /* If no specific values were available for the default value, */
    /* and the type requested is a float or integer, then use the  */
    /* range attribute to select a default value.                  */
    /*=============================================================*/

    if (theType == INTEGER_TYPE) {
        if (theConstraints->getMinValue()->type == INTEGER_TYPE) { return (theConstraints->getMinValue()->value); }
        else if (theConstraints->getMinValue()->type == FLOAT_TYPE) {
            return (CreateInteger(theEnv, (long long) theConstraints->getMinValue()->floatValue->contents));
        } else if (theConstraints->getMaxValue()->type == INTEGER_TYPE) { return (theConstraints->getMaxValue()->value); }
        else if (theConstraints->getMaxValue()->type == FLOAT_TYPE) {
            return (CreateInteger(theEnv, (long long) theConstraints->getMaxValue()->floatValue->contents));
        }
    } else if (theType == FLOAT_TYPE) {
        if (theConstraints->getMinValue()->type == FLOAT_TYPE) { return (theConstraints->getMinValue()->value); }
        else if (theConstraints->getMinValue()->type == INTEGER_TYPE) {
            return (CreateFloat(theEnv, (double) theConstraints->getMinValue()->integerValue->contents));
        } else if (theConstraints->getMaxValue()->type == FLOAT_TYPE) { return (theConstraints->getMaxValue()->value); }
        else if (theConstraints->getMaxValue()->type == INTEGER_TYPE) {
            return (CreateFloat(theEnv, (double) theConstraints->getMaxValue()->integerValue->contents));
        }
    }

    /*======================================*/
    /* Use the standard default value (such */
    /* as nil if symbols are allowed).      */
    /*======================================*/

    return (standardDefault);
}


/**********************************************/
/* ParseDefault: Parses a default value list. */
/**********************************************/
Expression *ParseDefault(
        const Environment&theEnv,
        const char *readSource,
        bool multifield,
        bool dynamic,
        bool evalStatic,
        bool *noneSpecified,
        bool *deriveSpecified,
        bool *error) {
    Expression *defaultList = nullptr, *lastDefault = nullptr;
    Expression *newItem, *tmpItem;
    struct token theToken;
    UDFValue theValue;
    CONSTRAINT_RECORD *rv;
    int specialVarCode;

    *noneSpecified = false;
    *deriveSpecified = false;

    SavePPBuffer(theEnv, " ");
    GetToken(theEnv, readSource, &theToken);

    /*===================================================*/
    /* Read the items contained in the default attribute */
    /* until a closing right parenthesis is encountered. */
    /*===================================================*/

    while (theToken.tknType != RIGHT_PARENTHESIS_TOKEN) {
        /*========================================*/
        /* Get the next item in the default list. */
        /*========================================*/

        newItem = ParseAtomOrExpression(theEnv, readSource, &theToken);
        if (newItem == nullptr) {
            ReturnExpression(theEnv, defaultList);
            *error = true;
            return nullptr;
        }

        /*===========================================================*/
        /* Check for invalid variable usage. With the expection of   */
        /* ?NONE for the default attribute, local variables may not  */
        /* be used within the default or default-dynamic attributes. */
        /*===========================================================*/

        if ((newItem->type == SF_VARIABLE) || (newItem->type == MF_VARIABLE)) {
            if (strcmp(newItem->lexemeValue->contents, "NONE") == 0) { specialVarCode = 0; }
            else if (strcmp(newItem->lexemeValue->contents, "DERIVE") == 0) { specialVarCode = 1; }
            else { specialVarCode = -1; }

            if ((dynamic) ||
                (newItem->type == MF_VARIABLE) ||
                (specialVarCode == -1) ||
                ((specialVarCode != -1) && (defaultList != nullptr))) {
                if (dynamic) SyntaxErrorMessage(theEnv, "default-dynamic attribute");
                else SyntaxErrorMessage(theEnv, "default attribute");
                ReturnExpression(theEnv, newItem);
                ReturnExpression(theEnv, defaultList);
                *error = true;
                return nullptr;
            }

            ReturnExpression(theEnv, newItem);

            /*============================================*/
            /* Check for the closing right parenthesis of */
            /* the default or default dynamic attribute.  */
            /*============================================*/

            GetToken(theEnv, readSource, &theToken);

            if (theToken.tknType != RIGHT_PARENTHESIS_TOKEN) {
                if (dynamic) SyntaxErrorMessage(theEnv, "default-dynamic attribute");
                else SyntaxErrorMessage(theEnv, "default attribute");
                PPBackup(theEnv);
                SavePPBuffer(theEnv, " ");
                SavePPBuffer(theEnv, theToken.printForm);
                *error = true;
            }

            if (specialVarCode == 0)
                *noneSpecified = true;
            else
                *deriveSpecified = true;
            return nullptr;
        }

        /*====================================================*/
        /* Look to see if any variables have been used within */
        /* expressions contained within the default list.     */
        /*====================================================*/

        if (ExpressionContainsVariables(newItem, false)) {
            ReturnExpression(theEnv, defaultList);
            ReturnExpression(theEnv, newItem);
            *error = true;
            if (dynamic) SyntaxErrorMessage(theEnv, "default-dynamic attribute");
            else SyntaxErrorMessage(theEnv, "default attribute");
            return nullptr;
        }

        /*============================================*/
        /* Add the default value to the default list. */
        /*============================================*/

        if (lastDefault == nullptr) { defaultList = newItem; }
        else { lastDefault->nextArg = newItem; }
        lastDefault = newItem;

        /*=======================================*/
        /* Begin parsing the next default value. */
        /*=======================================*/

        SavePPBuffer(theEnv, " ");
        GetToken(theEnv, readSource, &theToken);
    }

    /*=====================================*/
    /* Fix up pretty print representation. */
    /*=====================================*/

    PPBackup(theEnv);
    PPBackup(theEnv);
    SavePPBuffer(theEnv, ")");

    /*=========================================*/
    /* A single field slot's default attribute */
    /* must contain a single value.            */
    /*=========================================*/

    if (!multifield) {
        if (defaultList == nullptr) { *error = true; }
        else if (defaultList->nextArg != nullptr) { *error = true; }
        else {
            rv = ExpressionToConstraintRecord(theEnv, defaultList);
            rv->multifieldsAllowed = false;
            if (UnmatchableConstraint(rv)) *error = true;
            RemoveConstraint(theEnv, rv);
        }

        if (*error) {
            PrintErrorID(theEnv, "DEFAULT", 1, true);
            WriteString(theEnv, STDERR, "The default value for a single field slot must be a single field value.\n");
            ReturnExpression(theEnv, defaultList);
            return nullptr;
        }
    }

    /*=======================================================*/
    /* If the dynamic-default attribute is not being parsed, */
    /* evaluate the expressions to make the default value.   */
    /*=======================================================*/

    if (dynamic || (!evalStatic) || (defaultList == nullptr)) return (defaultList);

    tmpItem = defaultList;
    newItem = defaultList;

    defaultList = nullptr;

    while (newItem != nullptr) {
        SetEvaluationError(theEnv, false);
        if (EvaluateExpression(theEnv, newItem, &theValue)) *error = true;

        if ((theValue.header->type == MULTIFIELD_TYPE) &&
            !multifield &&
            !*error) {
            PrintErrorID(theEnv, "DEFAULT", 1, true);
            WriteString(theEnv, STDERR, "The default value for a single field slot must be a single field value.\n");
            *error = true;
        }

        if (*error) {
            ReturnExpression(theEnv, tmpItem);
            ReturnExpression(theEnv, defaultList);
            *error = true;
            return nullptr;
        }

        lastDefault = ConvertValueToExpression(theEnv, &theValue);

        defaultList = AppendExpressions(defaultList, lastDefault);

        newItem = newItem->nextArg;
    }

    ReturnExpression(theEnv, tmpItem);

    /*==========================*/
    /* Return the default list. */
    /*==========================*/

    return (defaultList);
}

