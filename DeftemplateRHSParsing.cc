/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  02/19/20             */
/*                                                     */
/*          DEFTEMPLATE RHS PARSING HEADER FILE        */
/*******************************************************/

/*************************************************************/
/* Purpose: Parses deftemplate fact patterns used with the   */
/*   assert function.                                        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added additional argument required for         */
/*            DeriveDefaultFromConstraints.                  */
/*                                                           */
/*            Added additional argument required for         */
/*            InvalidDeftemplateSlotMessage.                 */
/*                                                           */
/*      6.30: Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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
/*************************************************************/

#include "Setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include <cstdio>

#include "DefaultAttribute.h"
#include "ExternalFunctions.h"
#include "Fact.h"
#include "MemoryAllocation.h"
#include "DefmoduleUtility.h"
#include "PrettyPrint.h"
#include "PrintUtility.h"
#include "Router.h"
#include "Deftemplate.h"
#include "DeftemplateFunctions.h"
#include "DeftemplateLHS.h"
#include "DeftemplateUtilities.h"

#include "DeftemplateRHSParsing.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static struct expr *ParseAssertSlotValues(const Environment&, const char *, struct token *, struct templateSlot *, bool *, bool);
static struct expr *ReorderAssertSlotValues(const Environment&, struct templateSlot *, struct expr *, bool *);
static struct expr *GetSlotAssertValues(const Environment&, struct templateSlot *, struct expr *, bool *);
static struct expr *FindAssertSlotItem(templateSlot *, struct expr *);
static struct templateSlot *ParseSlotLabel(const Environment&, const char *, struct token *, Deftemplate *, bool *, TokenType);

/******************************************************************/
/* ParseAssertTemplate: Parses and builds the list of values that */
/*   are used for an assert of a fact with a deftemplate.         */
/******************************************************************/
struct expr *ParseAssertTemplate(
        const Environment&theEnv,
        const char *readSource,
        struct token *theToken,
        bool *error,
        TokenType endType,
        bool constantsOnly,
        Deftemplate *theDeftemplate) {
    struct expr *firstSlot, *lastSlot, *nextSlot = nullptr;
    struct expr *firstArg, *tempSlot;
    struct templateSlot *slotPtr;

    firstSlot = nullptr;
    lastSlot = nullptr;

    /*==============================================*/
    /* Parse each of the slot fields in the assert. */
    /*==============================================*/

    while ((slotPtr = ParseSlotLabel(theEnv, readSource, theToken, theDeftemplate, error, endType)) != nullptr) {
        /*========================================================*/
        /* Check to see that the slot hasn't already been parsed. */
        /*========================================================*/

        for (tempSlot = firstSlot;
             tempSlot != nullptr;
             tempSlot = tempSlot->nextArg) {
            if (tempSlot->value == (void *) slotPtr->slotName) {
                AlreadyParsedErrorMessage(theEnv, "slot ", slotPtr->slotName->contents);
                *error = true;
                ReturnExpression(theEnv, firstSlot);
                return nullptr;
            }
        }

        /*============================================*/
        /* Parse the values to be stored in the slot. */
        /*============================================*/

        nextSlot = ParseAssertSlotValues(theEnv, readSource, theToken,
                                         slotPtr, error, constantsOnly);

        if (*error) {
            ReturnExpression(theEnv, firstSlot);
            return nullptr;
        }

        /*============================================*/
        /* Check to see if the values to be stored in */
        /* the slot violate the slot's constraints.   */
        /*============================================*/

        if (CheckRHSSlotTypes(theEnv, nextSlot->argList, slotPtr, "assert") == 0) {
            *error = true;
            ReturnExpression(theEnv, firstSlot);
            ReturnExpression(theEnv, nextSlot);
            return nullptr;
        }

        /*===================================================*/
        /* Add the slot to the list of slots already parsed. */
        /*===================================================*/

        if (lastSlot == nullptr) { firstSlot = nextSlot; }
        else { lastSlot->nextArg = nextSlot; }

        lastSlot = nextSlot;
    }

    /*=================================================*/
    /* Return if an error occured parsing a slot name. */
    /*=================================================*/

    if (*error) {
        ReturnExpression(theEnv, firstSlot);
        return nullptr;
    }

    /*=============================================================*/
    /* Reorder the arguments to the order used by the deftemplate. */
    /*=============================================================*/

    firstArg = ReorderAssertSlotValues(theEnv, theDeftemplate->slotList, firstSlot, error);
    ReturnExpression(theEnv, firstSlot);

    /*==============================*/
    /* Return the assert arguments. */
    /*==============================*/

    return (firstArg);
}

/****************************************************************/
/* ParseSlotLabel: Parses the beginning of a slot definition.   */
/*   Checks for opening left parenthesis and a valid slot name. */
/****************************************************************/
static struct templateSlot *ParseSlotLabel(
        const Environment&theEnv,
        const char *inputSource,
        struct token *tempToken,
        Deftemplate *theDeftemplate,
        bool *error,
        TokenType endType) {
    struct templateSlot *slotPtr;

    /*========================*/
    /* Initialize error flag. */
    /*========================*/

    *error = false;

    /*============================================*/
    /* If token is a right parenthesis, then fact */
    /* template definition is complete.           */
    /*============================================*/

    GetToken(theEnv, inputSource, tempToken);
    if (tempToken->tknType == endType) { return nullptr; }

    /*=======================================*/
    /* Put a space between the template name */
    /* and the first slot definition.        */
    /*=======================================*/

    PPBackup(theEnv);
    SavePPBuffer(theEnv, " ");
    SavePPBuffer(theEnv, tempToken->printForm);

    /*=======================================================*/
    /* Slot definition begins with opening left parenthesis. */
    /*=======================================================*/

    if (tempToken->tknType != LEFT_PARENTHESIS_TOKEN) {
        SyntaxErrorMessage(theEnv, "deftemplate pattern");
        *error = true;
        return nullptr;
    }

    /*=============================*/
    /* Slot name must be a symbol. */
    /*=============================*/

    GetToken(theEnv, inputSource, tempToken);
    if (tempToken->tknType != SYMBOL_TOKEN) {
        SyntaxErrorMessage(theEnv, "deftemplate pattern");
        *error = true;
        return nullptr;
    }

    /*======================================================*/
    /* Check that the slot name is valid for this template. */
    /*======================================================*/

    if ((slotPtr = FindSlot(theDeftemplate, tempToken->lexemeValue, nullptr)) == nullptr) {
        InvalidDeftemplateSlotMessage(theEnv, tempToken->lexemeValue->contents,
                                      theDeftemplate->header.name->contents, true);
        *error = true;
        return nullptr;
    }

    /*====================================*/
    /* Return a pointer to the slot name. */
    /*====================================*/

    return slotPtr;
}

/**************************************************************************/
/* ParseAssertSlotValues: Gets a single assert slot value for a template. */
/**************************************************************************/
static struct expr *ParseAssertSlotValues(
        const Environment&theEnv,
        const char *inputSource,
        struct token *tempToken,
        struct templateSlot *slotPtr,
        bool *error,
        bool constantsOnly) {
    struct expr *nextSlot;
    struct expr *newField, *valueList, *lastValue;
    bool printError;

    /*=============================*/
    /* Handle a single field slot. */
    /*=============================*/

    if (slotPtr->multislot == false) {
        /*=====================*/
        /* Get the slot value. */
        /*=====================*/

        SavePPBuffer(theEnv, " ");

        newField = GetAssertArgument(theEnv, inputSource, tempToken, error,
                                     RIGHT_PARENTHESIS_TOKEN, constantsOnly, &printError);
        if (*error) {
            if (printError) SyntaxErrorMessage(theEnv, "deftemplate pattern");
            return nullptr;
        }

        /*=================================================*/
        /* A single field slot value must contain a value. */
        /* Only a multifield slot can be empty.            */
        /*=================================================*/

        if (newField == nullptr) {
            *error = true;
            SingleFieldSlotCardinalityError(theEnv, slotPtr->slotName->contents);
            return nullptr;
        }

        /*==============================================*/
        /* A function returning a multifield value can  */
        /* not be called to get the value for the slot. */
        /*==============================================*/

        if (newField->type == MF_VARIABLE) {
            *error = true;
            SingleFieldSlotCardinalityError(theEnv, slotPtr->slotName->contents);
            ReturnExpression(theEnv, newField);
            return nullptr;
        } else if (newField->type == FCALL) {
            if ((ExpressionUnknownFunctionType(newField) & SINGLEFIELD_BITS) == 0) {
                *error = true;
                SingleFieldSlotCardinalityError(theEnv, slotPtr->slotName->contents);
                ReturnExpression(theEnv, newField);
                return nullptr;
            }
        }

        /*============================*/
        /* Move on to the next token. */
        /*============================*/

        GetToken(theEnv, inputSource, tempToken);
    }

        /*========================================*/
        /* Handle a multifield slot. Build a list */
        /* of the values stored in the slot.      */
        /*========================================*/

    else {
        SavePPBuffer(theEnv, " ");
        valueList = GetAssertArgument(theEnv, inputSource, tempToken, error,
                                      RIGHT_PARENTHESIS_TOKEN, constantsOnly, &printError);
        if (*error) {
            if (printError) SyntaxErrorMessage(theEnv, "deftemplate pattern");
            return nullptr;
        }

        if (valueList == nullptr) {
            PPBackup(theEnv);
            PPBackup(theEnv);
            SavePPBuffer(theEnv, ")");
        }

        lastValue = valueList;

        while (lastValue != nullptr) /* (tempToken->tknType != RIGHT_PARENTHESIS_TOKEN) */
        {
            if (tempToken->tknType == RIGHT_PARENTHESIS_TOKEN) { SavePPBuffer(theEnv, " "); }
            else {
                /* PPBackup(theEnv); */
                SavePPBuffer(theEnv, " ");
                /* SavePPBuffer(theEnv,tempToken->printForm); */
            }

            newField = GetAssertArgument(theEnv, inputSource, tempToken, error,
                                         RIGHT_PARENTHESIS_TOKEN, constantsOnly, &printError);
            if (*error) {
                if (printError) SyntaxErrorMessage(theEnv, "deftemplate pattern");
                ReturnExpression(theEnv, valueList);
                return nullptr;
            }

            if (newField == nullptr) {
                PPBackup(theEnv);
                PPBackup(theEnv);
                SavePPBuffer(theEnv, ")");
            }

            lastValue->nextArg = newField;
            lastValue = newField;
        }

        newField = valueList;
    }

    /*==========================================================*/
    /* Slot definition must be closed with a right parenthesis. */
    /*==========================================================*/

    if (tempToken->tknType != RIGHT_PARENTHESIS_TOKEN) {
        SingleFieldSlotCardinalityError(theEnv, slotPtr->slotName->contents);
        *error = true;
        ReturnExpression(theEnv, newField);
        return nullptr;
    }

    /*=========================================================*/
    /* Build and return a structure describing the slot value. */
    /*=========================================================*/

    nextSlot = GenConstant(theEnv, SYMBOL_TYPE, slotPtr->slotName);
    nextSlot->argList = newField;

    return (nextSlot);
}

/*************************************************************************/
/* ReorderAssertSlotValues: Rearranges the asserted values to correspond */
/*   to the order of the values described by the deftemplate.            */
/*************************************************************************/
static struct expr *ReorderAssertSlotValues(
        const Environment&theEnv,
        struct templateSlot *slotPtr,
        struct expr *firstSlot,
        bool *error) {
    struct expr *firstArg = nullptr;
    struct expr *lastArg = nullptr, *newArg;

    /*=============================================*/
    /* Loop through each of the slots in the order */
    /* they're found in the deftemplate.           */
    /*=============================================*/

    for (;
            slotPtr != nullptr;
            slotPtr = slotPtr->next) {
        /*==============================================*/
        /* Get either the value specified in the assert */
        /* command or the default value for the slot.   */
        /*==============================================*/

        newArg = GetSlotAssertValues(theEnv, slotPtr, firstSlot, error);

        if (*error) {
            ReturnExpression(theEnv, firstArg);
            return nullptr;
        }

        /*=====================================*/
        /* Add the value to the list of values */
        /* for the assert command.             */
        /*=====================================*/

        if (newArg != nullptr) {
            if (lastArg == nullptr) { firstArg = newArg; }
            else { lastArg->nextArg = newArg; }

            lastArg = newArg;
        }
    }

    /*==============================*/
    /* Return the list of arguments */
    /* for the assert command.      */
    /*==============================*/

    return (firstArg);
}

/***************************************************************/
/* GetSlotAssertValues: Gets the assert value for a given slot */
/*   of a deftemplate. If the value was supplied by the user,  */
/*   it will be used. If not the default value or default      */
/*   default value will be used.                               */
/***************************************************************/
static struct expr *GetSlotAssertValues(
        const Environment&theEnv,
        struct templateSlot *slotPtr,
        struct expr *firstSlot,
        bool *error) {
    struct expr *slotItem;
    struct expr *newArg, *tempArg;
    UDFValue theDefault;
    const char *nullptrBitMap = "\0";

    /*==================================================*/
    /* Determine if the slot is assigned in the assert. */
    /*==================================================*/

    slotItem = FindAssertSlotItem(slotPtr, firstSlot);

    /*==========================================*/
    /* If the slot is assigned, use that value. */
    /*==========================================*/

    if (slotItem != nullptr) {
        newArg = slotItem->argList;
        slotItem->argList = nullptr;
    }

        /*=================================*/
        /* Otherwise, use a default value. */
        /*=================================*/

    else {
        /*================================================*/
        /* If the (default ?NONE) attribute was specified */
        /* for the slot, then a value must be supplied.   */
        /*================================================*/

        if (slotPtr->noDefault) {
            PrintErrorID(theEnv, "TMPLTRHS", 1, true);
            WriteString(theEnv, STDERR, "Slot '");
            WriteString(theEnv, STDERR, slotPtr->slotName->contents);
            WriteString(theEnv, STDERR, "' requires a value because of its (default ?NONE) attribute.\n");
            *error = true;
            return nullptr;
        }

            /*===================================================*/
            /* If the (default ?DERIVE) attribute was specified  */
            /* (the default), then derive the default value from */
            /* the slot's constraints.                           */
            /*===================================================*/

        else if ((slotPtr->defaultPresent == false) &&
                 (slotPtr->defaultDynamic == false)) {
            DeriveDefaultFromConstraints(theEnv, slotPtr->constraints, &theDefault,
                                         slotPtr->multislot, true);
            newArg = ConvertValueToExpression(theEnv, &theDefault);
        }

            /*=========================================*/
            /* Otherwise, use the expression contained */
            /* in the default attribute.               */
            /*=========================================*/

        else { newArg = CopyExpression(theEnv, slotPtr->defaultList); }
    }

    /*=======================================================*/
    /* Since a multifield slot default can contain a list of */
    /* values, the values need to have a store-multifield    */
    /* function called wrapped around it to group all of the */
    /* values into a single multifield value.                */
    /*=======================================================*/

    if (slotPtr->multislot) {
        tempArg = GenConstant(theEnv, FACT_STORE_MULTIFIELD, AddBitMap(theEnv, (void *) nullptrBitMap, 1));
        tempArg->argList = newArg;
        newArg = tempArg;
    }

    /*==============================================*/
    /* Return the value to be asserted in the slot. */
    /*==============================================*/

    return (newArg);
}

/*******************************************************************/
/* FindAssertSlotItem: Finds a particular slot in a list of slots. */
/*******************************************************************/
static struct expr *FindAssertSlotItem(
        struct templateSlot *slotPtr,
        struct expr *listOfSlots) {
    while (listOfSlots != nullptr) {
        if (listOfSlots->value == (void *) slotPtr->slotName) return (listOfSlots);
        listOfSlots = listOfSlots->nextArg;
    }

    return nullptr;
}

#endif /* DEFTEMPLATE_CONSTRUCT */

