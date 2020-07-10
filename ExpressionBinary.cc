/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  11/01/16             */
/*                                                     */
/*             EXPRESSION BSAVE/BLOAD MODULE           */
/*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    expression data structure.                             */
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

#if (BLOAD_AND_BSAVE)

#include <cstdio>

#include "BinaryLoad.h"
#include "BinarySave.h"
#include "Construct.h"
#include "Deffacts.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "MemoryAllocation.h"
#include "Defmodule.h"

#include "Network.h"

#if DEFGENERIC_CONSTRUCT
#include "GenericFunctionBinaryLoadSave.h"
#endif

#if DEFFUNCTION_CONSTRUCT
#include "DeffunctionBinaryLoadSave.h"
#endif

#if DEFTEMPLATE_CONSTRUCT
#include "Fact.h"
#include "DeftemplateBinarySaveLoad.h"
#endif

#if DEFGLOBAL_CONSTRUCT
#include "DefglobalBinary.h"
#endif

#include "ObjectBinaryLoadSave.h"
#include "InstanceFunctions.h"
#include "InstanceCommand.h"

#include "Expression.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void UpdateExpression(const Environment&, void *, unsigned long);

/***********************************************************/
/* AllocateExpressions: Determines the amount of space     */
/*   required for loading the binary image of expressions  */
/*   and allocates that amount of space.                   */
/***********************************************************/
void AllocateExpressions(
        const Environment&theEnv) {
    size_t space;

    GenReadBinary(theEnv, &ExpressionData(theEnv)->NumberOfExpressions, sizeof(long));
    if (ExpressionData(theEnv)->NumberOfExpressions == 0L)
        ExpressionData(theEnv)->ExpressionArray = nullptr;
    else {
        space = ExpressionData(theEnv)->NumberOfExpressions * sizeof(expr);
        ExpressionData(theEnv)->ExpressionArray = (expr *) genalloc(theEnv, space);
    }
}

/**********************************************/
/* RefreshExpressions: Refreshes the pointers */
/*   used by the expression binary image.     */
/**********************************************/
void RefreshExpressions(
        const Environment&theEnv) {
    if (ExpressionData(theEnv)->ExpressionArray == nullptr) return;

    BloadandRefresh(theEnv, ExpressionData(theEnv)->NumberOfExpressions,
                    sizeof(BSAVE_EXPRESSION), UpdateExpression);
}

/*********************************************************
  NAME         : UpdateExpression
  DESCRIPTION  : Given a bloaded expression buffer,
                   this routine refreshes the pointers
                   in the expression array
  INPUTS       : 1) a bloaded expression buffer
                 2) the index of the expression to refresh
  RETURNS      : Nothing useful
  SIDE EFFECTS : Expression updated
  NOTES        : None
 *********************************************************/
static void UpdateExpression(
        const Environment&theEnv,
        void *buf,
        unsigned long obji) {
    BSAVE_EXPRESSION *bexp;
    unsigned long theIndex;

    bexp = (BSAVE_EXPRESSION *) buf;
    ExpressionData(theEnv)->ExpressionArray[obji].type = bexp->type;
    switch (bexp->type) {
        case FCALL:
            ExpressionData(theEnv)->ExpressionArray[obji].value = BloadData(theEnv)->FunctionArray[bexp->value];
            break;

        case GCALL:
#if DEFGENERIC_CONSTRUCT
            ExpressionData(theEnv)->ExpressionArray[obji].value = GenericPointer(bexp->value);
#else
            ExpressionData(theEnv)->ExpressionArray[obji].value = nullptr;
#endif
            break;

        case PCALL:
#if DEFFUNCTION_CONSTRUCT
            ExpressionData(theEnv)->ExpressionArray[obji].value = DeffunctionPointer(bexp->value);
#else
            ExpressionData(theEnv)->ExpressionArray[obji].value = nullptr;
#endif
            break;

        case DEFTEMPLATE_PTR:
#if DEFTEMPLATE_CONSTRUCT
            ExpressionData(theEnv)->ExpressionArray[obji].value = DeftemplatePointer(bexp->value);
#else
            ExpressionData(theEnv)->ExpressionArray[obji].value = nullptr;
#endif
            break;

        case DEFCLASS_PTR:
            ExpressionData(theEnv)->ExpressionArray[obji].value = DefclassPointer(bexp->value);
            break;

        case DEFGLOBAL_PTR:

#if DEFGLOBAL_CONSTRUCT
            ExpressionData(theEnv)->ExpressionArray[obji].value = DefglobalPointer(bexp->value);
#else
            ExpressionData(theEnv)->ExpressionArray[obji].value = nullptr;
#endif
            break;

        case INTEGER_TYPE:
            ExpressionData(theEnv)->ExpressionArray[obji].value = SymbolData(theEnv)->IntegerArray[bexp->value];
            IncrementIntegerCount(ExpressionData(theEnv)->ExpressionArray[obji].integerValue);
            break;

        case FLOAT_TYPE:
            ExpressionData(theEnv)->ExpressionArray[obji].value = SymbolData(theEnv)->FloatArray[bexp->value];
            IncrementFloatCount(ExpressionData(theEnv)->ExpressionArray[obji].floatValue);
            break;

        case INSTANCE_NAME_TYPE:
        case GBL_VARIABLE:
        case SYMBOL_TYPE:
        case STRING_TYPE:
            ExpressionData(theEnv)->ExpressionArray[obji].value = SymbolData(theEnv)->SymbolArray[bexp->value];
            IncrementLexemeCount(ExpressionData(theEnv)->ExpressionArray[obji].lexemeValue);
            break;

#if DEFTEMPLATE_CONSTRUCT
        case FACT_ADDRESS_TYPE:
            ExpressionData(theEnv)->ExpressionArray[obji].value = &FactData(theEnv)->DummyFact;
            RetainFact((Fact *) ExpressionData(theEnv)->ExpressionArray[obji].value);
            break;
#endif

        case INSTANCE_ADDRESS_TYPE:
            ExpressionData(theEnv)->ExpressionArray[obji].value = &InstanceData(theEnv)->DummyInstance;
            RetainInstance((Instance *) ExpressionData(theEnv)->ExpressionArray[obji].value);
            break;

        case EXTERNAL_ADDRESS_TYPE:
            ExpressionData(theEnv)->ExpressionArray[obji].value = nullptr;
            break;

        case VOID_TYPE:
            break;

        default:
            if (EvaluationData(theEnv)->PrimitivesArray[bexp->type] == nullptr) break;
            if (EvaluationData(theEnv)->PrimitivesArray[bexp->type]->bitMap) {
                ExpressionData(theEnv)->ExpressionArray[obji].value = SymbolData(theEnv)->BitMapArray[bexp->value];
                IncrementBitMapCount((CLIPSBitMap *) ExpressionData(theEnv)->ExpressionArray[obji].value);
            }
            break;
    }

    theIndex = bexp->nextArg;
    if (theIndex == ULONG_MAX) { ExpressionData(theEnv)->ExpressionArray[obji].nextArg = nullptr; }
    else { ExpressionData(theEnv)->ExpressionArray[obji].nextArg = (expr *) &ExpressionData(theEnv)->ExpressionArray[theIndex]; }

    theIndex = bexp->argList;
    if (theIndex == ULONG_MAX) { ExpressionData(theEnv)->ExpressionArray[obji].argList = nullptr; }
    else { ExpressionData(theEnv)->ExpressionArray[obji].argList = (expr *) &ExpressionData(theEnv)->ExpressionArray[theIndex]; }
}

/*********************************************/
/* ClearBloadedExpressions: Clears the space */
/*   utilized by an expression binary image. */
/*********************************************/
void ClearBloadedExpressions(
        const Environment&theEnv) {
    unsigned long i;
    size_t space;

    /*===============================================*/
    /* Update the busy counts of atomic data values. */
    /*===============================================*/

    for (i = 0; i < ExpressionData(theEnv)->NumberOfExpressions; i++) {
        switch (ExpressionData(theEnv)->ExpressionArray[i].type) {
            case SYMBOL_TYPE          :
            case STRING_TYPE          :
            case INSTANCE_NAME_TYPE   :
            case GBL_VARIABLE    :
                ReleaseLexeme(theEnv, ExpressionData(theEnv)->ExpressionArray[i].lexemeValue);
                break;
            case FLOAT_TYPE           :
                ReleaseFloat(theEnv, ExpressionData(theEnv)->ExpressionArray[i].floatValue);
                break;
            case INTEGER_TYPE         :
                ReleaseInteger(theEnv, ExpressionData(theEnv)->ExpressionArray[i].integerValue);
                break;

#if DEFTEMPLATE_CONSTRUCT
            case FACT_ADDRESS_TYPE    :
                ReleaseFact((Fact *) ExpressionData(theEnv)->ExpressionArray[i].value);
                break;
#endif

            case INSTANCE_ADDRESS_TYPE :
                ReleaseInstance((Instance *) ExpressionData(theEnv)->ExpressionArray[i].value);
                break;
            case VOID_TYPE:
                break;

            default:
                if (EvaluationData(theEnv)->PrimitivesArray[ExpressionData(theEnv)->ExpressionArray[i].type] == nullptr) break;
                if (EvaluationData(theEnv)->PrimitivesArray[ExpressionData(
                        theEnv)->ExpressionArray[i].type]->bitMap) {
                    DecrementBitMapReferenceCount(theEnv, (CLIPSBitMap *) ExpressionData(
                            theEnv)->ExpressionArray[i].value);
                }
                break;
        }
    }

    /*===================================*/
    /* Free the binary expression array. */
    /*===================================*/

    space = ExpressionData(theEnv)->NumberOfExpressions * sizeof(expr);
    if (space != 0) genfree(theEnv, ExpressionData(theEnv)->ExpressionArray, space);
    ExpressionData(theEnv)->ExpressionArray = 0;
}

#if BLOAD_AND_BSAVE

/***************************************************
  NAME         : FindHashedExpressions
  DESCRIPTION  : Sets the bsave expression array
                 indices for hashed expression nodes
                 and marks the items needed by
                 these expressions
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Atoms marked and ids set
  NOTES        : None
 ***************************************************/
void FindHashedExpressions(
        const Environment&theEnv) {
    unsigned i;
    EXPRESSION_HN *exphash;

    for (i = 0; i < EXPRESSION_HASH_SIZE; i++)
        for (exphash = ExpressionData(theEnv)->ExpressionHashTable[i]; exphash != nullptr; exphash = exphash->next) {
            MarkNeededItems(theEnv, exphash->exp);
            exphash->bsaveID = ExpressionData(theEnv)->ExpressionCount;
            ExpressionData(theEnv)->ExpressionCount += ExpressionSize(exphash->exp);
        }
}

/***************************************************
  NAME         : BsaveHashedExpressions
  DESCRIPTION  : Writes out hashed expressions
  INPUTS       : Bsave file stream pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Expressions written
  NOTES        : None
 ***************************************************/
void BsaveHashedExpressions(
        const Environment&theEnv,
        FILE *fp) {
    unsigned i;
    EXPRESSION_HN *exphash;

    for (i = 0; i < EXPRESSION_HASH_SIZE; i++)
        for (exphash = ExpressionData(theEnv)->ExpressionHashTable[i]; exphash != nullptr; exphash = exphash->next)
            BsaveExpression(theEnv, exphash->exp, fp);
}

/***************************************************************/
/* BsaveConstructExpressions: Writes all expression needed by  */
/*   constructs for this binary image to the binary save file. */
/***************************************************************/
void BsaveConstructExpressions(
        const Environment&theEnv,
        FILE *fp) {
    struct BinaryItem *biPtr;

    for (biPtr = BsaveData(theEnv)->ListOfBinaryItems;
         biPtr != nullptr;
         biPtr = biPtr->next) {
        if (biPtr->expressionFunction != nullptr) { (*biPtr->expressionFunction)(theEnv, fp); }
    }
}

/***************************************/
/* BsaveExpression: Recursively saves  */
/*   an expression to the binary file. */
/***************************************/
void BsaveExpression(
        const Environment&theEnv,
        struct expr *testPtr,
        FILE *fp) {
    BSAVE_EXPRESSION newTest;
    unsigned long newIndex;

    while (testPtr != nullptr) {
        ExpressionData(theEnv)->ExpressionCount++;

        /*================*/
        /* Copy the type. */
        /*================*/

        newTest.type = testPtr->type;

        /*=======================================*/
        /* Convert the argList slot to an index. */
        /*=======================================*/

        if (testPtr->argList == nullptr) { newTest.argList = ULONG_MAX; }
        else { newTest.argList = ExpressionData(theEnv)->ExpressionCount; }

        /*========================================*/
        /* Convert the nextArg slot to an index. */
        /*========================================*/

        if (testPtr->nextArg == nullptr) { newTest.nextArg = ULONG_MAX; }
        else {
            newIndex = ExpressionData(theEnv)->ExpressionCount + ExpressionSize(testPtr->argList);
            newTest.nextArg = newIndex;
        }

        /*=========================*/
        /* Convert the value slot. */
        /*=========================*/

        switch (testPtr->type) {
            case FLOAT_TYPE:
                newTest.value = testPtr->floatValue->bucket;
                break;

            case INTEGER_TYPE:
                newTest.value = testPtr->integerValue->bucket;
                break;

            case FCALL:
                newTest.value = testPtr->functionValue->bsaveIndex;
                break;

            case GCALL:
#if DEFGENERIC_CONSTRUCT
                if (testPtr->value != nullptr)
                    newTest.value = testPtr->constructValue->bsaveID;
                else
#endif
                    newTest.value = ULONG_MAX;
                break;

            case PCALL:
#if DEFFUNCTION_CONSTRUCT
                if (testPtr->value != nullptr)
                    newTest.value = testPtr->constructValue->bsaveID;
                else
#endif
                    newTest.value = ULONG_MAX;
                break;

            case DEFTEMPLATE_PTR:
#if DEFTEMPLATE_CONSTRUCT
                if (testPtr->value != nullptr)
                    newTest.value = testPtr->constructValue->bsaveID;
                else
#endif
                    newTest.value = ULONG_MAX;
                break;

            case DEFCLASS_PTR:
                if (testPtr->value != nullptr)
                    newTest.value = testPtr->constructValue->bsaveID;
                else
                    newTest.value = ULONG_MAX;
                break;

            case DEFGLOBAL_PTR:
#if DEFGLOBAL_CONSTRUCT
                if (testPtr->value != nullptr)
                    newTest.value = testPtr->constructValue->bsaveID;
                else
#endif
                    newTest.value = ULONG_MAX;
                break;

            case INSTANCE_NAME_TYPE:
            case SYMBOL_TYPE:
            case GBL_VARIABLE:
            case STRING_TYPE:
                newTest.value = testPtr->lexemeValue->bucket;
                break;

            case FACT_ADDRESS_TYPE:
            case INSTANCE_ADDRESS_TYPE:
            case EXTERNAL_ADDRESS_TYPE:
                newTest.value = ULONG_MAX;
                break;

            case VOID_TYPE:
                break;

            default:
                if (EvaluationData(theEnv)->PrimitivesArray[testPtr->type] == nullptr) break;
                if (EvaluationData(
                        theEnv)->PrimitivesArray[testPtr->type]->bitMap) { newTest.value = ((CLIPSBitMap *) testPtr->value)->bucket; }
                break;
        }

        /*===========================*/
        /* Write out the expression. */
        /*===========================*/

        GenWrite(&newTest, sizeof(BSAVE_EXPRESSION), fp);

        /*==========================*/
        /* Write out argument list. */
        /*==========================*/

        if (testPtr->argList != nullptr) {
            BsaveExpression(theEnv, testPtr->argList, fp);
        }

        testPtr = testPtr->nextArg;
    }
}

#endif /* BLOAD_AND_BSAVE */

#endif /* (BLOAD_AND_BSAVE) */

