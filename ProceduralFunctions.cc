/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  10/18/16             */
/*                                                     */
/*             PROCEDURAL FUNCTIONS MODULE             */
/*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several procedural         */
/*   functions including if, while, loop-for-count, bind,    */
/*   progn, return, break, and switch                        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Local variables set with the bind function     */
/*            persist until a reset/clear command is issued. */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*      6.40: Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
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
/*            Added GCBlockStart and GCBlockEnd functions    */
/*            for garbage collection blocks.                 */
/*                                                           */
/*            Eval support for run time and bload only.      */
/*                                                           */
/*************************************************************/

#include <cstdio>

#include "Setup.h"

#include "ArgumentAccess.h"
#include "Constraint.h"
#include "Environment.h"
#include "Expression.h"
#include "MemoryAllocation.h"
#include "Multifield.h"
#include "ProceduralFunctionsParser.h"
#include "Router.h"
#include "Scanner.h"
#include "Utility.h"

#include "ProceduralFunctions.h"

#if DEFGLOBAL_CONSTRUCT
#include "Defglobal.h"
#include "ReferenceCounted.h"
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void DeallocateProceduralFunctionData(const Environment::Ptr&);

/**********************************************/
/* ProceduralFunctionDefinitions: Initializes */
/*   the procedural functions.                */
/**********************************************/
void ProceduralFunctionDefinitions(
        const Environment::Ptr&theEnv) {
    //AllocateEnvironmentData(theEnv, PRCDRFUN_DATA, sizeof(procedureFunctionData), DeallocateProceduralFunctionData);
    theEnv->allocateEnvironmentModule<procedureFunctionData>();
#if STUBBING_INACTIVE
    AddUDF(theEnv, "if", "*", 0, UNBOUNDED, nullptr, IfFunction);
    AddUDF(theEnv, "while", "*", 0, UNBOUNDED, nullptr, WhileFunction);
    AddUDF(theEnv, "loop-for-count", "*", 0, UNBOUNDED, nullptr, LoopForCountFunction);
    AddUDF(theEnv, "(get-loop-count)", "l", 1, 1, nullptr, GetLoopCount);
    AddUDF(theEnv, "bind", "*", 0, UNBOUNDED, nullptr, BindFunction);
    AddUDF(theEnv, "progn", "*", 0, UNBOUNDED, nullptr, PrognFunction);
    AddUDF(theEnv, "return", "*", 0, UNBOUNDED, nullptr, ReturnFunction);
    AddUDF(theEnv, "break", "v", 0, 0, nullptr, BreakFunction);
    AddUDF(theEnv, "switch", "*", 0, UNBOUNDED, nullptr, SwitchFunction);

    ProceduralFunctionParsers(theEnv);

    FuncSeqOvlFlags(theEnv, "progn", false, false);
    FuncSeqOvlFlags(theEnv, "if", false, false);
    FuncSeqOvlFlags(theEnv, "while", false, false);
    FuncSeqOvlFlags(theEnv, "loop-for-count", false, false);
    FuncSeqOvlFlags(theEnv, "return", false, false);
    FuncSeqOvlFlags(theEnv, "switch", false, false);

    AddResetFunction(theEnv, "bind", FlushBindList, 0, nullptr);
    AddClearFunction(theEnv, "bind", FlushBindList, 0, nullptr);
#endif
}
#if STUBBING_INACTIVE
/*************************************************************/
/* DeallocateProceduralFunctionData: Deallocates environment */
/*    data for procedural functions.                         */
/*************************************************************/
static void DeallocateProceduralFunctionData(
        const Environment::Ptr&theEnv) {
    UDFValue *nextPtr, *garbagePtr;

    garbagePtr = ProcedureFunctionData(theEnv)->BindList;

    while (garbagePtr != nullptr) {
        nextPtr = garbagePtr->next;
        rtn_struct(theEnv, UDFValue, garbagePtr);
        garbagePtr = nextPtr;
    }
}

/***************************************/
/* WhileFunction: H/L access routine   */
/*   for the while function.           */
/***************************************/
void WhileFunction(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UDFValue theResult;
    GCBlock gcb;

    /*====================================================*/
    /* Evaluate the body of the while loop as long as the */
    /* while condition evaluates to a non-FALSE value.    */
    /*====================================================*/

    GCBlockStart(theEnv, &gcb);

    UDFNthArgument(context, 1, ANY_TYPE_BITS, &theResult);
    while ((theResult.value != FalseSymbol(theEnv)) &&
           !EvaluationData(theEnv)->HaltExecution) {
        if (ProcedureFunctionData(theEnv)->BreakFlag || ProcedureFunctionData(theEnv)->ReturnFlag)
            break;

        UDFNthArgument(context, 2, ANY_TYPE_BITS, &theResult);

        if (ProcedureFunctionData(theEnv)->BreakFlag || ProcedureFunctionData(theEnv)->ReturnFlag)
            break;

        CleanCurrentGarbageFrame(theEnv, nullptr);
        CallPeriodicTasks(theEnv);

        UDFNthArgument(context, 1, ANY_TYPE_BITS, &theResult);
    }

    /*=====================================================*/
    /* Reset the break flag. The return flag is not reset  */
    /* because the while loop is probably contained within */
    /* a deffunction or RHS of a rule which needs to be    */
    /* returned from as well.                              */
    /*=====================================================*/

    ProcedureFunctionData(theEnv)->BreakFlag = false;

    /*====================================================*/
    /* If the return command was issued, then return that */
    /* value, otherwise return the symbol FALSE.          */
    /*====================================================*/

    if (ProcedureFunctionData(theEnv)->ReturnFlag) {
        returnValue->value = theResult.value;
        returnValue->begin = theResult.begin;
        returnValue->range = theResult.range;
    } else {
        returnValue->value = FalseSymbol(theEnv);
    }

    GCBlockEndUDF(theEnv, &gcb, returnValue);
    CallPeriodicTasks(theEnv);
}

/********************************************/
/* LoopForCountFunction: H/L access routine */
/*   for the loop-for-count function.       */
/********************************************/
void LoopForCountFunction(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *loopResult) {
    UDFValue theArg;
    long long iterationEnd;
    LOOP_COUNTER_STACK *tmpCounter;
    GCBlock gcb;

    tmpCounter = get_struct(theEnv, loopCounterStack);
    tmpCounter->loopCounter = 0L;
    tmpCounter->nxt = ProcedureFunctionData(theEnv)->LoopCounterStack;
    ProcedureFunctionData(theEnv)->LoopCounterStack = tmpCounter;

    if (!UDFNthArgument(context, 1, INTEGER_BIT, &theArg)) {
        loopResult->value = FalseSymbol(theEnv);
        ProcedureFunctionData(theEnv)->LoopCounterStack = tmpCounter->nxt;
        rtn_struct(theEnv, loopCounterStack, tmpCounter);
        return;
    }
    tmpCounter->loopCounter = theArg.integerValue->contents;
    if (!UDFNthArgument(context, 2, INTEGER_BIT, &theArg)) {
        loopResult->value = FalseSymbol(theEnv);
        ProcedureFunctionData(theEnv)->LoopCounterStack = tmpCounter->nxt;
        rtn_struct(theEnv, loopCounterStack, tmpCounter);
        return;
    }

    GCBlockStart(theEnv, &gcb);

    iterationEnd = theArg.integerValue->contents;
    while ((tmpCounter->loopCounter <= iterationEnd) &&
           !EvaluationData(theEnv)->HaltExecution) {
        if (ProcedureFunctionData(theEnv)->BreakFlag || ProcedureFunctionData(theEnv)->ReturnFlag)
            break;

        UDFNthArgument(context, 3, ANY_TYPE_BITS, &theArg);

        if (ProcedureFunctionData(theEnv)->BreakFlag || ProcedureFunctionData(theEnv)->ReturnFlag)
            break;

        CleanCurrentGarbageFrame(theEnv, nullptr);
        CallPeriodicTasks(theEnv);

        tmpCounter->loopCounter++;
    }

    ProcedureFunctionData(theEnv)->BreakFlag = false;
    if (ProcedureFunctionData(theEnv)->ReturnFlag) {
        loopResult->value = theArg.value;
        loopResult->begin = theArg.begin;
        loopResult->range = theArg.range;
    } else {
        loopResult->value = FalseSymbol(theEnv);
    }
    ProcedureFunctionData(theEnv)->LoopCounterStack = tmpCounter->nxt;
    rtn_struct(theEnv, loopCounterStack, tmpCounter);

    GCBlockEndUDF(theEnv, &gcb, loopResult);
    CallPeriodicTasks(theEnv);
}

/*****************/
/* GetLoopCount: */
/*****************/
void GetLoopCount(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    long long depth;
    UDFValue theArg;
    LOOP_COUNTER_STACK *tmpCounter;

    if (!UDFFirstArgument(context, INTEGER_BIT, &theArg)) { return; }
    depth = theArg.integerValue->contents;
    tmpCounter = ProcedureFunctionData(theEnv)->LoopCounterStack;
    while (depth > 0) {
        tmpCounter = tmpCounter->nxt;
        depth--;
    }

    returnValue->integerValue = CreateInteger(theEnv, tmpCounter->loopCounter);
}

/************************************/
/* IfFunction: H/L access routine   */
/*   for the if function.           */
/************************************/
void IfFunction(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    unsigned int numArgs;

    /*=========================*/
    /* Evaluate the condition. */
    /*=========================*/

    if (!UDFNthArgument(context, 1, ANY_TYPE_BITS, returnValue)) {
        returnValue->value = FalseSymbol(theEnv);
        return;
    }

    if (ProcedureFunctionData(theEnv)->BreakFlag ||
        ProcedureFunctionData(theEnv)->ReturnFlag) {
        returnValue->value = FalseSymbol(theEnv);
        return;
    }

    /*=========================================*/
    /* If the condition evaluated to FALSE and */
    /* an "else" portion exists, evaluate it   */
    /* and return the value.                   */
    /*=========================================*/

    numArgs = UDFArgumentCount(context);
    if ((returnValue->value == FalseSymbol(theEnv)) &&
        (numArgs == 3)) {
        UDFNthArgument(context, 3, ANY_TYPE_BITS, returnValue);
        return;
    }

        /*===================================================*/
        /* Otherwise if the symbol evaluated to a non-FALSE  */
        /* value, evaluate the "then" portion and return it. */
        /*===================================================*/

    else if (returnValue->value != FalseSymbol(theEnv)) {
        UDFNthArgument(context, 2, ANY_TYPE_BITS, returnValue);
        return;
    }

    /*=========================================*/
    /* Return FALSE if the condition evaluated */
    /* to FALSE and there is no "else" portion */
    /* of the if statement.                    */
    /*=========================================*/

    returnValue->value = FalseSymbol(theEnv);
}

/**************************************/
/* BindFunction: H/L access routine   */
/*   for the bind function.           */
/**************************************/
void BindFunction(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UDFValue *theBind, *lastBind;
    bool found = false,
            unbindVar = false;
    CLIPSLexeme *variableName = nullptr;
#if DEFGLOBAL_CONSTRUCT
    Defglobal *theGlobal = nullptr;
#endif

    /*===============================================*/
    /* Determine the name of the variable to be set. */
    /*===============================================*/

#if DEFGLOBAL_CONSTRUCT
    if (GetFirstArgument()->type == DEFGLOBAL_PTR) { theGlobal = (Defglobal *) GetFirstArgument()->value; }
    else
#endif
    {
        EvaluateExpression(theEnv, GetFirstArgument(), returnValue);
        variableName = returnValue->lexemeValue;
    }

    /*===========================================*/
    /* Determine the new value for the variable. */
    /*===========================================*/

    if (GetFirstArgument()->nextArg == nullptr) { unbindVar = true; }
    else if (GetFirstArgument()->nextArg->nextArg == nullptr) { EvaluateExpression(theEnv, GetFirstArgument()->nextArg, returnValue); }
    else { StoreInMultifield(theEnv, returnValue, GetFirstArgument()->nextArg, true); }

    /*==================================*/
    /* Bind a defglobal if appropriate. */
    /*==================================*/

#if DEFGLOBAL_CONSTRUCT
    if (theGlobal != nullptr) {
        QSetDefglobalValue(theEnv, theGlobal, returnValue, unbindVar);
        return;
    }
#endif

    /*===============================================*/
    /* Search for the variable in the list of binds. */
    /*===============================================*/

    theBind = ProcedureFunctionData(theEnv)->BindList;
    lastBind = nullptr;

    while ((theBind != nullptr) && !found) {
        if (theBind->supplementalInfo == (void *) variableName) { found = true; }
        else {
            lastBind = theBind;
            theBind = theBind->next;
        }
    }

    /*========================================================*/
    /* If variable was not in the list of binds, then add it. */
    /* Make sure that this operation preserves the bind list  */
    /* as a stack.                                            */
    /*========================================================*/

    if (!found) {
        if (!unbindVar) {
            theBind = get_struct(theEnv, UDFValue);
            theBind->supplementalInfo = (void *) variableName;
            IncrementLexemeCount(variableName);
            theBind->next = nullptr;
            if (lastBind == nullptr) { ProcedureFunctionData(theEnv)->BindList = theBind; }
            else { lastBind->next = theBind; }
        } else {
            returnValue->value = FalseSymbol(theEnv);
            return;
        }
    } else { ReleaseUDFV(theEnv, theBind); }

    /*================================*/
    /* Set the value of the variable. */
    /*================================*/

    if (!unbindVar) {
        theBind->value = returnValue->value;
        theBind->begin = returnValue->begin;
        theBind->range = returnValue->range;
        RetainUDFV(theEnv, returnValue);
    } else {
        if (lastBind == nullptr) ProcedureFunctionData(theEnv)->BindList = theBind->next;
        else lastBind->next = theBind->next;
        ReleaseLexeme(theEnv, (CLIPSLexeme *) theBind->supplementalInfo);
        rtn_struct(theEnv, UDFValue, theBind);
        returnValue->value = FalseSymbol(theEnv);
    }
}

/*******************************************/
/* GetBoundVariable: Searches the BindList */
/*   for a specified variable.             */
/*******************************************/
bool GetBoundVariable(
        const Environment::Ptr&theEnv,
        UDFValue *vPtr,
        CLIPSLexeme *varName) {
    UDFValue *bindPtr;

    for (bindPtr = ProcedureFunctionData(theEnv)->BindList; bindPtr != nullptr; bindPtr = bindPtr->next) {
        if (bindPtr->supplementalInfo == (void *) varName) {
            vPtr->value = bindPtr->value;
            vPtr->begin = bindPtr->begin;
            vPtr->range = bindPtr->range;
            return true;
        }
    }

    return false;
}
#endif
/*************************************************/
/* FlushBindList: Removes all variables from the */
/*   list of currently bound local variables.    */
/*************************************************/
void FlushBindList(
        const Environment::Ptr&theEnv,
        void *context) {
    ReturnValues(theEnv, ProcedureFunctionData(theEnv)->BindList, true);
    ProcedureFunctionData(theEnv)->BindList = nullptr;
}
#if STUBBING_INACTIVE

/***************************************/
/* PrognFunction: H/L access routine   */
/*   for the progn function.           */
/***************************************/
void PrognFunction(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    Expression *argPtr;

    argPtr = EvaluationData(theEnv)->CurrentExpression->argList;

    if (argPtr == nullptr) {
        returnValue->value = FalseSymbol(theEnv);
        return;
    }

    while ((argPtr != nullptr) && !GetHaltExecution(theEnv)) {
        EvaluateExpression(theEnv, argPtr, returnValue);

        if (ProcedureFunctionData(theEnv)->BreakFlag || ProcedureFunctionData(theEnv)->ReturnFlag)
            break;
        argPtr = argPtr->nextArg;
    }

    if (GetHaltExecution(theEnv)) {
        returnValue->value = FalseSymbol(theEnv);
        return;
    }

    return;
}

/***************************************************************/
/* ReturnFunction: H/L access routine for the return function. */
/***************************************************************/
void ReturnFunction(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    if (!UDFHasNextArgument(context)) {
        returnValue->voidValue = VoidConstant(theEnv);
    } else { UDFNextArgument(context, ANY_TYPE_BITS, returnValue); }
    ProcedureFunctionData(theEnv)->ReturnFlag = true;
}

/***************************************************************/
/* BreakFunction: H/L access routine for the break function.   */
/***************************************************************/
void BreakFunction(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    ProcedureFunctionData(theEnv)->BreakFlag = true;
}

/*****************************************************************/
/* SwitchFunction: H/L access routine for the switch function.   */
/*****************************************************************/
void SwitchFunction(
        const Environment::Ptr&theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    UDFValue switch_val, case_val;
    Expression *theExp;

    returnValue->lexemeValue = FalseSymbol(theEnv);

    /* ==========================
       Get the value to switch on
       ========================== */
    EvaluateExpression(theEnv, GetFirstArgument(), &switch_val);
    if (EvaluationData(theEnv)->EvaluationError)
        return;
    for (theExp = GetFirstArgument()->nextArg; theExp != nullptr; theExp = theExp->nextArg->nextArg) {
        /* =================================================
           VOID_TYPE is the default case (if any) for the switch
           ================================================= */
        if (theExp->type == VOID_TYPE) {
            EvaluateExpression(theEnv, theExp->nextArg, returnValue);
            return;
        }

        /* ====================================================
           If the case matches, evaluate the actions and return
           ==================================================== */
        EvaluateExpression(theEnv, theExp, &case_val);
        if (EvaluationData(theEnv)->EvaluationError)
            return;
        if (switch_val.header->type == case_val.header->type) {
            if ((case_val.header->type == MULTIFIELD_TYPE) ? MultifieldDOsEqual(&switch_val, &case_val) :
                (switch_val.value == case_val.value)) {
                EvaluateExpression(theEnv, theExp->nextArg, returnValue);
                return;
            }
        }
    }
}

#endif



