/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  05/29/19             */
/*                                                     */
/*             EXPRESSION OPERATIONS MODULE            */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides utility routines for manipulating and   */
/*   examining expressions.                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Add NegateExpression function.                 */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.31: Removed FACT_ADDRESS type from                 */
/*            ExpressionContainsVariables function.          */
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

#include "Setup.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctype.h>

#include "ConstraintChecking.h"
#include "ConstraintOperations.h"
#include "ConstraintUtilities.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "MemoryAllocation.h"
#include "PrintUtility.h"
#include "Router.h"

#include "Expression.h"

/************************************/
/* CheckArgumentAgainstRestriction: */
/************************************/
bool CheckArgumentAgainstRestriction(
        const Environment&theEnv,
        Expression *theExpression,
        unsigned theRestriction) {
    CONSTRAINT_RECORD *cr1, *cr2, *cr3;

    /*=============================================*/
    /* Generate a constraint record for the actual */
    /* argument passed to the function.            */
    /*=============================================*/

    cr1 = ExpressionToConstraintRecord(theEnv, theExpression);

    /*================================================*/
    /* Generate a constraint record based on the type */
    /* of argument expected by the function.          */
    /*================================================*/

    cr2 = ArgumentTypeToConstraintRecord(theEnv, theRestriction);

    /*===============================================*/
    /* Intersect the two constraint records and then */
    /* discard them.                                 */
    /*===============================================*/

    cr3 = IntersectConstraints(theEnv, cr1, cr2);

    RemoveConstraint(theEnv, cr1);
    RemoveConstraint(theEnv, cr2);

    /*====================================================*/
    /* If the intersection of the two constraint records  */
    /* is empty, then the argument passed to the function */
    /* doesn't satisfy the restrictions for the argument. */
    /*====================================================*/

    if (UnmatchableConstraint(cr3)) {
        RemoveConstraint(theEnv, cr3);
        return true;
    }

    /*===================================================*/
    /* The argument satisfies the function restrictions. */
    /*===================================================*/

    RemoveConstraint(theEnv, cr3);
    return false;
}
#if 0
/******************************************************/
/* ConstantExpression: Returns true if the expression */
/*   is a constant, otherwise false.                  */
/******************************************************/
bool ConstantExpression(
        Expression *testPtr) {
    while (testPtr != nullptr) {
        if ((testPtr->type != SYMBOL_TYPE) && (testPtr->type != STRING_TYPE) &&
            (testPtr->type != INSTANCE_NAME_TYPE) && (testPtr->type != INSTANCE_ADDRESS_TYPE) &&
            (testPtr->type != INTEGER_TYPE) && (testPtr->type != FLOAT_TYPE)) { return false; }
        testPtr = testPtr->nextArg;
    }

    return true;
}
#endif

/******************************************/
/* ConstantType: Returns true if the type */
/*   is a constant, otherwise false.      */
/******************************************/
bool ConstantType(
        int theType) {
    switch (theType) {
        case SYMBOL_TYPE:
        case STRING_TYPE:
        case INTEGER_TYPE:
        case FLOAT_TYPE:
        case INSTANCE_NAME_TYPE:
        case INSTANCE_ADDRESS_TYPE:
            return true;
    }

    return false;
}
/*****************************************************************************/
/* IdenticalExpression: Determines if two expressions are identical. Returns */
/*   true if the expressions are identical, otherwise false is returned.     */
/*****************************************************************************/
bool IdenticalExpression(
        Expression *firstList,
        Expression *secondList) {
#if 0
    /*==============================================*/
    /* Compare each argument in both expressions by */
    /* following the nextArg list.                  */
    /*==============================================*/

    for (;
            (firstList != nullptr) && (secondList != nullptr);
            firstList = firstList->nextArg, secondList = secondList->nextArg) {
        /*=========================*/
        /* Compare type and value. */
        /*=========================*/

        if (firstList->type != secondList->type) { return false; }

        if (firstList->value != secondList->value) { return false; }

        /*==============================*/
        /* Compare the arguments lists. */
        /*==============================*/

        if (!IdenticalExpression(firstList->argList, secondList->argList)) { return false; }
    }

    /*=====================================================*/
    /* If firstList and secondList aren't both nullptr, then  */
    /* one of the lists contains more expressions than the */
    /* other.                                              */
    /*=====================================================*/

    return firstList == secondList;

    /*============================*/
    /* Expressions are identical. */
    /*============================*/
#endif
    return false;

}
#if 0

/****************************************************/
/* CountArguments: Returns the number of structures */
/*   stored in an expression as traversed through   */
/*   the nextArg pointer but not the argList        */
/*   pointer.                                       */
/****************************************************/
unsigned short CountArguments(
        Expression *testPtr) {
    unsigned short size = 0;

    while (testPtr != nullptr) {
        size++;
        testPtr = testPtr->nextArg;
    }

    return size;
}

/******************************************/
/* CopyExpresssion: Copies an expression. */
/******************************************/
Expression *CopyExpression(
        const Environment&theEnv,
        Expression *original) {
    Expression *topLevel, *next, *last;

    if (original == nullptr) return nullptr;

    topLevel = GenConstant(theEnv, original->type, original->value);
    topLevel->argList = CopyExpression(theEnv, original->argList);

    last = topLevel;
    original = original->nextArg;
    while (original != nullptr) {
        next = GenConstant(theEnv, original->type, original->value);
        next->argList = CopyExpression(theEnv, original->argList);

        last->nextArg = next;
        last = next;
        original = original->nextArg;
    }

    return (topLevel);
}

/************************************************************/
/* ExpressionContainsVariables: Determines if an expression */
/*   contains any variables. Returns true if the expression */
/*   contains any variables, otherwise false is returned.   */
/************************************************************/
bool ExpressionContainsVariables(
        Expression *theExpression,
        bool globalsAreVariables) {
    while (theExpression != nullptr) {
        if (theExpression->argList != nullptr) {
            if (ExpressionContainsVariables(theExpression->argList, globalsAreVariables)) { return true; }
        }

        if ((theExpression->type == MF_VARIABLE) ||
            (theExpression->type == SF_VARIABLE) ||
            (((theExpression->type == GBL_VARIABLE) ||
              (theExpression->type == MF_GBL_VARIABLE)) &&
             globalsAreVariables)) { return true; }

        theExpression = theExpression->nextArg;
    }

    return false;
}
#endif
/*****************************************/
/* ExpressionSize: Returns the number of */
/*   structures stored in an expression. */
/*****************************************/
unsigned long ExpressionSize(
        Expression *testPtr) {
    unsigned long size = 0;
#if 0

    while (testPtr != nullptr) {
        size++;
        if (testPtr->argList != nullptr) { size += ExpressionSize(testPtr->argList); }
        testPtr = testPtr->nextArg;
    }
#endif
    return size;
}
#if 0

/************************************************/
/* GenConstant: Generates a constant expression */
/*   value of type string, symbol, or number.   */
/************************************************/
Expression *GenConstant(
        const Environment&theEnv,
        unsigned short type,
        void *value) {
    Expression *top;

    top = get_struct(theEnv, Expression);
    top->nextArg = nullptr;
    top->argList = nullptr;
    top->type = type;
    top->value = value;

    return top;
}

/*************************************************/
/* PrintExpression: Pretty prints an expression. */
/*************************************************/
void PrintExpression(
        const Environment&theEnv,
        const char *fileid,
        Expression *theExpression) {
    Expression *oldExpression;

    if (theExpression == nullptr) { return; }

    while (theExpression != nullptr) {
        switch (theExpression->type) {
            case SF_VARIABLE:
            case GBL_VARIABLE:
                WriteString(theEnv, fileid, "?");
                WriteString(theEnv, fileid, theExpression->lexemeValue->contents);
                break;

            case MF_VARIABLE:
            case MF_GBL_VARIABLE:
                WriteString(theEnv, fileid, "$?");
                WriteString(theEnv, fileid, theExpression->lexemeValue->contents);
                break;

            case FCALL:
                WriteString(theEnv, fileid, "(");
                WriteString(theEnv, fileid, ExpressionFunctionCallName(theExpression)->contents);
                if (theExpression->argList != nullptr) { WriteString(theEnv, fileid, " "); }
                PrintExpression(theEnv, fileid, theExpression->argList);
                WriteString(theEnv, fileid, ")");
                break;

            default:
                oldExpression = EvaluationData(theEnv)->CurrentExpression;
                EvaluationData(theEnv)->CurrentExpression = theExpression;
                PrintAtom(theEnv, fileid, theExpression->type, theExpression->value);
                EvaluationData(theEnv)->CurrentExpression = oldExpression;
                break;
        }

        theExpression = theExpression->nextArg;
        if (theExpression != nullptr) WriteString(theEnv, fileid, " ");
    }

}

/*************************************************************************/
/* CombineExpressions: Combines two expressions into a single equivalent */
/*   expression. Mainly serves to merge expressions containing "and"     */
/*   and "or" expressions without unnecessary duplication of the "and"   */
/*   and "or" expressions (i.e., two "and" expressions can be merged by  */
/*   placing them as arguments within another "and" expression, but it   */
/*   is more efficient to add the arguments of one of the "and"          */
/*   expressions to the list of arguments for the other and expression). */
/*************************************************************************/
Expression *CombineExpressions(
        const Environment&theEnv,
        Expression *expr1,
        Expression *expr2) {
    Expression *tempPtr;

    /*===========================================================*/
    /* If the 1st expression is nullptr, return the 2nd expression. */
    /*===========================================================*/

    if (expr1 == nullptr) return (expr2);

    /*===========================================================*/
    /* If the 2nd expression is nullptr, return the 1st expression. */
    /*===========================================================*/

    if (expr2 == nullptr) return (expr1);

    /*============================================================*/
    /* If the 1st expression is an "and" expression, and the 2nd  */
    /* expression is not an "and" expression, then include the    */
    /* 2nd expression in the argument list of the 1st expression. */
    /*============================================================*/

    if ((expr1->value == ExpressionData(theEnv)->PTR_AND) &&
        (expr2->value != ExpressionData(theEnv)->PTR_AND)) {
        tempPtr = expr1->argList;
        if (tempPtr == nullptr) {
            rtn_struct(theEnv, Expression, expr1);
            return (expr2);
        }

        while (tempPtr->nextArg != nullptr) { tempPtr = tempPtr->nextArg; }

        tempPtr->nextArg = expr2;
        return (expr1);
    }

    /*============================================================*/
    /* If the 2nd expression is an "and" expression, and the 1st  */
    /* expression is not an "and" expression, then include the    */
    /* 1st expression in the argument list of the 2nd expression. */
    /*============================================================*/

    if ((expr1->value != ExpressionData(theEnv)->PTR_AND) &&
        (expr2->value == ExpressionData(theEnv)->PTR_AND)) {
        tempPtr = expr2->argList;
        if (tempPtr == nullptr) {
            rtn_struct(theEnv, Expression, expr2);
            return (expr1);
        }

        expr2->argList = expr1;
        expr1->nextArg = tempPtr;

        return (expr2);
    }

    /*===========================================================*/
    /* If both expressions are "and" expressions, then add the   */
    /* 2nd expression to the argument list of the 1st expression */
    /* and throw away the extraneous "and" expression.           */
    /*===========================================================*/

    if ((expr1->value == ExpressionData(theEnv)->PTR_AND) &&
        (expr2->value == ExpressionData(theEnv)->PTR_AND)) {
        tempPtr = expr1->argList;
        if (tempPtr == nullptr) {
            rtn_struct(theEnv, Expression, expr1);
            return (expr2);
        }

        while (tempPtr->nextArg != nullptr) { tempPtr = tempPtr->nextArg; }

        tempPtr->nextArg = expr2->argList;
        rtn_struct(theEnv, Expression, expr2);

        return (expr1);
    }

    /*=====================================================*/
    /* If neither expression is an "and" expression, then  */
    /* create an "and" expression and add both expressions */
    /* to the argument list of that "and" expression.      */
    /*=====================================================*/

    tempPtr = GenConstant(theEnv, FCALL, ExpressionData(theEnv)->PTR_AND);
    tempPtr->argList = expr1;
    expr1->nextArg = expr2;
    return (tempPtr);
}

/*********************/
/* NegateExpression: */
/*********************/
Expression *NegateExpression(
        const Environment&theEnv,
        Expression *theExpression) {
    Expression *tempPtr;

    /*=========================================*/
    /* If the expression is nullptr, return nullptr. */
    /*=========================================*/

    if (theExpression == nullptr) return nullptr;

    /*==================================================*/
    /* The expression is already wrapped within a "not" */
    /* function call, just remove the function call.    */
    /*==================================================*/

    if (theExpression->value == ExpressionData(theEnv)->PTR_NOT) {
        tempPtr = theExpression->argList;
        rtn_struct(theEnv, Expression, theExpression);
        return (tempPtr);
    }

    /*===================================================*/
    /* Wrap the expression within a "not" function call. */
    /*===================================================*/

    tempPtr = GenConstant(theEnv, FCALL, ExpressionData(theEnv)->PTR_NOT);
    tempPtr->argList = theExpression;

    return (tempPtr);
}

/********************************************************/
/* AppendExpressions: Attaches an expression to the end */
/*   of another expression's nextArg list.              */
/********************************************************/
Expression *AppendExpressions(
        Expression *expr1,
        Expression *expr2) {
    Expression *tempPtr;

    /*===========================================================*/
    /* If the 1st expression is nullptr, return the 2nd expression. */
    /*===========================================================*/

    if (expr1 == nullptr) return (expr2);

    /*===========================================================*/
    /* If the 2nd expression is nullptr, return the 1st expression. */
    /*===========================================================*/

    if (expr2 == nullptr) return (expr1);

    /*====================================*/
    /* Find the end of the 1st expression */
    /* and attach the 2nd expression.     */
    /*====================================*/

    tempPtr = expr1;
    while (tempPtr->nextArg != nullptr) tempPtr = tempPtr->nextArg;
    tempPtr->nextArg = expr2;

    /*===============================*/
    /* Return the merged expression. */
    /*===============================*/

    return (expr1);
}
#endif

