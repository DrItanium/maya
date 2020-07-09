/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  11/01/16             */
/*                                                     */
/*                  EXPRESSION MODULE                  */
/*******************************************************/

/*************************************************************/
/* Purpose: Contains routines for creating, deleting,        */
/*   compacting, installing, and hashing expressions.        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Corrected link errors with non-default         */
/*            setup.h configuration settings.                */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Changed expression hashing value.              */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            Eval support for run time and bload only.      */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#include <cstdio>
#include <stdlib.h>
#include <cstring>
#include <ctype.h>

#include "BinaryLoad.h"
#include "Environment.h"
#include "Evaluation.h"
#include "ExternalFunctions.h"
#include "MemoryAllocation.h"
#include "PrintUtility.h"
#include "Router.h"

#include "Expression.h"

#define PRIME_ONE   257
#define PRIME_TWO   263
#define PRIME_THREE 269

/****************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS  */
/****************************************/

static unsigned long ListToPacked(expr *, struct expr *, unsigned long);
static EXPRESSION_HN *FindHashedExpression(Environment *, Expression *, unsigned *, EXPRESSION_HN **);
static unsigned HashExpression(Expression *);
static void DeallocateExpressionData(Environment *);

/**************************************************/
/* InitExpressionData: Initializes the function   */
/*   pointers used in generating some expressions */
/*   and the expression hash table.               */
/**************************************************/
void InitExpressionData(
        Environment *theEnv) {
    unsigned i;

    AllocateEnvironmentData(theEnv, EXPRESSION_DATA, sizeof(expressionData), DeallocateExpressionData);

    InitExpressionPointers(theEnv);

    ExpressionData(theEnv)->ExpressionHashTable = (EXPRESSION_HN **)
            gm2(theEnv, sizeof(EXPRESSION_HN *) * EXPRESSION_HASH_SIZE);
    for (i = 0; i < EXPRESSION_HASH_SIZE; i++)
        ExpressionData(theEnv)->ExpressionHashTable[i] = nullptr;
}

/*****************************************/
/* DeallocateExpressionData: Deallocates */
/*    environment data for expressions.  */
/*****************************************/
static void DeallocateExpressionData(
        Environment *theEnv) {
    int i;
    EXPRESSION_HN *tmpPtr, *nextPtr;

#if (BLOAD_AND_BSAVE)
    if (!Bloaded(theEnv))
#endif
    {
        for (i = 0; i < EXPRESSION_HASH_SIZE; i++) {
            tmpPtr = ExpressionData(theEnv)->ExpressionHashTable[i];
            while (tmpPtr != nullptr) {
                nextPtr = tmpPtr->next;
                ReturnPackedExpression(theEnv, tmpPtr->exp);
                rtn_struct(theEnv, exprHashNode, tmpPtr);
                tmpPtr = nextPtr;
            }
        }
    }

    rm(theEnv, ExpressionData(theEnv)->ExpressionHashTable,
       sizeof(EXPRESSION_HN *) * EXPRESSION_HASH_SIZE);

#if (BLOAD_AND_BSAVE)
    if ((ExpressionData(theEnv)->NumberOfExpressions != 0) && Bloaded(theEnv)) {
        genfree(theEnv, ExpressionData(theEnv)->ExpressionArray,
                ExpressionData(theEnv)->NumberOfExpressions * sizeof(expr));
    }
#endif
}

/****************************************************/
/* InitExpressionPointers: Initializes the function */
/*   pointers used in generating some expressions.  */
/****************************************************/
void InitExpressionPointers(
        Environment *theEnv) {
    ExpressionData(theEnv)->PTR_AND = FindFunction(theEnv, "and");
    ExpressionData(theEnv)->PTR_OR = FindFunction(theEnv, "or");
    ExpressionData(theEnv)->PTR_EQ = FindFunction(theEnv, "eq");
    ExpressionData(theEnv)->PTR_NEQ = FindFunction(theEnv, "neq");
    ExpressionData(theEnv)->PTR_NOT = FindFunction(theEnv, "not");

    if ((ExpressionData(theEnv)->PTR_AND == nullptr) || (ExpressionData(theEnv)->PTR_OR == nullptr) ||
        (ExpressionData(theEnv)->PTR_EQ == nullptr) || (ExpressionData(theEnv)->PTR_NEQ == nullptr) ||
        (ExpressionData(theEnv)->PTR_NOT == nullptr)) {
        SystemError(theEnv, "EXPRESSN", 1);
        ExitRouter(theEnv, EXIT_FAILURE);
    }
}

/***************************************************/
/* ExpressionInstall: Increments the busy count of */
/*   atomic data values found in an expression.    */
/***************************************************/
void ExpressionInstall(
        Environment *theEnv,
        struct expr *expression) {
    if (expression == nullptr) return;

    while (expression != nullptr) {
        AtomInstall(theEnv, expression->type, expression->value);
        ExpressionInstall(theEnv, expression->argList);
        expression = expression->nextArg;
    }
}

/*****************************************************/
/* ExpressionDeinstall: Decrements the busy count of */
/*   atomic data values found in an expression.      */
/*****************************************************/
void ExpressionDeinstall(
        Environment *theEnv,
        struct expr *expression) {
    if (expression == nullptr) return;

    while (expression != nullptr) {
        AtomDeinstall(theEnv, expression->type, expression->value);
        ExpressionDeinstall(theEnv, expression->argList);
        expression = expression->nextArg;
    }
}


/***********************************************************************/
/* PackExpression: Copies an expression (created using multiple memory */
/*   requests) into an array (created using a single memory request)   */
/*   while maintaining all appropriate links in the expression. A      */
/*   packed expression requires less total memory because it reduces   */
/*   the overhead required for multiple memory allocations.            */
/***********************************************************************/
struct expr *PackExpression(
        Environment *theEnv,
        struct expr *original) {
    struct expr *packPtr;

    if (original == nullptr) return nullptr;

    packPtr = (expr *)
            gm2(theEnv, sizeof(expr) * ExpressionSize(original));
    ListToPacked(original, packPtr, 0);

    return packPtr;
}

/***********************************************************/
/* ListToPacked: Copies a list of expressions to an array. */
/***********************************************************/
static unsigned long ListToPacked(
        struct expr *original,
        struct expr *destination,
        unsigned long count) {
    unsigned long i;

    if (original == nullptr) { return count; }

    while (original != nullptr) {
        i = count;
        count++;

        destination[i].type = original->type;
        destination[i].value = original->value;

        if (original->argList == nullptr) { destination[i].argList = nullptr; }
        else {
            destination[i].argList =
                    (expr *) &destination[count];
            count = ListToPacked(original->argList, destination, count);
        }

        if (original->nextArg == nullptr) { destination[i].nextArg = nullptr; }
        else {
            destination[i].nextArg = &destination[count];
        }

        original = original->nextArg;
    }

    return count;
}


/***************************************************************/
/* ReturnPackedExpression: Returns a packed expression created */
/*   using PackExpression to the memory manager.               */
/***************************************************************/
void ReturnPackedExpression(
        Environment *theEnv,
        struct expr *packPtr) {
    if (packPtr != nullptr) {
        rm(theEnv, packPtr, sizeof(expr) * ExpressionSize(packPtr));
    }
}

/***********************************************/
/* ReturnExpression: Returns a multiply linked */
/*   list of expr data structures.             */
/***********************************************/
void ReturnExpression(
        Environment *theEnv,
        struct expr *waste) {
    struct expr *tmp;

    while (waste != nullptr) {
        if (waste->argList != nullptr) ReturnExpression(theEnv, waste->argList);
        tmp = waste;
        waste = waste->nextArg;
        rtn_struct(theEnv, expr, tmp);
    }
}

/***************************************************
  NAME         : FindHashedExpression
  DESCRIPTION  : Determines if a given expression
                 is in the expression hash table
  INPUTS       : 1) The expression
                 2) A buffer to hold the hash
                    value
                 3) A buffer to hold the previous
                    node in the hash chain
  RETURNS      : The expression hash table entry
                 (nullptr if not found)
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static EXPRESSION_HN *FindHashedExpression(
        Environment *theEnv,
        Expression *theExp,
        unsigned *hashval,
        EXPRESSION_HN **prv) {
    EXPRESSION_HN *exphash;

    if (theExp == nullptr)
        return nullptr;
    *hashval = HashExpression(theExp);
    *prv = nullptr;
    exphash = ExpressionData(theEnv)->ExpressionHashTable[*hashval];
    while (exphash != nullptr) {
        if (IdenticalExpression(exphash->exp, theExp))
            return (exphash);
        *prv = exphash;
        exphash = exphash->next;
    }
    return nullptr;
}

/***************************************************
  NAME         : HashExpression
  DESCRIPTION  : Assigns a deterministic number to
                 an expression
  INPUTS       : The expression
  RETURNS      : The "value" of the expression
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static unsigned HashExpression(
        Expression *theExp) {
    unsigned long tally = PRIME_THREE;
    union {
        void *vv;
        unsigned long uv;
    } fis;

    if (theExp->argList != nullptr)
        tally += HashExpression(theExp->argList) * PRIME_ONE;
    while (theExp != nullptr) {
        tally += theExp->type * PRIME_TWO;
        fis.uv = 0;
        fis.vv = theExp->value;
        tally += fis.uv;
        theExp = theExp->nextArg;
    }
    return (unsigned) (tally % EXPRESSION_HASH_SIZE);
}

/***************************************************
  NAME         : RemoveHashedExpression
  DESCRIPTION  : Removes a hashed expression from
                 the hash table
  INPUTS       : The expression
  RETURNS      : Nothing useful
  SIDE EFFECTS : Hash node removed (or use count
                 decremented).  If the hash node
                 is removed, the expression is
                 deinstalled and deleted
  NOTES        : If the expression is in use by
                 others, then the use count is
                 merely decremented
 ***************************************************/
void RemoveHashedExpression(
        Environment *theEnv,
        Expression *theExp) {
    EXPRESSION_HN *exphash, *prv;
    unsigned hashval;

    exphash = FindHashedExpression(theEnv, theExp, &hashval, &prv);
    if (exphash == nullptr)
        return;
    if (--exphash->count != 0)
        return;
    if (prv == nullptr)
        ExpressionData(theEnv)->ExpressionHashTable[hashval] = exphash->next;
    else
        prv->next = exphash->next;
    ExpressionDeinstall(theEnv, exphash->exp);
    ReturnPackedExpression(theEnv, exphash->exp);
    rtn_struct(theEnv, exprHashNode, exphash);
}

/*****************************************************
  NAME         : AddHashedExpression
  DESCRIPTION  : Adds a new expression to the
                 expression hash table (or increments
                 the use count if it is already there)
  INPUTS       : The (new) expression
  RETURNS      : A pointer to the (new) hash node
  SIDE EFFECTS : Adds the new hash node or increments
                 the count of an existing one
  NOTES        : It is the caller's responsibility to
                 delete the passed expression.  This
                 routine copies, packs and installs
                 the given expression
 *****************************************************/
Expression *AddHashedExpression(
        Environment *theEnv,
        Expression *theExp) {
    EXPRESSION_HN *prv, *exphash;
    unsigned hashval;

    if (theExp == nullptr) return nullptr;
    exphash = FindHashedExpression(theEnv, theExp, &hashval, &prv);
    if (exphash != nullptr) {
        exphash->count++;
        return (exphash->exp);
    }
    exphash = get_struct(theEnv, exprHashNode);
    exphash->hashval = hashval;
    exphash->count = 1;
    exphash->exp = PackExpression(theEnv, theExp);
    ExpressionInstall(theEnv, exphash->exp);
    exphash->next = ExpressionData(theEnv)->ExpressionHashTable[exphash->hashval];
    ExpressionData(theEnv)->ExpressionHashTable[exphash->hashval] = exphash;
    exphash->bsaveID = 0L;
    return (exphash->exp);
}

#if (BLOAD_AND_BSAVE)

/***************************************************
  NAME         : HashedExpressionIndex
  DESCRIPTION  : Finds the expression bload array
                 index for a hashed expression
  INPUTS       : The expression
  RETURNS      : The bload index
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
unsigned long HashedExpressionIndex(
        Environment *theEnv,
        Expression *theExp) {
    EXPRESSION_HN *exphash, *prv;
    unsigned hashval;

    if (theExp == nullptr)
        return ULONG_MAX;
    exphash = FindHashedExpression(theEnv, theExp, &hashval, &prv);
    return ((exphash != nullptr) ? exphash->bsaveID : ULONG_MAX);
}

#endif /* (BLOAD_AND_BSAVE) */

/********************************************************/
/* SetSequenceOperatorRecognition: C access routine     */
/*   for the set-sequence-operator-recognition function */
/********************************************************/
bool SetSequenceOperatorRecognition(
        Environment *theEnv,
        bool value) {
    bool ov;

    ov = ExpressionData(theEnv)->SequenceOpMode;
    ExpressionData(theEnv)->SequenceOpMode = value;
    return ov;
}

/********************************************************/
/* GetSequenceOperatorRecognition: C access routine     */
/*   for the Get-sequence-operator-recognition function */
/********************************************************/
bool GetSequenceOperatorRecognition(
        Environment *theEnv) {
    return ExpressionData(theEnv)->SequenceOpMode;
}

