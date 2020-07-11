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
#include <cstdlib>
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

constexpr auto PRIME_ONE   = 257;
constexpr auto PRIME_TWO   = 263;
constexpr auto PRIME_THREE = 269;

/****************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS  */
/****************************************/

static unsigned long ListToPacked(Expression *, Expression *, unsigned long);
static ExpressionHashNode *FindHashedExpression(const Environment&, Expression *, unsigned *, ExpressionHashNode **);
static unsigned HashExpression(Expression *);
static void DeallocateExpressionData(const Environment&);

/**************************************************/
/* InitExpressionData: Initializes the function   */
/*   pointers used in generating some expressions */
/*   and the expression hash table.               */
/**************************************************/
void InitExpressionData(
        const Environment&theEnv) {
    unsigned i;
    AllocateEnvironmentData(theEnv, EXPRESSION_DATA, sizeof(ExpressionData), DeallocateExpressionData);

    InitExpressionPointers(theEnv);

    ExpressionData(theEnv)->ExpressionHashTable = (ExpressionHashNode **)
            gm2(theEnv, sizeof(ExpressionHashNode *) * EXPRESSION_HASH_SIZE);
    for (i = 0; i < EXPRESSION_HASH_SIZE; i++)
        ExpressionData(theEnv)->ExpressionHashTable[i] = nullptr;
}

/*****************************************/
/* DeallocateExpressionData: Deallocates */
/*    environment data for expressions.  */
/*****************************************/
static void DeallocateExpressionData(
        const Environment&theEnv) {
    int i;
    ExpressionHashNode *tmpPtr, *nextPtr;

#if (BLOAD_AND_BSAVE)
    if (!Bloaded(theEnv))
#endif
    {
        for (i = 0; i < EXPRESSION_HASH_SIZE; i++) {
            tmpPtr = ExpressionData(theEnv)->ExpressionHashTable[i];
            while (tmpPtr != nullptr) {
                nextPtr = tmpPtr->next;
                ReturnPackedExpression(theEnv, tmpPtr->exp);
                rtn_struct(theEnv, ExpressionHashNode, tmpPtr);
                tmpPtr = nextPtr;
            }
        }
    }

    rm(theEnv, ExpressionData(theEnv)->ExpressionHashTable,
       sizeof(ExpressionHashNode *) * EXPRESSION_HASH_SIZE);

#if (BLOAD_AND_BSAVE)
    if ((ExpressionData(theEnv)->NumberOfExpressions != 0) && Bloaded(theEnv)) {
        genfree(theEnv, ExpressionData(theEnv)->ExpressionArray,
                ExpressionData(theEnv)->NumberOfExpressions * sizeof(Expression));
    }
#endif
}

/****************************************************/
/* InitExpressionPointers: Initializes the function */
/*   pointers used in generating some expressions.  */
/****************************************************/
void InitExpressionPointers(
        const Environment&theEnv) {
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
        const Environment&theEnv,
        Expression *expression) {
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
        const Environment&theEnv,
        Expression *expression) {
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
Expression *PackExpression(
        const Environment&theEnv,
        Expression *original) {
    Expression *packPtr;

    if (original == nullptr) return nullptr;

    packPtr = (Expression*)
            gm2(theEnv, sizeof(Expression) * ExpressionSize(original));
    ListToPacked(original, packPtr, 0);

    return packPtr;
}

/***********************************************************/
/* ListToPacked: Copies a list of expressions to an array. */
/***********************************************************/
static unsigned long ListToPacked(
        Expression *original,
        Expression *destination,
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
                    (Expression*) &destination[count];
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
        const Environment&theEnv,
        Expression *packPtr) {
    if (packPtr != nullptr) {
        rm(theEnv, packPtr, sizeof(Expression) * ExpressionSize(packPtr));
    }
}

/***********************************************/
/* ReturnExpression: Returns a multiply linked */
/*   list of Expression data structures.             */
/***********************************************/
void ReturnExpression(
        const Environment&theEnv,
        Expression *waste) {
    Expression *tmp;

    while (waste != nullptr) {
        if (waste->argList != nullptr) ReturnExpression(theEnv, waste->argList);
        tmp = waste;
        waste = waste->nextArg;
        rtn_struct(theEnv, Expression, tmp);
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
static ExpressionHashNode *FindHashedExpression(
        const Environment&theEnv,
        Expression *theExp,
        unsigned *hashval,
        ExpressionHashNode **prv) {
    ExpressionHashNode *exphash;

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
        const Environment&theEnv,
        Expression *theExp) {
    ExpressionHashNode *exphash, *prv;
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
    rtn_struct(theEnv, ExpressionHashNode, exphash);
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
        const Environment&theEnv,
        Expression *theExp) {
    ExpressionHashNode *prv, *exphash;
    unsigned hashval;

    if (theExp == nullptr) return nullptr;
    exphash = FindHashedExpression(theEnv, theExp, &hashval, &prv);
    if (exphash != nullptr) {
        exphash->count++;
        return (exphash->exp);
    }
    exphash = get_struct(theEnv, ExpressionHashNode);
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
        const Environment&theEnv,
        Expression *theExp) {
    ExpressionHashNode *exphash, *prv;
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
        const Environment&theEnv,
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
        const Environment&theEnv) {
    return ExpressionData(theEnv)->SequenceOpMode;
}

