/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  11/01/16            */
/*                                                     */
/*               EXPRESSION HEADER FILE                */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Changed expression hashing value.              */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            Eval support for run time and bload only.      */
/*                                                           */
/*************************************************************/

#ifndef _H_expressn

#pragma once

#define _H_expressn

struct exprHashNode;
typedef struct savedContexts SavedContexts;

#include <cstdio>
#include "Entities.h"
#include "Construct.h"
#include "ExternalFunctions.h"
#include "Scanner.h"

/******************************/
/* Expression Data Structures */
/******************************/

struct expr {
    unsigned short type;
    union {
        void *value;
        CLIPSLexeme *lexemeValue;
        CLIPSFloat *floatValue;
        CLIPSInteger *integerValue;
        CLIPSBitMap *bitMapValue;
        ConstructHeader *constructValue;
        FunctionDefinition *functionValue;
    };
    Expression *argList;
    Expression *nextArg;
};

#define arg_list argList
#define next_arg nextArg

typedef struct exprHashNode {
    unsigned hashval;
    unsigned count;
    Expression *exp;
    struct exprHashNode *next;
    unsigned long bsaveID;
} EXPRESSION_HN;

struct savedContexts {
    bool rtn;
    bool brk;
    struct savedContexts *nxt;
};

constexpr auto EXPRESSION_HASH_SIZE = 503;

/********************/
/* ENVIRONMENT DATA */
/********************/

constexpr auto EXPRESSION_DATA = 45;

struct expressionData {
    FunctionDefinition *PTR_AND;
    FunctionDefinition *PTR_OR;
    FunctionDefinition *PTR_EQ;
    FunctionDefinition *PTR_NEQ;
    FunctionDefinition *PTR_NOT;
    EXPRESSION_HN **ExpressionHashTable;
#if (BLOAD_AND_BSAVE)
    unsigned long NumberOfExpressions;
    Expression *ExpressionArray;
    unsigned long ExpressionCount;
#endif
    SavedContexts *svContexts;
    bool ReturnContext;
    bool BreakContext;
    bool SequenceOpMode;
};

#define ExpressionData(theEnv) ((expressionData *) GetEnvironmentData(theEnv,EXPRESSION_DATA))

/********************/
/* Global Functions */
/********************/

void ReturnExpression(const Environment&, Expression *);
void ExpressionInstall(const Environment&, Expression *);
void ExpressionDeinstall(const Environment&, Expression *);
Expression *PackExpression(const Environment&, Expression *);
void ReturnPackedExpression(const Environment&, Expression *);
void InitExpressionData(const Environment&);
void InitExpressionPointers(const Environment&);
bool SetSequenceOperatorRecognition(const Environment&, bool);
bool GetSequenceOperatorRecognition(const Environment&);
Expression *AddHashedExpression(const Environment&, Expression *);
void RemoveHashedExpression(const Environment&, Expression *);
#if BLOAD_AND_BSAVE
unsigned long HashedExpressionIndex(const Environment&, Expression *);
#endif
#define ExpressionPointer(i) ((expr *) (((i) == ULONG_MAX) ? nullptr : &ExpressionData(theEnv)->ExpressionArray[i]))
#define HashedExpressionPointer(i) ExpressionPointer(i)

void AllocateExpressions(const Environment&);
void RefreshExpressions(const Environment&);
void ClearBloadedExpressions(const Environment&);
void FindHashedExpressions(const Environment&);
void BsaveHashedExpressions(const Environment&, FILE *);
void BsaveConstructExpressions(const Environment&, FILE *);
void BsaveExpression(const Environment&, struct expr *, FILE *);

bool ConstantExpression(expr *);
void PrintExpression(const Environment&, const char *, struct expr *);
unsigned long ExpressionSize(expr *);
unsigned short CountArguments(expr *);
struct expr *CopyExpression(const Environment&, struct expr *);
bool ExpressionContainsVariables(expr *, bool);
bool IdenticalExpression(expr *, struct expr *);
struct expr *GenConstant(const Environment&, unsigned short, void *);
bool CheckArgumentAgainstRestriction(const Environment&, struct expr *, unsigned);
bool ConstantType(int);
struct expr *CombineExpressions(const Environment&, struct expr *, struct expr *);
struct expr *AppendExpressions(expr *, struct expr *);
struct expr *NegateExpression(const Environment&, struct expr *);
enum FunctionArgumentsError{
    FAE_NO_ERROR = 0,
    FAE_COUNT_ERROR,
    FAE_TYPE_ERROR
};

struct expr *Function0Parse(const Environment&, const char *);
struct expr *Function1Parse(const Environment&, const char *);
struct expr *Function2Parse(const Environment&, const char *, const char *);
void PushRtnBrkContexts(const Environment&);
void PopRtnBrkContexts(const Environment&);
bool ReplaceSequenceExpansionOps(const Environment&, struct expr *, struct expr *,
                                 void *, void *);
struct expr *CollectArguments(const Environment&, struct expr *, const char *);
struct expr *ArgumentParse(const Environment&, const char *, bool *);
struct expr *ParseAtomOrExpression(const Environment&, const char *, struct token *);
Expression *ParseConstantArguments(const Environment&, const char *, bool *);
struct expr *GroupActions(const Environment&, const char *, struct token *,
                          bool, const char *, bool);
struct expr *RemoveUnneededProgn(const Environment&, struct expr *);
void PopulateRestriction(const Environment&, unsigned *, unsigned, const char *, unsigned int);

FunctionArgumentsError CheckExpressionAgainstRestrictions(const Environment&, struct expr *,
                                                          struct functionDefinition *, const char *);

bool RestrictionExists(const char *, int);
#endif




