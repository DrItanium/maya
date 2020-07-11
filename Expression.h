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

struct ExpressionHashNode;
typedef struct savedContexts SavedContexts;

#include <cstdio>
#include "Entities.h"
#include "Construct.h"
#include "ExternalFunctions.h"
#include "Scanner.h"

/******************************/
/* Expression Data Structures */
/******************************/

struct Expression {
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

struct ExpressionHashNode {
    unsigned hashval;
    unsigned count;
    Expression *exp;
    ExpressionHashNode *next;
    unsigned long bsaveID;
} ;

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

struct ExpressionData : public EnvironmentModule {
    /// @todo use std::shared_ptr where it makes perfect sense
    FunctionDefinition *PTR_AND = nullptr;
    FunctionDefinition *PTR_OR = nullptr;
    FunctionDefinition *PTR_EQ = nullptr;
    FunctionDefinition *PTR_NEQ = nullptr;
    FunctionDefinition *PTR_NOT = nullptr;
    /// @todo replace this with an actual hash table
    ExpressionHashNode **ExpressionHashTable = nullptr;
#if (BLOAD_AND_BSAVE)
    unsigned long NumberOfExpressions = 0;
    Expression *ExpressionArray = nullptr;
    unsigned long ExpressionCount = 0;
#endif
    SavedContexts *svContexts = nullptr;
    bool ReturnContext = false;
    bool BreakContext = false;
    bool SequenceOpMode = false;
};
RegisterEnvironmentModule(ExpressionData, EXPRESSION_DATA);
#define ExpressionData(theEnv) (GetEnvironmentData(theEnv,EXPRESSION_DATA))

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
#define ExpressionPointer(i) ((Expression*) (((i) == ULONG_MAX) ? nullptr : &ExpressionData(theEnv)->ExpressionArray[i]))
#define HashedExpressionPointer(i) ExpressionPointer(i)

void AllocateExpressions(const Environment&);
void RefreshExpressions(const Environment&);
void ClearBloadedExpressions(const Environment&);
void FindHashedExpressions(const Environment&);
void BsaveHashedExpressions(const Environment&, FILE *);
void BsaveConstructExpressions(const Environment&, FILE *);
void BsaveExpression(const Environment&, Expression *, FILE *);

bool ConstantExpression(Expression*);
void PrintExpression(const Environment&, const char *, Expression *);
unsigned long ExpressionSize(Expression *);
unsigned short CountArguments(Expression *);
Expression *CopyExpression(const Environment&, Expression *);
bool ExpressionContainsVariables(Expression *, bool);
bool IdenticalExpression(Expression *, Expression *);
Expression *GenConstant(const Environment&, unsigned short, void *);
bool CheckArgumentAgainstRestriction(const Environment&, Expression *, unsigned);
bool ConstantType(int);
Expression *CombineExpressions(const Environment&, Expression *, Expression *);
Expression *AppendExpressions(Expression *, Expression *);
Expression *NegateExpression(const Environment&, Expression *);
enum FunctionArgumentsError{
    FAE_NO_ERROR = 0,
    FAE_COUNT_ERROR,
    FAE_TYPE_ERROR
};

Expression *Function0Parse(const Environment&, const char *);
Expression *Function1Parse(const Environment&, const char *);
Expression *Function2Parse(const Environment&, const char *, const char *);
void PushRtnBrkContexts(const Environment&);
void PopRtnBrkContexts(const Environment&);
bool ReplaceSequenceExpansionOps(const Environment&, Expression *, Expression *,
                                 void *, void *);
Expression *CollectArguments(const Environment&, Expression *, const char *);
Expression *ArgumentParse(const Environment&, const char *, bool *);
Expression *ParseAtomOrExpression(const Environment&, const char *, struct token *);
Expression *ParseConstantArguments(const Environment&, const char *, bool *);
Expression *GroupActions(const Environment&, const char *, struct token *,
                          bool, const char *, bool);
Expression *RemoveUnneededProgn(const Environment&, Expression *);
void PopulateRestriction(const Environment&, unsigned *, unsigned, const char *, unsigned int);

FunctionArgumentsError CheckExpressionAgainstRestrictions(const Environment&, Expression *,
                                                          FunctionDefinition *, const char *);

bool RestrictionExists(const char *, int);
#endif




