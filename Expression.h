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
#include "Entities.hxx"
#include "Construct.h"
#include "ExternalFunctions.h"
#include "Scanner.h"
#include "Environment.h"

/******************************/
/* Expression Data Structures */
/******************************/

struct Expression {
public:
    using Self = Expression;
    using Ptr = std::shared_ptr<Self>;
public:
    unsigned short type;
    std::variant<std::monostate,
            CLIPSLexeme::Ptr,
            CLIPSFloat::Ptr,
            CLIPSInteger::Ptr,
            CLIPSBitMap::Ptr,
            std::shared_ptr<struct Fact>,
            std::shared_ptr<struct Instance>,
            Multifield::Ptr,
            std::shared_ptr<ConstructHeader>,
            std::shared_ptr<FunctionDefinition>> contents;
    Ptr argList;
    Ptr nextArg;
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

struct ExpressionModule : public EnvironmentModule {
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
RegisterEnvironmentModule(ExpressionModule, EXPRESSION_DATA, Expression);

/********************/
/* Global Functions */
/********************/

void ReturnExpression(const Environment::Ptr&, Expression *);
void ExpressionInstall(const Environment::Ptr&, Expression *);
void ExpressionDeinstall(const Environment::Ptr&, Expression *);
Expression *PackExpression(const Environment::Ptr&, Expression *);
void ReturnPackedExpression(const Environment::Ptr&, Expression *);
void InitExpressionData(const Environment::Ptr&);
void InitExpressionPointers(const Environment::Ptr&);
bool SetSequenceOperatorRecognition(const Environment::Ptr&, bool);
bool GetSequenceOperatorRecognition(const Environment::Ptr&);
Expression *AddHashedExpression(const Environment::Ptr&, Expression *);
void RemoveHashedExpression(const Environment::Ptr&, Expression *);
#if BLOAD_AND_BSAVE
unsigned long HashedExpressionIndex(const Environment::Ptr&, Expression *);
#endif
#define ExpressionPointer(i) ((Expression*) (((i) == ULONG_MAX) ? nullptr : &ExpressionData(theEnv)->ExpressionArray[i]))
#define HashedExpressionPointer(i) ExpressionPointer(i)

void AllocateExpressions(const Environment::Ptr&);
void RefreshExpressions(const Environment::Ptr&);
void ClearBloadedExpressions(const Environment::Ptr&);
void FindHashedExpressions(const Environment::Ptr&);
void BsaveHashedExpressions(const Environment::Ptr&, FILE *);
void BsaveConstructExpressions(const Environment::Ptr&, FILE *);
void BsaveExpression(const Environment::Ptr&, Expression *, FILE *);

bool ConstantExpression(Expression*);
void PrintExpression(const Environment::Ptr&, const char *, Expression *);
unsigned long ExpressionSize(Expression *);
unsigned short CountArguments(Expression *);
Expression *CopyExpression(const Environment::Ptr&, Expression *);
bool ExpressionContainsVariables(Expression *, bool);
bool IdenticalExpression(Expression *, Expression *);
Expression *GenConstant(const Environment::Ptr&, unsigned short, void *);
bool CheckArgumentAgainstRestriction(const Environment::Ptr&, Expression *, unsigned);
bool ConstantType(int);
Expression *CombineExpressions(const Environment::Ptr&, Expression *, Expression *);
Expression *AppendExpressions(Expression *, Expression *);
Expression *NegateExpression(const Environment::Ptr&, Expression *);
enum FunctionArgumentsError{
    FAE_NO_ERROR = 0,
    FAE_COUNT_ERROR,
    FAE_TYPE_ERROR
};

Expression *Function0Parse(const Environment::Ptr&, const char *);
Expression *Function1Parse(const Environment::Ptr&, const char *);
Expression *Function2Parse(const Environment::Ptr&, const char *, const char *);
void PushRtnBrkContexts(const Environment::Ptr&);
void PopRtnBrkContexts(const Environment::Ptr&);
bool ReplaceSequenceExpansionOps(const Environment::Ptr&, Expression *, Expression *,
                                 void *, void *);
Expression *CollectArguments(const Environment::Ptr&, Expression *, const char *);
Expression *ArgumentParse(const Environment::Ptr&, const char *, bool *);
Expression *ParseAtomOrExpression(const Environment::Ptr&, const char *, struct token *);
Expression *ParseConstantArguments(const Environment::Ptr&, const char *, bool *);
Expression *GroupActions(const Environment::Ptr&, const char *, struct token *,
                          bool, const char *, bool);
Expression *RemoveUnneededProgn(const Environment::Ptr&, Expression *);
void PopulateRestriction(const Environment::Ptr& theEnv,
                         unsigned * restriction,
                         unsigned defaultRestriction,
                         const std::string &restrictionString,
                         unsigned int position);

FunctionArgumentsError CheckExpressionAgainstRestrictions(const Environment::Ptr&, Expression *,
                                                          FunctionDefinition *, const char *);

bool RestrictionExists(const char *, int);
#endif




