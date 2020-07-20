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

//struct ExpressionHashNode;
//typedef struct SavedContexts SavedContexts;

#include "Environment.h"
#include "Entities.hxx"
#include "Evaluable.h"
//#include "Construct.h"
//#include "ExternalFunctions.h"
//#include "Scanner.h"
namespace maya {
/******************************/
/* Expression Data Structures */
/******************************/
    class ExternalFunction;
    struct Expression : public HoldsEnvironmentCallback {
    public:
        using Self = Expression;
        using Ptr = std::shared_ptr<Self>;
        using Container = std::variant<Evaluable::Ptr, std::shared_ptr<ExternalFunction>>;
        using PtrList = std::list<Ptr>;
    public:
        Expression(Environment& parent, const std::shared_ptr<Evaluable>& evaluable);
        Expression(Environment& parent, const std::shared_ptr<class ExternalFunction>& extnFunc);
        Container _contents;
        PtrList _args;
        // originally this struct had a nextArg field which was probably used to great effect in making a given expression
        // I have eliminated that design since it is more of a c-ism than a c++-ism
        /**
         * @brief Get the number of expressions stored within this expression
         * @return 1 + the number of elements in the arg list
         */
        size_t size() const noexcept;
        bool evaluate(UDFValue::Ptr returnValue);
    };
    /**
     * @brief Used to preserve return and break status when doing call frames
     */
    class SavedContexts {
    public:
        using Self = SavedContexts;
        using Ptr = std::shared_ptr<Self>;
    public:
        SavedContexts(bool rtn, bool brk) : _rtn(rtn), _brk(brk) { }
        [[nodiscard]] constexpr auto getReturnStatus() const noexcept { return _rtn; }
        [[nodiscard]] constexpr auto getBreakStatus() const noexcept { return _brk; }
    private:
        bool _rtn;
        bool _brk;
    };
#if STUBBING_INACTIVE
    struct ExpressionHashNode {
    public:
        using Self = ExpressionHashNode;
        using Ptr = std::shared_ptr<Self>;
    public:
        unsigned hashval;
        unsigned count;
        Expression::Ptr exp;
        unsigned long bsaveID;
    };
#endif
    constexpr auto EXPRESSION_HASH_SIZE = 503;

/********************/
/* ENVIRONMENT DATA */
/********************/
#if 0
    constexpr auto EXPRESSION_DATA = 45;
    using FunctionDefinitionPtr = std::shared_ptr<struct FunctionDefinition>;

    struct ExpressionModule : public EnvironmentModule {
    public:
        static void install(Environment &);
    public:
        ExpressionModule(Environment &parent);
#if STUBBING_INACTIVE
        /// @todo use std::shared_ptr where it makes perfect sense
        FunctionDefinitionPtr PTR_AND;
        FunctionDefinitionPtr PTR_OR;
        FunctionDefinitionPtr PTR_EQ;
        FunctionDefinitionPtr PTR_NEQ;
        FunctionDefinitionPtr PTR_NOT;
        /// @todo replace this with an actual hash table
        std::array<std::list<ExpressionHashNode::Ptr>, EXPRESSION_HASH_SIZE> _expressionHashTable;
#endif
#if (BLOAD_AND_BSAVE)
        unsigned long NumberOfExpressions = 0;
        Expression *ExpressionArray = nullptr;
        unsigned long ExpressionCount = 0;
#endif
#if STUBBING_INACTIVE
        std::list<SavedContexts> _svContexts;
        void setSequenceOperatorMode(bool value) noexcept { _sequenceOpMode = value; }
        constexpr auto getSequenceOperatorMode() const noexcept { return _sequenceOpMode; }
        void setReturnContext(bool value) noexcept { _returnContext = value; }
        constexpr auto getReturnContext() const noexcept { return _returnContext; }
        void setBreakContext(bool value) noexcept { _breakContext = value; }
        constexpr auto getBreakContext() const noexcept { return _breakContext; }
    private:
        bool _sequenceOpMode = false;
        bool _returnContext = false;
        bool _breakContext = false;
#endif
    };
#endif
#if STUBBING_INACTIVE
/********************/
/* Global Functions */
/********************/

    void ReturnExpression(const Environment::Ptr &, Expression *);
    void ExpressionInstall(const Environment::Ptr &, Expression *);
    void ExpressionDeinstall(const Environment::Ptr &, Expression *);
    Expression *PackExpression(const Environment::Ptr &, Expression *);
    void ReturnPackedExpression(const Environment::Ptr &, Expression *);
    Expression *AddHashedExpression(const Environment::Ptr &, Expression *);
    void RemoveHashedExpression(const Environment::Ptr &, Expression *);
#if BLOAD_AND_BSAVE
    unsigned long HashedExpressionIndex(const Environment::Ptr&, Expression *);
#endif
#define ExpressionPointer(i) ((Expression*) (((i) == ULONG_MAX) ? nullptr : &ExpressionData(theEnv)->ExpressionArray[i]))
#define HashedExpressionPointer(i) ExpressionPointer(i)

    void AllocateExpressions(const Environment::Ptr &);
    void RefreshExpressions(const Environment::Ptr &);
    void ClearBloadedExpressions(const Environment::Ptr &);
    void FindHashedExpressions(const Environment::Ptr &);
    void BsaveHashedExpressions(const Environment::Ptr &, FILE *);
    void BsaveConstructExpressions(const Environment::Ptr &, FILE *);
    void BsaveExpression(const Environment::Ptr &, Expression *, FILE *);

    void PrintExpression(const Environment::Ptr &, const char *, Expression *);
    Expression *CopyExpression(const Environment::Ptr &, Expression *);
    bool ExpressionContainsVariables(Expression *, bool);
    bool IdenticalExpression(Expression *, Expression *);
    bool CheckArgumentAgainstRestriction(const Environment::Ptr &, Expression *, unsigned);
    Expression *CombineExpressions(const Environment::Ptr &, Expression *, Expression *);
    Expression *AppendExpressions(Expression *, Expression *);
    Expression *NegateExpression(const Environment::Ptr &, Expression *);
    enum FunctionArgumentsError {
        FAE_NO_ERROR = 0,
        FAE_COUNT_ERROR,
        FAE_TYPE_ERROR
    };

    Expression *Function0Parse(const Environment::Ptr &, const char *);
    Expression *Function1Parse(const Environment::Ptr &, const char *);
    Expression *Function2Parse(const Environment::Ptr &, const char *, const char *);
    void PushRtnBrkContexts(const Environment::Ptr &);
    void PopRtnBrkContexts(const Environment::Ptr &);
    bool ReplaceSequenceExpansionOps(const Environment::Ptr &, Expression *, Expression *,
                                     void *, void *);
    Expression *CollectArguments(const Environment::Ptr &, Expression *, const char *);
    Expression *ArgumentParse(const Environment::Ptr &, const char *, bool *);
    Expression *ParseAtomOrExpression(const Environment::Ptr &, const char *, struct token *);
    Expression *ParseConstantArguments(const Environment::Ptr &, const char *, bool *);
    Expression *GroupActions(const Environment::Ptr &, const char *, struct token *,
                             bool, const char *, bool);
    Expression *RemoveUnneededProgn(const Environment::Ptr &, Expression *);
    void PopulateRestriction(const Environment::Ptr &theEnv,
                             unsigned *restriction,
                             unsigned defaultRestriction,
                             const std::string &restrictionString,
                             unsigned int position);

    FunctionArgumentsError CheckExpressionAgainstRestrictions(const Environment::Ptr &, Expression *,
                                                              FunctionDefinition *, const char *);

    bool RestrictionExists(const char *, int);
#endif
} // end namespace maya
#endif




