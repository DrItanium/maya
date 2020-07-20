/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  10/24/17            */
/*                                                     */
/*            EXTERNAL FUNCTIONS HEADER FILE           */
/*******************************************************/

/*************************************************************/
/* Purpose: Routines for adding new user or system defined   */
/*   functions.                                              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Added support for passing context information  */
/*            to user defined functions.                     */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Replaced ALLOW_ENVIRONMENT_GLOBALS macros      */
/*            with functions.                                */
/*                                                           */
/*      6.40: Changed restrictions from char * to            */
/*            CLIPSLexeme * to support strings               */
/*            originating from sources that are not          */
/*            statically allocated.                          */
/*                                                           */
/*            Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            Callbacks must be environment aware.           */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_extnfunc
#define _H_extnfunc
#include <functional>
#include <memory>

#include "Entities.hxx"
#include "Environment.h"
#include "Evaluation.h"
#include "Expression.h"
#include "Hashable.h"
//#include "Symbol.h"
//#include "UserData.h"

namespace maya {
    class ExternalFunction : public HoldsEnvironmentCallback, public Hashable {
    public:
        using Self = ExternalFunction;
        using Ptr = std::shared_ptr<Self>;
        using Body = std::function<void(Environment&, UDFContext&, UDFValue::Ptr)>;
        using Parser = std::function<Expression::Ptr(Environment&, Expression::Ptr, const std::string&)>;
    public:
        ExternalFunction(Environment& env, const std::string& name, unsigned int retType, Body function, Parser parser, const std::string& restrictions, unsigned short minArgs, unsigned short maxArgs);
        virtual ~ExternalFunction() = default;
        [[nodiscard]] Lexeme::Ptr getFunctionName() const noexcept { return _callFunctionName; }
        [[nodiscard]] Lexeme::Ptr getRestrictions() const noexcept { return _restrictions; }
        bool evaluate(UDFContext& context, UDFValue::Ptr returnValue);
        Expression::Ptr parse(Expression::Ptr, const std::string&);
        [[nodiscard]] constexpr auto isNeeded() const noexcept { return _neededFunction; }
        void setNeeded(bool value) noexcept { _neededFunction = value; }
        [[nodiscard]] constexpr auto sequenceUseOk() const noexcept { return _sequenceUseOk; }
        void setSequenceUseOk(bool value = true) noexcept { _sequenceUseOk = value; }
        [[nodiscard]] constexpr auto overloadable() const noexcept { return _overloadable; }
        [[nodiscard]] bool hasCustomParser() const noexcept { return (bool)_parser; }
        void setCustomParser(Parser parser) noexcept;
        [[nodiscard]] bool hasBody() const noexcept { return (bool)_function; }
        [[nodiscard]] constexpr auto getMinArgs() const noexcept { return _minArgs; }
        [[nodiscard]] constexpr auto getMaxArgs() const noexcept { return _maxArgs; }
        size_t hash(size_t limit) override;
    private:
        Lexeme::Ptr _callFunctionName;
        unsigned int _returnType;
        Body _function;
        Parser _parser;
        Lexeme::Ptr _restrictions;
        unsigned short _minArgs = 0;
        unsigned short _maxArgs = 0;
        bool _overloadable = true;
        bool _sequenceUseOk = true; // is the use of the sequence operator ($) allowed?
        bool _neededFunction = false;
#if 0
        unsigned long bsaveIndex = 0;
        std::shared_ptr<struct userData> userData; // not sure what this is for to be honest
#endif
    };

#if STUBBING_INACTIVE
    #define UnknownFunctionType(target) (((FunctionDefinition *) target)->unknownReturnValueType)
#define ExpressionFunctionPointer(target) ((target)->functionValue->functionPointer)
#define ExpressionFunctionCallName(target) ((target)->functionValue->callFunctionName)
#define ExpressionUnknownFunctionType(target) ((target)->functionValue->unknownReturnValueType)

/*==================*/
/* ENVIRONMENT DATA */
/*==================*/

        constexpr auto EXTERNAL_FUNCTION_DATA = 50;
        struct FunctionHash;
        struct externalFunctionData : public EnvironmentModule {
            FunctionDefinition *ListOfFunctions = nullptr;
            FunctionHash **FunctionHashtable = nullptr;
        };
        RegisterEnvironmentModule(externalFunctionData, EXTERNAL_FUNCTION_DATA, ExternalFunction);

        enum AddUDFError {
            AUE_NO_ERROR = 0,
                    AUE_MIN_EXCEEDS_MAX_ERROR,
                    AUE_FUNCTION_NAME_IN_USE_ERROR,
                    AUE_INVALID_ARGUMENT_TYPE_ERROR,
                    AUE_INVALID_RETURN_TYPE_ERROR,
                    AUE_CREATE_FUNCTION_STUBBED,
        };

        struct FunctionHash {
            FunctionDefinition *fdPtr = nullptr;
            FunctionHash *next = nullptr;
        };

        constexpr auto SIZE_FUNCTION_HASH = 517;

        void InitializeExternalFunctionData(const Environment::Ptr&);
        AddUDFError AddUDF(const Environment::Ptr&theEnv, const char *name, const char *returnTypes, unsigned short minArgs, unsigned short maxArgs,
        const char *argumentTypes, UserDefinedFunction *cFunctionPointer, void *context = nullptr);
        bool FuncSeqOvlFlags(const Environment::Ptr&, const char *, bool, bool);
        FunctionDefinition *GetFunctionList(const Environment::Ptr&);
        void InstallFunctionList(const Environment::Ptr&, FunctionDefinition *);
        FunctionDefinition *FindFunction(const Environment::Ptr&, const char *);
        unsigned GetNthRestriction(const Environment::Ptr&, FunctionDefinition *, unsigned int);
        bool RemoveUDF(const Environment::Ptr&, const char *);
        unsigned int UDFArgumentCount(UDFContext *);
        bool UDFNthArgument(UDFContext *, unsigned int, unsigned, UDFValue *);
        void UDFInvalidArgumentMessage(UDFContext *, const char *);
        std::string UDFContextFunctionName(UDFContext *);
        void PrintTypesString(const Environment::Ptr&, const char *, unsigned, bool);
        bool UDFFirstArgument(UDFContext *, unsigned, UDFValue *);
        bool UDFNextArgument(UDFContext *, unsigned, UDFValue *);
        void UDFThrowError(UDFContext *);

#define UDFHasNextArgument(context) (context->lastArg != nullptr)
#endif
} // end namespace maya
#endif /* _H_extnfunc */



