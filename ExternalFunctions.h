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

#pragma once

#define _H_extnfunc

#include "Entities.h"

#include "Evaluation.h"
#include "Expression.h"
#include "Symbol.h"
#include "UserData.h"

typedef void UserDefinedFunction(const Environment& theEnv, UDFContext *context, UDFValue *ret);
typedef struct expr* UserDefinedFunctionParser(const Environment&, struct expr*, const char*);

struct functionDefinition {
    CLIPSLexeme *callFunctionName;
    unsigned unknownReturnValueType;
    UserDefinedFunction* functionPointer;
    UserDefinedFunctionParser* parser;
    CLIPSLexeme *restrictions;
    unsigned short minArgs;
    unsigned short maxArgs;
    bool overloadable;
    bool sequenceuseok;
    bool neededFunction;
    unsigned long bsaveIndex;
    struct functionDefinition *next;
    struct userData *usrData;
    void *context;
};

#define UnknownFunctionType(target) (((functionDefinition *) target)->unknownReturnValueType)
#define ExpressionFunctionPointer(target) ((target)->functionValue->functionPointer)
#define ExpressionFunctionCallName(target) ((target)->functionValue->callFunctionName)
#define ExpressionUnknownFunctionType(target) ((target)->functionValue->unknownReturnValueType)

/*==================*/
/* ENVIRONMENT DATA */
/*==================*/

constexpr auto EXTERNAL_FUNCTION_DATA = 50;

struct externalFunctionData {
    struct functionDefinition *ListOfFunctions;
    struct FunctionHash **FunctionHashtable;
};

#define ExternalFunctionData(theEnv) ((externalFunctionData *) GetEnvironmentData(theEnv,EXTERNAL_FUNCTION_DATA))

enum AddUDFError {
    AUE_NO_ERROR = 0,
    AUE_MIN_EXCEEDS_MAX_ERROR,
    AUE_FUNCTION_NAME_IN_USE_ERROR,
    AUE_INVALID_ARGUMENT_TYPE_ERROR,
    AUE_INVALID_RETURN_TYPE_ERROR
};

struct FunctionHash {
    struct functionDefinition *fdPtr;
    struct FunctionHash *next;
};

constexpr auto SIZE_FUNCTION_HASH = 517;

void InitializeExternalFunctionData(const Environment&);
AddUDFError AddUDF(const Environment&theEnv, const char *name, const char *returnTypes, unsigned short minArgs, unsigned short maxArgs,
                   const char *argumentTypes, UserDefinedFunction *cFunctionPointer, void *context = nullptr);
bool AddFunctionParser(const Environment&, const char *,
                       struct expr *(*)(const Environment&, struct expr *, const char *));
bool RemoveFunctionParser(const Environment&, const char *);
bool FuncSeqOvlFlags(const Environment&, const char *, bool, bool);
struct functionDefinition *GetFunctionList(const Environment&);
void InstallFunctionList(const Environment&, struct functionDefinition *);
struct functionDefinition *FindFunction(const Environment&, const char *);
unsigned GetNthRestriction(const Environment&, struct functionDefinition *, unsigned int);
bool RemoveUDF(const Environment&, const char *);
int GetMinimumArgs(functionDefinition *);
int GetMaximumArgs(functionDefinition *);
unsigned int UDFArgumentCount(UDFContext *);
bool UDFNthArgument(UDFContext *, unsigned int, unsigned, UDFValue *);
void UDFInvalidArgumentMessage(UDFContext *, const char *);
const char *UDFContextFunctionName(UDFContext *);
void PrintTypesString(const Environment&, const char *, unsigned, bool);
bool UDFFirstArgument(UDFContext *, unsigned, UDFValue *);
bool UDFNextArgument(UDFContext *, unsigned, UDFValue *);
void UDFThrowError(UDFContext *);
void *GetUDFContext(const Environment&, const char *);

#define UDFHasNextArgument(context) (context->lastArg != nullptr)

#endif /* _H_extnfunc */



