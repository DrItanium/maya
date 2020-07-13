/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  02/19/20             */
/*                                                     */
/*               EXTERNAL FUNCTION MODULE              */
/*******************************************************/

/*************************************************************/
/* Purpose: Routines for adding new user or system defined   */
/*   functions.                                              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Corrected code to remove run-time program      */
/*            compiler warning.                              */
/*                                                           */
/*      6.30: Added support for passing context information  */
/*            to user defined functions.                     */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Changed restrictions from char * to            */
/*            CLIPSLexeme * to support strings               */
/*            originating from sources that are not          */
/*            statically allocated.                          */
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
/*            Eval support for run time and bload only.      */
/*                                                           */
/*************************************************************/

#include "Setup.h"

#include <ctype.h>
#include <cstdlib>

#include "ArgumentAccess.h"
#include "Constants.h"
#include "Environment.h"
#include "Expression.h"
#include "Fact.h"
#include "MemoryAllocation.h"
#include "Router.h"

#include "InstanceCommand.h"
#include "ExternalFunctions.h"
#include "ReferenceCounted.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void AddHashFunction(const Environment::Ptr&, FunctionDefinition *);
static void InitializeFunctionHashTable(const Environment::Ptr&);
static void DeallocateExternalFunctionData(const Environment::Ptr&);
static bool RemoveHashFunction(const Environment::Ptr&, FunctionDefinition *);
static AddUDFError DefineFunction(const Environment::Ptr&, const char *, unsigned, void (*)(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret),
                                  unsigned short, unsigned short, const char *, void *);
static void PrintType(const Environment::Ptr&, const char *, int, int *, const char *);
static void AssignErrorValue(UDFContext *);

/*********************************************************/
/* InitializeExternalFunctionData: Allocates environment */
/*    data for external functions.                       */
/*********************************************************/
void InitializeExternalFunctionData(
        const Environment::Ptr&theEnv) {
    auto ptr = std::make_unique<externalFunctionData>();
    theEnv->installEnvironmentModule(std::move(ptr));
    /// @todo move DeallocateExternalFunctionData to externalFunctionData's dtor
    //AllocateEnvironmentData(theEnv, EXTERNAL_FUNCTION_DATA, sizeof(externalFunctionData), DeallocateExternalFunctionData);
}

/***********************************************************/
/* DeallocateExternalFunctionData: Deallocates environment */
/*    data for external functions.                         */
/***********************************************************/
static void DeallocateExternalFunctionData(
        const Environment::Ptr&theEnv) {
    struct FunctionHash *fhPtr, *nextFHPtr;
    int i;

    FunctionDefinition *tmpPtr, *nextPtr;

    tmpPtr = ExternalFunctionData(theEnv)->ListOfFunctions;
    while (tmpPtr != nullptr) {
        nextPtr = tmpPtr->next;
        rtn_struct(theEnv, FunctionDefinition, tmpPtr);
        tmpPtr = nextPtr;
    }

    if (ExternalFunctionData(theEnv)->FunctionHashtable == nullptr) { return; }

    for (i = 0; i < SIZE_FUNCTION_HASH; i++) {
        fhPtr = ExternalFunctionData(theEnv)->FunctionHashtable[i];
        while (fhPtr != nullptr) {
            nextFHPtr = fhPtr->next;
            rtn_struct(theEnv, FunctionHash, fhPtr);
            fhPtr = nextFHPtr;
        }
    }

    genfree(theEnv, ExternalFunctionData(theEnv)->FunctionHashtable,
            sizeof(FunctionHash *) * SIZE_FUNCTION_HASH);
}


/****************************************************/
/* AddUDF: Used to define a system or user external */
/*   function so that the KB can access it.         */
/****************************************************/
AddUDFError AddUDF(const Environment::Ptr&theEnv, const char *name, const char *returnTypes, unsigned short minArgs, unsigned short maxArgs,
                   const char *argumentTypes, UserDefinedFunction *cFunctionPointer, void *context) {
    unsigned returnTypeBits;
    size_t i;
    const char *validTypeChars = "bdefilmnsyv*;";

    if ((minArgs != UNBOUNDED) && (minArgs > maxArgs)) { return AUE_MIN_EXCEEDS_MAX_ERROR; }

    if (argumentTypes != nullptr) {
        for (i = 0; argumentTypes[i] != EOS; i++) {
            if (strchr(validTypeChars, argumentTypes[i]) == nullptr) { return AUE_INVALID_ARGUMENT_TYPE_ERROR; }
        }
    }

    if (returnTypes != nullptr) {
        for (i = 0; returnTypes[i] != EOS; i++) {
            if (strchr(validTypeChars, returnTypes[i]) == nullptr) { return AUE_INVALID_RETURN_TYPE_ERROR; }
        }

        PopulateRestriction(theEnv, &returnTypeBits, ANY_TYPE_BITS, returnTypes, 0);
    } else { returnTypeBits = ANY_TYPE_BITS; }

    return DefineFunction(theEnv, name, returnTypeBits, cFunctionPointer,
                          minArgs, maxArgs, argumentTypes, context);
}

/*************************************************************/
/* DefineFunction: Used to define a system or user external  */
/*   function so that the KB can access it. Allows argument  */
/*   restrictions to be attached to the function.            */
/*************************************************************/
static AddUDFError DefineFunction(
        const Environment::Ptr&theEnv,
        const char *name,
        unsigned returnTypeBits,
        void (*pointer)(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret),
        unsigned short minArgs,
        unsigned short maxArgs,
        const char *restrictions,
        void *context) {
    return AddUDFError::AUE_CREATE_FUNCTION_STUBBED;
#if STUBBING_INACTIVE
    FunctionDefinition *newFunction;

    newFunction = FindFunction(theEnv, name);
    if (newFunction != nullptr) { return AUE_FUNCTION_NAME_IN_USE_ERROR; }

    newFunction = get_struct(theEnv, FunctionDefinition);
    newFunction->callFunctionName = CreateSymbol(theEnv, name);
    IncrementLexemeCount(newFunction->callFunctionName);
    newFunction->next = GetFunctionList(theEnv);
    ExternalFunctionData(theEnv)->ListOfFunctions = newFunction;
    AddHashFunction(theEnv, newFunction);

    newFunction->unknownReturnValueType = returnTypeBits;
    newFunction->functionPointer = pointer;

    newFunction->minArgs = minArgs;
    newFunction->maxArgs = maxArgs;

    if (restrictions == nullptr) { newFunction->restrictions = nullptr; }
    else {
        newFunction->restrictions = CreateString(theEnv, restrictions);
        IncrementLexemeCount(newFunction->restrictions);
    }

    newFunction->parser = nullptr;
    newFunction->overloadable = true;
    newFunction->sequenceuseok = true;
    newFunction->usrData = nullptr;
    newFunction->context = context;

    return AUE_NO_ERROR;
#endif
}

/********************************************/
/* RemoveUDF: Used to remove a function     */
/*   definition from the list of functions. */
/********************************************/
bool RemoveUDF(
        const Environment::Ptr&theEnv,
        const char *functionName) {
    CLIPSLexeme *findValue;
    FunctionDefinition *fPtr, *lastPtr = nullptr;

    findValue = FindSymbolHN(theEnv, functionName, SYMBOL_BIT);

    for (fPtr = ExternalFunctionData(theEnv)->ListOfFunctions;
         fPtr != nullptr;
         fPtr = fPtr->next) {
        if (fPtr->callFunctionName == findValue) {
            ReleaseLexeme(theEnv, fPtr->callFunctionName);
            RemoveHashFunction(theEnv, fPtr);

            if (lastPtr == nullptr) { ExternalFunctionData(theEnv)->ListOfFunctions = fPtr->next; }
            else { lastPtr->next = fPtr->next; }

            if (fPtr->restrictions != nullptr) { ReleaseLexeme(theEnv, fPtr->restrictions); }
            ClearUserDataList(theEnv, fPtr->usrData);
            rtn_struct(theEnv, FunctionDefinition, fPtr);
            return true;
        }

        lastPtr = fPtr;
    }

    return false;
}

/******************************************/
/* RemoveHashFunction: Removes a function */
/*   from the function hash table.        */
/******************************************/
static bool RemoveHashFunction(
        const Environment::Ptr&theEnv,
        FunctionDefinition *fdPtr) {
    struct FunctionHash *fhPtr, *lastPtr = nullptr;
    size_t hashValue;

    hashValue = HashSymbol(fdPtr->callFunctionName->contents, SIZE_FUNCTION_HASH);

    for (fhPtr = ExternalFunctionData(theEnv)->FunctionHashtable[hashValue];
         fhPtr != nullptr;
         fhPtr = fhPtr->next) {
        if (fhPtr->fdPtr == fdPtr) {
            if (lastPtr == nullptr) { ExternalFunctionData(theEnv)->FunctionHashtable[hashValue] = fhPtr->next; }
            else { lastPtr->next = fhPtr->next; }

            rtn_struct(theEnv, FunctionHash, fhPtr);
            return true;
        }

        lastPtr = fhPtr;
    }

    return false;
}


/***************************************************************************/
/* AddFunctionParser: Associates a specialized expression parsing function */
/*   with the function entry for a function which was defined using        */
/*   DefineFunction. When this function is parsed, the specialized parsing */
/*   function will be called to parse the arguments of the function. Only  */
/*   user and system defined functions can have specialized parsing        */
/*   routines. Generic functions and deffunctions can not have specialized */
/*   parsing routines.                                                     */
/***************************************************************************/
bool AddFunctionParser(
        const Environment::Ptr&theEnv,
        const char *functionName,
        Expression *(*fpPtr)(const Environment::Ptr&, Expression *, const char *)) {
    FunctionDefinition *fdPtr;

    fdPtr = FindFunction(theEnv, functionName);
    if (fdPtr == nullptr) {
        WriteString(theEnv, STDERR, "Function parsers can only be added for existing functions.\n");
        return false;
    }

    fdPtr->parser = fpPtr;
    fdPtr->overloadable = false;

    return true;
}


/*********************************************************************/
/* RemoveFunctionParser: Removes a specialized expression parsing    */
/*   function (if it exists) from the function entry for a function. */
/*********************************************************************/
bool RemoveFunctionParser(
        const Environment::Ptr&theEnv,
        const char *functionName) {
    FunctionDefinition *fdPtr;

    fdPtr = FindFunction(theEnv, functionName);
    if (fdPtr == nullptr) {
        WriteString(theEnv, STDERR, "Function parsers can only be removed from existing functions.\n");
        return false;
    }

    fdPtr->parser = nullptr;

    return true;
}

/*****************************************************************/
/* FuncSeqOvlFlags: Makes a system function overloadable or not, */
/* i.e. can the function be a method for a generic function.     */
/*****************************************************************/
bool FuncSeqOvlFlags(
        const Environment::Ptr&theEnv,
        const char *functionName,
        bool seqp,
        bool ovlp) {
    FunctionDefinition *fdPtr;

    fdPtr = FindFunction(theEnv, functionName);
    if (fdPtr == nullptr) {
        WriteString(theEnv, STDERR, "Only existing functions can be marked as using sequence expansion arguments/overloadable or not.\n");
        return false;
    }

    fdPtr->sequenceuseok = seqp;
    fdPtr->overloadable = ovlp;

    return true;
}


/***********************************************/
/* GetNthRestriction: Returns the restriction  */
/*   type for the nth parameter of a function. */
/***********************************************/
unsigned GetNthRestriction(
        const Environment::Ptr&theEnv,
        FunctionDefinition *theFunction,
        unsigned int position) {
    unsigned rv, df;

    if (theFunction == nullptr) return (ANY_TYPE_BITS);

    if (theFunction->restrictions == nullptr) return (ANY_TYPE_BITS);
    auto restrictions = theFunction->restrictions->contents;

    PopulateRestriction(theEnv, &df, ANY_TYPE_BITS, restrictions, 0);
    PopulateRestriction(theEnv, &rv, df, restrictions, position);

    return rv;
}

/*************************************************/
/* GetFunctionList: Returns the ListOfFunctions. */
/*************************************************/
FunctionDefinition *GetFunctionList(
        const Environment::Ptr&theEnv) {
    return (ExternalFunctionData(theEnv)->ListOfFunctions);
}

/**************************************************************/
/* InstallFunctionList: Sets the ListOfFunctions and adds all */
/*   the function entries to the FunctionHashTable.           */
/**************************************************************/
void InstallFunctionList(
        const Environment::Ptr&theEnv,
        FunctionDefinition *value) {
    int i;
    struct FunctionHash *fhPtr, *nextPtr;

    if (ExternalFunctionData(theEnv)->FunctionHashtable != nullptr) {
        for (i = 0; i < SIZE_FUNCTION_HASH; i++) {
            fhPtr = ExternalFunctionData(theEnv)->FunctionHashtable[i];
            while (fhPtr != nullptr) {
                nextPtr = fhPtr->next;
                rtn_struct(theEnv, FunctionHash, fhPtr);
                fhPtr = nextPtr;
            }
            ExternalFunctionData(theEnv)->FunctionHashtable[i] = nullptr;
        }
    }

    ExternalFunctionData(theEnv)->ListOfFunctions = value;

    while (value != nullptr) {
        AddHashFunction(theEnv, value);
        value = value->next;
    }
}

/********************************************************/
/* FindFunction: Returns a pointer to the corresponding */
/*   functionDefinition structure if a function name is */
/*   in the function list, otherwise returns nullptr.      */
/********************************************************/
FunctionDefinition *FindFunction(
        const Environment::Ptr&theEnv,
        const char *functionName) {
    struct FunctionHash *fhPtr;
    size_t hashValue;
    CLIPSLexeme *findValue;

    if (ExternalFunctionData(theEnv)->FunctionHashtable == nullptr) return nullptr;

    hashValue = HashSymbol(functionName, SIZE_FUNCTION_HASH);

    findValue = FindSymbolHN(theEnv, functionName, SYMBOL_BIT);

    for (fhPtr = ExternalFunctionData(theEnv)->FunctionHashtable[hashValue];
         fhPtr != nullptr;
         fhPtr = fhPtr->next) {
        if (fhPtr->fdPtr->callFunctionName == findValue) { return (fhPtr->fdPtr); }
    }

    return nullptr;
}

/********************************************************/
/* GetUDFContext: Returns the context associated a UDF. */
/********************************************************/
void *GetUDFContext(
        const Environment::Ptr&theEnv,
        const char *functionName) {
    struct FunctionHash *fhPtr;
    size_t hashValue;
    CLIPSLexeme *findValue;

    if (ExternalFunctionData(theEnv)->FunctionHashtable == nullptr) return nullptr;

    hashValue = HashSymbol(functionName, SIZE_FUNCTION_HASH);

    findValue = FindSymbolHN(theEnv, functionName, SYMBOL_BIT);

    for (fhPtr = ExternalFunctionData(theEnv)->FunctionHashtable[hashValue];
         fhPtr != nullptr;
         fhPtr = fhPtr->next) {
        if (fhPtr->fdPtr->callFunctionName == findValue) { return nullptr; }
    }

    return nullptr;
}

/*********************************************************/
/* InitializeFunctionHashTable: Purpose is to initialize */
/*   the function hash table to nullptr.                    */
/*********************************************************/
static void InitializeFunctionHashTable(
        const Environment::Ptr&theEnv) {
    int i;

    ExternalFunctionData(theEnv)->FunctionHashtable = (FunctionHash **)
            gm2(theEnv, sizeof(FunctionHash *) *
                        SIZE_FUNCTION_HASH);

    for (i = 0; i < SIZE_FUNCTION_HASH; i++) ExternalFunctionData(theEnv)->FunctionHashtable[i] = nullptr;
}

/****************************************************************/
/* AddHashFunction: Adds a function to the function hash table. */
/****************************************************************/
static void AddHashFunction(
        const Environment::Ptr&theEnv,
        FunctionDefinition *fdPtr) {
#if STUBBING_INACTIVE
    struct FunctionHash *newhash, *temp;
    size_t hashValue;

    if (ExternalFunctionData(theEnv)->FunctionHashtable == nullptr) InitializeFunctionHashTable(theEnv);

    newhash = get_struct(theEnv, FunctionHash);
    newhash->fdPtr = fdPtr;

    hashValue = HashSymbol(fdPtr->callFunctionName->contents, SIZE_FUNCTION_HASH);

    temp = ExternalFunctionData(theEnv)->FunctionHashtable[hashValue];
    ExternalFunctionData(theEnv)->FunctionHashtable[hashValue] = newhash;
    newhash->next = temp;
#endif
}

/*************************************************/
/* GetMinimumArgs: Returns the minimum number of */
/*   arguments expected by an external function. */
/*************************************************/
int GetMinimumArgs(
        FunctionDefinition *theFunction) {
    return theFunction->minArgs;
}

/*************************************************/
/* GetMaximumArgs: Returns the maximum number of */
/*   arguments expected by an external function. */
/*************************************************/
int GetMaximumArgs(
        FunctionDefinition *theFunction) {
    return theFunction->maxArgs;
}
#if STUBBING_INACTIVE
/********************/
/* AssignErrorValue */
/********************/
void AssignErrorValue(
        UDFContext *context) {
    if (context->theFunction->unknownReturnValueType &
        BOOLEAN_BIT) { context->returnValue->lexemeValue = context->environment->FalseSymbol; }
    else if (context->theFunction->unknownReturnValueType & STRING_BIT) {
        context->returnValue->lexemeValue = CreateString(context->environment, "");
    } else if (context->theFunction->unknownReturnValueType & SYMBOL_BIT) {
        context->returnValue->lexemeValue = CreateSymbol(context->environment, "nil");
    } else if (context->theFunction->unknownReturnValueType & INTEGER_BIT) {
        context->returnValue->integerValue = CreateInteger(context->environment, 0);
    } else if (context->theFunction->unknownReturnValueType & FLOAT_BIT) {
        context->returnValue->floatValue = CreateFloat(context->environment, 0.0);
    } else if (context->theFunction->unknownReturnValueType & MULTIFIELD_BIT) {
        SetMultifieldErrorValue(context->environment, context->returnValue);
    } else if (context->theFunction->unknownReturnValueType & INSTANCE_NAME_BIT) {
        context->returnValue->lexemeValue = CreateInstanceName(context->environment, "nil");
    } else if (context->theFunction->unknownReturnValueType & FACT_ADDRESS_BIT) {
        context->returnValue->factValue = &FactData(context->environment)->DummyFact;
    } else if (context->theFunction->unknownReturnValueType & INSTANCE_ADDRESS_BIT) {
        context->returnValue->value = &InstanceData(context->environment)->DummyInstance;
    } else if (context->theFunction->unknownReturnValueType & EXTERNAL_ADDRESS_BIT) {
        context->returnValue->value = CreateExternalAddress(context->environment, nullptr, 0);
    } else { context->returnValue->value = context->environment->VoidConstant; }
}
/*********************/
/* UDFArgumentCount: */
/*********************/
unsigned int UDFArgumentCount(
        UDFContext *context) {
    unsigned int count = 0;
    Expression *argPtr;

    for (argPtr = EvaluationData(context->environment)->CurrentExpression->argList;
         argPtr != nullptr;
         argPtr = argPtr->nextArg) { count++; }

    return count;
}
#endif

/*********************/
/* UDFFirstArgument: */
/*********************/
bool UDFFirstArgument(
        UDFContext *context,
        unsigned expectedType,
        UDFValue *returnValue) {
    context->lastArg = EvaluationData(context->environment)->CurrentExpression->argList;
    context->lastPosition = 1;
    return UDFNextArgument(context, expectedType, returnValue);
}
/********************/
/* UDFNextArgument: */
/********************/
bool UDFNextArgument(
        UDFContext *context,
        unsigned expectedType,
        UDFValue *returnValue) {
#if STUBBING_INACTIVE
    Expression *argPtr = context->lastArg;
    unsigned int argumentPosition = context->lastPosition;
    const Environment::Ptr&theEnv = context->environment;

    if (argPtr == nullptr) {
        SetHaltExecution(theEnv, true);
        SetEvaluationError(theEnv, true);
        return false;
    }

    context->lastPosition++;
    context->lastArg = context->lastArg->nextArg;

    switch (argPtr->type) {
        case INTEGER_TYPE:
            returnValue->value = argPtr->value;
            if (expectedType & INTEGER_BIT) return true;
            ExpectedTypeError0(theEnv, UDFContextFunctionName(context), argumentPosition);
            PrintTypesString(theEnv, STDERR, expectedType, true);
            SetHaltExecution(theEnv, true);
            SetEvaluationError(theEnv, true);
            AssignErrorValue(context);
            return false;
            break;

        case FLOAT_TYPE:
            returnValue->value = argPtr->value;
            if (expectedType & FLOAT_BIT) return true;
            ExpectedTypeError0(theEnv, UDFContextFunctionName(context), argumentPosition);
            PrintTypesString(theEnv, STDERR, expectedType, true);
            SetHaltExecution(theEnv, true);
            SetEvaluationError(theEnv, true);
            AssignErrorValue(context);
            return false;
            break;

        case SYMBOL_TYPE:
            returnValue->value = argPtr->value;
            if (expectedType & SYMBOL_BIT) return true;
            if (expectedType & BOOLEAN_BIT) {
                if ((returnValue->lexemeValue == FalseSymbol(theEnv)) ||
                    (returnValue->lexemeValue == TrueSymbol(theEnv))) { return true; }
            }
            ExpectedTypeError0(theEnv, UDFContextFunctionName(context), argumentPosition);
            PrintTypesString(theEnv, STDERR, expectedType, true);
            SetHaltExecution(theEnv, true);
            SetEvaluationError(theEnv, true);
            AssignErrorValue(context);
            return false;
            break;

        case STRING_TYPE:
            returnValue->value = argPtr->value;
            if (expectedType & STRING_BIT) return true;
            ExpectedTypeError0(theEnv, UDFContextFunctionName(context), argumentPosition);
            PrintTypesString(theEnv, STDERR, expectedType, true);
            SetHaltExecution(theEnv, true);
            SetEvaluationError(theEnv, true);
            AssignErrorValue(context);
            return false;
            break;

        case INSTANCE_NAME_TYPE:
            returnValue->value = argPtr->value;
            if (expectedType & INSTANCE_NAME_BIT) return true;
            ExpectedTypeError0(theEnv, UDFContextFunctionName(context), argumentPosition);
            PrintTypesString(theEnv, STDERR, expectedType, true);
            SetHaltExecution(theEnv, true);
            SetEvaluationError(theEnv, true);
            AssignErrorValue(context);
            return false;
            break;
    }

    EvaluateExpression(theEnv, argPtr, returnValue);

    switch (returnValue->header->type) {
        case VOID_TYPE:
            if (expectedType & VOID_BIT) {
                if (EvaluationData(theEnv)->EvaluationError) {
                    AssignErrorValue(context);
                    return false;
                } else return true;
            }
            break;

        case INTEGER_TYPE:
            if (expectedType & INTEGER_BIT) {
                if (EvaluationData(theEnv)->EvaluationError) {
                    AssignErrorValue(context);
                    return false;
                } else return true;
            }
            break;

        case FLOAT_TYPE:
            if (expectedType & FLOAT_BIT) {
                if (EvaluationData(theEnv)->EvaluationError) {
                    AssignErrorValue(context);
                    return false;
                } else return true;
            }
            break;

        case SYMBOL_TYPE:
            if (expectedType & SYMBOL_BIT) {
                if (EvaluationData(theEnv)->EvaluationError) {
                    AssignErrorValue(context);
                    return false;
                } else return true;
            }

            if (expectedType & BOOLEAN_BIT) {
                if (EvaluationData(theEnv)->EvaluationError) {
                    AssignErrorValue(context);
                    return false;
                } else if ((returnValue->lexemeValue == FalseSymbol(theEnv)) ||
                           (returnValue->lexemeValue == TrueSymbol(theEnv))) { return true; }
            }

            break;

        case STRING_TYPE:
            if (expectedType & STRING_BIT) {
                if (EvaluationData(theEnv)->EvaluationError) {
                    AssignErrorValue(context);
                    return false;
                } else return true;
            }
            break;

        case INSTANCE_NAME_TYPE:
            if (expectedType & INSTANCE_NAME_BIT) {
                if (EvaluationData(theEnv)->EvaluationError) {
                    AssignErrorValue(context);
                    return false;
                } else return true;
            }
            break;

        case EXTERNAL_ADDRESS_TYPE:
            if (expectedType & EXTERNAL_ADDRESS_BIT) {
                if (EvaluationData(theEnv)->EvaluationError) {
                    AssignErrorValue(context);
                    return false;
                } else return true;
            }
            break;

        case FACT_ADDRESS_TYPE:
            if (expectedType & FACT_ADDRESS_BIT) {
                if (EvaluationData(theEnv)->EvaluationError) {
                    AssignErrorValue(context);
                    return false;
                } else return true;
            }
            break;

        case INSTANCE_ADDRESS_TYPE:
            if (expectedType & INSTANCE_ADDRESS_BIT) {
                if (EvaluationData(theEnv)->EvaluationError) {
                    AssignErrorValue(context);
                    return false;
                } else return true;
            }
            break;

        case MULTIFIELD_TYPE:
            if (expectedType & MULTIFIELD_BIT) {
                if (EvaluationData(theEnv)->EvaluationError) {
                    AssignErrorValue(context);
                    return false;
                } else return true;
            }
            break;
    }

    ExpectedTypeError0(theEnv, UDFContextFunctionName(context), argumentPosition);
    PrintTypesString(theEnv, STDERR, expectedType, true);

    SetHaltExecution(theEnv, true);
    SetEvaluationError(theEnv, true);
    AssignErrorValue(context);

#endif
    return false;
}

/*******************/
/* UDFNthArgument: */
/*******************/
bool UDFNthArgument(
        UDFContext *context,
        unsigned int argumentPosition,
        unsigned expectedType,
        UDFValue *returnValue) {
    if (argumentPosition < context->lastPosition) {
        context->lastArg = EvaluationData(context->environment)->CurrentExpression->argList;
        context->lastPosition = 1;
    }

    for (; (context->lastArg != nullptr) && (context->lastPosition < argumentPosition);
           context->lastArg = context->lastArg->nextArg) { context->lastPosition++; }

    return UDFNextArgument(context, expectedType, returnValue);
}

/******************************/
/* UDFInvalidArgumentMessage: */
/******************************/
void UDFInvalidArgumentMessage(
        UDFContext *context,
        const char *typeString) {
    ExpectedTypeError1(context->environment,
                       UDFContextFunctionName(context).c_str(),
                       context->lastPosition - 1, typeString);
}

/******************/
/* UDFThrowError: */
/******************/
void UDFThrowError(
        UDFContext *context) {
    const Environment::Ptr&theEnv = context->environment;

    SetHaltExecution(theEnv, true);
    SetEvaluationError(theEnv, true);
}

/***************************/
/* UDFContextFunctionName: */
/***************************/
std::string UDFContextFunctionName(UDFContext *context) {
    return context->theFunction->callFunctionName->contents;
}

/**************/
/* PrintType: */
/**************/
static void PrintType(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        int typeCount,
        int *typesPrinted,
        const char *typeName) {
    if (*typesPrinted == 0) {
        WriteString(theEnv, logicalName, typeName);
        (*typesPrinted)++;
        return;
    }

    if (typeCount == 2) { WriteString(theEnv, logicalName, " or "); }
    else if (((*typesPrinted) + 1) == typeCount) { WriteString(theEnv, logicalName, ", or "); }
    else { WriteString(theEnv, logicalName, ", "); }

    WriteString(theEnv, logicalName, typeName);
    (*typesPrinted)++;
}

/********************/
/* PrintTypesString */
/********************/
void PrintTypesString(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        unsigned expectedType,
        bool printCRLF) {
    int typeCount, typesPrinted;

    typeCount = 0;
    if (expectedType & INTEGER_BIT) typeCount++;
    if (expectedType & FLOAT_BIT) typeCount++;
    if (expectedType & (SYMBOL_BIT | BOOLEAN_BIT)) typeCount++;
    if (expectedType & STRING_BIT) typeCount++;
    if (expectedType & INSTANCE_NAME_BIT) typeCount++;
    if (expectedType & INSTANCE_ADDRESS_BIT) typeCount++;
    if (expectedType & FACT_ADDRESS_BIT) typeCount++;
    if (expectedType & EXTERNAL_ADDRESS_BIT) typeCount++;
    if (expectedType & MULTIFIELD_BIT) typeCount++;

    typesPrinted = 0;
    if (expectedType & INTEGER_BIT) { PrintType(theEnv, logicalName, typeCount, &typesPrinted, "integer"); }

    if (expectedType & FLOAT_BIT) { PrintType(theEnv, logicalName, typeCount, &typesPrinted, "float"); }

    if (expectedType & SYMBOL_BIT) { PrintType(theEnv, logicalName, typeCount, &typesPrinted, "symbol"); }
    else if (expectedType & BOOLEAN_BIT) { PrintType(theEnv, logicalName, typeCount, &typesPrinted, "boolean"); }

    if (expectedType & STRING_BIT) { PrintType(theEnv, logicalName, typeCount, &typesPrinted, "string"); }

    if (expectedType & INSTANCE_NAME_BIT) { PrintType(theEnv, logicalName, typeCount, &typesPrinted, "instance name"); }

    if (expectedType & INSTANCE_ADDRESS_BIT) { PrintType(theEnv, logicalName, typeCount, &typesPrinted, "instance address"); }

    if (expectedType & FACT_ADDRESS_BIT) { PrintType(theEnv, logicalName, typeCount, &typesPrinted, "fact address"); }

    if (expectedType & EXTERNAL_ADDRESS_BIT) { PrintType(theEnv, logicalName, typeCount, &typesPrinted, "external address"); }

    if (expectedType & MULTIFIELD_BIT) { PrintType(theEnv, logicalName, typeCount, &typesPrinted, "multifield"); }

    if (printCRLF) { WriteString(theEnv, logicalName, ".\n"); }
}
