/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  12/07/17             */
/*                                                     */
/*                  EVALUATION MODULE                  */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for evaluating expressions.    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added EvaluateAndStoreInDataObject function.   */
/*                                                           */
/*      6.30: Added support for passing context information  */
/*            to user defined functions.                     */
/*                                                           */
/*            Added support for external address hash table  */
/*            and subtyping.                                 */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Support for DATA_OBJECT_ARRAY primitive.       */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
/*                                                           */
/*            Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
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
/*            Removed DATA_OBJECT_ARRAY primitive type.      */
/*                                                           */
/*            Modified GetFunctionReference to handle module */
/*            specifier for funcall.                         */
/*                                                           */
/*************************************************************/


#include "Setup.h"
#include "Environment.h"
#include "Evaluation.h"
#include "Problem.h"
#include "ExternalFunctions.h"

namespace maya {
/**************************************************/
/* InitializeEvaluationData: Allocates environment */
/*    data for expression evaluation.             */
/**************************************************/
    void InitializeEvaluationData(
            const Environment::Ptr &theEnv) {
#if STUBBING_INACTIVE
        struct externalAddressType cPointer = {"C", PrintCAddress, PrintCAddress, nullptr, NewCAddress, nullptr};
        /// @todo move deallocate evaluation data into the dtor of EvaluationModule
        //AllocateEnvironmentData(theEnv, EVALUATION_DATA, sizeof(EvaluationModule), DeallocateEvaluationData);
        theEnv->allocateEnvironmentModule<>()
        auto ptr = std::make_unique<EvaluationModule>();
        theEnv->installEnvironmentModule(std::move(ptr));
        InstallExternalAddressType(theEnv, &cPointer);
#endif
    }
#if STUBBING_INACTIVE
    void
    EvaluationModule::installPrimitive(EntityRecord::Ptr record, int position) {
        if (PrimitivesArray[position]) {
            throw Problem("Overwriting an already installed primitive");
        } else {
            PrimitivesArray[position] = record;
        }
    }
#endif
bool
Expression::evaluate(UDFValue::Ptr returnValue) {
   return std::visit([this, &returnValue](auto&& executable) {
        using K = std::decay_t<decltype(executable)>;
        if constexpr (std::is_same_v<K, Evaluable::Ptr>) {
            return executable->evaluate(returnValue);
        } else if (std::is_same_v<K, ExternalFunction::Ptr>) {
            UDFContext ctx(getParent(), executable, _args, returnValue);
            return executable->evaluate(ctx, returnValue);
        } else {
            /// @todo turn this into a compile time error
            throw Problem("Undefined type passed to the Expression container!");
            return false;
        }
   }, _contents);

}
#if STUBBING_INACTIVE
    /**************************************************************/
    /* EvaluateExpression: Evaluates an expression. Returns false */
    /*   if no errors occurred during evaluation, otherwise true. */
    /**************************************************************/
    bool EvaluateExpression(
            const Environment::Ptr&theEnv,
            Expression *problem,
            UDFValue *returnValue) {
        Expression *oldArgument;
        FunctionDefinition *fptr;
        UDFContext theUDFContext;
#if PROFILING_FUNCTIONS
        struct profileFrameInfo profileFrame;
#endif

        returnValue->contents = VoidConstant(theEnv);
        returnValue->begin = 0;
        returnValue->range = SIZE_MAX;

        if (problem == nullptr) {
            returnValue->contents = FalseSymbol(theEnv);
            return (EvaluationData(theEnv)->EvaluationError);
        }
        switch (problem->type) {
            case STRING_TYPE:
            case SYMBOL_TYPE:
            case FLOAT_TYPE:
            case INTEGER_TYPE:
            case INSTANCE_NAME_TYPE:
            case INSTANCE_ADDRESS_TYPE:
            case FACT_ADDRESS_TYPE:
            case EXTERNAL_ADDRESS_TYPE:
                returnValue->contents = problem->value;
                break;

            case FCALL: {
                fptr = problem->functionValue;

#if PROFILING_FUNCTIONS
                StartProfile(theEnv, &profileFrame,
                             &fptr->usrData,
                             ProfileFunctionData(theEnv)->ProfileUserFunctions);
#endif

                oldArgument = EvaluationData(theEnv)->CurrentExpression;
                EvaluationData(theEnv)->CurrentExpression = problem;

                theUDFContext.environment = theEnv;
                theUDFContext.theFunction = fptr;
                theUDFContext.lastArg = problem->argList;
                theUDFContext.lastPosition = 1;
                theUDFContext.returnValue = returnValue;
                fptr->functionPointer(theEnv, &theUDFContext, returnValue);
                if ((returnValue->header->type == MULTIFIELD_TYPE) &&
                    (returnValue->range == SIZE_MAX)) { returnValue->range = returnValue->multifieldValue->length; }

#if PROFILING_FUNCTIONS
                EndProfile(theEnv, &profileFrame);
#endif

                EvaluationData(theEnv)->CurrentExpression = oldArgument;
                break;
            }

            case MULTIFIELD_TYPE:
                returnValue->value = ((UDFValue *) (problem->value))->value;
                returnValue->begin = ((UDFValue *) (problem->value))->begin;
                returnValue->range = ((UDFValue *) (problem->value))->range;
                break;

            case MF_VARIABLE:
            case SF_VARIABLE:
                if (!GetBoundVariable(theEnv, returnValue, problem->lexemeValue)) {
                    PrintErrorID(theEnv, "EVALUATN", 1, false);
                    WriteString(theEnv, STDERR, "Variable ");
                    if (problem->type == MF_VARIABLE) { WriteString(theEnv, STDERR, "$?"); }
                    else { WriteString(theEnv, STDERR, "?"); }
                    WriteString(theEnv, STDERR, problem->lexemeValue->contents);
                    WriteString(theEnv, STDERR, " is unbound.\n");
                    returnValue->value = FalseSymbol(theEnv);
                    SetEvaluationError(theEnv, true);
                }
                break;

            default:
                if (EvaluationData(theEnv)->PrimitivesArray[problem->type] == nullptr) {
                    SystemError(theEnv, "EVALUATN", 3);
                    ExitRouter(theEnv, EXIT_FAILURE);
                }

                if (EvaluationData(theEnv)->PrimitivesArray[problem->type]->copyToEvaluate) {
                    returnValue->value = problem->value;
                    break;
                }

                if (EvaluationData(theEnv)->PrimitivesArray[problem->type]->evaluateFunction == nullptr) {
                    SystemError(theEnv, "EVALUATN", 4);
                    ExitRouter(theEnv, EXIT_FAILURE);
                }

                oldArgument = EvaluationData(theEnv)->CurrentExpression;
                EvaluationData(theEnv)->CurrentExpression = problem;

#if PROFILING_FUNCTIONS
                StartProfile(theEnv, &profileFrame,
                             &EvaluationData(theEnv)->PrimitivesArray[problem->type]->usrData,
                             ProfileFunctionData(theEnv)->ProfileUserFunctions);
#endif

                (*EvaluationData(theEnv)->PrimitivesArray[problem->type]->evaluateFunction)(theEnv, problem->value, returnValue);

#if PROFILING_FUNCTIONS
                EndProfile(theEnv, &profileFrame);
#endif

                EvaluationData(theEnv)->CurrentExpression = oldArgument;
                break;
        }

        return EvaluationData(theEnv)->EvaluationError;
    }
#endif
#if STUBBING_INACTIVE
    size_t
    EvaluationModule::installExternalAddressType(const externalAddressType &newType) {
        if (numberOfAddressTypes == MAXIMUM_EXTERNAL_ADDRESS_TYPES) {
            throw Problem("Too many external address types defined");
        }
        auto newPtr = std::make_shared<externalAddressType>(newType);
        auto newIndex = numberOfAddressTypes;
        ExternalAddressTypes[newIndex] = newPtr;
        ++numberOfAddressTypes;
        return newIndex;
    }
#endif

#if STUBBING_INACTIVE
    /**************************************************/
    /* WriteCLIPSValue: Prints a CLIPSValue structure */
    /*   to the specified logical name.               */
    /**************************************************/
    void WriteCLIPSValue(
            const Environment::Ptr&theEnv,
            const char *fileid,
            CLIPSValue *argPtr) {
        switch (argPtr->header->type) {
            case VOID_TYPE:
            case SYMBOL_TYPE:
            case STRING_TYPE:
            case INTEGER_TYPE:
            case FLOAT_TYPE:
            case EXTERNAL_ADDRESS_TYPE:
            case FACT_ADDRESS_TYPE:
            case INSTANCE_NAME_TYPE:
            case INSTANCE_ADDRESS_TYPE:
                PrintAtom(theEnv, fileid, argPtr->header->type, argPtr->value);
                break;

            case MULTIFIELD_TYPE:
                PrintMultifieldDriver(theEnv, fileid, argPtr->multifieldValue,
                                      0, argPtr->multifieldValue->length, true);
                break;

            default:
                WriteString(theEnv, fileid, "<UnknownPrintType");
                WriteInteger(theEnv, fileid, argPtr->header->type);
                WriteString(theEnv, fileid, ">");
                SetHaltExecution(theEnv, true);
                SetEvaluationError(theEnv, true);
                break;
        }
    }

    /**********************************************/
    /* WriteUDFValue: Prints a UDFValue structure */
    /*   to the specified logical name.           */
    /**********************************************/
    void WriteUDFValue(
            const Environment::Ptr&theEnv,
            const char *fileid,
            UDFValue *argPtr) {
        switch (argPtr->header->type) {
            case VOID_TYPE:
            case SYMBOL_TYPE:
            case STRING_TYPE:
            case INTEGER_TYPE:
            case FLOAT_TYPE:
            case EXTERNAL_ADDRESS_TYPE:
            case FACT_ADDRESS_TYPE:
            case INSTANCE_NAME_TYPE:
            case INSTANCE_ADDRESS_TYPE:
                PrintAtom(theEnv, fileid, argPtr->header->type, argPtr->value);
                break;

            case MULTIFIELD_TYPE:
                PrintMultifieldDriver(theEnv, fileid, argPtr->multifieldValue,
                                      argPtr->begin, argPtr->range, true);
                break;

            default:
                WriteString(theEnv, fileid, "<UnknownPrintType");
                WriteInteger(theEnv, fileid, argPtr->header->type);
                WriteString(theEnv, fileid, ">");
                SetHaltExecution(theEnv, true);
                SetEvaluationError(theEnv, true);
                break;
        }
    }

    /*************************************************/
    /* SetMultifieldErrorValue: Creates a multifield */
    /*   value of length zero for error returns.     */
    /*************************************************/
    void SetMultifieldErrorValue(
            const Environment::Ptr&theEnv,
            UDFValue *returnValue) {
        returnValue->value = CreateMultifield(theEnv, 0L);
        returnValue->begin = 0;
        returnValue->range = 0;
    }
#endif


#if STUBBING_INACTIVE
/******************************/
/* CreateFunctionCallBuilder: */
/******************************/
FunctionCallBuilder *CreateFunctionCallBuilder(
        const Environment::Ptr&theEnv,
        size_t theSize) {
    FunctionCallBuilder *theFC;

    if (theEnv == nullptr) return nullptr;

    theFC = get_struct(theEnv, functionCallBuilder);

    theFC->fcbEnv = theEnv;
    theFC->bufferReset = theSize;
    theFC->bufferMaximum = theSize;
    theFC->length = 0;

    if (theSize == 0) { theFC->contents = nullptr; }
    else { theFC->contents = (CLIPSValue *) gm2(theEnv, sizeof(CLIPSValue) * theSize); }

    return theFC;
}

/**********************/
/* FCBAppendUDFValue: */
/**********************/
void FCBAppendUDFValue(
        FunctionCallBuilder *theFCB,
        UDFValue *theValue) {
    const Environment::Ptr&theEnv = theFCB->fcbEnv;
    size_t i, neededSize, newSize;
    CLIPSValue *newArray;

    /*==============================================*/
    /* A void value can't be added to a multifield. */
    /*==============================================*/

    if (theValue->header->type == VOID_TYPE) { return; }

    /*=======================================*/
    /* Determine the amount of space needed. */
    /*=======================================*/

    neededSize = theFCB->length + 1;

    /*============================================*/
    /* Increase the size of the buffer if needed. */
    /*============================================*/

    if (neededSize > theFCB->bufferMaximum) {
        newSize = neededSize * 2;

        newArray = (CLIPSValue *) gm2(theEnv, sizeof(CLIPSValue) * newSize);

        for (i = 0; i < theFCB->length; i++) { newArray[i] = theFCB->contents[i]; }

        if (theFCB->bufferMaximum != 0) { rm(theFCB->fcbEnv, theFCB->contents, sizeof(CLIPSValue) * theFCB->bufferMaximum); }

        theFCB->bufferMaximum = newSize;
        theFCB->contents = newArray;
    }

    /*==================================*/
    /* Copy the new value to the array. */
    /*==================================*/

    if (theValue->header->type == MULTIFIELD_TYPE) {
        CLIPSValue newValue;

        UDFToCLIPSValue(theEnv, theValue, &newValue);
        theFCB->contents[theFCB->length].value = newValue.value;
    } else { theFCB->contents[theFCB->length].value = theValue->value; }

    Retain(theEnv, theFCB->contents[theFCB->length].header);
    theFCB->length++;
}

/**************/
/* FCBAppend: */
/**************/
void FCBAppend(
        FunctionCallBuilder *theFCB,
        CLIPSValue *theValue) {
    const Environment::Ptr&theEnv = theFCB->fcbEnv;
    size_t i, neededSize, newSize;
    CLIPSValue *newArray;

    /*==============================================*/
    /* A void value can't be added to a multifield. */
    /*==============================================*/

    if (theValue->header->type == VOID_TYPE) { return; }

    /*=======================================*/
    /* Determine the amount of space needed. */
    /*=======================================*/

    neededSize = theFCB->length + 1;

    /*============================================*/
    /* Increase the size of the buffer if needed. */
    /*============================================*/

    if (neededSize > theFCB->bufferMaximum) {
        newSize = neededSize * 2;

        newArray = (CLIPSValue *) gm2(theEnv, sizeof(CLIPSValue) * newSize);

        for (i = 0; i < theFCB->length; i++) { newArray[i] = theFCB->contents[i]; }

        if (theFCB->bufferMaximum != 0) { rm(theFCB->fcbEnv, theFCB->contents, sizeof(CLIPSValue) * theFCB->bufferMaximum); }

        theFCB->bufferMaximum = newSize;
        theFCB->contents = newArray;
    }

    /*===================================*/
    /* Copy the new values to the array. */
    /*===================================*/

    theFCB->contents[theFCB->length].value = theValue->value;
    Retain(theEnv, theFCB->contents[theFCB->length].header);
    theFCB->length++;
}

/**************************/
/* FCBAppendCLIPSInteger: */
/**************************/
void FCBAppendCLIPSInteger(
        FunctionCallBuilder *theFCB,
        Integer *pv) {
    CLIPSValue theValue;

    theValue.integerValue = pv;
    FCBAppend(theFCB, &theValue);
}

/*********************/
/* FCBAppendInteger: */
/*********************/
void FCBAppendInteger(
        FunctionCallBuilder *theFCB,
        long long intValue) {
    CLIPSValue theValue;
    Integer *pv = CreateInteger(theFCB->fcbEnv, intValue);

    theValue.integerValue = pv;
    FCBAppend(theFCB, &theValue);
}

/************************/
/* FCBAppendCLIPSFloat: */
/************************/
void FCBAppendCLIPSFloat(
        FunctionCallBuilder *theFCB,
        Float *pv) {
    CLIPSValue theValue;

    theValue.floatValue = pv;
    FCBAppend(theFCB, &theValue);
}

/*******************/
/* FCBAppendFloat: */
/*******************/
void FCBAppendFloat(
        FunctionCallBuilder *theFCB,
        double floatValue) {
    CLIPSValue theValue;
    Float *pv = CreateFloat(theFCB->fcbEnv, floatValue);

    theValue.floatValue = pv;
    FCBAppend(theFCB, &theValue);
}

/*************************/
/* FCBAppendCLIPSLexeme: */
/*************************/
void FCBAppendCLIPSLexeme(
        FunctionCallBuilder *theFCB,
        CLIPSLexeme *pv) {
    CLIPSValue theValue;

    theValue.lexemeValue = pv;
    FCBAppend(theFCB, &theValue);
}

/********************/
/* FCBAppendSymbol: */
/********************/
void FCBAppendSymbol(
        FunctionCallBuilder *theFCB,
        const char *strValue) {
    CLIPSValue theValue;
    CLIPSLexeme *pv = CreateSymbol(theFCB->fcbEnv, strValue);

    theValue.lexemeValue = pv;
    FCBAppend(theFCB, &theValue);
}

/********************/
/* FCBAppendString: */
/********************/
void FCBAppendString(
        FunctionCallBuilder *theFCB,
        const char *strValue) {
    CLIPSValue theValue;
    CLIPSLexeme *pv = CreateString(theFCB->fcbEnv, strValue);

    theValue.lexemeValue = pv;
    FCBAppend(theFCB, &theValue);
}

/**************************/
/* FCBAppendInstanceName: */
/**************************/
void FCBAppendInstanceName(
        FunctionCallBuilder *theFCB,
        const char *strValue) {
    CLIPSValue theValue;
    CLIPSLexeme *pv = CreateInstanceName(theFCB->fcbEnv, strValue);

    theValue.lexemeValue = pv;
    FCBAppend(theFCB, &theValue);
}

/**********************************/
/* FCBAppendCLIPSExternalAddress: */
/**********************************/
void FCBAppendCLIPSExternalAddress(
        FunctionCallBuilder *theFCB,
        ExternalAddress *pv) {
    CLIPSValue theValue;

    theValue.externalAddressValue = pv;
    FCBAppend(theFCB, &theValue);
}

/******************/
/* FCBAppendFact: */
/******************/
void FCBAppendFact(
        FunctionCallBuilder *theFCB,
        Fact *pv) {
    CLIPSValue theValue;

    theValue.factValue = pv;
    FCBAppend(theFCB, &theValue);
}

/**********************/
/* FCBAppendInstance: */
/**********************/
void FCBAppendInstance(
        FunctionCallBuilder *theFCB,
        Instance *pv) {
    CLIPSValue theValue;

    theValue.instanceValue = pv;
    FCBAppend(theFCB, &theValue);
}

/************************/
/* FCBAppendMultifield: */
/************************/
void FCBAppendMultifield(
        FunctionCallBuilder *theFCB,
        Multifield *pv) {
    CLIPSValue theValue;

    theValue.multifieldValue = pv;
    FCBAppend(theFCB, &theValue);
}

/***********/
/* FCBCall */
/***********/
FunctionCallBuilderError FCBCall(
        FunctionCallBuilder *theFCB,
        const char *functionName,
        CLIPSValue *returnValue) {
    Environment theEnv;
    Expression theReference, *lastAdd = nullptr, *nextAdd, *multiAdd;
    FunctionDefinition *theFunction = nullptr;
    size_t i, j;
    UDFValue udfReturnValue;
    GCBlock gcb;

    /*==========================*/
    /* Check for nullptr pointers. */
    /*==========================*/

    if ((theFCB == nullptr) || (functionName == nullptr)) { return FCBE_nullptr_POINTER_ERROR; }

    /*======================================*/
    /* Check to see if the function exists. */
    /*======================================*/

    if (!GetFunctionReference(theFCB->fcbEnv, functionName, &theReference)) { return FCBE_FUNCTION_NOT_FOUND_ERROR; }

    /*============================================*/
    /* Functions with specialized parsers  cannot */
    /* be used with a FunctionCallBuilder.        */
    /*============================================*/

    if (theReference.type == FCALL) {
        theFunction = FindFunction(theFCB->fcbEnv, functionName);
        if (theFunction->parser != nullptr) { return FCBE_INVALID_FUNCTION_ERROR; }
    }

    /*=======================================*/
    /* Append the arguments for the function */
    /* call to the expression.               */
    /*=======================================*/

    theEnv = theFCB->fcbEnv;

    for (i = 0; i < theFCB->length; i++) {
        /*====================================================*/
        /* Multifield values have to be dynamically recreated */
        /* through a create$ expression call.                 */
        /*====================================================*/

        if (theFCB->contents[i].header->type == MULTIFIELD_TYPE) {
            nextAdd = GenConstant(theEnv, FCALL, FindFunction(theEnv, "create$"));

            if (lastAdd == nullptr) { theReference.argList = nextAdd; }
            else { lastAdd->nextArg = nextAdd; }

            lastAdd = nextAdd;

            multiAdd = nullptr;
            for (j = 0; j < theFCB->contents[i].multifieldValue->length; j++) {
                nextAdd = GenConstant(theEnv, theFCB->contents[i].multifieldValue->contents[j].header->type,
                                      theFCB->contents[i].multifieldValue->contents[j].value);

                if (multiAdd == nullptr) { lastAdd->argList = nextAdd; }
                else { multiAdd->nextArg = nextAdd; }
                multiAdd = nextAdd;
            }
        }

            /*================================================================*/
            /* Single field values can just be appended to the argument list. */
            /*================================================================*/

        else {
            nextAdd = GenConstant(theEnv, theFCB->contents[i].header->type, theFCB->contents[i].value);

            if (lastAdd == nullptr) { theReference.argList = nextAdd; }
            else { lastAdd->nextArg = nextAdd; }
            lastAdd = nextAdd;
        }
    }

    ExpressionInstall(theEnv, &theReference);

    /*===========================================================*/
    /* Verify a deffunction has the correct number of arguments. */
    /*===========================================================*/

#if DEFFUNCTION_CONSTRUCT
    if (theReference.type == PCALL) {
        if (!CheckDeffunctionCall(theEnv, (Deffunction *) theReference.value, CountArguments(theReference.argList))) {
            ExpressionDeinstall(theEnv, &theReference);
            ReturnExpression(theEnv, theReference.argList);
            return FCBE_ARGUMENT_COUNT_ERROR;
        }
    }
#endif

    /*=========================================*/
    /* Verify the correct number of arguments. */
    /*=========================================*/

// TBD Support run time check of arguments
    if (theReference.type == FCALL) {
        FunctionArgumentsError theError;
        if ((theError = CheckExpressionAgainstRestrictions(theEnv, &theReference, theFunction, functionName)) != FAE_NO_ERROR) {
            ExpressionDeinstall(theEnv, &theReference);
            ReturnExpression(theEnv, theReference.argList);
            if (theError == FAE_TYPE_ERROR) return FCBE_ARGUMENT_TYPE_ERROR;
            else if (theError == FAE_COUNT_ERROR) return FCBE_ARGUMENT_COUNT_ERROR;
            else {
                SystemError(theEnv, "EVALUATN", 9);
                ExitRouter(theEnv, EXIT_FAILURE);
            }
        }
    }
    /*========================================*/
    /* Set up the frame for tracking garbage. */
    /*========================================*/

    GCBlockStart(theEnv, &gcb);

    /*=====================================*/
    /* If embedded, clear the error flags. */
    /*=====================================*/

    if (EvaluationData(theEnv)->CurrentExpression == nullptr) { ResetErrorFlags(theEnv); }

    /*======================*/
    /* Call the expression. */
    /*======================*/

    EvaluateExpression(theEnv, &theReference, &udfReturnValue);

    /*====================================================*/
    /* Convert a partial multifield to a full multifield. */
    /*====================================================*/

    NormalizeMultifield(theEnv, &udfReturnValue);

    /*========================================*/
    /* Return the expression data structures. */
    /*========================================*/

    ExpressionDeinstall(theEnv, &theReference);
    ReturnExpression(theEnv, theReference.argList);

    /*================================*/
    /* Restore the old garbage frame. */
    /*================================*/

    if (returnValue != nullptr) { GCBlockEndUDF(theEnv, &gcb, &udfReturnValue); }
    else { GCBlockEnd(theEnv, &gcb); }

    /*==========================================*/
    /* Perform periodic cleanup if the eval was */
    /* issued from an embedded controller.      */
    /*==========================================*/

    if (EvaluationData(theEnv)->CurrentExpression == nullptr) {
        if (returnValue != nullptr) { CleanCurrentGarbageFrame(theEnv, &udfReturnValue); }
        else { CleanCurrentGarbageFrame(theEnv, nullptr); }
        CallPeriodicTasks(theEnv);
    }

    if (returnValue != nullptr) { returnValue->value = udfReturnValue.value; }

    if (GetEvaluationError(theEnv)) return FCBE_PROCESSING_ERROR;

    return FCBE_NO_ERROR;
}

/*************/
/* FCBReset: */
/*************/
void FCBReset(
        FunctionCallBuilder *theFCB) {
    size_t i;

    for (i = 0; i < theFCB->length; i++) { Release(theFCB->fcbEnv, theFCB->contents[i].header); }

    if (theFCB->bufferReset != theFCB->bufferMaximum) {
        if (theFCB->bufferMaximum != 0) { rm(theFCB->fcbEnv, theFCB->contents, sizeof(CLIPSValue) * theFCB->bufferMaximum); }

        if (theFCB->bufferReset == 0) { theFCB->contents = nullptr; }
        else { theFCB->contents = (CLIPSValue *) gm2(theFCB->fcbEnv, sizeof(CLIPSValue) * theFCB->bufferReset); }

        theFCB->bufferMaximum = theFCB->bufferReset;
    }

    theFCB->length = 0;
}

/***************/
/* FCBDispose: */
/***************/
void FCBDispose(
        FunctionCallBuilder *theFCB) {
    const Environment::Ptr&theEnv = theFCB->fcbEnv;
    size_t i;

    for (i = 0; i < theFCB->length; i++) { Release(theFCB->fcbEnv, theFCB->contents[i].header); }

    if (theFCB->bufferMaximum != 0) { rm(theFCB->fcbEnv, theFCB->contents, sizeof(CLIPSValue) * theFCB->bufferMaximum); }

    rtn_struct(theEnv, multifieldBuilder, theFCB);
}

/*******************************/
/* DiscardCAddress: TBD Remove */
/*******************************/
/*
static bool DiscardCAddress(
  const Environment::Ptr&theEnv,
  void *theValue)
  {
   WriteString(theEnv,STDOUT,"Discarding C Address\n");

   return true;
  }
*/
#endif
}
