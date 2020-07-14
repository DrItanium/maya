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

#if 0
#include "ArgumentAccess.h"
#include "CommandLine.h"
#include "Constants.h"
#include "Fact.h"
#include "MemoryAllocation.h"
#include "DefmoduleUtility.h"
#include "Router.h"
#include "ProceduralFunctions.h"
#include "Multifield.h"
#include "PrintUtility.h"
#include "Expression.h"
#include "Utility.h"
#include "ConstructProfilingFunctions.h"
#include "SystemDependency.h"

#if DEFFUNCTION_CONSTRUCT
#include "Deffunction.h"
#endif

#if DEFGENERIC_CONSTRUCT
#include "GenericFunctionCommands.h"
#endif

#include "Object.h"
#include "InstanceCommand.h"

#include "Evaluation.h"
#include "ReferenceCounted.h"
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void DeallocateEvaluationData(const Environment::Ptr&);
static void PrintCAddress(const Environment::Ptr&, const char *, void *);
static void NewCAddress(UDFContext *, UDFValue *);
/*
static bool                    DiscardCAddress(void *,void *);
*/

/**************************************************/
/* InitializeEvaluationData: Allocates environment */
/*    data for expression evaluation.             */
/**************************************************/
void InitializeEvaluationData(
        const Environment::Ptr&theEnv) {
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

void
EvaluationModule::installPrimitive(EntityRecord::Ptr record, int position) {
    if (PrimitivesArray[position]) {
        throw "Overwriting an already installed primitive";
    } else {
        PrimitivesArray[position] = record;
    }
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
    /// @todo continue
#if STUBBING_INACTIVE
    std::visit([returnValue](auto&& value) {
        using K = std::decay_t<decltype(value)>;
        if constexpr ( std::is_same_v<K, CLIPSLexeme::Ptr> ||
                std::is_same_v<K, CLIPSFloat::Ptr> ||
                std::is_same_v<K, CLIPSInteger::Ptr> ||
                std::is_same_v<K, Fact::Ptr> ||
                std::is_same_v<K, std::shared_ptr<Instance>> ||
                std::is_same_v<K, CLIPSExternalAddress::Ptr>) {
            returnValue->contents = value;
        } else if constexpr (std::is_same_v<K, std::shared_ptr<FunctionDefinition>>) {

        } else if

    }, problem->contents);
#endif
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
size_t
EvaluationModule::installExternalAddressType(const externalAddressType& newType) {
    if (numberOfAddressTypes == MAXIMUM_EXTERNAL_ADDRESS_TYPES)  {
        throw "Too many external address types defined";
    }
    auto newPtr = std::make_shared<externalAddressType>(newType);
    auto newIndex = numberOfAddressTypes;
    ExternalAddressTypes[newIndex] = newPtr;
    ++numberOfAddressTypes;
    return newIndex;
}

/*******************/
/* ResetErrorFlags */
/*******************/
void
EvaluationModule::resetErrorFlags() noexcept {
    EvaluationError = false;
    HaltExecution = false;
}
void
EvaluationModule::setEvaluationError(bool value) noexcept {
   EvaluationError = value;
   if (value) {
       HaltExecution = true;
   }
}

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

/*****************************************/
/* AtomInstall: Increments the reference */
/*   count of an atomic data type.       */
/*****************************************/
void AtomInstall(
        const Environment::Ptr&theEnv,
        unsigned short type,
        void *vPtr) {
#if STUBBING_INACTIVE
    switch (type) {
        case SYMBOL_TYPE:
        case STRING_TYPE:
#if DEFGLOBAL_CONSTRUCT
        case GBL_VARIABLE:
#endif
        case INSTANCE_NAME_TYPE:
            IncrementLexemeCount(vPtr);
            break;

        case FLOAT_TYPE:
            IncrementFloatCount(vPtr);
            break;

        case INTEGER_TYPE:
            IncrementIntegerCount(vPtr);
            break;

        case EXTERNAL_ADDRESS_TYPE:
            IncrementExternalAddressCount(vPtr);
            break;

        case MULTIFIELD_TYPE:
            RetainMultifield(theEnv, (Multifield *) vPtr);
            break;

        case VOID_TYPE:
            break;

        default:
            if (EvaluationData(theEnv)->PrimitivesArray[type] == nullptr) break;
            if (EvaluationData(theEnv)->PrimitivesArray[type]->bitMap) IncrementBitMapCount(vPtr);
            else if (EvaluationData(theEnv)->PrimitivesArray[type]->incrementBusyCount) {
                (*EvaluationData(theEnv)->PrimitivesArray[type]->incrementBusyCount)(theEnv, vPtr);
            }
            break;
    }
#endif
}
/*******************************************/
/* AtomDeinstall: Decrements the reference */
/*   count of an atomic data type.         */
/*******************************************/
void AtomDeinstall(
        const Environment::Ptr&theEnv,
        unsigned short type,
        void *vPtr) {
#if STUBBING_INACTIVE
    switch (type) {
        case SYMBOL_TYPE:
        case STRING_TYPE:
#if DEFGLOBAL_CONSTRUCT
        case GBL_VARIABLE:
#endif
        case INSTANCE_NAME_TYPE:
            ReleaseLexeme(theEnv, (CLIPSLexeme *) vPtr);
            break;

        case FLOAT_TYPE:
            ReleaseFloat(theEnv, (CLIPSFloat *) vPtr);
            break;

        case INTEGER_TYPE:
            ReleaseInteger(theEnv, (CLIPSInteger *) vPtr);
            break;

        case EXTERNAL_ADDRESS_TYPE:
            ReleaseExternalAddress(theEnv, (CLIPSExternalAddress *) vPtr);
            break;

        case MULTIFIELD_TYPE:
            ReleaseMultifield(theEnv, (Multifield *) vPtr);
            break;

        case VOID_TYPE:
            break;

        default:
            if (EvaluationData(theEnv)->PrimitivesArray[type] == nullptr) break;
            if (EvaluationData(theEnv)->PrimitivesArray[type]->bitMap) DecrementBitMapReferenceCount(theEnv, (CLIPSBitMap *) vPtr);
            else if (EvaluationData(theEnv)->PrimitivesArray[type]->decrementBusyCount) {
                (*EvaluationData(theEnv)->PrimitivesArray[type]->decrementBusyCount)(theEnv, vPtr);
            }
    }
#endif
}
#if STUBBING_INACTIVE
/***************************************************/
/* CopyDataObject: Copies the values from a source */
/*   UDFValue to a destination UDFValue.           */
/***************************************************/
void CopyDataObject(
        const Environment::Ptr&theEnv,
        UDFValue *dst,
        UDFValue *src,
        int garbageMultifield) {
    if (src->header->type != MULTIFIELD_TYPE) {
        dst->value = src->value;
    } else {
        DuplicateMultifield(theEnv, dst, src);
        if (garbageMultifield) { AddToMultifieldList(theEnv, dst->multifieldValue); }
    }
}

/***********************************************/
/* TransferDataObjectValues: Copies the values */
/*   directly from a source UDFValue to a    */
/*   destination UDFValue.                   */
/***********************************************/
void TransferDataObjectValues(
        UDFValue *dst,
        UDFValue *src) {
    dst->value = src->value;
    dst->begin = src->begin;
    dst->range = src->range;
    dst->supplementalInfo = src->supplementalInfo;
    dst->next = src->next;
}

/************************************************************************/
/* ConvertValueToExpression: Converts the value stored in a data object */
/*   into an expression. For multifield values, a chain of expressions  */
/*   is generated and the chain is linked by the nextArg field. For a   */
/*   single field value, a single expression is created.                */
/************************************************************************/
Expression *ConvertValueToExpression(
        const Environment::Ptr&theEnv,
        UDFValue *theValue) {
    size_t i;
    Expression *head = nullptr, *last = nullptr, *newItem;

    if (theValue->header->type != MULTIFIELD_TYPE) { return (GenConstant(theEnv, theValue->header->type, theValue->value)); }

    for (i = theValue->begin; i < (theValue->begin + theValue->range); i++) {
        newItem = GenConstant(theEnv, theValue->multifieldValue->contents[i].header->type,
                              theValue->multifieldValue->contents[i].value);
        if (last == nullptr) head = newItem;
        else last->nextArg = newItem;
        last = newItem;
    }

    if (head == nullptr)
        return (GenConstant(theEnv, FCALL, FindFunction(theEnv, "create$")));

    return (head);
}

/****************************************/
/* GetAtomicHashValue: Returns the hash */
/*   value for an atomic data type.     */
/****************************************/
unsigned long GetAtomicHashValue(
        unsigned short type,
        void *value,
        unsigned short position) {
    unsigned long tvalue;
    union {
        double fv;
        void *vv;
        unsigned long liv;
    } fis;

    switch (type) {
        case FLOAT_TYPE:
            fis.liv = 0;
            fis.fv = ((CLIPSFloat *) value)->contents;
            tvalue = fis.liv;
            break;

        case INTEGER_TYPE:
            tvalue = (unsigned long) ((CLIPSInteger *) value)->contents;
            break;

        case EXTERNAL_ADDRESS_TYPE:
            fis.liv = 0;
            fis.vv = ((CLIPSExternalAddress *) value)->contents;
            tvalue = fis.liv;
            break;

        case FACT_ADDRESS_TYPE:
        case INSTANCE_ADDRESS_TYPE:
            fis.liv = 0;
            fis.vv = value;
            tvalue = fis.liv;
            break;

        case STRING_TYPE:
        case INSTANCE_NAME_TYPE:
        case SYMBOL_TYPE:
            tvalue = ((CLIPSLexeme *) value)->bucket;
            break;

        default:
            tvalue = type;
    }

    return tvalue * (position + 29);
}

/***********************************************************/
/* FunctionReferenceExpression: Returns an expression with */
/*   an appropriate expression reference to the specified  */
/*   name if it is the name of a deffunction, defgeneric,  */
/*   or user/system defined function.                      */
/***********************************************************/
Expression *FunctionReferenceExpression(
        const Environment::Ptr&theEnv,
        const char *name) {
#if DEFGENERIC_CONSTRUCT
    Defgeneric *gfunc;
#endif
#if DEFFUNCTION_CONSTRUCT
    Deffunction *dptr;
#endif
    FunctionDefinition *fptr;

    /*=====================================================*/
    /* Check to see if the function call is a deffunction. */
    /*=====================================================*/

#if DEFFUNCTION_CONSTRUCT
    if ((dptr = LookupDeffunctionInScope(theEnv, name)) != nullptr) { return (GenConstant(theEnv, PCALL, dptr)); }
#endif

    /*====================================================*/
    /* Check to see if the function call is a defgeneric. */
    /*====================================================*/

#if DEFGENERIC_CONSTRUCT
    if ((gfunc = LookupDefgenericInScope(theEnv, name)) != nullptr) { return (GenConstant(theEnv, GCALL, gfunc)); }
#endif

    /*======================================*/
    /* Check to see if the function call is */
    /* a system or user defined function.   */
    /*======================================*/

    if ((fptr = FindFunction(theEnv, name)) != nullptr) { return (GenConstant(theEnv, FCALL, fptr)); }

    /*===================================================*/
    /* The specified function name is not a deffunction, */
    /* defgeneric, or user/system defined function.      */
    /*===================================================*/

    return nullptr;
}

/******************************************************************/
/* GetFunctionReference: Fills an expression with an appropriate  */
/*   expression reference to the specified name if it is the      */
/*   name of a deffunction, defgeneric, or user/system defined    */
/*   function.                                                    */
/******************************************************************/
bool GetFunctionReference(
        const Environment::Ptr&theEnv,
        const char *name,
        Expression *theReference) {
#if DEFGENERIC_CONSTRUCT
    Defgeneric *gfunc;
#endif
#if DEFFUNCTION_CONSTRUCT
    Deffunction *dptr;
#endif
    FunctionDefinition *fptr;
    bool moduleSpecified = false;
    unsigned position;
    CLIPSLexeme *moduleName = nullptr, *constructName = nullptr;

    theReference->nextArg = nullptr;
    theReference->argList = nullptr;
    theReference->type = VOID_TYPE;
    theReference->value = nullptr;

    /*==============================*/
    /* Look for a module specifier. */
    /*==============================*/

    if ((position = FindModuleSeparator(name)) != 0) {
        moduleName = ExtractModuleName(theEnv, position, name);
        constructName = ExtractConstructName(theEnv, position, name, SYMBOL_TYPE);
        moduleSpecified = true;
    }

    /*====================================================*/
    /* Check to see if the function call is a defgeneric. */
    /*====================================================*/

#if DEFGENERIC_CONSTRUCT
    if (moduleSpecified) {
        if (ConstructExported(theEnv, "defgeneric", moduleName, constructName) ||
            GetCurrentModule(theEnv) == FindDefmodule(theEnv, moduleName->contents)) {
            if ((gfunc = FindDefgenericInModule(theEnv, name)) != nullptr) {
                theReference->type = GCALL;
                theReference->value = gfunc;
                return true;
            }
        }
    } else {
        if ((gfunc = LookupDefgenericInScope(theEnv, name)) != nullptr) {
            theReference->type = GCALL;
            theReference->value = gfunc;
            return true;
        }
    }
#endif

    /*=====================================================*/
    /* Check to see if the function call is a deffunction. */
    /*=====================================================*/

#if DEFFUNCTION_CONSTRUCT
    if (moduleSpecified) {
        if (ConstructExported(theEnv, "deffunction", moduleName, constructName) ||
            GetCurrentModule(theEnv) == FindDefmodule(theEnv, moduleName->contents)) {
            if ((dptr = FindDeffunctionInModule(theEnv, name)) != nullptr) {
                theReference->type = PCALL;
                theReference->value = dptr;
                return true;
            }
        }
    } else {
        if ((dptr = LookupDeffunctionInScope(theEnv, name)) != nullptr) {
            theReference->type = PCALL;
            theReference->value = dptr;
            return true;
        }
    }
#endif

    /*======================================*/
    /* Check to see if the function call is */
    /* a system or user defined function.   */
    /*======================================*/

    if ((fptr = FindFunction(theEnv, name)) != nullptr) {
        theReference->type = FCALL;
        theReference->value = fptr;
        return true;
    }

    /*===================================================*/
    /* The specified function name is not a deffunction, */
    /* defgeneric, or user/system defined function.      */
    /*===================================================*/

    return false;
}

/***********************************************************
  NAME         : EvaluateAndStoreInDataObject
  DESCRIPTION  : Evaluates slot-value expressions
                   and stores the result in a
                   Kernel data object
  INPUTS       : 1) Flag indicating if multifields are OK
                 2) The value-expression
                 3) The data object structure
                 4) Flag indicating if a multifield value
                    should be placed on the garbage list.
  RETURNS      : False on errors, true otherwise
  SIDE EFFECTS : Segment allocated for storing
                 multifield values
  NOTES        : None
 ***********************************************************/
bool EvaluateAndStoreInDataObject(
        const Environment::Ptr&theEnv,
        bool mfp,
        Expression *theExp,
        UDFValue *val,
        bool garbageSegment) {
    val->begin = 0;
    val->range = 0;

    if (theExp == nullptr) {
        if (garbageSegment) val->value = CreateMultifield(theEnv, 0L);
        else val->value = CreateUnmanagedMultifield(theEnv, 0L);

        return true;
    }

    if (!mfp && (theExp->nextArg == nullptr))
        EvaluateExpression(theEnv, theExp, val);
    else
        StoreInMultifield(theEnv, val, theExp, garbageSegment);

    return !EvaluationData(theEnv)->EvaluationError;
}
#endif
/******************/
/* PrintCAddress: */
/******************/
static void PrintCAddress(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        void *theValue) {
#if STUBBING_INACTIVE
    char buffer[20];

    WriteString(theEnv, logicalName, "<Pointer-C-");

    gensprintf(buffer, "%p", ((CLIPSExternalAddress *) theValue)->contents);
    WriteString(theEnv, logicalName, buffer);
    WriteString(theEnv, logicalName, ">");
#endif
}

/****************/
/* NewCAddress: */
/****************/
static void NewCAddress(
        UDFContext *context,
        UDFValue *rv) {
#if STUBBING_INACTIVE
    unsigned int numberOfArguments;
    const Environment::Ptr&theEnv = context->environment;

    numberOfArguments = UDFArgumentCount(context);

    if (numberOfArguments != 1) {
        PrintErrorID(theEnv, "NEW", 1, false);
        WriteString(theEnv, STDERR, "Function new expected no additional arguments for the C external language type.\n");
        SetEvaluationError(theEnv, true);
        return;
    }

    rv->value = CreateExternalAddress(theEnv, nullptr, 0);
#endif
}
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
        CLIPSInteger *pv) {
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
    CLIPSInteger *pv = CreateInteger(theFCB->fcbEnv, intValue);

    theValue.integerValue = pv;
    FCBAppend(theFCB, &theValue);
}

/************************/
/* FCBAppendCLIPSFloat: */
/************************/
void FCBAppendCLIPSFloat(
        FunctionCallBuilder *theFCB,
        CLIPSFloat *pv) {
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
    CLIPSFloat *pv = CreateFloat(theFCB->fcbEnv, floatValue);

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
        CLIPSExternalAddress *pv) {
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
