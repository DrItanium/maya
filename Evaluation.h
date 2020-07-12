/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  11/17/17            */
/*                                                     */
/*               EVALUATION HEADER FILE                */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for evaluating expressions.    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
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
/*            UDF redesign.                                  */
/*                                                           */
/*            Removed DATA_OBJECT_ARRAY primitive type.      */
/*                                                           */
/*************************************************************/

#ifndef _H_evaluatn

#pragma once

#define _H_evaluatn

#include "Constants.h"
#include "Entities.h"
#include "Environment.h"

typedef struct functionCallBuilder FunctionCallBuilder;

struct functionCallBuilder {
    Environment fcbEnv;
    CLIPSValue *contents;
    size_t bufferReset;
    size_t length;
    size_t bufferMaximum;
};

enum FunctionCallBuilderError {
    FCBE_NO_ERROR = 0,
    FCBE_nullptr_POINTER_ERROR,
    FCBE_FUNCTION_NOT_FOUND_ERROR,
    FCBE_INVALID_FUNCTION_ERROR,
    FCBE_ARGUMENT_COUNT_ERROR,
    FCBE_ARGUMENT_TYPE_ERROR,
    FCBE_PROCESSING_ERROR
} ;


constexpr auto C_POINTER_EXTERNAL_ADDRESS = 0;
constexpr auto PARAMETERS_UNBOUNDED = USHRT_MAX;

struct externalAddressType {
    const char *name;
    void (*shortPrintFunction)(const Environment&, const char *, void *);
    void (*longPrintFunction)(const Environment&, const char *, void *);
    bool (*discardFunction)(const Environment&, void *);
    void (*newFunction)(UDFContext *, UDFValue *);
    bool (*callFunction)(UDFContext *, UDFValue *, UDFValue *);
};

#define CoerceToLongInteger(t, v) ((t == INTEGER_TYPE) ? ValueToLong(v) : (long) ValueToDouble(v))
#define CoerceToInteger(t, v) ((t == INTEGER_TYPE) ? (int) ValueToLong(v) : (int) ValueToDouble(v))
#define CoerceToDouble(t, v) ((t == INTEGER_TYPE) ? (double) ValueToLong(v) : ValueToDouble(v))

#define GetFirstArgument()           (EvaluationData(theEnv)->CurrentExpression->argList)
#define GetNextArgument(ep)          (ep->nextArg)

constexpr auto BITS_PER_BYTE = 8;
constexpr auto MAXIMUM_PRIMITIVES = 150;
constexpr auto MAXIMUM_EXTERNAL_ADDRESS_TYPES = 128;
/// @todo rewrite as templated methods once I am confident in the design
#define BitwiseTest(n, b)   (((n) & (char) (1 << (b))) ? true : false)
static_assert(BitwiseTest(0b1, 0));
static_assert(!BitwiseTest(0, 0));
static_assert(BitwiseTest(0b10, 1));
static_assert(!BitwiseTest(0b01, 1));
static_assert(BitwiseTest(0b100, 2));
static_assert(!BitwiseTest(0b010, 2));
#define BitwiseSet(n, b)    (n |= (char) (1 << (b)))
#define BitwiseClear(n, b)  (n &= (char) ~(1 << (b)))

#define CountToBitMapSize(c) (((c) + (BITS_PER_BYTE - 1)) / BITS_PER_BYTE)
#define TestBitMap(map, id)  BitwiseTest(map[(id) / BITS_PER_BYTE],(id) % BITS_PER_BYTE)
#define SetBitMap(map, id)   BitwiseSet(map[(id) / BITS_PER_BYTE],(id) % BITS_PER_BYTE)
#define ClearBitMap(map, id) BitwiseClear(map[(id) / BITS_PER_BYTE],(id) % BITS_PER_BYTE)

constexpr auto EVALUATION_DATA = 44;

struct evaluationData : public EnvironmentModule {
    Expression *CurrentExpression;
    bool EvaluationError;
    bool HaltExecution;
    int CurrentEvaluationDepth;
    int numberOfAddressTypes;
    EntityRecord *PrimitivesArray[MAXIMUM_PRIMITIVES];
    struct externalAddressType *ExternalAddressTypes[MAXIMUM_EXTERNAL_ADDRESS_TYPES];
};
RegisterEnvironmentModule(evaluationData, EVALUATION_DATA, Evaluation);

void InitializeEvaluationData(const Environment&);
bool EvaluateExpression(const Environment&, Expression *, UDFValue *);
void SetEvaluationError(const Environment&, bool);
bool GetEvaluationError(const Environment&);
void SetHaltExecution(const Environment&, bool);
bool GetHaltExecution(const Environment&);
void ReturnValues(const Environment&, UDFValue *, bool);
void WriteUDFValue(const Environment&, const char *, UDFValue *);
void WriteCLIPSValue(const Environment&, const char *, CLIPSValue *);
void SetMultifieldErrorValue(const Environment&, UDFValue *);
void CopyDataObject(const Environment&, UDFValue *, UDFValue *, int);
void AtomInstall(const Environment&, unsigned short, void *);
void AtomDeinstall(const Environment&, unsigned short, void *);
void Retain(const Environment&, TypeHeader *);
void Release(const Environment&, TypeHeader *);
void RetainCV(const Environment&, CLIPSValue *);
void ReleaseCV(const Environment&, CLIPSValue *);
void RetainUDFV(const Environment&, UDFValue *);
void ReleaseUDFV(const Environment&, UDFValue *);
Expression *ConvertValueToExpression(const Environment&, UDFValue *);
unsigned long GetAtomicHashValue(unsigned short, void *, unsigned short);
void InstallPrimitive(const Environment&, EntityRecord *, int);
int InstallExternalAddressType(const Environment&, struct externalAddressType *);
void TransferDataObjectValues(UDFValue *, UDFValue *);
Expression *FunctionReferenceExpression(const Environment&, const char *);
bool GetFunctionReference(const Environment&, const char *, Expression *);
bool DOsEqual(UDFValue *, UDFValue *);
bool EvaluateAndStoreInDataObject(const Environment&, bool, Expression *, UDFValue *, bool);
void ResetErrorFlags(const Environment&);
FunctionCallBuilder *CreateFunctionCallBuilder(const Environment&, size_t);
void FCBAppendUDFValue(FunctionCallBuilder *, UDFValue *);
void FCBAppend(FunctionCallBuilder *, CLIPSValue *);
void FCBAppendCLIPSInteger(FunctionCallBuilder *, CLIPSInteger *);
void FCBAppendInteger(FunctionCallBuilder *, long long);
void FCBAppendCLIPSFloat(FunctionCallBuilder *, CLIPSFloat *);
void FCBAppendFloat(FunctionCallBuilder *, double);
void FCBAppendCLIPSLexeme(FunctionCallBuilder *, CLIPSLexeme *);
void FCBAppendSymbol(FunctionCallBuilder *, const char *);
void FCBAppendString(FunctionCallBuilder *, const char *);
void FCBAppendInstanceName(FunctionCallBuilder *, const char *);
void FCBAppendCLIPSExternalAddress(FunctionCallBuilder *, CLIPSExternalAddress *);
void FCBAppendFact(FunctionCallBuilder *, Fact *);
void FCBAppendInstance(FunctionCallBuilder *, Instance *);
void FCBAppendMultifield(FunctionCallBuilder *, Multifield *);
void FCBDispose(FunctionCallBuilder *);
void FCBReset(FunctionCallBuilder *);
FunctionCallBuilderError FCBCall(FunctionCallBuilder *, const char *, CLIPSValue *);

//#define CVIsType(cv, cvType) (((1 << (((TypeHeader *) (cv)->value)->type)) & (cvType)) ? true : false)
bool CVIsType(CLIPSValue::Ptr target, unsigned short type);
#define CVIsLexeme(cv) (CVIsType(cv, LEXEME_BITS))
#define CVIsInstanceName(cv) (CVIsType(cv, INSTANCE_NAME_BIT))
#define CVIsFloat(cv) (CVIsType(cv, FLOAT_BIT))
#define CVIsInteger(cv) (CVIsType(cv, INTEGER_BIT))
#define CVIsSymbol(cv) (CVIsType(cv, SYMBOL_BIT))
#define CVIsFactAddress(cv) (CVIsType(cv, FACT_ADDRESS_BIT))
#define CVIsInstanceAddress(cv) (CVIsType(cv, INSTANCE_ADDRESS_BIT))
#define CVIsString(cv) (CVIsType(cv, STRING_BIT))
#define CVIsNumber(cv) (CVIsType(cv, NUMBER_BITS))
#define CVIsExternalAddress(cv) (CVIsType(cv, EXTERNAL_ADDRESS_BIT))
#define CVIsMultifield(cv) (CVIsType(cv, MULTIFIELD_BIT))
#define ValueIsType(value, vType) ((1 << (((TypeHeader *) value)->type)) & (vType))

double CVCoerceToFloat(CLIPSValue::Ptr ptr);
double CVCoerceToFloat(CLIPSValue& ref);
double CVCoerceToFloat(CLIPSValue* ref);
double CVCoerceToFloat(UDFValue::Ptr ptr);
double CVCoerceToFloat(UDFValue& ref);
double CVCoerceToFloat(UDFValue* ref);
long long CVCoerceToInteger(CLIPSValue::Ptr ptr);
long long CVCoerceToInteger(CLIPSValue& ptr);
long long CVCoerceToInteger(CLIPSValue* ptr);
long long CVCoerceToInteger(UDFValue::Ptr ptr);
long long CVCoerceToInteger(UDFValue& ptr);
long long CVCoerceToInteger(UDFValue* ptr);

#if STUBBING_INACTIVE
#define CVCoerceToFloat(cv) (((cv)->header->type == FLOAT_TYPE) ? \
                             ((cv)->floatValue->contents) : \
                             ((double) (cv)->integerValue->contents))

#define CVCoerceToInteger(cv) (((cv)->header->type == INTEGER_TYPE) ? \
                               ((cv)->integerValue->contents) : \
                               ((long long) (cv)->floatValue->contents))
#endif

#endif /* _H_evaluatn */
