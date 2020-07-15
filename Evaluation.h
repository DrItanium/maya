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
#include "Entities.hxx"
#include "Environment.h"
namespace maya {
#if STUBBING_INACTIVE
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

    FunctionCallBuilder *CreateFunctionCallBuilder(const Environment::Ptr&, size_t);
    void FCBAppendUDFValue(FunctionCallBuilder *, UDFValue *);
    void FCBAppend(FunctionCallBuilder *, CLIPSValue *);
    void FCBAppendCLIPSInteger(FunctionCallBuilder *, Integer *);
    void FCBAppendInteger(FunctionCallBuilder *, long long);
    void FCBAppendCLIPSFloat(FunctionCallBuilder *, Float *);
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
#endif

    constexpr auto C_POINTER_EXTERNAL_ADDRESS = 0;
    constexpr auto PARAMETERS_UNBOUNDED = USHRT_MAX;

    struct externalAddressType {
    public:
        using Self = externalAddressType;
        using Ptr = std::shared_ptr<Self>;
    public:
        std::string name;
        EnvironmentPtrNoReturnFunction<const std::string &, std::any> shortPrintFunction;
        EnvironmentPtrNoReturnFunction<const std::string &, std::any> longPrintFunction;
        EnvironmentPtrFunction<bool, std::any> discardFunction;
        std::function<void(UDFContext::Ptr, UDFValue::Ptr)> newFunction;
        std::function<bool(UDFContext::Ptr, UDFValue::Ptr, UDFValue::Ptr)> callFunction;
        void shortPrint(const Environment::Ptr &env, const std::string &logicalName, std::any context);
        void longPrint(const Environment::Ptr &env, const std::string &logicalName, std::any context);
        bool discard(const Environment::Ptr &env, std::any context);
        void create(UDFContext::Ptr context, UDFValue::Ptr container);
        bool call(UDFContext::Ptr, UDFValue::Ptr);
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
    template<typename T, typename R, T pattern, T shift = static_cast<T>(0)>
    constexpr R decode(T value) noexcept {
        using KT = std::decay_t<T>;
        using KR = std::decay_t<R>;
        auto maskedOutPortion = (value & pattern);
        if constexpr (std::is_same_v<KR, bool> && std::is_integral_v<KT>) {
            // exploit boolean convertibility in the case where we are dealing with integral to boolean conversion
            return value & pattern;
        } else {
            return static_cast<R>((value & pattern) >> shift);
        }
    }

    template<typename T, typename R, T pattern, T shift = static_cast<T>(0)>
    constexpr T encode(T value, R input) noexcept {
        return (value & ~pattern) | ((static_cast<T>(input) << shift) & pattern);
    }

    static_assert(encode<int, short, 0b10, 1>(0b1101, 0b0001) == 0b1111);
    static_assert(encode<int, int, 0x0000'000F, 0>(0xFDED, 0x44) == 0xFDE4);

    template<typename T, T position>
    constexpr bool bitwiseTest(T value) noexcept {
        return decode<T, bool, static_cast<T>(1) << position>(value);
    }
    static_assert(bitwiseTest<int, 0>(0b1));
    static_assert(!bitwiseTest<int, 0>(0));
    static_assert(bitwiseTest<int, 1>(0b10));
    static_assert(!bitwiseTest<int, 1>(0b01));
    static_assert(bitwiseTest<int, 2>(0b100));
    static_assert(!bitwiseTest<int, 2>(0b010));
    template<typename T, T position>
    constexpr T bitwiseSet(T value) {
        return encode<T, bool, (static_cast<T>(1) << position), position>(value, true);
    }
    static_assert(bitwiseSet<int, 0>(0b1010) == 0b1011);
    static_assert(bitwiseSet<int, 1>(0b1010) == 0b1010);
    static_assert(bitwiseSet<int, 2>(0b1010) == 0b1110);
    static_assert(bitwiseSet<int, 3>(0b1010) == 0b1010);
    static_assert(bitwiseSet<int, 3>(0xFDED) == 0xFDED);
    template<typename T, T position>
    constexpr T bitwiseClear(T value) noexcept {
        return decode<T, T, ~(static_cast<T>(1) << position)>(value);
    }
    static_assert(bitwiseClear<int, 0>(0b1111) == 0b1110);
    static_assert(bitwiseClear<int, 1>(0b1111) == 0b1101);
    static_assert(bitwiseClear<int, 2>(0b1111) == 0b1011);
    static_assert(bitwiseClear<int, 3>(0b1111) == 0b0111);

    constexpr size_t countToBitmapSize(size_t size) {
        return (size + (BITS_PER_BYTE - 1)) / BITS_PER_BYTE;
    }

#define TestBitMap(map, id)  (bitwiseTest<decltype(id), id % BITS_PER_BYTE>(map[(id) / BITS_PER_BYTE]))
#define SetBitMap(map, id)   (bitwiseSet<decltype(id), id % BITS_PER_BYTE>(map[(id) / BITS_PER_BYTE]))
#define ClearBitMap(map, id) (bitwiseSet<decltype(id), id % BITS_PER_BYTE>(map [(id) / BITS_PER_BYTE]))

    constexpr auto EVALUATION_DATA = 44;

    struct EvaluationModule : public EnvironmentModule {
        EvaluationModule(Environment &parent) : EnvironmentModule(parent) {}
        std::shared_ptr<Expression> CurrentExpression;
        bool EvaluationError = false;
        bool HaltExecution = false;
        int CurrentEvaluationDepth = 0;
        size_t numberOfAddressTypes = 0;
        std::array<EntityRecord::Ptr, MAXIMUM_PRIMITIVES> PrimitivesArray;
        /// @todo make this a std::list instead to remove hardcoded limits
        std::array<externalAddressType::Ptr, MAXIMUM_EXTERNAL_ADDRESS_TYPES> ExternalAddressTypes;
        void installPrimitive(EntityRecord::Ptr record, int position);
        size_t installExternalAddressType(const externalAddressType &newType);
        void resetErrorFlags() noexcept;
        void setEvaluationError(bool value) noexcept;
        constexpr auto getEvaluationError() const noexcept { return EvaluationError; }
        constexpr auto shouldHaltExecution() const noexcept { return HaltExecution; }
        void haltExecution(bool value = true) noexcept { HaltExecution = value; }
    };

    RegisterEnvironmentModule(EvaluationModule, EVALUATION_DATA, Evaluation);

    void InitializeEvaluationData(const Environment::Ptr &);
    bool EvaluateExpression(const Environment::Ptr &, Expression *, UDFValue *);
    void WriteUDFValue(const Environment::Ptr &, const char *, UDFValue *);
    void WriteCLIPSValue(const Environment::Ptr &, const char *, CLIPSValue *);
    void SetMultifieldErrorValue(const Environment::Ptr &, UDFValue *);
    void CopyDataObject(const Environment::Ptr &, UDFValue *, UDFValue *, int);
    void AtomInstall(const Environment::Ptr &, unsigned short, void *);
    void AtomDeinstall(const Environment::Ptr &, unsigned short, void *);
    Expression *ConvertValueToExpression(const Environment::Ptr &, UDFValue *);
    unsigned long GetAtomicHashValue(unsigned short, void *, unsigned short);
    void TransferDataObjectValues(UDFValue *, UDFValue *);
    Expression *FunctionReferenceExpression(const Environment::Ptr &, const char *);
    bool GetFunctionReference(const Environment::Ptr &, const char *, Expression *);
    bool EvaluateAndStoreInDataObject(const Environment::Ptr &, bool, Expression *, UDFValue *, bool);

#if STUBBING_INACTIVE
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

#define CVCoerceToFloat(cv) (((cv)->header->type == FLOAT_TYPE) ? \
                             ((cv)->floatValue->contents) : \
                             ((double) (cv)->integerValue->contents))

#define CVCoerceToInteger(cv) (((cv)->header->type == INTEGER_TYPE) ? \
                               ((cv)->integerValue->contents) : \
                               ((long long) (cv)->floatValue->contents))
#endif
} // end namespace maya
#endif /* _H_evaluatn */
