/**
 * @brief Symbol related operations
 */
#ifndef _H_symbol

#pragma once

#define _H_symbol
#include <map>
#include <memory>
#include "Entities.hxx"

namespace maya {

#ifndef SYMBOL_HASH_SIZE
#define SYMBOL_HASH_SIZE       63559L
#endif

#ifndef FLOAT_HASH_SIZE
#define FLOAT_HASH_SIZE         8191
#endif

#ifndef INTEGER_HASH_SIZE
#define INTEGER_HASH_SIZE       8191
#endif

#ifndef BITMAP_HASH_SIZE
#define BITMAP_HASH_SIZE        8191
#endif

#ifndef EXTERNAL_ADDRESS_HASH_SIZE
#define EXTERNAL_ADDRESS_HASH_SIZE        8191
#endif
    class Ephemeron {
    public:
        using Self = Ephemeron;
        using Ptr = std::shared_ptr<Self>;
    public:
        Ephemeron(EphemeralAtom::Ptr target) : _target(target) { }
        EphemeralAtom::Ptr getAssociatedValue() const noexcept { return _target; }
    private:
        EphemeralAtom::Ptr _target;
    };
    class SymbolMatch {
    public:
        using Self = SymbolMatch;
        using Ptr = std::shared_ptr<Self>;
    public:
        SymbolMatch(Lexeme::Ptr match) : _match(match) { }
        Lexeme::Ptr getMatch() const noexcept { return _match; }
    private:
        Lexeme::Ptr _match;
    };
    class SymbolModule : public EnvironmentModule {
    public:
        template<typename T>
        using DataTable = std::multimap<size_t, typename T::Ptr>;
        static void install(Environment& env);
        static inline const std::string TrueString{"TRUE"};
        static inline const std::string FalseString{"FALSE"};
        static inline const std::string PositiveInfinityString{"+oo"};
        static inline const std::string NegativeInfinityString{"-oo"};
    public:
        SymbolModule(Environment& parent);
        Lexeme::Ptr createLexeme(const std::string& str, unsigned short type);
        inline Lexeme::Ptr createLexeme(const std::string& str, TreatAsString) { return createLexeme(str, STRING_TYPE); }
        inline Lexeme::Ptr createLexeme(const std::string& str, TreatAsSymbol) { return createLexeme(str, SYMBOL_TYPE); }
        inline Lexeme::Ptr createLexeme(const std::string& str, TreatAsInstanceName) { return createLexeme(str, INSTANCE_NAME_TYPE); }
        inline Lexeme::Ptr createString(const std::string& str) { return createLexeme(str, TreatAsString{}); }
        inline Lexeme::Ptr createSymbol(const std::string& str) { return createLexeme(str, TreatAsSymbol{}); }
        inline Lexeme::Ptr createInstanceName(const std::string& str) { return createLexeme(str, TreatAsInstanceName{}); }
        inline Lexeme::Ptr createBoolean(bool value) { return createSymbol(value ? TrueString : FalseString); }
        Float::Ptr createFloat(Float::BackingType value);
        Integer::Ptr createInteger(Integer::BackingType value);
        /// @todo bitmap support
        //BitMap::Ptr create();
        ExternalAddress::Ptr createExternalAddress(std::any contents, unsigned short kind);
        Lexeme::Ptr getTrueSymbol() const noexcept { return _trueSymbol; }
        Lexeme::Ptr getFalseSymbol() const noexcept { return _falseSymbol; }
    private:
        DataTable<Lexeme> _symbolTable;
        DataTable<Float> _floatTable;
        DataTable<Integer> _integerTable;
        //DataTable<BitMap> _bitmapTable;
        DataTable<ExternalAddress> _externalAddressTable;
        Lexeme::Ptr _positiveInfinity;
        Lexeme::Ptr _negativeInfinity;
        Integer::Ptr _zero;
        Lexeme::Ptr _trueSymbol;
        Lexeme::Ptr _falseSymbol;
        Void::Ptr _voidConstant;
    };
#if STUBBING_INACTIVE

#define IncrementLexemeCount(theValue) (theValue->retain())
#define IncrementFloatCount(theValue) (theValue->retain())
#define IncrementIntegerCount(theValue) (theValue->retain())
#define IncrementBitMapCount(theValue) (theValue->retain())
#define IncrementExternalAddressCount(theValue) (theValue->retain())

/*==================*/
/* ENVIRONMENT DATA */
/*==================*/

template<typename T, size_t capacity>
using PointerTable = std::array<std::list<typename T::Ptr>, capacity>;
struct symbolData : public EnvironmentModule {
    CLIPSLexeme::Ptr PositiveInfinity;
    CLIPSLexeme::Ptr NegativeInfinity;
    CLIPSInteger::Ptr Zero;
    PointerTable<CLIPSLexeme, SYMBOL_HASH_SIZE> SymbolTable;
    PointerTable<CLIPSFloat, FLOAT_HASH_SIZE> FloatTable;
    PointerTable<CLIPSInteger, INTEGER_HASH_SIZE> IntegerTable;
    PointerTable<CLIPSBitMap, BITMAP_HASH_SIZE> BitMapTable;
    PointerTable<CLIPSExternalAddress, EXTERNAL_ADDRESS_HASH_SIZE> ExternalAddressTable;
#if BSAVE_INSTANCES
    unsigned long NumberOfSymbols;
    unsigned long NumberOfFloats;
    unsigned long NumberOfIntegers;
    unsigned long NumberOfBitMaps;
    unsigned long NumberOfExternalAddresses;
    CLIPSLexeme **SymbolArray;
    Float **FloatArray;
    Integer **IntegerArray;
    BitMap **BitMapArray;
    ExternalAddress **ExternalAddressArray;
#endif
    CLIPSLexeme::Ptr createSymbol(const std::string& value);
    CLIPSLexeme::Ptr createString(const std::string& value);
    CLIPSLexeme::Ptr createInstanceName(const std::string& value);
    CLIPSLexeme::Ptr createBoolean(bool value);
    CLIPSInteger::Ptr createInteger(long long value);
    CLIPSFloat::Ptr createFloat(double value);
};

CLIPSLexeme::Ptr AddSymbol(const Environment::Ptr& theEnv, const std::string &contents, unsigned short type);
CLIPSLexeme *FindSymbolHN(const Environment::Ptr&, const char *, unsigned short);
CLIPSFloat::Ptr CreateFloat(const Environment::Ptr& theEnv, double value);
CLIPSInteger::Ptr CreateInteger(const Environment::Ptr& theEnv, long long value);
void *AddBitMap(const Environment::Ptr&, void *, unsigned short);
CLIPSExternalAddress::Ptr CreateExternalAddress(const Environment::Ptr& theEnv, void * ctx, unsigned short kind);
CLIPSInteger::Ptr FindLongHN(const Environment::Ptr& theEnv, long long value);
void IncrementBitMapReferenceCount(const Environment::Ptr&, CLIPSBitMap *);
void DecrementBitMapReferenceCount(const Environment::Ptr&, CLIPSBitMap *);
void ReleaseExternalAddress(const Environment::Ptr&, CLIPSExternalAddress *);
void RemoveEphemeralAtoms(const Environment::Ptr&);
void RefreshSpecialSymbols(const Environment::Ptr&);
struct symbolMatch *FindSymbolMatches(const Environment::Ptr&, const char *, unsigned *, size_t *);
void ReturnSymbolMatches(const Environment::Ptr&, struct symbolMatch *);
CLIPSLexeme *GetNextSymbolMatch(const Environment::Ptr&, const char *, size_t, CLIPSLexeme *, bool, size_t *);
void ClearBitString(void *, size_t);
void SetAtomicValueIndices(const Environment::Ptr&, bool);
void RestoreAtomicValueBuckets(const Environment::Ptr&);
void EphemerateValue(const Environment::Ptr&, void *);
bool BitStringHasBitsSet(void *, unsigned);

#define BitMapPointer(i) ((CLIPSBitMap *) (SymbolData(theEnv)->BitMapArray[i]))
#define SymbolPointer(i) ((CLIPSLexeme *) (SymbolData(theEnv)->SymbolArray[i]))
#define FloatPointer(i) ((CLIPSFloat *) (SymbolData(theEnv)->FloatArray[i]))
#define IntegerPointer(i) ((CLIPSInteger *) (SymbolData(theEnv)->IntegerArray[i]))

void MarkNeededAtomicValues(Environment);
void WriteNeededAtomicValues(const Environment::Ptr&, FILE *);
void ReadNeededAtomicValues(const Environment::Ptr&);
void InitAtomicValueNeededFlags(const Environment::Ptr&);
void FreeAtomicValueStorage(const Environment::Ptr&);
void WriteNeededSymbols(const Environment::Ptr&, FILE *);
void WriteNeededFloats(const Environment::Ptr&, FILE *);
void WriteNeededIntegers(const Environment::Ptr&, FILE *);
void ReadNeededSymbols(const Environment::Ptr&);
void ReadNeededFloats(const Environment::Ptr&);
void ReadNeededIntegers(const Environment::Ptr&);
#endif
}
#endif /* _H_symbol */



