/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  10/01/16            */
/*                                                     */
/*                 SYMBOL HEADER FILE                  */
/*******************************************************/

/*************************************************************/
/* Purpose: Manages the atomic data value hash tables for    */
/*   storing symbols, integers, floats, and bit maps.        */
/*   Contains routines for adding entries, examining the     */
/*   hash tables, and performing garbage collection to       */
/*   remove entries no longer in use.                        */
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
/*      6.24: CLIPS crashing on AMD64 processor in the       */
/*            function used to generate a hash value for     */
/*            integers. DR0871                               */
/*                                                           */
/*            Support for run-time programs directly passing */
/*            the hash tables for initialization.            */
/*                                                           */
/*            Corrected code generating compilation          */
/*            warnings.                                      */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Support for hashing EXTERNAL_ADDRESS_TYPE      */
/*            data type.                                     */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Used genstrcpy instead of strcpy.              */
/*                                                           */
/*            Added support for external address hash table  */
/*            and subtyping.                                 */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Added ValueToPointer and EnvValueToPointer     */
/*            macros.                                        */
/*                                                           */
/*      6.40: Refactored code to reduce header dependencies  */
/*            in sysdep.c.                                   */
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
/*************************************************************/

#ifndef _H_symbol

#pragma once

#define _H_symbol

#include <cstdlib>
#include <cstdio>

#include "Entities.h"

typedef struct genericHashNode GENERIC_HN;

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

/******************************/
/* genericHashNode STRUCTURE: */
/******************************/
struct genericHashNode {
    TypeHeader header;
    struct genericHashNode *next;
    long count;
    bool permanent: 1;
    bool markedEphemeral: 1;
    bool needed: 1;
    unsigned int bucket: 29;
};

/**********************************************************/
/* EPHEMERON STRUCTURE: Data structure used to keep track */
/*   of ephemeral symbols, floats, and integers.          */
/*                                                        */
/*   associatedValue: Contains a pointer to the storage   */
/*   structure for the symbol, float, or integer which is */
/*   ephemeral.                                           */
/*                                                        */
/*   next: Contains a pointer to the next ephemeral item  */
/*   in a list of ephemeral items.                        */
/**********************************************************/
struct ephemeron {
    GENERIC_HN *associatedValue;
    struct ephemeron *next;
};

/***************/
/* symbolMatch */
/***************/
struct symbolMatch {
    CLIPSLexeme *match;
    struct symbolMatch *next;
};
//#define IncrementLexemeCount(theValue) (((CLIPSLexeme *) theValue)->count++)
//#define IncrementFloatCount(theValue) (((CLIPSFloat *) theValue)->count++)
//#define IncrementIntegerCount(theValue) (((CLIPSInteger *) theValue)->count++)
//#define IncrementBitMapCount(theValue) (((CLIPSBitMap *) theValue)->count++)
//#define IncrementExternalAddressCount(theValue) (((CLIPSExternalAddress *) theValue)->count++)

#define IncrementLexemeCount(theValue) (theValue->retain())
#define IncrementFloatCount(theValue) (theValue->retain())
#define IncrementIntegerCount(theValue) (theValue->retain())
#define IncrementBitMapCount(theValue) (theValue->retain())
#define IncrementExternalAddressCount(theValue) (theValue->retain())

/*==================*/
/* ENVIRONMENT DATA */
/*==================*/

constexpr auto SYMBOL_DATA = 49;

struct symbolData : public EnvironmentModule {
    CLIPSLexeme *PositiveInfinity;
    CLIPSLexeme *NegativeInfinity;
    CLIPSInteger *Zero;
    CLIPSLexeme **SymbolTable;
    CLIPSFloat **FloatTable;
    CLIPSInteger **IntegerTable;
    CLIPSBitMap **BitMapTable;
    CLIPSExternalAddress **ExternalAddressTable;
#if BSAVE_INSTANCES
    unsigned long NumberOfSymbols;
    unsigned long NumberOfFloats;
    unsigned long NumberOfIntegers;
    unsigned long NumberOfBitMaps;
    unsigned long NumberOfExternalAddresses;
    CLIPSLexeme **SymbolArray;
    CLIPSFloat **FloatArray;
    CLIPSInteger **IntegerArray;
    CLIPSBitMap **BitMapArray;
    CLIPSExternalAddress **ExternalAddressArray;
#endif
};
RegisterEnvironmentModule(symbolData, SYMBOL_DATA, Symbol);

void InitializeAtomTables(const Environment&, CLIPSLexeme **, CLIPSFloat **,
                          CLIPSInteger **, CLIPSBitMap **,
                          CLIPSExternalAddress **);
CLIPSLexeme::Ptr AddSymbol(const Environment& theEnv, const char * contents, unsigned short type);
CLIPSLexeme *FindSymbolHN(const Environment&, const char *, unsigned short);
CLIPSFloat::Ptr CreateFloat(const Environment& theEnv, double value);
CLIPSInteger::Ptr CreateInteger(const Environment& theEnv, long long value);
void *AddBitMap(const Environment&, void *, unsigned short);
CLIPSExternalAddress::Ptr CreateExternalAddress(const Environment& theEnv, void * ctx, unsigned short kind);
CLIPSExternalAddress::Ptr CreateCExternalAddress(const Environment& theEnv, void * ctx);
CLIPSInteger *FindLongHN(const Environment&, long long);
size_t HashSymbol(const char *, size_t);
size_t HashFloat(double, size_t);
size_t HashInteger(long long, size_t);
size_t HashBitMap(const char *, size_t, unsigned);
size_t HashExternalAddress(void *, size_t);
void RetainLexeme(const Environment&, CLIPSLexeme *);
void RetainFloat(const Environment&, CLIPSFloat *);
void RetainInteger(const Environment&, CLIPSInteger *);
void IncrementBitMapReferenceCount(const Environment&, CLIPSBitMap *);
void RetainExternalAddress(const Environment&, CLIPSExternalAddress *);
void ReleaseLexeme(const Environment&, CLIPSLexeme *);
void ReleaseFloat(const Environment&, CLIPSFloat *);
void ReleaseInteger(const Environment&, CLIPSInteger *);
void DecrementBitMapReferenceCount(const Environment&, CLIPSBitMap *);
void ReleaseExternalAddress(const Environment&, CLIPSExternalAddress *);
void RemoveEphemeralAtoms(const Environment&);
CLIPSLexeme **GetSymbolTable(const Environment&);
void SetSymbolTable(const Environment&, CLIPSLexeme **);
CLIPSFloat **GetFloatTable(const Environment&);
void SetFloatTable(const Environment&, CLIPSFloat **);
CLIPSInteger **GetIntegerTable(const Environment&);
void SetIntegerTable(const Environment&, CLIPSInteger **);
CLIPSBitMap **GetBitMapTable(const Environment&);
void SetBitMapTable(const Environment&, CLIPSBitMap **);
CLIPSExternalAddress **GetExternalAddressTable(const Environment&);
void SetExternalAddressTable(const Environment&, CLIPSExternalAddress **);
void RefreshSpecialSymbols(const Environment&);
struct symbolMatch *FindSymbolMatches(const Environment&, const char *, unsigned *, size_t *);
void ReturnSymbolMatches(const Environment&, struct symbolMatch *);
CLIPSLexeme *GetNextSymbolMatch(const Environment&, const char *, size_t, CLIPSLexeme *, bool, size_t *);
void ClearBitString(void *, size_t);
void SetAtomicValueIndices(const Environment&, bool);
void RestoreAtomicValueBuckets(const Environment&);
void EphemerateValue(const Environment&, void *);
CLIPSLexeme::Ptr CreateSymbol(const Environment& theEnv, const char * str);
CLIPSLexeme::Ptr CreateString(const Environment& theEnv, const char * str);
CLIPSLexeme::Ptr CreateInstanceName(const Environment& theEnv, const char * str);
CLIPSLexeme::Ptr CreateBoolean(const Environment& theEnv, bool value);
bool BitStringHasBitsSet(void *, unsigned);

#define BitMapPointer(i) ((CLIPSBitMap *) (SymbolData(theEnv)->BitMapArray[i]))
#define SymbolPointer(i) ((CLIPSLexeme *) (SymbolData(theEnv)->SymbolArray[i]))
#define FloatPointer(i) ((CLIPSFloat *) (SymbolData(theEnv)->FloatArray[i]))
#define IntegerPointer(i) ((CLIPSInteger *) (SymbolData(theEnv)->IntegerArray[i]))

void MarkNeededAtomicValues(Environment);
void WriteNeededAtomicValues(const Environment&, FILE *);
void ReadNeededAtomicValues(const Environment&);
void InitAtomicValueNeededFlags(const Environment&);
void FreeAtomicValueStorage(const Environment&);
void WriteNeededSymbols(const Environment&, FILE *);
void WriteNeededFloats(const Environment&, FILE *);
void WriteNeededIntegers(const Environment&, FILE *);
void ReadNeededSymbols(const Environment&);
void ReadNeededFloats(const Environment&);
void ReadNeededIntegers(const Environment&);
#endif /* _H_symbol */



