/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  11/20/17             */
/*                                                     */
/*                    SYMBOL MODULE                    */
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
/*      Brian L. Dantes                                      */
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
/*      6.40: Refactored code to reduce header dependencies  */
/*            in sysdep.c.                                   */
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
/*************************************************************/

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "Setup.h"

#include "ArgumentAccess.h"
#include "Constants.h"
#include "Environment.h"
#include "MemoryAllocation.h"
#include "Multifield.h"
#include "PrintUtility.h"
#include "Router.h"
#include "SystemDependency.h"
#include "Utility.h"

#include "Symbol.h"
#include "ReferenceCounted.h"

/***************/
/* DEFINITIONS */
/***************/

#define FALSE_STRING "FALSE"
#define TRUE_STRING  "TRUE"
#define POSITIVE_INFINITY_STRING "+oo"
#define NEGATIVE_INFINITY_STRING "-oo"

constexpr auto AVERAGE_STRING_SIZE = 10;
constexpr auto AVERAGE_BITMAP_SIZE = sizeof(long);
constexpr auto NUMBER_OF_LONGS_FOR_HASH = 25;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void RemoveHashNode(const Environment&, genericHashNode::Ptr *, genericHashNode::Ptr **, int, int);
static void AddEphemeralHashNode(const Environment&, genericHashNode::Ptr *, struct ephemeron **,
                                 int, int, bool);
static void RemoveEphemeralHashNodes(const Environment&, struct ephemeron **,
                                     genericHashNode::Ptr **,
                                     int, int, int);
static const char *StringWithinString(const char *, const char *);
static size_t CommonPrefixLength(const char *, const char *);
static void DeallocateSymbolData(const Environment&);

/*******************************************************/
/* InitializeAtomTables: Initializes the SymbolTable,  */
/*   IntegerTable, and FloatTable. It also initializes */
/*   the TrueSymbol and FalseSymbol.                   */
/*******************************************************/
void InitializeAtomTables(const Environment&theEnv) {
    //AllocateEnvironmentData(theEnv, SYMBOL_DATA, sizeof(symbolData), DeallocateSymbolData);
    theEnv->allocateEnvironmentModule<symbolData>();

    /*========================*/
    /* Predefine some values. */
    /*========================*/

    theEnv->TrueSymbol = AddSymbol(theEnv, TRUE_STRING, SYMBOL_TYPE);
    IncrementLexemeCount(TrueSymbol(theEnv));
    theEnv->FalseSymbol = AddSymbol(theEnv, FALSE_STRING, SYMBOL_TYPE);
    IncrementLexemeCount(FalseSymbol(theEnv));
    SymbolData(theEnv)->PositiveInfinity = AddSymbol(theEnv, POSITIVE_INFINITY_STRING, SYMBOL_TYPE);
    IncrementLexemeCount(SymbolData(theEnv)->PositiveInfinity);
    SymbolData(theEnv)->NegativeInfinity = AddSymbol(theEnv, NEGATIVE_INFINITY_STRING, SYMBOL_TYPE);
    IncrementLexemeCount(SymbolData(theEnv)->NegativeInfinity);
    SymbolData(theEnv)->Zero = CreateInteger(theEnv, 0LL);
    IncrementIntegerCount(SymbolData(theEnv)->Zero);

    theEnv->VoidConstant = std::make_shared<CLIPSVoid>();
}

#if STUBBING_INACTIVE
/*************************************************/
/* DeallocateSymbolData: Deallocates environment */
/*    data for symbols.                          */
/*************************************************/
static void DeallocateSymbolData(
        const Environment&theEnv) {
    CLIPSLexeme *shPtr, *nextSHPtr;
    CLIPSInteger *ihPtr, *nextIHPtr;
    CLIPSFloat *fhPtr, *nextFHPtr;
    CLIPSBitMap *bmhPtr, *nextBMHPtr;
    CLIPSExternalAddress *eahPtr, *nextEAHPtr;
    if ((SymbolData(theEnv)->SymbolTable == nullptr) ||
        (SymbolData(theEnv)->FloatTable == nullptr) ||
        (SymbolData(theEnv)->IntegerTable == nullptr) ||
        (SymbolData(theEnv)->BitMapTable == nullptr) ||
        (SymbolData(theEnv)->ExternalAddressTable == nullptr)) { return; }

    genfree(theEnv, theEnv->VoidConstant, sizeof(TypeHeader));

    for (int i = 0; i < SYMBOL_HASH_SIZE; i++) {
        shPtr = SymbolData(theEnv)->SymbolTable[i];

        while (shPtr != nullptr) {
            nextSHPtr = shPtr->next;
            if (!shPtr->permanent) {
                rm(theEnv, (void *) shPtr->contents, strlen(shPtr->contents) + 1);
                rtn_struct(theEnv, CLIPSLexeme, shPtr);
            }
            shPtr = nextSHPtr;
        }
    }

    for (int i = 0; i < FLOAT_HASH_SIZE; i++) {
        fhPtr = SymbolData(theEnv)->FloatTable[i];

        while (fhPtr != nullptr) {
            nextFHPtr = fhPtr->next;
            if (!fhPtr->permanent) { rtn_struct(theEnv, CLIPSFloat, fhPtr); }
            fhPtr = nextFHPtr;
        }
    }

    for (int i = 0; i < INTEGER_HASH_SIZE; i++) {
        ihPtr = SymbolData(theEnv)->IntegerTable[i];

        while (ihPtr != nullptr) {
            nextIHPtr = ihPtr->next;
            if (!ihPtr->permanent) { rtn_struct(theEnv, CLIPSInteger, ihPtr); }
            ihPtr = nextIHPtr;
        }
    }

    for (int i = 0; i < BITMAP_HASH_SIZE; i++) {
        bmhPtr = SymbolData(theEnv)->BitMapTable[i];

        while (bmhPtr != nullptr) {
            nextBMHPtr = bmhPtr->next;
            if (!bmhPtr->permanent) {
                rm(theEnv, (void *) bmhPtr->contents, bmhPtr->size);
                rtn_struct(theEnv, CLIPSBitMap, bmhPtr);
            }
            bmhPtr = nextBMHPtr;
        }
    }

    for (int i = 0; i < EXTERNAL_ADDRESS_HASH_SIZE; i++) {
        eahPtr = SymbolData(theEnv)->ExternalAddressTable[i];

        while (eahPtr != nullptr) {
            nextEAHPtr = eahPtr->next;
            if (!eahPtr->permanent) {
                rtn_struct(theEnv, CLIPSExternalAddress, eahPtr);
            }
            eahPtr = nextEAHPtr;
        }
    }

    /*================================*/
    /* Remove the symbol hash tables. */
    /*================================*/

    rm(theEnv, SymbolData(theEnv)->SymbolTable, sizeof(CLIPSLexeme *) * SYMBOL_HASH_SIZE);

    genfree(theEnv, SymbolData(theEnv)->FloatTable, sizeof(CLIPSFloat *) * FLOAT_HASH_SIZE);

    genfree(theEnv, SymbolData(theEnv)->IntegerTable, sizeof(CLIPSInteger *) * INTEGER_HASH_SIZE);

    genfree(theEnv, SymbolData(theEnv)->BitMapTable, sizeof(CLIPSBitMap *) * BITMAP_HASH_SIZE);

    genfree(theEnv, SymbolData(theEnv)->ExternalAddressTable, sizeof(CLIPSExternalAddress *) * EXTERNAL_ADDRESS_HASH_SIZE);

    /*==============================*/
    /* Remove binary symbol tables. */
    /*==============================*/

#if BSAVE_INSTANCES
    if (SymbolData(theEnv)->SymbolArray != nullptr)
        rm(theEnv, SymbolData(theEnv)->SymbolArray, sizeof(CLIPSLexeme *) * SymbolData(theEnv)->NumberOfSymbols);
    if (SymbolData(theEnv)->FloatArray != nullptr)
        rm(theEnv, SymbolData(theEnv)->FloatArray, sizeof(CLIPSFloat *) * SymbolData(theEnv)->NumberOfFloats);
    if (SymbolData(theEnv)->IntegerArray != nullptr)
        rm(theEnv, SymbolData(theEnv)->IntegerArray, sizeof(CLIPSInteger *) * SymbolData(theEnv)->NumberOfIntegers);
    if (SymbolData(theEnv)->BitMapArray != nullptr)
        rm(theEnv, SymbolData(theEnv)->BitMapArray, sizeof(CLIPSBitMap *) * SymbolData(theEnv)->NumberOfBitMaps);
#endif
}
#endif

/*****************/
/* CreateBoolean */
/*****************/
CLIPSLexeme::Ptr CreateBoolean(
        const Environment&theEnv,
        bool value) {
    if (value) { return TrueSymbol(theEnv); }
    else { return FalseSymbol(theEnv); }
}

/****************/
/* CreateSymbol */
/****************/
CLIPSLexeme::Ptr CreateSymbol(
        const Environment&theEnv,
        const char *str) {
    return AddSymbol(theEnv, str, SYMBOL_TYPE);
}

/****************/
/* CreateString */
/****************/
CLIPSLexeme::Ptr CreateString(
        const Environment&theEnv,
        const char *str) {
    return AddSymbol(theEnv, str, STRING_TYPE);
}

/**********************/
/* CreateInstanceName */
/**********************/
CLIPSLexeme::Ptr CreateInstanceName(
        const Environment&theEnv,
        const char *str) {
    return AddSymbol(theEnv, str, INSTANCE_NAME_TYPE);
}

/********************************************************************/
/* AddSymbol: Searches for the string in the symbol table. If the   */
/*   string is already in the symbol table, then the address of the */
/*   string's location in the symbol table is returned. Otherwise,  */
/*   the string is added to the symbol table and then the address   */
/*   of the string's location in the symbol table is returned.      */
/********************************************************************/
CLIPSLexeme::Ptr AddSymbol(
        const Environment&theEnv,
        const std::string &contents,
        unsigned short type) {

    /*====================================*/
    /* Get the hash value for the string. */
    /*====================================*/
//    if (!contents) {
//        SystemError(theEnv, "SYMBOL", 1);
//        ExitRouter(theEnv, EXIT_FAILURE);
//    }

    auto tally = HashSymbol(contents, SYMBOL_HASH_SIZE);

    /*==================================================*/
    /* Search for the string in the list of entries for */
    /* this symbol table location.  If the string is    */
    /* found, then return the address of the string.    */
    /*==================================================*/
    for (auto& peek : SymbolData(theEnv)->SymbolTable[tally]) {
        std::string str (peek->contents);
        if (peek->header.type == type && (str == contents)) {
            return peek;
        }
    }

    /*==================================================*/
    /* Add the string at the end of the list of entries */
    /* for this symbol table location.                  */
    /*==================================================*/

    auto newEntry = getStruct<CLIPSLexeme>(theEnv);
    SymbolData(theEnv)->SymbolTable[tally].emplace_back(newEntry);

#if 0
    auto length = strlen(contents) + 1;
    buffer = (char *) gm2(theEnv, length);
    genstrcpy(buffer, contents);
    peek->contents = buffer;
    peek->next = nullptr;
    peek->bucket = (unsigned int) tally;
    peek->count = 0;
    peek->permanent = false;
    peek->header.type = type;
#endif
    newEntry->header.type = type;
    newEntry->setIsPermanent(false);
    newEntry->setBucket((unsigned int)tally);
    newEntry->contents = contents;

    /*================================================*/
    /* Add the string to the list of ephemeral items. */
    /*================================================*/
#if STUBBING_INACTIVE
    AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) peek, &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralSymbolList,
                         sizeof(CLIPSLexeme), AVERAGE_STRING_SIZE, true);
    UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
#endif
    /*===================================*/
    /* Return the address of the symbol. */
    /*===================================*/

    return newEntry;
}
/*****************************************************************/
/* FindSymbolHN: Searches for the string in the symbol table and */
/*   returns a pointer to it if found, otherwise returns nullptr.   */
/*****************************************************************/
CLIPSLexeme *FindSymbolHN(
        const Environment&theEnv,
        const char *str,
        unsigned short expectedType) {
#if STUBBING_INACTIVE
    size_t tally;
    CLIPSLexeme *peek;

    tally = HashSymbol(str, SYMBOL_HASH_SIZE);

    for (peek = SymbolData(theEnv)->SymbolTable[tally];
         peek != nullptr;
         peek = peek->next) {
        if (((1 << peek->header.type) & expectedType) &&
            (strcmp(str, peek->contents) == 0)) { return peek; }
    }
#endif

    return nullptr;
}
#if STUBBING_INACTIVE
/******************************************************************/
/* CreateFloat: Searches for the double in the hash table. If the */
/*   double is already in the hash table, then the address of the */
/*   double is returned. Otherwise, the double is hashed into the */
/*   table and the address of the double is also returned.        */
/******************************************************************/
CLIPSFloat::Ptr CreateFloat(
        const Environment&theEnv,
        double value) {
    size_t tally;
    CLIPSFloat *past = nullptr, *peek;

    /*====================================*/
    /* Get the hash value for the double. */
    /*====================================*/

    tally = HashFloat(value, FLOAT_HASH_SIZE);
    peek = SymbolData(theEnv)->FloatTable[tally];

    /*==================================================*/
    /* Search for the double in the list of entries for */
    /* this hash location.  If the double is found,     */
    /* then return the address of the double.           */
    /*==================================================*/

    while (peek != nullptr) {
        if (value == peek->contents) { return peek; }
        past = peek;
        peek = peek->next;
    }

    /*=================================================*/
    /* Add the float at the end of the list of entries */
    /* for this hash location.                         */
    /*=================================================*/

    peek = get_struct(theEnv, CLIPSFloat);

    if (past == nullptr) SymbolData(theEnv)->FloatTable[tally] = peek;
    else past->next = peek;

    peek->contents = value;
    peek->next = nullptr;
    peek->bucket = (unsigned int) tally;
    peek->count = 0;
    peek->permanent = false;
    peek->header.type = FLOAT_TYPE;

    /*===============================================*/
    /* Add the float to the list of ephemeral items. */
    /*===============================================*/

    AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) peek, &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralFloatList,
                         sizeof(CLIPSFloat), 0, true);
    UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;

    /*==================================*/
    /* Return the address of the float. */
    /*==================================*/

    return peek;
}
#endif
/****************************************************************/
/* CreateInteger: Searches for the long in the hash table. If   */
/*   the long is already in the hash table, then the address of */
/*   the long is returned. Otherwise, the long is hashed into   */
/*   the table and the address of the long is also returned.    */
/****************************************************************/
CLIPSInteger::Ptr CreateInteger(
        const Environment&theEnv,
        long long value) {

    /*==================================*/
    /* Get the hash value for the long. */
    /*==================================*/

    auto tally = HashInteger(value, INTEGER_HASH_SIZE);
    /*================================================*/
    /* Search for the long in the list of entries for */
    /* this hash location. If the long is found, then */
    /* return the address of the long.                */
    /*================================================*/
    for (auto& peek : SymbolData(theEnv)->IntegerTable[tally]) {
        if (value == peek->contents) {
            return peek;
        }
    }

    /*================================================*/
    /* Add the long at the end of the list of entries */
    /* for this hash location.                        */
    /*================================================*/
    auto newEntry = getStruct<CLIPSInteger>(theEnv);
    SymbolData(theEnv)->IntegerTable[tally].emplace_back(newEntry);
    newEntry->contents = value;
    newEntry->setBucket((unsigned int) tally);
    newEntry->setIsPermanent(false);
    newEntry->header.type = INTEGER_TYPE;

    /*=================================================*/
    /* Add the integer to the list of ephemeral items. */
    /*=================================================*/

#if STUBBING_INACTIVE
    AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) peek, &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralIntegerList,
                         sizeof(CLIPSInteger), 0, true);
    UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
#endif

    /*====================================*/
    /* Return the address of the integer. */
    /*====================================*/

    return newEntry;
}
#if STUBBING_INACTIVE
/*****************************************************************/
/* FindLongHN: Searches for the integer in the integer table and */
/*   returns a pointer to it if found, otherwise returns nullptr.   */
/*****************************************************************/
CLIPSInteger *FindLongHN(
        const Environment&theEnv,
        long long theLong) {
    size_t tally;
    CLIPSInteger *peek;

    tally = HashInteger(theLong, INTEGER_HASH_SIZE);

    for (peek = SymbolData(theEnv)->IntegerTable[tally];
         peek != nullptr;
         peek = peek->next) { if (peek->contents == theLong) return (peek); }

    return nullptr;
}

/******************************************************************/
/* AddBitMap: Searches for the bitmap in the hash table. If the   */
/*   bitmap is already in the hash table, then the address of the */
/*   bitmap is returned. Otherwise, the bitmap is hashed into the */
/*   table and the address of the bitmap is also returned.        */
/******************************************************************/
void *AddBitMap(
        const Environment&theEnv,
        void *vTheBitMap,
        unsigned short size) {
    char *theBitMap = (char *) vTheBitMap;
    size_t tally;
    unsigned short i;
    CLIPSBitMap *past = nullptr, *peek;
    char *buffer;

    /*====================================*/
    /* Get the hash value for the bitmap. */
    /*====================================*/

    if (theBitMap == nullptr) {
        SystemError(theEnv, "SYMBOL", 2);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    tally = HashBitMap(theBitMap, BITMAP_HASH_SIZE, size);
    peek = SymbolData(theEnv)->BitMapTable[tally];

    /*==================================================*/
    /* Search for the bitmap in the list of entries for */
    /* this hash table location.  If the bitmap is      */
    /* found, then return the address of the bitmap.    */
    /*==================================================*/

    while (peek != nullptr) {
        if (peek->size == size) {
            for (i = 0; i < size; i++) { if (peek->contents[i] != theBitMap[i]) break; }

            if (i == size) return ((void *) peek);
        }

        past = peek;
        peek = peek->next;
    }

    /*==================================================*/
    /* Add the bitmap at the end of the list of entries */
    /* for this hash table location.  Return the        */
    /*==================================================*/

    peek = get_struct(theEnv, CLIPSBitMap);
    if (past == nullptr) SymbolData(theEnv)->BitMapTable[tally] = peek;
    else past->next = peek;

    buffer = (char *) gm2(theEnv, size);
    for (i = 0; i < size; i++) buffer[i] = theBitMap[i];
    peek->contents = buffer;
    peek->next = nullptr;
    peek->bucket = (unsigned int) tally;
    peek->count = 0;
    peek->permanent = false;
    peek->size = size;
    peek->header.type = BITMAP_TYPE;

    /*================================================*/
    /* Add the bitmap to the list of ephemeral items. */
    /*================================================*/

    AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) peek, &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralBitMapList,
                         sizeof(CLIPSBitMap), sizeof(long), true);
    UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;

    /*===================================*/
    /* Return the address of the bitmap. */
    /*===================================*/

    return ((void *) peek);
}

/***********************************************/
/* CreateCExternalAddress: Creates an external */
/*    address for a C pointer.                 */
/***********************************************/
CLIPSExternalAddress::Ptr CreateCExternalAddress(
        const Environment&theEnv,
        void *ctx) {
    return CreateExternalAddress(theEnv, ctx, C_POINTER_EXTERNAL_ADDRESS);
}

/*******************************************************************/
/* CreateExternalAddress: Searches for the external address in the */
/*   hash table. If the external address is already in the hash    */
/*   table, then the address of the external address is returned.  */
/*   Otherwise, the external address is hashed into the table and  */
/*   the address of the external address is also returned.         */
/*******************************************************************/
CLIPSExternalAddress::Ptr CreateExternalAddress(
        const Environment&theEnv,
        void *ctx,
        unsigned short kind) {
    size_t tally;
    CLIPSExternalAddress *past = nullptr, *peek;

    /*====================================*/
    /* Get the hash value for the bitmap. */
    /*====================================*/

    tally = HashExternalAddress(ctx, EXTERNAL_ADDRESS_HASH_SIZE);

    peek = SymbolData(theEnv)->ExternalAddressTable[tally];

    /*=============================================================*/
    /* Search for the external address in the list of entries for  */
    /* this hash table location.  If the external addressis found, */
    /* then return the address of the external address.            */
    /*=============================================================*/

    while (peek != nullptr) {
        if ((peek->type == kind) &&
            (peek->contents == ctx)) { return peek; }

        past = peek;
        peek = peek->next;
    }

    /*=================================================*/
    /* Add the external address at the end of the list */
    /* of entries for this hash table location.        */
    /*=================================================*/

    peek = get_struct(theEnv, CLIPSExternalAddress);
    if (past == nullptr) SymbolData(theEnv)->ExternalAddressTable[tally] = peek;
    else past->next = peek;

    peek->contents = ctx;
    peek->type = kind;
    peek->next = nullptr;
    peek->bucket = (unsigned int) tally;
    peek->count = 0;
    peek->permanent = false;
    peek->header.type = EXTERNAL_ADDRESS_TYPE;

    /*================================================*/
    /* Add the bitmap to the list of ephemeral items. */
    /*================================================*/

    AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) peek, &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralExternalAddressList,
                         sizeof(CLIPSExternalAddress), sizeof(long), true);
    UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;

    /*=============================================*/
    /* Return the address of the external address. */
    /*=============================================*/

    return peek;
}
#endif
/***************************************************/
/* HashSymbol: Computes a hash value for a symbol. */
/***************************************************/
size_t HashSymbol(
        const std::string &str,
        size_t maximum) {
    size_t i;
    size_t tally = 0;
#if STUBBING_INACTIVE
    for (i = 0; word[i]; i++) { tally = tally * 127 + (size_t) word[i]; }
#endif
    if (maximum == 0) { return tally; }
    return tally % maximum;
}
#if STUBBING_INACTIVE
/*************************************************/
/* HashFloat: Computes a hash value for a float. */
/*************************************************/
size_t HashFloat(
        double number,
        size_t range) {
    size_t tally = 0;
    char *word;
    size_t i;

    word = (char *) &number;

    for (i = 0; i < sizeof(double); i++) { tally = tally * 127 + (size_t) word[i]; }

    if (range == 0) { return tally; }

    return tally % range;
}
#endif
/******************************************************/
/* HashInteger: Computes a hash value for an integer. */
/******************************************************/
size_t HashInteger(long long number, size_t range) {
    size_t tally;

#if WIN_MVC
    if (number < 0)
      { number = - number; }
    tally = (((size_t) number) % range);
#else
    tally = (((size_t) llabs(number)) % range);
#endif

    return tally;
}
#if STUBBING_INACTIVE
/****************************************/
/* HashExternalAddress: Computes a hash */
/*   value for an external address.     */
/****************************************/
size_t HashExternalAddress(
        void *theExternalAddress,
        size_t range) {
    size_t tally;
    union {
        void *vv;
        unsigned uv;
    } fis;

    fis.uv = 0;
    fis.vv = theExternalAddress;
    tally = (fis.uv / 256);

    if (range == 0) { return tally; }

    return (tally % range);
}

/***************************************************/
/* HashBitMap: Computes a hash value for a bitmap. */
/***************************************************/
size_t HashBitMap(
        const char *word,
        size_t range,
        unsigned length) {
    unsigned k, j, i;
    size_t tally;
    unsigned longLength;
    unsigned long count = 0L, tmpLong;
    char *tmpPtr;

    tmpPtr = (char *) &tmpLong;

    /*============================================================*/
    /* Add up the first part of the word as unsigned long values. */
    /*============================================================*/

    longLength = length / sizeof(unsigned long);
    for (i = 0, j = 0; i < longLength; i++) {
        for (k = 0; k < sizeof(unsigned long); k++, j++)
            tmpPtr[k] = word[j];
        count += tmpLong;
    }

    /*============================================*/
    /* Add the remaining characters to the count. */
    /*============================================*/

    for (; j < length; j++) count += (size_t) word[j];

    /*========================*/
    /* Return the hash value. */
    /*========================*/

    if (range == 0) { return count; }

    tally = (count % range);

    return tally;
}

#endif
/****************************************************/
/* RetainLexeme: Increments the count value for a   */
/*   SymbolTable entry. Adds the symbol to the      */
/*   EphemeralSymbolList if the count becomes zero. */
/****************************************************/
void RetainLexeme(
        const Environment&theEnv,
        CLIPSLexeme *theValue) {
    //theValue->count++;
}
/****************************************************/
/* ReleaseLexeme: Decrements the count value for a  */
/*   SymbolTable entry. Adds the symbol to the      */
/*   EphemeralSymbolList if the count becomes zero. */
/****************************************************/
void ReleaseLexeme(
        const Environment&theEnv,
        CLIPSLexeme *theValue) {
#if STUBBING_INACTIVE
    if (theValue->count < 0) {
        SystemError(theEnv, "SYMBOL", 3);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    if (theValue->count == 0) {
        SystemError(theEnv, "SYMBOL", 4);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    theValue->count--;

    if (theValue->count != 0) return;

    if (theValue->markedEphemeral == false) {
        AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) theValue, &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralSymbolList,
                             sizeof(CLIPSLexeme), AVERAGE_STRING_SIZE, true);
        UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
    }

    return;
#endif
}
#if STUBBING_INACTIVE

/***************************************************/
/* RetainFloat: Increments the count value for a   */
/*   FloatTable entry. Adds the float to the       */
/*   EphemeralFloatList if the count becomes zero. */
/***************************************************/
void RetainFloat(
        const Environment&theEnv,
        CLIPSFloat *theValue) {
    theValue->count++;
}

/***************************************************/
/* ReleaseFloat: Decrements the count value for a  */
/*   FloatTable entry. Adds the float to the       */
/*   EphemeralFloatList if the count becomes zero. */
/***************************************************/
void ReleaseFloat(
        const Environment&theEnv,
        CLIPSFloat *theValue) {
    if (theValue->count <= 0) {
        SystemError(theEnv, "SYMBOL", 5);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    theValue->count--;

    if (theValue->count != 0) return;

    if (theValue->markedEphemeral == false) {
        AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) theValue, &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralFloatList,
                             sizeof(CLIPSFloat), 0, true);
        UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
    }

    return;
}

/*****************************************************/
/* RetainInteger: Increments the count value for an  */
/*   IntegerTable entry. Adds the integer to the     */
/*   EphemeralIntegerList if the count becomes zero. */
/*****************************************************/
void RetainInteger(
        const Environment&theEnv,
        CLIPSInteger *theValue) {
    theValue->count++;
}

/*****************************************************/
/* ReleaseInteger: Decrements the count value for    */
/*   an IntegerTable entry. Adds the integer to the  */
/*   EphemeralIntegerList if the count becomes zero. */
/*****************************************************/
void ReleaseInteger(
        const Environment&theEnv,
        CLIPSInteger *theValue) {
    if (theValue->count <= 0) {
        SystemError(theEnv, "SYMBOL", 6);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    theValue->count--;

    if (theValue->count != 0) return;

    if (theValue->markedEphemeral == false) {
        AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) theValue, &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralIntegerList,
                             sizeof(CLIPSInteger), 0, true);
        UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
    }

    return;
}

/**************************************************************/
/* IncrementBitMapReferenceCount: Increments the count value  */
/*   for a BitmapTable entry. Adds the bitmap to the          */
/*   EphemeralBitMapList if the count becomes zero.           */
/**************************************************************/
void IncrementBitMapReferenceCount(
        const Environment&theEnv,
        CLIPSBitMap *theValue) {
    theValue->count++;
}

/**************************************************************/
/* DecrementBitMapReferenceCount: Decrements the count value  */
/*   for a BitmapTable entry. Adds the bitmap to the          */
/*   EphemeralBitMapList if the count becomes zero.           */
/**************************************************************/
void DecrementBitMapReferenceCount(
        const Environment&theEnv,
        CLIPSBitMap *theValue) {
    if (theValue->count < 0) {
        SystemError(theEnv, "SYMBOL", 7);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    if (theValue->count == 0) {
        SystemError(theEnv, "SYMBOL", 8);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    theValue->count--;

    if (theValue->count != 0) return;

    if (theValue->markedEphemeral == false) {
        AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) theValue, &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralBitMapList,
                             sizeof(CLIPSBitMap), sizeof(long), true);
        UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
    }

    return;
}

/*************************************************************/
/* RetainExternalAddress: Decrements the count value for an  */
/*   ExternAddressTable entry. Adds the bitmap to the        */
/*   EphemeralExternalAddressList if the count becomes zero. */
/*************************************************************/
void RetainExternalAddress(
        const Environment&theEnv,
        CLIPSExternalAddress *theValue) {
    theValue->count++;
}

/*************************************************************/
/* ReleaseExternalAddress: Decrements the count value for    */
/*   an ExternAddressTable entry. Adds the bitmap to the     */
/*   EphemeralExternalAddressList if the count becomes zero. */
/*************************************************************/
void ReleaseExternalAddress(
        const Environment&theEnv,
        CLIPSExternalAddress *theValue) {
    if (theValue->count < 0) {
        SystemError(theEnv, "SYMBOL", 9);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    if (theValue->count == 0) {
        SystemError(theEnv, "SYMBOL", 10);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    theValue->count--;

    if (theValue->count != 0) return;

    if (theValue->markedEphemeral == false) {
        AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) theValue, &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralExternalAddressList,
                             sizeof(CLIPSExternalAddress), sizeof(long), true);
        UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
    }

    return;
}

/************************************************/
/* RemoveHashNode: Removes a hash node from the */
/*   SymbolTable, FloatTable, IntegerTable,     */
/*   BitMapTable, or ExternalAddressTable.      */
/************************************************/
static void RemoveHashNode(
        const Environment&theEnv,
        genericHashNode::Ptr *theValue,
        genericHashNode::Ptr **theTable,
        int size,
        int type) {
    genericHashNode::Ptr *previousNode, *currentNode;
    CLIPSExternalAddress *theAddress;

    /*=============================================*/
    /* Find the entry in the specified hash table. */
    /*=============================================*/

    previousNode = nullptr;
    currentNode = theTable[theValue->bucket];

    while (currentNode != theValue) {
        previousNode = currentNode;
        currentNode = currentNode->next;

        if (currentNode == nullptr) {
            SystemError(theEnv, "SYMBOL", 11);
            ExitRouter(theEnv, EXIT_FAILURE);
        }
    }

    /*===========================================*/
    /* Remove the entry from the list of entries */
    /* stored in the hash table bucket.          */
    /*===========================================*/

    if (previousNode == nullptr) { theTable[theValue->bucket] = theValue->next; }
    else { previousNode->next = currentNode->next; }

    /*=================================================*/
    /* Symbol and bit map nodes have additional memory */
    /* use to store the character or bitmap string.    */
    /*=================================================*/

    if (type == SYMBOL_TYPE) {
        rm(theEnv, (void *) ((CLIPSLexeme *) theValue)->contents,
           strlen(((CLIPSLexeme *) theValue)->contents) + 1);
    } else if (type == BITMAPARRAY) {
        rm(theEnv, (void *) ((CLIPSBitMap *) theValue)->contents,
           ((CLIPSBitMap *) theValue)->size);
    } else if (type == EXTERNAL_ADDRESS_TYPE) {
        theAddress = (CLIPSExternalAddress *) theValue;

        if ((EvaluationData(theEnv)->ExternalAddressTypes[theAddress->type] != nullptr) &&
            (EvaluationData(theEnv)->ExternalAddressTypes[theAddress->type]->discardFunction != nullptr)) {
            (*EvaluationData(theEnv)->ExternalAddressTypes[theAddress->type]->discardFunction)(theEnv, theAddress->contents);
        }
    }

    /*===========================*/
    /* Return the table entry to */
    /* the pool of free memory.  */
    /*===========================*/

    rtn_sized_struct(theEnv, size, theValue);
}
#endif
/***********************************************************/
/* AddEphemeralHashNode: Adds a symbol, integer, float, or */
/*   bit map table entry to the list of ephemeral atomic   */
/*   values. These entries have a zero count indicating    */
/*   that no structure is using the data value.            */
/***********************************************************/
static void AddEphemeralHashNode(
        const Environment&theEnv,
        genericHashNode::Ptr theHashNode,
        struct ephemeron **theEphemeralList,
        int hashNodeSize,
        int averageContentsSize,
        bool checkCount) {
#if STUBBING_INACTIVE
    struct ephemeron *temp;

    /*===========================================*/
    /* If the count isn't zero then this routine */
    /* should never have been called.            */
    /*===========================================*/

    if (checkCount && (theHashNode->count != 0)) {
        SystemError(theEnv, "SYMBOL", 12);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    /*=====================================*/
    /* Mark the atomic value as ephemeral. */
    /*=====================================*/

    theHashNode->markedEphemeral = true;

    /*=============================*/
    /* Add the atomic value to the */
    /* list of ephemeral values.   */
    /*=============================*/

    temp = get_struct(theEnv, ephemeron);
    temp->associatedValue = theHashNode;
    temp->next = *theEphemeralList;
    *theEphemeralList = temp;
#endif
}
#if STUBBING_INACTIVE
/***************************************************/
/* RemoveEphemeralAtoms: Causes the removal of all */
/*   ephemeral symbols, integers, floats, and bit  */
/*   maps that still have a count value of zero,   */
/*   from their respective storage tables.         */
/***************************************************/
void RemoveEphemeralAtoms(
        const Environment&theEnv) {
    struct garbageFrame *theGarbageFrame;

    theGarbageFrame = UtilityData(theEnv)->CurrentGarbageFrame;
    if (!theGarbageFrame->dirty) return;

    RemoveEphemeralHashNodes(theEnv, &theGarbageFrame->ephemeralSymbolList, (genericHashNode::Ptr **) SymbolData(theEnv)->SymbolTable,
                             sizeof(CLIPSLexeme), SYMBOL_TYPE, AVERAGE_STRING_SIZE);
    RemoveEphemeralHashNodes(theEnv, &theGarbageFrame->ephemeralFloatList, (genericHashNode::Ptr **) SymbolData(theEnv)->FloatTable,
                             sizeof(CLIPSFloat), FLOAT_TYPE, 0);
    RemoveEphemeralHashNodes(theEnv, &theGarbageFrame->ephemeralIntegerList, (genericHashNode::Ptr **) SymbolData(theEnv)->IntegerTable,
                             sizeof(CLIPSInteger), INTEGER_TYPE, 0);
    RemoveEphemeralHashNodes(theEnv, &theGarbageFrame->ephemeralBitMapList, (genericHashNode::Ptr **) SymbolData(theEnv)->BitMapTable,
                             sizeof(CLIPSBitMap), BITMAPARRAY, AVERAGE_BITMAP_SIZE);
    RemoveEphemeralHashNodes(theEnv, &theGarbageFrame->ephemeralExternalAddressList,
                             (genericHashNode::Ptr **) SymbolData(theEnv)->ExternalAddressTable,
                             sizeof(CLIPSExternalAddress), EXTERNAL_ADDRESS_TYPE, 0);
}

/***********************************************/
/* EphemerateValue: Marks a value as ephemeral */
/*   if it is not already marked.              */
/***********************************************/
void EphemerateValue(
        const Environment&theEnv,
        void *theValue) {
    CLIPSLexeme *theSymbol;
    CLIPSFloat *theFloat;
    CLIPSInteger *theInteger;
    CLIPSExternalAddress *theExternalAddress;

    switch (((TypeHeader *) theValue)->type) {
        case SYMBOL_TYPE:
        case STRING_TYPE:
        case INSTANCE_NAME_TYPE:
            theSymbol = (CLIPSLexeme *) theValue;
            if (theSymbol->markedEphemeral) return;
            AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) theValue,
                                 &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralSymbolList,
                                 sizeof(CLIPSLexeme), AVERAGE_STRING_SIZE, false);
            UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
            break;

        case FLOAT_TYPE:
            theFloat = (CLIPSFloat *) theValue;
            if (theFloat->markedEphemeral) return;
            AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) theValue,
                                 &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralFloatList,
                                 sizeof(CLIPSFloat), 0, false);
            UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
            break;

        case INTEGER_TYPE:
            theInteger = (CLIPSInteger *) theValue;
            if (theInteger->markedEphemeral) return;
            AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) theValue,
                                 &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralIntegerList,
                                 sizeof(CLIPSInteger), 0, false);
            UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
            break;

        case EXTERNAL_ADDRESS_TYPE:
            theExternalAddress = (CLIPSExternalAddress *) theValue;
            if (theExternalAddress->markedEphemeral) return;
            AddEphemeralHashNode(theEnv, (genericHashNode::Ptr *) theValue,
                                 &UtilityData(theEnv)->CurrentGarbageFrame->ephemeralExternalAddressList,
                                 sizeof(CLIPSExternalAddress), sizeof(long), false);
            UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
            break;

        case MULTIFIELD_TYPE:
            EphemerateMultifield(theEnv, (Multifield *) theValue);
            break;

    }
}

/****************************************************************/
/* RemoveEphemeralHashNodes: Removes symbols from the ephemeral */
/*   symbol list that have a count of zero and were placed on   */
/*   the list at a higher level than the current evaluation     */
/*   depth. Since symbols are ordered in the list in descending */
/*   order, the removal process can end when a depth is reached */
/*   less than the current evaluation depth. Because ephemeral  */
/*   symbols can be "pulled" up through an evaluation depth,    */
/*   this routine needs to check through both the previous and  */
/*   current evaluation depth.                                  */
/****************************************************************/
static void RemoveEphemeralHashNodes(
        const Environment&theEnv,
        struct ephemeron **theEphemeralList,
        genericHashNode::Ptr **theTable,
        int hashNodeSize,
        int hashNodeType,
        int averageContentsSize) {
    struct ephemeron *edPtr, *lastPtr = nullptr, *nextPtr;

    edPtr = *theEphemeralList;

    while (edPtr != nullptr) {
        /*======================================================*/
        /* Check through previous and current evaluation depth  */
        /* because these symbols can be interspersed, otherwise */
        /* symbols are stored in descending evaluation depth.   */
        /*======================================================*/

        nextPtr = edPtr->next;

        /*==================================================*/
        /* Remove any symbols that have a count of zero and */
        /* were added to the ephemeral list at a higher     */
        /* evaluation depth.                                */
        /*==================================================*/

        if (edPtr->associatedValue->count == 0) {
            RemoveHashNode(theEnv, edPtr->associatedValue, theTable, hashNodeSize, hashNodeType);
            rtn_struct(theEnv, ephemeron, edPtr);
            if (lastPtr == nullptr) *theEphemeralList = nextPtr;
            else lastPtr->next = nextPtr;
        }

            /*=======================================*/
            /* Remove ephemeral status of any symbol */
            /* with a count greater than zero.       */
            /*=======================================*/

        else if (edPtr->associatedValue->count > 0) {
            edPtr->associatedValue->markedEphemeral = false;

            rtn_struct(theEnv, ephemeron, edPtr);

            if (lastPtr == nullptr) *theEphemeralList = nextPtr;
            else lastPtr->next = nextPtr;
        }

            /*==================================================*/
            /* Otherwise keep the symbol in the ephemeral list. */
            /*==================================================*/

        else { lastPtr = edPtr; }

        edPtr = nextPtr;
    }
}

/*********************************************************/
/* GetSymbolTable: Returns a pointer to the SymbolTable. */
/*********************************************************/
CLIPSLexeme **GetSymbolTable(
        const Environment&theEnv) {
    return (SymbolData(theEnv)->SymbolTable);
}

/******************************************************/
/* SetSymbolTable: Sets the value of the SymbolTable. */
/******************************************************/
void SetSymbolTable(
        const Environment&theEnv,
        CLIPSLexeme **value) {
    SymbolData(theEnv)->SymbolTable = value;
}

/*******************************************************/
/* GetFloatTable: Returns a pointer to the FloatTable. */
/*******************************************************/
CLIPSFloat **GetFloatTable(
        const Environment&theEnv) {
    return (SymbolData(theEnv)->FloatTable);
}

/****************************************************/
/* SetFloatTable: Sets the value of the FloatTable. */
/****************************************************/
void SetFloatTable(
        const Environment&theEnv,
        CLIPSFloat **value) {
    SymbolData(theEnv)->FloatTable = value;
}

/***********************************************************/
/* GetIntegerTable: Returns a pointer to the IntegerTable. */
/***********************************************************/
CLIPSInteger **GetIntegerTable(
        const Environment&theEnv) {
    return (SymbolData(theEnv)->IntegerTable);
}

/********************************************************/
/* SetIntegerTable: Sets the value of the IntegerTable. */
/********************************************************/
void SetIntegerTable(
        const Environment&theEnv,
        CLIPSInteger **value) {
    SymbolData(theEnv)->IntegerTable = value;
}

/*********************************************************/
/* GetBitMapTable: Returns a pointer to the BitMapTable. */
/*********************************************************/
CLIPSBitMap **GetBitMapTable(
        const Environment&theEnv) {
    return (SymbolData(theEnv)->BitMapTable);
}

/******************************************************/
/* SetBitMapTable: Sets the value of the BitMapTable. */
/******************************************************/
void SetBitMapTable(
        const Environment&theEnv,
        CLIPSBitMap **value) {
    SymbolData(theEnv)->BitMapTable = value;
}

/***************************************************************************/
/* GetExternalAddressTable: Returns a pointer to the ExternalAddressTable. */
/***************************************************************************/
CLIPSExternalAddress **GetExternalAddressTable(
        const Environment&theEnv) {
    return (SymbolData(theEnv)->ExternalAddressTable);
}

/************************************************************************/
/* SetExternalAddressTable: Sets the value of the ExternalAddressTable. */
/************************************************************************/
void SetExternalAddressTable(
        const Environment&theEnv,
        CLIPSExternalAddress **value) {
    SymbolData(theEnv)->ExternalAddressTable = value;
}

/******************************************************/
/* RefreshSpecialSymbols: Resets the values of the    */
/*   TrueSymbol, FalseSymbol, Zero, PositiveInfinity, */
/*   and NegativeInfinity symbols.                    */
/******************************************************/
void RefreshSpecialSymbols(
        const Environment&theEnv) {
    SymbolData(theEnv)->PositiveInfinity = FindSymbolHN(theEnv, POSITIVE_INFINITY_STRING, SYMBOL_BIT);
    SymbolData(theEnv)->NegativeInfinity = FindSymbolHN(theEnv, NEGATIVE_INFINITY_STRING, SYMBOL_BIT);
    SymbolData(theEnv)->Zero = FindLongHN(theEnv, 0L);
}

/***********************************************************/
/* FindSymbolMatches: Finds all symbols in the SymbolTable */
/*   which begin with a specified symbol. This function is */
/*   used to implement the command completion feature      */
/*   found in some of the machine specific interfaces.     */
/***********************************************************/
struct symbolMatch *FindSymbolMatches(
        const Environment&theEnv,
        const char *searchString,
        unsigned *numberOfMatches,
        size_t *commonPrefixLength) {
    struct symbolMatch *reply = nullptr, *temp;
    CLIPSLexeme *hashPtr = nullptr;
    size_t searchLength;

    searchLength = strlen(searchString);
    *numberOfMatches = 0;

    while ((hashPtr = GetNextSymbolMatch(theEnv, searchString, searchLength, hashPtr,
                                         false, commonPrefixLength)) != nullptr) {
        *numberOfMatches = *numberOfMatches + 1;
        temp = get_struct(theEnv, symbolMatch);
        temp->match = hashPtr;
        temp->next = reply;
        reply = temp;
    }

    return (reply);
}

/*********************************************************/
/* ReturnSymbolMatches: Returns a set of symbol matches. */
/*********************************************************/
void ReturnSymbolMatches(
        const Environment&theEnv,
        struct symbolMatch *listOfMatches) {
    struct symbolMatch *temp;

    while (listOfMatches != nullptr) {
        temp = listOfMatches->next;
        rtn_struct(theEnv, symbolMatch, listOfMatches);
        listOfMatches = temp;
    }
}

/***************************************************************/
/* ClearBitString: Initializes the values of a bitmap to zero. */
/***************************************************************/
void ClearBitString(
        void *vTheBitMap,
        size_t length) {
    char *theBitMap = (char *) vTheBitMap;
    size_t i;

    for (i = 0; i < length; i++) theBitMap[i] = '\0';
}

/****************************************/
/* BitStringHasBitsSet: Returns true if */
/*   the bit string has any bits set.   */
/****************************************/
bool BitStringHasBitsSet(
        void *vTheBitMap,
        unsigned length) {
    char *theBitMap = (char *) vTheBitMap;
    unsigned i;

    for (i = 0; i < length; i++) { if (theBitMap[i] != '\0') return true; }

    return false;
}

/*****************************************************************/
/* GetNextSymbolMatch: Finds the next symbol in the SymbolTable  */
/*   which begins with a specified symbol. This function is used */
/*   to implement the command completion feature found in some   */
/*   of the machine specific interfaces.                         */
/*****************************************************************/
CLIPSLexeme *GetNextSymbolMatch(
        const Environment&theEnv,
        const char *searchString,
        size_t searchLength,
        CLIPSLexeme *prevSymbol,
        bool anywhere,
        size_t *commonPrefixLength) {
    unsigned long i;
    CLIPSLexeme *hashPtr;
    bool flag = true;
    size_t prefixLength;

    /*==========================================*/
    /* If we're looking anywhere in the string, */
    /* then there's no common prefix length.    */
    /*==========================================*/

    if (anywhere && (commonPrefixLength != nullptr))
        *commonPrefixLength = 0;

    /*========================================================*/
    /* If we're starting the search from the beginning of the */
    /* symbol table, the previous symbol argument is nullptr.    */
    /*========================================================*/

    if (prevSymbol == nullptr) {
        i = 0;
        hashPtr = SymbolData(theEnv)->SymbolTable[0];
    }

        /*==========================================*/
        /* Otherwise start the search at the symbol */
        /* after the last symbol found.             */
        /*==========================================*/

    else {
        i = prevSymbol->bucket;
        hashPtr = prevSymbol->next;
    }

    /*==============================================*/
    /* Search through all the symbol table buckets. */
    /*==============================================*/

    while (flag) {
        /*===================================*/
        /* Search through all of the entries */
        /* in the bucket being examined.     */
        /*===================================*/

        for (; hashPtr != nullptr; hashPtr = hashPtr->next) {
            /*================================================*/
            /* Skip symbols that being with ( since these are */
            /* typically symbols for internal use. Also skip  */
            /* any symbols that are marked ephemeral since    */
            /* these aren't in use.                           */
            /*================================================*/

            if ((hashPtr->contents[0] == '(') ||
                (hashPtr->markedEphemeral)) { continue; }

            /*==================================================*/
            /* Two types of matching can be performed: the type */
            /* comparing just to the beginning of the string    */
            /* and the type which looks for the substring       */
            /* anywhere within the string being examined.       */
            /*==================================================*/

            if (!anywhere) {
                /*=============================================*/
                /* Determine the common prefix length between  */
                /* the previously found match (if available or */
                /* the search string if not) and the symbol    */
                /* table entry.                                */
                /*=============================================*/

                if (prevSymbol != nullptr)
                    prefixLength = CommonPrefixLength(prevSymbol->contents, hashPtr->contents);
                else
                    prefixLength = CommonPrefixLength(searchString, hashPtr->contents);

                /*===================================================*/
                /* If the prefix length is greater than or equal to  */
                /* the length of the search string, then we've found */
                /* a match. If this is the first match, the common   */
                /* prefix length is set to the length of the first   */
                /* match, otherwise the common prefix length is the  */
                /* smallest prefix length found among all matches.   */
                /*===================================================*/

                if (prefixLength >= searchLength) {
                    if (commonPrefixLength != nullptr) {
                        if (prevSymbol == nullptr)
                            *commonPrefixLength = strlen(hashPtr->contents);
                        else if (prefixLength < *commonPrefixLength)
                            *commonPrefixLength = prefixLength;
                    }
                    return (hashPtr);
                }
            } else {
                if (StringWithinString(hashPtr->contents, searchString) != nullptr) { return (hashPtr); }
            }
        }

        /*=================================================*/
        /* Move on to the next bucket in the symbol table. */
        /*=================================================*/

        if (++i >= SYMBOL_HASH_SIZE) flag = false;
        else hashPtr = SymbolData(theEnv)->SymbolTable[i];
    }

    /*=====================================*/
    /* There are no more matching symbols. */
    /*=====================================*/

    return nullptr;
}

/**********************************************/
/* StringWithinString: Determines if a string */
/*   is contained within another string.      */
/**********************************************/
static const char *StringWithinString(
        const char *cs,
        const char *ct) {
    unsigned i, j, k;

    for (i = 0; cs[i] != '\0'; i++) {
        for (j = i, k = 0; ct[k] != '\0' && cs[j] == ct[k]; j++, k++);
        if ((ct[k] == '\0') && (k != 0))
            return (cs + i);
    }
    return nullptr;
}

/************************************************/
/* CommonPrefixLength: Determines the length of */
/*    the maximumcommon prefix of two strings   */
/************************************************/
static size_t CommonPrefixLength(
        const char *cs,
        const char *ct) {
    unsigned i;

    for (i = 0; (cs[i] != '\0') && (ct[i] != '\0'); i++)
        if (cs[i] != ct[i])
            break;
    return (i);
}

#if BLOAD_AND_BSAVE || BSAVE_INSTANCES

/****************************************************************/
/* SetAtomicValueIndices: Sets the bucket values for hash table */
/*   entries with an index value that indicates the position of */
/*   the hash table in a hash table traversal (e.g. this is the */
/*   fifth entry in the  hash table.                            */
/****************************************************************/
void SetAtomicValueIndices(
        const Environment&theEnv,
        bool setAll) {
    unsigned int count;
    unsigned int i;
    CLIPSLexeme *symbolPtr, **symbolArray;
    CLIPSFloat *floatPtr, **floatArray;
    CLIPSInteger *integerPtr, **integerArray;
    CLIPSBitMap *bitMapPtr, **bitMapArray;

    /*===================================*/
    /* Set indices for the symbol table. */
    /*===================================*/

    count = 0;
    symbolArray = GetSymbolTable(theEnv);

    for (i = 0; i < SYMBOL_HASH_SIZE; i++) {
        for (symbolPtr = symbolArray[i];
             symbolPtr != nullptr;
             symbolPtr = symbolPtr->next) {
            if ((symbolPtr->neededSymbol == true) || setAll) { symbolPtr->bucket = count++; }
        }
    }

    /*==================================*/
    /* Set indices for the float table. */
    /*==================================*/

    count = 0;
    floatArray = GetFloatTable(theEnv);

    for (i = 0; i < FLOAT_HASH_SIZE; i++) {
        for (floatPtr = floatArray[i];
             floatPtr != nullptr;
             floatPtr = floatPtr->next) {
            if ((floatPtr->neededFloat == true) || setAll) { floatPtr->bucket = count++; }
        }
    }

    /*====================================*/
    /* Set indices for the integer table. */
    /*====================================*/

    count = 0;
    integerArray = GetIntegerTable(theEnv);

    for (i = 0; i < INTEGER_HASH_SIZE; i++) {
        for (integerPtr = integerArray[i];
             integerPtr != nullptr;
             integerPtr = integerPtr->next) {
            if ((integerPtr->neededInteger == true) || setAll) { integerPtr->bucket = count++; }
        }
    }

    /*===================================*/
    /* Set indices for the bitmap table. */
    /*===================================*/

    count = 0;
    bitMapArray = GetBitMapTable(theEnv);

    for (i = 0; i < BITMAP_HASH_SIZE; i++) {
        for (bitMapPtr = bitMapArray[i];
             bitMapPtr != nullptr;
             bitMapPtr = bitMapPtr->next) {
            if ((bitMapPtr->neededBitMap == true) || setAll) { bitMapPtr->bucket = count++; }
        }
    }
}

/***********************************************************************/
/* RestoreAtomicValueBuckets: Restores the bucket values of hash table */
/*   entries to the appropriate values. Normally called to undo the    */
/*   effects of a call to the SetAtomicValueIndices function.          */
/***********************************************************************/
void RestoreAtomicValueBuckets(
        const Environment&theEnv) {
    unsigned int i;
    CLIPSLexeme *symbolPtr, **symbolArray;
    CLIPSFloat *floatPtr, **floatArray;
    CLIPSInteger *integerPtr, **integerArray;
    CLIPSBitMap *bitMapPtr, **bitMapArray;

    /*================================================*/
    /* Restore the bucket values in the symbol table. */
    /*================================================*/

    symbolArray = GetSymbolTable(theEnv);

    for (i = 0; i < SYMBOL_HASH_SIZE; i++) {
        for (symbolPtr = symbolArray[i];
             symbolPtr != nullptr;
             symbolPtr = symbolPtr->next) { symbolPtr->bucket = i; }
    }

    /*===============================================*/
    /* Restore the bucket values in the float table. */
    /*===============================================*/

    floatArray = GetFloatTable(theEnv);

    for (i = 0; i < FLOAT_HASH_SIZE; i++) {
        for (floatPtr = floatArray[i];
             floatPtr != nullptr;
             floatPtr = floatPtr->next) { floatPtr->bucket = i; }
    }

    /*=================================================*/
    /* Restore the bucket values in the integer table. */
    /*=================================================*/

    integerArray = GetIntegerTable(theEnv);

    for (i = 0; i < INTEGER_HASH_SIZE; i++) {
        for (integerPtr = integerArray[i];
             integerPtr != nullptr;
             integerPtr = integerPtr->next) { integerPtr->bucket = i; }
    }

    /*================================================*/
    /* Restore the bucket values in the bitmap table. */
    /*================================================*/

    bitMapArray = GetBitMapTable(theEnv);

    for (i = 0; i < BITMAP_HASH_SIZE; i++) {
        for (bitMapPtr = bitMapArray[i];
             bitMapPtr != nullptr;
             bitMapPtr = bitMapPtr->next) { bitMapPtr->bucket = i; }
    }
}

#endif /* BLOAD_AND_BSAVE || BSAVE_INSTANCES */
#endif
