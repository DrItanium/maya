/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  05/03/19            */
/*                                                     */
/*                 UTILITY HEADER FILE                 */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of utility functions useful to    */
/*   other modules. Primarily these are the functions for    */
/*   handling periodic garbage collection and appending      */
/*   string data.                                            */
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
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Added CopyString, DeleteString,                */
/*            InsertInString,and EnlargeString functions.    */
/*                                                           */
/*            Used genstrncpy function instead of strncpy    */
/*            function.                                      */
/*                                                           */
/*            Support for typed EXTERNAL_ADDRESS_TYPE.       */
/*                                                           */
/*            Support for tracked memory (allows memory to   */
/*            be freed if CLIPS is exited while executing).  */
/*                                                           */
/*            Added UTF-8 routines.                          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.31: Added debugging code for checking the garbage  */
/*            frame.                                         */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
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
/*            Added GCBlockStart and GCBlockEnd functions    */
/*            for garbage collection blocks.                 */
/*                                                           */
/*            Added StringBuilder functions.                 */
/*                                                           */
/*            Moved BufferedRead and FreeReadBuffer from     */
/*            insfile.c to utility.c                         */
/*                                                           */
/*************************************************************/

#ifndef _H_utility

#pragma once

#define _H_utility

#include <cstdlib>

typedef struct callFunctionItem CallFunctionItem;
typedef struct callFunctionItemWithArg CallFunctionItemWithArg;
typedef struct boolCallFunctionItem BoolCallFunctionItem;
typedef struct voidCallFunctionItem VoidCallFunctionItem;

#include "Evaluation.h"
#include "Defmodule.h"

typedef struct gcBlock GCBlock;
typedef struct stringBuilder StringBuilder;

struct voidCallFunctionItem {
    const char *name;
    VoidCallFunction *func;
    int priority;
    struct voidCallFunctionItem *next;
    void *context;
};

struct boolCallFunctionItem {
    const char *name;
    BoolCallFunction *func;
    int priority;
    struct boolCallFunctionItem *next;
    void *context;
};

struct callFunctionItemWithArg {
    const char *name;
    VoidCallFunctionWithArg *func;
    int priority;
    struct callFunctionItemWithArg *next;
    void *context;
};

struct trackedMemory {
    void *theMemory;
    struct trackedMemory *next;
    struct trackedMemory *prev;
    size_t memSize;
};

struct garbageFrame {
    bool dirty;
    struct garbageFrame *priorFrame;
    struct ephemeron *ephemeralSymbolList;
    struct ephemeron *ephemeralFloatList;
    struct ephemeron *ephemeralIntegerList;
    struct ephemeron *ephemeralBitMapList;
    struct ephemeron *ephemeralExternalAddressList;
    Multifield *ListOfMultifields;
    Multifield *LastMultifield;
};

struct gcBlock {
    struct garbageFrame newGarbageFrame;
    struct garbageFrame *oldGarbageFrame;
    UDFValue *result;
};

struct stringBuilder {
    Environment sbEnv;
    char *contents;
    size_t bufferReset;
    size_t length;
    size_t bufferMaximum;
};

constexpr auto UTILITY_DATA = 55;
typedef void YieldTimeFunctionType();
struct utilityData {
    struct voidCallFunctionItem *ListOfCleanupFunctions;
    struct voidCallFunctionItem *ListOfPeriodicFunctions;
    bool PeriodicFunctionsEnabled;
    bool YieldFunctionEnabled;
    YieldTimeFunctionType* YieldTimeFunction;
    struct trackedMemory *trackList;
    struct garbageFrame MasterGarbageFrame;
    struct garbageFrame *CurrentGarbageFrame;
    size_t BinaryFileSize;
    size_t BinaryFileOffset;
    char *CurrentReadBuffer;
    size_t CurrentReadBufferSize;
    size_t CurrentReadBufferOffset;
};

#define UtilityData(theEnv) ((utilityData *) GetEnvironmentData(theEnv,UTILITY_DATA))

/* Is c the start of a utf8 sequence? */
constexpr bool IsUTF8Start(char ch) noexcept {
    return ((ch) & 0xC0) != 0x80;
}

constexpr bool IsUTF8MultiByteStart(char ch) noexcept {
    return (((unsigned char) ch) >= 0xC0) && (((unsigned char) ch) <= 0xF7);
}
constexpr bool IsUTF8MultiByteContinuation(char ch) noexcept {
    return (((unsigned char) ch) >= 0x80) && (((unsigned char) ch) <= 0xBF);
}


void InitializeUtilityData(const Environment&);
bool AddCleanupFunction(const Environment&, const char *, VoidCallFunction *, int, void *context = nullptr);
bool AddPeriodicFunction(const Environment&, const char *, VoidCallFunction *, int, void *context = nullptr);
bool RemoveCleanupFunction(const Environment&, const char *);
bool RemovePeriodicFunction(const Environment&, const char *);
char *CopyString(const Environment&, const char *);
void DeleteString(const Environment&, char *);
const char *AppendStrings(const Environment&, const char *, const char *);
const char *StringPrintForm(const Environment&, const char *);
char *AppendToString(const Environment&, const char *, char *, size_t *, size_t *);
char *InsertInString(const Environment&, const char *, size_t, char *, size_t *, size_t *);
char *AppendNToString(const Environment&, const char *, char *, size_t, size_t *, size_t *);
char *EnlargeString(const Environment&, size_t, char *, size_t *, size_t *);
char *ExpandStringWithChar(const Environment&, int, char *, size_t *, size_t *, size_t);
VoidCallFunctionItem *AddVoidFunctionToCallList(const Environment&, const char *, int, VoidCallFunction *,
                                                VoidCallFunctionItem *, void *context = nullptr);
BoolCallFunctionItem *AddBoolFunctionToCallList(const Environment&, const char *, int, BoolCallFunction *,
                                                BoolCallFunctionItem *, void *context = nullptr);
VoidCallFunctionItem *RemoveVoidFunctionFromCallList(const Environment&, const char *,
                                                     VoidCallFunctionItem *, bool *);
BoolCallFunctionItem *RemoveBoolFunctionFromCallList(const Environment&, const char *,
                                                     BoolCallFunctionItem *, bool *);
void DeallocateVoidCallList(const Environment&, VoidCallFunctionItem *);
void DeallocateBoolCallList(const Environment&, BoolCallFunctionItem *);
CallFunctionItemWithArg *AddFunctionToCallListWithArg(const Environment&, const char *, int,
                                                      VoidCallFunctionWithArg *,
                                                      CallFunctionItemWithArg *, void *context = nullptr);
CallFunctionItemWithArg *RemoveFunctionFromCallListWithArg(const Environment&, const char *,
                                                           struct callFunctionItemWithArg *,
                                                           bool *);
void DeallocateCallListWithArg(const Environment&, struct callFunctionItemWithArg *);
VoidCallFunctionItem *GetVoidFunctionFromCallList(const Environment&, const char *, VoidCallFunctionItem *);
BoolCallFunctionItem *GetBoolFunctionFromCallList(const Environment&, const char *, BoolCallFunctionItem *);
size_t ItemHashValue(const Environment&, unsigned short, void *, size_t);
void YieldTime(const Environment&);
bool EnablePeriodicFunctions(const Environment&, bool);
bool EnableYieldFunction(const Environment&, bool);
struct trackedMemory *AddTrackedMemory(const Environment&, void *, size_t);
void RemoveTrackedMemory(const Environment&, struct trackedMemory *);
void UTF8Increment(const char *, size_t *);
size_t UTF8Offset(const char *, size_t);
size_t UTF8Length(const char *);
size_t UTF8CharNum(const char *, size_t);
void RestorePriorGarbageFrame(const Environment&, struct garbageFrame *, struct garbageFrame *, UDFValue *);
void CallCleanupFunctions(const Environment&);
void CallPeriodicTasks(const Environment&);
void CleanCurrentGarbageFrame(const Environment&, UDFValue *);
void GCBlockStart(const Environment&, GCBlock *);
void GCBlockEnd(const Environment&, GCBlock *);
void GCBlockEndUDF(const Environment&, GCBlock *, UDFValue *);
bool CurrentGarbageFrameIsDirty(const Environment&);
StringBuilder *CreateStringBuilder(const Environment&, size_t);
void SBDispose(StringBuilder *);
void SBAppend(StringBuilder *, const char *);
void SBAppendInteger(StringBuilder *, long long);
void SBAppendFloat(StringBuilder *, double);
void SBAddChar(StringBuilder *, int);
void SBReset(StringBuilder *);
char *SBCopy(StringBuilder *);
void *GetPeriodicFunctionContext(const Environment&, const char *);
void BufferedRead(const Environment&, void *, size_t);
void FreeReadBuffer(const Environment&);

#endif /* _H_utility */




