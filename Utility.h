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

#include "Evaluation.h"
#include "Defmodule.h"
#include <sstream>
#include <string>

typedef struct gcBlock GCBlock;
typedef struct stringBuilder StringBuilder;
template<typename FunctionKind>
struct GenericCallFunctionItem {
public:
    using Self = GenericCallFunctionItem;
    using Ptr = std::shared_ptr<Self>;
    using Body = FunctionKind;
public:
    GenericCallFunctionItem(const std::string& name, int priority, Body body) : _name(name), _priority(priority), _body(body) { }
    auto getName() const noexcept { return _name; }
    constexpr auto getPriority() const noexcept { return _priority; }
    Body getBody() const noexcept { return _body; }
    Ptr getNext() const noexcept { return _next; }
    void setNext(Ptr value) noexcept { _next = value; }
private:
    std::string _name;
    int _priority;
    Body _body;
    Ptr _next;
};

template<typename ReturnType>
using NoExtraArgCallFunctionItem = GenericCallFunctionItem<std::function<ReturnType(const Environment::Ptr&)>>;
template<typename ReturnType, typename ArgumentKind>
using OneExtraArgCallFunctionItem = GenericCallFunctionItem<std::function<ReturnType(const Environment::Ptr&, ArgumentKind)>>;

template<typename ReturnType>
using OneExtraAnyArgCallFunctionItem = OneExtraArgCallFunctionItem<ReturnType, std::any>;
using VoidCallFunctionItem = NoExtraArgCallFunctionItem<void>;
using BoolCallFunctionItem = NoExtraArgCallFunctionItem<bool>;
using CallFunctionItemWithArg = OneExtraAnyArgCallFunctionItem<void>;

struct trackedMemory {
public:
    using Self = trackedMemory;
    using Ptr = std::shared_ptr<Self>;
public:
    void *theMemory;
    Ptr next;
    Ptr prev;
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
    stringBuilder(const Environment::Ptr& env);
    ~stringBuilder() = default;
    stringBuilder(const stringBuilder&);
    void reset();
    void appendChar(int);
    void append(long long);
    void append(double);
    void append(const char*);
    std::string contents() const noexcept;
private:
    Environment _env;
    std::stringstream _internal;

};

constexpr auto UTILITY_DATA = 55;
using YieldTimeFunctionBody = std::function<void()>;
struct utilityData : public EnvironmentModule {
    std::list<VoidCallFunctionItem> listOfCleanupFunctions;
    std::list<VoidCallFunctionItem> listOfPeriodicFunctions;
    bool PeriodicFunctionsEnabled;
    bool YieldFunctionEnabled;
    YieldTimeFunctionBody YieldTimeFunction;
    trackedMemory *trackList;
    garbageFrame MasterGarbageFrame;
    garbageFrame *CurrentGarbageFrame;
    size_t BinaryFileSize;
    size_t BinaryFileOffset;
    char *CurrentReadBuffer;
    size_t CurrentReadBufferSize;
    size_t CurrentReadBufferOffset;
};
RegisterEnvironmentModule(utilityData, UTILITY_DATA, Utility);

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


void InitializeUtilityData(const Environment::Ptr&);
bool AddCleanupFunction(const Environment::Ptr&, const char *, VoidCallFunction *, int, void *context = nullptr);
bool AddPeriodicFunction(const Environment::Ptr&, const char *, VoidCallFunction *, int, void *context = nullptr);
bool RemoveCleanupFunction(const Environment::Ptr&, const char *);
bool RemovePeriodicFunction(const Environment::Ptr&, const char *);
char *CopyString(const Environment::Ptr&, const char *);
void DeleteString(const Environment::Ptr&, char *);
const char *AppendStrings(const Environment::Ptr&, const char *, const char *);
const char *StringPrintForm(const Environment::Ptr&, const char *);
char *AppendToString(const Environment::Ptr&, const char *, char *, size_t *, size_t *);
char *InsertInString(const Environment::Ptr&, const char *, size_t, char *, size_t *, size_t *);
char *AppendNToString(const Environment::Ptr&, const char *, char *, size_t, size_t *, size_t *);
char *EnlargeString(const Environment::Ptr&, size_t, char *, size_t *, size_t *);
char *ExpandStringWithChar(const Environment::Ptr&, int, char *, size_t *, size_t *, size_t);
VoidCallFunctionItem *AddVoidFunctionToCallList(const Environment::Ptr&, const char *, int, VoidCallFunction *,
                                                VoidCallFunctionItem *, void *context = nullptr);
BoolCallFunctionItem *AddBoolFunctionToCallList(const Environment::Ptr&, const char *, int, BoolCallFunction *,
                                                BoolCallFunctionItem *, void *context = nullptr);
VoidCallFunctionItem *RemoveVoidFunctionFromCallList(const Environment::Ptr&, const char *,
                                                     VoidCallFunctionItem *, bool *);
BoolCallFunctionItem *RemoveBoolFunctionFromCallList(const Environment::Ptr&, const char *,
                                                     BoolCallFunctionItem *, bool *);
void DeallocateVoidCallList(const Environment::Ptr&, VoidCallFunctionItem *);
void DeallocateBoolCallList(const Environment::Ptr&, BoolCallFunctionItem *);
CallFunctionItemWithArg *AddFunctionToCallListWithArg(const Environment::Ptr&, const char *, int,
                                                      VoidCallFunctionWithArg *,
                                                      CallFunctionItemWithArg *, void *context = nullptr);
CallFunctionItemWithArg *RemoveFunctionFromCallListWithArg(const Environment::Ptr&, const char *,
                                                           struct callFunctionItemWithArg *,
                                                           bool *);
void DeallocateCallListWithArg(const Environment::Ptr&, struct callFunctionItemWithArg *);
VoidCallFunctionItem *GetVoidFunctionFromCallList(const Environment::Ptr&, const char *, VoidCallFunctionItem *);
BoolCallFunctionItem *GetBoolFunctionFromCallList(const Environment::Ptr&, const char *, BoolCallFunctionItem *);
size_t ItemHashValue(const Environment::Ptr&, unsigned short, void *, size_t);
void YieldTime(const Environment::Ptr&);
bool EnablePeriodicFunctions(const Environment::Ptr&, bool);
bool EnableYieldFunction(const Environment::Ptr&, bool);
struct trackedMemory *AddTrackedMemory(const Environment::Ptr&, void *, size_t);
void RemoveTrackedMemory(const Environment::Ptr&, struct trackedMemory *);
void UTF8Increment(const char *, size_t *);
size_t UTF8Offset(const char *, size_t);
size_t UTF8Length(const char *);
size_t UTF8CharNum(const char *, size_t);
void RestorePriorGarbageFrame(const Environment::Ptr&, struct garbageFrame *, struct garbageFrame *, UDFValue *);
void CallCleanupFunctions(const Environment::Ptr&);
void CallPeriodicTasks(const Environment::Ptr&);
void CleanCurrentGarbageFrame(const Environment::Ptr&, UDFValue *);
void GCBlockStart(const Environment::Ptr&, GCBlock *);
void GCBlockEnd(const Environment::Ptr&, GCBlock *);
void GCBlockEndUDF(const Environment::Ptr&, GCBlock *, UDFValue *);
bool CurrentGarbageFrameIsDirty(const Environment::Ptr&);
StringBuilder *CreateStringBuilder(const Environment::Ptr&, size_t);
void SBDispose(StringBuilder *);
void SBAppend(StringBuilder *, const char *);
void SBAppendInteger(StringBuilder *, long long);
void SBAppendFloat(StringBuilder *, double);
void SBAddChar(StringBuilder *, int);
void SBReset(StringBuilder *);
void *GetPeriodicFunctionContext(const Environment::Ptr&, const char *);
void BufferedRead(const Environment::Ptr&, void *, size_t);
void FreeReadBuffer(const Environment::Ptr&);

#endif /* _H_utility */




