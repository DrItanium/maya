/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  10/01/16            */
/*                                                     */
/*                  CONSTRUCT MODULE                   */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed garbage collection algorithm.          */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added code for capturing errors/warnings       */
/*            (EnvSetParserErrorCallback).                   */
/*                                                           */
/*            Fixed issue with save function when multiple   */
/*            defmodules exist.                              */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
/*                                                           */
/*            Added code to prevent a clear command from     */
/*            being executed during fact assertions via      */
/*            Increment/DecrementClearReadyLocks API.        */
/*                                                           */
/*            Added code to keep track of pointers to        */
/*            constructs that are contained externally to    */
/*            to constructs, DanglingConstructs.             */
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
/*            UDF redesign.                                  */
/*                                                           */
/*            Modified EnvClear to return completion status. */
/*                                                           */
/*            File name/line count displayed for errors      */
/*            and warnings during load command.              */
/*                                                           */
/*************************************************************/

#ifndef _H_constrct

#pragma once

#define _H_constrct

typedef struct construct Construct;

#include "Entities.h"
#include "UserData.h"
#include "Defmodule.h"
#include "Utility.h"
#include "StringFunctions.h"

typedef void SaveCallFunction(const Environment&, Defmodule *, const char *, void *);
typedef struct saveCallFunctionItem SaveCallFunctionItem;

typedef void ParserErrorFunction(const Environment&, const char *, const char *, const char *, long, void *);
typedef bool BeforeResetFunction(const Environment&);

#define CHS (ConstructHeader *)

struct saveCallFunctionItem {
    const char *name;
    SaveCallFunction *func;
    int priority;
    SaveCallFunctionItem *next;
};
typedef bool ConstructParseFunction(const Environment&, const char*);
typedef CLIPSLexeme* ConstructGetConstructNameFunction(ConstructHeader*);
typedef const char* ConstructGetPPFormFunction(ConstructHeader*);
typedef struct defmoduleItemHeader* ConstructGetModuleItemFunction(ConstructHeader*);
typedef void ConstructSetNextItemFunction(ConstructHeader*, ConstructHeader*);
struct construct {
    const char *constructName;
    const char *pluralName;
    ConstructParseFunction* parseFunction;
    FindConstructFunction *findFunction;
    ConstructGetConstructNameFunction* getConstructNameFunction;
    ConstructGetPPFormFunction* getPPFormFunction;
    ConstructGetModuleItemFunction* getModuleItemFunction;
    GetNextConstructFunction *getNextItemFunction;
    ConstructSetNextItemFunction* setNextItemFunction;
    IsConstructDeletableFunction *isConstructDeletableFunction;
    DeleteConstructFunction *deleteFunction;
    FreeConstructFunction *freeFunction;
    Construct *next;
};

constexpr auto CONSTRUCT_DATA = 42;

struct ConstructModule : public EnvironmentModule {
    bool ClearReadyInProgress;
    bool ClearInProgress;
    bool ResetReadyInProgress;
    bool ResetInProgress;
    short ClearReadyLocks;
    int DanglingConstructs;
    SaveCallFunctionItem *ListOfSaveFunctions;
    bool PrintWhileLoading;
    bool LoadInProgress;
    bool WatchCompilations;
    bool CheckSyntaxMode;
    bool ParsingConstruct;
    char *ErrorString;
    char *WarningString;
    char *ParsingFileName;
    char *ErrorFileName;
    char *WarningFileName;
    long ErrLineNumber;
    long WrnLineNumber;
    int errorCaptureRouterCount;
    size_t MaxErrChars;
    size_t CurErrPos;
    size_t MaxWrnChars;
    size_t CurWrnPos;
    ParserErrorFunction *ParserErrorCallback;
    void *ParserErrorContext;
    Construct *ListOfConstructs;
    struct voidCallFunctionItem *ListOfResetFunctions;
    struct voidCallFunctionItem *ListOfClearFunctions;
    struct boolCallFunctionItem *ListOfClearReadyFunctions;
    bool Executing;
    BeforeResetFunction *BeforeResetCallback;
};
RegisterEnvironmentModule(ConstructModule, CONSTRUCT_DATA, Construct);

bool Clear(const Environment&);
void Reset(const Environment&);
bool Save(const Environment&, const char *);

void InitializeConstructData(const Environment&);
bool AddResetFunction(const Environment&, const char *, VoidCallFunction *, int, void *context = nullptr);
bool RemoveResetFunction(const Environment&, const char *);
bool AddClearReadyFunction(const Environment&, const char *, BoolCallFunction *, int, void *context = nullptr);
bool RemoveClearReadyFunction(const Environment&, const char *);
bool AddClearFunction(const Environment&, const char *, VoidCallFunction *, int, void *context = nullptr);
bool RemoveClearFunction(const Environment&, const char *);
void IncrementClearReadyLocks(const Environment&);
void DecrementClearReadyLocks(const Environment&);
Construct *AddConstruct(const Environment&, const char *, const char *,
                        ConstructParseFunction*,
                        FindConstructFunction *,
                        ConstructGetConstructNameFunction*,
                        ConstructGetPPFormFunction*,
                        ConstructGetModuleItemFunction*,
                        GetNextConstructFunction *,
                        ConstructSetNextItemFunction*,
                        IsConstructDeletableFunction *,
                        DeleteConstructFunction *,
                        FreeConstructFunction *);
bool RemoveConstruct(const Environment&, const char *);
void SetCompilationsWatch(const Environment&, bool);
bool GetCompilationsWatch(const Environment&);
void SetPrintWhileLoading(const Environment&, bool);
bool GetPrintWhileLoading(const Environment&);
void SetLoadInProgress(const Environment&, bool);
bool GetLoadInProgress(const Environment&);
bool ExecutingConstruct(const Environment&);
void SetExecutingConstruct(const Environment&, bool);
void InitializeConstructs(const Environment&);
BeforeResetFunction *SetBeforeResetFunction(const Environment&, BeforeResetFunction *);
void ResetCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ClearCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool ClearReady(const Environment&);
Construct *FindConstruct(const Environment&, const char *);
void DeinstallConstructHeader(const Environment&, ConstructHeader *);
void DestroyConstructHeader(const Environment&, ConstructHeader *);
ParserErrorFunction *SetParserErrorCallback(const Environment&, ParserErrorFunction *, void *context = nullptr);

bool AddSaveFunction(const Environment&, const char *, SaveCallFunction *, int, void *context = nullptr);
bool RemoveSaveFunction(const Environment&, const char *);
SaveCallFunctionItem *AddSaveFunctionToCallList(const Environment&, const char *, int,
                                                SaveCallFunction *, SaveCallFunctionItem *, void *context = nullptr);
SaveCallFunctionItem *RemoveSaveFunctionFromCallList(const Environment&, const char *,
                                                     SaveCallFunctionItem *, bool *);
void DeallocateSaveCallList(const Environment&, SaveCallFunctionItem *);

#if BLOAD_AND_BSAVE

struct bsaveConstructHeader {
    unsigned long name;
    unsigned long whichModule;
    unsigned long next;
};

void MarkConstructHeaderNeededItems(ConstructHeader *, unsigned long);
void AssignBsaveConstructHeaderVals(bsaveConstructHeader *,
                                    ConstructHeader *);

void UpdateConstructHeader(const Environment&, struct bsaveConstructHeader *,
                           ConstructHeader *, ConstructType, size_t, void *, size_t, void *);
void UnmarkConstructHeader(const Environment&, ConstructHeader *);

#endif
enum LoadError {
    LE_NO_ERROR = 0,
    LE_OPEN_FILE_ERROR,
    LE_PARSING_ERROR,
};

LoadError Load(const Environment&, const char *);
bool LoadConstructsFromLogicalName(const Environment&, const char *);
bool LoadFromString(const Environment&, const char *, size_t);
BuildError ParseConstruct(const Environment&, const char *, const char *);
void ImportExportConflictMessage(const Environment&, const char *, const char *,
                                 const char *, const char *);
void FlushParsingMessages(const Environment&);
char *GetParsingFileName(const Environment&);
void SetParsingFileName(const Environment&, const char *);
char *GetErrorFileName(const Environment&);
void SetErrorFileName(const Environment&, const char *);
char *GetWarningFileName(const Environment&);
void SetWarningFileName(const Environment&, const char *);
void CreateErrorCaptureRouter(const Environment&);
void DeleteErrorCaptureRouter(const Environment&);

typedef bool ConstructGetWatchFunction(void *);
typedef void ConstructSetWatchFunction(void *, bool);
typedef void ConstructActionFunction(const Environment&, ConstructHeader *, void *);

void AddConstructToModule(ConstructHeader *);
bool DeleteNamedConstruct(const Environment&, const char *, Construct *);
ConstructHeader *FindNamedConstructInModule(const Environment&, const char *, Construct *);
ConstructHeader *FindNamedConstructInModuleOrImports(const Environment&, const char *, Construct *);
void UndefconstructCommand(UDFContext *, const char *, Construct *);
bool PPConstruct(const Environment&, const char *, const char *, Construct *);
const char *PPConstructNil(const Environment&, const char *, Construct *);
CLIPSLexeme *GetConstructModuleCommand(UDFContext *, const char *, Construct *);
Defmodule *GetConstructModule(const Environment&, const char *, Construct *);
bool Undefconstruct(const Environment&, ConstructHeader *, Construct *);
bool UndefconstructAll(const Environment&, Construct *);
void SaveConstruct(const Environment&, Defmodule *, const char *, Construct *);
const char *GetConstructNameString(ConstructHeader *);
const char *GetConstructModuleName(ConstructHeader *);
CLIPSLexeme *GetConstructNamePointer(ConstructHeader *);
void GetConstructListFunction(UDFContext *, UDFValue *, Construct *);
void GetConstructList(const Environment&, UDFValue *, Construct *,
                      Defmodule *);
void ListConstructCommand(UDFContext *, Construct *);
void ListConstruct(const Environment&, Construct *, const char *, Defmodule *);
void SetNextConstruct(ConstructHeader *, ConstructHeader *);
struct defmoduleItemHeader *GetConstructModuleItem(ConstructHeader *);
const char *GetConstructPPForm(ConstructHeader *);
void PPConstructCommand(UDFContext *, const char *, Construct *, UDFValue *);
ConstructHeader *GetNextConstructItem(const Environment&, ConstructHeader *, unsigned);
struct defmoduleItemHeader *GetConstructModuleItemByIndex(const Environment&, Defmodule *, unsigned);
void FreeConstructHeaderModule(const Environment&, struct defmoduleItemHeader *,
                               Construct *);
void DoForAllConstructs(const Environment&,
                        ConstructActionFunction *,
                        unsigned, bool, void *);
void DoForAllConstructsInModule(const Environment&, Defmodule *,
                                ConstructActionFunction *,
                                unsigned, bool, void *);
void InitializeConstructHeader(const Environment&, const char *, ConstructType,
                               ConstructHeader *, CLIPSLexeme *);
void SetConstructPPForm(const Environment&, ConstructHeader *, const char *);
ConstructHeader *LookupConstruct(const Environment&, Construct *, const char *, bool);
#if DEBUGGING_FUNCTIONS
bool ConstructPrintWatchAccess(const Environment&, Construct *, const char *,
                               Expression *,
                               ConstructGetWatchFunction *,
                               ConstructSetWatchFunction *);
bool ConstructSetWatchAccess(const Environment&, Construct *, bool,
                             Expression *,
                             ConstructGetWatchFunction *,
                             ConstructSetWatchFunction *);
#endif
bool ConstructsDeletable(const Environment&);

#endif /* _H_constrct */




