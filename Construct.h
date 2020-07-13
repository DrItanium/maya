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

#include "Entities.hxx"
#include "UserData.h"
#include "Defmodule.h"
#include "Utility.h"
#include "StringFunctions.h"

typedef void SaveCallFunction(const Environment::Ptr&, Defmodule *, const char *, void *);
typedef struct saveCallFunctionItem SaveCallFunctionItem;

typedef void ParserErrorFunction(const Environment::Ptr&, const char *, const char *, const char *, long, void *);
typedef bool BeforeResetFunction(const Environment::Ptr&);

#define CHS (ConstructHeader *)

struct saveCallFunctionItem {
    const char *name;
    SaveCallFunction *func;
    int priority;
    SaveCallFunctionItem *next;
};
typedef bool ConstructParseFunction(const Environment::Ptr&, const char*);
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

bool Clear(const Environment::Ptr&);
void Reset(const Environment::Ptr&);
bool Save(const Environment::Ptr&, const char *);

void InitializeConstructData(const Environment::Ptr&);
bool AddResetFunction(const Environment::Ptr&, const char *, VoidCallFunction *, int, void *context = nullptr);
bool RemoveResetFunction(const Environment::Ptr&, const char *);
bool AddClearReadyFunction(const Environment::Ptr&, const char *, BoolCallFunction *, int, void *context = nullptr);
bool RemoveClearReadyFunction(const Environment::Ptr&, const char *);
bool AddClearFunction(const Environment::Ptr&, const char *, VoidCallFunction *, int, void *context = nullptr);
bool RemoveClearFunction(const Environment::Ptr&, const char *);
void IncrementClearReadyLocks(const Environment::Ptr&);
void DecrementClearReadyLocks(const Environment::Ptr&);
Construct *AddConstruct(const Environment::Ptr&, const char *, const char *,
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
bool RemoveConstruct(const Environment::Ptr&, const char *);
void SetCompilationsWatch(const Environment::Ptr&, bool);
bool GetCompilationsWatch(const Environment::Ptr&);
void SetPrintWhileLoading(const Environment::Ptr&, bool);
bool GetPrintWhileLoading(const Environment::Ptr&);
void SetLoadInProgress(const Environment::Ptr&, bool);
bool GetLoadInProgress(const Environment::Ptr&);
bool ExecutingConstruct(const Environment::Ptr&);
void SetExecutingConstruct(const Environment::Ptr&, bool);
void InitializeConstructs(const Environment::Ptr&);
BeforeResetFunction *SetBeforeResetFunction(const Environment::Ptr&, BeforeResetFunction *);
void ResetCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ClearCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
bool ClearReady(const Environment::Ptr&);
Construct *FindConstruct(const Environment::Ptr&, const char *);
void DeinstallConstructHeader(const Environment::Ptr&, ConstructHeader *);
void DestroyConstructHeader(const Environment::Ptr&, ConstructHeader *);
ParserErrorFunction *SetParserErrorCallback(const Environment::Ptr&, ParserErrorFunction *, void *context = nullptr);

bool AddSaveFunction(const Environment::Ptr&, const char *, SaveCallFunction *, int, void *context = nullptr);
bool RemoveSaveFunction(const Environment::Ptr&, const char *);
SaveCallFunctionItem *AddSaveFunctionToCallList(const Environment::Ptr&, const char *, int,
                                                SaveCallFunction *, SaveCallFunctionItem *, void *context = nullptr);
SaveCallFunctionItem *RemoveSaveFunctionFromCallList(const Environment::Ptr&, const char *,
                                                     SaveCallFunctionItem *, bool *);
void DeallocateSaveCallList(const Environment::Ptr&, SaveCallFunctionItem *);

#if BLOAD_AND_BSAVE

struct bsaveConstructHeader {
    unsigned long name;
    unsigned long whichModule;
    unsigned long next;
};

void MarkConstructHeaderNeededItems(ConstructHeader *, unsigned long);
void AssignBsaveConstructHeaderVals(bsaveConstructHeader *,
                                    ConstructHeader *);

void UpdateConstructHeader(const Environment::Ptr&, struct bsaveConstructHeader *,
                           ConstructHeader *, ConstructType, size_t, void *, size_t, void *);
void UnmarkConstructHeader(const Environment::Ptr&, ConstructHeader *);

#endif
enum LoadError {
    LE_NO_ERROR = 0,
    LE_OPEN_FILE_ERROR,
    LE_PARSING_ERROR,
};

LoadError Load(const Environment::Ptr&, const char *);
bool LoadConstructsFromLogicalName(const Environment::Ptr&, const char *);
bool LoadFromString(const Environment::Ptr&, const char *, size_t);
BuildError ParseConstruct(const Environment::Ptr&, const char *, const char *);
void ImportExportConflictMessage(const Environment::Ptr&, const char *, const char *,
                                 const char *, const char *);
void FlushParsingMessages(const Environment::Ptr&);
char *GetParsingFileName(const Environment::Ptr&);
void SetParsingFileName(const Environment::Ptr&, const char *);
char *GetErrorFileName(const Environment::Ptr&);
void SetErrorFileName(const Environment::Ptr&, const char *);
char *GetWarningFileName(const Environment::Ptr&);
void SetWarningFileName(const Environment::Ptr&, const char *);
void CreateErrorCaptureRouter(const Environment::Ptr&);
void DeleteErrorCaptureRouter(const Environment::Ptr&);

typedef bool ConstructGetWatchFunction(void *);
typedef void ConstructSetWatchFunction(void *, bool);
typedef void ConstructActionFunction(const Environment::Ptr&, ConstructHeader *, void *);

void AddConstructToModule(ConstructHeader *);
bool DeleteNamedConstruct(const Environment::Ptr&, const char *, Construct *);
ConstructHeader *FindNamedConstructInModule(const Environment::Ptr&, const char *, Construct *);
ConstructHeader *FindNamedConstructInModuleOrImports(const Environment::Ptr&, const char *, Construct *);
void UndefconstructCommand(UDFContext *, const char *, Construct *);
bool PPConstruct(const Environment::Ptr&, const char *, const char *, Construct *);
const char *PPConstructNil(const Environment::Ptr&, const char *, Construct *);
CLIPSLexeme::Ptr GetConstructModuleCommand(UDFContext *, const char *, Construct *);
Defmodule *GetConstructModule(const Environment::Ptr&, const char *, Construct *);
bool Undefconstruct(const Environment::Ptr&, ConstructHeader *, Construct *);
bool UndefconstructAll(const Environment::Ptr&, Construct *);
void SaveConstruct(const Environment::Ptr&, Defmodule *, const char *, Construct *);
const char *GetConstructNameString(ConstructHeader *);
const char *GetConstructModuleName(ConstructHeader *);
CLIPSLexeme *GetConstructNamePointer(ConstructHeader *);
void GetConstructListFunction(UDFContext *, UDFValue *, Construct *);
void GetConstructList(const Environment::Ptr&, UDFValue *, Construct *,
                      Defmodule *);
void ListConstructCommand(UDFContext *, Construct *);
void ListConstruct(const Environment::Ptr&, Construct *, const char *, Defmodule *);
void SetNextConstruct(ConstructHeader *, ConstructHeader *);
struct defmoduleItemHeader *GetConstructModuleItem(ConstructHeader *);
const char *GetConstructPPForm(ConstructHeader *);
void PPConstructCommand(UDFContext *, const char *, Construct *, UDFValue *);
ConstructHeader *GetNextConstructItem(const Environment::Ptr&, ConstructHeader *, unsigned);
struct defmoduleItemHeader *GetConstructModuleItemByIndex(const Environment::Ptr&, Defmodule *, unsigned);
void FreeConstructHeaderModule(const Environment::Ptr&, struct defmoduleItemHeader *,
                               Construct *);
void DoForAllConstructs(const Environment::Ptr&,
                        ConstructActionFunction *,
                        unsigned, bool, void *);
void DoForAllConstructsInModule(const Environment::Ptr&, Defmodule *,
                                ConstructActionFunction *,
                                unsigned, bool, void *);
void InitializeConstructHeader(const Environment::Ptr&, const char *, ConstructType,
                               ConstructHeader *, CLIPSLexeme *);
void SetConstructPPForm(const Environment::Ptr&, ConstructHeader *, const char *);
ConstructHeader *LookupConstruct(const Environment::Ptr&, Construct *, const char *, bool);
#if DEBUGGING_FUNCTIONS
bool ConstructPrintWatchAccess(const Environment::Ptr&, Construct *, const char *,
                               Expression *,
                               ConstructGetWatchFunction *,
                               ConstructSetWatchFunction *);
bool ConstructSetWatchAccess(const Environment::Ptr&, Construct *, bool,
                             Expression *,
                             ConstructGetWatchFunction *,
                             ConstructSetWatchFunction *);
#endif
bool ConstructsDeletable(const Environment::Ptr&);

#endif /* _H_constrct */




