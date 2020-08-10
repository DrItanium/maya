
#ifndef _H_constrct
#pragma once
#define _H_constrct
#include <memory>
#include <string>
#include <list>
#include "HoldsEnvironmentCallback.h"
#include "LexemeAtom.h"
namespace maya {
    /*
     * Porting this to C++ is interesting because the Construct itself is an abstract interface to provide management functionality
     * for different types by a given string name. However the functionality in CLIPS is actually
     * emulating many different C++ concepts including:
     * - iterators
     * - find
     * - insert
     * - destructors (via the free function pointer)
     * - constructor (via the parse function pointer)
     *
     * In this case, the construct is a class which is responsible for tracking a collection of instances of a
     * given type by only the name of the construct as a whole. These concepts (as stated earlier) are bound to
     * a collection of the given type, not an instance of the type itself. Thus a Construct is really a ConstructList.
     */

    class DefmoduleItemHeader;
    /**
     * @brief Describes a more complex type which can be registered with maya (in clips this is known as a ConstructHeader which was composed into your
     * types!)
     */
    class Construct : public HoldsEnvironmentCallback {
    public:
        enum class Type {
            DefModule,
            DefRule,
            DefTemplate,
            DefFacts,
            DefGlobal,
            DefFunction,
            DefGeneric,
            DefMethod,
            DefClass,
            DefMessageHandler,
            DefInstances,
        };
    public:
        using Self = Construct;
        using Ptr = std::shared_ptr<Self>;
        using PtrList = std::list<Ptr>;
    public:
        Construct(Environment& parent, Type t, const std::string& name, const std::string& ppForm, std::shared_ptr<DefmoduleItemHeader> whichModule);
        virtual ~Construct() = default;
        constexpr auto getType() const noexcept { return _type; }
        auto getName() const noexcept { return _name; }
        std::string getPrettyPrintForm() const noexcept { return _ppForm; }
        auto getWhichModule() const noexcept { return _whichModule; }
    private:
        Type _type;
        Lexeme::Ptr _name;
        std::string _ppForm;
        std::shared_ptr<DefmoduleItemHeader> _whichModule;
        /// @todo reintroduce when binary save and load is reintroduced
        // unsigned long _bsaveID = 0;
        /// @todo figure out what userdata is for...
        // UserData::Ptr _userData;
    };
    class ConstructTypeMetadata : public HoldsEnvironmentCallback {
    public:
        using Self = ConstructTypeMetadata;
        using Ptr = std::shared_ptr<Self>;
        using ParseFunction = std::function<bool(Environment&, const std::string&)>;
        using FindFunction = std::function<Construct::Ptr(Environment&, const std::string&)>;
    public:
        ConstructTypeMetadata(Environment& parent, const std::string& name, const std::string& pluralForm, ParseFunction parseFn, FindFunction findFn);
        bool canParse() const noexcept { return _parse.operator bool(); }
        bool canFind() const noexcept { return _find.operator bool(); }
        bool parse(const std::string& logicalName) { return _parse ? _parse(getParent(), logicalName) : false; }
        Construct::Ptr find(const std::string& name) { return _find ? _find(getParent(), name) : nullptr; }
        std::string getConstructName() const noexcept { return _constructName; }
        std::string getPluralName() const noexcept { return _pluralName; }
    private:
        std::string _constructName;
        std::string _pluralName;
        ParseFunction _parse;
        FindFunction _find;
    };
} // end namespace maya
#if 0
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
/*
 * A metadata record which wraps common functionality, it does not define the common fields found in a construct type itself
 */
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
#endif

#endif /* _H_constrct */




