//
// Created by jwscoggins on 6/20/20.
//

#ifndef MAYA_FACT_H
#define MAYA_FACT_H

typedef struct factBuilder FactBuilder;
typedef struct factModifier FactModifier;
typedef struct factHashEntry FactHashEntry;
struct FactPatternNode;
#include "Setup.h"
#include "Network.h"
#include "Entities.h"
#include "Deftemplate.h"
#include "Expression.h"
#include "Evaluation.h"
#include "Scanner.h"
#include "Symbol.h"
#include "Reorder.h"

typedef void ModifyCallFunction(const Environment&, Fact *, Fact *, void *);
typedef struct modifyCallFunctionItem ModifyCallFunctionItem;

enum RetractError {
    RE_NO_ERROR = 0,
    RE_nullptr_POINTER_ERROR,
    RE_COULD_NOT_RETRACT_ERROR,
    RE_RULE_NETWORK_ERROR
} ;

enum AssertError{
    AE_NO_ERROR = 0,
    AE_nullptr_POINTER_ERROR,
    AE_RETRACTED_ERROR,
    AE_COULD_NOT_ASSERT_ERROR,
    AE_RULE_NETWORK_ERROR
} ;

enum AssertStringError {
    ASE_NO_ERROR = 0,
    ASE_nullptr_POINTER_ERROR,
    ASE_PARSING_ERROR,
    ASE_COULD_NOT_ASSERT_ERROR,
    ASE_RULE_NETWORK_ERROR
};

enum FactBuilderError {
    FBE_NO_ERROR = 0,
    FBE_nullptr_POINTER_ERROR,
    FBE_DEFTEMPLATE_NOT_FOUND_ERROR,
    FBE_IMPLIED_DEFTEMPLATE_ERROR,
    FBE_COULD_NOT_ASSERT_ERROR,
    FBE_RULE_NETWORK_ERROR
};

enum FactModifierError {
    FME_NO_ERROR = 0,
    FME_nullptr_POINTER_ERROR,
    FME_RETRACTED_ERROR,
    FME_IMPLIED_DEFTEMPLATE_ERROR,
    FME_COULD_NOT_MODIFY_ERROR,
    FME_RULE_NETWORK_ERROR
};

struct modifyCallFunctionItem {
    const char *name;
    ModifyCallFunction *func;
    int priority;
    ModifyCallFunctionItem *next;
    void *context;
};

struct Fact {
    union {
        PatternEntity patternHeader;
        TypeHeader header;
    };
    Deftemplate *whichDeftemplate;
    void *list;
    long long factIndex;
    unsigned long hashValue;
    bool garbage: 1;
    Fact *previousFact;
    Fact *nextFact;
    Fact *previousTemplateFact;
    Fact *nextTemplateFact;
    Multifield *basisSlots;
    Multifield theProposition;
};

struct factBuilder {
    Environment fbEnv;
    Deftemplate *fbDeftemplate;
    CLIPSValue *fbValueArray;
};

struct factModifier {
    Environment fmEnv;
    Fact *fmOldFact;
    CLIPSValue *fmValueArray;
    char *changeMap;
};

constexpr auto FACTS_DATA = 3;

struct factsData {
    bool ChangeToFactList;
#if DEBUGGING_FUNCTIONS
    bool WatchFacts;
#endif
    Fact DummyFact;
    Fact *GarbageFacts;
    Fact *LastFact;
    Fact *FactList;
    long long NextFactIndex;
    unsigned long NumberOfFacts;
    struct callFunctionItemWithArg *ListOfAssertFunctions;
    struct callFunctionItemWithArg *ListOfRetractFunctions;
    ModifyCallFunctionItem *ListOfModifyFunctions;
    PatternEntityRecord FactInfo;
    Deftemplate *CurrentDeftemplate;
    struct factHashEntry **FactHashTable;
    unsigned long FactHashTableSize;
    bool FactDuplication;
    Fact *CurrentPatternFact;
    struct multifieldMarker *CurrentPatternMarks;
    long LastModuleIndex;
    RetractError retractError;
    AssertError assertError;
    AssertStringError assertStringError;
    FactModifierError factModifierError;
    FactBuilderError factBuilderError;
};
RegisterEnvironmentModule(factsData, FACTS_DATA);
#define FactData(theEnv) (GetEnvironmentData(theEnv,FACTS_DATA))

Fact *Assert(Fact *);
AssertStringError GetAssertStringError(const Environment&);
Fact *AssertDriver(Fact *, long long, Fact *, Fact *, char *);
Fact *AssertString(const Environment&, const char *);
Fact *CreateFact(Deftemplate *);
void ReleaseFact(Fact *);
void DecrementFactCallback(const Environment&, Fact *);
long long FactIndex(Fact *);
GetSlotError GetFactSlot(Fact *, const char *, CLIPSValue *);
void PrintFactWithIdentifier(const Environment&, const char *, Fact *, const char *);
void PrintFact(const Environment&, const char *, Fact *, bool, bool, const char *);
void PrintFactIdentifierInLongForm(const Environment&, const char *, Fact *);
RetractError Retract(Fact *);
RetractError RetractDriver(const Environment&, Fact *, bool, char *);
RetractError RetractAllFacts(const Environment&);
Fact *CreateFactBySize(const Environment&, size_t);
void FactInstall(const Environment&, Fact *);
void FactDeinstall(const Environment&, Fact *);
Fact *GetNextFact(const Environment&, Fact *);
Fact *GetNextFactInScope(const Environment&, Fact *);
void FactPPForm(Fact *, StringBuilder *, bool);
bool GetFactListChanged(const Environment&);
void SetFactListChanged(const Environment&, bool);
unsigned long GetNumberOfFacts(const Environment&);
void InitializeFacts(const Environment&);
Fact *FindIndexedFact(const Environment&, long long);
void RetainFact(Fact *);
void IncrementFactCallback(const Environment&, Fact *);
void PrintFactIdentifier(const Environment&, const char *, Fact *);
void DecrementFactBasisCount(const Environment&, Fact *);
void IncrementFactBasisCount(const Environment&, Fact *);
bool FactIsDeleted(const Environment&, Fact *);
void ReturnFact(const Environment&, Fact *);
void MatchFactFunction(const Environment&, Fact *);
bool PutFactSlot(Fact *, const char *, CLIPSValue *);
bool AssignFactSlotDefaults(Fact *);
bool CopyFactSlotValues(const Environment&, Fact *, Fact *);
bool DeftemplateSlotDefault(const Environment&, Deftemplate *,
                            struct templateSlot *, UDFValue *, bool);
bool AddAssertFunction(const Environment&, const char *,
                       VoidCallFunctionWithArg *, int, void *);
bool RemoveAssertFunction(const Environment&, const char *);
bool AddRetractFunction(const Environment&, const char *,
                        VoidCallFunctionWithArg *, int, void *);
bool RemoveRetractFunction(const Environment&, const char *);
FactBuilder *CreateFactBuilder(const Environment&, const char *);
PutSlotError FBPutSlot(FactBuilder *, const char *, CLIPSValue *);
Fact *FBAssert(FactBuilder *);
void FBDispose(FactBuilder *);
void FBAbort(FactBuilder *);
FactBuilderError FBSetDeftemplate(FactBuilder *, const char *);
PutSlotError FBPutSlotCLIPSInteger(FactBuilder *, const char *, CLIPSInteger *);
PutSlotError FBPutSlotInteger(FactBuilder *, const char *, long long);
PutSlotError FBPutSlotCLIPSFloat(FactBuilder *, const char *, CLIPSFloat *);
PutSlotError FBPutSlotFloat(FactBuilder *, const char *, double);
PutSlotError FBPutSlotCLIPSLexeme(FactBuilder *, const char *, CLIPSLexeme *);
PutSlotError FBPutSlotSymbol(FactBuilder *, const char *, const char *);
PutSlotError FBPutSlotString(FactBuilder *, const char *, const char *);
PutSlotError FBPutSlotInstanceName(FactBuilder *, const char *, const char *);
PutSlotError FBPutSlotFact(FactBuilder *, const char *, Fact *);
PutSlotError FBPutSlotInstance(FactBuilder *, const char *, Instance *);
PutSlotError FBPutSlotCLIPSExternalAddress(FactBuilder *, const char *, CLIPSExternalAddress *);
PutSlotError FBPutSlotMultifield(FactBuilder *, const char *, Multifield *);
FactBuilderError FBError(const Environment&);
FactModifier *CreateFactModifier(const Environment&, Fact *);
PutSlotError FMPutSlot(FactModifier *, const char *, CLIPSValue *);
Fact *FMModify(FactModifier *);
void FMDispose(FactModifier *);
void FMAbort(FactModifier *);
FactModifierError FMSetFact(FactModifier *, Fact *);
PutSlotError FMPutSlotCLIPSInteger(FactModifier *, const char *, CLIPSInteger *);
PutSlotError FMPutSlotInteger(FactModifier *, const char *, long long);
PutSlotError FMPutSlotCLIPSFloat(FactModifier *, const char *, CLIPSFloat *);
PutSlotError FMPutSlotFloat(FactModifier *, const char *, double);
PutSlotError FMPutSlotCLIPSLexeme(FactModifier *, const char *, CLIPSLexeme *);
PutSlotError FMPutSlotSymbol(FactModifier *, const char *, const char *);
PutSlotError FMPutSlotString(FactModifier *, const char *, const char *);
PutSlotError FMPutSlotInstanceName(FactModifier *, const char *, const char *);
PutSlotError FMPutSlotFact(FactModifier *, const char *, Fact *);
PutSlotError FMPutSlotInstance(FactModifier *, const char *, Instance *);
PutSlotError FMPutSlotExternalAddress(FactModifier *, const char *, CLIPSExternalAddress *);
PutSlotError FMPutSlotMultifield(FactModifier *, const char *, Multifield *);
FactModifierError FMError(const Environment&);

bool AddModifyFunction(const Environment&, const char *, ModifyCallFunction *, int, void *);
bool RemoveModifyFunction(const Environment&, const char *);
ModifyCallFunctionItem *AddModifyFunctionToCallList(const Environment&, const char *, int,
                                                    ModifyCallFunction *, ModifyCallFunctionItem *, void *);
ModifyCallFunctionItem *RemoveModifyFunctionFromCallList(const Environment&, const char *,
                                                         ModifyCallFunctionItem *, bool *);
void DeallocateModifyCallList(const Environment&, ModifyCallFunctionItem *);
// factmanager end
#if FACT_SET_QUERIES

typedef struct query_template {
    Deftemplate *templatePtr;
    struct query_template *chain, *nxt;
} QUERY_TEMPLATE;

typedef struct query_soln {
    Fact **soln;
    struct query_soln *nxt;
} QUERY_SOLN;

typedef struct query_core {
    Fact **solns;
    Expression *query, *action;
    QUERY_SOLN *soln_set, *soln_bottom;
    unsigned soln_size, soln_cnt;
    UDFValue *result;
} QUERY_CORE;

typedef struct query_stack {
    QUERY_CORE *core;
    struct query_stack *nxt;
} QUERY_STACK;

constexpr auto FACT_QUERY_DATA = 63;

struct factQueryData {
    CLIPSLexeme *QUERY_DELIMITER_SYMBOL;
    QUERY_CORE *QueryCore;
    QUERY_STACK *QueryCoreStack;
    bool AbortQuery;
};
RegisterEnvironmentModule(factQueryData, FACT_QUERY_DATA);
#define FactQueryData(theEnv) (GetEnvironmentData(theEnv,FACT_QUERY_DATA))

#define QUERY_DELIMITER_STRING     "(QDS)"

void SetupFactQuery(const Environment&);
void GetQueryFact(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void GetQueryFactSlot(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void AnyFacts(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void QueryFindFact(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void QueryFindAllFacts(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void QueryDoForFact(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void QueryDoForAllFacts(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void DelayedQueryDoForAllFacts(const Environment&theEnv, UDFContext *context, UDFValue *ret);

Expression *FactParseQueryNoAction(const Environment&, Expression *, const char *);
Expression *FactParseQueryAction(const Environment&, Expression *, const char *);
#endif /* FACT_SET_QUERIES */

bool FactPNGetVar1(const Environment&, void *, UDFValue *);
bool FactPNGetVar2(const Environment&, void *, UDFValue *);
bool FactPNGetVar3(const Environment&, void *, UDFValue *);
bool FactJNGetVar1(const Environment&, void *, UDFValue *);
bool FactJNGetVar2(const Environment&, void *, UDFValue *);
bool FactJNGetVar3(const Environment&, void *, UDFValue *);
bool FactSlotLength(const Environment&, void *, UDFValue *);
bool FactJNCompVars1(const Environment&, void *, UDFValue *);
bool FactJNCompVars2(const Environment&, void *, UDFValue *);
bool FactPNCompVars1(const Environment&, void *, UDFValue *);
bool FactPNConstant1(const Environment&, void *, UDFValue *);
bool FactPNConstant2(const Environment&, void *, UDFValue *);
bool FactStoreMultifield(const Environment&, void *, UDFValue *);
size_t AdjustFieldPosition(const Environment&, struct multifieldMarker *,
                           unsigned short, unsigned short, size_t *);
Expression *BuildRHSAssert(const Environment&, const char *, struct token *, bool *, bool, bool, const char *);
Expression *GetAssertArgument(const Environment&, const char *, struct token *, bool *, TokenType, bool, bool *);
Expression *GetRHSPattern(const Environment&, const char *, struct token *, bool *, bool,
                           bool, bool, TokenType);
Fact *StringToFact(const Environment&, const char *);

void PrintFactJNCompVars1(const Environment&, const char *, void *);
void PrintFactJNCompVars2(const Environment&, const char *, void *);
void PrintFactPNCompVars1(const Environment&, const char *, void *);
void PrintFactJNGetVar1(const Environment&, const char *, void *);
void PrintFactJNGetVar2(const Environment&, const char *, void *);
void PrintFactJNGetVar3(const Environment&, const char *, void *);
void PrintFactPNGetVar1(const Environment&, const char *, void *);
void PrintFactPNGetVar2(const Environment&, const char *, void *);
void PrintFactPNGetVar3(const Environment&, const char *, void *);
void PrintFactSlotLength(const Environment&, const char *, void *);
void PrintFactPNConstant1(const Environment&, const char *, void *);
void PrintFactPNConstant2(const Environment&, const char *, void *);

void FactPatternMatch(const Environment&, Fact *,
                      FactPatternNode *, size_t, size_t,
                      struct multifieldMarker *,
                      struct multifieldMarker *);
void MarkFactPatternForIncrementalReset(const Environment&, PatternNodeHeader *, bool);
void FactsIncrementalReset(const Environment&);

constexpr auto FACTBIN_DATA = 62;

struct factBinaryData {
    FactPatternNode *FactPatternArray;
    unsigned long NumberOfPatterns;
};
RegisterEnvironmentModule(factBinaryData, FACTBIN_DATA);
#define FactBinaryData(theEnv) (GetEnvironmentData(theEnv,FACTBIN_DATA))

void FactBinarySetup(const Environment&);

#define BsaveFactPatternIndex(patPtr) ((patPtr == nullptr) ? ULONG_MAX : ((FactPatternNode *) patPtr)->bsaveID)
#define BloadFactPatternPointer(i) ((FactPatternNode *) ((i == ULONG_MAX) ? nullptr : &FactBinaryData(theEnv)->FactPatternArray[i]))

struct FactPatternNode {
    PatternNodeHeader header;
    unsigned long bsaveID;
    unsigned short whichField; // TBD seems to be 1 based rather than 0 based
    unsigned short whichSlot;
    unsigned short leaveFields;
    Expression *networkTest;
    FactPatternNode *nextLevel;
    FactPatternNode *lastLevel;
    FactPatternNode *leftNode;
    FactPatternNode *rightNode;
};

void InitializeFactPatterns(const Environment&);
void DestroyFactPatternNetwork(const Environment&, FactPatternNode *);
#if DEFTEMPLATE_CONSTRUCT

void FactCommandDefinitions(const Environment&);
void AssertCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void RetractCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void AssertStringFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void FactsCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void Facts(const Environment&, const char *, Defmodule *, long long, long long, long long);
void SetFactDuplicationCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void GetFactDuplicationCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void FactIndexFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);

#endif /* DEFTEMPLATE_CONSTRUCT */

void FactFileCommandDefinitions(const Environment&);
void SaveFactsCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void LoadFactsCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
long SaveFacts(const Environment&, const char *, SaveScope);
long SaveFactsDriver(const Environment&, const char *, SaveScope, Expression *);
long LoadFacts(const Environment&, const char *);
long LoadFactsFromString(const Environment&, const char *, size_t);
void BinarySaveFactsCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void BinaryLoadFactsCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
long BinarySaveFacts(const Environment&, const char *, SaveScope);
long BinarySaveFactsDriver(const Environment&, const char *, SaveScope, Expression *);
long BinaryLoadFacts(const Environment&, const char *);

void FactFunctionDefinitions(const Environment&);
/**********************************************************/
/* factGetVarPN1Call: This structure is used to store the */
/*   arguments to the most general extraction routine for */
/*   retrieving a variable from the fact pattern network. */
/**********************************************************/
struct factGetVarPN1Call {
    bool factAddress: 1;
    bool allFields: 1;
    unsigned short whichField;
    unsigned short whichSlot;
};

/***********************************************************/
/* factGetVarPN2Call: This structure is used to store the  */
/*   arguments to the most specific extraction routine for */
/*   retrieving a variable from the fact pattern network.  */
/*   It is used for retrieving the single value stored in  */
/*   a single field slot (the slot index can be used to    */
/*   directly to retrieve the value from the fact array).  */
/***********************************************************/
struct factGetVarPN2Call {
    unsigned short whichSlot;
};

/**********************************************************/
/* factGetVarPN3Call:  */
/**********************************************************/
struct factGetVarPN3Call {
    bool fromBeginning: 1;
    bool fromEnd: 1;
    unsigned short beginOffset;
    unsigned short endOffset;
    unsigned short whichSlot;
};

/**************************************************************/
/* factConstantPN1Call: Used for testing for a constant value */
/*   in the fact pattern network. Compare the value of a      */
/*   single field slot to a constant.                         */
/**************************************************************/
struct factConstantPN1Call {
    bool testForEquality: 1;
    unsigned short whichSlot;
};

/******************************************************************/
/* factConstantPN2Call: Used for testing for a constant value in  */
/*   the fact pattern network. Compare the value of a multifield  */
/*   slot to a constant (where the value retrieved for comparison */
/*   from the slot contains no multifields before or only one     */
/*   multifield before and none after).                           */
/******************************************************************/
struct factConstantPN2Call {
    bool testForEquality: 1;
    bool fromBeginning: 1;
    unsigned short offset;
    unsigned short whichSlot;
};

/**********************************************************/
/* factGetVarJN1Call: This structure is used to store the */
/*   arguments to the most general extraction routine for */
/*   retrieving a fact variable from the join network.    */
/**********************************************************/
struct factGetVarJN1Call {
    bool factAddress: 1;
    bool allFields: 1;
    bool lhs: 1;
    bool rhs: 1;
    unsigned short whichPattern;
    unsigned short whichSlot;
    unsigned short whichField;
};

/**********************************************************/
/* factGetVarJN2Call:  */
/**********************************************************/
struct factGetVarJN2Call {
    bool lhs: 1;
    bool rhs: 1;
    unsigned short whichPattern;
    unsigned short whichSlot;
};

/**********************************************************/
/* factGetVarJN3Call:  */
/**********************************************************/
struct factGetVarJN3Call {
    bool fromBeginning: 1;
    bool fromEnd: 1;
    bool lhs: 1;
    bool rhs: 1;
    unsigned short beginOffset;
    unsigned short endOffset;
    unsigned short whichPattern;
    unsigned short whichSlot;
};

/**********************************************************/
/* factCompVarsPN1Call:  */
/**********************************************************/
struct factCompVarsPN1Call {
    bool pass: 1;
    bool fail: 1;
    unsigned short field1;
    unsigned short field2;
};

/**********************************************************/
/* factCompVarsJN1Call:  */
/**********************************************************/
struct factCompVarsJN1Call {
    bool pass: 1;
    bool fail: 1;
    bool p1lhs: 1;
    bool p1rhs: 1;
    bool p2lhs: 1;
    bool p2rhs: 1;
    unsigned short pattern1;
    unsigned short pattern2;
    unsigned short slot1;
    unsigned short slot2;
};

/**********************************************************/
/* factCompVarsJN2Call:  */
/**********************************************************/
struct factCompVarsJN2Call {
    bool pass: 1;
    bool fail: 1;
    bool p1lhs: 1;
    bool p1rhs: 1;
    bool p2lhs: 1;
    bool p2rhs: 1;
    bool fromBeginning1: 1;
    bool fromBeginning2: 1;
    unsigned short offset1;
    unsigned short offset2;
    unsigned short pattern1;
    unsigned short pattern2;
    unsigned short slot1;
    unsigned short slot2;
};

/**********************************************************/
/* factCheckLengthPNCall: This structure is used to store */
/*   the  arguments to the routine for determining if the */
/*   length of a multifield slot is equal or greater than */
/*   a specified value.                                   */
/**********************************************************/

struct factCheckLengthPNCall {
    bool exactly : 1;
    unsigned short minLength;
    unsigned short whichSlot;
};

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

void InitializeFactReteFunctions(const Environment&);
Expression *FactPNVariableComparison(const Environment&, struct lhsParseNode *,
                                      struct lhsParseNode *);
Expression *FactJNVariableComparison(const Environment&, struct lhsParseNode *,
                                      struct lhsParseNode *, bool);
void FactReplaceGetvar(const Environment&, Expression *, struct lhsParseNode *, int);
void FactReplaceGetfield(const Environment&, Expression *, struct lhsParseNode *);
Expression *FactGenPNConstant(const Environment&, struct lhsParseNode *);
Expression *FactGenGetfield(const Environment&, struct lhsParseNode *);
Expression *FactGenGetvar(const Environment&, struct lhsParseNode *, int);
Expression *FactGenCheckLength(const Environment&, struct lhsParseNode *);
Expression *FactGenCheckZeroLength(const Environment&, unsigned short);
void FactRelationFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
CLIPSLexeme *FactRelation(Fact *);
Deftemplate *FactDeftemplate(Fact *);
void FactExistpFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool FactExistp(Fact *);
void FactSlotValueFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void FactSlotValue(const Environment&, Fact *, const char *, CLIPSValue *);
void FactSlotNamesFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void FactSlotNames(Fact *, CLIPSValue *);
void GetFactListFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void GetFactList(const Environment&, CLIPSValue *, Defmodule *);
void PPFactFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void PPFact(Fact *, const char *, bool);
Fact *GetFactAddressOrIndexArgument(UDFContext *, bool);
void FactAddresspFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);

struct factHashEntry {
    Fact *theFact;
    FactHashEntry *next;
};

constexpr auto SIZE_FACT_HASH = 16231;

void AddHashedFact(const Environment&, Fact *, size_t);
bool RemoveHashedFact(const Environment&, Fact *);
size_t HandleFactDuplication(const Environment&, Fact *, Fact **, long long);
bool GetFactDuplication(const Environment&);
bool SetFactDuplication(const Environment&, bool);
void InitializeFactHashTable(const Environment&);
void ShowFactHashTableCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
size_t HashFact(Fact *);
bool FactWillBeAsserted(const Environment&, Fact *);

bool FactPatternParserFind(CLIPSLexeme *);
struct lhsParseNode *FactPatternParse(const Environment&, const char *, struct token *);
struct lhsParseNode *SequenceRestrictionParse(const Environment&, const char *, struct token *);

#endif //MAYA_FACT_H
