/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  01/21/18            */
/*                                                     */
/*             CLASS FUNCTIONS HEADER FILE             */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Borland C (IBM_TBC) and Metrowerks CodeWarrior */
/*            (MAC_MCW, IBM_MCW) are no longer supported.    */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Used genstrcpy and genstrcat instead of strcpy */
/*            and strcat.                                    */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.31: Optimization of slot ID creation previously    */
/*            provided by NewSlotNameID function.            */
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
/*            Removed initial-object support.                */
/*                                                           */
/*************************************************************/

#ifndef _H_classfun

#pragma once

#define _H_classfun

#include "Object.h"
#include "Scanner.h"
#include "Environment.h"
#include "Entities.h"
#include "ExternalFunctions.h"
#include "Construct.h"
#include "Defmodule.h"
#include "Object.h"
#include "Symbol.h"
#include "Evaluation.h"

#define TestTraversalID(traversalRecord, id) TestBitMap(traversalRecord,id)
#define SetTraversalID(traversalRecord, id) SetBitMap(traversalRecord,id)
#define ClearTraversalID(traversalRecord, id) ClearBitMap(traversalRecord,id)


constexpr auto CLASS_TABLE_HASH_SIZE     = 167; // TBD Larger?
constexpr auto SLOT_NAME_TABLE_HASH_SIZE = 167; // TBD Larger?

constexpr auto ISA_ID  = 0;
constexpr auto NAME_ID = 1;

constexpr auto SLOT_NAME_NOT_FOUND = USHRT_MAX;

void IncrementDefclassBusyCount(const Environment::Ptr&, Defclass *);
void DecrementDefclassBusyCount(const Environment::Ptr&, Defclass *);
bool InstancesPurge(const Environment::Ptr&, void *context = nullptr);

void InitializeClasses(const Environment::Ptr&);
SlotDescriptor *FindClassSlot(Defclass *, CLIPSLexeme *);
void ClassExistError(const Environment::Ptr&, const char *, const char *);
void DeleteClassLinks(const Environment::Ptr&, CLASS_LINK *);
void PrintClassName(const Environment::Ptr&, const char *, Defclass *, bool, bool);

#if DEBUGGING_FUNCTIONS
void PrintPackedClassLinks(const Environment::Ptr&, const char *, const char *, PACKED_CLASS_LINKS *);
#endif

void PutClassInTable(const Environment::Ptr&, Defclass *);
void RemoveClassFromTable(const Environment::Ptr&, Defclass *);
void AddClassLink(const Environment::Ptr&, PACKED_CLASS_LINKS *, Defclass *, bool, unsigned int);
void DeleteSubclassLink(const Environment::Ptr&, Defclass *, Defclass *);
void DeleteSuperclassLink(const Environment::Ptr&, Defclass *, Defclass *);
Defclass *NewClass(const Environment::Ptr&, CLIPSLexeme *);
void DeletePackedClassLinks(const Environment::Ptr&, PACKED_CLASS_LINKS *, bool);
void AssignClassID(const Environment::Ptr&, Defclass *);
SLOT_NAME *AddSlotName(const Environment::Ptr&, CLIPSLexeme *, unsigned short, bool);
void DeleteSlotName(const Environment::Ptr&, SLOT_NAME *);
void RemoveDefclass(const Environment::Ptr&, Defclass *);
void InstallClass(const Environment::Ptr&, Defclass *, bool);
void DestroyDefclass(const Environment::Ptr&, Defclass *);

bool IsClassBeingUsed(Defclass *);
bool RemoveAllUserClasses(const Environment::Ptr&);
bool DeleteClassUAG(const Environment::Ptr&, Defclass *);
void MarkBitMapSubclasses(char *, Defclass *, int);

unsigned short FindSlotNameID(const Environment::Ptr&, CLIPSLexeme *);
CLIPSLexeme *FindIDSlotName(const Environment::Ptr&, unsigned short);
SLOT_NAME *FindIDSlotNameHash(const Environment::Ptr&, unsigned short);
int GetTraversalID(const Environment::Ptr&);
void ReleaseTraversalID(const Environment::Ptr&);
unsigned int HashClass(CLIPSLexeme *);

constexpr auto DEFCLASS_DATA = 21;
constexpr auto PRIMITIVE_CLASSES = 9;

enum ClassDefaultsMode {
    CONVENIENCE_MODE,
    CONSERVATION_MODE
};


const char *DefclassName(Defclass *);
const char *DefclassPPForm(Defclass *);
struct defmoduleItemHeader
*GetDefclassModule(const Environment::Ptr&, Defclass *);
const char *DefclassModule(Defclass *);
CLIPSLexeme *GetDefclassNamePointer(Defclass *);
void SetNextDefclass(Defclass *, Defclass *);
void SetDefclassPPForm(const Environment::Ptr&, Defclass *, char *);

Defclass *FindDefclass(const Environment::Ptr&, const char *);
Defclass *FindDefclassInModule(const Environment::Ptr&, const char *);
Defclass *LookupDefclassByMdlOrScope(const Environment::Ptr&, const char *);
Defclass *LookupDefclassInScope(const Environment::Ptr&, const char *);
Defclass *LookupDefclassAnywhere(const Environment::Ptr&, Defmodule *, const char *);
bool DefclassInScope(const Environment::Ptr&, Defclass *, Defmodule *);
Defclass *GetNextDefclass(const Environment::Ptr&, Defclass *);
bool DefclassIsDeletable(Defclass *);

void UndefclassCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
ClassDefaultsMode SetClassDefaultsMode(const Environment::Ptr&, ClassDefaultsMode);
ClassDefaultsMode GetClassDefaultsMode(const Environment::Ptr&);
void GetClassDefaultsModeCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SetClassDefaultsModeCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);

#if DEBUGGING_FUNCTIONS
void PPDefclassCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ListDefclassesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ListDefclasses(const Environment::Ptr&, const char *, Defmodule *);
bool DefclassGetWatchInstances(Defclass *);
void DefclassSetWatchInstances(Defclass *, bool);
bool DefclassGetWatchSlots(Defclass *);
void DefclassSetWatchSlots(Defclass *, bool);
bool DefclassWatchAccess(const Environment::Ptr&, int, bool, Expression *);
bool DefclassWatchPrint(const Environment::Ptr&, const char *, int, Expression *);
#endif

void GetDefclassListFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GetDefclassList(const Environment::Ptr&, CLIPSValue *, Defmodule *);
bool Undefclass(Defclass *, const Environment::Ptr&);
bool HasSuperclass(Defclass *, Defclass *);

CLIPSLexeme *CheckClassAndSlot(UDFContext *, const char *, Defclass **);

void SaveDefclasses(const Environment::Ptr&, Defmodule *, const char *, void * context = nullptr);

struct defclassData : public EnvironmentModule {
    Construct *DefclassConstruct;
    unsigned DefclassModuleIndex;
    EntityRecord DefclassEntityRecord;
    Defclass *PrimitiveClassMap[PRIMITIVE_CLASSES];
    Defclass **ClassIDMap;
    Defclass **ClassTable;
    unsigned short MaxClassID;
    unsigned short AvailClassID;
    SLOT_NAME **SlotNameTable;
    CLIPSLexeme::Ptr ISA_SYMBOL;
    CLIPSLexeme::Ptr NAME_SYMBOL;
#if DEBUGGING_FUNCTIONS
    bool WatchInstances;
    bool WatchSlots;
#endif
    unsigned short CTID = 0;
    struct token ObjectParseToken;
    ClassDefaultsMode ClassDefaultsModeValue = CONVENIENCE_MODE;
    int newSlotID = 2; // IS_A and NAME assigned 0 and 1
};
RegisterEnvironmentModule(defclassData, DEFCLASS_DATA, Defclass);


#if DEBUGGING_FUNCTIONS

void BrowseClassesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void BrowseClasses(Defclass *, const char *);
void DescribeClassCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void DescribeClass(Defclass *, const char *);

#endif /* DEBUGGING_FUNCTIONS */

const char *GetCreateAccessorString(SlotDescriptor *);
void GetDefclassModuleCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SuperclassPCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
bool SuperclassP(Defclass *, Defclass *);
void SubclassPCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
bool SubclassP(Defclass *, Defclass *);
void SlotExistPCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
bool SlotExistP(Defclass *, const char *, bool);
void MessageHandlerExistPCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SlotWritablePCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
bool SlotWritableP(Defclass *, const char *);
void SlotInitablePCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
bool SlotInitableP(Defclass *, const char *);
void SlotPublicPCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
bool SlotPublicP(Defclass *, const char *);
void SlotDirectAccessPCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
bool SlotDirectAccessP(Defclass *, const char *);
void SlotDefaultValueCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
bool SlotDefaultValue(Defclass *, const char *, CLIPSValue *);
void ClassExistPCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
int SlotDefaultP(const Environment::Ptr&, Defclass *, const char *);


void ClassAbstractPCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ClassReactivePCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
Defclass *ClassInfoFnxArgs(UDFContext *, const char *, bool *);
void ClassSlotsCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ClassSuperclassesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ClassSubclassesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GetDefmessageHandlersListCmd(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SlotFacetsCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SlotSourcesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SlotTypesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SlotAllowedValuesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SlotAllowedClassesCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SlotRangeCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SlotCardinalityCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
bool ClassAbstractP(Defclass *);
bool ClassReactiveP(Defclass *);
void ClassSlots(Defclass *, CLIPSValue *, bool);
void GetDefmessageHandlerList(const Environment::Ptr&, Defclass *, CLIPSValue *, bool);
void ClassSuperclasses(Defclass *, CLIPSValue *, bool);
void ClassSubclasses(Defclass *, CLIPSValue *, bool);
void ClassSubclassAddresses(const Environment::Ptr&, Defclass *, UDFValue *, bool);
bool SlotFacets(Defclass *, const char *, CLIPSValue *);
bool SlotSources(Defclass *, const char *, CLIPSValue *);
bool SlotTypes(Defclass *, const char *, CLIPSValue *);
bool SlotAllowedValues(Defclass *, const char *, CLIPSValue *);
bool SlotAllowedClasses(Defclass *, const char *, CLIPSValue *);
bool SlotRange(Defclass *, const char *, CLIPSValue *);
bool SlotCardinality(Defclass *, const char *, CLIPSValue *);

#include "Construct.h"
#include "Object.h"


void SetupObjectSystem(const Environment::Ptr&);
void CreateSystemClasses(const Environment::Ptr&, void *);


bool ParseDefclass(const Environment::Ptr&, const char *);

#if DEFMODULE_CONSTRUCT
void *CreateClassScopeMap(const Environment::Ptr&, Defclass *);
#endif

#define MATCH_RLN            "pattern-match"
#define REACTIVE_RLN         "reactive"
#define NONREACTIVE_RLN      "non-reactive"


struct tempSlotLink {
private:
    SlotDescriptor *desc;
    tempSlotLink *nxt;
public:
    auto getDescription() const noexcept { return desc; }
    void setDescription(SlotDescriptor* value) noexcept { desc = value; }
    auto getNext() const noexcept { return nxt; }
    void setNext(tempSlotLink* value) noexcept { nxt = value; }
};
using TEMP_SLOT_LINK  = tempSlotLink;

TEMP_SLOT_LINK *ParseSlot(const Environment::Ptr&, const char *, const char *, TEMP_SLOT_LINK *, PACKED_CLASS_LINKS *, bool);
void DeleteSlots(const Environment::Ptr&, TEMP_SLOT_LINK *);
#endif









