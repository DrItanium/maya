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

#define TestTraversalID(traversalRecord, id) TestBitMap(traversalRecord,id)
#define SetTraversalID(traversalRecord, id) SetBitMap(traversalRecord,id)
#define ClearTraversalID(traversalRecord, id) ClearBitMap(traversalRecord,id)


constexpr auto CLASS_TABLE_HASH_SIZE     = 167; // TBD Larger?
constexpr auto SLOT_NAME_TABLE_HASH_SIZE = 167; // TBD Larger?

constexpr auto ISA_ID  = 0;
constexpr auto NAME_ID = 1;

constexpr auto SLOT_NAME_NOT_FOUND = USHRT_MAX;

void IncrementDefclassBusyCount(const Environment&, Defclass *);
void DecrementDefclassBusyCount(const Environment&, Defclass *);
bool InstancesPurge(const Environment&, void *context = nullptr);

void InitializeClasses(const Environment&);
SlotDescriptor *FindClassSlot(Defclass *, CLIPSLexeme *);
void ClassExistError(const Environment&, const char *, const char *);
void DeleteClassLinks(const Environment&, CLASS_LINK *);
void PrintClassName(const Environment&, const char *, Defclass *, bool, bool);

#if DEBUGGING_FUNCTIONS
void PrintPackedClassLinks(const Environment&, const char *, const char *, PACKED_CLASS_LINKS *);
#endif

void PutClassInTable(const Environment&, Defclass *);
void RemoveClassFromTable(const Environment&, Defclass *);
void AddClassLink(const Environment&, PACKED_CLASS_LINKS *, Defclass *, bool, unsigned int);
void DeleteSubclassLink(const Environment&, Defclass *, Defclass *);
void DeleteSuperclassLink(const Environment&, Defclass *, Defclass *);
Defclass *NewClass(const Environment&, CLIPSLexeme *);
void DeletePackedClassLinks(const Environment&, PACKED_CLASS_LINKS *, bool);
void AssignClassID(const Environment&, Defclass *);
SLOT_NAME *AddSlotName(const Environment&, CLIPSLexeme *, unsigned short, bool);
void DeleteSlotName(const Environment&, SLOT_NAME *);
void RemoveDefclass(const Environment&, Defclass *);
void InstallClass(const Environment&, Defclass *, bool);
void DestroyDefclass(const Environment&, Defclass *);

bool IsClassBeingUsed(Defclass *);
bool RemoveAllUserClasses(const Environment&);
bool DeleteClassUAG(const Environment&, Defclass *);
void MarkBitMapSubclasses(char *, Defclass *, int);

unsigned short FindSlotNameID(const Environment&, CLIPSLexeme *);
CLIPSLexeme *FindIDSlotName(const Environment&, unsigned short);
SLOT_NAME *FindIDSlotNameHash(const Environment&, unsigned short);
int GetTraversalID(const Environment&);
void ReleaseTraversalID(const Environment&);
unsigned int HashClass(CLIPSLexeme *);

constexpr auto DEFCLASS_DATA = 21;
constexpr auto PRIMITIVE_CLASSES = 9;

enum ClassDefaultsMode {
    CONVENIENCE_MODE,
    CONSERVATION_MODE
};

#include "Construct.h"
#include "Defmodule.h"
#include "Object.h"
#include "Symbol.h"

const char *DefclassName(Defclass *);
const char *DefclassPPForm(Defclass *);
struct defmoduleItemHeader
*GetDefclassModule(const Environment&, Defclass *);
const char *DefclassModule(Defclass *);
CLIPSLexeme *GetDefclassNamePointer(Defclass *);
void SetNextDefclass(Defclass *, Defclass *);
void SetDefclassPPForm(const Environment&, Defclass *, char *);

Defclass *FindDefclass(const Environment&, const char *);
Defclass *FindDefclassInModule(const Environment&, const char *);
Defclass *LookupDefclassByMdlOrScope(const Environment&, const char *);
Defclass *LookupDefclassInScope(const Environment&, const char *);
Defclass *LookupDefclassAnywhere(const Environment&, Defmodule *, const char *);
bool DefclassInScope(const Environment&, Defclass *, Defmodule *);
Defclass *GetNextDefclass(const Environment&, Defclass *);
bool DefclassIsDeletable(Defclass *);

void UndefclassCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
ClassDefaultsMode SetClassDefaultsMode(const Environment&, ClassDefaultsMode);
ClassDefaultsMode GetClassDefaultsMode(const Environment&);
void GetClassDefaultsModeCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SetClassDefaultsModeCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);

#if DEBUGGING_FUNCTIONS
void PPDefclassCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ListDefclassesCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ListDefclasses(const Environment&, const char *, Defmodule *);
bool DefclassGetWatchInstances(Defclass *);
void DefclassSetWatchInstances(Defclass *, bool);
bool DefclassGetWatchSlots(Defclass *);
void DefclassSetWatchSlots(Defclass *, bool);
bool DefclassWatchAccess(const Environment&, int, bool, Expression *);
bool DefclassWatchPrint(const Environment&, const char *, int, Expression *);
#endif

void GetDefclassListFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void GetDefclassList(const Environment&, CLIPSValue *, Defmodule *);
bool Undefclass(Defclass *, const Environment&);
bool HasSuperclass(Defclass *, Defclass *);

CLIPSLexeme *CheckClassAndSlot(UDFContext *, const char *, Defclass **);

void SaveDefclasses(const Environment&, Defmodule *, const char *, void * context = nullptr);

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

#include "Entities.h"
#include "Object.h"
#include "ExternalFunctions.h"

#if DEBUGGING_FUNCTIONS

void BrowseClassesCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void BrowseClasses(Defclass *, const char *);
void DescribeClassCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void DescribeClass(Defclass *, const char *);

#endif /* DEBUGGING_FUNCTIONS */

const char *GetCreateAccessorString(SlotDescriptor *);
void GetDefclassModuleCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SuperclassPCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool SuperclassP(Defclass *, Defclass *);
void SubclassPCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool SubclassP(Defclass *, Defclass *);
void SlotExistPCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool SlotExistP(Defclass *, const char *, bool);
void MessageHandlerExistPCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SlotWritablePCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool SlotWritableP(Defclass *, const char *);
void SlotInitablePCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool SlotInitableP(Defclass *, const char *);
void SlotPublicPCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool SlotPublicP(Defclass *, const char *);
void SlotDirectAccessPCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool SlotDirectAccessP(Defclass *, const char *);
void SlotDefaultValueCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool SlotDefaultValue(Defclass *, const char *, CLIPSValue *);
void ClassExistPCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
int SlotDefaultP(const Environment&, Defclass *, const char *);

#endif









