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

#include "ClassCommands.h"

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
    CLIPSLexeme *ISA_SYMBOL;
    CLIPSLexeme *NAME_SYMBOL;
#if DEBUGGING_FUNCTIONS
    bool WatchInstances;
    bool WatchSlots;
#endif
    unsigned short CTID;
    struct token ObjectParseToken;
    ClassDefaultsMode ClassDefaultsModeValue;
    int newSlotID;
};
RegisterEnvironmentModule(defclassData, DEFCLASS_DATA);

#define DefclassData(theEnv) (GetEnvironmentData(theEnv,DEFCLASS_DATA))

#endif









