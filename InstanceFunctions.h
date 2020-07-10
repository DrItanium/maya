/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  11/18/17            */
/*                                                     */
/*               INSTANCE FUNCTIONS MODULE             */
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
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*            Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Link error occurs for the SlotExistError       */
/*            function when OBJECT_SYSTEM is set to 0 in     */
/*            setup.h. DR0865                                */
/*                                                           */
/*            Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Moved EvaluateAndStoreInDataObject to          */
/*            evaluatn.c                                     */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed slot override default ?NONE bug.         */
/*                                                           */
/*      6.31: Fix for compilation with -std=c89.             */
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
/*************************************************************/

#ifndef _H_insfun

#pragma once

#define _H_insfun

#include "Entities.h"
#include "Object.h"

typedef struct igarbage {
    Instance *ins;
    struct igarbage *nxt;
} IGARBAGE;

constexpr auto INSTANCE_TABLE_HASH_SIZE = 8191;
#define InstanceSizeHeuristic(ins)      sizeof(Instance)

void RetainInstance(Instance *);
void ReleaseInstance(Instance *);
void IncrementInstanceCallback(const Environment&, Instance *);
void DecrementInstanceCallback(const Environment&, Instance *);
void InitializeInstanceTable(const Environment&);
void CleanupInstances(const Environment&, void *);
unsigned HashInstance(CLIPSLexeme *);
void DestroyAllInstances(const Environment&, void *);
void RemoveInstanceData(const Environment&, Instance *);
Instance *FindInstanceBySymbol(const Environment&, CLIPSLexeme *);
Instance *FindInstanceInModule(const Environment&, CLIPSLexeme *, Defmodule *,
                               Defmodule *, bool);
InstanceSlot *FindInstanceSlot(const Environment&, Instance *, CLIPSLexeme *);
int FindInstanceTemplateSlot(const Environment&, Defclass *, CLIPSLexeme *);
PutSlotError PutSlotValue(const Environment&, Instance *, InstanceSlot *, UDFValue *, UDFValue *, const char *);
PutSlotError DirectPutSlotValue(const Environment&, Instance *, InstanceSlot *, UDFValue *, UDFValue *);
PutSlotError ValidSlotValue(const Environment&, UDFValue *, SlotDescriptor *, Instance *, const char *);
Instance *CheckInstance(UDFContext *);
void NoInstanceError(const Environment&, const char *, const char *);
void StaleInstanceAddress(const Environment&, const char *, int);
bool GetInstancesChanged(const Environment&);
void SetInstancesChanged(const Environment&, bool);
void PrintSlot(const Environment&, const char *, SlotDescriptor *, Instance *, const char *);
void PrintInstanceNameAndClass(const Environment&, const char *, Instance *, bool);
void PrintInstanceName(const Environment&, const char *, Instance *);
void PrintInstanceLongForm(const Environment&, const char *, Instance *);
void DecrementObjectBasisCount(const Environment&, Instance *);
void IncrementObjectBasisCount(const Environment&, Instance *);
void MatchObjectFunction(const Environment&, Instance *);
bool NetworkSynchronized(const Environment&, Instance *);
bool InstanceIsDeleted(const Environment&, Instance *);

#endif /* _H_insfun */







