/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  08/25/16            */
/*                                                     */
/*     CLASS INFO PROGRAMMATIC ACCESS HEADER FILE      */
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
/*                                                            */
/*      6.24: Added allowed-classes slot facet.              */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Borland C (IBM_TBC) and Metrowerks CodeWarrior */
/*            (MAC_MCW, IBM_MCW) are no longer supported.    */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
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

#ifndef _H_classinf

#pragma once

#define _H_classinf

#include "Evaluation.h"

void ClassAbstractPCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ClassReactivePCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
Defclass *ClassInfoFnxArgs(UDFContext *, const char *, bool *);
void ClassSlotsCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ClassSuperclassesCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ClassSubclassesCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void GetDefmessageHandlersListCmd(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SlotFacetsCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SlotSourcesCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SlotTypesCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SlotAllowedValuesCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SlotAllowedClassesCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SlotRangeCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SlotCardinalityCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool ClassAbstractP(Defclass *);
bool ClassReactiveP(Defclass *);
void ClassSlots(Defclass *, CLIPSValue *, bool);
void GetDefmessageHandlerList(const Environment&, Defclass *, CLIPSValue *, bool);
void ClassSuperclasses(Defclass *, CLIPSValue *, bool);
void ClassSubclasses(Defclass *, CLIPSValue *, bool);
void ClassSubclassAddresses(const Environment&, Defclass *, UDFValue *, bool);
bool SlotFacets(Defclass *, const char *, CLIPSValue *);
bool SlotSources(Defclass *, const char *, CLIPSValue *);
bool SlotTypes(Defclass *, const char *, CLIPSValue *);
bool SlotAllowedValues(Defclass *, const char *, CLIPSValue *);
bool SlotAllowedClasses(Defclass *, const char *, CLIPSValue *);
bool SlotRange(Defclass *, const char *, CLIPSValue *);
bool SlotCardinality(Defclass *, const char *, CLIPSValue *);

#endif /* _H_classinf */





