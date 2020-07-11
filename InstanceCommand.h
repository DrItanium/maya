/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  11/14/17            */
/*                                                     */
/*            INSTANCE COMMAND HEADER MODULE           */
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
/*            Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*      6.24: Loading a binary instance file from a run-time */
/*            program caused a bus error. DR0866             */
/*                                                           */
/*            Removed LOGICAL_DEPENDENCIES compilation flag. */
/*                                                           */
/*            Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
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

#ifndef _H_inscom

#pragma once

#define _H_inscom

enum UnmakeInstanceError{
    UIE_NO_ERROR = 0,
    UIE_nullptr_POINTER_ERROR,
    UIE_COULD_NOT_DELETE_ERROR,
    UIE_DELETED_ERROR,
    UIE_RULE_NETWORK_ERROR
} ;

enum MakeInstanceError {
    MIE_NO_ERROR = 0,
    MIE_nullptr_POINTER_ERROR,
    MIE_PARSING_ERROR,
    MIE_COULD_NOT_CREATE_ERROR,
    MIE_RULE_NETWORK_ERROR
};

enum InstanceBuilderError {
    IBE_NO_ERROR = 0,
    IBE_nullptr_POINTER_ERROR,
    IBE_DEFCLASS_NOT_FOUND_ERROR,
    IBE_COULD_NOT_CREATE_ERROR,
    IBE_RULE_NETWORK_ERROR
};

enum InstanceModifierError {
    IME_NO_ERROR = 0,
    IME_nullptr_POINTER_ERROR,
    IME_DELETED_ERROR,
    IME_COULD_NOT_MODIFY_ERROR,
    IME_RULE_NETWORK_ERROR
};

#include "InstanceFunctions.h"
#include "Object.h"

constexpr auto INSTANCE_DATA = 29;

struct instanceData : public EnvironmentModule {
    Instance DummyInstance;
    Instance **InstanceTable;
    bool MaintainGarbageInstances;
    bool MkInsMsgPass;
    bool ChangesToInstances;
    IGARBAGE *InstanceGarbageList;
    PatternEntityRecord InstanceInfo;
    Instance *InstanceList;
    unsigned long GlobalNumberOfInstances;
    Instance *CurrentInstance;
    Instance *InstanceListBottom;
    bool ObjectModDupMsgValid;
    UnmakeInstanceError unmakeInstanceError;
    MakeInstanceError makeInstanceError;
    InstanceModifierError instanceModifierError;
    InstanceBuilderError instanceBuilderError;
};
RegisterEnvironmentModule(instanceData, INSTANCE_DATA, Instance);

void SetupInstances(const Environment&);
UnmakeInstanceError DeleteInstance(Instance *);
UnmakeInstanceError DeleteAllInstances(const Environment&);
UnmakeInstanceError UnmakeInstance(Instance *);
bool UnmakeInstanceCallback(Instance *, const Environment&);
UnmakeInstanceError UnmakeAllInstances(const Environment&);
#if DEBUGGING_FUNCTIONS
void InstancesCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void PPInstanceCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void Instances(const Environment&, const char *, Defmodule *, const char *, bool);
#endif
Instance *MakeInstance(const Environment&, const char *);
MakeInstanceError GetMakeInstanceError(const Environment&);
Instance *CreateRawInstance(const Environment&, Defclass *, const char *);
Instance *FindInstance(const Environment&, Defmodule *, const char *, bool);
bool ValidInstanceAddress(Instance *);
GetSlotError DirectGetSlot(Instance *, const char *, CLIPSValue *);
PutSlotError DirectPutSlot(Instance *, const char *, CLIPSValue *);
PutSlotError DirectPutSlotInteger(Instance *, const char *, long long);
PutSlotError DirectPutSlotFloat(Instance *, const char *, double);
PutSlotError DirectPutSlotSymbol(Instance *, const char *, const char *);
PutSlotError DirectPutSlotString(Instance *, const char *, const char *);
PutSlotError DirectPutSlotInstanceName(Instance *, const char *, const char *);
PutSlotError DirectPutSlotCLIPSInteger(Instance *, const char *, CLIPSInteger *);
PutSlotError DirectPutSlotCLIPSFloat(Instance *, const char *, CLIPSFloat *);
PutSlotError DirectPutSlotCLIPSLexeme(Instance *, const char *, CLIPSLexeme *);
PutSlotError DirectPutSlotFact(Instance *, const char *, Fact *);
PutSlotError DirectPutSlotInstance(Instance *, const char *, Instance *);
PutSlotError DirectPutSlotMultifield(Instance *, const char *, Multifield *);
PutSlotError DirectPutSlotCLIPSExternalAddress(Instance *, const char *, CLIPSExternalAddress *);
const char *InstanceName(Instance *);
Defclass *InstanceClass(Instance *);
unsigned long GetGlobalNumberOfInstances(const Environment&);
Instance *GetNextInstance(const Environment&, Instance *);
Instance *GetNextInstanceInScope(const Environment&, Instance *);
Instance *GetNextInstanceInClass(Defclass *, Instance *);
Instance *GetNextInstanceInClassAndSubclasses(Defclass **, Instance *, UDFValue *);
void InstancePPForm(Instance *, StringBuilder *);
void ClassCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void DeleteInstanceCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void UnmakeInstanceCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SymbolToInstanceNameFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void InstanceNameToSymbolFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void InstanceAddressCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void InstanceNameCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void InstanceAddressPCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void InstanceNamePCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void InstancePCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void InstanceExistPCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void CreateInstanceHandler(const Environment&theEnv, UDFContext *context, UDFValue *ret);

#endif /* _H_inscom */
