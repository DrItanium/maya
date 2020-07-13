/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  02/20/20            */
/*                                                     */
/*                                                     */
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
/*      6.23: Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Removed IMPERATIVE_METHODS compilation flag.   */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed linkage issue when DEBUGGING_FUNCTIONS   */
/*            is set to 0 and PROFILING_FUNCTIONS is set to  */
/*            1.                                             */
/*                                                           */
/*            Fixed typing issue when OBJECT_SYSTEM          */
/*            compiler flag is set to 0.                     */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_genrcfun

#pragma once

#define _H_genrcfun

typedef struct defgenericModule DEFGENERIC_MODULE;
typedef struct restriction RESTRICTION;
typedef struct defmethod Defmethod;
typedef struct defgeneric Defgeneric;

#include <cstdio>

#include "Entities.hxx"
#include "Construct.h"
#include "Expression.h"
#include "Evaluation.h"
#include "Defmodule.h"
#include "Symbol.h"

constexpr auto METHOD_NOT_FOUND = USHRT_MAX;
constexpr auto RESTRICTIONS_UNBOUNDED = USHRT_MAX;

struct defgenericModule {
    struct defmoduleItemHeader header;
};

struct restriction {
    void **types;
    Expression *query;
    unsigned short tcnt;
};

struct defmethod {
    ConstructHeader header;
    unsigned short index;
    unsigned busy;
    unsigned short restrictionCount;
    unsigned short minRestrictions;
    unsigned short maxRestrictions;
    unsigned short localVarCount;
    bool system: 1;
    bool trace: 1;
    RESTRICTION *restrictions;
    Expression *actions;
};

struct defgeneric {
    ConstructHeader header;
    unsigned busy; // TBD bool?
    bool trace;
    Defmethod *methods;
    unsigned short mcnt;
    unsigned short new_index;
};

constexpr auto DEFGENERIC_DATA = 27;

struct defgenericData : public EnvironmentModule {
    Construct *DefgenericConstruct;
    unsigned int DefgenericModuleIndex;
    EntityRecord GenericEntityRecord;
#if DEBUGGING_FUNCTIONS
    bool WatchGenerics;
    bool WatchMethods;
#endif
    Defgeneric *CurrentGeneric;
    Defmethod *CurrentMethod;
    UDFValue *GenericCurrentArgument;
    unsigned OldGenericBusySave;
};
RegisterEnvironmentModule(defgenericData, DEFGENERIC_DATA,Defgeneric);
#define SaveBusyCount(gfunc)    (DefgenericData(theEnv)->OldGenericBusySave = gfunc->busy)
#define RestoreBusyCount(gfunc) (gfunc->busy = DefgenericData(theEnv)->OldGenericBusySave)

bool ClearDefgenericsReady(const Environment::Ptr&, void *);
void *AllocateDefgenericModule(const Environment::Ptr&);
void FreeDefgenericModule(const Environment::Ptr&, void *);

bool ClearDefmethods(const Environment::Ptr&);
bool RemoveAllExplicitMethods(const Environment::Ptr&, Defgeneric *);
void RemoveDefgeneric(const Environment::Ptr&, Defgeneric *);
bool ClearDefgenerics(const Environment::Ptr&);
void MethodAlterError(const Environment::Ptr&, Defgeneric *);
void DeleteMethodInfo(const Environment::Ptr&, Defgeneric *, Defmethod *);
void DestroyMethodInfo(const Environment::Ptr&, Defgeneric *, Defmethod *);
bool MethodsExecuting(Defgeneric *);

unsigned short FindMethodByIndex(Defgeneric *, unsigned short);
#if DEBUGGING_FUNCTIONS || PROFILING_FUNCTIONS
void PrintMethod(const Environment::Ptr&, Defmethod *, StringBuilder *);
#endif
#if DEBUGGING_FUNCTIONS
void PreviewGeneric(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
#endif
Defgeneric *CheckGenericExists(const Environment::Ptr&, const char *, const char *);
unsigned short CheckMethodExists(const Environment::Ptr&, const char *, Defgeneric *, unsigned short);

void PrintGenericName(const Environment::Ptr&, const char *, Defgeneric *);

#endif /* _H_genrcfun */

