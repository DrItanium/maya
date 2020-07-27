/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  11/01/16            */
/*                                                     */
/*              DEFFUNCTION HEADER FILE                */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*      Gary D. Riley                                        */
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
/*            Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warning.                              */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added missing initializer for ENTITY_RECORD.   */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Changed find construct functionality so that   */
/*            imported modules are search when locating a    */
/*            named construct.                               */
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

#ifndef _H_dffnxfun

#pragma once

#define _H_dffnxfun

typedef struct deffunction Deffunction;
typedef struct deffunctionModuleData DeffunctionModuleData;

#include "PatternEntity.hxx"
#include "Defmodule.h"
#include "Construct.h"
#include "Evaluation.h"

struct deffunctionModuleData {
    struct defmoduleItemHeader header;
};

struct deffunction {
    ConstructHeader header;
    unsigned busy;
    unsigned executing;
    bool trace;
    Expression *code;
    unsigned short minNumberOfParameters;
    unsigned short maxNumberOfParameters;
    unsigned short numberOfLocalVars;
};

constexpr auto DEFFUNCTION_DATA = 23;

struct deffunctionData : public EnvironmentModule {
    Construct *DeffunctionConstruct;
    unsigned DeffunctionModuleIndex;
    EntityRecord DeffunctionEntityRecord;
#if DEBUGGING_FUNCTIONS
    bool WatchDeffunctions;
#endif
    struct CodeGeneratorItem *DeffunctionCodeItem;
    Deffunction *ExecutingDeffunction;
};
RegisterEnvironmentModule(deffunctionData, DEFFUNCTION_DATA, Deffunction);

bool CheckDeffunctionCall(const Environment::Ptr&, Deffunction *, int);
void DeffunctionGetBind(UDFValue *);
void DFRtnUnknown(UDFValue *);
void DFWildargs(UDFValue *);
const char *DeffunctionModule(Deffunction *);
Deffunction *FindDeffunction(const Environment::Ptr&, const char *);
Deffunction *FindDeffunctionInModule(const Environment::Ptr&, const char *);
void GetDeffunctionList(const Environment::Ptr&, CLIPSValue *, Defmodule *);
const char *DeffunctionName(Deffunction *);
CLIPSLexeme *GetDeffunctionNamePointer(const Environment::Ptr&, Deffunction *);
const char *DeffunctionPPForm(Deffunction *);
Deffunction *GetNextDeffunction(const Environment::Ptr&, Deffunction *);
bool DeffunctionIsDeletable(Deffunction *);
void SetDeffunctionPPForm(const Environment::Ptr&, Deffunction *, const char *);
bool Undeffunction(Deffunction *, const Environment::Ptr&);
void GetDeffunctionListFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GetDeffunctionModuleCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
Deffunction *LookupDeffunctionByMdlOrScope(const Environment::Ptr&, const char *);
Deffunction *LookupDeffunctionInScope(const Environment::Ptr&, const char *);
void RemoveDeffunction(const Environment::Ptr&, Deffunction *);
void SetupDeffunctions(const Environment::Ptr&);
void UndeffunctionCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
#if DEBUGGING_FUNCTIONS
bool DeffunctionGetWatch(Deffunction *);
void ListDeffunctions(const Environment::Ptr&, const char *, Defmodule *);
void DeffunctionSetWatch(Deffunction *, bool);
void ListDeffunctionsCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void PPDeffunctionCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
#endif

#endif /* _H_dffnxfun */






