/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  11/13/17            */
/*                                                     */
/*                DEFGLOBAL HEADER FILE                */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
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
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
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

#ifndef _H_globldef

#pragma once

#define _H_globldef

typedef struct defglobal Defglobal;

#include "Construct.h"
#include "DefmoduleBinarySaveLoad.h"
#include "Construct.h"
#include "Defglobal.h"
#include "Construct.h"
#include "Evaluation.h"
#include "Expression.h"
#include "Defmodule.h"
#include "Symbol.h"
#include "Evaluation.h"
#include "Defglobal.h"

constexpr auto DEFGLOBAL_DATA = 1;

struct defglobalData : public EnvironmentModule {
    Construct *DefglobalConstruct;
    unsigned DefglobalModuleIndex;
    bool ChangeToGlobals;
#if DEBUGGING_FUNCTIONS
    bool WatchGlobals;
#endif
    bool ResetGlobals;
    EntityRecord GlobalInfo;
    EntityRecord DefglobalPtrRecord;
    long LastModuleIndex;
    Defmodule *TheDefmodule;
};

struct defglobal {
    ConstructHeader header;
    bool watch: 1;
    bool inScope: 1;
    long busyCount;
    CLIPSValue current;
    Expression *initial;
};

struct defglobalModule {
    struct defmoduleItemHeader header;
};
RegisterEnvironmentModule(defglobalData, DEFGLOBAL_DATA, Defglobal);

void InitializeDefglobals(const Environment&);
Defglobal *FindDefglobal(const Environment&, const char *);
Defglobal *FindDefglobalInModule(const Environment&, const char *);
Defglobal *GetNextDefglobal(const Environment&, Defglobal *);
void CreateInitialFactDefglobal();
bool DefglobalIsDeletable(Defglobal *);
struct defglobalModule *GetDefglobalModuleItem(const Environment&, Defmodule *);
void QSetDefglobalValue(const Environment&, Defglobal *, UDFValue *, bool);
Defglobal *QFindDefglobal(const Environment&, CLIPSLexeme *);
void DefglobalValueForm(Defglobal *, StringBuilder *);
bool GetGlobalsChanged(const Environment&);
void SetGlobalsChanged(const Environment&, bool);
void DefglobalGetValue(Defglobal *, CLIPSValue *);
void DefglobalSetValue(Defglobal *, CLIPSValue *);
void DefglobalSetInteger(Defglobal *, long long);
void DefglobalSetFloat(Defglobal *, double);
void DefglobalSetSymbol(Defglobal *, const char *);
void DefglobalSetString(Defglobal *, const char *);
void DefglobalSetInstanceName(Defglobal *, const char *);
void DefglobalSetCLIPSInteger(Defglobal *, CLIPSInteger *);
void DefglobalSetCLIPSFloat(Defglobal *, CLIPSFloat *);
void DefglobalSetCLIPSLexeme(Defglobal *, CLIPSLexeme *);
void DefglobalSetFact(Defglobal *, Fact *);
void DefglobalSetInstance(Defglobal *, Instance *);
void DefglobalSetMultifield(Defglobal *, Multifield *);
void DefglobalSetCLIPSExternalAddress(Defglobal *, CLIPSExternalAddress *);
void UpdateDefglobalScope(const Environment&);
Defglobal *GetNextDefglobalInScope(const Environment&, Defglobal *);
bool QGetDefglobalUDFValue(const Environment&, Defglobal *, UDFValue *);
const char *DefglobalModule(Defglobal *);
const char *DefglobalName(Defglobal *);
const char *DefglobalPPForm(Defglobal *);


void DefglobalBasicCommands(const Environment&);
void UndefglobalCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool Undefglobal(Defglobal *, const Environment&);
void GetDefglobalListFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void GetDefglobalList(const Environment&, CLIPSValue *, Defmodule *);
void DefglobalModuleFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void PPDefglobalCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool PPDefglobal(const Environment&, const char *, const char *);
void ListDefglobalsCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
#if DEBUGGING_FUNCTIONS
bool DefglobalGetWatch(Defglobal *);
void ListDefglobals(const Environment&, const char *, Defmodule *);
void DefglobalSetWatch(Defglobal *, bool);
#endif
void ResetDefglobals(const Environment&, void *);

#if BBLOAD_AND_BSAVE
struct bsaveDefglobal {
    struct bsaveConstructHeader header;
    unsigned long initial;
};

struct bsaveDefglobalModule {
    struct bsaveDefmoduleItemHeader header;
};

constexpr auto GLOBLBIN_DATA = 60;

struct defglobalBinaryData : public EnvironmentModule {
    Defglobal *DefglobalArray;
    unsigned long NumberOfDefglobals;
    struct defglobalModule *ModuleArray;
    unsigned long NumberOfDefglobalModules;
};
RegisterEnvironmentModule(defglobalBinaryData, GLOBLBIN_DATA, DefglobalBinary);

#define DefglobalPointer(i) (&DefglobalBinaryData(theEnv)->DefglobalArray[i])

void DefglobalBinarySetup(const Environment&);
void *BloadDefglobalModuleReference(const Environment&, unsigned long);
#endif
void DefglobalCommandDefinitions(const Environment&);
void SetResetGlobalsCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool SetResetGlobals(const Environment&, bool);
void GetResetGlobalsCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool GetResetGlobals(const Environment&);
void ShowDefglobalsCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ShowDefglobals(const Environment&, const char *, Defmodule *);
#include "Expression.h"

bool ParseDefglobal(const Environment&, const char *);
bool ReplaceGlobalVariable(const Environment&, Expression *);
void GlobalReferenceErrorMessage(const Environment&, const char *);
#endif /* _H_globldef */


