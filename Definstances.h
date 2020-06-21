/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  08/25/16            */
/*                                                     */
/*               DEFINSTANCES HEADER FILE              */
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
/*      6.23: Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            GetConstructNameAndComment API change.         */
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

#ifndef _H_defins

#pragma once

#define _H_defins

#if DEFINSTANCES_CONSTRUCT

struct definstances;

#include "Construct.h"
#include "Construct.h"
#include "Defmodule.h"
#include "Object.h"

typedef struct definstancesModule {
    struct defmoduleItemHeader header;
} DEFINSTANCES_MODULE;

typedef struct definstances {
    ConstructHeader header;
    unsigned busy;
    Expression *mkinstance;
} Definstances;

#define DEFINSTANCES_DATA 22

struct definstancesData {
    Construct *DefinstancesConstruct;
    unsigned DefinstancesModuleIndex;
};

#define DefinstancesData(theEnv) ((struct definstancesData *) GetEnvironmentData(theEnv,DEFINSTANCES_DATA))

const char *DefinstancesModule(Definstances *);
const char *DefinstancesModuleName(Environment *, Definstances *);
Definstances *FindDefinstances(Environment *, const char *);
Definstances *FindDefinstancesInModule(Environment *, const char *);
void GetDefinstancesList(Environment *, CLIPSValue *, Defmodule *);
const char *DefinstancesName(Definstances *);
CLIPSLexeme *GetDefinstancesNamePointer(Environment *, Definstances *);
const char *DefinstancesPPForm(Definstances *);
Definstances *GetNextDefinstances(Environment *, Definstances *);
bool DefinstancesIsDeletable(Definstances *);
void SetDefinstancesPPForm(Environment *, Definstances *, const char *);
bool Undefinstances(Definstances *, Environment *);
void GetDefinstancesListFunction(Environment *, UDFContext *, UDFValue *);
void GetDefinstancesModuleCommand(Environment *, UDFContext *, UDFValue *);
void SetupDefinstances(Environment *);
void UndefinstancesCommand(Environment *, UDFContext *, UDFValue *);
#if DEBUGGING_FUNCTIONS
void PPDefinstancesCommand(Environment *, UDFContext *, UDFValue *);
void ListDefinstancesCommand(Environment *, UDFContext *, UDFValue *);
void ListDefinstances(Environment *, const char *, Defmodule *);
#endif

#endif /* DEFINSTANCES_CONSTRUCT */

#endif /* _H_defins */




