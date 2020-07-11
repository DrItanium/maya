/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  08/06/16            */
/*                                                     */
/*                DEFFACTS HEADER FILE                 */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
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
/*************************************************************/

#ifndef _H_dffctdef

#pragma once

#define _H_dffctdef

typedef struct deffacts Deffacts;

#include "Construct.h"
#include "Construct.h"
#include "Evaluation.h"
#include "Expression.h"
#include "Defmodule.h"
#include "Symbol.h"

constexpr auto DEFFACTS_DATA = 0;

struct deffactsData : public EnvironmentModule {
    Construct *DeffactsConstruct;
    unsigned DeffactsModuleIndex;
};

struct deffacts {
    ConstructHeader header;
    Expression *assertList;
};

struct deffactsModule {
    struct defmoduleItemHeader header;
};
RegisterEnvironmentModule(deffactsData, DEFFACTS_DATA, Deffacts);

void InitializeDeffacts(const Environment&);
Deffacts *FindDeffacts(const Environment&, const char *);
Deffacts *FindDeffactsInModule(const Environment&, const char *);
Deffacts *GetNextDeffacts(const Environment&, Deffacts *);
void CreateInitialFactDeffacts();
bool DeffactsIsDeletable(Deffacts *);
struct deffactsModule *GetDeffactsModuleItem(const Environment&, Defmodule *);
const char *DeffactsModule(Deffacts *);
const char *DeffactsName(Deffacts *);
const char *DeffactsPPForm(Deffacts *);

#endif /* _H_dffctdef */


