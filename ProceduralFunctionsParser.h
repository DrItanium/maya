/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  10/18/16            */
/*                                                     */
/*       PROCEDURAL FUNCTIONS PARSER HEADER FILE       */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Local variables set with the bind function     */
/*            persist until a reset/clear command is issued. */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
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
/*            Eval support for run time and bload only.      */
/*                                                           */
/*************************************************************/

#ifndef _H_prcdrpsr

#pragma once

#define _H_prcdrpsr

#include "Constraint.h"

struct BindInfo {
    CLIPSLexeme *name;
    CONSTRAINT_RECORD *constraints;
    struct BindInfo *next;
};

void ProceduralFunctionParsers(const Environment::Ptr&);
struct BindInfo *GetParsedBindNames(const Environment::Ptr&);
void SetParsedBindNames(const Environment::Ptr&, struct BindInfo *);
void ClearParsedBindNames(const Environment::Ptr&);
bool ParsedBindNamesEmpty(const Environment::Ptr&);
unsigned short SearchParsedBindNames(const Environment::Ptr&, CLIPSLexeme *);
unsigned short CountParsedBindNames(const Environment::Ptr&);
void RemoveParsedBindName(const Environment::Ptr&, CLIPSLexeme *);
struct constraintRecord *FindBindConstraints(const Environment::Ptr&, CLIPSLexeme *);

#endif /* _H_prcdrpsr */




