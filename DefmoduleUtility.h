/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  11/01/16            */
/*                                                     */
/*            DEFMODULE UTILITY HEADER FILE            */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for parsing module/construct   */
/*   names and searching through modules for specific        */
/*   constructs.                                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Used genstrncpy instead of strncpy.            */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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
/*************************************************************/

#ifndef _H_modulutl

#pragma once

#define _H_modulutl

typedef void *GetNextItemFunction(const Environment&, void *);
typedef void PrintItemFunction(const Environment&, const char *, void *);

#include "Defmodule.h"
#include "Symbol.h"
#include "Scanner.h"

unsigned FindModuleSeparator(const char *);
CLIPSLexeme *ExtractModuleName(const Environment&, unsigned, const char *);
CLIPSLexeme *ExtractConstructName(const Environment&, unsigned, const char *, unsigned);
const char *ExtractModuleAndConstructName(const Environment&, const char *);
ConstructHeader *FindImportedConstruct(const Environment&, const char *, Defmodule *,
                                       const char *, unsigned int *, bool, Defmodule *);
void AmbiguousReferenceErrorMessage(const Environment&, const char *, const char *);
void MarkModulesAsUnvisited(const Environment&);
bool AllImportedModulesVisited(const Environment&, Defmodule *);
void ListItemsDriver(const Environment&,
                     const char *, Defmodule *,
                     const char *, const char *,
                     GetNextItemFunction *,
                     const char *(*)(void *),
                     PrintItemFunction *,
                     bool (*)(void *));
long DoForAllModules(const Environment&,
                     void (*)(Defmodule *, void *),
                     int, void *);
bool ConstructExported(const Environment&, const char *, CLIPSLexeme *, CLIPSLexeme *);

void RemoveConstructFromModule(const Environment&, ConstructHeader *);
CLIPSLexeme *GetConstructNameAndComment(const Environment&, const char *,
                                        struct token *, const char *,
                                        FindConstructFunction *,
                                        DeleteConstructFunction *,
                                        const char *, bool, bool, bool, bool);

#endif /* _H_modulutl */



