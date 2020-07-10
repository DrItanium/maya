/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  10/01/16            */
/*                                                     */
/*             DEFMODULE PARSER HEADER FILE            */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: GetConstructNameAndComment API change.         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed linkage issue when DEFMODULE_CONSTRUCT   */
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
/*************************************************************/

#ifndef _H_modulpsr

#pragma once

#define _H_modulpsr

struct portConstructItem;

#include "Evaluation.h"
#include "Defmodule.h"
#include "Symbol.h"
#include "Scanner.h"

struct portConstructItem {
    const char *constructName;
    TokenType typeExpected;
    struct portConstructItem *next;
};

void SetNumberOfDefmodules(const Environment&, unsigned short);
void AddAfterModuleDefinedFunction(const Environment&, const char *, VoidCallFunction *, int, void *);
bool ParseDefmodule(const Environment&, const char *);
void AddPortConstructItem(const Environment&, const char *, TokenType);
struct portConstructItem *ValidPortConstructItem(const Environment&, const char *);
bool FindImportExportConflict(const Environment&, const char *, Defmodule *, const char *);

#endif /* _H_modulpsr */


