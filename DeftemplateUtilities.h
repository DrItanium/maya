/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/30/16            */
/*                                                     */
/*          DEFTEMPLATE UTILITIES HEADER FILE          */
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
/*      6.23: Added support for templates maintaining their  */
/*            own list of facts.                             */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added additional arguments to                  */
/*            InvalidDeftemplateSlotMessage function.        */
/*                                                           */
/*            Added additional arguments to                  */
/*            PrintTemplateFact function.                    */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Used gensprintf instead of sprintf.            */
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
/*            Watch facts for modify command only prints     */
/*            changed slots.                                 */
/*                                                           */
/*************************************************************/

#ifndef _H_tmpltutl

#pragma once

#define _H_tmpltutl

#include "Constraint.h"
#include "Evaluation.h"
#include "Expression.h"
#include "Fact.h"
#include "Symbol.h"

void InvalidDeftemplateSlotMessage(const Environment&, const char *, const char *, bool);
void SingleFieldSlotCardinalityError(const Environment&, const char *);
void MultiIntoSingleFieldSlotError(const Environment&, struct templateSlot *, Deftemplate *);
void CheckTemplateFact(const Environment&, Fact *);
bool CheckRHSSlotTypes(const Environment&, Expression *, struct templateSlot *, const char *);
struct templateSlot *GetNthSlot(Deftemplate *, long long);
int FindSlotPosition(Deftemplate *, CLIPSLexeme *);
void PrintTemplateFact(const Environment&, const char *, Fact *, bool, bool, const char *);
void UpdateDeftemplateScope(const Environment&);
struct templateSlot *FindSlot(Deftemplate *, CLIPSLexeme *, unsigned short *);
Deftemplate *CreateImpliedDeftemplate(const Environment&, CLIPSLexeme *, bool);

#endif /* _H_tmpltutl */



