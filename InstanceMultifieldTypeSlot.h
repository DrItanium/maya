/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  10/18/16            */
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
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Changed integer type/precision.                */
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
/*            Eval support for run time and bload only.      */
/*                                                           */
/*************************************************************/

#ifndef _H_insmult

#pragma once

#define _H_insmult

#include "Evaluation.h"

void SetupInstanceMultifieldCommands(const Environment&);
void MVSlotReplaceCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void MVSlotInsertCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void MVSlotDeleteCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void DirectMVReplaceCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void DirectMVInsertCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void DirectMVDeleteCommand(const Environment&theEnv, UDFContext *context, UDFValue *ret);

#endif /* _H_insmult */



