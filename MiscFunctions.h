/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/17/18            */
/*                                                     */
/*          MISCELLANEOUS FUNCTIONS HEADER FILE        */
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
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*            Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Removed CONFLICT_RESOLUTION_STRATEGIES,        */
/*            DYNAMIC_SALIENCE, INCREMENTAL_RESET,           */
/*            LOGICAL_DEPENDENCIES, IMPERATIVE_METHODS       */
/*            INSTANCE_PATTERN_MATCHING,                     */
/*            IMPERATIVE_MESSAGE_HANDLERS, and               */
/*            AUXILIARY_MESSAGE_HANDLERS compilation flags.  */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Used gensprintf instead of sprintf.            */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems.                   */
/*                                                           */
/*            Renamed EX_MATH compiler flag to               */
/*            EXTENDED_MATH_FUNCTIONS.                       */
/*                                                           */
/*            Combined BASIC_IO and EXT_IO compilation       */
/*            flags into the IO_FUNCTIONS compilation flag.  */
/*                                                           */
/*            Removed code associated with HELP_FUNCTIONS    */
/*            and EMACS_EDITOR compiler flags.               */
/*                                                           */
/*            Added operating-system function.               */
/*                                                           */
/*            Added new function (for future use).           */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.31: Added local-time and gm-time functions.        */
/*                                                           */
/*      6.40: Refactored code to reduce header dependencies  */
/*            in sysdep.c.                                   */
/*                                                           */
/*            Removed LOCALE definition.                     */
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
/*            Added get-error, set-error, and clear-error    */
/*            functions.                                     */
/*                                                           */
/*************************************************************/

#ifndef _H_miscfun

#pragma once

#define _H_miscfun

void MiscFunctionDefinitions(const Environment::Ptr&);
void ExitCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void CreateFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SetgenFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GensymFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GensymStarFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void RandomFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SeedFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void LengthFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void OptionsCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void OperatingSystemFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ExpandFuncCall(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void DummyExpandFuncMultifield(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void CauseEvaluationError(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SetSORCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GetSORCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GetFunctionRestrictions(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AproposCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GensymStar(const Environment::Ptr&, UDFValue *);
void GetFunctionListFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void FuncallFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void NewFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void CallFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void TimerFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void TimeFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SystemCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void LocalTimeFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GMTimeFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GetErrorFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ClearErrorValue(const Environment::Ptr&);
void ClearErrorFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SetErrorFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SetErrorValue(const Environment::Ptr&, TypeHeader *);
void VoidFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);

#endif /* _H_miscfun */






