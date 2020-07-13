/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/08/18            */
/*                                                     */
/*            PREDICATE FUNCTIONS HEADER FILE          */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
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
/*            Deprecated the pointerp function and added     */
/*            the external-addressp function.                */
/*                                                           */
/*************************************************************/

#ifndef _H_prdctfun

#pragma once

#define _H_prdctfun

void PredicateFunctionDefinitions(const Environment::Ptr&);
void EqFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void NeqFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void StringpFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SymbolpFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void LexemepFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void NumberpFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void FloatpFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void IntegerpFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void MultifieldpFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ExternalAddresspFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void NotFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AndFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void OrFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void LessThanOrEqualFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GreaterThanOrEqualFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void LessThanFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GreaterThanFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void NumericEqualFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void NumericNotEqualFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void OddpFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void EvenpFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);

#endif /* _H_prdctfun */



