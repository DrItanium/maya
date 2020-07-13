/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  09/09/19            */
/*                                                     */
/*          EXTENDED MATH FUNCTIONS HEADER FILE        */
/*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for numerous extended math     */
/*   functions including cos, sin, tan, sec, csc, cot, acos, */
/*   asin, atan, atan2, asec, acsc, acot, cosh, sinh, tanh,  */
/*   sech, csch, coth, acosh, asinh, atanh, asech, acsch,    */
/*   acoth, mod, exp, log, log10, sqrt, pi, deg-rad,         */
/*   rad-deg,  deg-grad, grad-deg, **, and round.            */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Renamed EX_MATH compiler flag to               */
/*            EXTENDED_MATH_FUNCTIONS.                       */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Added atan2 function.                          */
/*                                                           */
/*************************************************************/

#ifndef _H_emathfun

#pragma once

#define _H_emathfun

void ExtendedMathFunctionDefinitions(const Environment::Ptr&);
#if EXTENDED_MATH_FUNCTIONS
void CosFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SinFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void TanFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SecFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void CscFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void CotFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AcosFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AsinFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AtanFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void Atan2Function(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AsecFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AcscFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AcotFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void CoshFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SinhFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void TanhFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SechFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void CschFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void CothFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AcoshFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AsinhFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AtanhFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AsechFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AcschFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void AcothFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void RoundFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ModFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ExpFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void LogFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void Log10Function(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SqrtFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void PiFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void DegRadFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void RadDegFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void DegGradFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GradDegFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void PowFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
#endif

#endif /* _H_emathfun */



