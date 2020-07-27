/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  11/17/17            */
/*                                                     */
/*                MULTIFIELD HEADER FILE               */
/*******************************************************/

/*************************************************************/
/* Purpose: Routines for creating and manipulating           */
/*   multifield values.                                      */
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
/*            Corrected code to remove compiler warnings.    */
/*                                                           */
/*            Moved ImplodeMultifield from multifun.c.       */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Used DataObjectToString instead of             */
/*            ValueToString in implode$ to handle            */
/*            print representation of external addresses.    */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed issue with StoreInMultifield when        */
/*            asserting void values in implied deftemplate   */
/*            facts.                                         */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_multifld

#pragma once

#define _H_multifld

#include "PatternEntity.hxx"
#include "Evaluation.h"
typedef struct multifieldBuilder MultifieldBuilder;

struct multifieldBuilder {
    Environment mbEnv;
    CLIPSValue *contents;
    size_t bufferReset;
    size_t length;
    size_t bufferMaximum;
};

Multifield::Ptr CreateUnmanagedMultifield(const Environment::Ptr&, size_t);
void ReturnMultifield(const Environment::Ptr&, Multifield *);
void RetainMultifield(const Environment::Ptr&, Multifield *);
void ReleaseMultifield(const Environment::Ptr&, Multifield *);
void IncrementCLIPSValueMultifieldReferenceCount(const Environment::Ptr&, Multifield *);
void DecrementCLIPSValueMultifieldReferenceCount(const Environment::Ptr&, Multifield *);
Multifield *StringToMultifield(const Environment::Ptr&, const char *);
Multifield *CreateMultifield(const Environment::Ptr&, size_t);
void AddToMultifieldList(const Environment::Ptr&, Multifield *);
void FlushMultifields(const Environment::Ptr&);
void DuplicateMultifield(const Environment::Ptr&, UDFValue *, UDFValue *);
void WriteMultifield(const Environment::Ptr&, const char *, Multifield *);
void PrintMultifieldDriver(const Environment::Ptr&, const char *, Multifield *, size_t, size_t, bool);
bool MultifieldDOsEqual(UDFValue *, UDFValue *);
void StoreInMultifield(const Environment::Ptr&, UDFValue *, Expression::Ptr, bool);
Multifield *CopyMultifield(const Environment::Ptr&, Multifield *);
bool MultifieldsEqual(Multifield *, Multifield *);
Multifield *DOToMultifield(const Environment::Ptr&, UDFValue *);
size_t HashMultifield(Multifield *, size_t);
Multifield *GetMultifieldList(const Environment::Ptr&);
CLIPSLexeme *ImplodeMultifield(const Environment::Ptr&, UDFValue *);
void EphemerateMultifield(const Environment::Ptr&, Multifield *);
Multifield *ArrayToMultifield(const Environment::Ptr&, CLIPSValue *, unsigned long);
void NormalizeMultifield(const Environment::Ptr&, UDFValue *);
void CLIPSToUDFValue(CLIPSValue *, UDFValue *);
void UDFToCLIPSValue(const Environment::Ptr&, UDFValue *, CLIPSValue *);
MultifieldBuilder *CreateMultifieldBuilder(const Environment::Ptr&, size_t);
void MBReset(MultifieldBuilder *);
void MBDispose(MultifieldBuilder *);
void MBAppend(MultifieldBuilder *theMB, CLIPSValue *);
Multifield *MBCreate(MultifieldBuilder *);
Multifield *EmptyMultifield(const Environment::Ptr&);
void MBAppendCLIPSInteger(MultifieldBuilder *, CLIPSInteger *);
void MBAppendInteger(MultifieldBuilder *, long long);
void MBAppendCLIPSFloat(MultifieldBuilder *, CLIPSFloat *);
void MBAppendFloat(MultifieldBuilder *, double);
void MBAppendCLIPSLexeme(MultifieldBuilder *, CLIPSLexeme *);
void MBAppendSymbol(MultifieldBuilder *, const char *);
void MBAppendString(MultifieldBuilder *, const char *);
void MBAppendInstanceName(MultifieldBuilder *, const char *);
void MBAppendCLIPSExternalAddress(MultifieldBuilder *, CLIPSExternalAddress *);
void MBAppendFact(MultifieldBuilder *, Fact *);
void MBAppendInstance(MultifieldBuilder *, Instance *);
void MBAppendMultifield(MultifieldBuilder *, Multifield *);
void MBAppendUDFValue(MultifieldBuilder *theMB, UDFValue *);

#include "Evaluation.h"

constexpr auto VALUE_NOT_FOUND = SIZE_MAX;

void MultifieldFunctionDefinitions(const Environment::Ptr&);
#if MULTIFIELD_FUNCTIONS
void DeleteFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ReplaceFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void DeleteMemberFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ReplaceMemberFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void InsertFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ExplodeFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ImplodeFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SubseqFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void FirstFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void RestFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void NthFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SubsetpFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void MemberFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void MultifieldPrognFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ForeachFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GetMvPrognField(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GetMvPrognIndex(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
bool FindDOsInSegment(UDFValue *, unsigned int, UDFValue *,
                      size_t *, size_t *, size_t *, unsigned int);
#endif
bool ReplaceMultiValueFieldSizet(const Environment::Ptr&, UDFValue *, UDFValue *,
                                 size_t, size_t, UDFValue *, const char *);
bool InsertMultiValueField(const Environment::Ptr&, UDFValue *, UDFValue *,
                           size_t, UDFValue *, const char *);
void MVRangeError(const Environment::Ptr&, long long, long long, size_t, const char *);
size_t FindValueInMultifield(UDFValue *, UDFValue *);

#endif /* _H_multifld */




