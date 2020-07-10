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

#include "Entities.h"
#include "Evaluation.h"
typedef struct multifieldBuilder MultifieldBuilder;

struct multifieldBuilder {
    Environment mbEnv;
    CLIPSValue *contents;
    size_t bufferReset;
    size_t length;
    size_t bufferMaximum;
};

Multifield *CreateUnmanagedMultifield(const Environment&, size_t);
void ReturnMultifield(const Environment&, Multifield *);
void RetainMultifield(const Environment&, Multifield *);
void ReleaseMultifield(const Environment&, Multifield *);
void IncrementCLIPSValueMultifieldReferenceCount(const Environment&, Multifield *);
void DecrementCLIPSValueMultifieldReferenceCount(const Environment&, Multifield *);
Multifield *StringToMultifield(const Environment&, const char *);
Multifield *CreateMultifield(const Environment&, size_t);
void AddToMultifieldList(const Environment&, Multifield *);
void FlushMultifields(const Environment&);
void DuplicateMultifield(const Environment&, UDFValue *, UDFValue *);
void WriteMultifield(const Environment&, const char *, Multifield *);
void PrintMultifieldDriver(const Environment&, const char *, Multifield *, size_t, size_t, bool);
bool MultifieldDOsEqual(UDFValue *, UDFValue *);
void StoreInMultifield(const Environment&, UDFValue *, Expression *, bool);
Multifield *CopyMultifield(const Environment&, Multifield *);
bool MultifieldsEqual(Multifield *, Multifield *);
Multifield *DOToMultifield(const Environment&, UDFValue *);
size_t HashMultifield(Multifield *, size_t);
Multifield *GetMultifieldList(const Environment&);
CLIPSLexeme *ImplodeMultifield(const Environment&, UDFValue *);
void EphemerateMultifield(const Environment&, Multifield *);
Multifield *ArrayToMultifield(const Environment&, CLIPSValue *, unsigned long);
void NormalizeMultifield(const Environment&, UDFValue *);
void CLIPSToUDFValue(CLIPSValue *, UDFValue *);
void UDFToCLIPSValue(const Environment&, UDFValue *, CLIPSValue *);
MultifieldBuilder *CreateMultifieldBuilder(const Environment&, size_t);
void MBReset(MultifieldBuilder *);
void MBDispose(MultifieldBuilder *);
void MBAppend(MultifieldBuilder *theMB, CLIPSValue *);
Multifield *MBCreate(MultifieldBuilder *);
Multifield *EmptyMultifield(const Environment&);
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

void MultifieldFunctionDefinitions(const Environment&);
#if MULTIFIELD_FUNCTIONS
void DeleteFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ReplaceFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void DeleteMemberFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ReplaceMemberFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void InsertFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ExplodeFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ImplodeFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SubseqFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void FirstFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void RestFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void NthFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void SubsetpFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void MemberFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void MultifieldPrognFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void ForeachFunction(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void GetMvPrognField(const Environment&theEnv, UDFContext *context, UDFValue *ret);
void GetMvPrognIndex(const Environment&theEnv, UDFContext *context, UDFValue *ret);
bool FindDOsInSegment(UDFValue *, unsigned int, UDFValue *,
                      size_t *, size_t *, size_t *, unsigned int);
#endif
bool ReplaceMultiValueFieldSizet(const Environment&, UDFValue *, UDFValue *,
                                 size_t, size_t, UDFValue *, const char *);
bool InsertMultiValueField(const Environment&, UDFValue *, UDFValue *,
                           size_t, UDFValue *, const char *);
void MVRangeError(const Environment&, long long, long long, size_t, const char *);
size_t FindValueInMultifield(UDFValue *, UDFValue *);

#endif /* _H_multifld */




