/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  08/25/16            */
/*                                                     */
/*                 BSAVE HEADER FILE                   */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Used genstrncpy instead of strncpy.            */
/*                                                           */
/*            Borland C (IBM_TBC) and Metrowerks CodeWarrior */
/*            (MAC_MCW, IBM_MCW) are no longer supported.    */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_bsave

#pragma once

#define _H_bsave

struct BinaryItem;

#include <cstdio>

#include "Expression.h"

struct BinaryItem {
    const char *name;
    void (*findFunction)(const Environment::Ptr&);
    void (*bloadStorageFunction)(const Environment::Ptr&);
    void (*bloadFunction)(const Environment::Ptr&);
    void (*clearFunction)(const Environment::Ptr&);
    void (*expressionFunction)(const Environment::Ptr&, FILE *);
    void (*bsaveStorageFunction)(const Environment::Ptr&, FILE *);
    void (*bsaveFunction)(const Environment::Ptr&, FILE *);
    int priority;
    struct BinaryItem *next;
};

#if BLOAD_AND_BSAVE
typedef struct bloadcntsv {
    unsigned long val;
    struct bloadcntsv *nxt;
} BLOADCNTSV;
#endif

typedef struct bsave_expr {
    unsigned short type;
    unsigned long value;
    unsigned long arg_list;
    unsigned long next_arg;
} BSAVE_EXPRESSION;

constexpr auto CONSTRUCT_HEADER_SIZE = 20;

constexpr auto BSAVE_DATA = 39;

struct bsaveData : public EnvironmentModule {
    struct BinaryItem *ListOfBinaryItems;
#if BLOAD_AND_BSAVE
    BLOADCNTSV *BloadCountSaveTop;
#endif
};
RegisterEnvironmentModule(bsaveData, BSAVE_DATA, Bsave);

void InitializeBsaveData(const Environment::Ptr&);
void BsaveCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
#if BLOAD_AND_BSAVE
bool Bsave(const Environment::Ptr&, const char *);
void MarkNeededItems(const Environment::Ptr&, Expression *);
void SaveBloadCount(const Environment::Ptr&, unsigned long);
void RestoreBloadCount(const Environment::Ptr&, unsigned long *);
#endif
bool AddBinaryItem(const Environment::Ptr&, const char *, int,
                   void (*)(const Environment::Ptr&),
                   void (*)(const Environment::Ptr&, FILE *),
                   void (*)(const Environment::Ptr&, FILE *),
                   void (*)(const Environment::Ptr&, FILE *),
                   void (*)(const Environment::Ptr&),
                   void (*)(const Environment::Ptr&),
                   void (*)(const Environment::Ptr&));

#endif /* _H_bsave */







