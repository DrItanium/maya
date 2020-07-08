/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  08/25/16            */
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
/*            Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_insquery

#pragma once

#define _H_insquery

#if INSTANCE_SET_QUERIES

#include "Object.h"

typedef struct query_class {
    Defclass *cls;
    Defmodule *theModule;
    struct query_class *chain, *nxt;
} QUERY_CLASS;

typedef struct query_soln {
    Instance **soln;
    struct query_soln *nxt;
} QUERY_SOLN;

typedef struct query_core {
    Instance **solns;
    Expression *query, *action;
    QUERY_SOLN *soln_set, *soln_bottom;
    unsigned soln_size, soln_cnt;
    UDFValue *result;
} QUERY_CORE;

typedef struct query_stack {
    QUERY_CORE *core;
    struct query_stack *nxt;
} QUERY_STACK;

#define INSTANCE_QUERY_DATA 31

struct instanceQueryData {
    CLIPSLexeme *QUERY_DELIMITER_SYMBOL;
    QUERY_CORE *QueryCore;
    QUERY_STACK *QueryCoreStack;
    bool AbortQuery;
};

#define InstanceQueryData(theEnv) ((struct instanceQueryData *) GetEnvironmentData(theEnv,INSTANCE_QUERY_DATA))

#define QUERY_DELIMITER_STRING     "(QDS)"

void SetupQuery(Environment *);
void GetQueryInstance(Environment *env, UDFContext *context, UDFValue *ret);
void GetQueryInstanceSlot(Environment *env, UDFContext *context, UDFValue *ret);
void AnyInstances(Environment *env, UDFContext *context, UDFValue *ret);
void QueryFindInstance(Environment *env, UDFContext *context, UDFValue *ret);
void QueryFindAllInstances(Environment *env, UDFContext *context, UDFValue *ret);
void QueryDoForInstance(Environment *env, UDFContext *context, UDFValue *ret);
void QueryDoForAllInstances(Environment *env, UDFContext *context, UDFValue *ret);
void DelayedQueryDoForAllInstances(Environment *env, UDFContext *context, UDFValue *ret);

#endif /* INSTANCE_SET_QUERIES */

#endif /* _H_insquery */





