/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  11/01/16            */
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
/* Revision History:                                          */
/*                                                            */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859   */
/*                                                            */
/*            Changed name of variable log to logName         */
/*            because of Unix compiler warnings of shadowed   */
/*            definitions.                                    */
/*                                                            */
/*      6.24: Renamed BOOLEAN macro type to intBool.          */
/*                                                            */
/*            Added pragmas to remove compilation warnings.   */
/*                                                            */
/*      6.30: Updated ENTITY_RECORD definitions to include    */
/*            additional nullptr initializers.                   */
/*                                                            */
/*            Added ReleaseProcParameters call.               */
/*                                                            */
/*            Added tracked memory calls.                     */
/*                                                            */
/*            Removed conditional code for unsupported        */
/*            compilers/operating systems (IBM_MCW,           */
/*            MAC_MCW, and IBM_TBC).                          */
/*                                                            */
/*            Added const qualifiers to remove C++            */
/*            deprecation warnings.                           */
/*                                                            */
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

#ifndef _H_prccode

#pragma once

#define _H_prccode

#include "Expression.h"
#include "Evaluation.h"
#include "Defmodule.h"
#include "Scanner.h"
#include "Symbol.h"

typedef struct ProcParamStack {
    UDFValue *ParamArray;

#if DEFGENERIC_CONSTRUCT
    Expression *ParamExpressions;
#endif

    unsigned int ParamArraySize;
    UDFValue *WildcardValue;
    void (*UnboundErrFunc)(const Environment&, const char *);
    struct ProcParamStack *nxt;
} PROC_PARAM_STACK;

constexpr auto PROCEDURAL_PRIMITIVE_DATA = 37;

struct proceduralPrimitiveData : public EnvironmentModule {
    Multifield *NoParamValue;
    UDFValue *ProcParamArray;
    unsigned int ProcParamArraySize;
    Expression *CurrentProcActions;
#if DEFGENERIC_CONSTRUCT
    Expression *ProcParamExpressions;
#endif
    PROC_PARAM_STACK *pstack;
    UDFValue *WildcardValue;
    UDFValue *LocalVarArray;
    void (*ProcUnboundErrFunc)(const Environment&, const char *);
    EntityRecord ProcParameterInfo;
    EntityRecord ProcWildInfo;
    EntityRecord ProcGetInfo;
    EntityRecord ProcBindInfo;
#if !DEFFUNCTION_CONSTRUCT
    EntityRecord DeffunctionEntityRecord;
#endif
#if !DEFGENERIC_CONSTRUCT
    EntityRecord GenericEntityRecord;
#endif
    unsigned int Oldindex;
};
RegisterEnvironmentModule(proceduralPrimitiveData, PROCEDURAL_PRIMITIVE_DATA, ProceduralPrimitive);

void InstallProcedurePrimitives(const Environment&);

Expression *ParseProcParameters(const Environment&, const char *, struct token *, Expression *,
                                CLIPSLexeme **, unsigned short *, unsigned short *, bool *,
                                bool (*)(const Environment&, const char *));
Expression *ParseProcActions(const Environment&, const char *, const char *, struct token *, Expression *, CLIPSLexeme *,
                             int (*)(const Environment&, Expression *, void *),
                             int (*)(const Environment&, Expression *, void *),
                             unsigned short *, void *);
int ReplaceProcVars(const Environment&, const char *, Expression *, Expression *, CLIPSLexeme *,
                    int (*)(const Environment&, Expression *, void *), void *);
#if DEFGENERIC_CONSTRUCT
Expression *GenProcWildcardReference(const Environment&, int);
#endif

void PushProcParameters(const Environment&, Expression *, unsigned int, const char *, const char *, void (*)(const Environment&, const char *));
void PopProcParameters(const Environment&);

#if DEFGENERIC_CONSTRUCT
Expression *GetProcParamExpressions(const Environment&);
#endif

void EvaluateProcActions(const Environment&, Defmodule *, Expression *, unsigned short,
                         UDFValue *, void (*)(const Environment&, const char *));
void PrintProcParamArray(const Environment&, const char *);
void GrabProcWildargs(const Environment&, UDFValue *, unsigned int);

#endif /* _H_prccode */

