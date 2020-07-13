/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  08/25/16            */
/*                                                     */
/*          PROCEDURAL FUNCTIONS HEADER FILE           */
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
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Local variables set with the bind function     */
/*            persist until a reset/clear command is issued. */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Support for long long integers.                */
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

#ifndef _H_prcdrfun

#pragma once

#define _H_prcdrfun

#include "Evaluation.h"

typedef struct loopCounterStack {
    long long loopCounter;
    struct loopCounterStack *nxt;
} LOOP_COUNTER_STACK;

constexpr auto PRCDRFUN_DATA = 13;

struct procedureFunctionData : public EnvironmentModule {
    bool ReturnFlag;
    bool BreakFlag;
    LOOP_COUNTER_STACK *LoopCounterStack;
    UDFValue *BindList;
};
RegisterEnvironmentModule(procedureFunctionData, PRCDRFUN_DATA, ProcedureFunction);

void ProceduralFunctionDefinitions(const Environment::Ptr&);
void WhileFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void LoopForCountFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void GetLoopCount(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void IfFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void BindFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void PrognFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ReturnFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void BreakFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SwitchFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
bool GetBoundVariable(const Environment::Ptr&, UDFValue *, CLIPSLexeme *);
void FlushBindList(const Environment::Ptr&, void *);

#endif /* _H_prcdrfun */






