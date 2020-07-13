/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  08/25/16            */
/*                                                     */
/*                                                     */
/*******************************************************/

/*************************************************************/
/* Purpose: Message-passing support functions                */
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
/*      6.24: Removed IMPERATIVE_MESSAGE_HANDLERS and        */
/*            AUXILIARY_MESSAGE_HANDLERS compilation flags.  */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: The return value of DirectMessage indicates    */
/*            whether an execution error has occurred.       */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed garbage collection algorithm.          */
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

#ifndef _H_msgpass

#pragma once

#define _H_msgpass

#define GetActiveInstance(theEnv) (GetNthMessageArgument(theEnv,0)->instanceValue)

#include "Object.h"

typedef struct messageHandlerLink {
    DefmessageHandler *hnd;
    struct messageHandlerLink *nxt;
    struct messageHandlerLink *nxtInStack;
} HANDLER_LINK;

bool DirectMessage(const Environment::Ptr&, CLIPSLexeme *, Instance *,
                   UDFValue *, Expression *);
void Send(const Environment::Ptr&, CLIPSValue *, const char *, const char *, CLIPSValue *);
void DestroyHandlerLinks(const Environment::Ptr&, HANDLER_LINK *);
void SendCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
UDFValue *GetNthMessageArgument(const Environment::Ptr&, int);

bool NextHandlerAvailable(const Environment::Ptr&);
void NextHandlerAvailableFunction(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void CallNextHandler(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void FindApplicableOfName(const Environment::Ptr&, Defclass *, HANDLER_LINK *[],
                          HANDLER_LINK *[], CLIPSLexeme *);
HANDLER_LINK *JoinHandlerLinks(const Environment::Ptr&, HANDLER_LINK *[], HANDLER_LINK *[], CLIPSLexeme *);

void PrintHandlerSlotGetFunction(const Environment::Ptr&, const char *, void *);
bool HandlerSlotGetFunction(const Environment::Ptr&, void *, UDFValue *);
void PrintHandlerSlotPutFunction(const Environment::Ptr&, const char *, void *);
bool HandlerSlotPutFunction(const Environment::Ptr&, void *, UDFValue *);
void DynamicHandlerGetSlot(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void DynamicHandlerPutSlot(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);

#endif /* _H_object */







