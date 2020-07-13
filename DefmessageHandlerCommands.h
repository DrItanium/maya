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
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Removed IMPERATIVE_MESSAGE_HANDLERS            */
/*                    compilation flag.                      */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warnings.                             */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added DeallocateMessageHandlerData to          */
/*            deallocate message handler environment data.   */
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

#ifndef _H_msgcom

#pragma once

#define _H_msgcom

#include "DefmessageHandlerDispatch.h"
#include "Object.h"

constexpr auto MESSAGE_HANDLER_DATA = 32;

struct messageHandlerData : public EnvironmentModule {
    EntityRecord HandlerGetInfo;
    EntityRecord HandlerPutInfo;
    CLIPSLexeme *INIT_SYMBOL;
    CLIPSLexeme *DELETE_SYMBOL;
    CLIPSLexeme *CREATE_SYMBOL;
#if DEBUGGING_FUNCTIONS
    bool WatchHandlers;
    bool WatchMessages;
#endif
    const char *hndquals[4];
    CLIPSLexeme *SELF_SYMBOL;
    CLIPSLexeme *CurrentMessageName;
    HANDLER_LINK *CurrentCore;
    HANDLER_LINK *TopOfCore;
    HANDLER_LINK *NextInCore;
    HANDLER_LINK *OldCore;
};
RegisterEnvironmentModule(messageHandlerData, MESSAGE_HANDLER_DATA, MessageHandler);

#define INIT_STRING   "init"
#define DELETE_STRING "delete"
#define PRINT_STRING  "print"
#define CREATE_STRING "create"

void SetupMessageHandlers(const Environment::Ptr&);
const char *DefmessageHandlerName(Defclass *, unsigned);
const char *DefmessageHandlerType(Defclass *, unsigned);
unsigned GetNextDefmessageHandler(Defclass *, unsigned);
DefmessageHandler
*GetDefmessageHandlerPointer(Defclass *, unsigned int);
#if DEBUGGING_FUNCTIONS
bool DefmessageHandlerGetWatch(Defclass *, unsigned);
void DefmessageHandlerSetWatch(Defclass *, unsigned, bool);
#endif
unsigned FindDefmessageHandler(Defclass *, const char *, const char *);
bool DefmessageHandlerIsDeletable(Defclass *, unsigned);
void UndefmessageHandlerCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
bool UndefmessageHandler(Defclass *, unsigned, const Environment::Ptr&);
#if DEBUGGING_FUNCTIONS
void PPDefmessageHandlerCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void ListDefmessageHandlersCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void PreviewSendCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
const char *DefmessageHandlerPPForm(Defclass *, unsigned);
void ListDefmessageHandlers(const Environment::Ptr&, Defclass *, const char *, bool);
void PreviewSend(Defclass *, const char *, const char *);
unsigned long DisplayHandlersInLinks(const Environment::Ptr&, const char *, PACKED_CLASS_LINKS *, unsigned int);
#endif

#endif /* _H_msgcom */





