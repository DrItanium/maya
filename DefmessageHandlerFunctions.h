/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  10/01/16            */
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
/*      6.23: Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Removed IMPERATIVE_MESSAGE_HANDLERS and        */
/*            AUXILIARY_MESSAGE_HANDLERS compilation flags.  */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Changed integer type/precision.                */
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
/*************************************************************/

#ifndef _H_msgfun

#pragma once

#define _H_msgfun

typedef struct handlerSlotReference {
    unsigned classID;
    unsigned short slotID;
} HANDLER_SLOT_REFERENCE;

#include "DefmessageHandlerDispatch.h"
#include "Object.h"

#define BEGIN_TRACE ">>"
#define END_TRACE   "<<"

/* =================================================================================
   Message-handler types - don't change these values: a string array depends on them
   ================================================================================= */
constexpr auto MAROUND       = 0;
constexpr auto MBEFORE       = 1;
constexpr auto MPRIMARY      = 2;
constexpr auto MAFTER        = 3;
constexpr auto MERROR        = 4;

constexpr auto LOOKUP_HANDLER_INDEX   = 0;
constexpr auto LOOKUP_HANDLER_ADDRESS = 1;

void UnboundHandlerErr(const Environment&, const char *);
void PrintNoHandlerError(const Environment&, const char *);
bool CheckHandlerArgCount(const Environment&);
void SlotAccessViolationError(const Environment&, const char *, Instance *, Defclass *);
void SlotVisibilityViolationError(const Environment&, SlotDescriptor *, Defclass *, bool);

void NewSystemHandler(const Environment&, const char *, const char *, const char *, unsigned short);
DefmessageHandler
*InsertHandlerHeader(const Environment&, Defclass *, CLIPSLexeme *, unsigned);

DefmessageHandler
*NewHandler();
bool HandlersExecuting(Defclass *);
bool DeleteHandler(const Environment&, Defclass *, CLIPSLexeme *, int, bool);
void DeallocateMarkedHandlers(const Environment&, Defclass *);
unsigned short HandlerType(const Environment&, const char *, bool, const char *);
bool CheckCurrentMessage(const Environment&, const char *, bool);
void PrintHandler(const Environment&, const char *, DefmessageHandler *, bool, bool);
DefmessageHandler
*FindHandlerByAddress(Defclass *, CLIPSLexeme *, unsigned);
int FindHandlerByIndex(Defclass *, CLIPSLexeme *, unsigned);
int FindHandlerNameGroup(Defclass *, CLIPSLexeme *);
void HandlerDeleteError(const Environment&, const char *);

#if DEBUGGING_FUNCTIONS
void DisplayCore(const Environment&, const char *, HANDLER_LINK *, int);
HANDLER_LINK *FindPreviewApplicableHandlers(const Environment&, Defclass *, CLIPSLexeme *);
void WatchMessage(const Environment&, const char *, const char *);
void WatchHandler(const Environment&, const char *, HANDLER_LINK *, const char *);
#endif

#endif /* _H_msgfun */







