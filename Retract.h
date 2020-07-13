/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/30/16            */
/*                                                     */
/*                RETRACT HEADER FILE                  */
/*******************************************************/

/*************************************************************/
/* Purpose:  Handles join network activity associated with   */
/*   with the removal of a data entity such as a fact or     */
/*   instance.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed LOGICAL_DEPENDENCIES compilation flag. */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Rule with exists CE has incorrect activation.  */
/*            DR0867                                         */
/*                                                           */
/*      6.30: Added support for hashed memories.             */
/*                                                           */
/*            Added additional developer statistics to help  */
/*            analyze join network performance.              */
/*                                                           */
/*            Removed pseudo-facts used in not CEs.          */
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

#ifndef _H_retract

#pragma once

#define _H_retract

#include "Match.h"
#include "Network.h"

struct rdriveinfo {
    PartialMatch *link;
    struct joinNode *jlist;
    struct rdriveinfo *next;
};

void NetworkRetract(const Environment::Ptr&, struct patternMatch *);
void ReturnPartialMatch(const Environment::Ptr&, PartialMatch *);
void DestroyPartialMatch(const Environment::Ptr&, PartialMatch *);
void FlushGarbagePartialMatches(const Environment::Ptr&);
void DeletePartialMatches(const Environment::Ptr&, PartialMatch *);
void PosEntryRetractBeta(const Environment::Ptr&, PartialMatch *, PartialMatch *, int);
void PosEntryRetractAlpha(const Environment::Ptr&, PartialMatch *, int);
bool PartialMatchWillBeDeleted(const Environment::Ptr&, PartialMatch *);

#endif /* _H_retract */



