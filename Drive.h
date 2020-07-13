/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  07/30/16            */
/*                                                     */
/*                  DRIVE HEADER FILE                  */
/*******************************************************/

/*************************************************************/
/* Purpose: Handles join network activity associated with    */
/*   with the addition of a data entity such as a fact or    */
/*   instance.                                               */
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
/*      6.30: Added support for hashed memories.             */
/*                                                           */
/*            Added additional developer statistics to help  */
/*            analyze join network performance.              */
/*                                                           */
/*            Removed pseudo-facts used in not CE.           */
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

#ifndef _H_drive

#pragma once

#define _H_drive

#include "Expression.h"
#include "Match.h"
#include "Network.h"

void NetworkAssert(const Environment::Ptr&, PartialMatch *, struct joinNode *);
bool EvaluateJoinExpression(const Environment::Ptr&, Expression *, struct joinNode *);
void NetworkAssertLeft(const Environment::Ptr&, PartialMatch *, struct joinNode *, int);
void NetworkAssertRight(const Environment::Ptr&, PartialMatch *, struct joinNode *, int);
void PPDrive(const Environment::Ptr&, PartialMatch *, PartialMatch *, struct joinNode *, int);
unsigned long BetaMemoryHashValue(const Environment::Ptr&, Expression *, PartialMatch *, PartialMatch *, struct joinNode *);
bool EvaluateSecondaryNetworkTest(const Environment::Ptr&, PartialMatch *, struct joinNode *);
void EPMDrive(const Environment::Ptr&, PartialMatch *, struct joinNode *, int);

#endif /* _H_drive */





