   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.50  10/20/23            */
   /*                                                     */
   /*                FACT GOAL HEADER FILE                */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.50: Support for data driven backward chaining.     */
/*                                                           */
/*************************************************************/

#ifndef _H_factgoal

#pragma once

#define _H_factgoal

#include "entities.h"
#include "conscomp.h"
#include "tmpltdef.h"

typedef enum
  {
   RETRACT_ACTION,
   ASSERT_ACTION
  } GoalQueueAction;

struct queueItem
  {
   GoalQueueAction action;
   struct fact *theFact;
   struct queueItem *nextInQueue;
  };

struct extractedInfo
  {
   unsigned short field;
   bool multifield;
   bool fromLeft;
   bool fromRight;
   unsigned short offset;
   CLIPSValue theValue;
   struct extractedInfo *next;
  };

   void                           AttachGoal(Environment *,struct joinNode *,struct partialMatch *,struct partialMatch *,bool);
   Fact                          *GenerateGoal(Environment *,struct joinNode *,struct partialMatch *);
   void                           UpdateGoalSupport(Environment *,struct partialMatch *);
   void                           ProcessGoalQueue(Environment *);
   void                           AddToGoalQueue(Fact *,GoalQueueAction);

#endif /* _H_factgoal */
