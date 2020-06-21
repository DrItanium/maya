   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  07/02/18             */
   /*                                                     */
   /*          DEFRULE BASIC COMMANDS HEADER FILE         */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements core commands for the defrule         */
/*   construct such as clear, reset, save, undefrule,        */
/*   ppdefrule, list-defrules, and                           */
/*   get-defrule-list.                                       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*            Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Support for join network changes.              */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Added code to prevent a clear command from     */
/*            being executed during fact assertions via      */
/*            JoinOperationInProgress mechanism.             */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
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
/*            Pretty print functions accept optional logical */
/*            name argument.                                 */
/*                                                           */
/*************************************************************/

#include "setup.h"

#if DEFRULE_CONSTRUCT

#include <stdio.h>

#include "ArgumentAccess.h"
#include "constrct.h"
#include "drive.h"
#include "Engine.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "multifld.h"
#include "reteutil.h"
#include "Router.h"
#include "ruledef.h"
#include "Watch.h"

#if BLOAD || BLOAD_AND_BSAVE
#include "rulebin.h"
#endif

#include "rulebsc.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    ResetDefrules(Environment *,void *);
   static void                    ResetDefrulesPrime(Environment *,void *);
   static void                    SaveDefrules(Environment *,Defmodule *,const char *,void *);
   static bool                    ClearDefrulesReady(Environment *,void *);
   static void                    ClearDefrules(Environment *,void *);

/*************************************************************/
/* DefruleBasicCommands: Initializes basic defrule commands. */
/*************************************************************/
void DefruleBasicCommands(
  Environment *theEnv)
  {
   AddResetFunction(theEnv,"defrule",ResetDefrules,70,NULL);
   AddResetFunction(theEnv,"defrule",ResetDefrulesPrime,10,NULL);
   AddSaveFunction(theEnv,"defrule",SaveDefrules,0,NULL);
   AddClearReadyFunction(theEnv,"defrule",ClearDefrulesReady,0,NULL);
   AddClearFunction(theEnv,"defrule",ClearDefrules,0,NULL);

#if DEBUGGING_FUNCTIONS
   AddWatchItem(theEnv,"rules",0,&DefruleData(theEnv)->WatchRules,70,DefruleWatchAccess,DefruleWatchPrint);
#endif

   AddUDF(theEnv,"get-defrule-list","m",0,1,"y",GetDefruleListFunction,"GetDefruleListFunction",NULL);
   AddUDF(theEnv,"undefrule","v",1,1,"y",UndefruleCommand,"UndefruleCommand",NULL);
   AddUDF(theEnv,"defrule-module","y",1,1,"y",DefruleModuleFunction,"DefruleModuleFunction",NULL);

#if DEBUGGING_FUNCTIONS
   AddUDF(theEnv,"rules","v",0,1,"y",ListDefrulesCommand,"ListDefrulesCommand",NULL);
   AddUDF(theEnv,"list-defrules","v",0,1,"y",ListDefrulesCommand,"ListDefrulesCommand",NULL);
   AddUDF(theEnv,"ppdefrule","vs",1,2,";y;ldsyn",PPDefruleCommand,"PPDefruleCommand",NULL);
#endif

#if (BLOAD || BLOAD_AND_BSAVE)
   DefruleBinarySetup(theEnv);
#endif

  }

/*****************************************************/
/* ResetDefrules: Defrule reset routine for use with */
/*   the reset command. Sets the current entity time */
/*   tag (used by the conflict resolution strategies */
/*   for recency) to zero. The focus stack is also   */
/*   cleared.                                        */
/*****************************************************/
static void ResetDefrules(
  Environment *theEnv,
  void *context)
  {
   Defmodule *theModule;
   struct joinLink *theLink;
   struct partialMatch *notParent;

   DefruleData(theEnv)->CurrentEntityTimeTag = 1L;
   ClearFocusStack(theEnv);
   theModule = FindDefmodule(theEnv,"MAIN");
   Focus(theModule);

   for (theLink = DefruleData(theEnv)->RightPrimeJoins;
        theLink != NULL;
        theLink = theLink->next)
     { PosEntryRetractAlpha(theEnv,theLink->join->rightMemory->beta[0],NETWORK_ASSERT); }

   for (theLink = DefruleData(theEnv)->LeftPrimeJoins;
        theLink != NULL;
        theLink = theLink->next)
     {
      if ((theLink->join->patternIsNegated || theLink->join->joinFromTheRight) &&
          (! theLink->join->patternIsExists))
        {
         notParent = theLink->join->leftMemory->beta[0];

         if (notParent->marker)
           { RemoveBlockedLink(notParent); }

         /*==========================================================*/
         /* Prevent any retractions from generating partial matches. */
         /*==========================================================*/

         notParent->marker = notParent;

         if (notParent->children != NULL)
           { PosEntryRetractBeta(theEnv,notParent,notParent->children,NETWORK_ASSERT); }
           /*
         if (notParent->dependents != NULL)
           { RemoveLogicalSupport(theEnv,notParent); } */
        }
     }
  }

/***********************/
/* ResetDefrulesPrime: */
/***********************/
static void ResetDefrulesPrime(
  Environment *theEnv,
  void *context)
  {
   struct joinLink *theLink;
   struct partialMatch *notParent;

   for (theLink = DefruleData(theEnv)->RightPrimeJoins;
        theLink != NULL;
        theLink = theLink->next)
     { NetworkAssert(theEnv,theLink->join->rightMemory->beta[0],theLink->join); }

   for (theLink = DefruleData(theEnv)->LeftPrimeJoins;
        theLink != NULL;
        theLink = theLink->next)
     {
      if ((theLink->join->patternIsNegated || theLink->join->joinFromTheRight) &&
          (! theLink->join->patternIsExists))
        {
         notParent = theLink->join->leftMemory->beta[0];

         if (theLink->join->secondaryNetworkTest != NULL)
           {
            if (EvaluateSecondaryNetworkTest(theEnv,notParent,theLink->join) == false)
              { continue; }
           }

         notParent->marker = NULL;

         EPMDrive(theEnv,notParent,theLink->join,NETWORK_ASSERT);
        }
     }

  }


/******************************************************************/
/* ClearDefrulesReady: Indicates whether defrules can be cleared. */
/******************************************************************/
static bool ClearDefrulesReady(
  Environment *theEnv,
  void *context)
  {
   if (EngineData(theEnv)->ExecutingRule != NULL) return false;

   if (EngineData(theEnv)->JoinOperationInProgress) return false;

   ClearFocusStack(theEnv);
   if (GetCurrentModule(theEnv) == NULL) return false;

   DefruleData(theEnv)->CurrentEntityTimeTag = 1L;

   return true;
  }

/***************************************************************/
/* ClearDefrules: Pushes the MAIN module as the current focus. */
/***************************************************************/
static void ClearDefrules(
  Environment *theEnv,
  void *context)
  {
   Defmodule *theModule;

   theModule = FindDefmodule(theEnv,"MAIN");
   Focus(theModule);
  }


/**************************************/
/* SaveDefrules: Defrule save routine */
/*   for use with the save command.   */
/**************************************/
static void SaveDefrules(
  Environment *theEnv,
  Defmodule *theModule,
  const char *logicalName,
  void *context)
  {
   SaveConstruct(theEnv,theModule,logicalName,DefruleData(theEnv)->DefruleConstruct);
  }

/******************************************/
/* UndefruleCommand: H/L access routine   */
/*   for the undefrule command.           */
/******************************************/
void UndefruleCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UndefconstructCommand(context,"undefrule",DefruleData(theEnv)->DefruleConstruct);
  }

/********************************/
/* Undefrule: C access routine  */
/*   for the undefrule command. */
/********************************/
bool Undefrule(
  Defrule *theDefrule,
  Environment *allEnv)
  {
   Environment *theEnv;
   
   if (theDefrule == NULL)
     {
      theEnv = allEnv;
      return Undefconstruct(theEnv,NULL,DefruleData(theEnv)->DefruleConstruct);
     }
   else
     {
      theEnv = theDefrule->header.env;
      return Undefconstruct(theEnv,&theDefrule->header,DefruleData(theEnv)->DefruleConstruct);
     }
  }

/************************************************/
/* GetDefruleListFunction: H/L access routine   */
/*   for the get-defrule-list function.         */
/************************************************/
void GetDefruleListFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   GetConstructListFunction(context,returnValue,DefruleData(theEnv)->DefruleConstruct);
  }

/****************************************/
/* GetDefruleList: C access routine     */
/*   for the get-defrule-list function. */
/****************************************/
void GetDefruleList(
  Environment *theEnv,
  CLIPSValue *returnValue,
  Defmodule *theModule)
  {
   UDFValue result;
   
   GetConstructList(theEnv,&result,DefruleData(theEnv)->DefruleConstruct,theModule);
   NormalizeMultifield(theEnv,&result);
   returnValue->value = result.value;
  }

/*********************************************/
/* DefruleModuleFunction: H/L access routine */
/*   for the defrule-module function.        */
/*********************************************/
void DefruleModuleFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   returnValue->value = GetConstructModuleCommand(context,"defrule-module",DefruleData(theEnv)->DefruleConstruct);
  }

#if DEBUGGING_FUNCTIONS

/******************************************/
/* PPDefruleCommand: H/L access routine   */
/*   for the ppdefrule command.           */
/******************************************/
void PPDefruleCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   PPConstructCommand(context,"ppdefrule",DefruleData(theEnv)->DefruleConstruct,returnValue);
  }

/***********************************/
/* PPDefrule: C access routine for */
/*   the ppdefrule command.        */
/***********************************/
bool PPDefrule(
  Environment *theEnv,
  const char *defruleName,
  const char *logicalName)
  {
   return(PPConstruct(theEnv,defruleName,logicalName,DefruleData(theEnv)->DefruleConstruct));
  }

/*********************************************/
/* ListDefrulesCommand: H/L access routine   */
/*   for the list-defrules command.          */
/*********************************************/
void ListDefrulesCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   ListConstructCommand(context,DefruleData(theEnv)->DefruleConstruct);
  }

/************************************/
/* ListDefrules: C access routine   */
/*   for the list-defrules command. */
/************************************/
void ListDefrules(
  Environment *theEnv,
  const char *logicalName,
  Defmodule *theModule)
  {
   ListConstruct(theEnv,DefruleData(theEnv)->DefruleConstruct,logicalName,theModule);
  }

/*******************************************************/
/* DefruleGetWatchActivations: C access routine for    */
/*   retrieving the current watch value of a defrule's */
/*   activations.                                      */
/*******************************************************/
bool DefruleGetWatchActivations(
  Defrule *rulePtr)
  {
   Defrule *thePtr;

   for (thePtr = rulePtr;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { if (thePtr->watchActivation) return true; }

   return false;
  }

/********************************************/
/* DefruleGetWatchFirings: C access routine */
/*   for retrieving the current watch value */
/*   of a defrule's firings.                */
/********************************************/
bool DefruleGetWatchFirings(
  Defrule *rulePtr)
  {
   Defrule *thePtr;

   for (thePtr = rulePtr;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { if (thePtr->watchFiring) return true; }

   return false;
  }

/************************************************/
/* DefruleSetWatchActivations: C access routine */
/*   for setting the current watch value of a   */
/*   defrule's activations.                     */
/************************************************/
void DefruleSetWatchActivations(
  Defrule *rulePtr,
  bool newState)
  {
   Defrule *thePtr;

   for (thePtr = rulePtr;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { thePtr->watchActivation = newState; }
  }

/********************************************/
/* DefruleSetWatchFirings: C access routine */
/*   for setting the current watch value of */
/*   a defrule's firings.                   */
/********************************************/
void DefruleSetWatchFirings(
  Defrule *rulePtr,
  bool newState)
  {
   Defrule *thePtr;

   for (thePtr = rulePtr;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { thePtr->watchFiring = newState; }
  }

/*******************************************************************/
/* DefruleWatchAccess: Access function for setting the watch flags */
/*   associated with rules (activations and rule firings).         */
/*******************************************************************/
bool DefruleWatchAccess(
  Environment *theEnv,
  int code,
  bool newState,
  struct expr *argExprs)
  {
   if (code)
     return(ConstructSetWatchAccess(theEnv,DefruleData(theEnv)->DefruleConstruct,newState,argExprs,
                                    (ConstructGetWatchFunction *) DefruleGetWatchActivations,
                                    (ConstructSetWatchFunction *) DefruleSetWatchActivations));
   else
     return(ConstructSetWatchAccess(theEnv,DefruleData(theEnv)->DefruleConstruct,newState,argExprs,
                                    (ConstructGetWatchFunction *) DefruleGetWatchFirings,
                                    (ConstructSetWatchFunction *) DefruleSetWatchFirings));
  }

/*****************************************************************/
/* DefruleWatchPrint: Access routine for printing which defrules */
/*   have their watch flag set via the list-watch-items command. */
/*****************************************************************/
bool DefruleWatchPrint(
  Environment *theEnv,
  const char *logName,
  int code,
  struct expr *argExprs)
  {
   if (code)
     return(ConstructPrintWatchAccess(theEnv,DefruleData(theEnv)->DefruleConstruct,logName,argExprs,
                                      (ConstructGetWatchFunction *) DefruleGetWatchActivations,
                                      (ConstructSetWatchFunction *) DefruleSetWatchActivations));
   else
     return(ConstructPrintWatchAccess(theEnv,DefruleData(theEnv)->DefruleConstruct,logName,argExprs,
                                      (ConstructGetWatchFunction *) DefruleGetWatchActivations,
                                      (ConstructSetWatchFunction *) DefruleGetWatchActivations));
  }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFTEMPLATE_CONSTRUCT */
