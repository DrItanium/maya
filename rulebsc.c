   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  01/06/16             */
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
/*************************************************************/

#include "setup.h"

#if DEFRULE_CONSTRUCT

#include <stdio.h>

#include "argacces.h"
#include "constrct.h"
#include "drive.h"
#include "engine.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "reteutil.h"
#include "router.h"
#include "ruledef.h"
#include "watch.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "rulebin.h"
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "rulecmp.h"
#endif

#include "rulebsc.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    ResetDefrules(void *);
   static void                    ResetDefrulesPrime(void *);
   static void                    SaveDefrules(void *,void *,const char *);
#if (! RUN_TIME)
   static bool                    ClearDefrulesReady(void *);
   static void                    ClearDefrules(void *);
#endif

/*************************************************************/
/* DefruleBasicCommands: Initializes basic defrule commands. */
/*************************************************************/
void DefruleBasicCommands(
  void *theEnv)
  {
   EnvAddResetFunction(theEnv,"defrule",ResetDefrules,70);
   EnvAddResetFunction(theEnv,"defrule",ResetDefrulesPrime,10);
   AddSaveFunction(theEnv,"defrule",SaveDefrules,0);
#if (! RUN_TIME)
   AddClearReadyFunction(theEnv,"defrule",ClearDefrulesReady,0);
   EnvAddClearFunction(theEnv,"defrule",ClearDefrules,0);
#endif
   
#if DEBUGGING_FUNCTIONS
   AddWatchItem(theEnv,"rules",0,&DefruleData(theEnv)->WatchRules,70,DefruleWatchAccess,DefruleWatchPrint);
#endif

#if ! RUN_TIME
   EnvAddUDF(theEnv,"get-defrule-list","m", GetDefruleListFunction,"GetDefruleListFunction",0,1,"y",NULL);
   EnvAddUDF(theEnv,"undefrule","v", UndefruleCommand,"UndefruleCommand",1,1,"y",NULL);
   EnvAddUDF(theEnv,"defrule-module","y", DefruleModuleFunction,"DefruleModuleFunction",1,1,"y",NULL);

#if DEBUGGING_FUNCTIONS
   EnvAddUDF(theEnv,"rules","v", ListDefrulesCommand,"ListDefrulesCommand",0,1,"y",NULL);
   EnvAddUDF(theEnv,"list-defrules","v",  ListDefrulesCommand,"ListDefrulesCommand",0,1,"y",NULL);
   EnvAddUDF(theEnv,"ppdefrule","v", PPDefruleCommand,"PPDefruleCommand",1,1,"y",NULL);
#endif

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)
   DefruleBinarySetup(theEnv);
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   DefruleCompilerSetup(theEnv);
#endif

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
  void *theEnv)
  {
   struct defmodule *theModule;
   struct joinLink *theLink;
   struct partialMatch *notParent;
  
   DefruleData(theEnv)->CurrentEntityTimeTag = 1L;
   EnvClearFocusStack(theEnv);
   theModule = (struct defmodule *) EnvFindDefmodule(theEnv,"MAIN");
   EnvFocus(theEnv,(void *) theModule);
   
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

/*****************************************************/
/* ResetDefrulesPrime:                      */
/*****************************************************/
static void ResetDefrulesPrime(
  void *theEnv)
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

#if (! RUN_TIME)

/******************************************************************/
/* ClearDefrulesReady: Indicates whether defrules can be cleared. */
/******************************************************************/
static bool ClearDefrulesReady(
  void *theEnv)
  {
   if (EngineData(theEnv)->ExecutingRule != NULL) return(false);

   if (EngineData(theEnv)->JoinOperationInProgress) return(false);

   EnvClearFocusStack(theEnv);
   if (EnvGetCurrentModule(theEnv) == NULL) return(false);

   DefruleData(theEnv)->CurrentEntityTimeTag = 1L;

   return(true);
  }

/***************************************************************/
/* ClearDefrules: Pushes the MAIN module as the current focus. */
/***************************************************************/
static void ClearDefrules(
  void *theEnv)
  {
   struct defmodule *theModule;

   theModule = (struct defmodule *) EnvFindDefmodule(theEnv,"MAIN");
   EnvFocus(theEnv,(void *) theModule);
  }

#endif

/**************************************/
/* SaveDefrules: Defrule save routine */
/*   for use with the save command.   */
/**************************************/
static void SaveDefrules(
  void *theEnv,
  void *theModule,
  const char *logicalName)
  {
   SaveConstruct(theEnv,theModule,logicalName,DefruleData(theEnv)->DefruleConstruct); 
  }

/******************************************/
/* UndefruleCommand: H/L access routine   */
/*   for the undefrule command.           */
/******************************************/
void UndefruleCommand(
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   void *theEnv = UDFContextEnvironment(context);
   UndefconstructCommand(context,"undefrule",DefruleData(theEnv)->DefruleConstruct);
  }

/**********************************/
/* EnvUndefrule: C access routine */
/*   for the undefrule command.   */
/**********************************/
bool EnvUndefrule(
  void *theEnv,
  void *theDefrule)
  {
   return(Undefconstruct(theEnv,theDefrule,DefruleData(theEnv)->DefruleConstruct)); 
  }

/************************************************/
/* GetDefruleListFunction: H/L access routine   */
/*   for the get-defrule-list function.         */
/************************************************/
void GetDefruleListFunction(
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   void *theEnv = UDFContextEnvironment(context);
   GetConstructListFunction(context,"get-defrule-list",returnValue,DefruleData(theEnv)->DefruleConstruct);
  }

/****************************************/
/* EnvGetDefruleList: C access routine  */
/*   for the get-defrule-list function. */
/****************************************/
void EnvGetDefruleList(
  void *theEnv,
  DATA_OBJECT_PTR returnValue,
  void *theModule)
  {
   GetConstructList(theEnv,returnValue,DefruleData(theEnv)->DefruleConstruct,(struct defmodule *) theModule);
  }

/*********************************************/
/* DefruleModuleFunction: H/L access routine */
/*   for the defrule-module function.        */
/*********************************************/
void DefruleModuleFunction(
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   void *theEnv = UDFContextEnvironment(context);
   CVSetCLIPSSymbol(returnValue,GetConstructModuleCommand(context,"defrule-module",DefruleData(theEnv)->DefruleConstruct));
  }

#if DEBUGGING_FUNCTIONS

/******************************************/
/* PPDefruleCommand: H/L access routine   */
/*   for the ppdefrule command.           */
/******************************************/
void PPDefruleCommand(
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   void *theEnv = UDFContextEnvironment(context);
   PPConstructCommand(context,"ppdefrule",DefruleData(theEnv)->DefruleConstruct);
  }

/***********************************/
/* PPDefrule: C access routine for */
/*   the ppdefrule command.        */
/***********************************/
int PPDefrule(
  void *theEnv,
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
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   void *theEnv = UDFContextEnvironment(context);
   ListConstructCommand(context,"list-defrules",DefruleData(theEnv)->DefruleConstruct);
  }

/*************************************/
/* EnvListDefrules: C access routine */
/*   for the list-defrules command.  */
/*************************************/
void EnvListDefrules(
  void *theEnv,
  const char *logicalName,
  void *theModule)
  {
   ListConstruct(theEnv,DefruleData(theEnv)->DefruleConstruct,logicalName,(struct defmodule *) theModule); 
  }

/*******************************************************/
/* EnvGetDefruleWatchActivations: C access routine for */
/*   retrieving the current watch value of a defrule's */
/*   activations.                                      */
/*******************************************************/
bool EnvGetDefruleWatchActivations(
  void *theEnv,
  void *rulePtr)
  {
   struct defrule *thePtr;
#if MAC_XCD
#pragma unused(theEnv)
#endif

   for (thePtr = (struct defrule *) rulePtr;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { if (thePtr->watchActivation) return(true); }

   return(false);
  }

/***********************************************/
/* EnvGetDefruleWatchFirings: C access routine */
/*   for retrieving the current watch value of */
/*   a defrule's firings.                      */
/***********************************************/
bool EnvGetDefruleWatchFirings(
  void *theEnv,
  void *rulePtr)
  {
   struct defrule *thePtr;
#if MAC_XCD
#pragma unused(theEnv)
#endif

   for (thePtr = (struct defrule *) rulePtr;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { if (thePtr->watchFiring) return(true); }

   return(false);
  }

/***************************************************/
/* EnvSetDefruleWatchActivations: C access routine */
/*   for setting the current watch value of a      */
/*   defrule's activations.                        */
/***************************************************/
void EnvSetDefruleWatchActivations(
  void *theEnv,
  bool newState,
  void *rulePtr)
  {
   struct defrule *thePtr;
#if MAC_XCD
#pragma unused(theEnv)
#endif

   for (thePtr = (struct defrule *) rulePtr;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { thePtr->watchActivation = newState; }
  }

/****************************************************/
/* EnvSetDefruleWatchFirings: C access routine for  */
/*   setting the current watch value of a defrule's */ 
/*   firings.                                       */
/****************************************************/
void EnvSetDefruleWatchFirings(
  void *theEnv,
  bool newState,
  void *rulePtr)
  {
   struct defrule *thePtr;
#if MAC_XCD
#pragma unused(theEnv)
#endif

   for (thePtr = (struct defrule *) rulePtr;
        thePtr != NULL;
        thePtr = thePtr->disjunct)
     { thePtr->watchFiring = newState; }
  }

/*******************************************************************/
/* DefruleWatchAccess: Access function for setting the watch flags */
/*   associated with rules (activations and rule firings).         */
/*******************************************************************/
bool DefruleWatchAccess(
  void *theEnv,
  int code,
  bool newState,
  struct expr *argExprs)
  {
   if (code)
     return(ConstructSetWatchAccess(theEnv,DefruleData(theEnv)->DefruleConstruct,newState,argExprs,
                                    EnvGetDefruleWatchActivations,EnvSetDefruleWatchActivations));
   else
     return(ConstructSetWatchAccess(theEnv,DefruleData(theEnv)->DefruleConstruct,newState,argExprs,
                                    EnvGetDefruleWatchFirings,EnvSetDefruleWatchFirings));
  }

/*****************************************************************/
/* DefruleWatchPrint: Access routine for printing which defrules */
/*   have their watch flag set via the list-watch-items command. */
/*****************************************************************/
bool DefruleWatchPrint(
  void *theEnv,
  const char *logName,
  int code,
  struct expr *argExprs)
  {   
   if (code)
     return(ConstructPrintWatchAccess(theEnv,DefruleData(theEnv)->DefruleConstruct,logName,argExprs,
                                      EnvGetDefruleWatchActivations,EnvSetDefruleWatchActivations));
   else
     return(ConstructPrintWatchAccess(theEnv,DefruleData(theEnv)->DefruleConstruct,logName,argExprs,
                                      EnvGetDefruleWatchActivations,EnvSetDefruleWatchActivations));
  }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFTEMPLATE_CONSTRUCT */

