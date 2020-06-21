   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  07/02/18             */
   /*                                                     */
   /*         DEFFACTS BASIC COMMANDS HEADER FILE         */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements core commands for the deffacts        */
/*   construct such as clear, reset, save, undeffacts,       */
/*   ppdeffacts, list-deffacts, and get-deffacts-list.       */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Changed find construct functionality so that   */
/*            imported modules are search when locating a    */
/*            named construct.                               */
/*                                                           */
/*      6.40: Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
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
/*            Removed initial-fact support.                  */
/*                                                           */
/*            Pretty print functions accept optional logical */
/*            name argument.                                 */
/*                                                           */
/*************************************************************/

#include "setup.h"

#if DEFFACTS_CONSTRUCT

#include <stdio.h>
#include <string.h>

#include "ArgumentAccess.h"
#include "Construct.h"
#include "cstrccom.h"
#include "cstrcpsr.h"
#include "dffctdef.h"
#include "dffctpsr.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "Fact.h"
#include "MemoryAllocation.h"
#include "Multifield.h"
#include "Router.h"
#include "Scanner.h"
#include "tmpltdef.h"

#if BLOAD || BLOAD_AND_BSAVE
#include "dffctbin.h"
#endif
#include "dffctbsc.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    ResetDeffacts(Environment *,void *);
   static void                    SaveDeffacts(Environment *,Defmodule *,const char *,void *);
   static void                    ResetDeffactsAction(Environment *,ConstructHeader *,void *);

/***************************************************************/
/* DeffactsBasicCommands: Initializes basic deffacts commands. */
/***************************************************************/
void DeffactsBasicCommands(
  Environment *theEnv)
  {
   AddResetFunction(theEnv,"deffacts",ResetDeffacts,0,NULL);
   AddSaveFunction(theEnv,"deffacts",SaveDeffacts,10,NULL);

   AddUDF(theEnv,"get-deffacts-list","m",0,1,"y",GetDeffactsListFunction,"GetDeffactsListFunction",NULL);
   AddUDF(theEnv,"undeffacts","v",1,1,"y",UndeffactsCommand,"UndeffactsCommand",NULL);
   AddUDF(theEnv,"deffacts-module","y",1,1,"y",DeffactsModuleFunction,"DeffactsModuleFunction",NULL);

#if DEBUGGING_FUNCTIONS
   AddUDF(theEnv,"list-deffacts","v",0,1,"y",ListDeffactsCommand,"ListDeffactsCommand",NULL);
   AddUDF(theEnv,"ppdeffacts","vs",1,2,";y;ldsyn",PPDeffactsCommand,"PPDeffactsCommand",NULL);
#endif

#if (BLOAD || BLOAD_AND_BSAVE)
   DeffactsBinarySetup(theEnv);
#endif

  }

/**********************************************************/
/* ResetDeffacts: Deffacts reset routine for use with the */
/*   reset command. Asserts all of the facts contained in */
/*   deffacts constructs.                                 */
/**********************************************************/
static void ResetDeffacts(
  Environment *theEnv,
  void *context)
  {
   DoForAllConstructs(theEnv,
                      ResetDeffactsAction,
                      DeffactsData(theEnv)->DeffactsModuleIndex,
                      true,NULL);
  }

/*****************************************************/
/* ResetDeffactsAction: Action to be applied to each */
/*   deffacts construct during a reset command.      */
/*****************************************************/
static void ResetDeffactsAction(
  Environment *theEnv,
  ConstructHeader *theConstruct,
  void *buffer)
  {
#if MAC_XCD
#pragma unused(buffer)
#endif
   UDFValue returnValue;
   Deffacts *theDeffacts = (Deffacts *) theConstruct;

   if (theDeffacts->assertList == NULL) return;

   SetEvaluationError(theEnv,false);

   EvaluateExpression(theEnv,theDeffacts->assertList,&returnValue);
  }

/***************************************/
/* SaveDeffacts: Deffacts save routine */
/*   for use with the save command.    */
/***************************************/
static void SaveDeffacts(
  Environment *theEnv,
  Defmodule *theModule,
  const char *logicalName,
  void *context)
  {
   SaveConstruct(theEnv,theModule,logicalName,DeffactsData(theEnv)->DeffactsConstruct);
  }

/*******************************************/
/* UndeffactsCommand: H/L access routine   */
/*   for the undeffacts command.           */
/*******************************************/
void UndeffactsCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UndefconstructCommand(context,"undeffacts",DeffactsData(theEnv)->DeffactsConstruct);
  }

/*********************************/
/* Undeffacts: C access routine  */
/*   for the undeffacts command. */
/*********************************/
bool Undeffacts(
  Deffacts *theDeffacts,
  Environment *allEnv)
  {
   Environment *theEnv;
   
   if (theDeffacts == NULL)
     {
      theEnv = allEnv;
      return Undefconstruct(theEnv,NULL,DeffactsData(theEnv)->DeffactsConstruct);
     }
   else
     {
      theEnv = theDeffacts->header.env;
      return Undefconstruct(theEnv,&theDeffacts->header,DeffactsData(theEnv)->DeffactsConstruct);
     }
  }

/*************************************************/
/* GetDeffactsListFunction: H/L access routine   */
/*   for the get-deffacts-list function.         */
/*************************************************/
void GetDeffactsListFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   GetConstructListFunction(context,returnValue,DeffactsData(theEnv)->DeffactsConstruct);
  }

/*****************************************/
/* GetDeffactsList: C access routine     */
/*   for the get-deffacts-list function. */
/*****************************************/
void GetDeffactsList(
  Environment *theEnv,
  CLIPSValue *returnValue,
  Defmodule *theModule)
  {
   UDFValue result;
   
   GetConstructList(theEnv,&result,DeffactsData(theEnv)->DeffactsConstruct,theModule);
   NormalizeMultifield(theEnv,&result);
   returnValue->value = result.value;
  }

/************************************************/
/* DeffactsModuleFunction: H/L access routine   */
/*   for the deffacts-module function.          */
/************************************************/
void DeffactsModuleFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   returnValue->value = GetConstructModuleCommand(context,"deffacts-module",DeffactsData(theEnv)->DeffactsConstruct);
  }

#if DEBUGGING_FUNCTIONS

/*******************************************/
/* PPDeffactsCommand: H/L access routine   */
/*   for the ppdeffacts command.           */
/*******************************************/
void PPDeffactsCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   PPConstructCommand(context,"ppdeffacts",DeffactsData(theEnv)->DeffactsConstruct,returnValue);
  }

/************************************/
/* PPDeffacts: C access routine for */
/*   the ppdeffacts command.        */
/************************************/
bool PPDeffacts(
  Environment *theEnv,
  const char *deffactsName,
  const char *logicalName)
  {
   return(PPConstruct(theEnv,deffactsName,logicalName,DeffactsData(theEnv)->DeffactsConstruct));
  }

/*********************************************/
/* ListDeffactsCommand: H/L access routine   */
/*   for the list-deffacts command.          */
/*********************************************/
void ListDeffactsCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   ListConstructCommand(context,DeffactsData(theEnv)->DeffactsConstruct);
  }

/************************************/
/* ListDeffacts: C access routine   */
/*   for the list-deffacts command. */
/************************************/
void ListDeffacts(
  Environment *theEnv,
  const char *logicalName,
  Defmodule *theModule)
  {
   ListConstruct(theEnv,DeffactsData(theEnv)->DeffactsConstruct,logicalName,theModule);
  }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFFACTS_CONSTRUCT */


