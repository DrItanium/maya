   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  01/06/16             */
   /*                                                     */
   /*               ARGUMENT ACCESS MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides access routines for accessing arguments */
/*   passed to user or system functions defined using the    */
/*   DefineFunction protocol.                                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added IllegalLogicalNameMessage function.      */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Support for fact-address arguments.            */
/*                                                           */
/*      6.40: Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
/*                                                           */
/*            Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
/*                                                           */
/*************************************************************/

#include "setup.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "cstrnchk.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "factmngr.h"
#include "insfun.h"
#include "prntutil.h"
#include "router.h"
#include "sysdep.h"

#include "argacces.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    NonexistantError(void *,const char *,const char *,int);
   static void                    ExpectedTypeError3(void *,const char *,const char *,int,const char *);

/*******************************************************************/
/* EnvRtnLexeme: Access function to retrieve the nth argument from */
/*   a user or system function defined using the DefineFunction    */
/*   protocol. The argument retrieved must be a symbol, string, or */
/*   instance name, otherwise an error is generated. Only the      */
/*   value of the argument is returned (i.e. the string "a" would  */
/*   be returned for a, "a", and [a]).                             */
/*******************************************************************/
const char *EnvRtnLexeme(
  void *theEnv,
  int argumentPosition)
  {
   int count = 1;
   DATA_OBJECT result;
   struct expr *argPtr;

   /*=====================================================*/
   /* Find the appropriate argument in the argument list. */
   /*=====================================================*/

   for (argPtr = EvaluationData(theEnv)->CurrentExpression->argList;
        (argPtr != NULL) && (count < argumentPosition);
        argPtr = argPtr->nextArg)
     { count++; }

   if (argPtr == NULL)
     {
      NonexistantError(theEnv,"RtnLexeme",
                       ValueToString(ExpressionFunctionCallName(EvaluationData(theEnv)->CurrentExpression)),
                       argumentPosition);
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      return(NULL);
     }

   /*============================================*/
   /* Return the value of the nth argument if it */
   /* is a symbol, string, or instance name.     */
   /*============================================*/

   EvaluateExpression(theEnv,argPtr,&result);

   if ((result.type == SYMBOL) ||
#if OBJECT_SYSTEM
       (result.type == INSTANCE_NAME) ||
#endif
       (result.type == STRING))
     { return(ValueToString(result.value));}

   /*======================================================*/
   /* Generate an error if the argument is the wrong type. */
   /*======================================================*/

   ExpectedTypeError3(theEnv,"RtnLexeme",
                  ValueToString(ExpressionFunctionCallName(EvaluationData(theEnv)->CurrentExpression)),
                  argumentPosition,"symbol, string, or instance name");
   EnvSetHaltExecution(theEnv,true);
   EnvSetEvaluationError(theEnv,true);
   return(NULL);
  }

/*******************************************************************/
/* EnvRtnDouble: Access function to retrieve the nth argument from */
/*   a user or system function defined using the DefineFunction    */
/*   protocol. The argument retrieved must be a either a float or  */
/*   an integer (type conversion to a float is performed for       */
/*   integers), otherwise an error is generated. Only the value of */
/*   the argument is returned (i.e. the float 3.0 would be         */
/*   returned for 3.0 and 3).                                      */
/*******************************************************************/
double EnvRtnDouble(
  void *theEnv,
  int argumentPosition)
  {
   int count = 1;
   DATA_OBJECT result;
   struct expr *argPtr;

   /*=====================================================*/
   /* Find the appropriate argument in the argument list. */
   /*=====================================================*/

   for (argPtr = EvaluationData(theEnv)->CurrentExpression->argList;
        (argPtr != NULL) && (count < argumentPosition);
        argPtr = argPtr->nextArg)
     { count++; }

   if (argPtr == NULL)
     {
      NonexistantError(theEnv,"RtnDouble",
                       ValueToString(ExpressionFunctionCallName(EvaluationData(theEnv)->CurrentExpression)),
                       argumentPosition);
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      return(1.0);
     }

   /*======================================*/
   /* Return the value of the nth argument */
   /* if it is a float or integer.         */
   /*======================================*/

   EvaluateExpression(theEnv,argPtr,&result);

   if (result.type == FLOAT)
     { return(ValueToDouble(result.value)); }
   else if (result.type == INTEGER)
     { return((double) ValueToLong(result.value)); }

   /*======================================================*/
   /* Generate an error if the argument is the wrong type. */
   /*======================================================*/

   ExpectedTypeError3(theEnv,"RtnDouble",
                  ValueToString(ExpressionFunctionCallName(EvaluationData(theEnv)->CurrentExpression)),
                  argumentPosition,"number");
   EnvSetHaltExecution(theEnv,true);
   EnvSetEvaluationError(theEnv,true);
   return(1.0);
  }

/*****************************************************************/
/* EnvRtnLong: Access function to retrieve the nth argument from */
/*   a user or system function defined using the DefineFunction  */
/*   protocol. The argument retrieved must be a either a float   */
/*   or an integer (type conversion to an integer is performed   */
/*   for floats), otherwise an error is generated. Only the      */
/*   value of the argument is returned (i.e. the integer 4       */
/*   would be returned for 4.3 and 4).                           */
/*****************************************************************/
long long EnvRtnLong(
  void *theEnv,
  int argumentPosition)
  {
   int count = 1;
   DATA_OBJECT result;
   struct expr *argPtr;

   /*=====================================================*/
   /* Find the appropriate argument in the argument list. */
   /*=====================================================*/

   for (argPtr = EvaluationData(theEnv)->CurrentExpression->argList;
        (argPtr != NULL) && (count < argumentPosition);
        argPtr = argPtr->nextArg)
     { count++; }

   if (argPtr == NULL)
     {
      NonexistantError(theEnv,"RtnLong",
                       ValueToString(ExpressionFunctionCallName(EvaluationData(theEnv)->CurrentExpression)),
                       argumentPosition);
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      return(1L);
     }

   /*======================================*/
   /* Return the value of the nth argument */
   /* if it is a float or integer.         */
   /*======================================*/

   EvaluateExpression(theEnv,argPtr,&result);

   if (result.type == FLOAT)
     { return((long) ValueToDouble(result.value)); }
   else if (result.type == INTEGER)
     { return(ValueToLong(result.value)); }

   /*======================================================*/
   /* Generate an error if the argument is the wrong type. */
   /*======================================================*/

   ExpectedTypeError3(theEnv,"RtnLong",
                  ValueToString(ExpressionFunctionCallName(EvaluationData(theEnv)->CurrentExpression)),
                  argumentPosition,"number");
   EnvSetHaltExecution(theEnv,true);
   EnvSetEvaluationError(theEnv,true);
   return(1L);
  }

/********************************************************************/
/* EnvRtnUnknown: Access function to retrieve the nth argument from */
/*   a user or system function defined using the DefineFunction     */
/*   protocol. The argument retrieved can be of any type. The value */
/*   and type of the argument are returned in a DATA_OBJECT         */
/*   structure provided by the calling function.                    */
/********************************************************************/
DATA_OBJECT_PTR EnvRtnUnknown(
  void *theEnv,
  int argumentPosition,
  DATA_OBJECT_PTR returnValue)
  {
   int count = 1;
   struct expr *argPtr;

   /*=====================================================*/
   /* Find the appropriate argument in the argument list. */
   /*=====================================================*/

   for (argPtr = EvaluationData(theEnv)->CurrentExpression->argList;
        (argPtr != NULL) && (count < argumentPosition);
        argPtr = argPtr->nextArg)
     { count++; }

   if (argPtr == NULL)
     {
      returnValue->environment = theEnv;
      NonexistantError(theEnv,"RtnUnknown",
                       ValueToString(ExpressionFunctionCallName(EvaluationData(theEnv)->CurrentExpression)),
                       argumentPosition);
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      return(NULL);
     }

   /*=======================================*/
   /* Return the value of the nth argument. */
   /*=======================================*/

   EvaluateExpression(theEnv,argPtr,returnValue);
   return(returnValue);
  }

/***********************************************************/
/* EnvRtnArgCount: Returns the length of the argument list */
/*   for the function call currently being evaluated.      */
/***********************************************************/
int EnvRtnArgCount(
  void *theEnv)
  {
   int count = 0;
   struct expr *argPtr;

   for (argPtr = EvaluationData(theEnv)->CurrentExpression->argList;
        argPtr != NULL;
        argPtr = argPtr->nextArg)
     { count++; }

   return(count);
  }
  
/************************************************************************/
/* EnvArgCountCheck: Given the expected number of arguments, determines */
/*   if the function currently being evaluated has the correct number   */
/*   of arguments. Three types of argument checking are provided by     */
/*   this function: 1) The function has exactly the expected number of  */
/*   arguments; 2) The function has at least the expected number of     */
/*   arguments; 3) The function has at most the expected number of      */
/*   arguments. The number of arguments is returned if no error occurs, */
/*   otherwise -1 is returned.                                          */
/************************************************************************/
int EnvArgCountCheck(
  void *theEnv,
  const char *functionName,
  int countRelation,
  int expectedNumber)
  {
   int numberOfArguments;

   /*==============================================*/
   /* Get the number of arguments for the function */
   /* currently being evaluated.                   */
   /*==============================================*/

   numberOfArguments = EnvRtnArgCount(theEnv);

   /*=========================================================*/
   /* If the function satisfies expected number of arguments, */
   /* constraint, then return the number of arguments found.  */
   /*=========================================================*/

   if (countRelation == EXACTLY)
     { if (numberOfArguments == expectedNumber) return(numberOfArguments); }
   else if (countRelation == AT_LEAST)
     { if (numberOfArguments >= expectedNumber) return(numberOfArguments); }
   else if (countRelation == NO_MORE_THAN)
     { if (numberOfArguments <= expectedNumber) return(numberOfArguments); }

   /*================================================*/
   /* The correct number of arguments was not found. */
   /* Generate an error message and return -1.       */
   /*================================================*/

   ExpectedCountError(theEnv,functionName,countRelation,expectedNumber);

   EnvSetHaltExecution(theEnv,true);
   EnvSetEvaluationError(theEnv,true);

   return(-1);
  }

/****************************************************************/
/* EnvArgRangeCheck: Checks that the number of arguments passed */
/*   to a function falls within a specified minimum and maximum */
/*   range. The number of arguments passed to the function is   */
/*   returned if no error occurs, otherwise -1 is returned.     */
/****************************************************************/
int EnvArgRangeCheck(
  void *theEnv,
  const char *functionName,
  int min,
  int max)
  {
   int numberOfArguments;

   numberOfArguments = EnvRtnArgCount(theEnv);
   if ((numberOfArguments < min) || (numberOfArguments > max))
     {
      PrintErrorID(theEnv,"ARGACCES",1,false);
      EnvPrintRouter(theEnv,WERROR,"Function ");
      EnvPrintRouter(theEnv,WERROR,functionName);
      EnvPrintRouter(theEnv,WERROR," expected at least ");
      PrintLongInteger(theEnv,WERROR,(long) min);
      EnvPrintRouter(theEnv,WERROR," and no more than ");
      PrintLongInteger(theEnv,WERROR,(long) max);
      EnvPrintRouter(theEnv,WERROR," arguments.\n");
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      return(-1);
     }

   return(numberOfArguments);
  }

/*************************************************************/
/* EnvArgTypeCheck: Retrieves the nth argument passed to the */
/*   function call currently being evaluated and determines  */
/*   if it matches a specified type. Returns true if the     */
/*   argument was successfully retrieved and is of the       */
/*   appropriate type, otherwise returns false.              */
/*************************************************************/
bool EnvArgTypeCheck(
  void *theEnv,
  const char *functionName,
  int argumentPosition,
  int expectedType,
  DATA_OBJECT_PTR returnValue)
  {
   /*========================*/
   /* Retrieve the argument. */
   /*========================*/

   EnvRtnUnknown(theEnv,argumentPosition,returnValue);
   if (EvaluationData(theEnv)->EvaluationError) return(false);

   /*========================================*/
   /* If the argument's type exactly matches */
   /* the expected type, then return true.   */
   /*========================================*/

   if (returnValue->type == expectedType) return (true);

   /*=============================================================*/
   /* Some expected types encompass more than one primitive type. */
   /* If the argument's type matches one of the primitive types   */
   /* encompassed by the expected type, then return true.         */
   /*=============================================================*/

   if ((expectedType == INTEGER_OR_FLOAT) &&
       ((returnValue->type == INTEGER) || (returnValue->type == FLOAT)))
     { return(true); }

   if ((expectedType == SYMBOL_OR_STRING) &&
       ((returnValue->type == SYMBOL) || (returnValue->type == STRING)))
     { return(true); }

#if OBJECT_SYSTEM
   if (((expectedType == SYMBOL_OR_STRING) || (expectedType == SYMBOL)) &&
       (returnValue->type == INSTANCE_NAME))
     { return(true); }

   if ((expectedType == INSTANCE_NAME) &&
       ((returnValue->type == INSTANCE_NAME) || (returnValue->type == SYMBOL)))
     { return(true); }

   if ((expectedType == INSTANCE_OR_INSTANCE_NAME) &&
       ((returnValue->type == INSTANCE_ADDRESS) ||
        (returnValue->type == INSTANCE_NAME) ||
        (returnValue->type == SYMBOL)))
     { return(true); }
#endif

   /*===========================================================*/
   /* If the expected type is float and the argument's type is  */
   /* integer (or vice versa), then convert the argument's type */
   /* to match the expected type and then return true.          */
   /*===========================================================*/

   if ((returnValue->type == INTEGER) && (expectedType == FLOAT))
     {
      returnValue->type = FLOAT;
      returnValue->value = (void *) EnvAddDouble(theEnv,(double) ValueToLong(returnValue->value));
      return(true);
     }

   if ((returnValue->type == FLOAT) && (expectedType == INTEGER))
     {
      returnValue->type = INTEGER;
      returnValue->value = (void *) EnvAddLong(theEnv,(long long) ValueToDouble(returnValue->value));
      return(true);
     }

   /*=====================================================*/
   /* The argument's type didn't match the expected type. */
   /* Print an error message and return false.            */
   /*=====================================================*/

   if (expectedType == FLOAT) ExpectedTypeError1(theEnv,functionName,argumentPosition,"float");
   else if (expectedType == INTEGER) ExpectedTypeError1(theEnv,functionName,argumentPosition,"integer");
   else if (expectedType == SYMBOL) ExpectedTypeError1(theEnv,functionName,argumentPosition,"symbol");
   else if (expectedType == STRING) ExpectedTypeError1(theEnv,functionName,argumentPosition,"string");
   else if (expectedType == MULTIFIELD) ExpectedTypeError1(theEnv,functionName,argumentPosition,"multifield");
   else if (expectedType == INTEGER_OR_FLOAT)  ExpectedTypeError1(theEnv,functionName,argumentPosition,"integer or float");
   else if (expectedType == SYMBOL_OR_STRING) ExpectedTypeError1(theEnv,functionName,argumentPosition,"symbol or string");
   else if (expectedType == FACT_ADDRESS) ExpectedTypeError1(theEnv,functionName,argumentPosition,"fact address");
#if OBJECT_SYSTEM
   else if (expectedType == INSTANCE_NAME) ExpectedTypeError1(theEnv,functionName,argumentPosition,"instance name");
   else if (expectedType == INSTANCE_ADDRESS) ExpectedTypeError1(theEnv,functionName,argumentPosition,"instance address");
   else if (expectedType == INSTANCE_OR_INSTANCE_NAME) ExpectedTypeError1(theEnv,functionName,argumentPosition,"instance address or instance name");
#endif

   EnvSetHaltExecution(theEnv,true);
   EnvSetEvaluationError(theEnv,true);

   return(false);
  }

/******************************************************************/
/* GetNumericArgument: Evaluates an expression to yield a numeric */
/*  argument. This provides quicker retrieval than using some of  */
/*  the other argument access routines. The numeric argument is   */
/*  returned in a DATA_OBJECT supplied by the calling function.   */
/*  true is returned if a numeric argument was successfully       */
/*  retrieved, otherwise false is returned.                       */
/******************************************************************/
bool GetNumericArgument(
  void *theEnv,
  struct expr *theArgument,
  const char *functionName,
  DATA_OBJECT *result,
  bool convertToFloat,
  int whichArgument)
  {
   unsigned short theType;
   void *theValue;

   /*==================================================================*/
   /* Evaluate the expression (don't bother calling EvaluateExpression */
   /* if the type is float or integer).                                */
   /*==================================================================*/

   switch(theArgument->type)
     {
      case FLOAT:
      case INTEGER:
        theType = theArgument->type;
        theValue = theArgument->value;
        break;

      default:
        EvaluateExpression(theEnv,theArgument,result);
        theType = result->type;
        theValue = result->value;
        break;
     }

   /*==========================================*/
   /* If the argument is not float or integer, */
   /* print an error message and return false. */
   /*==========================================*/

   if ((theType != FLOAT) && (theType != INTEGER))
     {
      ExpectedTypeError1(theEnv,functionName,whichArgument,"integer or float");
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      result->type = INTEGER;
      result->value = (void *) EnvAddLong(theEnv,0LL);
      return(false);
     }

   /*==========================================================*/
   /* If the argument is an integer and the "convert to float" */
   /* flag is true, then convert the integer to a float.       */
   /*==========================================================*/

   if ((convertToFloat) && (theType == INTEGER))
     {
      theType = FLOAT;
      theValue = (void *) EnvAddDouble(theEnv,(double) ValueToLong(theValue));
     }

   /*============================================================*/
   /* The numeric argument was successfully retrieved. Store the */
   /* argument in the user supplied DATA_OBJECT and return true. */
   /*============================================================*/

   result->type = theType;
   result->value = theValue;

   return(true);
  }

/*********************************************************************/
/* GetLogicalName: Retrieves the nth argument passed to the function */
/*   call currently being evaluated and determines if it is a valid  */
/*   logical name. If valid, the logical name is returned, otherwise */
/*   NULL is returned.                                               */
/*********************************************************************/
const char *GetLogicalName(
  UDFContext *context,
  const char *defaultLogicalName)
  {
   const char *logicalName;
   CLIPSValue theArg;
   void *theEnv = UDFContextEnvironment(context);

   if (! UDFNextArgument(context,ANY_TYPE,&theArg))
     { return(NULL); }

   if (CVIsType(&theArg,LEXEME_TYPES) ||
       CVIsType(&theArg,INSTANCE_NAME_TYPE))
     {
      logicalName = CVToString(&theArg);
      if ((strcmp(logicalName,"t") == 0) || (strcmp(logicalName,"T") == 0))
        { logicalName = defaultLogicalName; }
     }
   else if (CVIsType(&theArg,FLOAT_TYPE))
     {
      logicalName = ValueToString(EnvAddSymbol(theEnv,FloatToString(theEnv,CVToFloat(&theArg))));
     }
   else if (CVIsType(&theArg,INTEGER_TYPE))
     {
      logicalName = ValueToString(EnvAddSymbol(theEnv,LongIntegerToString(theEnv,CVToInteger(&theArg))));
     }
   else
     { logicalName = NULL; }

   return(logicalName);
  }

/************************************************************/
/* GetFileName: Retrieves the nth argument passed to the    */
/*   function call currently being evaluated and determines */
/*   if it is a valid file name. If valid, the file name is */
/*   returned, otherwise NULL is returned.                  */
/************************************************************/
const char *GetFileName(
  UDFContext *context)
  {
   CLIPSValue theArg;
   
   if (! UDFNextArgument(context,LEXEME_TYPES,&theArg))
     { return(NULL); }

   return(CVToString(&theArg));
  }

/******************************************************************/
/* OpenErrorMessage: Generalized error message for opening files. */
/******************************************************************/
void OpenErrorMessage(
  void *theEnv,
  const char *functionName,
  const char *fileName)
  {
   PrintErrorID(theEnv,"ARGACCES",2,false);
   EnvPrintRouter(theEnv,WERROR,"Function ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR," was unable to open file ");
   EnvPrintRouter(theEnv,WERROR,fileName);
   EnvPrintRouter(theEnv,WERROR,".\n");
  }

/************************************************************/
/* GetModuleName: Retrieves the nth argument passed to the  */
/*   function call currently being evaluated and determines */
/*   if it is a valid module name. If valid, the module     */
/*   name is returned or NULL is returned to indicate all   */
/*   modules.                                               */
/************************************************************/
struct defmodule *GetModuleName(
  void *theEnv,
  const char *functionName,
  int whichArgument,
  bool *error)
  {
   DATA_OBJECT result;
   struct defmodule *theModule;

   *error = false;

   /*========================*/
   /* Retrieve the argument. */
   /*========================*/

   EnvRtnUnknown(theEnv,whichArgument,&result);

   /*=================================*/
   /* A module name must be a symbol. */
   /*=================================*/

   if (GetType(result) != SYMBOL)
     {
      ExpectedTypeError1(theEnv,functionName,whichArgument,"defmodule name");
      *error = true;
      return(NULL);
     }

   /*=======================================*/
   /* Check to see that the symbol actually */
   /* corresponds to a defined module.      */
   /*=======================================*/

   if ((theModule = (struct defmodule *) EnvFindDefmodule(theEnv,DOToString(result))) == NULL)
     {
      if (strcmp("*",DOToString(result)) != 0)
        {
         ExpectedTypeError1(theEnv,functionName,whichArgument,"defmodule name");
         *error = true;
        }
      return(NULL);
     }

   /*=================================*/
   /* Return a pointer to the module. */
   /*=================================*/

   return(theModule);
  }

/****************************************************************/
/* GetConstructName: Retrieves the 1st argument passed to the   */
/*   function call currently being evaluated and determines if  */
/*   it is a valid name for a construct. Also checks that the   */
/*   function is only passed a single argument. This routine    */
/*   is used by functions such as ppdeftemplate, undefrule,     */
/*   etc... to retrieve the construct name on which to operate. */
/****************************************************************/
const char *GetConstructName(
  UDFContext *context,
  const char *functionName,
  const char *constructType)
  {
   CLIPSValue result;

   if (! UDFFirstArgument(context,ANY_TYPE,&result))
     { return(NULL); }

   if (! CVIsType(&result,SYMBOL_TYPE))
     {
      UDFInvalidArgumentMessage(context,constructType);
      return(NULL);
     }

   return(CVToString(&result));
  }

/**************************************************************************/
/* NonexistantError: Prints the error message for a nonexistant argument. */
/**************************************************************************/
static void NonexistantError(
  void *theEnv,
  const char *accessFunction,
  const char *functionName,
  int argumentPosition)
  {
   PrintErrorID(theEnv,"ARGACCES",3,false);
   EnvPrintRouter(theEnv,WERROR,"Function ");
   EnvPrintRouter(theEnv,WERROR,accessFunction);
   EnvPrintRouter(theEnv,WERROR," received a request from function ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR," for argument #");
   PrintLongInteger(theEnv,WERROR,(long int) argumentPosition);
   EnvPrintRouter(theEnv,WERROR," which is non-existent\n");
  }

/*********************************************************/
/* ExpectedCountError: Prints the error message for an   */
/*   incorrect number of arguments passed to a function. */
/*********************************************************/
void ExpectedCountError(
  void *theEnv,
  const char *functionName,
  int countRelation,
  int expectedNumber)
  {
   PrintErrorID(theEnv,"ARGACCES",4,false);
   EnvPrintRouter(theEnv,WERROR,"Function ");
   EnvPrintRouter(theEnv,WERROR,functionName);

   if (countRelation == EXACTLY)
     { EnvPrintRouter(theEnv,WERROR," expected exactly "); }
   else if (countRelation == AT_LEAST)
     { EnvPrintRouter(theEnv,WERROR," expected at least "); }
   else if (countRelation == NO_MORE_THAN)
     { EnvPrintRouter(theEnv,WERROR," expected no more than "); }
   else
     { EnvPrintRouter(theEnv,WERROR," generated an illegal argument check for "); }

   PrintLongInteger(theEnv,WERROR,(long int) expectedNumber);
   EnvPrintRouter(theEnv,WERROR," argument(s)\n");
  }

/*************************************************************/
/*  NAME         : CheckFunctionArgCount                     */
/*  DESCRIPTION  : Checks the number of arguments against    */
/*                 the system function restriction list      */
/*  INPUTS       : 1) Name of the calling function           */
/*                 2) The restriction list can be NULL       */
/*                 3) The number of arguments                */
/*  RETURNS      : true if OK, false otherwise               */
/*  SIDE EFFECTS : EvaluationError set on errrors            */
/*  NOTES        : Used to check generic function implicit   */
/*                 method (system function) calls and system */
/*                 function calls which have the sequence    */
/*                 expansion operator in their argument list */
/*************************************************************/
bool CheckFunctionArgCount(
  void *theEnv,
  struct FunctionDefinition *func,
  int argumentCount)
  {
   int minArguments, maxArguments;
   const char *functionName;
   const char *restrictions;
   char theChar[2];

   theChar[0] = '0';
   theChar[1] = EOS;

   functionName = func->callFunctionName->contents;
   if (func->restrictions == NULL) restrictions = NULL;
   else restrictions = func->restrictions->contents;
     
   /*=====================================================*/
   /* If there are no restrictions, then there is no need */
   /* to check for the correct number of arguments.       */
   /*=====================================================*/

   if (func->returnValueType !='z')
     { if (restrictions == NULL) return(true); }

   /*===========================================*/
   /* Determine the minimum number of arguments */
   /* required by the function.                 */
   /*===========================================*/

   if (func->returnValueType !='z')
     {
      if (isdigit(restrictions[0]))
        {
         theChar[0] = restrictions[0];
         minArguments = atoi(theChar);
        }
      else
        { minArguments = UNBOUNDED; }
     }
   else
     { minArguments = func->minArgs; }
   
   /*===========================================*/
   /* Determine the maximum number of arguments */
   /* required by the function.                 */
   /*===========================================*/

   if (func->returnValueType !='z')
     {
      if (isdigit(restrictions[1]))
        {
         theChar[0] = restrictions[1];
         maxArguments = atoi(theChar);
        }
      else
        { maxArguments = UNBOUNDED; }
     }
   else
     { maxArguments = func->maxArgs; }

   /*=====================================*/
   /* If the function has no restrictions */
   /* on function arguments, return true. */
   /*=====================================*/

   if ((minArguments == UNBOUNDED) && (maxArguments == UNBOUNDED))
     { return(true); }

   /*==============================================*/
   /* If the function expects exactly N arguments, */
   /* then check to see if there are N arguments.  */
   /*==============================================*/

   if (minArguments == maxArguments)
     {
      if (argumentCount != minArguments)
        {
         ExpectedCountError(theEnv,functionName,EXACTLY,minArguments);
         EnvSetEvaluationError(theEnv,true);
         return(false);
        }
      return(true);
     }

   /*==================================*/
   /* Check to see if there were fewer */
   /* arguments passed than expected.  */
   /*==================================*/

   if (argumentCount < minArguments)
     {
      ExpectedCountError(theEnv,functionName,AT_LEAST,minArguments);
      EnvSetEvaluationError(theEnv,true);
      return(false);
     }

   /*=================================*/
   /* Check to see if there were more */
   /* arguments passed than expected. */
   /*=================================*/

   if ((maxArguments != UNBOUNDED) && (argumentCount > maxArguments))
     {
      ExpectedCountError(theEnv,functionName,NO_MORE_THAN,maxArguments);
      EnvSetEvaluationError(theEnv,true);
      return(false);
     }

   /*===============================*/
   /* The number of arguments falls */
   /* within the expected range.    */
   /*===============================*/

   return(true);
  }

/*******************************************************************/
/* ExpectedTypeError0: Prints the error message for the wrong type */
/*   of argument passed to a user or system defined function.      */
/*******************************************************************/
void ExpectedTypeError0(
  void *theEnv,
  const char *functionName,
  int whichArg)
  {
   PrintErrorID(theEnv,"ARGACCES",5,false);
   EnvPrintRouter(theEnv,WERROR,"Function ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR," expected argument #");
   PrintLongInteger(theEnv,WERROR,(long int) whichArg);
   EnvPrintRouter(theEnv,WERROR," to be of type ");
  }

/*******************************************************************/
/* ExpectedTypeError1: Prints the error message for the wrong type */
/*   of argument passed to a user or system defined function. The  */
/*   expected type is passed as a string to this function.         */
/*******************************************************************/
void ExpectedTypeError1(
  void *theEnv,
  const char *functionName,
  int whichArg,
  const char *expectedType)
  {
   ExpectedTypeError0(theEnv,functionName,whichArg);
   EnvPrintRouter(theEnv,WERROR,expectedType);
   EnvPrintRouter(theEnv,WERROR,"\n");
  }

/**************************************************************/
/* ExpectedTypeError2: Prints the error message for the wrong */
/*   type of argument passed to a user or system defined      */
/*   function. The expected type is derived by examining the  */
/*   function's argument restriction list.                    */
/**************************************************************/
void ExpectedTypeError2(
  void *theEnv,
  const char *functionName,
  int whichArg)
  {
   struct FunctionDefinition *theFunction;
   const char *theType;

   theFunction = FindFunction(theEnv,functionName);

   if (theFunction == NULL) return;

   theType = GetArgumentTypeName(GetNthRestriction(theFunction,whichArg));

   ExpectedTypeError1(theEnv,functionName,whichArg,theType);
  }

/*******************************************************************/
/* ExpectedTypeError3: Prints the error message for the wrong type */
/*   of argument passed to a user or system defined function when  */
/*   the argument was requested by calling RtnLexeme, RtnLong, or  */
/*   RtnDouble.                                                    */
/*******************************************************************/
static void ExpectedTypeError3(
  void *theEnv,
  const char *accessFunction,
  const char *functionName,
  int argumentPosition,
  const char *type)
  {
   PrintErrorID(theEnv,"ARGACCES",6,false);
   EnvPrintRouter(theEnv,WERROR,"Function ");
   EnvPrintRouter(theEnv,WERROR,accessFunction);
   EnvPrintRouter(theEnv,WERROR," received a request from function ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR," for argument #");
   PrintLongInteger(theEnv,WERROR,(long int) argumentPosition);
   EnvPrintRouter(theEnv,WERROR," which is not of type ");
   EnvPrintRouter(theEnv,WERROR,type);
   EnvPrintRouter(theEnv,WERROR,"\n");
  }

/***************************************************/
/* GetFactOrInstanceArgument: Utility routine for  */
/*   retrieving a fact or instance argument        */
/***************************************************/
void *GetFactOrInstanceArgument(
  void *theEnv,
  int thePosition,
  DATA_OBJECT *item,
  const char *functionName)
  {
#if DEFTEMPLATE_CONSTRUCT || OBJECT_SYSTEM
   void *ptr;
#endif

   /*==============================*/
   /* Retrieve the first argument. */
   /*==============================*/

   EnvRtnUnknown(theEnv,thePosition,item);

   /*==================================================*/
   /* Fact and instance addresses are valid arguments. */
   /*==================================================*/

   if ((GetpType(item) == FACT_ADDRESS) ||
       (GetpType(item) == INSTANCE_ADDRESS))
     { return(GetpValue(item)); }

   /*==================================================*/
   /* An integer is a valid argument if it corresponds */
   /* to the fact index of an existing fact.           */
   /*==================================================*/

#if DEFTEMPLATE_CONSTRUCT
   else if (GetpType(item) == INTEGER)
     {
      if ((ptr = (void *) FindIndexedFact(theEnv,DOPToLong(item))) == NULL)
        {
         char tempBuffer[20];
         gensprintf(tempBuffer,"f-%lld",DOPToLong(item));
         CantFindItemErrorMessage(theEnv,"fact",tempBuffer);
        }
      return(ptr);
     }
#endif

   /*================================================*/
   /* Instance names and symbols are valid arguments */
   /* if they correspond to an existing instance.    */
   /*================================================*/

#if OBJECT_SYSTEM
   else if ((GetpType(item) == INSTANCE_NAME) || (GetpType(item) == SYMBOL))
     {
      if ((ptr = (void *) FindInstanceBySymbol(theEnv,(SYMBOL_HN *) GetpValue(item))) == NULL)
        {
         CantFindItemErrorMessage(theEnv,"instance",ValueToString(GetpValue(item)));
        }
      return(ptr);
     }
#endif

   /*========================================*/
   /* Any other type is an invalid argument. */
   /*========================================*/

   ExpectedTypeError2(theEnv,functionName,thePosition);
   return(NULL);
  }

/****************************************************/
/* IllegalLogicalNameMessage: Generic error message */
/*   for illegal logical names.                     */
/****************************************************/
void IllegalLogicalNameMessage(
  void *theEnv,
  const char *theFunction)
  {
   PrintErrorID(theEnv,"IOFUN",1,false);
   EnvPrintRouter(theEnv,WERROR,"Illegal logical name used for ");
   EnvPrintRouter(theEnv,WERROR,theFunction);
   EnvPrintRouter(theEnv,WERROR," function.\n");
  }


