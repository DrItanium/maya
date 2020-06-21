   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  10/18/16             */
   /*                                                     */
   /*            FACT RHS PATTERN PARSER MODULE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a number of routines for parsing fact   */
/*   patterns typically found on the RHS of a rule (such as  */
/*   the assert command). Also contains some functions for   */
/*   parsing RHS slot values (used by functions such as      */
/*   assert, modify, and duplicate).                         */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Chris Culbert                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Added code to prevent a clear command from     */
/*            being executed during fact assertions via      */
/*            Increment/DecrementClearReadyLocks API.        */
/*                                                           */
/*            Added code to keep track of pointers to        */
/*            constructs that are contained externally to    */
/*            to constructs, DanglingConstructs.             */
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
/*            UDF redesign.                                  */
/*                                                           */
/*            Eval support for run time and bload only.      */
/*                                                           */
/*************************************************************/

#include <stdio.h>
#include <string.h>

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#if BLOAD_AND_BSAVE || BLOAD
#include "bload.h"
#endif

#include "constant.h"
#include "cstrcpsr.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "modulutl.h"
#include "modulpsr.h"
#include "pattern.h"
#include "pprint.h"
#include "prntutil.h"
#include "router.h"
#include "strngrtr.h"
#include "tmpltpsr.h"
#include "tmpltrhs.h"
#include "tmpltutl.h"

#include "factrhs.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD || BLOAD_AND_BSAVE
   static void                       NoSuchTemplateError(Environment *,const char *);
#endif

/**********************************************************************/
/* BuildRHSAssert: Parses zero or more RHS fact patterns (the format  */
/*   which is used by the assert command and the deffacts construct). */
/*   Each of the RHS patterns is attached to an assert command and if */
/*   there is more than one assert command, then a progn command is   */
/*   wrapped around all of the assert commands.                       */
/**********************************************************************/
struct expr *BuildRHSAssert(
  Environment *theEnv,
  const char *logicalName,
  struct token *theToken,
  bool *error,
  bool atLeastOne,
  bool readFirstParen,
  const char *whereParsed)
  {
   struct expr *lastOne, *nextOne, *assertList, *stub;

   *error = false;

   /*===============================================================*/
   /* If the first parenthesis of the RHS fact pattern has not been */
   /* read yet, then get the next token. If a right parenthesis is  */
   /* encountered then exit (however, set the error return value if */
   /* at least one fact was expected).                              */
   /*===============================================================*/

   if (readFirstParen == false)
     {
      if (theToken->tknType == RIGHT_PARENTHESIS_TOKEN)
        {
         if (atLeastOne)
           {
            *error = true;
            SyntaxErrorMessage(theEnv,whereParsed);
           }
         return NULL;
        }
     }

   /*================================================*/
   /* Parse the facts until no more are encountered. */
   /*================================================*/

   lastOne = assertList = NULL;
   while ((nextOne = GetRHSPattern(theEnv,logicalName,theToken,
                                   error,false,readFirstParen,
                                   true,RIGHT_PARENTHESIS_TOKEN)) != NULL)
     {
      PPCRAndIndent(theEnv);

      stub = GenConstant(theEnv,FCALL,FindFunction(theEnv,"assert"));
      stub->argList = nextOne;
      nextOne = stub;

      if (lastOne == NULL)
        { assertList = nextOne; }
      else
        { lastOne->nextArg = nextOne; }
      lastOne = nextOne;

      readFirstParen = true;
     }

   /*======================================================*/
   /* If an error was detected while parsing, then return. */
   /*======================================================*/

   if (*error)
     {
      ReturnExpression(theEnv,assertList);
      return NULL;
     }

   /*======================================*/
   /* Fix the pretty print representation. */
   /*======================================*/

   if (theToken->tknType == RIGHT_PARENTHESIS_TOKEN)
     {
      PPBackup(theEnv);
      PPBackup(theEnv);
      SavePPBuffer(theEnv,")");
     }

   /*==============================================================*/
   /* If no facts are being asserted then return NULL. In addition */
   /* if at least one fact was required, then signal an error.     */
   /*==============================================================*/

   if (assertList == NULL)
     {
      if (atLeastOne)
        {
         *error = true;
         SyntaxErrorMessage(theEnv,whereParsed);
        }

      return NULL;
     }

   /*===============================================*/
   /* If more than one fact is being asserted, then */
   /* wrap the assert commands within a progn call. */
   /*===============================================*/

   if (assertList->nextArg != NULL)
     {
      stub = GenConstant(theEnv,FCALL,FindFunction(theEnv,"progn"));
      stub->argList = assertList;
      assertList = stub;
     }

   /*==========================================================*/
   /* Return the expression for asserting the specified facts. */
   /*==========================================================*/

   return assertList;
  }

/***************************************************************/
/* GetRHSPattern: Parses a single RHS fact pattern. The return */
/*   value is the fact just parsed (or NULL if the delimiter   */
/*   for no more facts is the first token parsed). If an error */
/*   occurs, then the error flag passed as an argument is set. */
/***************************************************************/
struct expr *GetRHSPattern(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  bool *error,
  bool constantsOnly,
  bool readFirstParen,
  bool checkFirstParen,
  TokenType endType)
  {
   struct expr *lastOne = NULL;
   struct expr *nextOne, *firstOne, *argHead = NULL;
   bool printError;
   unsigned int count;
   Deftemplate *theDeftemplate;
   CLIPSLexeme *templateName;
   const char *nullBitMap = "\0";

   /*=================================================*/
   /* Get the opening parenthesis of the RHS pattern. */
   /*=================================================*/

   *error = false;

   if (readFirstParen) GetToken(theEnv,readSource,tempToken);

   if (checkFirstParen)
     {
      if (tempToken->tknType == endType) return NULL;

      if (tempToken->tknType != LEFT_PARENTHESIS_TOKEN)
        {
         SyntaxErrorMessage(theEnv,"RHS patterns");
         *error = true;
         return NULL;
        }
     }

   /*======================================================*/
   /* The first field of an asserted fact must be a symbol */
   /* (but not = or : which have special significance).    */
   /*======================================================*/

   GetToken(theEnv,readSource,tempToken);
   if (tempToken->tknType != SYMBOL_TOKEN)
     {
      SyntaxErrorMessage(theEnv,"first field of a RHS pattern");
      *error = true;
      return NULL;
     }
   else if ((strcmp(tempToken->lexemeValue->contents,"=") == 0) ||
            (strcmp(tempToken->lexemeValue->contents,":") == 0))
     {
      SyntaxErrorMessage(theEnv,"first field of a RHS pattern");
      *error = true;
      return NULL;
     }

   /*=========================================================*/
   /* Check to see if the relation name is a reserved symbol. */
   /*=========================================================*/

   templateName = (CLIPSLexeme *) tempToken->value;

   if (ReservedPatternSymbol(theEnv,templateName->contents,NULL))
     {
      ReservedPatternSymbolErrorMsg(theEnv,templateName->contents,"a relation name");
      *error = true;
      return NULL;
     }

   /*============================================================*/
   /* A module separator in the name is illegal in this context. */
   /*============================================================*/

   if (FindModuleSeparator(templateName->contents))
     {
      IllegalModuleSpecifierMessage(theEnv);

      *error = true;
      return NULL;
     }

   /*=============================================================*/
   /* Determine if there is an associated deftemplate. If so, let */
   /* the deftemplate parsing functions parse the RHS pattern and */
   /* then return the fact pattern that was parsed.               */
   /*=============================================================*/

   theDeftemplate = (Deftemplate *)
                    FindImportedConstruct(theEnv,"deftemplate",NULL,templateName->contents,
                                          &count,true,NULL);

   if (count > 1)
     {
      AmbiguousReferenceErrorMessage(theEnv,"deftemplate",templateName->contents);
      *error = true;
      return NULL;
     }

   /*======================================================*/
   /* If no deftemplate exists with the specified relation */
   /* name, then create an implied deftemplate.            */
   /*======================================================*/

   if (theDeftemplate == NULL)
     {
#if BLOAD || BLOAD_AND_BSAVE
      if ((Bloaded(theEnv)) && (! ConstructData(theEnv)->CheckSyntaxMode))
        {
         NoSuchTemplateError(theEnv,templateName->contents);
         *error = true;
         return NULL;
        }
#endif
#if DEFMODULE_CONSTRUCT
      if (FindImportExportConflict(theEnv,"deftemplate",GetCurrentModule(theEnv),templateName->contents))
        {
         ImportExportConflictMessage(theEnv,"implied deftemplate",templateName->contents,NULL,NULL);
         *error = true;
         return NULL;
        }
#endif
      if (! ConstructData(theEnv)->CheckSyntaxMode)
        { theDeftemplate = CreateImpliedDeftemplate(theEnv,templateName,true); }
     }

   /*=========================================*/
   /* If an explicit deftemplate exists, then */
   /* parse the fact as a deftemplate fact.   */
   /*=========================================*/

   if ((theDeftemplate != NULL) && (theDeftemplate->implied == false))
     {
      firstOne = GenConstant(theEnv,DEFTEMPLATE_PTR,theDeftemplate);
      firstOne->nextArg = ParseAssertTemplate(theEnv,readSource,tempToken,
                                              error,endType,
                                              constantsOnly,theDeftemplate);

      if (! ConstructData(theEnv)->ParsingConstruct)
        { ConstructData(theEnv)->DanglingConstructs++; }

      if (*error)
        {
         ReturnExpression(theEnv,firstOne);
         firstOne = NULL;
        }

      return(firstOne);
     }

   /*========================================*/
   /* Parse the fact as an ordered RHS fact. */
   /*========================================*/

   firstOne = GenConstant(theEnv,DEFTEMPLATE_PTR,theDeftemplate);

   if (! ConstructData(theEnv)->ParsingConstruct)
     { ConstructData(theEnv)->DanglingConstructs++; }

   SavePPBuffer(theEnv," ");

   while ((nextOne = GetAssertArgument(theEnv,readSource,tempToken,
                                        error,endType,constantsOnly,&printError)) != NULL)
     {
      if (argHead == NULL) argHead = nextOne;
      else lastOne->nextArg = nextOne;
      lastOne = nextOne;
      SavePPBuffer(theEnv," ");
     }

   /*===========================================================*/
   /* If an error occurred, set the error flag and return NULL. */
   /*===========================================================*/

   if (*error)
     {
      if (printError) SyntaxErrorMessage(theEnv,"RHS patterns");
      ReturnExpression(theEnv,firstOne);
      ReturnExpression(theEnv,argHead);
      return NULL;
     }

   /*=====================================*/
   /* Fix the pretty print representation */
   /* of the RHS ordered fact.            */
   /*=====================================*/

   PPBackup(theEnv);
   PPBackup(theEnv);
   SavePPBuffer(theEnv,tempToken->printForm);

   /*==========================================================*/
   /* Ordered fact assertions are processed by stuffing all of */
   /* the fact's proposition (except the relation name) into a */
   /* single multifield slot.                                  */
   /*==========================================================*/

   firstOne->nextArg = GenConstant(theEnv,FACT_STORE_MULTIFIELD,AddBitMap(theEnv,(void *) nullBitMap,1));
   firstOne->nextArg->argList = argHead;

   /*==============================*/
   /* Return the RHS ordered fact. */
   /*==============================*/

   return(firstOne);
  }

/********************************************************************/
/* GetAssertArgument: Parses a single RHS slot value and returns an */
/*   expression representing the value. When parsing a deftemplate  */
/*   slot, the slot name has already been parsed when this function */
/*   is called. NULL is returned if a slot or fact delimiter is     */
/*   encountered. In the event of a parse error, the error flag     */
/*   passed as an argument is set.                                  */
/********************************************************************/
struct expr *GetAssertArgument(
  Environment *theEnv,
  const char *logicalName,
  struct token *theToken,
  bool *error,
  TokenType endType,
  bool constantsOnly,
  bool *printError)
  {
   struct expr *nextField;

   /*=================================================*/
   /* Read in the first token of the slot's value. If */
   /* the end delimiter is encountered, then return.  */
   /*=================================================*/

   *printError = true;
   GetToken(theEnv,logicalName,theToken);
   if (theToken->tknType == endType) return NULL;

   /*=============================================================*/
   /* If an equal sign of left parenthesis was parsed, then parse */
   /* a function which is to be evaluated to determine the slot's */
   /* value. The equal sign corresponds to the return value       */
   /* constraint which can be used in LHS fact patterns. The      */
   /* equal sign is no longer necessary on either the LHS or RHS  */
   /* of a rule to indicate that a function is being evaluated to */
   /* determine its value either for assignment or pattern        */
   /* matching.                                                   */
   /*=============================================================*/

   if ((theToken->tknType == SYMBOL_TOKEN) ?
       (strcmp(theToken->lexemeValue->contents,"=") == 0) :
       (theToken->tknType == LEFT_PARENTHESIS_TOKEN))
     {
      if (constantsOnly)
        {
         *error = true;
         return NULL;
        }

      if (theToken->tknType == LEFT_PARENTHESIS_TOKEN) nextField = Function1Parse(theEnv,logicalName);
      else nextField = Function0Parse(theEnv,logicalName);
      if (nextField == NULL)
        {
         *printError = false;
         *error = true;
        }
      else
        {
         theToken->tknType= RIGHT_PARENTHESIS_TOKEN;
         theToken->value = CreateString(theEnv,")");
         theToken->printForm = ")";
        }

      return(nextField);
     }

   /*==================================================*/
   /* Constants are always allowed as RHS slot values. */
   /*==================================================*/

   if ((theToken->tknType == SYMBOL_TOKEN) || (theToken->tknType == STRING_TOKEN) ||
#if OBJECT_SYSTEM
           (theToken->tknType == INSTANCE_NAME_TOKEN) ||
#endif
           (theToken->tknType == FLOAT_TOKEN) || (theToken->tknType == INTEGER_TOKEN))
     { return(GenConstant(theEnv,TokenTypeToType(theToken->tknType),theToken->value)); }

   /*========================================*/
   /* Variables are also allowed as RHS slot */
   /* values under some circumstances.       */
   /*========================================*/

   if ((theToken->tknType == SF_VARIABLE_TOKEN) ||
#if DEFGLOBAL_CONSTRUCT
            (theToken->tknType == GBL_VARIABLE_TOKEN) ||
            (theToken->tknType == MF_GBL_VARIABLE_TOKEN) ||
#endif
            (theToken->tknType == MF_VARIABLE_TOKEN))
     {
      if (constantsOnly)
        {
         *error = true;
         return NULL;
        }

      return(GenConstant(theEnv,TokenTypeToType(theToken->tknType),theToken->value));
     }

   /*==========================================================*/
   /* If none of the other cases have been satisfied, then the */
   /* token parsed is not appropriate for a RHS slot value.    */
   /*==========================================================*/

   *error = true;
   return NULL;
  }

/****************************************************/
/* StringToFact: Converts the string representation */
/*   of a fact to a fact data structure.            */
/****************************************************/
Fact *StringToFact(
  Environment *theEnv,
  const char *str)
  {
   struct token theToken;
   Fact *factPtr;
   unsigned numberOfFields = 0, whichField;
   struct expr *assertArgs, *tempPtr;
   bool error = false;
   UDFValue theResult;

   /*=========================================*/
   /* Open a string router and parse the fact */
   /* using the router as an input source.    */
   /*=========================================*/

   SetEvaluationError(theEnv,false);

   OpenStringSource(theEnv,"assert_str",str,0);

   assertArgs = GetRHSPattern(theEnv,"assert_str",&theToken,
                              &error,false,true,
                              true,RIGHT_PARENTHESIS_TOKEN);

   CloseStringSource(theEnv,"assert_str");

   /*===========================================*/
   /* Check for errors or the use of variables. */
   /*===========================================*/

   if ((assertArgs == NULL) && (! error))
     {
      SyntaxErrorMessage(theEnv,"RHS patterns");
      ReturnExpression(theEnv,assertArgs);
      return NULL;
     }

   if (error)
     {
      ReturnExpression(theEnv,assertArgs);
      return NULL;
     }

   if (ExpressionContainsVariables(assertArgs,false))
     {
      LocalVariableErrorMessage(theEnv,"the assert-string function");
      SetEvaluationError(theEnv,true);
      ReturnExpression(theEnv,assertArgs);
      return NULL;
     }

   /*=======================================================*/
   /* Count the number of fields needed for the fact and    */
   /* create a fact data structure of the appropriate size. */
   /*=======================================================*/

   for (tempPtr = assertArgs->nextArg; tempPtr != NULL; tempPtr = tempPtr->nextArg)
     { numberOfFields++; }

   factPtr = CreateFactBySize(theEnv,numberOfFields);
   factPtr->whichDeftemplate = (Deftemplate *) assertArgs->value;

   /*=============================================*/
   /* Copy the fields to the fact data structure. */
   /*=============================================*/

   IncrementClearReadyLocks(theEnv);
   ExpressionInstall(theEnv,assertArgs); /* DR0836 */
   whichField = 0;
   for (tempPtr = assertArgs->nextArg; tempPtr != NULL; tempPtr = tempPtr->nextArg)
     {
      EvaluateExpression(theEnv,tempPtr,&theResult);
      factPtr->theProposition.contents[whichField].value = theResult.value;
      whichField++;
     }
   ExpressionDeinstall(theEnv,assertArgs); /* DR0836 */
   ReturnExpression(theEnv,assertArgs);
   DecrementClearReadyLocks(theEnv);

   /*==================*/
   /* Return the fact. */
   /*==================*/

   return(factPtr);
  }

#if BLOAD || BLOAD_AND_BSAVE

/*********************************************************/
/* NoSuchTemplateError: Prints out an error message      */
/* in a bload active environment */
/* when an implied deftemplate cannot be created for     */
/* an assert                                             */
/*********************************************************/
static void NoSuchTemplateError(
  Environment *theEnv,
  const char *templateName)
  {
   PrintErrorID(theEnv,"FACTRHS",1,false);
   WriteString(theEnv,STDERR,"Implied deftemplate '");
   WriteString(theEnv,STDERR,templateName);
   WriteString(theEnv,STDERR,"' cannot be created with binary load in effect.\n");
  }

#endif /* BLOAD || BLOAD_AND_BSAVE */

#endif /* DEFTEMPLATE_CONSTRUCT */


