#include "clips.h"
#include "libmaya.h"
#if PARSING_EXTENSIONS
void NextTokenFunction(void *,DATA_OBJECT_PTR);
void NextTokenFromStdin(void *,struct token *);
void* EmptyList(void*);

void ParsingExtensionsFunctions(void* theEnv) {
   EnvDefineFunction2(theEnv,"next-token", 'u', PTIEF NextTokenFunction,  "NextTokenFunction", "*1");
   EnvDefineFunction2(theEnv,"empty$",     'w', PTIEF EmptyList, "EmptyList", "11m");
}
void* EmptyList(void* theEnv) {
    DATA_OBJECT arg;
    if (EnvArgTypeCheck(theEnv,"empty$",1, MULTIFIELD, &arg) == 0) {
        return EnvFalseSymbol(theEnv);
    } else {
        if (GetDOLength(arg) == 0) {
            return EnvTrueSymbol(theEnv);
        } else {
            return EnvFalseSymbol(theEnv);
        }
    }
}
/****************************************************************/
/* NextTokenFunction: H/L access routine for the read function. */
/****************************************************************/
void NextTokenFunction(
  void *theEnv,
  DATA_OBJECT_PTR returnValue)
  {
   struct token theToken;
   int numberOfArguments;
   const char *logicalName = nullptr;
   void* mf = 0;

   /*===============================================*/
   /* Check for an appropriate number of arguments. */
   /*===============================================*/

   if ((numberOfArguments = EnvArgCountCheck(theEnv,"read",NO_MORE_THAN,1)) == -1)
     {
      returnValue->type = STRING;
      returnValue->value = (void *) EnvAddSymbol(theEnv,"*** READ ERROR ***");
      return;
     }

   /*======================================================*/
   /* Determine the logical name from which input is read. */
   /*======================================================*/

   if (numberOfArguments == 0)
     { logicalName = "stdin"; }
   else if (numberOfArguments == 1)
     {
      logicalName = GetLogicalName(theEnv,1,"stdin");
      if (logicalName == nullptr)
        {
         IllegalLogicalNameMessage(theEnv,"read");
         SetHaltExecution(theEnv,TRUE);
         SetEvaluationError(theEnv,TRUE);
         returnValue->type = STRING;
         returnValue->value = (void *) EnvAddSymbol(theEnv,"*** READ ERROR ***");
         return;
        }
     }

   /*============================================*/
   /* Check to see that the logical name exists. */
   /*============================================*/

   if (QueryRouters(theEnv,logicalName) == FALSE)
     {
      UnrecognizedRouterMessage(theEnv,logicalName);
      SetHaltExecution(theEnv,TRUE);
      SetEvaluationError(theEnv,TRUE);
      returnValue->type = STRING;
      returnValue->value = (void *) EnvAddSymbol(theEnv,"*** READ ERROR ***");
      return;
     }

   /*=======================================*/
   /* Collect input into string if the read */
   /* source is stdin, else just get token. */
   /*=======================================*/

   if (strcmp(logicalName,"stdin") == 0)
     { NextTokenFromStdin(theEnv,&theToken); }
   else
     { GetToken(theEnv,logicalName,&theToken); }

   RouterData(theEnv)->CommandBufferInputCount = 0;
   RouterData(theEnv)->AwaitingInput = FALSE;

   /*====================================================*/
   /* Copy the token to the return value data structure. */
   /*====================================================*/

   returnValue->type = theToken.type;
   if ((theToken.type == FLOAT) || (theToken.type == STRING) ||
       (theToken.type == INSTANCE_NAME) ||
       (theToken.type == SYMBOL) || (theToken.type == INTEGER))
     { returnValue->value = theToken.value; }
   else if (theToken.type == UNKNOWN_VALUE)
     {
      returnValue->type = STRING;
      returnValue->value = (void *) EnvAddSymbol(theEnv,"*** READ ERROR ***");
     }
   else if (theToken.type == STOP) 
     {
      mf = EnvCreateMultifield(theEnv, 2);
      SetMFType(mf, 1, SYMBOL);
      SetMFValue(mf, 1, EnvAddSymbol(theEnv, "STOP"));
      SetMFType(mf, 2, STRING);
      SetMFValue(mf, 2, EnvAddSymbol(theEnv, "EOF"));
      returnValue->type = MULTIFIELD;
      returnValue->value = mf;
      SetpDOBegin(returnValue, 1);
      SetpDOEnd(returnValue, 2);
     }
#define specialCaseEntry(symbol, str) \
   else if(theToken.type == symbol) { \
       mf = EnvCreateMultifield(theEnv, 2); \
       SetMFType(mf, 1, SYMBOL); \
       SetMFValue(mf, 1, EnvAddSymbol(theEnv, str)); \
       SetMFType(mf, 2, STRING); \
       SetMFValue(mf, 2, EnvAddSymbol(theEnv, theToken.printForm)); \
       returnValue->type = MULTIFIELD; \
       returnValue->value = mf; \
       SetpDOBegin(returnValue, 1); \
       SetpDOEnd(returnValue, 2); \
   }
specialCaseEntry(NOT_CONSTRAINT, "NOT_CONSTRAINT")
specialCaseEntry(AND_CONSTRAINT, "AND_CONSTRAINT")
specialCaseEntry(OR_CONSTRAINT, "OR_CONSTRAINT")
specialCaseEntry(LPAREN, "LPAREN")
specialCaseEntry(RPAREN, "RPAREN")
specialCaseEntry(GBL_VARIABLE, "GBL_VARIABLE")
specialCaseEntry(MF_GBL_VARIABLE, "MF_GBL_VARIABLE")
specialCaseEntry(SF_VARIABLE, "SF_VARIABLE")
specialCaseEntry(MF_VARIABLE, "MF_VARIABLE")
specialCaseEntry(SF_WILDCARD, "SF_WILDCARD")
specialCaseEntry(MF_WILDCARD, "MF_WILDCARD")
#undef specialCaseEntry
   else
     {
      returnValue->type = STRING;
      returnValue->value = (void *) EnvAddSymbol(theEnv,theToken.printForm);
     }

   return;
  }
/********************************************************/
/* NextTokenFromStdin: Special routine used by the read */
/*   function to read a token from standard input.      */
/********************************************************/
void NextTokenFromStdin(
  void *theEnv,
  struct token *theToken)
  {
   char *inputString;
   size_t inputStringSize;
   int inchar;

   /*=============================================*/
   /* Continue processing until a token is found. */
   /*=============================================*/

   theToken->type = STOP;
   while (theToken->type == STOP)
     {
      /*===========================================*/
      /* Initialize the variables used for storing */
      /* the characters retrieved from stdin.      */
      /*===========================================*/

      inputString = nullptr;
      RouterData(theEnv)->CommandBufferInputCount = 0;
      RouterData(theEnv)->AwaitingInput = TRUE;
      inputStringSize = 0;
      inchar = EnvGetcRouter(theEnv,"stdin");

      /*========================================================*/
      /* Continue reading characters until a carriage return is */
      /* entered or the user halts execution (usually with      */
      /* control-c). Waiting for the carriage return prevents   */
      /* the input from being prematurely parsed (such as when  */
      /* a space is entered after a symbol has been typed).     */
      /*========================================================*/

      while ((inchar != '\n') && (inchar != '\r') && (inchar != EOF) &&
             (! GetHaltExecution(theEnv)))
        {
         inputString = ExpandStringWithChar(theEnv,inchar,inputString,&RouterData(theEnv)->CommandBufferInputCount,
                                            &inputStringSize,inputStringSize + 80);
         inchar = EnvGetcRouter(theEnv,"stdin");
        }

      /*==================================================*/
      /* Open a string input source using the characters  */
      /* retrieved from stdin and extract the first token */
      /* contained in the string.                         */
      /*==================================================*/

      OpenStringSource(theEnv,"read",inputString,0);
      GetToken(theEnv,"read",theToken);
      CloseStringSource(theEnv,"read");
      if (inputStringSize > 0) rm(theEnv,inputString,inputStringSize);

      /*===========================================*/
      /* Pressing control-c (or comparable action) */
      /* aborts the read function.                 */
      /*===========================================*/

      if (GetHaltExecution(theEnv))
        {
         theToken->type = STRING;
         theToken->value = (void *) EnvAddSymbol(theEnv,"*** READ ERROR ***");
        }

      /*====================================================*/
      /* Return the EOF symbol if the end of file for stdin */
      /* has been encountered. This typically won't occur,  */
      /* but is possible (for example by pressing control-d */
      /* in the UNIX operating system).                     */
      /*====================================================*/

      if ((theToken->type == STOP) && (inchar == EOF))
        {
         theToken->type = SYMBOL;
         theToken->value = (void *) EnvAddSymbol(theEnv,"EOF");
        }
     }
  }
#endif // PARSING_EXTENSIONS
