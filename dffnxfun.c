   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  07/02/18             */
   /*                                                     */
   /*                 DEFFUNCTION MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*            Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warning.                              */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added missing initializer for ENTITY_RECORD.   */
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
/*            Added code to keep track of pointers to        */
/*            constructs that are contained externally to    */
/*            to constructs, DanglingConstructs.             */
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

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if DEFFUNCTION_CONSTRUCT

#if (BLOAD || BLOAD_AND_BSAVE)
#include "BinaryLoad.h"
#include "dffnxbin.h"
#endif

#include "constrct.h"
#include "cstrcpsr.h"
#include "dffnxpsr.h"
#include "modulpsr.h"

#include "Environment.h"

#include "ExternalFunctions.h"

#include "dffnxexe.h"

#if DEBUGGING_FUNCTIONS
#include "Watch.h"
#endif

#include "ArgumentAccess.h"
#include "cstrccom.h"
#include "memalloc.h"
#include "modulutl.h"
#include "multifld.h"
#include "prntutil.h"
#include "Router.h"

#include "dffnxfun.h"

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

   static void                    PrintDeffunctionCall(Environment *,const char *,Deffunction *);
   static bool                    EvaluateDeffunctionCall(Environment *,Deffunction *,UDFValue *);
   static void                    DecrementDeffunctionBusyCount(Environment *,Deffunction *);
   static void                    IncrementDeffunctionBusyCount(Environment *,Deffunction *);
   static void                    DeallocateDeffunctionData(Environment *);

   static void                    DestroyDeffunctionAction(Environment *,ConstructHeader *,void *);
   static void                   *AllocateModule(Environment *);
   static void                    ReturnModule(Environment *,void *);
   static bool                    ClearDeffunctionsReady(Environment *,void *);

   static bool                    RemoveAllDeffunctions(Environment *);
   static void                    DeffunctionDeleteError(Environment *,const char *);
   static void                    SaveDeffunctionHeaders(Environment *,Defmodule *,const char *,void *);
   static void                    SaveDeffunctionHeader(Environment *,ConstructHeader *,void *);
   static void                    SaveDeffunctions(Environment *,Defmodule *,const char *,void *);

#if DEBUGGING_FUNCTIONS
   static bool                    DeffunctionWatchAccess(Environment *,int,bool,Expression *);
   static bool                    DeffunctionWatchPrint(Environment *,const char *,int,Expression *);
#endif

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : SetupDeffunctions
  DESCRIPTION  : Initializes parsers and access
                 functions for deffunctions
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction environment initialized
  NOTES        : None
 ***************************************************/
void SetupDeffunctions(
  Environment *theEnv)
  {
   EntityRecord deffunctionEntityRecord =
                     { "PCALL", PCALL,0,0,1,
                       (EntityPrintFunction *) PrintDeffunctionCall,
                       (EntityPrintFunction *) PrintDeffunctionCall,
                       NULL,
                       (EntityEvaluationFunction *) EvaluateDeffunctionCall,
                       NULL,
                       (EntityBusyCountFunction *) DecrementDeffunctionBusyCount,
                       (EntityBusyCountFunction *) IncrementDeffunctionBusyCount,
                       NULL,NULL,NULL,NULL,NULL };

   AllocateEnvironmentData(theEnv,DEFFUNCTION_DATA,sizeof(struct deffunctionData),DeallocateDeffunctionData);
   memcpy(&DeffunctionData(theEnv)->DeffunctionEntityRecord,&deffunctionEntityRecord,sizeof(struct entityRecord));

   InstallPrimitive(theEnv,&DeffunctionData(theEnv)->DeffunctionEntityRecord,PCALL);

   DeffunctionData(theEnv)->DeffunctionModuleIndex =
                RegisterModuleItem(theEnv,"deffunction",
                                    AllocateModule,
                                    ReturnModule,
#if BLOAD_AND_BSAVE || BLOAD
                                    BloadDeffunctionModuleReference,
#else
                                    NULL,
#endif
                                    (FindConstructFunction *) FindDeffunctionInModule);
   DeffunctionData(theEnv)->DeffunctionConstruct = AddConstruct(theEnv,"deffunction","deffunctions",
                                       ParseDeffunction,
                                       (FindConstructFunction *) FindDeffunction,
                                       GetConstructNamePointer,GetConstructPPForm,
                                       GetConstructModuleItem,
                                       (GetNextConstructFunction *) GetNextDeffunction,
                                       SetNextConstruct,
                                       (IsConstructDeletableFunction *) DeffunctionIsDeletable,
                                       (DeleteConstructFunction *) Undeffunction,
                                       (FreeConstructFunction *) RemoveDeffunction
                                       );

   AddClearReadyFunction(theEnv,"deffunction",ClearDeffunctionsReady,0,NULL);

#if DEFMODULE_CONSTRUCT
   AddPortConstructItem(theEnv,"deffunction",SYMBOL_TOKEN);
#endif
   AddSaveFunction(theEnv,"deffunction-headers",SaveDeffunctionHeaders,1000,NULL);
   AddSaveFunction(theEnv,"deffunctions",SaveDeffunctions,0,NULL);
   AddUDF(theEnv,"undeffunction","v",1,1,"y",UndeffunctionCommand,"UndeffunctionCommand",NULL);

#if DEBUGGING_FUNCTIONS
   AddUDF(theEnv,"list-deffunctions","v",0,1,"y",ListDeffunctionsCommand,"ListDeffunctionsCommand",NULL);
   AddUDF(theEnv,"ppdeffunction","vs",1,2,";y;ldsyn",PPDeffunctionCommand,"PPDeffunctionCommand",NULL);
#endif

   AddUDF(theEnv,"get-deffunction-list","m",0,1,"y",GetDeffunctionListFunction,"GetDeffunctionListFunction",NULL);
   AddUDF(theEnv,"deffunction-module","y",1,1,"y",GetDeffunctionModuleCommand,"GetDeffunctionModuleCommand",NULL);

#if BLOAD_AND_BSAVE || BLOAD
   SetupDeffunctionsBload(theEnv);
#endif



#if DEBUGGING_FUNCTIONS
   AddWatchItem(theEnv,"deffunctions",0,&DeffunctionData(theEnv)->WatchDeffunctions,32,
                DeffunctionWatchAccess,DeffunctionWatchPrint);
#endif

  }

/******************************************************/
/* DeallocateDeffunctionData: Deallocates environment */
/*    data for the deffunction construct.             */
/******************************************************/
static void DeallocateDeffunctionData(
  Environment *theEnv)
  {
   DeffunctionModuleData *theModuleItem;
   Defmodule *theModule;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded(theEnv)) return;
#endif

   DoForAllConstructs(theEnv,
                      DestroyDeffunctionAction,
                      DeffunctionData(theEnv)->DeffunctionModuleIndex,false,NULL);

   for (theModule = GetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = GetNextDefmodule(theEnv,theModule))
     {
      theModuleItem = (struct deffunctionModuleData *)
                      GetModuleItem(theEnv,theModule,
                                    DeffunctionData(theEnv)->DeffunctionModuleIndex);
      rtn_struct(theEnv,deffunctionModuleData,theModuleItem);
     }
  }

/*****************************************************/
/* DestroyDeffunctionAction: Action used to remove   */
/*   deffunctions as a result of DestroyEnvironment. */
/*****************************************************/
static void DestroyDeffunctionAction(
  Environment *theEnv,
  ConstructHeader *theConstruct,
  void *buffer)
  {
#if MAC_XCD
#pragma unused(buffer)
#endif
   Deffunction *theDeffunction = (Deffunction *) theConstruct;

   if (theDeffunction == NULL) return;

   ReturnPackedExpression(theEnv,theDeffunction->code);

   DestroyConstructHeader(theEnv,&theDeffunction->header);

   rtn_struct(theEnv,deffunction,theDeffunction);
  }

/***************************************************
  NAME         : FindDeffunction
  DESCRIPTION  : Searches for a deffunction
  INPUTS       : The name of the deffunction
                 (possibly including a module name)
  RETURNS      : Pointer to the deffunction if
                 found, otherwise NULL
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
Deffunction *FindDeffunction(
  Environment *theEnv,
  const char *dfnxModuleAndName)
  {
   return (Deffunction *) FindNamedConstructInModuleOrImports(theEnv,dfnxModuleAndName,DeffunctionData(theEnv)->DeffunctionConstruct);
  }

/***************************************************
  NAME         : FindDeffunctionInModule
  DESCRIPTION  : Searches for a deffunction
  INPUTS       : The name of the deffunction
                 (possibly including a module name)
  RETURNS      : Pointer to the deffunction if
                 found, otherwise NULL
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
Deffunction *FindDeffunctionInModule(
  Environment *theEnv,
  const char *dfnxModuleAndName)
  {
   return (Deffunction *) FindNamedConstructInModule(theEnv,dfnxModuleAndName,DeffunctionData(theEnv)->DeffunctionConstruct);
  }

/***************************************************
  NAME         : LookupDeffunctionByMdlOrScope
  DESCRIPTION  : Finds a deffunction anywhere (if
                 module is specified) or in current
                 or imported modules
  INPUTS       : The deffunction name
  RETURNS      : The deffunction (NULL if not found)
  SIDE EFFECTS : Error message printed on
                  ambiguous references
  NOTES        : None
 ***************************************************/
Deffunction *LookupDeffunctionByMdlOrScope(
  Environment *theEnv,
  const char *deffunctionName)
  {
   return((Deffunction *) LookupConstruct(theEnv,DeffunctionData(theEnv)->DeffunctionConstruct,deffunctionName,true));
  }

/***************************************************
  NAME         : LookupDeffunctionInScope
  DESCRIPTION  : Finds a deffunction in current or
                   imported modules (module
                   specifier is not allowed)
  INPUTS       : The deffunction name
  RETURNS      : The deffunction (NULL if not found)
  SIDE EFFECTS : Error message printed on
                  ambiguous references
  NOTES        : None
 ***************************************************/
Deffunction *LookupDeffunctionInScope(
  Environment *theEnv,
  const char *deffunctionName)
  {
   return (Deffunction *) LookupConstruct(theEnv,DeffunctionData(theEnv)->DeffunctionConstruct,deffunctionName,false);
  }

/***************************************************
  NAME         : Undeffunction
  DESCRIPTION  : External interface routine for
                 removing a deffunction
  INPUTS       : Deffunction pointer
  RETURNS      : False if unsuccessful,
                 true otherwise
  SIDE EFFECTS : Deffunction deleted, if possible
  NOTES        : None
 ***************************************************/
bool Undeffunction(
  Deffunction *theDeffunction,
  Environment *allEnv)
  {   
   Environment *theEnv;
   bool success;
   GCBlock gcb;
  
   if (theDeffunction == NULL)
     { theEnv = allEnv; }
   else
     { theEnv = theDeffunction->header.env; }

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded(theEnv) == true)
     return false;
#endif

   GCBlockStart(theEnv,&gcb);
   if (theDeffunction == NULL)
     {
      success = RemoveAllDeffunctions(theEnv);
      GCBlockEnd(theEnv,&gcb);
      return success;
     }
      
   if (DeffunctionIsDeletable(theDeffunction) == false)
     {
      GCBlockEnd(theEnv,&gcb);
      return false;
     }
     
   RemoveConstructFromModule(theEnv,&theDeffunction->header);
   RemoveDeffunction(theEnv,theDeffunction);
   GCBlockEnd(theEnv,&gcb);

   return true;
  }

/****************************************************
  NAME         : GetNextDeffunction
  DESCRIPTION  : Accesses list of deffunctions
  INPUTS       : Deffunction pointer
  RETURNS      : The next deffunction, or the
                 first deffunction (if input is NULL)
  SIDE EFFECTS : None
  NOTES        : None
 ****************************************************/
Deffunction *GetNextDeffunction(
  Environment *theEnv,
  Deffunction *theDeffunction)
  {
   return (Deffunction *)
          GetNextConstructItem(theEnv,&theDeffunction->header,
                               DeffunctionData(theEnv)->DeffunctionModuleIndex);
  }

/***************************************************
  NAME         : DeffunctionIsDeletable
  DESCRIPTION  : Determines if a deffunction is
                 executing or referenced by another
                 expression
  INPUTS       : Deffunction pointer
  RETURNS      : True if the deffunction can
                 be deleted, false otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
bool DeffunctionIsDeletable(
  Deffunction *theDeffunction)
  {
   Environment *theEnv = theDeffunction->header.env;

   if (! ConstructsDeletable(theEnv))
     { return false; }

   return(((theDeffunction->busy == 0) && (theDeffunction->executing == 0)) ? true : false);
  }


/***************************************************
  NAME         : RemoveDeffunction
  DESCRIPTION  : Removes a deffunction
  INPUTS       : Deffunction pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction deallocated
  NOTES        : Assumes deffunction is not in use!!
 ***************************************************/
void RemoveDeffunction(
  Environment *theEnv,
  Deffunction *theDeffunction)
  {
   if (theDeffunction == NULL)
     return;
   ReleaseLexeme(theEnv,GetDeffunctionNamePointer(theEnv,theDeffunction));
   ExpressionDeinstall(theEnv,theDeffunction->code);
   ReturnPackedExpression(theEnv,theDeffunction->code);
   SetDeffunctionPPForm(theEnv,theDeffunction,NULL);
   ClearUserDataList(theEnv,theDeffunction->header.usrData);
   rtn_struct(theEnv,deffunction,theDeffunction);
  }


/********************************************************
  NAME         : UndeffunctionCommand
  DESCRIPTION  : Deletes the named deffunction(s)
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction(s) removed
  NOTES        : H/L Syntax: (undeffunction <name> | *)
 ********************************************************/
void UndeffunctionCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UndefconstructCommand(context,"undeffunction",DeffunctionData(theEnv)->DeffunctionConstruct);
  }

/****************************************************************
  NAME         : GetDeffunctionModuleCommand
  DESCRIPTION  : Determines to which module a deffunction belongs
  INPUTS       : None
  RETURNS      : The symbolic name of the module
  SIDE EFFECTS : None
  NOTES        : H/L Syntax: (deffunction-module <dfnx-name>)
 ****************************************************************/
void GetDeffunctionModuleCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   returnValue->value = GetConstructModuleCommand(context,"deffunction-module",DeffunctionData(theEnv)->DeffunctionConstruct);
  }

#if DEBUGGING_FUNCTIONS

/****************************************************
  NAME         : PPDeffunctionCommand
  DESCRIPTION  : Displays the pretty-print form of a
                 deffunction
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Pretty-print form displayed to
                 STDOUT logical name
  NOTES        : H/L Syntax: (ppdeffunction <name>)
 ****************************************************/
void PPDeffunctionCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   PPConstructCommand(context,"ppdeffunction",DeffunctionData(theEnv)->DeffunctionConstruct,returnValue);
  }

/***************************************************
  NAME         : ListDeffunctionsCommand
  DESCRIPTION  : Displays all deffunction names
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction name sprinted
  NOTES        : H/L Interface
 ***************************************************/
void ListDeffunctionsCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   ListConstructCommand(context,DeffunctionData(theEnv)->DeffunctionConstruct);
  }

/***************************************************
  NAME         : ListDeffunctions
  DESCRIPTION  : Displays all deffunction names
  INPUTS       : 1) The logical name of the output
                 2) The module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction name sprinted
  NOTES        : C Interface
 ***************************************************/
void ListDeffunctions(
  Environment *theEnv,
  const char *logicalName,
  Defmodule *theModule)
  {
   ListConstruct(theEnv,DeffunctionData(theEnv)->DeffunctionConstruct,logicalName,theModule);
  }

#endif

/***************************************************************
  NAME         : GetDeffunctionListFunction
  DESCRIPTION  : Groups all deffunction names into
                 a multifield list
  INPUTS       : A data object buffer to hold
                 the multifield result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multifield allocated and filled
  NOTES        : H/L Syntax: (get-deffunction-list [<module>])
 ***************************************************************/
void GetDeffunctionListFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   GetConstructListFunction(context,returnValue,DeffunctionData(theEnv)->DeffunctionConstruct);
  }

/***************************************************************
  NAME         : GetDeffunctionList
  DESCRIPTION  : Groups all deffunction names into
                 a multifield list
  INPUTS       : 1) A data object buffer to hold
                    the multifield result
                 2) The module from which to obtain deffunctions
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multifield allocated and filled
  NOTES        : External C access
 ***************************************************************/
void GetDeffunctionList(
  Environment *theEnv,
  CLIPSValue *returnValue,
  Defmodule *theModule)
  {
   UDFValue result;
   
   GetConstructList(theEnv,&result,DeffunctionData(theEnv)->DeffunctionConstruct,theModule);
   NormalizeMultifield(theEnv,&result);
   returnValue->value = result.value;
  }

/*******************************************************
  NAME         : CheckDeffunctionCall
  DESCRIPTION  : Checks the number of arguments
                 passed to a deffunction
  INPUTS       : 1) Deffunction pointer
                 2) The number of arguments
  RETURNS      : True if OK, false otherwise
  SIDE EFFECTS : Message printed on errors
  NOTES        : None
 *******************************************************/
bool CheckDeffunctionCall(
  Environment *theEnv,
  Deffunction *theDeffunction,
  int args)
  {
   if (theDeffunction == NULL)
     return false;

   if (args < theDeffunction->minNumberOfParameters)
     {
      if (theDeffunction->maxNumberOfParameters == PARAMETERS_UNBOUNDED)
        ExpectedCountError(theEnv,DeffunctionName(theDeffunction),
                           AT_LEAST,theDeffunction->minNumberOfParameters);
      else
        ExpectedCountError(theEnv,DeffunctionName(theDeffunction),
                           EXACTLY,theDeffunction->minNumberOfParameters);
      return false;
     }
   else if ((args > theDeffunction->minNumberOfParameters) &&
            (theDeffunction->maxNumberOfParameters != PARAMETERS_UNBOUNDED))
     {
      ExpectedCountError(theEnv,DeffunctionName(theDeffunction),
                         EXACTLY,theDeffunction->minNumberOfParameters);
      return false;
     }
   return true;
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : PrintDeffunctionCall
  DESCRIPTION  : PrintExpression() support function
                 for deffunction calls
  INPUTS       : 1) The output logical name
                 2) The deffunction
  RETURNS      : Nothing useful
  SIDE EFFECTS : Call expression printed
  NOTES        : None
 ***************************************************/
static void PrintDeffunctionCall(
  Environment *theEnv,
  const char *logName,
  Deffunction *theDeffunction)
  {
#if DEVELOPER

   WriteString(theEnv,logName,"(");
   WriteString(theEnv,logName,DeffunctionName(theDeffunction));
   if (GetFirstArgument() != NULL)
     {
      WriteString(theEnv,logName," ");
      PrintExpression(theEnv,logName,GetFirstArgument());
     }
   WriteString(theEnv,logName,")");
#else
#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(logName)
#pragma unused(theDeffunction)
#endif
#endif
  }

/*******************************************************
  NAME         : EvaluateDeffunctionCall
  DESCRIPTION  : Primitive support function for
                 calling a deffunction
  INPUTS       : 1) The deffunction
                 2) A data object buffer to hold
                    the evaluation result
  RETURNS      : False if the deffunction
                 returns the symbol false,
                 true otherwise
  SIDE EFFECTS : Data obejct buffer set and any
                 side-effects of calling the deffunction
  NOTES        : None
 *******************************************************/
static bool EvaluateDeffunctionCall(
  Environment *theEnv,
  Deffunction *theDeffunction,
  UDFValue *returnValue)
  {
   CallDeffunction(theEnv,theDeffunction,GetFirstArgument(),returnValue);
   if (returnValue->value == FalseSymbol(theEnv))
     { return false; }
   return true;
  }

/***************************************************
  NAME         : DecrementDeffunctionBusyCount
  DESCRIPTION  : Lowers the busy count of a
                 deffunction construct
  INPUTS       : The deffunction
  RETURNS      : Nothing useful
  SIDE EFFECTS : Busy count decremented if a clear
                 is not in progress (see comment)
  NOTES        : None
 ***************************************************/
static void DecrementDeffunctionBusyCount(
  Environment *theEnv,
  Deffunction *theDeffunction)
  {
   /* ==============================================
      The deffunctions to which expressions in other
      constructs may refer may already have been
      deleted - thus, it is important not to modify
      the busy flag during a clear.
      ============================================== */
   if (! ConstructData(theEnv)->ClearInProgress)
     theDeffunction->busy--;
  }

/***************************************************
  NAME         : IncrementDeffunctionBusyCount
  DESCRIPTION  : Raises the busy count of a
                 deffunction construct
  INPUTS       : The deffunction
  RETURNS      : Nothing useful
  SIDE EFFECTS : Busy count incremented
  NOTES        : None
 ***************************************************/
static void IncrementDeffunctionBusyCount(
  Environment *theEnv,
  Deffunction *theDeffunction)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif
   if (! ConstructData(theEnv)->ParsingConstruct)
     { ConstructData(theEnv)->DanglingConstructs++; }

   theDeffunction->busy++;
  }


/*****************************************************
  NAME         : AllocateModule
  DESCRIPTION  : Creates and initializes a
                 list of deffunctions for a new module
  INPUTS       : None
  RETURNS      : The new deffunction module
  SIDE EFFECTS : Deffunction module created
  NOTES        : None
 *****************************************************/
static void *AllocateModule(
  Environment *theEnv)
  {
   return (void *) get_struct(theEnv,deffunctionModuleData);
  }

/***************************************************
  NAME         : ReturnModule
  DESCRIPTION  : Removes a deffunction module and
                 all associated deffunctions
  INPUTS       : The deffunction module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Module and deffunctions deleted
  NOTES        : None
 ***************************************************/
static void ReturnModule(
  Environment *theEnv,
  void *theItem)
  {
   FreeConstructHeaderModule(theEnv,(struct defmoduleItemHeader *) theItem,DeffunctionData(theEnv)->DeffunctionConstruct);
   rtn_struct(theEnv,deffunctionModuleData,theItem);
  }

/***************************************************
  NAME         : ClearDeffunctionsReady
  DESCRIPTION  : Determines if it is safe to
                 remove all deffunctions
                 Assumes *all* constructs will be
                 deleted - only checks to see if
                 any deffunctions are currently
                 executing
  INPUTS       : None
  RETURNS      : True if no deffunctions are
                 executing, false otherwise
  SIDE EFFECTS : None
  NOTES        : Used by (clear) and (bload)
 ***************************************************/
static bool ClearDeffunctionsReady(
  Environment *theEnv,
  void *context)
  {
   return((DeffunctionData(theEnv)->ExecutingDeffunction != NULL) ? false : true);
  }


/***************************************************
  NAME         : RemoveAllDeffunctions
  DESCRIPTION  : Removes all deffunctions
  INPUTS       : None
  RETURNS      : True if all deffunctions
                 removed, false otherwise
  SIDE EFFECTS : Deffunctions removed
  NOTES        : None
 ***************************************************/
static bool RemoveAllDeffunctions(
  Environment *theEnv)
  {
   Deffunction *dptr, *dtmp;
   unsigned oldbusy;
   bool success = true;

#if BLOAD || BLOAD_AND_BSAVE

   if (Bloaded(theEnv) == true)
     return false;
#endif

   dptr = GetNextDeffunction(theEnv,NULL);
   while (dptr != NULL)
     {
      if (dptr->executing > 0)
        {
         DeffunctionDeleteError(theEnv,DeffunctionName(dptr));
         success = false;
        }
      else
        {
         oldbusy = dptr->busy;
         ExpressionDeinstall(theEnv,dptr->code);
         dptr->busy = oldbusy;
         ReturnPackedExpression(theEnv,dptr->code);
         dptr->code = NULL;
        }
      dptr = GetNextDeffunction(theEnv,dptr);
     }

   dptr = GetNextDeffunction(theEnv,NULL);
   while (dptr != NULL)
     {
      dtmp = dptr;
      dptr = GetNextDeffunction(theEnv,dptr);
      if (dtmp->executing == 0)
        {
         if (dtmp->busy > 0)
           {
            PrintWarningID(theEnv,"DFFNXFUN",1,false);
            WriteString(theEnv,STDWRN,"Deffunction '");
            WriteString(theEnv,STDWRN,DeffunctionName(dtmp));
            WriteString(theEnv,STDWRN,"' only partially deleted due to usage by other constructs.\n");
            SetDeffunctionPPForm(theEnv,dtmp,NULL);
            success = false;
           }
         else
           {
            RemoveConstructFromModule(theEnv,&dtmp->header);
            RemoveDeffunction(theEnv,dtmp);
           }
        }
     }
   return(success);
  }

/****************************************************
  NAME         : DeffunctionDeleteError
  DESCRIPTION  : Prints out an error message when
                 a deffunction deletion attempt fails
  INPUTS       : The deffunction name
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error message printed
  NOTES        : None
 ****************************************************/
static void DeffunctionDeleteError(
  Environment *theEnv,
  const char *dfnxName)
  {
   CantDeleteItemErrorMessage(theEnv,"deffunction",dfnxName);
  }

/***************************************************
  NAME         : SaveDeffunctionHeaders
  DESCRIPTION  : Writes out deffunction forward
                 declarations for (save) command
  INPUTS       : The logical output name
  RETURNS      : Nothing useful
  SIDE EFFECTS : Writes out deffunctions with no
                 body of actions
  NOTES        : Used for deffunctions which are
                 mutually recursive with other
                 constructs
 ***************************************************/
static void SaveDeffunctionHeaders(
  Environment *theEnv,
  Defmodule *theModule,
  const char *logicalName,
  void *context)
  {
   DoForAllConstructsInModule(theEnv,theModule,
                              SaveDeffunctionHeader,
                              DeffunctionData(theEnv)->DeffunctionModuleIndex,
                              false,(void *) logicalName);
  }

/***************************************************
  NAME         : SaveDeffunctionHeader
  DESCRIPTION  : Writes a deffunction forward
                 declaration to the save file
  INPUTS       : 1) The deffunction
                 2) The logical name of the output
  RETURNS      : Nothing useful
  SIDE EFFECTS : Defffunction header written
  NOTES        : None
 ***************************************************/
static void SaveDeffunctionHeader(
  Environment *theEnv,
  ConstructHeader *theDeffunction,
  void *userBuffer)
  {
   Deffunction *dfnxPtr = (Deffunction *) theDeffunction;
   const char *logicalName = (const char *) userBuffer;
   unsigned short i;

   if (DeffunctionPPForm(dfnxPtr) != NULL)
     {
      WriteString(theEnv,logicalName,"(deffunction ");
      WriteString(theEnv,logicalName,DeffunctionModule(dfnxPtr));
      WriteString(theEnv,logicalName,"::");
      WriteString(theEnv,logicalName,DeffunctionName(dfnxPtr));
      WriteString(theEnv,logicalName," (");
      for (i = 0 ; i < dfnxPtr->minNumberOfParameters ; i++)
        {
         WriteString(theEnv,logicalName,"?p");
         PrintUnsignedInteger(theEnv,logicalName,i);
         if ((i + 1) != dfnxPtr->minNumberOfParameters)
           WriteString(theEnv,logicalName," ");
        }
      if (dfnxPtr->maxNumberOfParameters == PARAMETERS_UNBOUNDED)
        {
         if (dfnxPtr->minNumberOfParameters != 0)
           WriteString(theEnv,logicalName," ");
         WriteString(theEnv,logicalName,"$?wildargs))\n\n");
        }
      else
        WriteString(theEnv,logicalName,"))\n\n");
     }
  }

/***************************************************
  NAME         : SaveDeffunctions
  DESCRIPTION  : Writes out deffunctions
                 for (save) command
  INPUTS       : The logical output name
  RETURNS      : Nothing useful
  SIDE EFFECTS : Writes out deffunctions
  NOTES        : None
 ***************************************************/
static void SaveDeffunctions(
  Environment *theEnv,
  Defmodule *theModule,
  const char *logicalName,
  void *context)
  {
   SaveConstruct(theEnv,theModule,logicalName,DeffunctionData(theEnv)->DeffunctionConstruct);
  }


#if DEBUGGING_FUNCTIONS

/******************************************************************
  NAME         : DeffunctionWatchAccess
  DESCRIPTION  : Parses a list of deffunction names passed by
                 AddWatchItem() and sets the traces accordingly
  INPUTS       : 1) A code indicating which trace flag is to be set
                    Ignored
                 2) The value to which to set the trace flags
                 3) A list of expressions containing the names
                    of the deffunctions for which to set traces
  RETURNS      : True if all OK, false otherwise
  SIDE EFFECTS : Watch flags set in specified deffunctions
  NOTES        : Accessory function for AddWatchItem()
 ******************************************************************/
static bool DeffunctionWatchAccess(
  Environment *theEnv,
  int code,
  bool newState,
  Expression *argExprs)
  {
#if MAC_XCD
#pragma unused(code)
#endif

   return(ConstructSetWatchAccess(theEnv,DeffunctionData(theEnv)->DeffunctionConstruct,newState,argExprs,
                                  (ConstructGetWatchFunction *) DeffunctionGetWatch,
                                  (ConstructSetWatchFunction *) DeffunctionSetWatch));
  }

/***********************************************************************
  NAME         : DeffunctionWatchPrint
  DESCRIPTION  : Parses a list of deffunction names passed by
                 AddWatchItem() and displays the traces accordingly
  INPUTS       : 1) The logical name of the output
                 2) A code indicating which trace flag is to be examined
                    Ignored
                 3) A list of expressions containing the names
                    of the deffunctions for which to examine traces
  RETURNS      : True if all OK, false otherwise
  SIDE EFFECTS : Watch flags displayed for specified deffunctions
  NOTES        : Accessory function for AddWatchItem()
 ***********************************************************************/
static bool DeffunctionWatchPrint(
  Environment *theEnv,
  const char *logName,
  int code,
  Expression *argExprs)
  {
#if MAC_XCD
#pragma unused(code)
#endif

   return(ConstructPrintWatchAccess(theEnv,DeffunctionData(theEnv)->DeffunctionConstruct,logName,argExprs,
                                    (ConstructGetWatchFunction *) DeffunctionGetWatch,
                                    (ConstructSetWatchFunction *) DeffunctionSetWatch));
  }

/*********************************************************
  NAME         : DeffunctionSetWatch
  DESCRIPTION  : Sets the trace to ON/OFF for the
                 deffunction
  INPUTS       : 1) True to set the trace on,
                    false to set it off
                 2) A pointer to the deffunction
  RETURNS      : Nothing useful
  SIDE EFFECTS : Watch flag for the deffunction set
  NOTES        : None
 *********************************************************/
void DeffunctionSetWatch(
  Deffunction *theDeffunction,
  bool newState)
  {
   theDeffunction->trace = newState;
  }

/*********************************************************
  NAME         : DeffunctionGetWatch
  DESCRIPTION  : Determines if trace messages are
                 gnerated when executing deffunction
  INPUTS       : A pointer to the deffunction
  RETURNS      : True if a trace is active,
                 false otherwise
  SIDE EFFECTS : None
  NOTES        : None
 *********************************************************/
bool DeffunctionGetWatch(
  Deffunction *theDeffunction)
  {
   return theDeffunction->trace;
  }

#endif

/*##################################*/
/* Additional Environment Functions */
/*##################################*/

const char *DeffunctionModule(
  Deffunction *theDeffunction)
  {
   return GetConstructModuleName(&theDeffunction->header);
  }

const char *DeffunctionName(
  Deffunction *theDeffunction)
  {
   return GetConstructNameString(&theDeffunction->header);
  }

const char *DeffunctionPPForm(
  Deffunction *theDeffunction)
  {
   return GetConstructPPForm(&theDeffunction->header);
  }

CLIPSLexeme *GetDeffunctionNamePointer(
  Environment *theEnv,
  Deffunction *theDeffunction)
  {
   return GetConstructNamePointer(&theDeffunction->header);
  }

void SetDeffunctionPPForm(
  Environment *theEnv,
  Deffunction *theDeffunction,
  const char *thePPForm)
  {
   SetConstructPPForm(theEnv,&theDeffunction->header,thePPForm);
  }

#endif


