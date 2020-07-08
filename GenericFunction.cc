/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  02/20/20             */
/*                                                     */
/*                                                     */
/*******************************************************/

/*************************************************************/
/* Purpose: Generic Functions Internal Routines              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Removed IMPERATIVE_METHODS compilation flag.   */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed linkage issue when DEBUGGING_FUNCTIONS   */
/*            is set to 0 and PROFILING_FUNCTIONS is set to  */
/*            1.                                             */
/*                                                           */
/*            Fixed typing issue when OBJECT_SYSTEM          */
/*            compiler flag is set to 0.                     */
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
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "Setup.h"

#if DEFGENERIC_CONSTRUCT

#if BLOAD_AND_BSAVE
#include "BinaryLoad.h"
#endif

#include "ClassCommands.h"
#include "ClassFunctions.h"

#include "ArgumentAccess.h"
#include "Construct.h"
#include "Construct.h"
#include "Construct.h"
#include "Environment.h"
#include "GenericFunctionCommands.h"
#include "GenericFunctionExecution.h"
#include "GenericFunction.h"
#include "MemoryAllocation.h"
#include "DefmoduleUtility.h"
#include "ProceduralCodeSupportRoutines.h"
#include "PrintUtility.h"
#include "Router.h"
#include "SystemDependency.h"

#include "GenericFunction.h"

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

#if DEBUGGING_FUNCTIONS
static void DisplayGenericCore(Environment *, Defgeneric *);
#endif

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */


/***************************************************
  NAME         : ClearDefgenericsReady
  DESCRIPTION  : Determines if it is safe to
                 remove all defgenerics
                 Assumes *all* constructs will be
                 deleted - only checks to see if
                 any methods are currently
                 executing
  INPUTS       : None
  RETURNS      : True if no methods are
                 executing, false otherwise
  SIDE EFFECTS : None
  NOTES        : Used by (clear) and (bload)
 ***************************************************/
bool ClearDefgenericsReady(
        Environment *theEnv,
        void *context) {
    return ((DefgenericData(theEnv)->CurrentGeneric != nullptr) ? false : true);
}

/*****************************************************
  NAME         : AllocateDefgenericModule
  DESCRIPTION  : Creates and initializes a
                 list of defgenerics for a new module
  INPUTS       : None
  RETURNS      : The new deffunction module
  SIDE EFFECTS : Deffunction module created
  NOTES        : None
 *****************************************************/
void *AllocateDefgenericModule(
        Environment *theEnv) {
    return (void *) get_struct(theEnv, defgenericModule);
}

/***************************************************
  NAME         : FreeDefgenericModule
  DESCRIPTION  : Removes a deffunction module and
                 all associated deffunctions
  INPUTS       : The deffunction module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Module and deffunctions deleted
  NOTES        : None
 ***************************************************/
void FreeDefgenericModule(
        Environment *theEnv,
        void *theItem) {
    FreeConstructHeaderModule(theEnv, (struct defmoduleItemHeader *) theItem, DefgenericData(theEnv)->DefgenericConstruct);
    rtn_struct(theEnv, defgenericModule, theItem);
}



/************************************************************
  NAME         : ClearDefmethods
  DESCRIPTION  : Deletes all defmethods - generic headers
                   are left intact
  INPUTS       : None
  RETURNS      : True if all methods deleted, false otherwise
  SIDE EFFECTS : Defmethods deleted
  NOTES        : Clearing generic functions is done in
                   two stages

                 1) Delete all methods (to clear any
                    references to other constructs)
                 2) Delete all generic headers

                 This allows other constructs which
                   mutually refer to generic functions
                   to be cleared
 ************************************************************/
bool ClearDefmethods(
        Environment *theEnv) {
    Defgeneric *gfunc;
    bool success = true;

#if BLOAD_AND_BSAVE
    if (Bloaded(theEnv) == true) return false;
#endif

    gfunc = GetNextDefgeneric(theEnv, nullptr);
    while (gfunc != nullptr) {
        if (RemoveAllExplicitMethods(theEnv, gfunc) == false)
            success = false;
        gfunc = GetNextDefgeneric(theEnv, gfunc);
    }
    return success;
}

/*****************************************************************
  NAME         : RemoveAllExplicitMethods
  DESCRIPTION  : Deletes all explicit defmethods - generic headers
                   are left intact (as well as a method for an
                   overloaded system function)
  INPUTS       : None
  RETURNS      : True if all methods deleted, false otherwise
  SIDE EFFECTS : Explicit defmethods deleted
  NOTES        : None
 *****************************************************************/
bool RemoveAllExplicitMethods(
        Environment *theEnv,
        Defgeneric *gfunc) {
    unsigned short i, j;
    unsigned short systemMethodCount = 0;
    Defmethod *narr;

    if (MethodsExecuting(gfunc) == false) {
        for (i = 0; i < gfunc->mcnt; i++) {
            if (gfunc->methods[i].system)
                systemMethodCount++;
            else
                DeleteMethodInfo(theEnv, gfunc, &gfunc->methods[i]);
        }
        if (systemMethodCount != 0) {
            narr = (Defmethod *) gm2(theEnv, (systemMethodCount * sizeof(Defmethod)));
            i = 0;
            j = 0;
            while (i < gfunc->mcnt) {
                if (gfunc->methods[i].system)
                    GenCopyMemory(Defmethod, 1, &narr[j++], &gfunc->methods[i]);
                i++;
            }
            rm(theEnv, gfunc->methods, (sizeof(Defmethod) * gfunc->mcnt));
            gfunc->mcnt = systemMethodCount;
            gfunc->methods = narr;
        } else {
            if (gfunc->mcnt != 0)
                rm(theEnv, gfunc->methods, (sizeof(Defmethod) * gfunc->mcnt));
            gfunc->mcnt = 0;
            gfunc->methods = nullptr;
        }
        return true;
    }
    return false;
}

/**************************************************
  NAME         : RemoveDefgeneric
  DESCRIPTION  : Removes a generic function node
                   from the generic list along with
                   all its methods
  INPUTS       : The generic function
  RETURNS      : Nothing useful
  SIDE EFFECTS : List adjusted
                 Nodes deallocated
  NOTES        : Assumes generic is not in use!!!
 **************************************************/
void RemoveDefgeneric(
        Environment *theEnv,
        Defgeneric *theDefgeneric) {
    long i;

    for (i = 0; i < theDefgeneric->mcnt; i++)
        DeleteMethodInfo(theEnv, theDefgeneric, &theDefgeneric->methods[i]);

    if (theDefgeneric->mcnt != 0) { rm(theEnv, theDefgeneric->methods, (sizeof(Defmethod) * theDefgeneric->mcnt)); }
    ReleaseLexeme(theEnv, GetDefgenericNamePointer(theDefgeneric));
    SetDefgenericPPForm(theEnv, theDefgeneric, nullptr);
    ClearUserDataList(theEnv, theDefgeneric->header.usrData);
    rtn_struct(theEnv, defgeneric, theDefgeneric);
}

/****************************************************************
  NAME         : ClearDefgenerics
  DESCRIPTION  : Deletes all generic headers
  INPUTS       : None
  RETURNS      : True if all methods deleted, false otherwise
  SIDE EFFECTS : Generic headers deleted (and any implicit system
                  function methods)
  NOTES        : None
 ****************************************************************/
bool ClearDefgenerics(
        Environment *theEnv) {
    Defgeneric *gfunc, *gtmp;
    bool success = true;

#if BLOAD_AND_BSAVE
    if (Bloaded(theEnv) == true) return false;
#endif

    gfunc = GetNextDefgeneric(theEnv, nullptr);
    while (gfunc != nullptr) {
        gtmp = gfunc;
        gfunc = GetNextDefgeneric(theEnv, gfunc);
        if (RemoveAllExplicitMethods(theEnv, gtmp) == false) {
            CantDeleteItemErrorMessage(theEnv, "generic function", DefgenericName(gtmp));
            success = false;
        } else {
            RemoveConstructFromModule(theEnv, &gtmp->header);
            RemoveDefgeneric(theEnv, gtmp);
        }
    }
    return (success);
}

/********************************************************
  NAME         : MethodAlterError
  DESCRIPTION  : Prints out an error message reflecting
                   that a generic function's methods
                   cannot be altered while any of them
                   are executing
  INPUTS       : The generic function
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************/
void MethodAlterError(
        Environment *theEnv,
        Defgeneric *gfunc) {
    PrintErrorID(theEnv, "GENRCFUN", 1, false);
    WriteString(theEnv, STDERR, "Defgeneric '");
    WriteString(theEnv, STDERR, DefgenericName(gfunc));
    WriteString(theEnv, STDERR, "' cannot be modified while one of its methods is executing.\n");
}

/***************************************************
  NAME         : DeleteMethodInfo
  DESCRIPTION  : Deallocates all the data associated
                  w/ a method but does not release
                  the method structure itself
  INPUTS       : 1) The generic function address
                 2) The method address
  RETURNS      : Nothing useful
  SIDE EFFECTS : Nodes deallocated
  NOTES        : None
 ***************************************************/
void DeleteMethodInfo(
        Environment *theEnv,
        Defgeneric *gfunc,
        Defmethod *meth) {
    short j, k;
    RESTRICTION *rptr;

    SaveBusyCount(gfunc);
    ExpressionDeinstall(theEnv, meth->actions);
    ReturnPackedExpression(theEnv, meth->actions);
    ClearUserDataList(theEnv, meth->header.usrData);
    if (meth->header.ppForm != nullptr)
        rm(theEnv, (void *) meth->header.ppForm, (sizeof(char) * (strlen(meth->header.ppForm) + 1)));
    for (j = 0; j < meth->restrictionCount; j++) {
        rptr = &meth->restrictions[j];

        for (k = 0; k < rptr->tcnt; k++)
                DecrementDefclassBusyCount(theEnv, (Defclass *) rptr->types[k]);

        if (rptr->types != nullptr)
            rm(theEnv, rptr->types, (sizeof(void *) * rptr->tcnt));
        ExpressionDeinstall(theEnv, rptr->query);
        ReturnPackedExpression(theEnv, rptr->query);
    }
    if (meth->restrictions != nullptr)
        rm(theEnv, meth->restrictions,
           (sizeof(RESTRICTION) * meth->restrictionCount));
    RestoreBusyCount(gfunc);
}

/***************************************************
  NAME         : DestroyMethodInfo
  DESCRIPTION  : Deallocates all the data associated
                  w/ a method but does not release
                  the method structure itself
  INPUTS       : 1) The generic function address
                 2) The method address
  RETURNS      : Nothing useful
  SIDE EFFECTS : Nodes deallocated
  NOTES        : None
 ***************************************************/
void DestroyMethodInfo(
        Environment *theEnv,
        Defgeneric *gfunc,
        Defmethod *meth) {
    int j;
    RESTRICTION *rptr;
#if MAC_XCD
#pragma unused(gfunc)
#endif

    ReturnPackedExpression(theEnv, meth->actions);

    ClearUserDataList(theEnv, meth->header.usrData);
    if (meth->header.ppForm != nullptr)
        rm(theEnv, (void *) meth->header.ppForm, (sizeof(char) * (strlen(meth->header.ppForm) + 1)));
    for (j = 0; j < meth->restrictionCount; j++) {
        rptr = &meth->restrictions[j];

        if (rptr->types != nullptr)
            rm(theEnv, rptr->types, (sizeof(void *) * rptr->tcnt));
        ReturnPackedExpression(theEnv, rptr->query);
    }

    if (meth->restrictions != nullptr)
        rm(theEnv, meth->restrictions,
           (sizeof(RESTRICTION) * meth->restrictionCount));
}

/***************************************************
  NAME         : MethodsExecuting
  DESCRIPTION  : Determines if any of the methods of
                   a generic function are currently
                   executing
  INPUTS       : The generic function address
  RETURNS      : True if any methods are executing,
                   false otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
bool MethodsExecuting(
        Defgeneric *gfunc) {
    long i;

    for (i = 0; i < gfunc->mcnt; i++)
        if (gfunc->methods[i].busy > 0)
            return true;
    return false;
}

/*****************************************************
  NAME         : FindMethodByIndex
  DESCRIPTION  : Finds a generic function method of
                   specified index
  INPUTS       : 1) The generic function
                 2) The index
  RETURNS      : The position of the method in the
                   generic function's method array,
                   -1 if not found
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
unsigned short FindMethodByIndex(
        Defgeneric *gfunc,
        unsigned short theIndex) {
    unsigned short i;

    for (i = 0; i < gfunc->mcnt; i++) {
        if (gfunc->methods[i].index == theIndex) { return i; }
    }

    return METHOD_NOT_FOUND;
}

#if DEBUGGING_FUNCTIONS || PROFILING_FUNCTIONS

/******************************************************************
  NAME         : PrintMethod
  DESCRIPTION  : Lists a brief description of methods for a method
  INPUTS       : 1) Buffer for method info
                 2) Size of buffer (not including space for '\0')
                 3) The method address
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : A terminating newline is NOT included
 ******************************************************************/
void PrintMethod(
        Environment *theEnv,
        Defmethod *meth,
        StringBuilder *theSB) {
#if MAC_XCD
#pragma unused(theEnv)
#endif
    unsigned short j, k;
    RESTRICTION *rptr;
    char numbuf[15];

    SBReset(theSB);
    if (meth->system)
        SBAppend(theSB, "SYS");
    gensprintf(numbuf, "%-2hu ", meth->index);
    SBAppend(theSB, numbuf);
    for (j = 0; j < meth->restrictionCount; j++) {
        rptr = &meth->restrictions[j];
        if (((j + 1) == meth->restrictionCount) && (meth->maxRestrictions == RESTRICTIONS_UNBOUNDED)) {
            if ((rptr->tcnt == 0) && (rptr->query == nullptr)) {
                SBAppend(theSB, "$?");
                break;
            }
            SBAppend(theSB, "($? ");
        } else
            SBAppend(theSB, "(");
        for (k = 0; k < rptr->tcnt; k++) {
            SBAppend(theSB, DefclassName((Defclass *) rptr->types[k]));
            if ((k + 1) < rptr->tcnt)
                SBAppend(theSB, " ");
        }
        if (rptr->query != nullptr) {
            if (rptr->tcnt != 0)
                SBAppend(theSB, " ");
            SBAppend(theSB, "<qry>");
        }
        SBAppend(theSB, ")");
        if ((j + 1) != meth->restrictionCount)
            SBAppend(theSB, " ");
    }
}

#endif /* DEBUGGING_FUNCTIONS || PROFILING_FUNCTIONS */

#if DEBUGGING_FUNCTIONS

/*************************************************************
  NAME         : PreviewGeneric
  DESCRIPTION  : Allows the user to see a printout of all the
                   applicable methods for a particular generic
                   function call
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Any side-effects of evaluating the generic
                   function arguments
                 and evaluating query-functions to determine
                   the set of applicable methods
  NOTES        : H/L Syntax: (preview-generic <func> <args>)
 *************************************************************/
void PreviewGeneric(
        Environment *theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    Defgeneric *gfunc;
    Defgeneric *previousGeneric;
    bool oldce;
    UDFValue theArg;

    EvaluationData(theEnv)->EvaluationError = false;
    if (!UDFFirstArgument(context, SYMBOL_BIT, &theArg)) return;

    gfunc = LookupDefgenericByMdlOrScope(theEnv, theArg.lexemeValue->contents);
    if (gfunc == nullptr) {
        PrintErrorID(theEnv, "GENRCFUN", 3, false);
        WriteString(theEnv, STDERR, "Unable to find generic function '");
        WriteString(theEnv, STDERR, theArg.lexemeValue->contents);
        WriteString(theEnv, STDERR, "' in function preview-generic.\n");
        return;
    }
    oldce = ExecutingConstruct(theEnv);
    SetExecutingConstruct(theEnv, true);
    previousGeneric = DefgenericData(theEnv)->CurrentGeneric;
    DefgenericData(theEnv)->CurrentGeneric = gfunc;
    EvaluationData(theEnv)->CurrentEvaluationDepth++;
    PushProcParameters(theEnv, GetFirstArgument()->nextArg,
                       CountArguments(GetFirstArgument()->nextArg),
                       DefgenericName(gfunc), "generic function",
                       UnboundMethodErr);
    if (EvaluationData(theEnv)->EvaluationError) {
        PopProcParameters(theEnv);
        DefgenericData(theEnv)->CurrentGeneric = previousGeneric;
        EvaluationData(theEnv)->CurrentEvaluationDepth--;
        SetExecutingConstruct(theEnv, oldce);
        return;
    }
    gfunc->busy++;
    DisplayGenericCore(theEnv, gfunc);
    gfunc->busy--;
    PopProcParameters(theEnv);
    DefgenericData(theEnv)->CurrentGeneric = previousGeneric;
    EvaluationData(theEnv)->CurrentEvaluationDepth--;
    SetExecutingConstruct(theEnv, oldce);
}

#endif /* DEBUGGING_FUNCTIONS */

/***************************************************
  NAME         : CheckGenericExists
  DESCRIPTION  : Finds the address of named
                  generic function and prints out
                  error message if not found
  INPUTS       : 1) Calling function
                 2) Name of generic function
  RETURNS      : Generic function address (nullptr if
                   not found)
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
Defgeneric *CheckGenericExists(
        Environment *theEnv,
        const char *fname,
        const char *gname) {
    Defgeneric *gfunc;

    gfunc = LookupDefgenericByMdlOrScope(theEnv, gname);
    if (gfunc == nullptr) {
        PrintErrorID(theEnv, "GENRCFUN", 3, false);
        WriteString(theEnv, STDERR, "Unable to find generic function '");
        WriteString(theEnv, STDERR, gname);
        WriteString(theEnv, STDERR, "' in function '");
        WriteString(theEnv, STDERR, fname);
        WriteString(theEnv, STDERR, "'.\n");
        SetEvaluationError(theEnv, true);
    }
    return (gfunc);
}

/***************************************************
  NAME         : CheckMethodExists
  DESCRIPTION  : Finds the array index of the
                  specified method and prints out
                  error message if not found
  INPUTS       : 1) Calling function
                 2) Generic function address
                 3) Index of method
  RETURNS      : Method array index (METHOD_NOT_FOUND if not found)
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
unsigned short CheckMethodExists(
        Environment *theEnv,
        const char *fname,
        Defgeneric *gfunc,
        unsigned short mi) {
    unsigned short fi;

    fi = FindMethodByIndex(gfunc, mi);
    if (fi == METHOD_NOT_FOUND) {
        PrintErrorID(theEnv, "GENRCFUN", 2, false);
        WriteString(theEnv, STDERR, "Unable to find method '");
        WriteString(theEnv, STDERR, DefgenericName(gfunc));
        WriteString(theEnv, STDERR, "' #");
        PrintUnsignedInteger(theEnv, STDERR, mi);
        WriteString(theEnv, STDERR, " in function '");
        WriteString(theEnv, STDERR, fname);
        WriteString(theEnv, STDERR, "'.\n");
        SetEvaluationError(theEnv, true);
    }
    return fi;
}

/******************************************************
  NAME         : PrintGenericName
  DESCRIPTION  : Prints the name of a gneric function
                 (including the module name if the
                  generic is not in the current module)
  INPUTS       : 1) The logical name of the output
                 2) The generic functions
  RETURNS      : Nothing useful
  SIDE EFFECTS : Generic name printed
  NOTES        : None
 ******************************************************/
void PrintGenericName(
        Environment *theEnv,
        const char *logName,
        Defgeneric *gfunc) {
    if (gfunc->header.whichModule->theModule != GetCurrentModule(theEnv)) {
        WriteString(theEnv, logName, DefgenericModule(gfunc));
        WriteString(theEnv, logName, "::");
    }
    WriteString(theEnv, logName, gfunc->header.name->contents);
}

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if DEBUGGING_FUNCTIONS

/*********************************************************
  NAME         : DisplayGenericCore
  DESCRIPTION  : Prints out a description of a core
                   frame of applicable methods for
                   a particular call of a generic function
  INPUTS       : The generic function
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 *********************************************************/
static void DisplayGenericCore(
        Environment *theEnv,
        Defgeneric *gfunc) {
    long i;
    bool rtn = false;
    StringBuilder *theSB;

    theSB = CreateStringBuilder(theEnv, 256);

    for (i = 0; i < gfunc->mcnt; i++) {
        gfunc->methods[i].busy++;
        if (IsMethodApplicable(theEnv, &gfunc->methods[i])) {
            rtn = true;
            WriteString(theEnv, STDOUT, DefgenericName(gfunc));
            WriteString(theEnv, STDOUT, " #");
            PrintMethod(theEnv, &gfunc->methods[i], theSB);
            WriteString(theEnv, STDOUT, theSB->contents);
            WriteString(theEnv, STDOUT, "\n");
        }
        gfunc->methods[i].busy--;
    }
    if (rtn == false) {
        WriteString(theEnv, STDOUT, "No applicable methods for ");
        WriteString(theEnv, STDOUT, DefgenericName(gfunc));
        WriteString(theEnv, STDOUT, ".\n");
    }

    SBDispose(theSB);
}

#endif

#endif

