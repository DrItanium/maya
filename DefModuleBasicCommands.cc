/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  07/02/18             */
/*                                                     */
/*         DEFMODULE BASIC COMMANDS HEADER FILE        */
/*******************************************************/

/*************************************************************/
/* Purpose: Implements core commands for the defmodule       */
/*   construct such as clear, reset, save, ppdefmodule       */
/*   list-defmodules, and get-defmodule-list.                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
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

#include "Setup.h"

#include <cstdio>
#include <cstring>

#include "ArgumentAccess.h"
#include "BinaryLoad.h"
#include "Construct.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "DefmoduleBinarySaveLoad.h"
#include "Multifield.h"
#include "PrintUtility.h"
#include "Router.h"

#include "DefModuleBasicCommands.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void ClearDefmodules(Environment *, void *);
#if DEFMODULE_CONSTRUCT
static void SaveDefmodules(Environment *, Defmodule *, const char *, void *);
#endif

/*****************************************************************/
/* DefmoduleBasicCommands: Initializes basic defmodule commands. */
/*****************************************************************/
void DefmoduleBasicCommands(
        Environment *theEnv) {
    AddClearFunction(theEnv, "defmodule", ClearDefmodules, 2000, nullptr);

#if DEFMODULE_CONSTRUCT
    AddSaveFunction(theEnv, "defmodule", SaveDefmodules, 1100, nullptr);

    AddUDF(theEnv, "get-defmodule-list", "m", 0, 0, nullptr, GetDefmoduleListFunction, nullptr);

#if DEBUGGING_FUNCTIONS
    AddUDF(theEnv, "list-defmodules", "v", 0, 0, nullptr, ListDefmodulesCommand, nullptr);
    AddUDF(theEnv, "ppdefmodule", "v", 1, 2, ";y;ldsyn", PPDefmoduleCommand, nullptr);
#endif
#endif

#if (BLOAD_AND_BSAVE)
    DefmoduleBinarySetup(theEnv);
#endif

}

/*********************************************************/
/* ClearDefmodules: Defmodule clear routine for use with */
/*   the clear command. Creates the MAIN module.         */
/*********************************************************/
static void ClearDefmodules(
        Environment *theEnv,
        void *context) {
#if (BLOAD_AND_BSAVE)
    if (Bloaded(theEnv)) return;
#endif
    RemoveAllDefmodules(theEnv, nullptr);

    CreateMainModule(theEnv, nullptr);
    DefmoduleData(theEnv)->MainModuleRedefinable = true;
}

#if DEFMODULE_CONSTRUCT

/******************************************/
/* SaveDefmodules: Defmodule save routine */
/*   for use with the save command.       */
/******************************************/
static void SaveDefmodules(
        Environment *theEnv,
        Defmodule *theModule,
        const char *logicalName,
        void *context) {
    const char *ppform;

    ppform = DefmodulePPForm(theModule);
    if (ppform != nullptr) {
        WriteString(theEnv, logicalName, ppform);
        WriteString(theEnv, logicalName, "\n");
    }
}

/************************************************/
/* GetDefmoduleListFunction: H/L access routine */
/*   for the get-defmodule-list function.       */
/************************************************/
void GetDefmoduleListFunction(
        Environment *theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    CLIPSValue result;

    GetDefmoduleList(theEnv, &result);
    CLIPSToUDFValue(&result, returnValue);
}

/******************************************/
/* GetDefmoduleList: C access routine     */
/*   for the get-defmodule-list function. */
/******************************************/
void GetDefmoduleList(
        Environment *theEnv,
        CLIPSValue *returnValue) {
    Defmodule *theConstruct;
    unsigned long count = 0;
    Multifield *theList;

    /*====================================*/
    /* Determine the number of constructs */
    /* of the specified type.             */
    /*====================================*/

    for (theConstruct = GetNextDefmodule(theEnv, nullptr);
         theConstruct != nullptr;
         theConstruct = GetNextDefmodule(theEnv, theConstruct)) { count++; }

    /*===========================*/
    /* Create a multifield large */
    /* enough to store the list. */
    /*===========================*/

    theList = CreateMultifield(theEnv, count);
    returnValue->value = theList;

    /*====================================*/
    /* Store the names in the multifield. */
    /*====================================*/

    for (theConstruct = GetNextDefmodule(theEnv, nullptr), count = 0;
         theConstruct != nullptr;
         theConstruct = GetNextDefmodule(theEnv, theConstruct), count++) {
        if (EvaluationData(theEnv)->HaltExecution) {
            returnValue->multifieldValue = CreateMultifield(theEnv, 0L);
            return;
        }
        theList->contents[count].lexemeValue = CreateSymbol(theEnv, DefmoduleName(theConstruct));
    }
}

#if DEBUGGING_FUNCTIONS

/********************************************/
/* PPDefmoduleCommand: H/L access routine   */
/*   for the ppdefmodule command.           */
/********************************************/
void PPDefmoduleCommand(
        Environment *theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    const char *defmoduleName;
    const char *logicalName;
    const char *ppForm;

    defmoduleName = GetConstructName(context, "ppdefmodule", "defmodule name");
    if (defmoduleName == nullptr) return;

    if (UDFHasNextArgument(context)) {
        logicalName = GetLogicalName(context, STDOUT);
        if (logicalName == nullptr) {
            IllegalLogicalNameMessage(theEnv, "ppdefmodule");
            SetHaltExecution(theEnv, true);
            SetEvaluationError(theEnv, true);
            return;
        }
    } else { logicalName = STDOUT; }

    if (strcmp(logicalName, "nil") == 0) {
        ppForm = PPDefmoduleNil(theEnv, defmoduleName);

        if (ppForm == nullptr) { CantFindItemErrorMessage(theEnv, "defmodule", defmoduleName, true); }

        returnValue->lexemeValue = CreateString(theEnv, ppForm);

        return;
    }

    PPDefmodule(theEnv, defmoduleName, logicalName);

    return;
}

/****************************************/
/* PPDefmoduleNil: C access routine for */
/*   the ppdefmodule command.           */
/****************************************/
const char *PPDefmoduleNil(
        Environment *theEnv,
        const char *defmoduleName) {
    Defmodule *defmodulePtr;

    defmodulePtr = FindDefmodule(theEnv, defmoduleName);
    if (defmodulePtr == nullptr) {
        CantFindItemErrorMessage(theEnv, "defmodule", defmoduleName, true);
        return nullptr;
    }

    if (DefmodulePPForm(defmodulePtr) == nullptr) return "";

    return DefmodulePPForm(defmodulePtr);
}

/*************************************/
/* PPDefmodule: C access routine for */
/*   the ppdefmodule command.        */
/*************************************/
bool PPDefmodule(
        Environment *theEnv,
        const char *defmoduleName,
        const char *logicalName) {
    Defmodule *defmodulePtr;

    defmodulePtr = FindDefmodule(theEnv, defmoduleName);
    if (defmodulePtr == nullptr) {
        CantFindItemErrorMessage(theEnv, "defmodule", defmoduleName, true);
        return false;
    }

    if (DefmodulePPForm(defmodulePtr) == nullptr) return true;
    WriteString(theEnv, logicalName, DefmodulePPForm(defmodulePtr));

    return true;
}

/***********************************************/
/* ListDefmodulesCommand: H/L access routine   */
/*   for the list-defmodules command.          */
/***********************************************/
void ListDefmodulesCommand(
        Environment *theEnv,
        UDFContext *context,
        UDFValue *returnValue) {
    ListDefmodules(theEnv, STDOUT);
}

/**************************************/
/* ListDefmodules: C access routine   */
/*   for the list-defmodules command. */
/**************************************/
void ListDefmodules(
        Environment *theEnv,
        const char *logicalName) {
    Defmodule *theModule;
    unsigned int count = 0;

    for (theModule = GetNextDefmodule(theEnv, nullptr);
         theModule != nullptr;
         theModule = GetNextDefmodule(theEnv, theModule)) {
        WriteString(theEnv, logicalName, DefmoduleName(theModule));
        WriteString(theEnv, logicalName, "\n");
        count++;
    }

    PrintTally(theEnv, logicalName, count, "defmodule", "defmodules");
}

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFMODULE_CONSTRUCT */


