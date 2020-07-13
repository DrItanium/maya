/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  10/18/16             */
/*                                                     */
/*                ENVIRONMENT MODULE                   */
/*******************************************************/

/*************************************************************/
/* Purpose: Routines for supporting multiple environments.   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added code to CreateEnvironment to free        */
/*            already allocated data if one of the malloc    */
/*            calls fail.                                    */
/*                                                           */
/*            Modified AllocateEnvironmentData to print a    */
/*            message if it was unable to allocate memory.   */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added CreateRuntimeEnvironment function.       */
/*                                                           */
/*            Added support for context information when an  */
/*            environment is created (i.e a pointer from the */
/*            CLIPS environment to its parent environment).  */
/*                                                           */
/*      6.30: Added support for passing context information  */
/*            to user defined functions and callback         */
/*            functions.                                     */
/*                                                           */
/*            Support for hashing EXTERNAL_ADDRESS_TYPE      */
/*            data type.                                     */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Removed deallocating message parameter from    */
/*            EnvReleaseMem.                                 */
/*                                                           */
/*            Removed support for BLOCK_MEMORY.              */
/*                                                           */
/*      6.40: Refactored code to reduce header dependencies  */
/*            in sysdep.c.                                   */
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
/*            Eval support for run time and bload only.      */
/*                                                           */
/*************************************************************/

#include "Environment.h"
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <iostream>

#include "Setup.h"

#include "BasicMathFunctions.h"
#include "CommandLine.h"
#include "ExtendedMathFunctions.h"
#include "Environment.h"
#include "Engine.h"
#include "File.h"
#include "IOFunctions.h"
#include "MemoryAllocation.h"
#include "MiscFunctions.h"
#include "Multifield.h"
#include "ParsingFunctions.h"
#include "PrettyPrint.h"
#include "ProceduralCodeSupportRoutines.h"
#include "ProceduralFunctions.h"
#include "PredicateFunctions.h"
#include "PrintUtility.h"
#include "ConstructProfilingFunctions.h"
#include "Router.h"
#include "SortingFunctions.h"
#include "StringFunctions.h"
#include "SystemDependency.h"
#include "Utility.h"
#include "Watch.h"


#if DEFFACTS_CONSTRUCT
#include "Deffacts.h"
#endif

#include "Defrule.h"

#if DEFGENERIC_CONSTRUCT
#include "GenericFunctionCommands.h"
#endif

#if DEFFUNCTION_CONSTRUCT
#include "Deffunction.h"
#endif

#if DEFGLOBAL_CONSTRUCT
#include "Defglobal.h"
#endif

#if DEFTEMPLATE_CONSTRUCT
#include "Deftemplate.h"
#endif

#include "ClassInitialization.h"

#if DEVELOPER
#include "developr.h"
#endif

constexpr auto SIZE_ENVIRONMENT_HASH = 131;

void
EnvironmentModule::onClear() noexcept {
   // by default do nothing
}

void
EnvironmentModule::onReset() noexcept {

}



/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

extern void UserFunctions(const Environment&);

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static void RemoveEnvironmentCleanupFunctions(const Environment&);
static Environment CreateEnvironmentDriver();
static void SystemFunctionDefinitions(const Environment&);
static void InitializeEnvironment(const Environment&);

/************************************************************/
/* CreateEnvironment: Creates an environment data structure */
/*   and initializes its content to zero/nullptr.              */
/************************************************************/
Environment CreateEnvironment() {
    return CreateEnvironmentDriver();
}

/*********************************************************/
/* CreateEnvironmentDriver: Creates an environment data  */
/*   structure and initializes its content to zero/nullptr. */
/*********************************************************/
Environment CreateEnvironmentDriver() {
    Environment theEnvironment = std::make_shared<EnvironmentData>();
    if (!theEnvironment) {
        std::cout << "\n[ENVRNMNT5] Unable to create new environment.\n";
        return nullptr;
    }

    /*=============================================*/
    /* Allocate storage for the cleanup functions. */
    /*=============================================*/
    InitializeEnvironment(theEnvironment);
    CleanCurrentGarbageFrame(theEnvironment, nullptr);

    return theEnvironment;
}

/**********************************************/
/* DestroyEnvironment: Destroys the specified */
/*   environment returning all of its memory. */
/**********************************************/
bool DestroyEnvironment(Environment& theEnvironment) {
    struct environmentCleanupFunction *cleanupPtr;
    int i;
    bool rv = true;
    /// @todo fix this
#if STUBBING_INACTIVE
    const auto& theMemData = MemoryData(theEnvironment);


    ReleaseMem(theEnvironment, -1);
    for (i = 0; i < MAXIMUM_ENVIRONMENT_POSITIONS; i++) {
        if (theEnvironment->cleanupFunctions[i] != nullptr) { (*theEnvironment->cleanupFunctions[i])(theEnvironment); }
    }

    free(theEnvironment->cleanupFunctions);

    for (cleanupPtr = theEnvironment->listOfCleanupEnvironmentFunctions;
         cleanupPtr != nullptr;
         cleanupPtr = cleanupPtr->next) { (*cleanupPtr->func)(theEnvironment); }

    RemoveEnvironmentCleanupFunctions(theEnvironment);

    ReleaseMem(theEnvironment, -1);

    if ((theMemData->MemoryAmount != 0) || (theMemData->MemoryCalls != 0)) {
        std::cout << "\n[ENVRNMNT8] Environment data not fully deallocated.\n";
        std::cout << "\n[ENVRNMNT8] MemoryAmount = " << theMemData->MemoryAmount << '\n';
        std::cout << "\n[ENVRNMNT8] MemoryCalls = " << theMemData->MemoryCalls<< '\n';
        rv = false;
    }

#if (MEM_TABLE_SIZE > 0)
    free(theMemData->MemoryTable);
#endif

    for (i = 0; i < MAXIMUM_ENVIRONMENT_POSITIONS; i++) {
        if (theEnvironment->theData[i] != nullptr) {
            free(theEnvironment->theData[i]);
            theEnvironment->theData[i] = nullptr;
        }
    }

    free(theEnvironment->theData);

    theEnvironment.reset();
    //free(theEnvironment);
#endif
    return rv;
}

/**************************************************/
/* RemoveEnvironmentCleanupFunctions: Removes the */
/*   list of environment cleanup functions.       */
/**************************************************/
static void RemoveEnvironmentCleanupFunctions(
        const Environment& theEnv) {
    struct environmentCleanupFunction *nextPtr;
#if STUBBING_INACTIVE
    while (theEnv->listOfCleanupEnvironmentFunctions != nullptr) {
        nextPtr = theEnv->listOfCleanupEnvironmentFunctions->next;
        free(theEnv->listOfCleanupEnvironmentFunctions);
        theEnv->listOfCleanupEnvironmentFunctions = nextPtr;
    }
#endif
}

/**************************************************/
/* InitializeEnvironment: Performs initialization */
/*   of the KB environment.                       */
/**************************************************/
static void InitializeEnvironment(const Environment&theEnvironment) {
    /*================================================*/
    /* Don't allow the initialization to occur twice. */
    /*================================================*/

    if (theEnvironment->initialized) return;

    /*===================================================*/
    /* Initialize environment data for various features. */
    /*===================================================*/

    InitializeCommandLineData(theEnvironment);
    InitializeConstructData(theEnvironment);
    InitializeEvaluationData(theEnvironment);
    InitializeExternalFunctionData(theEnvironment);
    InitializePrettyPrintData(theEnvironment);
    InitializePrintUtilityData(theEnvironment);
    InitializeScannerData(theEnvironment);
    InitializeSystemDependentData(theEnvironment);
    InitializeUserDataData(theEnvironment);
    InitializeUtilityData(theEnvironment);
#if DEBUGGING_FUNCTIONS
    InitializeWatchData(theEnvironment);
#endif

    /*===============================================*/
    /* Initialize the hash tables for atomic values. */
    /*===============================================*/

    InitializeAtomTables(theEnvironment);


    /*=========================================*/
    /* Initialize file and string I/O routers. */
    /*=========================================*/

    InitializeDefaultRouters(theEnvironment);

    /*=========================================================*/
    /* Initialize some system dependent features such as time. */
    /*=========================================================*/

    InitializeNonportableFeatures(theEnvironment);

    /*=============================================*/
    /* Register system and user defined functions. */
    /*=============================================*/

    SystemFunctionDefinitions(theEnvironment);
    UserFunctions(theEnvironment);

    /*====================================*/
    /* Initialize the constraint manager. */
    /*====================================*/

    InitializeConstraints(theEnvironment);

    /*==========================================*/
    /* Initialize the expression hash table and */
    /* pointers to specific functions.          */
    /*==========================================*/

    InitExpressionData(theEnvironment);

    /*===================================*/
    /* Initialize the construct manager. */
    /*===================================*/

    InitializeConstructs(theEnvironment);

    /*=====================================*/
    /* Initialize the defmodule construct. */
    /*=====================================*/

    AllocateDefmoduleGlobals(theEnvironment);

    /*===================================*/
    /* Initialize the defrule construct. */
    /*===================================*/

    InitializeDefrules(theEnvironment);

    /*====================================*/
    /* Initialize the deffacts construct. */
    /*====================================*/

#if DEFFACTS_CONSTRUCT
    InitializeDeffacts(theEnvironment);
#endif

    /*=====================================================*/
    /* Initialize the defgeneric and defmethod constructs. */
    /*=====================================================*/

#if DEFGENERIC_CONSTRUCT
    SetupGenericFunctions(theEnvironment);
#endif

    /*=======================================*/
    /* Initialize the deffunction construct. */
    /*=======================================*/

#if DEFFUNCTION_CONSTRUCT
    SetupDeffunctions(theEnvironment);
#endif

    /*=====================================*/
    /* Initialize the defglobal construct. */
    /*=====================================*/

#if DEFGLOBAL_CONSTRUCT
    InitializeDefglobals(theEnvironment);
#endif

    /*=======================================*/
    /* Initialize the deftemplate construct. */
    /*=======================================*/

#if DEFTEMPLATE_CONSTRUCT
    InitializeDeftemplates(theEnvironment);
#endif

    /*=============================*/
    /* Initialize COOL constructs. */
    /*=============================*/
    SetupObjectSystem(theEnvironment);

    /*=====================================*/
    /* Initialize the defmodule construct. */
    /*=====================================*/

    InitializeDefmodules(theEnvironment);

    /*======================================================*/
    /* Register commands and functions for development use. */
    /*======================================================*/

#if DEVELOPER
    DeveloperCommands(theEnvironment);
#endif

    /*=========================================*/
    /* Install the special function primitives */
    /* used by procedural code in constructs.  */
    /*=========================================*/

    InstallProcedurePrimitives(theEnvironment);

    /*========================*/
    /* Issue a clear command. */
    /*========================*/
    Clear(theEnvironment);

    /*=============================*/
    /* Initialization is complete. */
    /*=============================*/

    theEnvironment->initialized = true;
}

/**************************************************/
/* SystemFunctionDefinitions: Sets up definitions */
/*   of system defined functions.                 */
/**************************************************/
static void SystemFunctionDefinitions(
        const Environment&theEnv) {
    ProceduralFunctionDefinitions(theEnv);
    MiscFunctionDefinitions(theEnv);

#if IO_FUNCTIONS
    IOFunctionDefinitions(theEnv);
#endif

    PredicateFunctionDefinitions(theEnv);
    BasicMathFunctionDefinitions(theEnv);
    FileCommandDefinitions(theEnv);
    SortFunctionDefinitions(theEnv);

#if DEBUGGING_FUNCTIONS
    WatchFunctionDefinitions(theEnv);
#endif

#if MULTIFIELD_FUNCTIONS
    MultifieldFunctionDefinitions(theEnv);
#endif

#if STRING_FUNCTIONS
    StringFunctionDefinitions(theEnv);
#endif

#if EXTENDED_MATH_FUNCTIONS
    ExtendedMathFunctionDefinitions(theEnv);
#endif

#if PROFILING_FUNCTIONS
    ConstructProfilingFunctionDefinitions(theEnv);
#endif
    ParseFunctionDefinitions(theEnv);
}

