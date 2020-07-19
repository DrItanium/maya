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

#include "Setup.h"
#include "Router.h"
#if 0
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
//#include "PrintUtility.h"
#include "ConstructProfilingFunctions.h"
#include "SortingFunctions.h"
//#include "StringFunctions.h"
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

#include "ClassFunctions.h"

#if DEVELOPER
#include "developr.h"
#endif
#endif
#ifndef SYMBOL_HASH_SIZE
#define SYMBOL_HASH_SIZE       63559L
#endif

#ifndef FLOAT_HASH_SIZE
#define FLOAT_HASH_SIZE         8191
#endif

#ifndef INTEGER_HASH_SIZE
#define INTEGER_HASH_SIZE       8191
#endif

#ifndef BITMAP_HASH_SIZE
#define BITMAP_HASH_SIZE        8191
#endif

#ifndef EXTERNAL_ADDRESS_HASH_SIZE
#define EXTERNAL_ADDRESS_HASH_SIZE        8191
#endif
namespace maya {

    void
    EnvironmentModule::onClear() noexcept {
        // by default do nothing
    }

    void
    EnvironmentModule::onReset() noexcept {

    }

    Environment::Ptr
    Environment::create() {
        return std::make_shared<Environment>();
    }
    Environment::Environment() : _voidConstant(std::make_shared<Void>(*this)){
        _positiveInfinity = createSymbol(PositiveInfinityString);
        _negativeInfinity = createSymbol(NegativeInfinityString);
        _zero = createInteger(0);
        _trueSymbol = createSymbol(TrueString);
        _falseSymbol = createSymbol(FalseString);
        // setup the initial symbols
        /// @todo fix this code
        /*===================================================*/
        /* Initialize environment data for various features. */
        /*===================================================*/
#if STUBBING_INACTIVE
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

        ProceduralFunctionDefinitions(theEnvironment);
        MiscFunctionDefinitions(theEnvironment);

#if IO_FUNCTIONS
        IOFunctionDefinitions(theEnvironment);
#endif

        PredicateFunctionDefinitions(theEnvironment);
        BasicMathFunctionDefinitions(theEnvironment);
        FileCommandDefinitions(theEnvironment);
        SortFunctionDefinitions(theEnvironment);

#if DEBUGGING_FUNCTIONS
        WatchFunctionDefinitions(theEnvironment);
#endif

#if MULTIFIELD_FUNCTIONS
        MultifieldFunctionDefinitions(theEnvironment);
#endif

#if STRING_FUNCTIONS
        StringFunctionDefinitions(theEnvironment);
#endif

#if EXTENDED_MATH_FUNCTIONS
        ExtendedMathFunctionDefinitions(theEnvironment);
#endif

#if PROFILING_FUNCTIONS
        ConstructProfilingFunctionDefinitions(theEnvironment);
#endif
        ParseFunctionDefinitions(theEnvironment);
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
        /// allocate storage for cleanup
        CleanCurrentGarbageFrame(theEnvironment, nullptr);
#endif

    }

    void
    Environment::install(std::function<void(Environment &)> body) {
        body(*this);
    }


    Lexeme::Ptr
    Environment::createLexeme(const std::string &str, unsigned short type) {
        auto newLexeme = std::make_shared<Lexeme>(*this, str, type);
        auto hashCode = newLexeme->hash(SYMBOL_HASH_SIZE);
        /// see if the target symbol is in the table by finding the target hash entry
        auto range = _symbolTable.equal_range(hashCode);
        for (auto iter = range.first; iter != range.second; ++iter) {
            if (auto peek = iter->second; (peek->getType() == type) && (str == peek->getContents())) {
                return peek;
            }
        }

        // we did not find the symbol so instead, add it to the list
        // configure the rest of the design as well
        newLexeme->setBucket(hashCode);
        newLexeme->setIsPermanent(false);
        _symbolTable.emplace(hashCode, newLexeme);
        // in the CLIPS original code there is an addition to the list of ephemeral nodes.
        // This list of nodes is used to do garbage collection as we go through. I believe that
        // this is not necessary due to the fact that the shared_ptr keeps track of data destruction.
        // If we have a use_count of 1 then we destroy the target entry
        //
        // Anyway, return the newLexeme
        return newLexeme;
    }
    Float::Ptr
    Environment::createFloat(Float::BackingType value) {
        Float::Ptr newFloat = std::make_shared<Float>(*this, value) ;
        auto hash = newFloat->hash(FLOAT_HASH_SIZE);
        auto range = _floatTable.equal_range(hash);
        for(auto iter = range.first; iter != range.second; ++iter) {
            if (auto peek = iter->second; peek->getContents() == value) {
                return peek;
            }
        }
        newFloat->setBucket(hash);
        newFloat->setIsPermanent(false);
        /// @todo ephemeral hash garbage frame code was here, add it back in if it makes sense
        _floatTable.emplace(hash, newFloat);
        return newFloat;
    }
    Integer::Ptr
    Environment::createInteger(Integer::BackingType value) {
        Integer::Ptr newInteger = std::make_shared<Integer>(*this, value);
        auto hash = newInteger->hash(INTEGER_HASH_SIZE);
        auto range = _integerTable.equal_range(hash);
        for (auto iter = range.first; iter != range.second; ++iter) {
            if (auto peek = iter->second; peek->getContents() == value) {
                return peek;
            }
        }
        // not in the list
        newInteger->setBucket(hash);
        newInteger->setIsPermanent(false);
        // ephemeral hash node support was here
        _integerTable.emplace(hash, newInteger);
        return newInteger;
    }

    Lexeme::Ptr
    Environment::createBoolean(bool value) noexcept {
        return value ? _trueSymbol : _falseSymbol;
    }

}
