/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  10/11/17             */
/*                                                     */
/*                    MEMORY MODULE                    */
/*******************************************************/

/*************************************************************/
/* Purpose: Memory allocation routines.                      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed HaltExecution check from the           */
/*            EnvReleaseMem function. DR0863                 */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove compiler warnings.    */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems.                   */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Removed genlongalloc/genlongfree functions.    */
/*                                                           */
/*            Added get_mem and rtn_mem macros.              */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Removed deallocating message parameter from    */
/*            EnvReleaseMem.                                 */
/*                                                           */
/*            Removed support for BLOCK_MEMORY.              */
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
/*************************************************************/

#include <cstdio>

#include "Setup.h"

#include "Constants.h"
#include "Environment.h"
#include "MemoryAllocation.h"
#include "PrintUtility.h"
#include "Router.h"
#include "Utility.h"

#include <cstdlib>
#include <memory>

/***************************************************/
/* genalloc: A generic memory allocation function. */
/***************************************************/
void *genalloc(
        const Environment::Ptr&theEnv,
        size_t size) {
    return nullptr;
}

/****************************************************/
/* genfree: A generic memory deallocation function. */
/****************************************************/
void genfree(
        const Environment::Ptr&theEnv,
        void *waste,
        size_t size) {
}

/******************************************************/
/* genrealloc: Simple (i.e. dumb) version of realloc. */
/******************************************************/
void *genrealloc(
        const Environment::Ptr&theEnv,
        void *oldaddr,
        size_t oldsz,
        size_t newsz) {
    return nullptr;
}

/*****************************************************/
/* gm1: Allocates memory and sets all bytes to zero. */
/*****************************************************/
void *gm1(
        const Environment::Ptr&theEnv,
        size_t size) {
    return nullptr;
}

/*****************************************************/
/* gm2: Allocates memory and does not initialize it. */
/*****************************************************/
void *gm2(
        const Environment::Ptr&theEnv,
        size_t size) {
    return nullptr;
}

/****************************************/
/* rm: Returns a block of memory to the */
/*   maintained pool of free memory.    */
/****************************************/
void rm(
        const Environment::Ptr&theEnv,
        void *str,
        size_t size) {
    /// @todo eliminate this
}

