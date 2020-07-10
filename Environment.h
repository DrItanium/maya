/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  10/18/16            */
/*                                                     */
/*                ENVRNMNT HEADER FILE                 */
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
/*            Support for hashing EXTERNAL_ADDRESS_TYPE data */
/*            type.                                          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Refactored code to reduce header dependencies  */
/*            in sysdep.c.                                   */
/*                                                           */
/*            Removed LOCALE definition.                     */
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

#ifndef _H_envrnmnt

#pragma once

#define _H_envrnmnt

#include <stdbool.h>
#include <cstdlib>
#include <memory>

//typedef struct environmentData Environment;

using Environment = std::shared_ptr<struct environmentData>;
typedef void EnvironmentCleanupFunction(const Environment&);

#include "Entities.h"
#include "ExternalFunctions.h"

constexpr auto USER_ENVIRONMENT_DATA = 70;
constexpr auto MAXIMUM_ENVIRONMENT_POSITIONS = 100;

struct environmentCleanupFunction {
    const char *name;
    void (*func)(const Environment&);
    int priority;
    struct environmentCleanupFunction *next;
};

struct environmentData {
    bool initialized: 1;
    void *context;
    CLIPSLexeme *TrueSymbol;
    CLIPSLexeme *FalseSymbol;
    CLIPSVoid *VoidConstant;
    void **theData;
    void (**cleanupFunctions)(const Environment&);
    environmentCleanupFunction *listOfCleanupEnvironmentFunctions;
    environmentData *next;
};


inline auto VoidConstant(const Environment& theEnv) noexcept { return theEnv->VoidConstant; }
inline auto FalseSymbol(const Environment& theEnv) noexcept { return theEnv->FalseSymbol; }
inline auto TrueSymbol(const Environment& theEnv) noexcept { return theEnv->TrueSymbol; }

inline void* getEnvironmentData(const Environment& theEnv, size_t position) noexcept {
    return theEnv->theData[position];
}
inline void setEnvironmentData(const Environment& theEnv, size_t position, void* value) noexcept {
    theEnv->theData[position] = value;
}
#define GetEnvironmentData(theEnv, position) (getEnvironmentData(theEnv, position))
#define SetEnvironmentData(theEnv, position, value) (setEnvironmentData(theEnv, position, value))

bool AllocateEnvironmentData(const Environment&, unsigned, size_t, EnvironmentCleanupFunction * = nullptr);
bool AddEnvironmentCleanupFunction(const Environment&, const char *, EnvironmentCleanupFunction *, int);
void *GetEnvironmentContext(const Environment&);
void *SetEnvironmentContext(const Environment&, void *);

Environment CreateEnvironment();
Environment CreateRuntimeEnvironment(CLIPSLexeme **, CLIPSFloat **,
                                      CLIPSInteger **, CLIPSBitMap **,
                                      struct functionDefinition *);
bool DestroyEnvironment(const Environment&);

#endif /* _H_envrnmnt */

