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
#include <vector>
#include <array>
#include <any>
#include <functional>
#include <tuple>
#include <iostream>
#include <list>

using Environment = std::shared_ptr<struct environmentData>;
//typedef void EnvironmentCleanupFunction(const Environment&);
using EnvironmentCleanupFunctionBody = std::function<void(const Environment&)>;

constexpr auto USER_ENVIRONMENT_DATA = 70;
constexpr auto MAXIMUM_ENVIRONMENT_POSITIONS = 100;

struct EnvironmentCleanupFunction {
    const char *name;
    EnvironmentCleanupFunctionBody func;
    int priority;
    EnvironmentCleanupFunction *next;
};
class EnvironmentModule {
public:
    EnvironmentModule() = default;
    virtual ~EnvironmentModule() = default;
};

template<typename T>
class EnvironmentModuleTypeToIndex final {
    EnvironmentModuleTypeToIndex() = delete;
    ~EnvironmentModuleTypeToIndex() = delete;
    EnvironmentModuleTypeToIndex(EnvironmentModuleTypeToIndex<T>&&) = delete;
    EnvironmentModuleTypeToIndex(const EnvironmentModuleTypeToIndex<T>&) = delete;
    EnvironmentModuleTypeToIndex<T>& operator=(const EnvironmentModuleTypeToIndex<T>&) = delete;
    EnvironmentModuleTypeToIndex<T>& operator=(EnvironmentModuleTypeToIndex<T>&&) = delete;
};

template<size_t index>
class EnvironmentModuleIndexToType final {
    EnvironmentModuleIndexToType() = delete;
    ~EnvironmentModuleIndexToType() = delete;
    EnvironmentModuleIndexToType(EnvironmentModuleIndexToType<index>&&) = delete;
    EnvironmentModuleIndexToType(const EnvironmentModuleIndexToType<index>&) = delete;
    EnvironmentModuleIndexToType<index>& operator=(const EnvironmentModuleIndexToType<index>&) = delete;
    EnvironmentModuleIndexToType<index>& operator=(EnvironmentModuleIndexToType<index>&&) = delete;
};

template<typename T>
constexpr size_t EnvironmentModuleIndex = EnvironmentModuleTypeToIndex<T>::index;
template<size_t index>
using EnvironmentModuleType = typename EnvironmentModuleIndexToType<index>::type;
#define RegisterEnvironmentModule(actual_type, pos) \
template<> class EnvironmentModuleTypeToIndex<actual_type> final { \
    EnvironmentModuleTypeToIndex() = delete; \
~EnvironmentModuleTypeToIndex() = delete; \
EnvironmentModuleTypeToIndex(EnvironmentModuleTypeToIndex< actual_type >&&) = delete;\
EnvironmentModuleTypeToIndex(const EnvironmentModuleTypeToIndex< actual_type >&) = delete; \
EnvironmentModuleTypeToIndex< actual_type >& operator=(const EnvironmentModuleTypeToIndex< actual_type >&) = delete; \
EnvironmentModuleTypeToIndex< actual_type >& operator=(EnvironmentModuleTypeToIndex< actual_type >&&) = delete; \
public: \
static constexpr size_t index = pos ; \
}; \
template<> class EnvironmentModuleIndexToType< pos > final { \
    EnvironmentModuleIndexToType() = delete; \
~EnvironmentModuleIndexToType() = delete; \
EnvironmentModuleIndexToType(EnvironmentModuleIndexToType< pos >&&) = delete;\
EnvironmentModuleIndexToType(const EnvironmentModuleIndexToType< pos >&) = delete; \
EnvironmentModuleIndexToType< pos >& operator=(const EnvironmentModuleIndexToType< pos >&) = delete; \
EnvironmentModuleIndexToType< pos >& operator=(EnvironmentModuleIndexToType< pos >&&) = delete; \
public: \
using type = actual_type ; \
}

struct CLIPSLexeme;
struct CLIPSVoid;
struct environmentData {
    bool initialized = false;
    void *context = nullptr;
    CLIPSLexeme *TrueSymbol = nullptr;
    CLIPSLexeme *FalseSymbol = nullptr;
    CLIPSVoid *VoidConstant = nullptr;
    std::array<std::unique_ptr<EnvironmentModule>, MAXIMUM_ENVIRONMENT_POSITIONS> environmentModules;
    std::list<EnvironmentCleanupFunction> listOfCleanupEnvironmentFunctions;
    environmentData *next = nullptr;
    template<typename T>
    bool installEnvironmentModule(std::unique_ptr<T>&& module) noexcept {
        constexpr auto position = EnvironmentModuleIndex<T>;
        if (position >= MAXIMUM_ENVIRONMENT_POSITIONS) {
            std::cout << "\n[ENVRNMNT2] Environment data position " << position << " exceeds the maximum allowed.\n";
            return false;
        }
        if (auto& ptr = environmentModules[position]; ptr) {
            // already populated, stop now
            std::cout << "\n[ENVRNMNT3] Environment data position " << position << " already allocated.\n";
            return false;
        }
        environmentModules[position] = std::move(module);
        return true;
    }
    template<size_t position>
    const std::unique_ptr<EnvironmentModuleType<position>>& getEnvironmentModule() {
        if (position >= MAXIMUM_ENVIRONMENT_POSITIONS) {
            std::cout << "\n[ENVRNMNT2] Environment data position " << position << " exceeds the maximum allowed.\n";
            throw 44;
        } else {
            auto& thing = environmentModules[position];
            return (std::unique_ptr<EnvironmentModuleType<position>>&)thing;
        }
    }

};


inline auto VoidConstant(const Environment& theEnv) noexcept { return theEnv->VoidConstant; }
inline auto FalseSymbol(const Environment& theEnv) noexcept { return theEnv->FalseSymbol; }
inline auto TrueSymbol(const Environment& theEnv) noexcept { return theEnv->TrueSymbol; }

#define GetEnvironmentData(theEnv, position) (theEnv->getEnvironmentModule<position>())

bool AllocateEnvironmentData(const Environment&, unsigned, size_t, EnvironmentCleanupFunctionBody = nullptr);
bool AddEnvironmentCleanupFunction(const Environment&, const char *, EnvironmentCleanupFunctionBody, int);
void *GetEnvironmentContext(const Environment&);
void *SetEnvironmentContext(const Environment&, void *);

Environment CreateEnvironment();
struct CLIPSFloat;
struct CLIPSInteger;
struct CLIPSBitMap;
struct FunctionDefinition;
Environment CreateRuntimeEnvironment(CLIPSLexeme **, CLIPSFloat **,
                                      CLIPSInteger **, CLIPSBitMap **,
                                      FunctionDefinition *);
bool DestroyEnvironment(Environment&);

#endif /* _H_envrnmnt */

