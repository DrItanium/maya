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

#include <memory>
#include <vector>
#include <array>
#include <any>
#include <functional>
#include <tuple>
#include <iostream>
#include <list>
#include <sstream>
#include <map>
#include <typeinfo>
#include <typeindex>

#include "Setup.h"
#include "HoldsEnvironmentCallback.h"
#include "Problem.h"
#include "Entities.hxx"
namespace maya {
    constexpr auto USER_ENVIRONMENT_DATA = 70;
    class Environment;
    class EnvironmentModule : public HoldsEnvironmentCallback {
    public:
        EnvironmentModule(Environment& parent) : HoldsEnvironmentCallback(parent) { }
        virtual ~EnvironmentModule() = default;
        // called when the (clear) function is invoked
        virtual void onClear() noexcept;
        virtual void onReset() noexcept;
    };

#define RegisterEnvironmentModule(actual_type, pos, declAccessorPrefix) \
inline decltype(auto) declAccessorPrefix ## Data (const Environment::Ptr& theEnv) { \
    return theEnv->getEnvironmentModule<actual_type>(); \
} \
inline decltype(auto) declAccessorPrefix ## Data(Environment& theEnv) { \
    return theEnv.getEnvironmentModule<actual_type>(); \
}


    class Environment {
    public:
        using Self = Environment;
        using Ptr = std::shared_ptr<Self>;
    public:
        static Ptr create();
    public:
        /// @todo Make these symbol pointers shared_ptrs
        Environment();
    private:
        std::map<std::type_index, std::unique_ptr<EnvironmentModule>> _modules;
        template<typename T>
        void installEnvironmentModule(std::unique_ptr<T>&& module) {
            if (_modules.find(typeid(T)) != _modules.end()) {
                std::stringstream ss;
                ss << "[ENVRNMNT3] Environment module of type: '" << typeid(T).name() << "' already allocated.";
                auto str = ss.str();
                throw Problem(str);
            } else {
                // not found in our container so instead, we need to install it
                _modules[typeid(T)] = std::move(module);
            }
        }
    public:
        template<typename T>
        const std::unique_ptr<T>& getEnvironmentModule() {
            if (_modules.find(typeid(T)) != _modules.end()) {
                return (std::unique_ptr<T>&)_modules[typeid(T)];
            } else {
                std::stringstream ss;
                ss << "Unallocated environment module of type " << typeid(T).name() << " requested!";
                auto str = ss.str();
                throw Problem(str);
            }
        }
        template<typename T, typename ... Args>
        void allocateEnvironmentModule(Args&& ... args) {
            auto ptr = std::make_unique<T>(*this, std::forward<Args>(args)...);
            installEnvironmentModule(std::move(ptr));
        }
        /**
         * @brief Invokes the body of a passed function using this instance as the environment argument; Useful for installing modules into this environment
         * @param body the function to invoke
         */
        void install(std::function<void(Environment&)> body);
        /**
         * @brief Call the static install methods on a series of provided types
         * @tparam First the first type to call install on
         * @tparam Rest the other types to call install on
         */
        template<typename First, typename ... Rest>
        void install() {
            First::install(*this);
            (Rest::install(*this), ...) ;
        }
    public:
        void printErrorID(const std::string& module, int errorID, bool printCR);
        void incrementLineCount() noexcept;
        void decrementLineCount() noexcept;
        void writeString(const std::string& logicalName, const std::string& string);
        void haltExecution(bool value = true) noexcept;
        bool executionHalted() const noexcept;
        Lexeme::Ptr getVoidConstant() const noexcept;
        Lexeme::Ptr getTrueSymbol() const noexcept;
        Lexeme::Ptr getFalseSymbol() const noexcept;
    };
} // end namespace maya

#endif /* _H_envrnmnt */

