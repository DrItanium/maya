/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  04/04/19            */
/*                                                     */
/*                 ROUTER HEADER FILE                  */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides a centralized mechanism for handling    */
/*   input and output requests.                              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed conversion of '\r' to '\n' from the    */
/*            EnvGetcRouter function.                        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added support for passing context information  */
/*            to the router functions.                       */
/*                                                           */
/*      6.30: Fixed issues with passing context to routers.  */
/*                                                           */
/*            Added AwaitingInput flag.                      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Added STDOUT and STDIN logical name            */
/*            definitions.                                   */
/*                                                           */
/*      6.31: Compiler warning fix.                          */
/*                                                           */
/*      6.40: Added InputBufferCount function.               */
/*                                                           */
/*            Added check for reuse of existing router name. */
/*                                                           */
/*            Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Changed return values for router functions.    */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            Callbacks must be environment aware.           */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Removed WPROMPT, WDISPLAY, WTRACE, and WDIALOG */
/*            logical names.                                 */
/*                                                           */
/*************************************************************/

#ifndef _H_router

#pragma once

#define _H_router

#include <cstdio>
#include <string>
#include <memory>
#include <list>
#include <map>
#include <functional>
#include "HoldsEnvironmentCallback.h"


namespace maya {

    const std::string &STDOUT() noexcept;
    const std::string &STDIN() noexcept;
    const std::string &STDERR() noexcept;
    const std::string &STDWRN() noexcept;

    class Router : public HoldsEnvironmentCallback {
    public:
        using Self = Router;
        using Ptr = std::shared_ptr<Self>;
    public:
        Router(Environment &callback, const std::string &name, int priority);
        virtual ~Router() = default;
        std::string getName() const noexcept { return _name; }
        constexpr auto isActive() const noexcept { return _active; }
        void activate() noexcept { _active = true; }
        void deactivate() noexcept { _active = false; }
        constexpr auto getPriority() const noexcept { return _priority; }
        virtual bool query(const std::string &logicalName) = 0;
        virtual void write(const std::string &logicalName, const std::string &value) = 0;
        virtual void onExit(int exitCode) = 0;
        virtual int read(const std::string &logicalName) = 0;
        virtual int unread(const std::string &logicalName, int value) = 0;
        virtual bool canWriteTo() const noexcept = 0;
        virtual bool canQuery() const noexcept = 0;
        virtual bool canRead() const noexcept = 0;
        virtual bool canUnread() const noexcept = 0;
        virtual bool canExit() const noexcept = 0;
        bool respondsTo(const std::string &logicalName) noexcept;
    private:
        std::string _name;
        bool _active = true;
        int _priority = 0;
    };

    struct LambdaRouter : public Router {
    public:
        using QueryFunction =
        std::function< bool(Environment
        &, const std::string&)>;
        using WriteFunction =
        std::function< void(Environment
        &, const std::string&, const std::string&)>;
        using ExitFunction = std::function<void(Environment & , int)>;
        using ReadFunction =
        std::function< int(Environment
        &, const std::string&)>;
        using UnreadFunction =
        std::function< int(Environment
        &, const std::string&, int)>;
    public:
        LambdaRouter(Environment &callback, const std::string &name, int priority, QueryFunction queryFn = nullptr,
                     WriteFunction writeFn = nullptr, ReadFunction readFn = nullptr,
                     UnreadFunction unreadFn = nullptr, ExitFunction exitFn = nullptr);
        ~LambdaRouter() override = default;
        bool query(const std::string &logicalName) override {
            if (_queryCallback) {
                return _queryCallback(_parent, logicalName);
            } else {
                return false;
            }
        }
        void write(const std::string &logicalName, const std::string &value) override {
            if (_writeCallback) {
                _writeCallback(_parent, logicalName, value);
            }
        }
        void onExit(int exitCode) override {
            if (_exitCallback) {
                _exitCallback(_parent, exitCode);
            }
        }
        int read(const std::string &logicalName) override {
            if (_readCallback) {
                return _readCallback(_parent, logicalName);
            } else {
                return -1;
            }
        }
        int unread(const std::string &logicalName, int value) override {
            if (_unreadCallback) {
                return _unreadCallback(_parent, logicalName, value);
            } else {
                return -1;
            }
        }
        bool canWriteTo() const noexcept override { return _writeCallback.operator bool(); }
        bool canQuery() const noexcept override { return _queryCallback.operator bool(); }
        bool canExit() const noexcept override { return _exitCallback.operator bool(); }
        bool canRead() const noexcept override { return _readCallback.operator bool(); }
        bool canUnread() const noexcept override { return _unreadCallback.operator bool(); }
    private:
        QueryFunction _queryCallback;
        WriteFunction _writeCallback;
        ExitFunction _exitCallback;
        ReadFunction _readCallback;
        UnreadFunction _unreadCallback;
    };
}
#endif /* _H_router */
