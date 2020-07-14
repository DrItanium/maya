/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*            CLIPS Version 6.40  03/20/19             */
/*                                                     */
/*                  I/O ROUTER MODULE                  */
/*******************************************************/

/*************************************************************/
/* Purpose: Provides a centralized mechanism for handling    */
/*   input and output requests.                              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
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
/*      6.31: Fixed line count issue when using Windows      */
/*            line endings in Unix.                          */
/*                                                           */
/*      6.40: Added InputBufferCount function.               */
/*                                                           */
/*            Added check for reuse of existing router name. */
/*                                                           */
/*            Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
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
/*************************************************************/

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "Setup.h"

#include "ArgumentAccess.h"
#include "Constants.h"
#include "Environment.h"
#include "ExternalFunctions.h"
#include "File.h"
#include "MemoryAllocation.h"
#include "PrintUtility.h"
#include "Scanner.h"
#include "StringRouter.h"
#include "SystemDependency.h"

#include "Router.h"

/**********************/
/* STRING DEFINITIONS */
/**********************/

const std::string& STDOUT() noexcept {
    static std::string contents = "stdout";
    return contents;
}
const std::string& STDIN() noexcept {
    static std::string contents = "stdin";
    return contents;
}

const std::string& STDERR() noexcept {
    static std::string contents = "stderr";
    return contents;
}
const std::string& STDWRN() noexcept {
    static std::string contents = "stdwrn";
    return contents;
}

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static bool QueryRouter(const Environment::Ptr&, const char *, struct router *);
static void DeallocateRouterData(const Environment::Ptr&);

/*********************************************************/
/* InitializeDefaultRouters: Initializes output streams. */
/*********************************************************/
void
RouterModule::install(Environment &theEnv) {
    theEnv.allocateEnvironmentModule<RouterModule>();
    //const auto& routerModule = RouterData(theEnv);
#if STUBBING_INACTIVE
    InitializeFileRouter(theEnv);
    InitializeStringRouter(theEnv);
#endif
}
bool
RouterModule::printRouterExists(const std::string &logicalName) {
    for (const auto& router : _listOfRouters) {
        if (router->canWriteTo() && router->respondsTo(logicalName)) {
            return true;
        }
    }
    return false;
}

bool
Router::respondsTo(const std::string &str) noexcept {
    return _active && canQuery() && query(str);
}
void
RouterModule::writeString(const std::string &logicalName, const std::string &str) {
    if (str.empty()) {
        return;
    }
    for (auto router : _listOfRouters) {
        if (router->canWriteTo() && router->respondsTo(str)) {
            router->write(logicalName, str);
            return;
        }
    }
    if (logicalName != STDERR()) {
        unrecognizedRouterMessage(logicalName);
    }
}
void
RouterModule::write(const std::string &str) {
    writeString(STDOUT(), str);
}

void
RouterModule::writeLine(const std::string &str) {
    writeString(STDOUT(), str);
    writeString(STDOUT(), "\n");
}
int
RouterModule::read(const std::string& logicalName) {
    for (auto& router : _listOfRouters) {
        if (router->canRead() && router->respondsTo(logicalName)) {
            auto inchar = router->read(logicalName);
            if (inchar == '\n') {
                if (!_lineCountRouter.empty() && (logicalName == _lineCountRouter)) {
                    _parent.incrementLineCount();
                }
            }
            return inchar;
        }
    }
    unrecognizedRouterMessage(logicalName);
    return -1;
}

/***************************************************/
/* UnreadRouter: Generic unget character function. */
/***************************************************/
int
RouterModule::unread(const std::string &logicalName, int toUnread) {
    for (auto& router : _listOfRouters) {
        if (router->canUnread() && router->respondsTo(logicalName)) {
            if (toUnread == '\n') {
                if (!_lineCountRouter.empty() && (logicalName == _lineCountRouter)) {
                    _parent.decrementLineCount();
                }
            }
            return router->unread(logicalName, toUnread);
        }
    }
    // the logical name was not recognized by any routers
    unrecognizedRouterMessage(logicalName);
    return -1;
}
void
RouterModule::exit(int code) {
    _abort = false;
    for (auto& router : _listOfRouters) {
        if (router->isActive()) {
            if (router->canExit()) {
                router->onExit(code);
            }
        }
    }
    if (_abort) {
        return;
    }
    /// @todo reimplement as C++ code
    //genexit(theEnv, num);
}

/********************************************/
/* AbortExit: Forces ExitRouter to terminate */
/*   after calling all closing routers.     */
/********************************************/
void
RouterModule::abortExit() noexcept {
    _abort = true;
}
/*********************************************************/
/* AddRouter: Adds an I/O router to the list of routers. */
/*********************************************************/
bool
RouterModule::addRouter(std::function<Router::Ptr (Environment &)> makerFunction) {
    return insertRouter(makerFunction(_parent));
}
bool
RouterModule::addRouter(const std::string &name, int priority, LambdaRouter::QueryFunction queryFn, LambdaRouter::WriteFunction writeFn, LambdaRouter::ReadFunction readFn, LambdaRouter::UnreadFunction unreadFn, LambdaRouter::ExitFunction exitFn) {
    return insertRouter(std::make_shared<LambdaRouter>(_parent, name, priority, queryFn, writeFn, readFn, unreadFn, exitFn));
}
bool
RouterModule::insertRouter(Router::Ptr router) {
   /// @todo implement this
#if STUBBING_INACTIVE
    struct router *newPtr, *lastPtr, *currentPtr;
    char *nameCopy;

    /*==================================================*/
    /* Reject the router if the name is already in use. */
    /*==================================================*/

    for (currentPtr = RouterData(theEnv)->ListOfRouters;
         currentPtr != nullptr;
         currentPtr = currentPtr->next) {
        if (strcmp(currentPtr->name, routerName) == 0) { return false; }
    }

    newPtr = get_struct(theEnv, router);

    nameCopy = (char *) genalloc(theEnv, strlen(routerName) + 1);
    genstrcpy(nameCopy, routerName);
    newPtr->name = nameCopy;

    newPtr->active = true;
    newPtr->context = context;
    newPtr->priority = priority;
    newPtr->queryCallback = queryFunction;
    newPtr->writeCallback = writeFunction;
    newPtr->exitCallback = exitFunction;
    newPtr->readCallback = readFunction;
    newPtr->unreadCallback = unreadFunction;
    newPtr->next = nullptr;

    if (RouterData(theEnv)->ListOfRouters == nullptr) {
        RouterData(theEnv)->ListOfRouters = newPtr;
        return true;
    }

    lastPtr = nullptr;
    currentPtr = RouterData(theEnv)->ListOfRouters;
    while ((currentPtr != nullptr) ? (priority < currentPtr->priority) : false) {
        lastPtr = currentPtr;
        currentPtr = currentPtr->next;
    }

    if (lastPtr == nullptr) {
        newPtr->next = RouterData(theEnv)->ListOfRouters;
        RouterData(theEnv)->ListOfRouters = newPtr;
    } else {
        newPtr->next = currentPtr;
        lastPtr->next = newPtr;
    }

    return true;
#endif
    return false;
}
bool
RouterModule::destroy(const std::string &logicalName) {
    for (auto pos = _listOfRouters.begin(); pos != _listOfRouters.end(); ++pos) {
        auto router = *pos; // this will go out of scope and cause the shared pointer to reclaim
        if (router->getName() == logicalName) {
           // need to purge it out
           _listOfRouters.erase(pos);
           return true;
        }
    }
    return false;
}
bool
RouterModule::query(const std::string &logicalName) {
    for (auto& router : _listOfRouters) {
        if (router->respondsTo(logicalName)) {
            return true;
        }
    }
    return false;
}

bool
RouterModule::deactivate(const std::string &logicalName) {
    for (auto& element : _listOfRouters) {
        if (element->getName() == logicalName) {
            element->deactivate();
            return true;
        }
    }
    return false;
}

bool
RouterModule::activate(const std::string &logicalName) {
    for (auto& element : _listOfRouters) {
        if (element->getName() == logicalName) {
            element->activate();
            return true;
        }
    }
    return false;
}
Router::Ptr
RouterModule::find(const std::string& logicalName) {
    for (auto& element : _listOfRouters) {
        if (element->getName() == logicalName) {
            return element;
        }
    }
    return nullptr;
}

/*****************************************************/
/* UnrecognizedRouterMessage: Standard error message */
/*   for an unrecognized router name.                */
/*****************************************************/
void
RouterModule::unrecognizedRouterMessage(const std::string& logicalName) {
    _parent.printErrorID("ROUTER", 1, false);
    writeStrings(STDERR(), "Logical name '", logicalName, "' was not recognized by any routers.\n");
}

void
Environment::writeString(const std::string &logicalName, const std::string &string) {
    getEnvironmentModule<RouterModule>()->writeString(logicalName, string);
}
