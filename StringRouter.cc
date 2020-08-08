#include "StringRouter.h"
#include "Environment.h"
#include <memory>
namespace maya {
    StringRouter::StringRouter(Environment &env, const std::string &logicalName) : Router(env, logicalName, 0) {

    }
    bool
    StringRouter::query(const std::string& logicalName) {
        return logicalName == getName();
    }
    bool
    StringRouter::canQuery() const noexcept {
        return true;
    }
    void
    DestinationStringRouter::write(const std::string &, const std::string &value) {
        _output << value;
    }
    void DestinationStringRouter::onExit(int exitCode) { }
    int DestinationStringRouter::read(const std::string &logicalName) { return 0; }
    int DestinationStringRouter::unread(const std::string &logicalName, int value) { return 0; }
    bool DestinationStringRouter::canWriteTo() const noexcept { return true; }
    bool DestinationStringRouter::canRead() const noexcept { return false; }
    bool DestinationStringRouter::canUnread() const noexcept { return false; }
    bool DestinationStringRouter::canExit() const noexcept { return false; }
    std::string
    DestinationStringRouter::getContents() const noexcept {
        auto str = _output.str();
        return str;
    }
    void SourceStringRouter::write(const std::string &logicalName, const std::string &value) { }
    void SourceStringRouter::onExit(int exitCode) { }
    int SourceStringRouter::read(const std::string &logicalName) {
        if (_input.bad() || _input.eof()) {
            return EOF;
        } else {
            return _input.get();
        }
    }
    int SourceStringRouter::unread(const std::string &logicalName, int value) {
        _input.putback(value);
        if (_input.bad() || _input.eof()) {
            return EOF;
        } else {
            return value;
        }
    }
    bool SourceStringRouter::canWriteTo() const noexcept { return false; }
    bool SourceStringRouter::canRead() const noexcept { return true; }
    bool SourceStringRouter::canUnread() const noexcept { return true; }
    bool SourceStringRouter::canExit() const noexcept { return false; }
    SourceStringRouter::SourceStringRouter(Environment &env, const std::string &logicalName, const std::string &inputString) : StringRouter(env, logicalName), _input(inputString) { }
    std::shared_ptr<HasExtractableContents>
    Environment::openStringDestination(const std::string &name) {
        if (auto result = _stringRouters.find(name); result != _stringRouters.end()) {
            return nullptr;
        } else {
            auto ptr = std::make_shared<DestinationStringRouter>(*this, name);
            _stringRouters.emplace(name, ptr);
            return ptr;
        }
    }
    bool
    Environment::openStringSource(const std::string &name, const std::string &theString) {
        if (auto result = _stringRouters.find(name); result != _stringRouters.end()) {
            return false;
        } else {
            auto ptr = std::make_shared<SourceStringRouter>(*this, name, theString);
            _stringRouters.emplace(name, ptr);
            return true;
        }
    }

    bool
    Environment::closeStringSource(const std::string &name) {
        return _stringRouters.erase(name) > 0;
    }

    bool
    Environment::closeStringDestination(const std::string &name) {
        return closeStringSource(name);
    }
    StringRouter::Ptr
    Environment::findStringRouter(const std::string &name) {
        if (auto outcome = _stringRouters.find(name); outcome != _stringRouters.end()) {
            return outcome->second;
        } else {
            return nullptr;
        }
    }
    bool
    Environment::stringRouterRespondsTo(const std::string &name) {
        return (bool)findStringRouter(name);
    }
    bool StringRouterInterfaceRouter::canExit() const noexcept { return false; }
    void StringRouterInterfaceRouter::onExit(int exitCode) { }
    bool
    StringRouterInterfaceRouter::query(const std::string &logicalName) {
       return getParent().stringRouterRespondsTo(logicalName);
    }
    int
    StringRouterInterfaceRouter::read(const std::string &logicalName) {
        if (auto target = getParent().findStringRouter(logicalName); target) {
            if (target->canRead()) {
                return target->read(logicalName);
            } else {
                return EOF;
            }
        } else {
            throw "Query was successful but string router could not be found!!!!";
        }
    }
    int
    StringRouterInterfaceRouter::unread(const std::string &logicalName, int value) {
        if (auto target = getParent().findStringRouter(logicalName); target) {
            if (target->canUnread()) {
                return target->unread(logicalName, value);
            } else {
                return 0;
            }
        } else {
            throw "Query was successful but string router could not be found!!!!";
        }
        return 0;
    }
    void
    StringRouterInterfaceRouter::write(const std::string &logicalName, const std::string &value) {
        if (auto target = getParent().findStringRouter(logicalName); target) {
            if (target->canWriteTo()) {
                target->write(logicalName, value);
            }
        } else {
            throw "Could not find a successfully queried string router!";
        }
    }
    bool StringRouterInterfaceRouter::canWriteTo() const noexcept { return true; }
    bool StringRouterInterfaceRouter::canQuery() const noexcept { return true; }
    bool StringRouterInterfaceRouter::canRead() const noexcept { return true; }
    bool StringRouterInterfaceRouter::canUnread() const noexcept { return true; }
} // end namespace maya
#if 0
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "Setup.h"

#include "Constants.h"
#include "Environment.h"
#include "MemoryAllocation.h"
#include "PrintUtility.h"
#include "Router.h"
#include "SystemDependency.h"

#include "StringRouter.h"

#define READ_STRING 0
#define WRITE_STRING 1

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

static bool QueryStringCallback(const Environment::Ptr&, const char *, void *);
static void WriteStringCallback(const Environment::Ptr&, const char *, const char *, void *);
static int ReadStringCallback(const Environment::Ptr&, const char *, void *);
static int UnreadStringCallback(const Environment::Ptr&, const char *, int, void *);
static StringRouter *FindStringRouter(const Environment::Ptr&, const char *);
static bool CreateReadStringSource(const Environment::Ptr&, const char *, const char *, size_t, size_t);
static void DeallocateStringRouterData(const Environment::Ptr&);
static StringBuilderRouter *FindStringBuilderRouter(const Environment::Ptr&, const char *);
static bool QueryStringBuilderCallback(const Environment::Ptr&, const char *, void *);
static void WriteStringBuilderCallback(const Environment::Ptr&, const char *, const char *, void *);

/**********************************************************/
/* InitializeStringRouter: Initializes string I/O router. */
/**********************************************************/
void InitializeStringRouter(
        const Environment::Ptr&theEnv) {
    //AllocateEnvironmentData(theEnv, STRING_ROUTER_DATA, sizeof(stringRouterData), DeallocateStringRouterData);
    theEnv->allocateEnvironmentModule<stringRouterData>();

    AddRouter(theEnv, "string", 0, QueryStringCallback, WriteStringCallback, ReadStringCallback, UnreadStringCallback, nullptr, nullptr);
    AddRouter(theEnv, "stringBuilder", 0, QueryStringBuilderCallback, WriteStringBuilderCallback, nullptr, nullptr, nullptr, nullptr);
}

/*******************************************/
/* DeallocateStringRouterData: Deallocates */
/*    environment data for string routers. */
/*******************************************/
static void DeallocateStringRouterData(
        const Environment::Ptr&theEnv) {
    StringRouter *tmpPtr, *nextPtr;
    StringBuilderRouter *tmpSBPtr, *nextSBPtr;

    tmpPtr = StringRouterData(theEnv)->ListOfStringRouters;
    while (tmpPtr != nullptr) {
        nextPtr = tmpPtr->next;
        rm(theEnv, (void *) tmpPtr->name, strlen(tmpPtr->name) + 1);
        rtn_struct(theEnv, stringRouter, tmpPtr);
        tmpPtr = nextPtr;
    }

    tmpSBPtr = StringRouterData(theEnv)->ListOfStringBuilderRouters;
    while (tmpSBPtr != nullptr) {
        nextSBPtr = tmpSBPtr->next;
        rm(theEnv, (void *) tmpSBPtr->name, strlen(tmpSBPtr->name) + 1);
        rtn_struct(theEnv, stringBuilderRouter, tmpSBPtr);
        tmpSBPtr = nextSBPtr;
    }
}

/*********************************************************************/
/* QueryStringCallback: Find routine for string router logical names. */
/*********************************************************************/
static bool QueryStringCallback(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        void *context) {
    struct stringRouter *head;

    head = StringRouterData(theEnv)->ListOfStringRouters;
    while (head != nullptr) {
        if (strcmp(head->name, logicalName) == 0) { return true; }
        head = head->next;
    }

    return false;
}

/**********************************************************/
/* WriteStringCallback: Print routine for string routers. */
/**********************************************************/
static void WriteStringCallback(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        const char *str,
        void *context) {
    struct stringRouter *head;

    head = FindStringRouter(theEnv, logicalName);
    if (head == nullptr) {
        SystemError(theEnv, "ROUTER", 3);
        ExitRouter(theEnv, EXIT_FAILURE);
        return;
    }

    if (head->readWriteType != WRITE_STRING) return;

    if (head->maximumPosition == 0) return;

    if ((head->currentPosition + 1) >= head->maximumPosition) return;

    genstrncpy(&head->writeString[head->currentPosition],
               str, (STD_SIZE) (head->maximumPosition - head->currentPosition) - 1);

    head->currentPosition += strlen(str);
}

/********************************************************/
/* ReadStringCallback: Getc routine for string routers. */
/********************************************************/
static int ReadStringCallback(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        void *context) {
    struct stringRouter *head;
    int rc;

    head = FindStringRouter(theEnv, logicalName);
    if (head == nullptr) {
        SystemError(theEnv, "ROUTER", 1);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    if (head->readWriteType != READ_STRING) return (EOF);
    if (head->currentPosition >= head->maximumPosition) {
        head->currentPosition++;
        return (EOF);
    }

    rc = (unsigned char) head->readString[head->currentPosition];
    head->currentPosition++;

    return (rc);
}

/************************************************************/
/* UnreadStringCallback: Ungetc routine for string routers. */
/************************************************************/
static int UnreadStringCallback(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        int ch,
        void *context) {
    struct stringRouter *head;
#if MAC_XCD
#pragma unused(ch)
#endif

    head = FindStringRouter(theEnv, logicalName);

    if (head == nullptr) {
        SystemError(theEnv, "ROUTER", 2);
        ExitRouter(theEnv, EXIT_FAILURE);
    }

    if (head->readWriteType != READ_STRING) return 0;
    if (head->currentPosition > 0) { head->currentPosition--; }

    return 1;
}

/************************************************/
/* OpenStringSource: Opens a new string router. */
/************************************************/
bool OpenStringSource(
        const Environment::Ptr&theEnv,
        const char *name,
        const char *str,
        size_t currentPosition) {
    size_t maximumPosition;

    if (str == nullptr) {
        currentPosition = 0;
        maximumPosition = 0;
    } else { maximumPosition = strlen(str); }

    return (CreateReadStringSource(theEnv, name, str, currentPosition, maximumPosition));
}

/******************************************************/
/* OpenTextSource: Opens a new string router for text */
/*   (which is not nullptr terminated).                  */
/******************************************************/
bool OpenTextSource(
        const Environment::Ptr&theEnv,
        const char *name,
        const char *str,
        size_t currentPosition,
        size_t maximumPosition) {
    if (str == nullptr) {
        currentPosition = 0;
        maximumPosition = 0;
    }

    return CreateReadStringSource(theEnv, name, str, currentPosition, maximumPosition);
}

/******************************************************************/
/* CreateReadStringSource: Creates a new string router for input. */
/******************************************************************/
static bool CreateReadStringSource(
        const Environment::Ptr&theEnv,
        const char *name,
        const char *str,
        size_t currentPosition,
        size_t maximumPosition) {
#if STUBBING_INACTIVE
    struct stringRouter *newStringRouter;
    char *theName;

    if (FindStringRouter(theEnv, name) != nullptr) return false;

    newStringRouter = get_struct(theEnv, stringRouter);
    theName = (char *) gm1(theEnv, strlen(name) + 1);
    genstrcpy(theName, name);
    newStringRouter->name = theName;
    newStringRouter->writeString = nullptr;
    newStringRouter->readString = str;
    newStringRouter->currentPosition = currentPosition;
    newStringRouter->readWriteType = READ_STRING;
    newStringRouter->maximumPosition = maximumPosition;
    newStringRouter->next = StringRouterData(theEnv)->ListOfStringRouters;
    StringRouterData(theEnv)->ListOfStringRouters = newStringRouter;

    return true;
#endif
    return false;
}

/**********************************************/
/* CloseStringSource: Closes a string router. */
/**********************************************/
bool CloseStringSource(
        const Environment::Ptr&theEnv,
        const char *name) {
    struct stringRouter *head, *last;

    last = nullptr;
    head = StringRouterData(theEnv)->ListOfStringRouters;
    while (head != nullptr) {
        if (strcmp(head->name, name) == 0) {
            if (last == nullptr) {
                StringRouterData(theEnv)->ListOfStringRouters = head->next;
                rm(theEnv, (void *) head->name, strlen(head->name) + 1);
                rtn_struct(theEnv, stringRouter, head);
                return true;
            } else {
                last->next = head->next;
                rm(theEnv, (void *) head->name, strlen(head->name) + 1);
                rtn_struct(theEnv, stringRouter, head);
                return true;
            }
        }
        last = head;
        head = head->next;
    }

    return false;
}

/******************************************************************/
/* OpenStringDestination: Opens a new string router for printing. */
/******************************************************************/
bool OpenStringDestination(
        const Environment::Ptr&theEnv,
        const char *name,
        char *str,
        size_t maximumPosition) {
#if STUBBING_INACTIVE
    struct stringRouter *newStringRouter;
    char *theName;

    if (FindStringRouter(theEnv, name) != nullptr) return false;

    newStringRouter = get_struct(theEnv, stringRouter);
    theName = (char *) gm1(theEnv, strlen(name) + 1);
    genstrcpy(theName, name);
    newStringRouter->name = theName;
    newStringRouter->readString = nullptr;
    newStringRouter->writeString = str;
    newStringRouter->currentPosition = 0;
    newStringRouter->readWriteType = WRITE_STRING;
    newStringRouter->maximumPosition = maximumPosition;
    newStringRouter->next = StringRouterData(theEnv)->ListOfStringRouters;
    StringRouterData(theEnv)->ListOfStringRouters = newStringRouter;

    return true;
#endif
    return false;
}

/***************************************************/
/* CloseStringDestination: Closes a string router. */
/***************************************************/
bool CloseStringDestination(
        const Environment::Ptr&theEnv,
        const char *name) {
    return CloseStringSource(theEnv, name);
}

/*******************************************************************/
/* FindStringRouter: Returns a pointer to the named string router. */
/*******************************************************************/
static struct stringRouter *FindStringRouter(
        const Environment::Ptr&theEnv,
        const char *name) {
    struct stringRouter *head;

    head = StringRouterData(theEnv)->ListOfStringRouters;
    while (head != nullptr) {
        if (strcmp(head->name, name) == 0) { return (head); }
        head = head->next;
    }

    return nullptr;
}

/*********************************************/
/* OpenStringBuilderDestination: Opens a new */
/*   StringBuilder router for printing.      */
/*********************************************/
bool OpenStringBuilderDestination(
        const Environment::Ptr&theEnv,
        const char *name,
        StringBuilder *theSB) {
#if STUBBING_INACTIVE
    StringBuilderRouter *newStringRouter;
    char *theName;

    if (FindStringBuilderRouter(theEnv, name) != nullptr) return false;

    newStringRouter = get_struct(theEnv, stringBuilderRouter);
    theName = (char *) gm1(theEnv, strlen(name) + 1);
    genstrcpy(theName, name);
    newStringRouter->name = theName;
    newStringRouter->SBR = theSB;
    newStringRouter->next = StringRouterData(theEnv)->ListOfStringBuilderRouters;
    StringRouterData(theEnv)->ListOfStringBuilderRouters = newStringRouter;

    return true;
#endif
    return false;
}

/*****************************************/
/* CloseStringBuilderDestination: Closes */
/*   a StringBuilder router.             */
/*****************************************/
bool CloseStringBuilderDestination(
        const Environment::Ptr&theEnv,
        const char *name) {
    StringBuilderRouter *head, *last;

    last = nullptr;
    head = StringRouterData(theEnv)->ListOfStringBuilderRouters;
    while (head != nullptr) {
        if (strcmp(head->name, name) == 0) {
            if (last == nullptr) {
                StringRouterData(theEnv)->ListOfStringBuilderRouters = head->next;
                rm(theEnv, (void *) head->name, strlen(head->name) + 1);
                rtn_struct(theEnv, stringBuilderRouter, head);
                return true;
            } else {
                last->next = head->next;
                rm(theEnv, (void *) head->name, strlen(head->name) + 1);
                rtn_struct(theEnv, stringBuilderRouter, head);
                return true;
            }
        }
        last = head;
        head = head->next;
    }

    return false;
}

/**********************************************/
/* FindStringBuilderRouter: Returns a pointer */
/*   to the named StringBuilder router.       */
/**********************************************/
static struct stringBuilderRouter *FindStringBuilderRouter(
        const Environment::Ptr&theEnv,
        const char *name) {
    StringBuilderRouter *head;

    head = StringRouterData(theEnv)->ListOfStringBuilderRouters;
    while (head != nullptr) {
        if (strcmp(head->name, name) == 0) { return head; }
        head = head->next;
    }

    return nullptr;
}

/*********************************************/
/* QueryStringBuilderCallback: Query routine */
/*   for stringBuilder router logical names. */
/*********************************************/
static bool QueryStringBuilderCallback(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        void *context) {
    StringBuilderRouter *head;

    head = StringRouterData(theEnv)->ListOfStringBuilderRouters;
    while (head != nullptr) {
        if (strcmp(head->name, logicalName) == 0) { return true; }
        head = head->next;
    }

    return false;
}

/*********************************************/
/* WriteStringBuilderCallback: Print routine */
/*    for stringBuilder routers.             */
/*********************************************/
static void WriteStringBuilderCallback(
        const Environment::Ptr&theEnv,
        const char *logicalName,
        const char *str,
        void *context) {
    StringBuilderRouter *head;

    head = FindStringBuilderRouter(theEnv, logicalName);
    if (head == nullptr) {
        SystemError(theEnv, "ROUTER", 3);
        ExitRouter(theEnv, EXIT_FAILURE);
        return;
    }

    SBAppend(head->SBR, str);
}

#endif
