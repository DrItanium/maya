/**
 * @brief Allows a string to be wrapped into an IO router
 */
#ifndef _H_strngrtr
#define _H_strngrtr
#include "Router.h"
#include <sstream>
#include <string>
#include <memory>
namespace maya {
    class Environment;
    class HasExtractableContents {
    public:
        virtual std::string getContents() const noexcept = 0;
    };
    class StringRouter : public Router {
    public:
        using Self = StringRouter;
        using Ptr = std::shared_ptr<Self>;
    public:
        using Router::Router;
        ~StringRouter() override = default;
    };
    class DestinationStringRouter : public StringRouter, public HasExtractableContents {
    public:
        using Self = DestinationStringRouter;
        using Ptr = std::shared_ptr<Self>;
    public:
        using StringRouter::StringRouter;
        ~DestinationStringRouter() override = default;
        std::string getContents() const noexcept override;
        bool query(const std::string &logicalName) override;
        void write(const std::string &logicalName, const std::string &value) override;
        void onExit(int exitCode) override;
        int read(const std::string &logicalName) override;
        int unread(const std::string &logicalName, int value) override;
        bool canWriteTo() const noexcept override;
        bool canQuery() const noexcept override;
        bool canRead() const noexcept override;
        bool canUnread() const noexcept override;
        bool canExit() const noexcept override;
    private:
        std::ostringstream _output;
    };
    class SourceStringRouter : public StringRouter {
    public:
        using Self = SourceStringRouter;
        using Ptr = std::shared_ptr<Self>;
    public:
        SourceStringRouter(Environment& env, const std::string& logicalName, int priority, const std::string& inputString);
        ~SourceStringRouter() override = default;
        bool query(const std::string &logicalName) override;
        void write(const std::string &logicalName, const std::string &value) override;
        void onExit(int exitCode) override;
        int read(const std::string &logicalName) override;
        int unread(const std::string &logicalName, int value) override;
        bool canWriteTo() const noexcept override;
        bool canQuery() const noexcept override;
        bool canRead() const noexcept override;
        bool canUnread() const noexcept override;
        bool canExit() const noexcept override;

    private:
        std::istringstream _input;
    };
} // end namespace maya
#if 0
typedef struct stringRouter StringRouter;
typedef struct stringBuilderRouter StringBuilderRouter;

#include <cstdio>
#include "Utility.h"

constexpr auto STRING_ROUTER_DATA = 48;

struct stringRouter {
    const char *name;
    const char *readString;
    char *writeString;
    size_t currentPosition;
    size_t maximumPosition;
    int readWriteType;
    StringRouter *next;
};

struct stringBuilderRouter {
    const char *name;
    StringBuilder *SBR;
    StringBuilderRouter *next;
};

struct stringRouterData : public EnvironmentModule {
    StringRouter *ListOfStringRouters;
    StringBuilderRouter *ListOfStringBuilderRouters;
};
RegisterEnvironmentModule(stringRouterData, STRING_ROUTER_DATA, StringRouter);

/**************************/
/* I/O ROUTER DEFINITIONS */
/**************************/

void InitializeStringRouter(const Environment::Ptr&);
bool OpenStringSource(const Environment::Ptr&, const char *, const char *, size_t);
bool OpenTextSource(const Environment::Ptr&, const char *, const char *, size_t, size_t);
bool CloseStringSource(const Environment::Ptr&, const char *);
bool OpenStringDestination(const Environment::Ptr&, const char *, char *, size_t);
bool CloseStringDestination(const Environment::Ptr&, const char *);
bool OpenStringBuilderDestination(const Environment::Ptr&, const char *, StringBuilder *);
bool CloseStringBuilderDestination(const Environment::Ptr&, const char *);
#endif
#endif /* _H_strngrtr */


