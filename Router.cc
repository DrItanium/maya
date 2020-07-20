
#include "Setup.h"

//#include "ArgumentAccess.h"
#include "Constants.h"
#include "Environment.h"
//#include "ExternalFunctions.h"
//#include "File.h"
//#include "MemoryAllocation.h"
//#include "PrintUtility.h"
//#include "Scanner.h"
//#include "StringRouter.h"
//#include "SystemDependency.h"

#include "Router.h"
namespace maya {
    bool
    Router::respondsTo(const std::string &str) noexcept {
        return _active && canQuery() && query(str);
    }
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


    LambdaRouter::LambdaRouter(Environment &callback, const std::string &name, int priority, QueryFunction queryFn,
                               WriteFunction writeFn, ReadFunction readFn, UnreadFunction unreadFn, ExitFunction exitFn) :
                               Router(callback, name, priority), _queryCallback(queryFn),
                               _writeCallback(writeFn),
                               _readCallback(readFn),
                               _unreadCallback(unreadFn),
                               _exitCallback(exitFn){
}
    Router::Router(Environment &callback, const std::string &name, int priority) : HoldsEnvironmentCallback(callback), _name(name), _priority(priority) { }

} // end namespace maya
