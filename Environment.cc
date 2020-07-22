#include "Setup.h"
#include "Environment.h"
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

    Environment::Environment() : _voidConstant(std::make_shared<Void>(*this)){
        _positiveInfinity = createSymbol(PositiveInfinityString);
        _negativeInfinity = createSymbol(NegativeInfinityString);
        _zero = createInteger(0);
        _trueSymbol = createSymbol(TrueString);
        _falseSymbol = createSymbol(FalseString);
#if STUBBING_INACTIVE
        ExpressionData(theEnv)->PTR_AND = FindFunction(theEnv, "and");
    ExpressionData(theEnv)->PTR_OR = FindFunction(theEnv, "or");
    ExpressionData(theEnv)->PTR_EQ = FindFunction(theEnv, "eq");
    ExpressionData(theEnv)->PTR_NEQ = FindFunction(theEnv, "neq");
    ExpressionData(theEnv)->PTR_NOT = FindFunction(theEnv, "not");

    if ((ExpressionData(theEnv)->PTR_AND == nullptr) || (ExpressionData(theEnv)->PTR_OR == nullptr) ||
        (ExpressionData(theEnv)->PTR_EQ == nullptr) || (ExpressionData(theEnv)->PTR_NEQ == nullptr) ||
        (ExpressionData(theEnv)->PTR_NOT == nullptr)) {
        SystemError(theEnv, "EXPRESSN", 1);
        ExitRouter(theEnv, EXIT_FAILURE);
    }
#endif
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
        // ephemeral hash node support was here
        _integerTable.emplace(hash, newInteger);
        return newInteger;
    }
#if 0
    void
    Environment::installEntity(Entity::Ptr target) {
        auto hash = target->hash(BITMAP_HASH_SIZE);
        auto range = _entityTable.equal_range(hash);
        for (auto iter = range.first; iter != range.second; ++iter) {

        }
    }
#endif

    Lexeme::Ptr
    Environment::createBoolean(bool value) noexcept {
        return value ? _trueSymbol : _falseSymbol;
    }

    void
    Environment::incrementLineCount() noexcept {
        /// @todo implement
    }
    void
    Environment::decrementLineCount() noexcept {
        /// @todo implement
    }
    void
    Environment::printErrorID(const std::string &module, int errorID, bool printCR) {
        /// @todo implement
    }

/*********************************************************/
/* AddRouter: Adds an I/O router to the list of routers. */
/*********************************************************/
    bool
    Environment::addRouter(std::function<Router::Ptr (Environment &)> makerFunction) {
        return insertRouter(makerFunction(*this));
    }
    bool
    Environment::addRouter(const std::string &name, int priority, LambdaRouter::QueryFunction queryFn, LambdaRouter::WriteFunction writeFn, LambdaRouter::ReadFunction readFn, LambdaRouter::UnreadFunction unreadFn, LambdaRouter::ExitFunction exitFn) {
        return insertRouter(std::make_shared<LambdaRouter>(*this, name, priority, queryFn, writeFn, readFn, unreadFn, exitFn));
    }
    bool
    Environment::insertRouter(Router::Ptr router) {
        /// @todo implement this
        if (_listOfRouters.empty()) {
            _listOfRouters.emplace_back(router);
            return true;
        }
        // first check and see if the router is already claimed
        for (auto& existingRouter : _listOfRouters) {
            if (existingRouter->getName() == router->getName()) {
                return false;
            }
        }
        for (auto pos = _listOfRouters.begin(); pos != _listOfRouters.end(); ++pos) {
            auto rtr = *pos;
            if (router->getPriority() >= rtr->getPriority()) {
                // we have found the place to insert before
                _listOfRouters.emplace(pos, router);
                return true;
            }
        }
        // if we got here then it means we should emplace_back this entry since it was never greater than any of the other entries
        _listOfRouters.emplace_back(router);
        return true;
    }
    bool
    Environment::destroyRouter(const std::string &logicalName) {
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
    Environment::queryRouter(const std::string &logicalName) {
        for (auto& router : _listOfRouters) {
            if (router->respondsTo(logicalName)) {
                return true;
            }
        }
        return false;
    }

    bool
    Environment::deactivateRouter(const std::string &logicalName) {
        for (auto& element : _listOfRouters) {
            if (element->getName() == logicalName) {
                element->deactivate();
                return true;
            }
        }
        return false;
    }

    bool
    Environment::activateRouter(const std::string &logicalName) {
        for (auto& element : _listOfRouters) {
            if (element->getName() == logicalName) {
                element->activate();
                return true;
            }
        }
        return false;
    }
    Router::Ptr
    Environment::findRouter(const std::string& logicalName) {
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
    Environment::unrecognizedRouterMessage(const std::string& logicalName) {
        printErrorID("ROUTER", 1, false);
        writeStringsRouter(STDERR(), "Logical name '", logicalName, "' was not recognized by any routers.\n");
    }
    bool
    Environment::printRouterExists(const std::string &logicalName) {
        for (const auto& router : _listOfRouters) {
            if (router->canWriteTo() && router->respondsTo(logicalName)) {
                return true;
            }
        }
        return false;
    }

    void
    Environment::writeStringRouter(const std::string &logicalName, const std::string &str) {
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
    Environment::write(const std::string &str) {
        writeStringRouter(STDOUT(), str);
    }

    void
    Environment::writeLine(const std::string &str) {
        writeStringRouter(STDOUT(), str);
        writeStringRouter(STDOUT(), "\n");
    }
    int
    Environment::readRouter(const std::string& logicalName) {
        for (auto& router : _listOfRouters) {
            if (router->canRead() && router->respondsTo(logicalName)) {
                auto inchar = router->read(logicalName);
                if (inchar == '\n') {
                    if (!_lineCountRouter.empty() && (logicalName == _lineCountRouter)) {
                        incrementLineCount();
                    }
                }
                return inchar;
            }
        }
        unrecognizedRouterMessage(logicalName);
        return -1;
    }

    int
    Environment::unreadRouter(const std::string &logicalName, int toUnread) {
        for (auto& router : _listOfRouters) {
            if (router->canUnread() && router->respondsTo(logicalName)) {
                if (toUnread == '\n') {
                    if (!_lineCountRouter.empty() && (logicalName == _lineCountRouter)) {
                        decrementLineCount();
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
    Environment::exitRouter(int code) {
        _abortExit = false;
        for (auto& router : _listOfRouters) {
            if (router->isActive()) {
                if (router->canExit()) {
                    router->onExit(code);
                }
            }
        }
        if (_abortExit) {
            return;
        }
        /// @todo reimplement as C++ code
        //genexit(theEnv, num);
    }
    void
    Environment::resetErrorFlags() noexcept {
        _evaluationError = false;
        _executionHalted = false;
    }
    void
    Environment::setEvaluationError(bool value) noexcept {
        _evaluationError = value;
        if (value) {
            _executionHalted = true;
        }
    }
    bool Environment::isClearReadyInProgress() const noexcept {
        return _clearReadyInProgress;
    }
    void Environment::setClearReadyInProgress(bool clearReadyInProgress) noexcept {
        _clearReadyInProgress = clearReadyInProgress;
    }
    bool Environment::isClearInProgress() const noexcept {
        return _clearInProgress;
    }
    void Environment::setClearInProgress(bool clearInProgress) noexcept {
        _clearInProgress = clearInProgress;
    }
    bool Environment::isResetReadyInProgress() const noexcept {
        return _resetReadyInProgress;
    }
    void Environment::setResetReadyInProgress(bool resetReadyInProgress) noexcept {
        _resetReadyInProgress = resetReadyInProgress;
    }
    bool Environment::isResetInProgress() const noexcept {
        return _resetInProgress;
    }
    void Environment::setResetInProgress(bool resetInProgress) noexcept {
        _resetInProgress = resetInProgress;
    }
    int16_t Environment::getClearReadyLocks() const noexcept {
        return _clearReadyLocks;
    }
    void Environment::setClearReadyLocks(int16_t clearReadyLocks) noexcept {
        _clearReadyLocks = clearReadyLocks;
    }
    int Environment::getDanglingConstructs() const noexcept {
        return _danglingConstructs;
    }
    void Environment::setDanglingConstructs(int danglingConstructs) noexcept {
        _danglingConstructs = danglingConstructs;
    }
    bool Environment::isPrintWhileLoading() const noexcept {
        return _printWhileLoading;
    }
    void Environment::setPrintWhileLoading(bool printWhileLoading) noexcept {
        _printWhileLoading = printWhileLoading;
    }
    bool Environment::isLoadInProgress() const noexcept {
        return _loadInProgress;
    }
    void Environment::setLoadInProgress(bool loadInProgress) noexcept {
        _loadInProgress = loadInProgress;
    }
    bool Environment::isWatchCompilations() const noexcept {
        return _watchCompilations;
    }
    void Environment::setWatchCompilations(bool watchCompilations) noexcept {
        _watchCompilations = watchCompilations;
    }
    bool Environment::isCheckSyntaxMode() const noexcept {
        return _checkSyntaxMode;
    }
    void Environment::setCheckSyntaxMode(bool checkSyntaxMode) noexcept {
        _checkSyntaxMode = checkSyntaxMode;
    }
    bool Environment::isParsingConstruct() const noexcept {
        return _parsingConstruct;
    }
    void Environment::setParsingConstruct(bool parsingConstruct) noexcept {
        _parsingConstruct = parsingConstruct;
    }
    std::string
    Environment::getErrorString() const noexcept {
        return _errorString;
    }
    void Environment::setErrorString(const std::string &errorString) noexcept {
        _errorString = errorString;
    }
    std::string
    Environment::getWarningString() const noexcept {
        return _warningString;
    }
    void
    Environment::setWarningString(const std::string &warningString) noexcept {
        _warningString = warningString;
    }
    std::string
    Environment::getParsingFileName() const noexcept {
        return _parsingFileName;
    }
    void Environment::setParsingFileName(const std::string &parsingFileName) noexcept {
        _parsingFileName = parsingFileName;
    }
    std::string Environment::getErrorFileName() const noexcept {
        return _errorFileName;
    }
    void Environment::setErrorFileName(const std::string &errorFileName) noexcept {
        _errorFileName = errorFileName;
    }
    std::string Environment::getWarningFileName() const noexcept {
        return _warningFileName;
    }
    void Environment::setWarningFileName(const std::string &warningFileName) noexcept {
        _warningFileName = warningFileName;
    }
    ssize_t Environment::getErrorLineNumber() const noexcept {
        return _errorLineNumber;
    }
    void Environment::setErrorLineNumber(ssize_t errorLineNumber) noexcept {
        _errorLineNumber = errorLineNumber;
    }
    void Environment::setWarnLineNumber(ssize_t warnLineNumber) noexcept {
        _warnLineNumber = warnLineNumber;
    }
    void Environment::setErrorCaptureRouterCount(ssize_t errorCaptureRouterCount) noexcept {
        _errorCaptureRouterCount = errorCaptureRouterCount;
    }
    void Environment::setMaxErrorChars(size_t maxErrorChars) noexcept {
        _maxErrorChars = maxErrorChars;
    }
    void Environment::setCurrentErrorPosition(size_t currentErrorPosition) noexcept {
        _currentErrorPosition = currentErrorPosition;
    }
    void Environment::setMaxWarnChars(size_t maxWarnChars) noexcept {
        _maxWarnChars = maxWarnChars;
    }
    void Environment::setCurrentWarnPosition(size_t currentWarnPosition) noexcept {
        _currentWarnPosition = currentWarnPosition;
    }
    void Environment::setExecuting(bool executing) noexcept {
        _executing = executing;
    }
    void Environment::setBeforeResetCallback(std::function<bool(Environment &)> beforeResetCallback) noexcept {
        _beforeResetCallback = beforeResetCallback;
    }

}
