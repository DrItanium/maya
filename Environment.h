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
#include <forward_list>

#include "Setup.h"
#include "HoldsEnvironmentCallback.h"
#include "Problem.h"
#include "Entities.hxx"
#include "Router.h"
#include "Construct.h"
#include "CallFunctionItem.h"
#include "UDFContext.h"
#include "VoidAtom.h"
#include "LexemeAtom.h"
#include "FloatAtom.h"
#include "IntegerAtom.h"
#include "BitmapAtom.h"
namespace maya {
    class Environment {
    public:
        using Self = Environment;
        using Ptr = std::shared_ptr<Self>;
        template<typename T>
        using DataTable = std::multimap<size_t, typename T::Ptr>;
    public:
        static inline const std::string TrueString{"TRUE"};
        static inline const std::string FalseString{"FALSE"};
        static inline const std::string PositiveInfinityString{"+oo"};
        static inline const std::string NegativeInfinityString{"-oo"};
    public:
        /// @todo Make these symbol pointers shared_ptrs
        Environment();
    public:
        void printErrorID(const std::string& module, int errorID, bool printCR);
        [[nodiscard]] std::shared_ptr<struct Void> getVoidConstant() const noexcept { return _voidConstant; }
        [[nodiscard]] auto getTrueSymbol() const noexcept { return _trueSymbol; }
        [[nodiscard]] auto getFalseSymbol() const noexcept { return _falseSymbol; }
        [[nodiscard]] std::shared_ptr<struct Lexeme> createLexeme(const std::string& text, unsigned short type);
        [[nodiscard]] auto createLexeme(const std::string& text, TreatAsSymbol) { return createLexeme(text, SYMBOL_TYPE); }
        [[nodiscard]] auto createLexeme(const std::string& text, TreatAsString) { return createLexeme(text, STRING_TYPE); }
        [[nodiscard]] auto createLexeme(const std::string& text, TreatAsInstanceName) { return createLexeme(text, INSTANCE_NAME_TYPE); }
        inline auto createSymbol(const std::string& text) { return createLexeme(text, TreatAsSymbol{}); }
        inline auto createString(const std::string& text) { return createLexeme(text, TreatAsString{}); }
        inline auto createInstanceName(const std::string& text) { return createLexeme(text, TreatAsInstanceName{}); }
        Lexeme::Ptr createBoolean(bool value) noexcept;
        Integer::Ptr createInteger(Integer::BackingType value);
        Float::Ptr createFloat(Float::BackingType value);
        BitMap::Ptr createBitmap(BitMap::Ptr target);
        template<typename T, typename ... Args, std::enable_if_t<std::is_base_of_v<BitMap, std::decay_t<T>>, int> = 0>
        typename T::Ptr createBitmap(Args&& ... args) {
            return createBitmap(std::make_shared<T>(*this, std::forward<Args>(args)...));
        }
        Lexeme::Ptr findSymbol(const std::string& name, unsigned short type) noexcept;
        Integer::Ptr findInteger(Integer::BackingType value) noexcept;

        /// @todo implement these later
        //ExternalAddress::Ptr createExternalAddress(std::any contents, unsigned short kind);
        [[nodiscard]] constexpr bool executionHalted() const noexcept { return _executionHalted; }
        [[nodiscard]] constexpr bool isSequenceOpMode() const noexcept { return _sequenceOpMode; }
        [[nodiscard]] constexpr bool isReturnContext() const noexcept { return _returnContext; }
        [[nodiscard]] constexpr bool isBreakContext() const noexcept { return _breakContext; }
        void setSequenceOpMode(bool sequenceOpMode) noexcept { _sequenceOpMode = sequenceOpMode; }
        void setReturnContext(bool returnContext) noexcept { _returnContext = returnContext; }
        void setBreakContext(bool breakContext) noexcept { _breakContext = breakContext; }
        void setHaltExecution(bool value) noexcept { _executionHalted = value; }
        void haltExecution() noexcept { setHaltExecution(true); }
        [[nodiscard]] constexpr bool getAbortExit() const noexcept { return _abortExit; }
        void setAbortExit(bool value) noexcept { _abortExit = value; }
        void abortExit() noexcept { setAbortExit(true); }
    private:
        bool insertRouter(Router::Ptr router);
    public:
        bool addRouter(std::function<Router::Ptr(Environment&)> makerFunction);
        bool addRouter(const std::string& name, int priority,
                       LambdaRouter::QueryFunction queryFn = nullptr,
                       LambdaRouter::WriteFunction writeFn = nullptr,
                       LambdaRouter::ReadFunction readFn = nullptr,
                       LambdaRouter::UnreadFunction unreadFn = nullptr,
                       LambdaRouter::ExitFunction exitFn = nullptr);
        void exitRouter(int code);
        int readRouter(const std::string& logicalName);
        int unreadRouter(const std::string& logicalName, int toUnread);
        void writeStringRouter(const std::string& logicalName, const std::string& str);
        template<typename ... Strs>
        void writeStringsRouter(const std::string& logicalName, Strs&& ... strings) {
            (writeStringRouter(logicalName, strings), ...);
        }
        void write(const std::string& str);
        void writeLine(const std::string& str);
        bool destroyRouter(const std::string& logicalName);
        bool queryRouter(const std::string& logicalName);
        bool deactivateRouter(const std::string& logicalName);
        bool activateRouter(const std::string& logicalName);
        Router::Ptr findRouter(const std::string& logicalName);
        bool printRouterExists(const std::string& logicalName);
        void unrecognizedRouterMessage(const std::string& logicalName);
        [[nodiscard]] constexpr bool isAwaitingInput() const noexcept { return _awaitingInput; }
        [[nodiscard]] constexpr size_t getCommandBufferInputCount() const noexcept { return _commandBufferInputCount; }
        [[nodiscard]] constexpr size_t getInputUngets() const noexcept { return _inputUngets; }
        [[nodiscard]] std::string getLineCountRouter() const noexcept { return _lineCountRouter; }
        void setLineCountRouter(const std::string& value) noexcept { _lineCountRouter = value; }
        void incrementLineCount() noexcept;
        void decrementLineCount() noexcept;
    public: // evaluation
        //void installPrimitive(EntityRecord::Ptr record, int position);
        //size_t installExternalAddressType(const externalAddressType &newType);
        void resetErrorFlags() noexcept;
        void setEvaluationError(bool value) noexcept;
        [[nodiscard]] constexpr auto getEvaluationError() const noexcept { return _evaluationError; }
    public:
        [[nodiscard]] std::function<void(Environment &, std::string, std::string, std::string, long)> getParserErrorCallback() const noexcept { return _parserErrorCallback; }
        void setParserErrorCallback(std::function<void(Environment &, std::string, std::string, std::string, long)> parserErrorCallback) { _parserErrorCallback = parserErrorCallback; }
    public:
        bool isClearReadyInProgress() const noexcept;
        void setClearReadyInProgress(bool clearReadyInProgress) noexcept;
        bool isClearInProgress() const noexcept;
        void setClearInProgress(bool clearInProgress) noexcept;
        bool isResetReadyInProgress() const noexcept;
        void setResetReadyInProgress(bool resetReadyInProgress) noexcept;
        bool isResetInProgress() const noexcept;
        void setResetInProgress(bool resetInProgress) noexcept;
        int16_t getClearReadyLocks() const noexcept;
        void setClearReadyLocks(int16_t clearReadyLocks) noexcept;
        int getDanglingConstructs() const noexcept;
        void setDanglingConstructs(int danglingConstructs) noexcept;
        bool isPrintWhileLoading() const noexcept;
        void setPrintWhileLoading(bool printWhileLoading) noexcept;
        bool isLoadInProgress() const noexcept;
        void setLoadInProgress(bool loadInProgress) noexcept;
        bool isWatchCompilations() const noexcept;
        void setWatchCompilations(bool watchCompilations) noexcept;
        bool isCheckSyntaxMode() const noexcept;
        void setCheckSyntaxMode(bool checkSyntaxMode) noexcept;
        bool isParsingConstruct() const noexcept;
        void setParsingConstruct(bool parsingConstruct) noexcept;
        std::string getErrorString() const noexcept;
        void setErrorString(const std::string &errorString) noexcept;
        std::string getWarningString() const noexcept;
        void setWarningString(const std::string &warningString) noexcept;
        std::string getParsingFileName() const noexcept;
        void setParsingFileName(const std::string &parsingFileName) noexcept;
        std::string getErrorFileName() const noexcept;
        void setErrorFileName(const std::string &errorFileName) noexcept;
        std::string getWarningFileName() const noexcept;
        void setWarningFileName(const std::string &warningFileName) noexcept;
        ssize_t getErrorLineNumber() const noexcept;
        void setErrorLineNumber(ssize_t errorLineNumber) noexcept;
        constexpr ssize_t getWarnLineNumber() const noexcept { return _warnLineNumber; }
        void setWarnLineNumber(ssize_t warnLineNumber) noexcept;
        constexpr ssize_t getErrorCaptureRouterCount() const noexcept { return _errorCaptureRouterCount; }
        void setErrorCaptureRouterCount(ssize_t errorCaptureRouterCount) noexcept;
        constexpr size_t getMaxErrorChars() const noexcept { return _maxErrorChars; }
        void setMaxErrorChars(size_t maxErrorChars) noexcept;
        constexpr size_t getCurrentErrorPosition() const noexcept { return _currentErrorPosition; }
        void setCurrentErrorPosition(size_t currentErrorPosition) noexcept;
        [[nodiscard]] constexpr size_t getMaxWarnChars() const noexcept { return _maxWarnChars; }
        void setMaxWarnChars(size_t maxWarnChars) noexcept;
        [[nodiscard]] constexpr size_t getCurrentWarnPosition() const noexcept { return _currentWarnPosition; }
        void setCurrentWarnPosition(size_t currentWarnPosition) noexcept;
        [[nodiscard]] constexpr bool isExecuting() const noexcept { return _executing; }
        void setExecuting(bool executing) noexcept;
        [[nodiscard]] std::function<bool(Environment &)> getBeforeResetCallback() const noexcept { return _beforeResetCallback; }
        void setBeforeResetCallback(std::function<bool(Environment &)> beforeResetCallback) noexcept;
        void clear();
        void reset();
        bool save(const std::string& path);
        bool addResetFunction(VoidCallFunctionItem::Ptr target) noexcept;
        template<typename ... Args>
        bool addResetFunction(Args&& ... args) noexcept {
            return addResetFunction(std::make_shared<VoidCallFunctionItem>(*this, std::forward<Args>(args)...));
        }
        bool removeResetFunction(const std::string& name) noexcept;
        bool addClearReadyFunction(BoolCallFunctionItem::Ptr target) noexcept;
        template<typename ... Args>
        bool addClearReadyFunction(Args&& ... args) noexcept {
            return addClearReadyFunction(std::make_shared<BoolCallFunctionItem>(*this, std::forward<Args>(args)...));
        }
        bool removeClearReadyFunction(const std::string& name) noexcept;
        bool addClearFunction(VoidCallFunctionItem::Ptr) noexcept;
        template<typename ... Args>
        bool addClearFunction(Args&& ... args) noexcept {
            return addClearFunction(std::make_shared<VoidCallFunctionItem>(*this, std::forward<Args>(args)...));
        }
        bool removeClearFunction(const std::string& name) noexcept;
        void incrementClearReadyLocks() noexcept;
        void decrementClearReadyLocks() noexcept;


    private: // symbol
        Void::Ptr _voidConstant;
        DataTable<Lexeme> _symbolTable;
        DataTable<Float> _floatTable;
        DataTable<Integer> _integerTable;
        DataTable<BitMap> _bitmapTable;
        /// @todo reimplement this later
        //DataTable<ExternalAddress> _externalAddressTable;
        Lexeme::Ptr _positiveInfinity;
        Lexeme::Ptr _negativeInfinity;
        Integer::Ptr _zero;
        Lexeme::Ptr _trueSymbol;
        Lexeme::Ptr _falseSymbol;
#if BSAVE_INSTANCES
        unsigned long NumberOfSymbols;
    unsigned long NumberOfFloats;
    unsigned long NumberOfIntegers;
    unsigned long NumberOfBitMaps;
    unsigned long NumberOfExternalAddresses;
    CLIPSLexeme **SymbolArray;
    Float **FloatArray;
    Integer **IntegerArray;
    BitMap **BitMapArray;
    ExternalAddress **ExternalAddressArray;
#endif
    private: // expression
        bool _sequenceOpMode = false;
        bool _returnContext = false;
        bool _breakContext = false;
    private: // router
        bool _abortExit = false;
        std::list<Router::Ptr> _listOfRouters;
        bool _awaitingInput = true;
        size_t _commandBufferInputCount = 0;
        size_t _inputUngets = 0;
        std::string _lineCountRouter;
    private: // evaluation
        std::shared_ptr<class Expression> _currentExpression;
        bool _evaluationError = false;
        bool _executionHalted = false;
        size_t _currentEvaluationDepth = 0;
        size_t _numberOfAddressTypes = 0;
        // this is not necessary because C++ merges the entity record and the entity instance together
        // it is called a class/struct
        //std::array<EntityRecord::Ptr, MAXIMUM_PRIMITIVES> PrimitivesArray;
        /// @todo make this a std::list instead to remove hardcoded limits
        //std::array<externalAddressType::Ptr, MAXIMUM_EXTERNAL_ADDRESS_TYPES> ExternalAddressTypes;
    private: // construct
        bool _clearReadyInProgress = false;
        bool _clearInProgress = false;
        bool _resetReadyInProgress = false;
        bool _resetInProgress = false;
        int16_t _clearReadyLocks = 0;
        int _danglingConstructs = 0;
        // std::list<SaveCallFunctionItem::Ptr> _listOfSaveFunctions;
        bool _printWhileLoading = false;
        bool _loadInProgress = false;
        bool _watchCompilations = false;
        bool _checkSyntaxMode = false;
        bool _parsingConstruct = false;
        std::string _errorString,
                    _warningString,
                    _parsingFileName,
                    _errorFileName,
                    _warningFileName;
        ssize_t _errorLineNumber = 0;
        ssize_t _warnLineNumber = 0;
        ssize_t _errorCaptureRouterCount = 0;
        size_t _maxErrorChars = 0;
        size_t _currentErrorPosition = 0;
        size_t _maxWarnChars = 0;
        size_t _currentWarnPosition = 0;
        std::function<void(Environment&, std::string, std::string, std::string, long)> _parserErrorCallback;
        //std::map<std::string, Construct::Ptr> _listOfConstructs;
        std::forward_list<VoidCallFunctionItem::Ptr> _resetFunctions;
        std::forward_list<VoidCallFunctionItem::Ptr> _clearFunctions;
        std::forward_list<BoolCallFunctionItem::Ptr> _clearReadyFunctions;
        bool _executing = false;
        std::function<bool(Environment&)> _beforeResetCallback;
    };
} // end namespace maya

#endif /* _H_envrnmnt */

