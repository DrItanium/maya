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
        static inline const std::string TrueString{"TRUE"};
        static inline const std::string FalseString{"FALSE"};
        static inline const std::string PositiveInfinityString{"+oo"};
        static inline const std::string NegativeInfinityString{"-oo"};
    public:
        /// @todo Make these symbol pointers shared_ptrs
        Environment();
    private:
        Void::Ptr _voidConstant;
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
        const std::unique_ptr<T>& getEnvironmentModule() const {
            if (_modules.find(typeid(T)) != _modules.end()) {
                return (std::unique_ptr<T>&)_modules.at(typeid(T));
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
        void haltExecution(bool value = true) noexcept { _executionHalted = value; }
        constexpr bool executionHalted() const noexcept { return _executionHalted; }
        Void::Ptr getVoidConstant() const noexcept { return _voidConstant; }
        Lexeme::Ptr getTrueSymbol() const noexcept { return _trueSymbol; }
        Lexeme::Ptr getFalseSymbol() const noexcept { return _falseSymbol; }
        Lexeme::Ptr createLexeme(const std::string& text, unsigned short type);
        Lexeme::Ptr createLexeme(const std::string& text, TreatAsSymbol) { return createLexeme(text, SYMBOL_TYPE); }
        Lexeme::Ptr createLexeme(const std::string& text, TreatAsString) { return createLexeme(text, STRING_TYPE); }
        Lexeme::Ptr createLexeme(const std::string& text, TreatAsInstanceName) { return createLexeme(text, INSTANCE_NAME_TYPE); }
        inline Lexeme::Ptr createSymbol(const std::string& text) { return createLexeme(text, TreatAsSymbol{}); }
        inline Lexeme::Ptr createString(const std::string& text) { return createLexeme(text, TreatAsString{}); }
        inline Lexeme::Ptr createInstanceName(const std::string& text) { return createLexeme(text, TreatAsInstanceName{}); }
        Lexeme::Ptr createBoolean(bool value) noexcept;
        Integer::Ptr createInteger(Integer::BackingType value);
        Float::Ptr createFloat(Float::BackingType value);
        /// @todo implement these later
        //BitMap::Ptr create();
        //ExternalAddress::Ptr createExternalAddress(std::any contents, unsigned short kind);
    public:
        template<typename T>
        using DataTable = std::multimap<size_t, typename T::Ptr>;
    private:
        DataTable<Lexeme> _symbolTable;
        DataTable<Float> _floatTable;
        DataTable<Integer> _integerTable;
        /// @todo reimplement this later
        //DataTable<BitMap> _bitmapTable;
        //DataTable<ExternalAddress> _externalAddressTable;
        Lexeme::Ptr _positiveInfinity;
        Lexeme::Ptr _negativeInfinity;
        Integer::Ptr _zero;
        Lexeme::Ptr _trueSymbol;
        Lexeme::Ptr _falseSymbol;
        bool _executionHalted = false;
    };
} // end namespace maya

#endif /* _H_envrnmnt */

