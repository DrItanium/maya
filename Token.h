//
// Created by jwscoggins on 7/28/20.
//

#ifndef MAYA_TOKEN_H
#define MAYA_TOKEN_H
#include "LexemeAtom.h"
#include "FloatAtom.h"
#include "IntegerAtom.h"
#include "IORouterAware.h"
#include <memory>
#include <variant>
namespace maya {
    class Token : public HoldsEnvironmentCallback, public IORouterAware {
    public:
        using Self = Token;
        using Ptr = std::shared_ptr<Self>;
        using Contents = std::variant<std::nullptr_t, Lexeme::Ptr, Float::Ptr, Integer::Ptr>;
    public:
        enum class Type {
            Symbol = 1025,
            String,
            InstanceName,
            Float,
            Integer,
            LeftParen,
            RightParen,
            SFVariable,
            MFVariable,
            GlobalVariable,
            SFWildcard,
            MFWildcard,
            MFGlobalVariable,
            NotConstraint,
            AndConstraint,
            OrConstraint,
            Stop,
            Unknown,
        };
        static const std::string& toString(Type t) noexcept;
    public:
        Token(Environment& parent) : HoldsEnvironmentCallback(parent) {}
        Token(Environment& parent, Type type, const std::string& printForm, Contents contents) : HoldsEnvironmentCallback(parent), _tokenType(type), _printForm(printForm), _contents(contents)  { }
        constexpr auto getType() const noexcept { return _tokenType; }
        void setType(Type value) noexcept { _tokenType = value; }
        std::string getPrintForm() const noexcept { return _printForm; }
        void setPrintForm(const std::string& value) noexcept { _printForm = value; }
        void setContents(Contents contents) noexcept { _contents = contents; }
        const Contents& getContents() const noexcept { return _contents; }
        constexpr uint16_t getGlobalType() const noexcept {
            switch (_tokenType) {
                case Type::Float: return FLOAT_TYPE;
                case Type::Integer: return INTEGER_TYPE;
                case Type::Symbol: return SYMBOL_TYPE;
                case Type::String: return STRING_TYPE;
                case Type::InstanceName: return INSTANCE_NAME_TYPE;
                case Type::SFVariable: return SF_VARIABLE;
                case Type::MFVariable: return MF_VARIABLE;
                case Type::GlobalVariable: return GBL_VARIABLE;
                case Type::MFGlobalVariable: return MF_GBL_VARIABLE;
                default: return VOID_TYPE;
            }
        }
        constexpr bool isStopToken() const noexcept { return _tokenType == Type::Stop; }
        void dump(const std::string& logicalName);
        constexpr bool isNumber() const noexcept { return _tokenType == Type::Integer || _tokenType == Type::Float; }
        constexpr bool isLexeme() const noexcept { return _tokenType == Type::String || _tokenType == Type::Symbol || _tokenType == Type::InstanceName; }
        constexpr bool isConstant() const noexcept { return isNumber() || isLexeme(); }
        constexpr bool isVariable() const noexcept {
            switch(_tokenType) {
                case Type::GlobalVariable:
                case Type::MFGlobalVariable:
                case Type::SFVariable:
                case Type::MFVariable:
                    return true;
                default:
                    return false;
            }
        }
        void write(const std::string &logicalName) override;
    private:
        Type _tokenType = Type::Unknown;
        std::string _printForm{"unknown"};
        Contents _contents = nullptr;
    };
}

#endif //MAYA_TOKEN_H
