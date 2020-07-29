//
// Created by jwscoggins on 7/28/20.
//

#ifndef MAYA_TOKEN_H
#define MAYA_TOKEN_H
#include "LexemeAtom.h"
#include "FloatAtom.h"
#include "IntegerAtom.h"
#include <memory>
#include <variant>
namespace maya {
    class Token {
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
    public:
        Token() = default;
        Token(Type type, const std::string& printForm, Contents contents) : _tokenType(type), _printForm(printForm), _contents(contents)  { }
        Token(const Token& other);
        constexpr auto getType() const noexcept { return _tokenType; }
        std::string getPrintForm() const noexcept { return _printForm; }
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
    private:
        Type _tokenType = Type::Unknown;
        std::string _printForm{"unknown"};
        Contents _contents = nullptr;
    };
}

#endif //MAYA_TOKEN_H
