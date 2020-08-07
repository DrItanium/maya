//
// Created by jwscoggins on 7/28/20.
//

#include "Token.h"
#include "Constants.h"
#include "Environment.h"
#include <string>
#include <map>

namespace maya {
    const std::string&
    Token::toString(Type t) noexcept {
        static std::map<Type, std::string> lookupTable {
                { Type::String, "String"},
                { Type::Symbol, "Symbol"},
                { Type::RightParen, "Right Paren"},
                { Type::LeftParen, "Left Paren"},
                { Type::Stop, "Stop"},
                {Type::Float, "Float"},
                {Type::InstanceName, "InstanceName"},
                {Type::OrConstraint, "Or Constraint"},
                {Type::AndConstraint, "And Constraint"},
                { Type::NotConstraint, "Not Constraint"},
                { Type::Integer, "Integer"},
                { Type::Unknown, "Unknown"},
                { Type::SFVariable, "Singlefield Variable"},
                { Type::SFWildcard, "Singlefield Wildcard"},
                { Type::MFVariable, "Multifield Variable"},
                { Type::MFWildcard, "Multifield Wildcard"},
                { Type::GlobalVariable, "Global Variable"},
                { Type::MFGlobalVariable, "Multifield Global Variable"}
        };
        static std::string undefinedType {"Undefined Type"};

        if (auto result = lookupTable.find(t); result != lookupTable.end()) {
            return result->second;
        } else {
            return undefinedType;
        }
    }

    Token::Token(const Token & other) : Token(other._tokenType, other._printForm, other._contents) { }
    void
    Token::dump(Environment &env, const std::string &logicalName) {
        env.writeStringsRouter(logicalName,  "{ ", toString(_tokenType),
                               ", \"", _printForm, "\", " );
        std::visit([&env, &logicalName](auto&& value) {
            using K = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<K, std::nullptr_t>) {
                env.writeStringRouter(logicalName, "nil");
            } else {
                value->write(logicalName);
            }
        }, _contents);
        env.writeStringRouter(logicalName, " }");
    }
} // end namespace maya
