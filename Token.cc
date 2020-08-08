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

    void
    Token::dump(const std::string &logicalName) {
        getParent().writeStringsRouter(logicalName,  "{ ", toString(_tokenType),
                               ", \"", _printForm, "\", " );
        std::visit([this, &logicalName](auto&& value) {
            using K = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<K, std::nullptr_t>) {
                getParent().writeStringRouter(logicalName, "nil");
            } else {
                value->write(logicalName);
            }
        }, _contents);
        getParent().writeStringRouter(logicalName, " }");
    }
    void Token::write(const std::string &logicalName) {
        std::visit([this, logicalName](auto&& value) {
            using K = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<K, Lexeme::Ptr> || std::is_same_v<K, Integer::Ptr> || std::is_same_v<K, Float::Ptr>) {
                value->write(logicalName);
            } else {
                // do nothing for the time being, we can jump through the types later on
            }
        }, _contents);
    }
} // end namespace maya
