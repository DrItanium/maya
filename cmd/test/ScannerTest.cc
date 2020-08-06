//
// Created by jwscoggins on 8/6/20.
//

#include "Environment.h"
#include "Router.h"
#include <string>
#include <iostream>
#include <memory>
#include <type_traits>
class StringParsingRouter : public maya::Router {
public:
    using Parent = maya::Router;
public:
    using Parent::Parent;
    ~StringParsingRouter() override = default;
    void setStringToParse(const std::string& value) noexcept { _contents.str(value); }
    bool canQuery() const noexcept override { return true; }
    bool canWriteTo() const noexcept override { return false; }
    bool canRead() const noexcept override { return true; }
    bool canExit() const noexcept override { return false; }
    bool canUnread() const noexcept override { return true; }
    void onExit(int exitCode) override { }
    void write(const std::string& logicalName, const std::string& value) override { }
    int unread(const std::string& logicalName, int value) override {
        _contents.putback(value);
        if (_contents.good()) {
            return value;
        } else {
            return EOF;
        }
    }
    int read(const std::string& logicalName) override {
        if (_contents.good()) {
            return _contents.get();
        } else {
            return EOF;
        }
    }
    bool query(const std::string& logicalName) override { return logicalName == getName(); }
private:
    std::istringstream _contents;
};
int main(int argc, char** argv) {
    auto env = std::make_shared<maya::Environment>();
    auto expectSymbol = [env](const std::string& target) {
        auto first = env->getToken("test");
        std::cout << "Expecting a symbol '" << target << "'" << std::endl;
        std::cout << "\tIs a symbol? " << std::boolalpha << (first.getType() == maya::Token::Type::Symbol) << std::endl;
        std::cout << "\tIs is word '" << target << "? " << std::boolalpha;
        std::visit([&target](auto&& value) {
            using K = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<maya::Lexeme::Ptr, K>) {
                auto lexemeValue = value;
                std::cout << (lexemeValue->getContents() == target);
                std::cout << "(" << lexemeValue->getContents() << "[" << lexemeValue->getContents().length() << "] == "
                          << target << "[" << target.length() << "])";
            } else {
                std::cout << false;
            }
        }, first.getContents());
        std::cout << std::endl;
    };
    auto expectInteger = [env](int64_t value) {
        auto nextToken = env->getToken("test");
        std::cout << "Expecting an integer '" << value << "'" << std::endl;
        std::cout << "\tIs an integer? " << std::boolalpha << (nextToken.getType() == maya::Token::Type::Integer) << std::endl;
        std::cout << "\tIs is the number " << value << "? " << std::boolalpha;
        std::visit([ival = value](auto&& value) {
            using K = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<maya::Integer::Ptr, K>) {
                auto integerValue = value;
                std::cout << (integerValue->getContents() == ival);
                std::cout << "(" << integerValue->getContents() << " == " << ival << ")";
            } else {
                std::cout << false;
            }
        }, nextToken.getContents());
        std::cout << std::endl;
    };

    auto expectFloat = [env](double value) {
        auto nextToken = env->getToken("test");
        std::cout << "Expecting an integer '" << value << "'" << std::endl;
        std::cout << "\tIs an integer? " << std::boolalpha << (nextToken.getType() == maya::Token::Type::Float) << std::endl;
        std::cout << "\tIs is the number " << value << "? " << std::boolalpha;
        std::visit([fval = value](auto&& value) {
            using K = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<maya::Float::Ptr, K>) {
                auto integerValue = value;
                std::cout << (integerValue->getContents() == fval);
                std::cout << "(" << integerValue->getContents() << " == " << fval << ")";
            } else {
                std::cout << false;
            }
        }, nextToken.getContents());
        std::cout << std::endl;
    };
    env->addRouter([](maya::Environment& env) -> maya::Router::Ptr {
        auto ptr = std::make_shared<StringParsingRouter>(env, "test", 50);
        ptr->setStringToParse("a b cat 1 -2 +3 4.0 4.1 +4.0 -4.2");
        return ptr;
    });
    expectSymbol("a");
    expectSymbol("b");
    expectSymbol("cat");

    expectInteger(1);
    expectInteger(-2);
    expectInteger(+3);

    expectFloat(4.0);
    expectFloat(4.1);
    expectFloat(+4.0);
    expectFloat(-4.2);


    return 0;
}


