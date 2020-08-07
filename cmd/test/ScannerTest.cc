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
template<typename T, typename E>
void expect(maya::Environment::Ptr env, const std::string& logicalName, const std::string& kind, T comparison, maya::Token::Type expectedType) noexcept {
    auto tok = env->getToken(logicalName);
    std::cout << "Expecting a " << kind << std::endl;
    std::cout << "\tIs a " << kind << "? " << std::boolalpha << (tok.getType() == expectedType) << std::endl;
    std::cout << "\tHas the value " << comparison << "? " << std::boolalpha;
    std::visit([&comparison](auto&& value) {
        using K = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<E, K>) {
            std::cout << (value->getContents() == comparison);
            std::cout << " (" << value->getContents()  << " == " << comparison << ")";
        } else {
            std::cout << "Wrong type so false!";
        }
    }, tok.getContents());
    std::cout << std::endl;
}

/**
 * @brief Expect function for the non atomic tokens ('(', ')', '&', '|', etc)
 */
void expect(maya::Environment::Ptr env, const std::string& logicalName, const std::string& kind, maya::Token::Type expectedType) noexcept {
    auto tok = env->getToken(logicalName);
    std::cout << "Expecting a " << kind << std::endl;
    std::cout << "\tIs a " << kind << "? " << std::boolalpha << (tok.getType() == expectedType) << std::endl;
    std::cout << std::endl;
}
int main(int argc, char** argv) {
    auto env = std::make_shared<maya::Environment>();
    auto expectSymbol = [env](const std::string& target) { expect<const std::string&, maya::Lexeme::Ptr>(env, "test", "symbol", target, maya::Token::Type::Symbol); };
    auto expectInteger = [env](int64_t target) { expect<int64_t, maya::Integer::Ptr>(env, "test", "integer", target, maya::Token::Type::Integer); };
    auto expectFloat = [env](double target) { expect<double, maya::Float::Ptr>(env, "test", "integer", target, maya::Token::Type::Float); };
    auto expectOpenParen = [env]() { expect(env, "test", "open paren", maya::Token::Type::LeftParen); };
    auto expectCloseParen = [env]() { expect(env, "test", "close paren", maya::Token::Type::RightParen); };
    env->addRouter([](maya::Environment& env) -> maya::Router::Ptr {
        auto ptr = std::make_shared<StringParsingRouter>(env, "test", 50);
        ptr->setStringToParse("a b cat 1 -2 +3 4.0 4.1 +4.0 -4.2 (+ 1 2 3 4 5)");
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

    expectOpenParen();
    expectSymbol("+");
    expectInteger(1);
    expectInteger(2);
    expectInteger(3);
    expectInteger(4);
    expectInteger(5);
    expectCloseParen();

    return 0;
}


