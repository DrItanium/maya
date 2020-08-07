//
// Created by jwscoggins on 8/7/20.
//

#include "Environment.h"
#include "Token.h"
#include <iostream>

int main(int argc, char** argv) {
    auto environment = std::make_shared<maya::Environment>();
    environment->addRouter("stdio", 50,
    [](maya::Environment& env, const std::string& logicalName) { return logicalName == "stdio"; },
    [](maya::Environment& env, const std::string& lName, const std::string& text) {
        std::cout << text;
        },
    [](maya::Environment& env, const std::string& lName) {
        if (auto value = std::cin.get(); std::cin.good()) {
            return value;
        } else {
            return EOF;
        }
    },
    [](maya::Environment& env, const std::string& lName, int val) { return std::cin.putback(val).good() ? val : EOF; });
    environment->addRouter("stderr", 50,
                           [](maya::Environment&, const std::string& logicalName) { return logicalName == "stderr"; },
                           [](maya::Environment&, const std::string& lName, const std::string& text) { std::cerr << text; });
    int64_t callDepth = 0;
    for (auto i = environment->getToken("stdio"); !i.isStopToken(); i = environment->getToken("stdio")) {
        // insert a tab for each nested depth
        for (int64_t c = 0; c < callDepth; ++c) {
            environment->writeStringRouter("stdio", "\t");
        }
        i.dump(*environment, "stdio");
        environment->writeStringRouter("stdio", "\n");
        switch (i.getType()) {
            case maya::Token::Type::LeftParen:
                ++callDepth;
                break;
            case maya::Token::Type::RightParen:
                --callDepth;
                break;
            default:
                break;
        }
        if (callDepth < 0) {
            environment->writeStringRouter("stderr", "Found a paren mismatch at this point\n");
            return 1;
        }
    }
    if (callDepth != 0) {
        environment->writeStringRouter("stderr", "Not all parens were matched!\n");
        return 1;
    }
    return 0;
}