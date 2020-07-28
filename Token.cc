//
// Created by jwscoggins on 7/28/20.
//

#include "Token.h"
#include "Constants.h"

namespace maya {
    Token::Token(const Token & other) : Token(other._tokenType, other._printForm, other._contents) { }
} // end namespace maya
