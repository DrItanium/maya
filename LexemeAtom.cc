//
// Created by jwscoggins on 7/26/20.
//

#include "LexemeAtom.h"
#include "Environment.h"
#include <string>
#include <iostream>
namespace maya {
    size_t
    Lexeme::hash(size_t range) const {
        size_t tally = 0;
        for (const auto &c : _contents) {
            tally = tally * 127 + (size_t) c;
        }
        if (range == 0) {
            return tally;
        } else {
            return tally % range;
        }
    }
    void
    Lexeme::write(const std::string &logicalName) {
        getParent().writeStringRouter(logicalName, _contents);
    }
    bool
    Lexeme::evaluate(UDFValue::Ptr retVal) {
        retVal->setContents(this->shared_from_this());
        return true;
    }
}

std::ostream&
operator<<(std::ostream& os, const maya::Lexeme& lexeme) {
    os << lexeme.getContents();
    return os;
}
