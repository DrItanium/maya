//
// Created by jwscoggins on 7/26/20.
//

#include "FloatAtom.h"
#include "Environment.h"
#include <sstream>
namespace maya {
    void
    Float::write(const std::string &logicalName) {
        std::stringstream converter;
        converter << _contents;
        auto target = converter.str();
        getParent().writeStringRouter(logicalName, target);
    }
    size_t
    Float::hash(size_t range) const {
        size_t tally = 0;
        union {
            decltype(_contents) value;
            char word[sizeof(decltype(_contents))];
        } view;
        view.value = _contents;
        for (int i = 0; i < sizeof(decltype(_contents)); ++i) {
            tally = tally * 127 + (size_t) view.word[i];
        }
        if (range == 0) {
            return tally;
        } else {
            return tally % range;
        }
    }
    UDFValue::Ptr
    Float::evaluate() {
        return std::make_shared<UDFValue>(getParent(), shared_from_this(), nullptr);
    }
} // end namespace maya

std::ostream&
operator<<(std::ostream& os, const maya::Float& value) {
    os << value.getContents();
    return os;
}
