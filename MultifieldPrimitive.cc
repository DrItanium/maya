//
// Created by jwscoggins on 7/26/20.
//

#include "MultifieldPrimitive.h"

namespace maya {
    bool
    Multifield::evaluate(UDFValue::Ptr retVal) {

        return true;
    }

    size_t
    Multifield::hash(size_t range) const {
        size_t count = 0;
        size_t index = 0;
        for (const auto& target : contents) {
            auto intermediateHash = target->hash(range);
            count += (intermediateHash * (index + 29));
            ++index;
        }
        return count;
    }
}
