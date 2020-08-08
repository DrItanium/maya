//
// Created by jwscoggins on 7/26/20.
//

#include "MultifieldPrimitive.h"
#include "Expression.h"

namespace maya {
    UDFValue::Ptr
    Multifield::evaluate() {
        return std::make_shared<UDFValue>(getParent(), getParent().getFalseSymbol(), nullptr);
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
