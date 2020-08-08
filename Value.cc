//
// Created by jwscoggins on 7/26/20.
//

#include "Value.h"
#include "Environment.h"
#include <variant>
namespace maya {
    size_t
    Value::hash(size_t range) const {
        return std::visit([range](auto&& value) -> size_t {
                if (value) {
                    return value->hash(range);
                } else {
                    return 0;
                }
            }, _contents);
    }

    UDFValue::Ptr
    Value::evaluate() {
        return std::visit([this](auto&& value) {
            if (value) {
                return value->evaluate();
            } else {
                return std::make_shared<UDFValue>(getParent(), getParent().getFalseSymbol(), nullptr);
            }
        }, _contents);
    }


#if 0
    bool
    operator==(const UDFValue& a, const UDFValue& b) {
       if (a.getType() != b.getType()) {
            return false;
       }
       /// @todo have to handle multifield equality checks
       return a.getValue() == b.getValue();
    }
#endif
}