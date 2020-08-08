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

    bool
    Value::evaluate(UDFValue::Ptr retVal) {
        return std::visit([this, retVal](auto&& value) -> bool {
            if (value) {
                return value->evaluate(retVal);
            } else {
                retVal->setContents(getParent().getFalseSymbol());
                return false;
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