//
// Created by jwscoggins on 7/26/20.
//

#include "Value.h"
namespace maya {
    size_t
    Value::hash(size_t range) const {
        return _contents.has_value() ? std::any_cast<Hashable::Ptr>(_contents)->hash(range) : 0;
    }

    bool
    Value::evaluate(UDFValue::Ptr retVal) {
        return _contents.has_value() ? std::any_cast<Evaluable::Ptr>(_contents)->evaluate(retVal) : false;
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