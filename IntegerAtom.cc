//
// Created by jwscoggins on 7/26/20.
//

#include "IntegerAtom.h"
#include "Environment.h"
#include <sstream>
#include "Setup.h"
#include <memory>
std::ostream&
operator<<(std::ostream& os, const maya::Integer& value) {
    os << value.getContents();
    return os;
}
namespace maya {
    size_t
    Integer::hash(size_t range) const {
#if WIN_MVC
        auto tmp = contents;
        if (tmp < 0) {
            tmp = -tmp;
        }
        return (((size_t)tmp) % range);
#else
        return (((size_t) llabs(_contents)) % range);
#endif
    }
    void
    Integer::write(const std::string &logicalName) {
        std::stringstream ss;
        ss << _contents;
        auto tmp = ss.str();
        getParent().writeStringRouter(logicalName, tmp);
    }

    UDFValue::Ptr
    Integer::evaluate() {
        return std::make_shared<UDFValue>(getParent(), shared_from_this(), nullptr);
    }


} // end namespace maya