//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_TYPEHEADER_H
#define MAYA_TYPEHEADER_H
#include "Constants.h"

namespace maya {
    struct TypeHeader {
        TypeHeader(unsigned short t = 0) : _type(t) {}
        [[nodiscard]] constexpr auto getType() const noexcept { return _type; }
        [[nodiscard]] constexpr auto isConstantType() const noexcept { return maya::isConstantType(_type); }
    private:
        unsigned short _type;
    };
}
#endif //MAYA_TYPEHEADER_H
