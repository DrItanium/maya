//
// Created by jwscoggins on 7/26/20.
//

#include "BitmapAtom.h"

namespace maya {
    bool
    BitMap::contentsEqual(Ptr other) const {
        return (getTypeHashCode() == other->getTypeHashCode()) && compareInternals(other);
    }
} // end namespace maya