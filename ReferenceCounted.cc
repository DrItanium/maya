//
// Created by jwscoggins on 7/11/20.
//

#include "ReferenceCounted.h"
#include "Problem.h"
namespace maya {
    void
    ReferenceCounted::release() {
        if (_count < 0) {
            throw Problem("unbalanced retain release");
        }
        --_count;
    }

    void
    ReferenceCounted::retain() {
        ++_count;
    }

    bool
    ReferenceCounted::canRelease() const noexcept {
        return !_permanent && _count == 0;
    }
}