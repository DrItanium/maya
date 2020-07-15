//
// Created by jwscoggins on 7/15/20.
//

#include "BusyCountable.h"

namespace maya {
    void
    BusyCountable::retain() {
        ++_busyCount;
    }
    void
    BusyCountable::release() {
        --_busyCount;
    }
    bool
    BusyCountable::canRelease() const noexcept {
        return _busyCount == 0;
    }
}
