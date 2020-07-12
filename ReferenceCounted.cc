//
// Created by jwscoggins on 7/11/20.
//

#include "ReferenceCounted.h"

void
ReferenceCounted::release() {
    if (_count < 0) {
        throw "unbalanced retain release";
    }
    --_count;
}

void
ReferenceCounted::retain() {
    ++_count;
}