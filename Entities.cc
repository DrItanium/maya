//
// Created by jwscoggins on 7/12/20.
//
#include "Entities.hxx"
#include "Environment.h"
#include <type_traits>
#include "Fact.h"
#include "Object.h"


//size_t hash(size_t range) override;
size_t
CLIPSExternalAddress::hash(size_t range) {
    /// @todo implement
    return 0;
}

size_t
CLIPSBitMap::hash(size_t range) {
    /// @todo implement
    return 0;

}

size_t
CLIPSLexeme::hash(size_t range) {
    size_t tally = 0;
    for (const auto& c : contents) {
        tally = tally * 127 + (size_t)c;
    }
    if (range == 0) {
        return tally;
    } else {
        return tally % range;
    }
}

size_t
CLIPSInteger::hash(size_t range) {
#if WIN_MVC
    auto tmp = contents;
    if (tmp < 0) {
        tmp = -tmp;
    }
    return (((size_t)tmp) % range);
#else
    return (((size_t) llabs(contents)) % range);
#endif
}

size_t
CLIPSFloat::hash(size_t range) {
    size_t tally = 0;
    union {
        decltype(contents) value;
        char word[sizeof(decltype(contents))];
    } view;
    view.value = contents;
    for (int i = 0; i < sizeof(decltype(contents)); ++i) {
        tally = tally * 127 + (size_t) view.word[i];
    }
    if (range == 0) {
        return tally;
    } else {
        return tally % range;
    }
}

UDFContext::UDFContext(Environment &parent) : HoldsEnvironmentCallback(parent) {}

void
Multifield::retain() {
    ++_busyCount;
}
void
Multifield::release() {
    --_busyCount;
}

bool
Multifield::canRelease() const noexcept {
    return _busyCount == 0;
}

void
PatternEntity::retain() {
    ++_busyCount;
}
void
PatternEntity::release() {
    --_busyCount;
}

bool
PatternEntity::canRelease() const noexcept {
    return _busyCount == 0;
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


