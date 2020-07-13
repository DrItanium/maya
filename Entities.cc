//
// Created by jwscoggins on 7/12/20.
//
#include "Entities.h"
#include "Environment.h"


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

UDFContext::UDFContext(Environment &parent) : environment(parent) {}