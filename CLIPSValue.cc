//
// Created by jwscoggins on 7/12/20.
//

#include "Entities.hxx"
#include "Fact.h"
#include "Object.h"
#include <variant>
#include <type_traits>


void
HoldsOntoGenericValue::retain() {
    std::visit([](auto&& value) {
        using K = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<K, std::monostate> || std::is_same_v<K, CLIPSVoid::Ptr>) {
            // do nothing
        } else {
            value->retain();
        }
    }, contents);
}
void
HoldsOntoGenericValue::release() {
    std::visit([](auto&& value) {
        using K = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<K, std::monostate> || std::is_same_v<K, CLIPSVoid::Ptr>) {
            // do nothing
        } else {
            value->release();
        }
    }, contents);
}

bool
HoldsOntoGenericValue::canRelease() const noexcept {
    return std::visit([](auto&& value) {
        using K = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<K, std::monostate> || std::is_same_v<K, CLIPSVoid::Ptr>) {
            return false;
        } else {
            return value->canRelease();
        }
    }, contents);
}
