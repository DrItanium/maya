//
// Created by jwscoggins on 7/12/20.
//

#include "Entities.hxx"
#include "UserSetup.h"
#include <variant>
#include <type_traits>

namespace maya {
    void
    HoldsOntoGenericValue::retain() {
#if STUBBING_INACTIVE
        std::visit([](auto &&value) {
            using K = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<K, std::monostate> || std::is_same_v<K, CLIPSVoid::Ptr>) {
                // do nothing
            } else {
                value->retain();
            }
        }, contents);
#endif
    }
    void
    HoldsOntoGenericValue::release() {
#if STUBBING_INACTIVE
        std::visit([](auto &&value) {
            using K = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<K, std::monostate> || std::is_same_v<K, CLIPSVoid::Ptr>) {
                // do nothing
            } else {
                value->release();
            }
        }, contents);
#endif
    }

    bool
    HoldsOntoGenericValue::canRelease() const noexcept {
#if STUBBING_INACTIVE
        return std::visit([](auto &&value) {
            using K = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<K, std::monostate> || std::is_same_v<K, CLIPSVoid::Ptr>) {
                return false;
            } else {
                return value->canRelease();
            }
        }, contents);
#else
        return false;
#endif
    }
}