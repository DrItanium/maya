//
// Created by jwscoggins on 7/12/20.
//

#include "Entities.h"
#include "Fact.h"
#include "Object.h"
#include <variant>
#include <type_traits>


void
CLIPSValue::retain() {
    std::visit([](auto&& value){
        using K = std::decay_t<decltype(value)>;
        if constexpr (!std::is_same_v<K, std::monostate> &&
                      !std::is_same_v<K, Fact::Ptr> &&
                      !std::is_same_v<K, CLIPSVoid::Ptr>) {
            value->retain();
        }
    }, contents);
}

void
CLIPSValue::release() {
    std::visit([](auto&& value){
        using K = std::decay_t<decltype(value)>;
        if constexpr (!std::is_same_v<K, std::monostate> &&
                      !std::is_same_v<K, Fact::Ptr> &&
                      !std::is_same_v<K, CLIPSVoid::Ptr>) {
            value->release();
        }
    }, contents);

}