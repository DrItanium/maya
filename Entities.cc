//
// Created by jwscoggins on 7/12/20.
//
#include "Entities.hxx"
#include "Environment.h"
#include <type_traits>
//#include "Fact.h"
//#include "Object.h"
#include <sstream>

namespace maya {

//size_t hash(size_t range) override;
    size_t
    ExternalAddress::hash(size_t range) {
        /// @todo implement
        return 0;
    }

    size_t
    BitMap::hash(size_t range) {
        /// @todo implement
        return 0;
    }

    size_t
    Lexeme::hash(size_t range) {
        size_t tally = 0;
        for (const auto &c : _contents) {
            tally = tally * 127 + (size_t) c;
        }
        if (range == 0) {
            return tally;
        } else {
            return tally % range;
        }
    }

    size_t
    Integer::hash(size_t range) {
#if WIN_MVC
        auto tmp = contents;
        if (tmp < 0) {
            tmp = -tmp;
        }
        return (((size_t)tmp) % range);
#else
        return (((size_t) llabs(_contents)) % range);
#endif
    }

    size_t
    Float::hash(size_t range) {
        size_t tally = 0;
        union {
            decltype(_contents) value;
            char word[sizeof(decltype(_contents))];
        } view;
        view.value = _contents;
        for (int i = 0; i < sizeof(decltype(_contents)); ++i) {
            tally = tally * 127 + (size_t) view.word[i];
        }
        if (range == 0) {
            return tally;
        } else {
            return tally % range;
        }
    }

    UDFContext::UDFContext(Environment &parent) : HoldsEnvironmentCallback(parent) {}

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

    void
    Lexeme::write(const std::string &logicalName) {
        _parent.writeString(logicalName, _contents);
    }

    template<typename T>
    std::string toString(T value) {
        std::stringstream converter;
        converter << value;
        auto target = converter.str();
        return target;
    }

    void
    Float::write(const std::string &logicalName) {
        _parent.writeString(logicalName, toString(_contents));
    }

    void
    Integer::write(const std::string &logicalName) {
        _parent.writeString(logicalName, toString(_contents));
    }

    void
    BitMap::write(const std::string &logicalName) {
        /// @todo implement
    }
    void
    ExternalAddress::write(const std::string& logicalName) {
        /// @todo implement
    }
    bool
    Entity::evaluate(std::shared_ptr<UDFValue> returnValue) {
        return false;
    }
    void
    Entity::longPrint(const std::string &logicalName) {

    }
    void
    Entity::shortPrint(const std::string &logicalName) {

    }
    void
    PatternEntity::incrementBasisCount() {

    }
    void
    PatternEntity::decrementBasisCount() {

    }
    void
    PatternEntity::onMatch() {

    }
    bool
    PatternEntity::synchronized() {
        return true;
    }
} // end namespace maya