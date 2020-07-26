//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_VALUE_H
#define MAYA_VALUE_H
#include <memory>
#include <any>
#include <variant>
#include "Atom.h"
#include "HoldsEnvironmentCallback.h"
namespace maya {
    /**
     * @brief Generic container to hold onto an Atom or another kind of thing
     */
    class Value : public HoldsEnvironmentCallback {
    public:
        using Self = Value;
        using Ptr = std::shared_ptr<Self>;
        using Contents = std::any;
    public:
        Value(Environment& parent, Contents atom) : HoldsEnvironmentCallback(parent), _contents(atom) { }
        [[nodiscard]] const Contents& getContents() const noexcept { return _contents; }
        void setContents(Contents value) noexcept { _contents = value; }
    private:
        Contents _contents;
    };

    class UDFValue : public Value {
    public:
        using Self = Value;
        using Ptr = std::shared_ptr<Self>;
    public:
        UDFValue(Environment& parent, std::any atom, std::any supplementalInfo, size_t begin = 0, size_t range = 0) : Value(parent, atom), _supplementalInfo(supplementalInfo), _begin(begin), _range(range) { }
        std::shared_ptr<struct Expression> toExpression();
        constexpr auto getBegin() const noexcept { return _begin; }
        void setBegin(size_t value) noexcept { _begin = value; }
        constexpr auto getRange() const noexcept { return _range; }
        void setRange(size_t value) noexcept { _range = value; }
        const auto& getSupplementalInfo() const noexcept { return _supplementalInfo; }
        void setSupplementalInfo(std::any value) noexcept { _supplementalInfo = value; }
    private:
        std::any _supplementalInfo;
        size_t _begin;
        size_t _range;
    };
    //bool operator==(const UDFValue &a, const UDFValue &b);
}
#endif //MAYA_VALUE_H
