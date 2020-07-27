//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_MULTIFIELDPRIMITIVE_H
#define MAYA_MULTIFIELDPRIMITIVE_H
#include "Value.h"
#include "HoldsEnvironmentCallback.h"
#include "TypeHeader.h"
#include "Evaluable.h"
#include "Constants.h"
#include <memory>
#include <vector>
namespace maya {
    struct Multifield : public HoldsEnvironmentCallback, public TypeHeader, public Evaluable {
    public:
        using Self = Multifield;
        using Ptr = std::shared_ptr<Self>;
    public:
        Multifield(Environment &parent) : HoldsEnvironmentCallback(parent), TypeHeader(MULTIFIELD_TYPE) {}
        ~Multifield() override = default;
        bool evaluate(UDFValue::Ptr retVal) override;
    public:
        auto length() const noexcept { return contents.size(); }
        std::vector<Value::Ptr> contents;
    };
} // end namespace maya
#endif //MAYA_MULTIFIELDPRIMITIVE_H
