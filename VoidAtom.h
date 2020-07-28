//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_VOIDATOM_H
#define MAYA_VOIDATOM_H
#include "Atom.h"
#include <memory>
namespace maya {
class Void final : public Atom {
    public:
        using Self = Void;
        using Ptr = std::shared_ptr<Self>;
    public:
        Void(maya::Environment &parent);
        ~Void() override = default;
        size_t hash(size_t range) const override;
        void write(const std::string &logicalName) override;
        bool evaluate(std::shared_ptr<struct UDFValue> retVal) override;
    };
} // end namespace maya

#endif //MAYA_VOIDATOM_H
