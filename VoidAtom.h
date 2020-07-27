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
        size_t hash(size_t range) const override { return 0; }
        void write(const std::string &logicalName) override {}
    };
} // end namespace maya

#endif //MAYA_VOIDATOM_H