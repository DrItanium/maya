//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_FLOATATOM_H
#define MAYA_FLOATATOM_H
#include "Atom.h"
#include "Constants.h"
#include "TransferEvaluable.h"
#include <memory>
#include <string>
namespace maya {
    class Float : public EphemeralAtom, public TransferEvaluable<Float> {
    public:
        using Self = Float;
        using Ptr = std::shared_ptr<Self>;
        using BackingType = double;
    public:
        Float(Environment &parent, BackingType value = 0.0) : EphemeralAtom(parent, FLOAT_TYPE), _contents(value) {}
        ~Float() override = default;
        size_t hash(size_t range) const override;
        void write(const std::string &logicalName) override;
        constexpr auto getContents() const noexcept { return _contents; }
    private:
        BackingType _contents;
    };
} // end namespace maya
std::ostream& operator<<(std::ostream& os, const maya::Float& value);
#endif //MAYA_FLOATATOM_H
