//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_INTEGERATOM_H
#define MAYA_INTEGERATOM_H
#include "Atom.h"
#include "Constants.h"
#include "TransferEvaluable.h"
#include <memory>
#include <string>
namespace maya {
    class Integer : public EphemeralAtom, public TransferEvaluable<Integer> {
    public:
        using Self = Integer;
        using Ptr = std::shared_ptr<Self>;
        using BackingType = long long;
    public:
        Integer(Environment &parent, BackingType value = 0) : EphemeralAtom(parent, INTEGER_TYPE), _contents(value) {}
        ~Integer() override = default;
        size_t hash(size_t range) const override;
        void write(const std::string &logicalName) override;
        constexpr auto getContents() const noexcept { return _contents; }
    private:
        BackingType _contents;
    };
    std::ostream& operator<<(std::ostream& os, const maya::Integer& value);
} // end namespace maya
#endif //MAYA_INTEGERATOM_H
