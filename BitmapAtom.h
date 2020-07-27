//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_BITMAPATOM_H
#define MAYA_BITMAPATOM_H
#include "Atom.h"
#include "Evaluable.h"
#include <memory>
#include <string>
#include <typeinfo>
namespace maya {
/**
     * @brief A generic entity which holds onto a binary view of some kind of bits. At this level, the act of preventing duplicates
     * is provided through getByte and numBytes.
     */
    class BitMap : public Atom, public Evaluable {
    public:
        using Self = BitMap;
        using Ptr = std::shared_ptr<Self>;
    public:
        BitMap(Environment &parent, unsigned short type) : Atom(parent, type) {}
        ~BitMap() override = default;
        [[nodiscard]] bool contentsEqual(Ptr other) const;
        [[nodiscard]] size_t getTypeHashCode() const noexcept { return typeid(*this).hash_code(); }
    protected:
        /**
         * @brief Compares the internals of a given BitMap when it is already known that the other is of the same polymorphic type as this instance
         * @param other the other BitMap that has already been polymorphically verified to be of the right type
         * @return boolean signifying if the internals are equal to one another provided that they are of the same type
         */
        [[nodiscard]] virtual bool compareInternals(Ptr other) const = 0;
    };
}

#endif //MAYA_BITMAPATOM_H
