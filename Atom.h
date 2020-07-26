//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_ATOM_H
#define MAYA_ATOM_H
#include "TypeHeader.h"
#include "HoldsEnvironmentCallback.h"
#include "IORouterAware.h"
#include "Hashable.h"
#include <memory>
namespace maya {
    class Environment;
    class Atom : public HoldsEnvironmentCallback, public TypeHeader, public IORouterAware, public maya::Hashable {
    public:
        using Self = Atom;
        using Ptr = std::shared_ptr<Self>;
    public:
        Atom(Environment &parent, unsigned short type = 0) : HoldsEnvironmentCallback(parent), TypeHeader(type) {}
        ~Atom() override = default;
    };

/**
 * @brief A special type inserted to make the ephemeron type make sense
 */
    class EphemeralAtom : public Atom {
    public:
        using Self = EphemeralAtom;
        using Ptr = std::shared_ptr<Self>;
    public:
        using Atom::Atom;
    };
} // end namespace maya
#endif //MAYA_ATOM_H
