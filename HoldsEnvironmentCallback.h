//
// Created by jwscoggins on 7/13/20.
//

#ifndef MAYA_HOLDSENVIRONMENTCALLBACK_H
#define MAYA_HOLDSENVIRONMENTCALLBACK_H
#include <memory>
namespace maya {
    struct Environment;

    class HoldsEnvironmentCallback {
    public:
        HoldsEnvironmentCallback(Environment &parent);
        Environment &getParent() noexcept { return _parent; }
        const Environment &getParent() const noexcept { return _parent; }
    protected:
        Environment &_parent;
    };
}
#endif //MAYA_HOLDSENVIRONMENTCALLBACK_H
