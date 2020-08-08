//
// Created by jwscoggins on 7/13/20.
//

#ifndef MAYA_HOLDSENVIRONMENTCALLBACK_H
#define MAYA_HOLDSENVIRONMENTCALLBACK_H
#include <memory>
#include <functional>
namespace maya {
    struct Environment;

    class HoldsEnvironmentCallback {
    public:
        HoldsEnvironmentCallback(Environment &parent);
        Environment &getParent() noexcept { return _parent.get(); }
        const Environment &getParent() const noexcept { return _parent.get(); }
    private:
        std::reference_wrapper<Environment> _parent;
    };
}
#endif //MAYA_HOLDSENVIRONMENTCALLBACK_H
