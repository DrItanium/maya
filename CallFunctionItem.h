//
// Created by jwscoggins on 7/19/20.
//

#ifndef MAYA_CALLFUNCTIONITEM_H
#define MAYA_CALLFUNCTIONITEM_H
#include <functional>
#include <memory>
#include <string>

#include "Environment.h"
#include "HoldsEnvironmentCallback.h"

namespace maya {
    template<typename ReturnType, typename ... Args>
    class CallFunctionItem : public HoldsEnvironmentCallback {
    public:
        using Self = CallFunctionItem;
        using Ptr = std::shared_ptr<Self>;
        using BodyFunction = std::function<ReturnType(Environment&, Args&& ...)>;
    public:
        CallFunctionItem(Environment& _environment, const std::string& name, int priority, BodyFunction executableBody) : HoldsEnvironmentCallback(_environment), _name(name), _priority(priority), _body(executableBody) { }
        [[nodiscard]] std::string getName() const noexcept { return _name; }
        [[nodiscard]] constexpr auto getPriority() const noexcept { return _priority; }
        [[nodiscard]] ReturnType operator()(Args&& ... args) {
            return invoke(std::forward<Args>(args)...);
        }
        [[nodiscard]] ReturnType invoke(Args&& ... args) {
            return _body(_parent, std::forward<Args>(args)...);
        }
    private:
        std::string _name;
        int _priority = 0;
        BodyFunction _body;
    };
    using VoidCallFunctionItem = CallFunctionItem<void>;
    using BoolCallFunctionItem = CallFunctionItem<bool>;
    template<typename T>
    using VoidCallFunctionItemWithOneArg = CallFunctionItem<void, T>;
    template<typename T>
    using BoolCallFunctionItemWithOneArg = CallFunctionItem<bool, T>;
} // end namespace maya
#endif //MAYA_CALLFUNCTIONITEM_H
