//
// Created by jwscoggins on 8/10/20.
//

#ifndef MAYA_SAVECALLFUNCTIONITEM_H
#define MAYA_SAVECALLFUNCTIONITEM_H
#include <string>
#include <functional>
#include <memory>
#include "HoldsEnvironmentCallback.h"
namespace maya {
    class Defmodule;
    class SaveCallFunctionItem {
    public:
        using Self = SaveCallFunctionItem;
        using Ptr = std::shared_ptr<Self>;
    public:
        SaveCallFunctionItem(Environment& parent, const std::string& name, int priority);
        virtual ~SaveCallFunctionItem() = default;
        [[nodiscard]] std::string getName() const noexcept { return _name; }
        [[nodiscard]] constexpr auto getPriority() const noexcept { return _priority; }
        virtual void invoke(std::shared_ptr<Defmodule>, const std::string&) = 0;
    private:
        std::string _name;
        int _priority;
    };
} // end namespace maya

#endif //MAYA_SAVECALLFUNCTIONITEM_H
