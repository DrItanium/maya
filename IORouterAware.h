//
// Created by jwscoggins on 7/14/20.
//

#ifndef MAYA_IOROUTERAWARE_H
#define MAYA_IOROUTERAWARE_H
#include <memory>
#include <string>
namespace maya {
    using EnvironmentPtr = std::shared_ptr<struct Environment>;

    class IORouterAware {
    public:
        virtual ~IORouterAware() = default;
        /**
         * @brief Emit the contents of this object to a router
         * @param theEnv the environment pointer that this object is a part of
         * @param logicalName the logical name to write to
         */
        virtual void write(const EnvironmentPtr &theEnv, const std::string &logicalName) = 0;
    };
} // end namespace maya
#endif //MAYA_IOROUTERAWARE_H
