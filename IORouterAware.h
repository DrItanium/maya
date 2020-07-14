//
// Created by jwscoggins on 7/14/20.
//

#ifndef MAYA_IOROUTERAWARE_H
#define MAYA_IOROUTERAWARE_H
#include <memory>
#include <string>
using EnvironmentPtr = std::shared_ptr<struct Environment>;
class IORouterAware {
public:
    virtual ~IORouterAware() = default;
    virtual void write(const EnvironmentPtr& theEnv, const std::string& logicalName) = 0;
};

#endif //MAYA_IOROUTERAWARE_H
