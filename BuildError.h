//
// Created by jwscoggins on 8/8/20.
//

#ifndef MAYA_BUILDERROR_H
#define MAYA_BUILDERROR_H
namespace maya {
    enum class BuildError {
        None,
        CouldNotBuild,
        ConstructNotFound,
        Parsing,
    };
} // end namespace maya
#endif //MAYA_BUILDERROR_H
