//
// Created by jwscoggins on 7/13/20.
//

#ifndef MAYA_HASHABLE_H
#define MAYA_HASHABLE_H
#include <cstddef>
namespace maya {
    class Hashable {
    public:
        virtual size_t hash(size_t) = 0;
    };
} // end namespace maya

#endif //MAYA_HASHABLE_H
