//
// Created by jwscoggins on 7/13/20.
//

#ifndef MAYA_HASHABLE_H
#define MAYA_HASHABLE_H
#include <cstddef>
#include <memory>
namespace maya {
    class Hashable {
    public:
        using Self = Hashable;
        using Ptr = std::shared_ptr<Self>;
    public:
        virtual ~Hashable() = default;
        virtual size_t hash(size_t range) const = 0;
    };
} // end namespace maya

#endif //MAYA_HASHABLE_H
