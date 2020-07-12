//
// Created by jwscoggins on 7/11/20.
//

#ifndef MAYA_REFERENCECOUNTED_H
#define MAYA_REFERENCECOUNTED_H

class ReferenceCounted {
public:
    constexpr auto getCount() const noexcept { return _count; }
    void retain();
    void release();
private:
    long _count = 0;
};

#endif //MAYA_REFERENCECOUNTED_H
