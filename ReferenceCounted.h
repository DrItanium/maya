//
// Created by jwscoggins on 7/11/20.
//

#ifndef MAYA_REFERENCECOUNTED_H
#define MAYA_REFERENCECOUNTED_H

#include <memory>
#include <variant>

class ReferenceCounted {
public:
    constexpr auto getCount() const noexcept { return _count; }
    void retain();
    void release();
    constexpr auto isPermanent() const noexcept { return _permanent; }
    void setIsPermanent(bool value = true) noexcept { _permanent = value; }
    constexpr auto isMarkedEphemeral() const noexcept { return _markedEphemeral; }
    void markEphemeral(bool value = true) noexcept { _markedEphemeral = value; }
    constexpr auto isNeededContents() const noexcept { return _neededContents; }
    void markContentsNeeded(bool value = true) noexcept {_neededContents = value; }
    constexpr auto getBucket() const noexcept { return _bucket; }
    void setBucket(unsigned int value) noexcept { _bucket = value; }
private:
    long _count = 0;
    bool _permanent: 1;
    bool _markedEphemeral: 1;
    bool _neededContents : 1;
    unsigned int _bucket: 29;
};

#endif //MAYA_REFERENCECOUNTED_H
