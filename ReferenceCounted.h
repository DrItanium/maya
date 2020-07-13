//
// Created by jwscoggins on 7/11/20.
//

#ifndef MAYA_REFERENCECOUNTED_H
#define MAYA_REFERENCECOUNTED_H

#include <memory>
#include <variant>
class ReferenceCountable {
public:
    virtual ~ReferenceCountable() = default;
    virtual void retain() = 0;
    virtual void release() = 0;
    virtual bool canRelease() const noexcept = 0;
};
class ReferenceCounted : public ReferenceCountable {
public:
    ~ReferenceCounted() override = default;
    constexpr auto getCount() const noexcept { return _count; }
    void retain() override;
    void release() override;
    bool canRelease() const noexcept override;
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
