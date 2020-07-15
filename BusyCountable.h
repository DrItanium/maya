//
// Created by jwscoggins on 7/15/20.
//

#ifndef MAYA_BUSYCOUNTABLE_H
#define MAYA_BUSYCOUNTABLE_H
#include "ReferenceCounted.h"
namespace maya {
    class BusyCountable : public ReferenceCountable {
    public:
        virtual ~BusyCountable() = default;
        constexpr auto getBusyCount() const noexcept { return _busyCount; }
        void retain() override;
        void release() override;
        bool canRelease() const noexcept override;
    private:
        unsigned int _busyCount = 0;
    } ;
}

#endif //MAYA_BUSYCOUNTABLE_H
