//
// Created by jwscoggins on 7/21/20.
//

#ifndef MAYA_HASBINARYSAVEID_H
#define MAYA_HASBINARYSAVEID_H

namespace maya {
    /**
     * @brief A mixin class which provides a binary save id
     */
    class HasBinarySaveID {
    public:
        virtual ~HasBinarySaveID() = default;
        constexpr auto getBinarySaveID() const noexcept { return _bsaveID; }
        void setBinarySaveID(unsigned long value) noexcept { _bsaveID = value; }
    private:
        unsigned long _bsaveID = 0;
    };
}

#endif //MAYA_HASBINARYSAVEID_H
