/**
 * @file
 * Common types and declarations
 * @copyright
 * maya
 * Copyright (c) 2012-2022, Joshua Scoggins
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

#ifndef MAYA_COMMON_H
#define MAYA_COMMON_H
#include <cstdint>
#include <cstddef>
#include <type_traits>
using Address = uint32_t;
/**
 * @brief A view of a 16-bit number which can be broken up into different components transparently
 */
union SplitWord16 {
    explicit constexpr SplitWord16(uint16_t value = 0) noexcept : wholeValue_(value) { }
    constexpr SplitWord16(uint8_t lower, uint8_t upper) noexcept : bytes{lower, upper} { }
    [[nodiscard]] constexpr auto getWholeValue() const noexcept { return wholeValue_; }
    [[nodiscard]] constexpr auto getLowerHalf() const noexcept { return bytes[0]; }
    [[nodiscard]] constexpr auto getUpperHalf() const noexcept { return bytes[1]; }
    uint16_t wholeValue_ = 0;
    uint8_t bytes[sizeof(uint16_t) / sizeof(uint8_t)];
};

/**
 * @brief A view of a 32-bit number which can be broken up into different components transparently
 */
union SplitWord32 {
    // adding this dropped program size by over 500 bytes!
    explicit constexpr SplitWord32(uint32_t value = 0) noexcept : wholeValue_(value) { }
    /**
     * @brief Build a SplitWord32 from two 16-bit values
     * @param lower The lower half
     * @param upper The upper half
     */
    constexpr SplitWord32(uint16_t lower, uint16_t upper) noexcept : halves{lower, upper} {}
    /**
     * @brief Build a SplitWord32 from four 8-bit values
     * @param lowest Bits 0-7
     * @param lower Bits 8-15
     * @param higher Bits 16-23
     * @param highest Bits 24-31
     */
    constexpr SplitWord32(uint8_t lowest, uint8_t lower, uint8_t higher, uint8_t highest) noexcept : bytes{lowest, lower, higher, highest} {}
    /**
     * @brief Build a SplitWord32 from two SplitWord16s
     * @param lower The lower half of the number
     * @param upper The upper half of the number
     */
    constexpr SplitWord32(const SplitWord16& lower, const SplitWord16& upper) noexcept : words_{lower, upper} { }
    /**
     * @brief Get the backing 32-bit value
     * @return The backing store 32-bit value as is
     */
    [[nodiscard]] constexpr auto getWholeValue() const noexcept { return wholeValue_; }
    /**
     * @brief View this value as a 32-bit signed number
     * @return The backing store as a 32-bit signed number
     */
    [[nodiscard]] constexpr auto getSignedRepresentation() const noexcept { return signedRepresentation_; }
    /**
     * @brief Constexpr method meant to get the target page byte (which is made up of bits 8-15)
     * @return The target page as an 8-bit value
     */
    [[nodiscard]] constexpr auto getTargetPage() const noexcept { return static_cast<uint8_t>(wholeValue_ >> 8); }
    /**
     * @brief Constexpr method meant to allow one to get the most significant byte.
     * @return The most significant byte
     */
    [[nodiscard]] constexpr auto getMostSignificantByte() const noexcept { return static_cast<uint8_t>(wholeValue_ >> 24); }
    /**
     * @brief Get the lower 16-bits as a raw 16-bit number
     * @return The lower half as a plain 16-bit number
     */
    [[nodiscard]] constexpr auto getLowerHalf() const noexcept { return halves[0]; }
    /**
     * @brief Get the upper 16-bits as a raw 16-bit number
     * @return The upper half as a plain 16-bit number
     */
    [[nodiscard]] constexpr auto getUpperHalf() const noexcept { return halves[1]; }
    /**
     * @brief Set the lower 16-bits of the backing store through the use of a SplitWord16
     * @param value The new lower half value
     */
    void setLowerHalf(SplitWord16 value) noexcept { words_[0] = value; }
    /**
     * @brief Set the upper 16-bits of the backing store through the use of a SplitWord16
     * @param value The new upper half value
     */
    void setUpperHalf(SplitWord16 value) noexcept { words_[1] = value; }
    /**
     * @brief View the lower half of this number as a SplitWord16
     * @return The lower half of this number as a SplitWord16
     */
    [[nodiscard]] constexpr auto getLowerWord() const noexcept { return words_[0]; }
    /**
     * @brief View the upper half of this number as a SplitWord16
     * @return The upper half of this number as a SplitWord16
     */
    [[nodiscard]] constexpr auto getUpperWord() const noexcept { return words_[1]; }
    uint32_t wholeValue_ = 0;
    int32_t signedRepresentation_;
    uint8_t bytes[sizeof(uint32_t)];
    uint16_t halves[sizeof(uint32_t) / sizeof(uint16_t)];
    SplitWord16 words_[sizeof(uint32_t) / sizeof(SplitWord16)];
    float floatingPointRepresentation_;
};
constexpr uint64_t pow2(uint64_t value) noexcept {
    if (value == 0) {
        return 1;
    } else {
        return pow2(value - 1) * 2;
    }
}


template<uint8_t numBits>
using ClosestBitValue_t = std::conditional_t<numBits <= 8, uint8_t,
                                        std::conditional_t<numBits <= 16, uint16_t,
                                        std::conditional_t<numBits <= 32, uint32_t, uint64_t>>>;

static_assert(std::is_same_v<ClosestBitValue_t<1>, ClosestBitValue_t<4>>);
static_assert(std::is_same_v<ClosestBitValue_t<4>, uint8_t>);
static_assert(std::is_same_v<ClosestBitValue_t<10>, uint16_t>);
static_assert(!std::is_same_v<ClosestBitValue_t<10>, ClosestBitValue_t<4>>);

static constexpr uint8_t numberOfBitsForCount(uint64_t count) noexcept {
    switch (count) {
#define X(offset) case pow2(offset): return offset
        X(1);
        X(2);
        X(3);
        X(4);
        X(5);
        X(6);
        X(7);
        X(8);
        X(9);
        X(10);
        X(11);
        X(12);
        X(13);
        X(14);
        X(15);
        X(16);
        X(17);
        X(18);
        X(19);
        X(20);
        X(21);
        X(22);
        X(23);
        X(24);
        X(25);
        X(26);
        X(27);
        X(28);
        X(29);
        X(30);
        X(31);
        X(32);
        X(33);
        X(34);
        X(35);
        X(36);
        X(37);
        X(38);
        X(39);
        X(40);
        X(41);
        X(42);
        X(43);
        X(44);
        X(45);
        X(46);
        X(47);
        X(48);
        X(49);
        X(50);
        X(51);
        X(52);
        X(53);
        X(54);
        X(55);
        X(56);
        X(57);
        X(58);
        X(59);
        X(60);
        X(61);
        X(62);
        X(63);
#undef X
        default: return 0;
    }
}
static constexpr uint8_t getNumberOfBitsForNumberOfEntries(uint64_t count) noexcept { return numberOfBitsForCount(count); }

#endif //MAYA_COMMON_H