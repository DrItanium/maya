/**
 * @file
 * Eight way rand Pseudo LRU Cache Way implementation
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

#ifndef SXCHIPSET_EIGHTWAYRANDPLRUENTRY_H
#define SXCHIPSET_EIGHTWAYRANDPLRUENTRY_H
#include "Common.h"
#include "TaggedCacheAddress.h"
#include "CacheEntry.h"
#include <random>

/**
 * @brief Divides a series of 8 cache lines in the set into several subsets where the replacement algorithm uses a combination of random replacement and tree plru
 * @tparam numTagBits The number of bits that make up a tag overall
 * @tparam totalBitCount  The total number of bits that make up an address partially serviced by this cache set
 * @tparam numLowestBits The number of bytes that each cache line will store
 * @tparam T The backing storage type or where we read from and write to on a cache miss
 */
template<uint8_t numTagBits, uint8_t totalBitCount, uint8_t numLowestBits, typename T, bool debugMode = false>
class EightWayRandPLRUCacheSet {
public:
    static constexpr auto NumberOfWays = 8;
    static constexpr auto WayMask = NumberOfWays - 1;
    using CacheEntry = ::CacheEntry<numTagBits, totalBitCount, numLowestBits, T, debugMode>;
    using TaggedAddress = typename CacheEntry::TaggedAddress;
    static constexpr auto NumBytesCached = CacheEntry::NumBytesCached;
public:
    //[[gnu::noinline]]
    CacheEntry& getLine(const TaggedAddress& theAddress) noexcept {
        uint8_t targetIndex = 0xFF;
        for (uint8_t i = 0; i < NumberOfWays; ++i) {
            if (ways_[i].matches(theAddress)) {
                updateFlags(i);
                return ways_[i];
            } else if ((targetIndex >= NumberOfWays) && !ways_[i].isValid()) {
                targetIndex = i;
            }
        }

        auto index = (targetIndex < NumberOfWays) ? targetIndex : getLeastRecentlyUsed();
        updateFlags(index);
        ways_[index].reset(theAddress);
        return ways_[index];
    }
    void clear() noexcept {
        for (auto& way : ways_) {
            way.clear();
        }
        bits_ = 0;
    }
    [[nodiscard]] constexpr auto getWay(size_t index = 0) const noexcept { return ways_[index & WayMask]; }
    void setWay(CacheEntry& way, size_t index = 0) noexcept { ways_[index & WayMask] = &way; }
    [[nodiscard]] constexpr size_t size() const noexcept { return NumberOfWays; }
private:
    void updateFlags(uint8_t index) noexcept {
        constexpr uint8_t masks[8] {
                0b1110,
                0b0001,
                0b1101,
                0b0010,
                0b1011,
                0b0100,
                0b0111,
                0b1000,
        };
        // Take the index provided and see if the least significant bit is zero or not
        // if it is zero then and the tracking bits with the value stored in the masks table
        // if we just used 0, then we do bits_ = bits_ & (0b1110) which will clear the least significant bit
        // if the index is 1 then we do bits_ = bits | (0b0001) which will set the least significant bit
        // when 2 => bits_ &= 0b1101 -> which will clear the next least significant bit
        // and so on.
        if (auto rIndex = index & 0b111; (rIndex & 0b1) == 0) {
            bits_ &= masks[rIndex];
        } else {
            bits_ |= masks[rIndex];
        }
    }
    static constexpr auto NumberOfGroups = 4;
    [[nodiscard]] uint8_t getLeastRecentlyUsed() const noexcept {
        static bool initialized = false;
        static uint8_t counter = 0;
        static uint8_t randomTable[256] = { 0 };
        static std::random_device theDevice;
        static std::uniform_int_distribution<uint8_t> distribution(0, NumberOfGroups);
        static constexpr uint8_t secondLookupTable[4][2] {
                { 1, 0 },
                {3, 2},
                {5, 4},
                {7, 6},
        };
        static constexpr uint8_t maskLookup[4] {
            0b0001,
            0b0010,
            0b0100,
            0b1000,
        };
        if (!initialized) {
            initialized = true;
            counter = 0;
            for (uint16_t i = 0; i < 256; ++i) {
                randomTable[i] = distribution(theDevice);
            }
        }
        auto theIndex = randomTable[counter++];
        return secondLookupTable[theIndex][(bits_ & maskLookup[theIndex]) ? 1 : 0];
    }
private:
    // This is RandPLRU Tree so we need to organize things correctly, I'm going to try four groups of two
    CacheEntry ways_[NumberOfWays];
    uint8_t bits_ = 0;
};

#endif //SXCHIPSET_EIGHTWAYPSEUDOLRUENTRY_H
