/**
 * @file
 * Code to make it easy for a given device to act as chipset for an i960
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
#include "platform/os.h"
extern "C" {
#include "clips/clips.h"
}
#include <iostream>
#include <array>
#include <type_traits>
#include <chrono>
#include "interface/spi.h"
#include "interface/gpio.h"
#include "ram.h"
#include "ChipsetInterface.h"
#include "SinglePoolCache.h"
#include "DirectMappedCacheWay.h"
#include "EightWayRandPLRUEntry.h"
extern "C" {
#include <signal.h>
}

void
loadStoreStyle(Electron::FunctionBuilder* builder, i960::LoadStoreStyle theStyle) noexcept {
    // give the microcode an abstract representation of the load store style by making them symbols
    switch (theStyle) {
        case i960::LoadStoreStyle::Full16:
            builder->add(Electron::FunctionBuilder::symbol("full16"));
            break;
        case i960::LoadStoreStyle::Lower8:
            builder->add(Electron::FunctionBuilder::symbol("lower8"));
            break;
        case i960::LoadStoreStyle::Upper8:
            builder->add(Electron::FunctionBuilder::symbol("upper8"));
            break;
        default:
            /// let the microcode do error handling as it is increases flexibility
            builder->add(Electron::FunctionBuilder::symbol("none"));
            break;
    }
}
void
loadStoreStyle(Electron::FunctionBuilder* builder) noexcept {
    loadStoreStyle(builder, i960::ChipsetInterface::get().getStyle());
}
/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
static void                    CatchCtrlC(int);
#endif
CacheInstance_t<EightWayRandPLRUCacheSet,
        16 * 1024 * 1024,
        32,
        6,
        i960::ChipsetInterface,
        false> theCache;
/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/
template<uint32_t addressMask>
void
doWriteOperation(i960::ChipsetInterface& theChipset, uint32_t baseAddress) noexcept {
    static Electron::Value returnNothing;
    theChipset.setupDataLinesForWrite();
    // write operation
    if (auto maskedAddress = baseAddress & (addressMask); theChipset.call<bool>("span-is-cacheable", maskedAddress)) {
        using TaggedAddress = decltype(theCache)::TaggedAddress;
        TaggedAddress addr(baseAddress);
        auto& theLine = theCache.getLine(addr);
        auto start = addr.getOffset() >> 1;
        auto end = start + 8;
        for (auto i = start; i < end; ++i) {
            theChipset.waitForCycleUnlock();
            theLine.set(i,
                        theChipset.getStyle(),
                        SplitWord16{theChipset.getDataLines()});
            if (theChipset.signalCPU()) {
                break;
            }
            baseAddress += 2;
        }
        // have the expert system handle processing
    } else {
        while (true) {
            theChipset.waitForCycleUnlock();
            //auto startTime = std::chrono::system_clock::now();
            theChipset.call("perform-write",
                            &returnNothing,
                            baseAddress,
                            theChipset.getDataLines(),
                            [](auto *builder) { loadStoreStyle(builder); });
            // okay we've made the transaction
            // now do a run on it, if we were really cool then it would make even more sense to introduce a time stamp to make sure
            // that we maintain ordering. It would also allow us to delay committing the operation until after we are done with the transaction
            // in question.

            // The other way to solve this would be to have the microcode return the function to invoke but for now, this is just fine.
            theChipset.run(-1);
            //auto endTime = std::chrono::system_clock::now();
            //std::cout << "\tLinear Write Time: " << std::chrono::duration_cast<std::chrono::microseconds>(endTime - startTime).count() << " microseconds" << std::endl;
            if (theChipset.signalCPU()) {
                break;
            }
            baseAddress += 2;
        }
    }
}
template<uint32_t addressMask>
void
doReadOperation(i960::ChipsetInterface& theChipset, uint32_t baseAddress) noexcept {
    theChipset.setupDataLinesForRead();
    // mask the address to be the current 16-byte chunk and then load all 16-bytes
    if (auto maskedAddress = baseAddress & (addressMask); theChipset.call<bool>("span-is-cacheable", maskedAddress)) {
        using TaggedAddress = decltype(theCache)::TaggedAddress;
        TaggedAddress addr(baseAddress);
        auto& theLine = theCache.getLine(addr);
        // if the span is cacheable then load a 16-byte span ahead of time, hold onto this cache for the lifetime of the
        // current transaction only. Implementing a data cache later on may make more sense too
        //auto startTime = std::chrono::system_clock::now();
        //auto endTime = std::chrono::system_clock::now();
        //std::cout << "\tTotal Burst Read Time: " << std::chrono::duration_cast<std::chrono::microseconds>(endTime - startTime).count() << " microseconds" << std::endl;
        auto start = addr.getOffset() >> 1;
        auto end = start + 8;
        uint16_t storage[8];
        for (auto i = 0u, j = start; i < 8; ++i, ++j) {
            storage[i] = theLine.get(j);
        }
        for (auto a : storage) {
            theChipset.waitForCycleUnlock();
            theChipset.setDataLines(a);
            if (theChipset.signalCPU()) {
                break;
            }
        }
    } else {
        // we can actually go ahead and pull the data ahead of the cycle unlock on a read
        while (true) {
            theChipset.waitForCycleUnlock();
            // we want to make sure the management says it is waiting but we can compute the expensive part ahead of time or at least get started on it
            theChipset.setDataLines(theChipset.call<uint16_t>("perform-read", baseAddress));
            if (theChipset.signalCPU()) {
                break;
            }
            baseAddress += 2;
        }
    }
}
int main(int argc, char *argv[]) {
#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
    signal(SIGINT,CatchCtrlC);
#endif
    theCache.begin();
    try {
        Neutron::GPIO::begin();
        if (!Neutron::SPI::begin(0, 10 * 1000 * 1000)) {
            i960::shutdown("Could not open SPI Bus!");
        }
    } catch (std::system_error& err) {
        i960::shutdown(err.what());
        return -1;
    }
    auto& theChipset = i960::ChipsetInterface::get();
    theChipset.setupPins();
    theChipset.putManagementEngineInReset();
#ifdef V1Layout
    theChipset.setupDataLines();
#endif
    std::cout << "Keeping the i960 In Reset but the Management Engine active" << std::endl;
    theChipset.pullManagementEngineOutOfReset();
    /// @todo insert calls to add extended clips functionality here
    i960::installRAMExtensions(theChipset);
    theChipset.loadMicrocode();
    theChipset.reset(); // (reset)
    theChipset.call("ucode-init"); // setup the ucode before we pull the i960 out of reset
    // we are not actually going to do (run) at this point, it is up to the native code here to call into the ucode
    std::cout << "Pulling the i960 out of reset!" << std::endl;
    theChipset.pull960OutOfReset();
    theChipset.waitForBootSignal();
    std::cout << "Successfully booted the i960!" << std::endl;
    // This is the state machine for handling the i960 that has a management engine attached to it!
    // With the microcontroller based designs, there is a lot of extra code surrounding having onboard caches to accelerate
    // performance. Hopefully, the raspberry pi offsets any potential bottlenecks by being exponentially faster than those designs.
    while (true) {
        theChipset.waitForTransactionStart();
        constexpr uint32_t addressMask = ~0b1111;
        if (auto baseAddress = theChipset.getAddress(); theChipset.isReadOperation()) {
            doReadOperation<addressMask>(theChipset, baseAddress);
        } else {
            doWriteOperation<addressMask>(theChipset, baseAddress);
        }
    }

    theChipset.shutdown("Strange Termination!");
    return -1;
    // configure the pins of the raspberry pi
}



#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC || DARWIN
/***************/
/* CatchCtrlC: */
/***************/
void CatchCtrlC(int sgnl)
{
    i960::shutdown("CTRL+C Given");
}
#endif
