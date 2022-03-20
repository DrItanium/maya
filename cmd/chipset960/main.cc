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
#include <list>
#include <type_traits>
#include <chrono>
#include "interface/spi.h"
#include "interface/gpio.h"
#include "ram.h"
#include "ChipsetInterface.h"
#include "SinglePoolCache.h"
#include "DirectMappedCacheWay.h"
#include "CacheEntry.h"
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
CacheInstance_t<DirectMappedCacheWay,
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
        while (true) {
            theChipset.waitForCycleUnlock();
            // load the data into the expert system to be processed later on when finished
            theChipset.call("perform-write",
                            &returnNothing,
                            baseAddress,
                            theChipset.getDataLines(),
                            [style = theChipset.getStyle()](auto *builder) { loadStoreStyle(builder, style); });
            if (theChipset.signalCPU()) {
                break;
            }
            baseAddress += 2;
        }
        // have the expert system handle processing
        theChipset.run(-1L);
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
            // now do a run on it
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
int main(int argc, char *argv[]) {
#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
    signal(SIGINT,CatchCtrlC);
#endif
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
    theChipset.setupDataLines();
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
    std::array<uint16_t, 8> readStorage;
    while (true) {
        theChipset.waitForTransactionStart();
        constexpr uint32_t addressMask = ~0b1111;
        if (auto baseAddress = theChipset.getAddress(); theChipset.isReadOperation()) {
            theChipset.setupDataLinesForRead();
            // mask the address to be the current 16-byte chunk and then load all 16-bytes
            if (auto maskedAddress = baseAddress & (addressMask); theChipset.call<bool>("span-is-cacheable", maskedAddress)) {
                // if the span is cacheable then load a 16-byte span ahead of time, hold onto this cache for the lifetime of the
                // current transaction only. Implementing a data cache later on may make more sense too
                //auto startTime = std::chrono::system_clock::now();
                for (int i = 0;i < 8; ++i, maskedAddress += 2) {
                    readStorage[i] = theChipset.call<uint16_t>("perform-read", maskedAddress);
                }
                //auto endTime = std::chrono::system_clock::now();
                //std::cout << "\tTotal Burst Read Time: " << std::chrono::duration_cast<std::chrono::microseconds>(endTime - startTime).count() << " microseconds" << std::endl;
                uint32_t spanOffset = (baseAddress & 0b1111) >> 1;
                for (auto i = spanOffset; i < 8; ++i) {
                    theChipset.waitForCycleUnlock();
                    theChipset.setDataLines(readStorage[i]);
                    if (theChipset.signalCPU()) {
                        break;
                    }
                }
            } else {
                while (true) {
                    theChipset.waitForCycleUnlock();
                    {
                        // just in case the compiler is getting cute
                        theChipset.setDataLines(theChipset.call<uint16_t>("perform-read", baseAddress));
                    }
                    if (theChipset.signalCPU()) {
                        break;
                    }
                    baseAddress += 2;
                }
            }
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
