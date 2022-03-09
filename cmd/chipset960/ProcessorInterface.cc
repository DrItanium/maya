/**
 * @file
 * Implementation of processor interface routines
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
//
// Created by jwscoggins on 3/8/22.
//
#include <iostream>
#include "ChipsetInterface.h"

namespace i960 {

    template<typename ... T>
    void
    configurePinBlock(T&& ... pins) noexcept {
        (pins.configure(), ...);
    }
    void
    digitalWrite(Pinout pin, PinValue value) {
        Neutron::GPIO::digitalWrite(static_cast<int>(pin), value) ;

    }
    PinValue
    digitalRead(Pinout pin) {
        return Neutron::GPIO::digitalRead(static_cast<int>(pin));
    }
    void
    pinMode(Pinout pin, PinDirection direction) noexcept {
        Neutron::GPIO::pinMode(static_cast<int>(pin), direction);
    }

    bool
    ChipsetInterface::isReadOperation() noexcept {
        return WR.isAsserted();
    }

    bool
    ChipsetInterface::isWriteOperation() noexcept {
        return WR.isDeasserted();
    }

    LoadStoreStyle
    ChipsetInterface::getStyle() noexcept {
        if (BE0.isAsserted()) {
            if (BE1.isAsserted())  {
                return LoadStoreStyle::Full16;
            } else {
                return LoadStoreStyle::Lower8;
            }
        } else {
            if (BE1.isAsserted()) {
                return LoadStoreStyle::Upper8;
            } else {
                return LoadStoreStyle::None;
            }
        }
    }
    uint32_t
    ChipsetInterface::getAddress() noexcept {
        /// @todo implement
        return 0;
    }
    void
    ChipsetInterface::performReadTransaction() noexcept {

    }
    void
    ChipsetInterface::performWriteTransaction() noexcept {

    }
    void
    ChipsetInterface::newDataCycle() noexcept {
        auto address = getAddress();
        if (isReadOperation()) {
            performReadTransaction();
        } else {
            performWriteTransaction();
        }
    }
    void
    ChipsetInterface::waitForTransactionStart() noexcept {
        while (InTransaction.isDeasserted());
    }
    void
    ChipsetInterface::setupDataLinesForRead() noexcept {
        std::cout << "Setting up Data Lines" << std::endl;
        /// @todo implement
    }
    void
    ChipsetInterface::systemSetup() noexcept {
        std::cout << "Performing System Setup" << std::endl;
        /// @todo implement
    }
    void
    ChipsetInterface::waitForBootSignal() noexcept {
        while (BootSuccessful.digitalRead() == PinValue::Low);
        /// @todo add interrupt to BootSuccessful pin
        Neutron::GPIO::attachInterrupt(static_cast<int>(Pinout::BootSuccessful),
                                       Neutron::GPIO::InterruptMode::Falling,
                                       []() { i960::shutdown("CHECKSUM FAILURE"); });
    }
    void
    ChipsetInterface::shutdown(const std::string& str) noexcept {
        SetHaltExecution(getRawEnvironment(),true);
        CloseAllBatchSources(getRawEnvironment());
        ManagementEngineReset.assertPin();
        WaitBoot960.assertPin();
        ManagementEngineReset.deassertPin();
        // exit at this point
        exit(1);
    }
    ChipsetInterface::ChipsetInterface() : ChipsetInterface::Parent() {
        // setup the extra extensions as needed
        installProcessorExtensions();
    }
    void
    ChipsetInterface::setupRam() noexcept {
        ram_ = std::make_unique<MemoryCell[]>(NumberOfCells);
        for (uint32_t i = 0; i < NumberOfCells; ++i) {
            ram_[i].word = 0;
        }
    }
    void
    ChipsetInterface::setupPins() noexcept {
        i960::configurePinBlock(Ready,
                                BootSuccessful,
                                WR,
                                BE0,
                                BE1,
                                InTransaction,
                                DoCycle,
                                Blast,
                                ManagementEngineReset,
                                WaitBoot960,
                                IOEXP_INT0,
                                IOEXP_INT1,
                                IOEXP_INT2,
                                IOEXP_INT3,
                                IOEXP_INT4,
                                IOEXP_INT5,
                                IOEXP_INT6,
                                IOEXP_INT7);
        std::cout << "Pulling Management Engine into Reset and starting configuration" << std::endl;
    }
    void
    shutdown(const std::string& msg) noexcept {
        ChipsetInterface::get().shutdown(msg);
    }
    void
    ChipsetInterface::begin() {
        setupPins();
        ManagementEngineReset.assertPin();
        WaitBoot960.assertPin();
        setupRam();
        installProcessorExtensions();
        /// @todo introduce a delay?
        ManagementEngineReset.deassertPin();
        std::cout << "Keeping the i960 In Reset but the Management Engine active" << std::endl;
        systemSetup();
        setupDataLinesForRead();
        WaitBoot960.deassertPin();
        waitForBootSignal();
        std::cout << "i960 Successfully Booted!" << std::endl;
    }
    void
    ChipsetInterface::invoke() {
        while (true) {
            waitForTransactionStart();
            newDataCycle();
        }
    }
}