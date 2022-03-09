/**
 * @file
 * Abstraction for chipset interface routines
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

#ifndef MAYA_CHIPSETINTERFACE_H
#define MAYA_CHIPSETINTERFACE_H
#include "platform/os.h"
#include "electron/Environment.h"
#include "interface/spi.h"
#include "interface/gpio.h"
namespace i960 {
    enum class Pinout {
        BootSuccessful = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<7>,
        Ready = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<29>,
        WR = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<31>,
        BE0 = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<32>,
        BE1 = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<33>,
        InTransaction = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<8>,
        DoCycle = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<10>,
        Blast = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<36>,
        MeReset = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<11>,
        WaitBoot960 = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<12>,
        IoExpander_Int0 = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<35>,
        IoExpander_Int1 = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<38>,
        IoExpander_Int2 = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<40>,
        IoExpander_Int3 = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<15>,
        IoExpander_Int4 = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<16>,
        IoExpander_Int5 = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<18>,
        IoExpander_Int6 = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<22>,
        IoExpander_Int7 = Neutron::GPIO::RaspberryPi::PhysicalToBCMTranslation_v<37>,
    };
    using PinDirection = Neutron::GPIO::PinMode;
    using PinValue = Neutron::GPIO::PinValue;
    void digitalWrite(Pinout pin, PinValue value);
    PinValue digitalRead(Pinout pin);
    void pinMode(Pinout pin, PinDirection direction) noexcept;
    struct PinConfiguration {
        constexpr PinConfiguration(Pinout pin, PinDirection direction, PinValue asserted = PinValue::Low, PinValue deasserted = PinValue::High) noexcept : pin_(pin), direction_(direction), asserted_(asserted), deasserted_(deasserted) {}
        [[nodiscard]] constexpr auto getPinValue() const noexcept { return static_cast<std::underlying_type_t<Pinout>>(pin_); }
        [[nodiscard]] constexpr auto getPin() const noexcept { return pin_; }
        [[nodiscard]] constexpr auto getDirection() const noexcept { return direction_; }
        [[nodiscard]] constexpr auto getAssertedState() const noexcept { return asserted_; }
        [[nodiscard]] constexpr auto getDeassertedState() const noexcept { return deasserted_; }

        [[nodiscard]] constexpr auto isOutputPin() const noexcept { return getDirection() == PinDirection::Output; }
        [[nodiscard]] constexpr auto isInputPin() const noexcept { return getDirection() == PinDirection::Input; }
        [[nodiscard]] constexpr auto isInputPullupPin() const noexcept { return getDirection() == PinDirection::InputPullup; }
        inline void digitalWrite(PinValue value) const { i960::digitalWrite(getPin(), value); }
        [[nodiscard]] inline PinValue digitalRead() const { return i960::digitalRead(getPin()); }
        [[nodiscard]] inline bool isAsserted() const noexcept { return digitalRead() == getAssertedState(); }
        [[nodiscard]] inline bool isDeasserted() const noexcept { return digitalRead() == getDeassertedState(); }
        inline void deassertPin() const { digitalWrite(getDeassertedState()); }
        inline void assertPin() const { digitalWrite(getAssertedState()); }
        /**
         * @brief Set the pin's mode and even deassert it if it is an output
         */
        void configure() const {
            i960::pinMode(getPin(), getDirection());
            if (isOutputPin()) {
                // make sure we deassert the pin if it is an output
                deassertPin();
            }
        }
    private:
        Pinout pin_;
        PinDirection direction_;
        PinValue asserted_, deasserted_;
    };
    constexpr PinConfiguration BootSuccessful {Pinout::BootSuccessful, PinDirection::Input, PinValue::High, PinValue::Low};
    constexpr PinConfiguration Ready {Pinout::Ready, PinDirection::Output };
    constexpr PinConfiguration WR {Pinout::WR, PinDirection::Input};
    constexpr PinConfiguration BE0 {Pinout::BE0, PinDirection::Input};
    constexpr PinConfiguration BE1 {Pinout::BE1, PinDirection::Input};
    constexpr PinConfiguration InTransaction {Pinout::InTransaction, PinDirection::Input};
    constexpr PinConfiguration DoCycle {Pinout::InTransaction, PinDirection::Input};
    constexpr PinConfiguration Blast{Pinout::InTransaction, PinDirection::Input, PinValue::High, PinValue::Low};
    constexpr PinConfiguration ManagementEngineReset { Pinout::MeReset, PinDirection::Output};
    constexpr PinConfiguration WaitBoot960 { Pinout::WaitBoot960, PinDirection::Output};
    constexpr PinConfiguration IOEXP_INT0 { Pinout::IoExpander_Int0, PinDirection::Input};
    constexpr PinConfiguration IOEXP_INT1 { Pinout::IoExpander_Int1, PinDirection::Input};
    constexpr PinConfiguration IOEXP_INT2 { Pinout::IoExpander_Int2, PinDirection::Input};
    constexpr PinConfiguration IOEXP_INT3 { Pinout::IoExpander_Int3, PinDirection::Input};
    constexpr PinConfiguration IOEXP_INT4 { Pinout::IoExpander_Int4, PinDirection::Input};
    constexpr PinConfiguration IOEXP_INT5 { Pinout::IoExpander_Int5, PinDirection::Input};
    constexpr PinConfiguration IOEXP_INT6 { Pinout::IoExpander_Int6, PinDirection::Input};
    constexpr PinConfiguration IOEXP_INT7 { Pinout::IoExpander_Int7, PinDirection::Input};
    enum class LoadStoreStyle : byte {
        None,
        Lower8,
        Upper8,
        Full16,
    };
    void waitForCycleUnlock() noexcept;
    void waitForBootSignal() noexcept;
    void systemSetup() noexcept;
    void setupDataLinesForRead() noexcept;
    bool isReadOpeation() noexcept;
    bool isWriteOperation() noexcept;
    LoadStoreStyle getStyle() noexcept;
    uint32_t getAddress() noexcept;
    void performReadTransaction() noexcept;
    void performWriteTransaction() noexcept;
    void newDataCycle() noexcept;
    void waitForTransactionStart() noexcept;
    void shutdown(const std::string &message) noexcept;
    void installExtensions(Electron::Environment &theEnv);
}
#endif //MAYA_CHIPSETINTERFACE_H
