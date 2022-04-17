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
#include "Common.h"
#include "platform/os.h"
#include "electron/Environment.h"
#include "interface/spi.h"
#include "interface/gpio.h"
#include "platform/config.h"
#include <memory>
namespace i960 {
    template<int index>
    constexpr auto PhysicalPinToGPIOIndex_v = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<index>;
    enum class Pinout {
#ifdef V1Layout
        BootSuccessful = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<7>,
        Ready = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<29>,
        WR = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<31>,
        BE0 = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<32>,
        BE1 = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<33>,
        InTransaction = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<8>,
        DoCycle = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<10>,
        Blast = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<36>,
        MeReset = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<11>,
        WaitBoot960 = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<12>,
        IoExpander_Int0 = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<35>,
        IoExpander_Int1 = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<38>,
        IoExpander_Int2 = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<40>,
        IoExpander_Int3 = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<15>,
        IoExpander_Int4 = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<16>,
        IoExpander_Int5 = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<18>,
        IoExpander_Int6 = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<22>,
        IoExpander_Int7 = Neutron::GPIO::RaspberryPi::PhysicalPinToGPIOIndex_v<37>,
#else
        BootSuccessful = PhysicalPinToGPIOIndex_v<29>,
        Ready = PhysicalPinToGPIOIndex_v<31>,
        GPIO0 = PhysicalPinToGPIOIndex_v<32>,
        GPIO1 = PhysicalPinToGPIOIndex_v<33>,
        GPIO2 = PhysicalPinToGPIOIndex_v<8>,
        GPIO3 = PhysicalPinToGPIOIndex_v<10>,
        GPIO4 = PhysicalPinToGPIOIndex_v<36>,
        GPIO5 = PhysicalPinToGPIOIndex_v<11>,
        GPIO6 = PhysicalPinToGPIOIndex_v<12>,
        GPIO7 = PhysicalPinToGPIOIndex_v<35>,
        GPIO8 = PhysicalPinToGPIOIndex_v<38>,
        GPIO9 = PhysicalPinToGPIOIndex_v<40>,
        GPIO10 = PhysicalPinToGPIOIndex_v<15>,
        GPIO11 = PhysicalPinToGPIOIndex_v<16>,
        GPIO12 = PhysicalPinToGPIOIndex_v<18>,
        GPIO13 = PhysicalPinToGPIOIndex_v<22>,
        GPIO14 = PhysicalPinToGPIOIndex_v<37>,
        GPIO15 = PhysicalPinToGPIOIndex_v<13>,
        Data0 = GPIO0,
        Data1 = GPIO1,
        Data2 = GPIO2,
        Data3 = GPIO3,
        Data4 = GPIO4,
        Data5 = GPIO5,
        Data6 = GPIO6,
        Data7 = GPIO7,
        Address0 = GPIO8,
        Address1 = GPIO9,
        Address2 = GPIO10,
        Address3 = GPIO11,
        Address4 = GPIO12,
        Read = GPIO13,
        Write = GPIO14,
        BusEnable = GPIO15,
#endif
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
#ifdef V1Layout
    constexpr PinConfiguration WR {Pinout::WR, PinDirection::Input};
    constexpr PinConfiguration BE0 {Pinout::BE0, PinDirection::Input};
    constexpr PinConfiguration BE1 {Pinout::BE1, PinDirection::Input};
    constexpr PinConfiguration InTransaction {Pinout::InTransaction, PinDirection::Input};
    constexpr PinConfiguration DoCycle {Pinout::DoCycle, PinDirection::Input};
    constexpr PinConfiguration Blast{Pinout::Blast, PinDirection::Input, PinValue::High, PinValue::Low};
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
#else
    /// @todo data lines
    constexpr PinConfiguration Address0 { Pinout::Address0, PinDirection::Output };
    constexpr PinConfiguration Address1 { Pinout::Address1, PinDirection::Output };
    constexpr PinConfiguration Address2 { Pinout::Address2, PinDirection::Output };
    constexpr PinConfiguration Address3 { Pinout::Address3, PinDirection::Output };
    constexpr PinConfiguration Address4 { Pinout::Address4, PinDirection::Output };
    constexpr PinConfiguration Read { Pinout::Read, PinDirection::Output };
    constexpr PinConfiguration Write { Pinout::Write, PinDirection::Output };
    constexpr PinConfiguration BusEnable { Pinout::BusEnable, PinDirection::Output };
#endif
    enum class LoadStoreStyle : byte {
        None,
        Lower8,
        Upper8,
        Full16,
    };
    void shutdown(const std::string &message) noexcept;

    union MemoryCell {
        uint16_t word;
        uint8_t bytes[2];
    };
    class ChipsetInterface : public Electron::Environment
    {
    public:
        static constexpr uint32_t MemorySize = 64 * 1024 * 1024;
        static constexpr auto NumberOfCells = MemorySize / sizeof(MemoryCell);
        using Parent = Electron::Environment;
        using Address = uint32_t;
    private:
        ChipsetInterface();
    public:
        ~ChipsetInterface() override = default;
        ChipsetInterface(const ChipsetInterface&) = delete;
        ChipsetInterface(ChipsetInterface&&) = delete;
        ChipsetInterface& operator=(const ChipsetInterface&) = delete;
        ChipsetInterface& operator=(ChipsetInterface&&) = delete;
        static ChipsetInterface& get() noexcept {
            static ChipsetInterface theInterface;
            return theInterface;
        }
        void setupPins() noexcept;
        void putManagementEngineInReset() noexcept;
        void pullManagementEngineOutOfReset() noexcept;
        /**
         * @brief Deasserts WaitBoot960
         */
        void pull960OutOfReset() noexcept;
        void loadMicrocode() noexcept;
        void begin();
        void shutdown(const std::string& reason) noexcept;
        bool isReadOperation() noexcept;
        bool isWriteOperation() noexcept;
        void waitForCycleUnlock() noexcept;
        void waitForBootSignal() noexcept;
        LoadStoreStyle getStyle() noexcept;
        void waitForTransactionStart() noexcept;
        void setupDataLinesForRead() noexcept;
        void setupDataLinesForWrite() noexcept;
        void setDataLines(uint16_t value) noexcept;
        uint16_t getDataLines() noexcept;
        void setupDataLines() noexcept;
        enum class IOExpanderAddress : uint8_t {
            Lower16Lines= 0,
            Upper16Lines,
            DataLines,
            Backplane,

        };
        enum class MCP23x17Registers : uint8_t {
            IODIRA = 0,
            IODIRB,
            IPOLA,
            IPOLB,
            GPINTENA,
            GPINTENB,
            DEFVALA,
            DEFVALB,
            INTCONA,
            INTCONB,
            _IOCONA,
            _IOCONB,
            GPPUA,
            GPPUB,
            INTFA,
            INTFB,
            INTCAPA,
            INTCAPB,
            GPIOA,
            GPIOB,
            OLATA,
            OLATB,
            OLAT = OLATA,
            GPIO = GPIOA,
            IOCON = _IOCONA,
            IODIR = IODIRA,
            INTCAP = INTCAPA,
            INTF = INTFA,
            GPPU = GPPUA,
            INTCON = INTCONA,
            DEFVAL = DEFVALA,
            GPINTEN = GPINTENA,
            IPOL = IPOLA,
        };
        uint16_t read16(IOExpanderAddress address, MCP23x17Registers target);
        uint8_t read8(IOExpanderAddress address, MCP23x17Registers target);
        void write16(IOExpanderAddress address, MCP23x17Registers target, uint16_t value);
        void write8(IOExpanderAddress address, MCP23x17Registers target, uint8_t value);
        template<IOExpanderAddress address, MCP23x17Registers target>
        void write16(uint16_t value) noexcept {
            write16(address, target, value);
        }
        template<IOExpanderAddress address, MCP23x17Registers target>
        void write8(uint8_t value) noexcept {
            write8(address, target, value);
        }
        template<IOExpanderAddress address, MCP23x17Registers target>
        uint8_t read8() noexcept {
            return read8(address, target);
        }
        template<IOExpanderAddress address, MCP23x17Registers target>
        uint16_t read16() noexcept {
            return read16(address, target);
        }
        [[nodiscard]] inline uint16_t readGPIO16(IOExpanderAddress address) { return read16(address, MCP23x17Registers::GPIO); }
        template<IOExpanderAddress address>
        [[nodiscard]] inline uint16_t readGPIO16() {
            return read16<address, MCP23x17Registers::GPIO>();
        }
        inline void writeGPIO16(IOExpanderAddress address, uint16_t value) { write16(address, MCP23x17Registers::GPIO, value); }
        template<IOExpanderAddress address>
        inline void writeGPIO16(uint16_t value) {
            write16<address, MCP23x17Registers::GPIO>(value);
        }

        template<IOExpanderAddress address>
        inline void setDirection(uint16_t direction) {
            write16<address, MCP23x17Registers::IODIR>(direction);
        }
        template<IOExpanderAddress address>
        inline void setIOCON(uint8_t iocon) {
            write8<address, MCP23x17Registers::IOCON>(iocon);
        }
        template<IOExpanderAddress address>
        [[nodiscard]] inline uint8_t getIOCON() {
            return read8<address, MCP23x17Registers::IOCON>();
        }
        [[nodiscard]] uint32_t getAddress();
        bool signalCPU() noexcept;
    private:
        void doSPITransaction(uint8_t* storage, int count);
        void installExtensions() noexcept;
    private:
        static constexpr uint8_t generateReadOpcode(IOExpanderAddress address) noexcept { return 0b0100'0001 | (static_cast<uint8_t>(address) << 1); }
        static constexpr uint8_t generateWriteOpcode(IOExpanderAddress address) noexcept { return 0b0100'0000 | (static_cast<uint8_t>(address) << 1); }
    public:
        static size_t read(uint32_t address, uint16_t* storage, size_t length);
        static size_t write(uint32_t address, uint16_t* storage, size_t length);
    private:
        bool extensionsInstalled_ = false;
        uint16_t backplaneGPIOStatus_ = 0x0000;
        uint16_t backplaneGPIODirection_ = 0x0000;
        uint16_t currentDataLineDirection_ = 0xFFFF;
        SplitWord16 latchedDataOutput_ { 0 };
        SplitWord16 latchedDataInput_ { 0 };
        SplitWord32 address_{0};
    };
}
#endif //MAYA_CHIPSETINTERFACE_H
