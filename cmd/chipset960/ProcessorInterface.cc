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
#ifdef V1Layout
        return WR.isAsserted();
#else
        return false;
#endif
    }

    bool
    ChipsetInterface::isWriteOperation() noexcept {
#ifdef V1Layout
        return WR.isDeasserted();
#else
        return false;
#endif
    }

    LoadStoreStyle
    ChipsetInterface::getStyle() noexcept {
        auto be0Asserted = false;
        auto be1Asserted = false;
#ifdef V1Layout
        be0Asserted = BE0.isAsserted();
        be1Asserted = BE1.isAsserted();
#else
#endif
        if (be0Asserted) {
            if (be1Asserted)  {
                return LoadStoreStyle::Full16;
            } else {
                return LoadStoreStyle::Lower8;
            }
        } else {
            if (be1Asserted) {
                return LoadStoreStyle::Upper8;
            } else {
                return LoadStoreStyle::None;
            }
        }
    }
    void
    ChipsetInterface::waitForTransactionStart() noexcept {
#ifdef V1Layout
        while (InTransaction.isDeasserted());
#else
#endif
    }
#if V1Layout
    void
    ChipsetInterface::write8(IOExpanderAddress address, MCP23x17Registers target, uint8_t value) {
        uint8_t command[4] {
                generateWriteOpcode(address),
                static_cast<uint8_t>(target),
                value,
                0,
        };
        doSPITransaction(command, 3);
    }
    void
    ChipsetInterface::write16(IOExpanderAddress address, MCP23x17Registers target, uint16_t value) {
        uint8_t command[4] {
                generateWriteOpcode(address) ,
                static_cast<uint8_t>(target),
                static_cast<uint8_t>(value),
                static_cast<uint8_t>(value >> 8),
        };
        doSPITransaction(command, 4);
    }
    uint8_t
    ChipsetInterface::read8(IOExpanderAddress address, MCP23x17Registers target) {
        uint8_t command[4] {
                generateReadOpcode(address) ,
                static_cast<uint8_t>(target),
                0,
                0,
        };
        doSPITransaction(command, 3);
        return command[2];
    }
    uint16_t
    ChipsetInterface::read16(IOExpanderAddress address, MCP23x17Registers target) {
        uint8_t command[4] {
                generateReadOpcode(address) ,
                static_cast<uint8_t>(target),
                0,
                0,
        };
        doSPITransaction(command, 4);
        return static_cast<uint16_t>(command[2]) | (static_cast<uint16_t>(command[3]) << 8);
    }
    void
    ChipsetInterface::doSPITransaction(uint8_t *command, int count) {
        Neutron::SPI::transfer(0, reinterpret_cast<char*>(command), count);
    }
    void
    ChipsetInterface::setupDataLines() noexcept {
        std::cout << "Setting up Data Lines" << std::endl;
        /// setup HAEN
        static constexpr uint8_t ioconDefault = 0b0000'1000;
        setIOCON<IOExpanderAddress::Lower16Lines>(ioconDefault);
        setIOCON<IOExpanderAddress::Upper16Lines>(ioconDefault);
        setIOCON<IOExpanderAddress::DataLines>(ioconDefault);
        setIOCON<IOExpanderAddress::Backplane>(ioconDefault);
        if (auto result = getIOCON<IOExpanderAddress::Lower16Lines>(); result != ioconDefault) {
            if (result == 0) {
                shutdown("Unable to communicate with the IO Expanders! There is probably a lock open on the SPI device! Reboot the Raspberry Pi!");
            } else {
                shutdown("Outcome from ioexpander is not what is expected but is not zero! Double check the code!");
            }
        }
        setDirection<IOExpanderAddress::Backplane>(backplaneGPIODirection_);
        writeGPIO16<IOExpanderAddress::Backplane>(backplaneGPIOStatus_);
        setDirection<IOExpanderAddress::Lower16Lines>(0xFFFF);
        setDirection<IOExpanderAddress::Upper16Lines>(0xFFFF);
        setDirection<IOExpanderAddress::DataLines>(currentDataLineDirection_);
        // setup interrupts to accelerate performance of address reads
        write16(IOExpanderAddress::Lower16Lines, MCP23x17Registers::GPINTEN, 0xFFFF);
        write16(IOExpanderAddress::Lower16Lines, MCP23x17Registers::INTCON, 0);
        write16(IOExpanderAddress::Upper16Lines, MCP23x17Registers::GPINTEN, 0xFFFF);
        write16(IOExpanderAddress::Upper16Lines, MCP23x17Registers::INTCON, 0);
        write16(IOExpanderAddress::DataLines, MCP23x17Registers::GPINTEN, 0xFFFF);
        write16(IOExpanderAddress::DataLines, MCP23x17Registers::INTCON, 0);
        write16(IOExpanderAddress::DataLines, MCP23x17Registers::OLAT, latchedDataOutput_.getWholeValue());
    }
#endif
    void
    ChipsetInterface::waitForBootSignal() noexcept {
        std::cout << "Waiting for boot signal" << std::endl;
        while (BootSuccessful.digitalRead() == PinValue::Low);
        /// @todo add interrupt to BootSuccessful pin
        Neutron::GPIO::attachInterrupt(static_cast<int>(Pinout::BootSuccessful),
                                       Neutron::GPIO::InterruptMode::Falling,
                                       []() { i960::shutdown("CHECKSUM FAILURE"); });
    }
    void
    ChipsetInterface::shutdown(const std::string& str) noexcept {
        std::cout << "SHUTDOWN: " << str << std::endl;
        SetHaltExecution(getRawEnvironment(),true);
        CloseAllBatchSources(getRawEnvironment());
#ifdef V1Layout
        ManagementEngineReset.assertPin();
        WaitBoot960.assertPin();
        ManagementEngineReset.deassertPin();
#else
#endif
        /// @todo insert GPIO release command here
        // exit at this point
        exit(1);
    }
    ChipsetInterface::ChipsetInterface() : ChipsetInterface::Parent() {
        installExtensions();
    }
    void
    ChipsetInterface::setupPins() noexcept {
#ifdef V1Layout
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
#else
#endif
    }
    void
    shutdown(const std::string& msg) noexcept {
        ChipsetInterface::get().shutdown(msg);
    }
    void
    ChipsetInterface::putManagementEngineInReset() noexcept {
#ifdef V1Layout
        ManagementEngineReset.assertPin();
        WaitBoot960.assertPin();
#else
#endif
    }
    void
    ChipsetInterface::pullManagementEngineOutOfReset() noexcept {
#ifdef V1Layout
        ManagementEngineReset.deassertPin();
#else

#endif
    }
    void
    ChipsetInterface::loadMicrocode() noexcept {
        if (!batchFile("ucode.clp")) {
            shutdown("Cannot find ucode.clp!");
        }
    }
    void
    ChipsetInterface::pull960OutOfReset() noexcept {
#ifdef V1Layout
        WaitBoot960.deassertPin();
#else
#endif
    }
    bool
    ChipsetInterface::signalCPU() noexcept {
        auto outcome = false;
        Ready.assertPin();
#ifdef V1Layout
        while (InTransaction.isAsserted() && Blast.isDeasserted());
        outcome = InTransaction.isDeasserted();
#else
#endif
        Ready.deassertPin();
        return outcome;
    }
    void
    ChipsetInterface::waitForCycleUnlock() noexcept {
#ifdef V1Layout
        while (DoCycle.isDeasserted());
#else
#endif
    }
    uint16_t
    ChipsetInterface::getDataLines() noexcept {
#ifdef V1Layout
        uint8_t actionType = digitalRead(Pinout::IoExpander_Int4) == PinValue::High ? 0b0001 : 0;
        actionType |= digitalRead(Pinout::IoExpander_Int5) == PinValue::High ? 0b0010 : 0;
        switch (actionType & 0b11) {
            case 0b00:
                latchedDataInput_.wholeValue_ = readGPIO16<IOExpanderAddress::DataLines>();
                break;
            case 0b01:
                latchedDataInput_.bytes[1] = read8<IOExpanderAddress::DataLines, MCP23x17Registers::GPIOB>();
                break;
            case 0b10:
                latchedDataInput_.bytes[0] = read8<IOExpanderAddress::DataLines, MCP23x17Registers::GPIOA>();
                break;
            case 0b11:
                break;
        }
        return latchedDataInput_.getWholeValue();
#else
        return 0;
#endif
    }

    void
    ChipsetInterface::setDataLines(uint16_t value) noexcept {
#ifdef V1Layout
        if (SplitWord16 wrap(value); wrap.getWholeValue() != latchedDataOutput_.getWholeValue()) {
            if (wrap.getLowerHalf() == latchedDataOutput_.getLowerHalf()) {
                // okay it is the upper half that is not the same
                write8<IOExpanderAddress::DataLines, MCP23x17Registers::GPIOB>(wrap.getUpperHalf());
            } else if (wrap.getUpperHalf() == latchedDataOutput_.getUpperHalf()) {
                // okay so it is the lower half that is different.
                write8<IOExpanderAddress::DataLines, MCP23x17Registers::GPIOA>(wrap.getLowerHalf());
            } else {
                // both the upper and lower halves are different
                writeGPIO16<IOExpanderAddress::DataLines>(wrap.getWholeValue());
            }
            // then update the latch
            latchedDataOutput_ = wrap;
        }
#else
#endif
    }
    void
    ChipsetInterface::setupDataLinesForWrite() noexcept {
#ifdef V1Layout
        // input
        if (!currentDataLineDirection_) {
            currentDataLineDirection_ = ~currentDataLineDirection_;
            setDirection<IOExpanderAddress::DataLines>(currentDataLineDirection_);
        }
#endif
    }
    void
    ChipsetInterface::setupDataLinesForRead() noexcept {
#ifdef V1Layout
        // output
        if (currentDataLineDirection_) {
            currentDataLineDirection_ = ~currentDataLineDirection_;
            setDirection<IOExpanderAddress::DataLines>(currentDataLineDirection_);
        }
#endif
    }
    namespace {
        void
        doChipsetShutdown(UDF_ARGS__) noexcept {
            auto& theEnv = reinterpret_cast<ChipsetInterface&>(Electron::Environment::fromRaw(env));
            UDFValue message;
            if (!theEnv.firstArgument(context, Electron::ArgumentBits::Lexeme, &message)) {
                out->lexemeValue = theEnv.falseSymbol();
                return;
            }
            out->lexemeValue = theEnv.trueSymbol();
            std::string theStr(message.lexemeValue->contents);
            theEnv.shutdown(theStr);
        }
        /**
         * @brief CLIPS interface function to convert a lexeme in HEX form to
         */
        void
        doHexConversion(UDF_ARGS__) noexcept {
            auto& theEnv = reinterpret_cast<ChipsetInterface&>(Electron::Environment::fromRaw(env));
            UDFValue value;
            if (!theEnv.firstArgument(context, Electron::ArgumentBits::Lexeme, &value)) {
                out->lexemeValue = theEnv.falseSymbol();
                return;
            }
            std::string theStr(value.lexemeValue->contents);
            if (theStr.length() <= 2) {
                out->lexemeValue = theEnv.falseSymbol();
            } else {
                uint32_t resultantAddress = 0;
                std::istringstream iss(theStr);
                iss >> std::hex >> resultantAddress;
                out->integerValue = theEnv.createInteger(static_cast<long long>(resultantAddress));
            }
        }
        void
        performGetLowerHalfOfAddress(UDF_ARGS__) noexcept {
            auto& theEnv = Electron::Environment::fromRaw(env);
            UDFValue value;
            if (!theEnv.firstArgument(context, Electron::ArgumentBits::Integer, &value)) {
                out->lexemeValue = theEnv.falseSymbol();
                return;
            }
            auto address = static_cast<uint32_t>(value.integerValue->contents);
            out->integerValue = theEnv.createInteger(static_cast<uint16_t>(address));
        }
        void
        performGetUpperHalfOfAddress(UDF_ARGS__) noexcept {
            auto& theEnv = Electron::Environment::fromRaw(env);
            UDFValue value;
            if (!theEnv.firstArgument(context, Electron::ArgumentBits::Integer, &value)) {
                out->lexemeValue = theEnv.falseSymbol();
                return;
            }
            auto address = static_cast<uint32_t>(value.integerValue->contents);
            out->integerValue = theEnv.createInteger(static_cast<uint16_t>(address >> 16));
        }
        void
        performGetLowerHalfOfWord(UDF_ARGS__) noexcept {
            auto& theEnv = Electron::Environment::fromRaw(env);
            UDFValue value;
            if (!theEnv.firstArgument(context, Electron::ArgumentBits::Integer, &value)) {
                out->lexemeValue = theEnv.falseSymbol();
                return;
            }
            auto address = static_cast<uint16_t>(value.integerValue->contents);
            out->integerValue = theEnv.createInteger(static_cast<uint8_t>(address));
        }
        void
        performGetUpperHalfOfWord(UDF_ARGS__) noexcept {
            auto& theEnv = Electron::Environment::fromRaw(env);
            UDFValue value;
            if (!theEnv.firstArgument(context, Electron::ArgumentBits::Integer, &value)) {
                out->lexemeValue = theEnv.falseSymbol();
                return;
            }
            auto address = static_cast<uint16_t>(value.integerValue->contents);
            out->integerValue = theEnv.createInteger(static_cast<uint8_t>(address >> 8));
        }
    }
    void
    ChipsetInterface::installExtensions() noexcept {
        if (!extensionsInstalled_) {
            extensionsInstalled_ = true;
            addFunction("shutdown960",
                        Electron::makeReturnType(Electron::ArgumentTypes::Boolean),
                        1, 1, Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Symbol,
                                                                                  Electron::ArgumentTypes::String}),
                        doChipsetShutdown,
                        "doChipsetShutdown");
            addFunction("hex32->number",
                        Electron::optionalReturnType(Electron::ArgumentTypes::Integer),
                        1, 1,
                        Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Symbol, Electron::ArgumentTypes::String}),
                        doHexConversion,
                        "doHexConversion");
            addFunction("word32-lower-half",
                        Electron::makeReturnType(Electron::ArgumentTypes::Integer),
                        1, 1,
                        Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer}),
                        performGetLowerHalfOfAddress,
                        "getLowerHalfOfAddress");
            addFunction("word32-upper-half",
                        Electron::makeReturnType(Electron::ArgumentTypes::Integer),
                        1, 1,
                        Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer}),
                        performGetUpperHalfOfAddress,
                        "getUpperHalfOfAddress");
            addFunction("word16-lower-half",
                        Electron::makeReturnType(Electron::ArgumentTypes::Integer),
                        1, 1,
                        Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer}),
                        performGetLowerHalfOfWord,
                        "getLowerHalfOfWord");
            addFunction("word16-upper-half",
                        Electron::makeReturnType(Electron::ArgumentTypes::Integer),
                        1, 1,
                        Electron::makeArgumentList(Electron::SingleArgument{Electron::ArgumentTypes::Integer}),
                        performGetUpperHalfOfWord,
                        "getUpperHalfOfWord");

        }
    }
    size_t
    ChipsetInterface::read(uint32_t address, uint16_t* storage, size_t length) {
        auto& theChipset = get();
        for (int i = 0; i < length; ++i, address += 2) {
            storage[i] = theChipset.call<uint16_t>("perform-read", address);
        }
        return length;
    }
    size_t
    ChipsetInterface::write(uint32_t address, uint16_t* storage, size_t length) {
        auto& theChipset = get();
        Electron::Value returnsNothing;
        // this implementation is _not_ safe if one is not using uint16_t values
        for (int i = 0; i < length; ++i, address += 2) {
            theChipset.call("perform-write",
                            &returnsNothing,
                            address,
                            storage[i],
                            Electron::FunctionBuilder::symbol("full16"));
        }
        theChipset.run(-1L);
        return length;
    }
    uint32_t
    ChipsetInterface::getAddress() {
#ifdef V1Layout
        // read the interrupts for the address lines
        uint8_t actionType = digitalRead(Pinout::IoExpander_Int0) == PinValue::High ? 0b0001 : 0;
        actionType |= digitalRead(Pinout::IoExpander_Int1) == PinValue::High ? 0b0010 : 0;
        actionType |= digitalRead(Pinout::IoExpander_Int2) == PinValue::High ? 0b0100 : 0;
        actionType |= digitalRead(Pinout::IoExpander_Int3) == PinValue::High ? 0b1000 : 0;
        // for now we only care about the upper and lower halves for testing purposes
        /// @todo implement 8-bit reads
        switch (actionType & 0b1111) {
            case 0b0001:
                address_.halves[1] = readGPIO16<IOExpanderAddress::Upper16Lines>();
                address_.bytes[1] = read8<IOExpanderAddress::Lower16Lines, MCP23x17Registers::GPIOB>();
                break;
            case 0b0010:
                address_.halves[1] = readGPIO16<IOExpanderAddress::Upper16Lines>();
                address_.bytes[0] = read8<IOExpanderAddress::Lower16Lines, MCP23x17Registers::GPIOA>();
                break;
            case 0b0011:
                address_.halves[1] = readGPIO16<IOExpanderAddress::Upper16Lines>();
                break;
            case 0b0100:
                address_.halves[0] = readGPIO16<IOExpanderAddress::Lower16Lines>();
                address_.bytes[3] = read8<IOExpanderAddress::Upper16Lines, MCP23x17Registers::GPIOB>();
                break;
            case 0b0101:
                address_.bytes[3] = read8<IOExpanderAddress::Upper16Lines, MCP23x17Registers::GPIOB>();
                address_.bytes[1] = read8<IOExpanderAddress::Lower16Lines, MCP23x17Registers::GPIOB>();
                break;
            case 0b0110:
                address_.bytes[3] = read8<IOExpanderAddress::Upper16Lines, MCP23x17Registers::GPIOB>();
                address_.bytes[0] = read8<IOExpanderAddress::Lower16Lines, MCP23x17Registers::GPIOA>();
                break;
            case 0b0111:
                address_.bytes[3] = read8<IOExpanderAddress::Upper16Lines, MCP23x17Registers::GPIOB>();
                break;
            case 0b1000:
                address_.halves[0] = readGPIO16<IOExpanderAddress::Lower16Lines>();
                address_.bytes[2] = read8<IOExpanderAddress::Upper16Lines, MCP23x17Registers::GPIOA>();
                break;
            case 0b1001:
                address_.bytes[2] = read8<IOExpanderAddress::Upper16Lines, MCP23x17Registers::GPIOA>();
                address_.bytes[1] = read8<IOExpanderAddress::Lower16Lines, MCP23x17Registers::GPIOB>();
                break;
            case 0b1010:
                address_.bytes[2] = read8<IOExpanderAddress::Upper16Lines, MCP23x17Registers::GPIOA>();
                address_.bytes[0] = read8<IOExpanderAddress::Lower16Lines, MCP23x17Registers::GPIOA>();
                break;
            case 0b1011:
                address_.bytes[2] = read8<IOExpanderAddress::Upper16Lines, MCP23x17Registers::GPIOA>();
                break;
            case 0b1100:
                address_.halves[0] = readGPIO16<IOExpanderAddress::Lower16Lines>();
                break;
            case 0b1101:
                address_.bytes[1] = read8<IOExpanderAddress::Lower16Lines, MCP23x17Registers::GPIOB>();
                break;
            case 0b1110:
                address_.bytes[0] = read8<IOExpanderAddress::Lower16Lines, MCP23x17Registers::GPIOA>();
                break;
            case 0b1111:
                break;
            default:
                // all are zero or the zeros span upper and lower halves
               address_.halves[0] = readGPIO16<IOExpanderAddress::Lower16Lines>();
               address_.halves[1] = readGPIO16<IOExpanderAddress::Upper16Lines>();
               break;
        }
        return address_.getWholeValue();
#else
        return 0;
#endif
    }
}