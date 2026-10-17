/**
 * @file
 * Implementation of the extensions to electron which provide binary manipulation operations
 * @copyright
 * Copyright (c) 2015-2022 Parasoft Corporation
 * Copyright (c) 2026 Joshua Scoggins
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 */

#include "electron/Environment.h"
#include "electron/BinaryExtensions.h"
#include "electron/MultifieldBuilder.h"

#include <tuple>
#include <optional>
#include <bit>

extern "C" {
#include "clips/clips.h"
}


namespace Electron
{
static void leftShift(UDF_ARGS__) noexcept;
static void rightShift(UDF_ARGS__) noexcept;
static void andOperation(UDF_ARGS__) noexcept;
static void orOperation(UDF_ARGS__) noexcept;
static void notOperation(UDF_ARGS__) noexcept;
static void xorOperation(UDF_ARGS__) noexcept;
static void setbitOperation(UDF_ARGS__) noexcept;
static void clrbitOperation(UDF_ARGS__) noexcept;
static void notbitOperation(UDF_ARGS__) noexcept;
static void chkbitOperation(UDF_ARGS__) noexcept;
static void alterbitOperation(UDF_ARGS__) noexcept;
static void andnotOperation(UDF_ARGS__) noexcept;
static void notandOperation(UDF_ARGS__) noexcept;
static void ornotOperation(UDF_ARGS__) noexcept;
static void notorOperation(UDF_ARGS__) noexcept;
static void nandOperation(UDF_ARGS__) noexcept;
static void norOperation(UDF_ARGS__) noexcept;
static void xnorOperation(UDF_ARGS__) noexcept;
static void splitIntoHalvesOperation(UDF_ARGS__) noexcept;
static void extractOperation(UDF_ARGS__) noexcept;
static void modifyOperation(UDF_ARGS__) noexcept;
static void rotateLeftOperation(UDF_ARGS__) noexcept;
static void rotateRightOperation(UDF_ARGS__) noexcept;
static void countLeadingZeros(UDF_ARGS__) noexcept;
static void countLeadingOnes(UDF_ARGS__) noexcept;
static void countTrailingZeros(UDF_ARGS__) noexcept;
static void countTrailingOnes(UDF_ARGS__) noexcept;
static void populationCount(UDF_ARGS__) noexcept;
static void getPlatformEndianness(UDF_ARGS__) noexcept;

void
InitializeBinaryExtensions(RawEnvironment* env)
{
    auto& theEnv = Environment::fromRaw(env);
    theEnv.addFunction("bitwise-left-shift", "lb", 2, 2, "l", leftShift, "leftShift");
    theEnv.addFunction("bitwise-right-shift", "lb", 2, 2, "l", rightShift, "rightShift");
    theEnv.addFunction("bitwise-and", "l", 2, 2, "l", andOperation, "andOperation");
    theEnv.addFunction("bitwise-or", "l", 2, 2, "l", orOperation, "orOperation");
    theEnv.addFunction("bitwise-xor", "l", 2, 2, "l", xorOperation, "xorOperation");
    theEnv.addFunction("bitwise-not", "l", 1, 1, "l", notOperation, "notOperation");
    theEnv.addFunction("bitwise-setbit", "l", 2, 2, "l", setbitOperation, "setbitOperation");
    theEnv.addFunction("bitwise-clrbit", "l", 2, 2, "l", clrbitOperation, "clrbitOperation");
    theEnv.addFunction("bitwise-notbit", "l", 2, 2, "l", notbitOperation, "notbitOperation");
    theEnv.addFunction("bitwise-chkbit", "b", 2, 2, "l", chkbitOperation, "chkbitOperation");
    theEnv.addFunction("bitwise-alterbit", "lb", 3, 3, "l;l;l;b", alterbitOperation, "alterbitOperation");
    theEnv.addFunction("bitwise-andnot", "l", 2, 2, "l", andnotOperation, "andnotOperation");
    theEnv.addFunction("bitwise-ornot", "l", 2, 2, "l", ornotOperation, "ornotOperation");
    theEnv.addFunction("bitwise-notand", "l", 2, 2, "l", notandOperation, "notandOperation");
    theEnv.addFunction("bitwise-notor", "l", 2, 2, "l", notorOperation, "notorOperation");
    theEnv.addFunction("bitwise-xnor", "l", 2, 2, "l", xnorOperation, "xnorOperation");
    theEnv.addFunction("bitwise-nand", "l", 2, 2, "l", nandOperation, "nandOperation");
    theEnv.addFunction("bitwise-nor", "l", 2, 2, "l", norOperation, "norOperation");
    theEnv.addFunction("bitwise-split", "m", 1, 1, "l", splitIntoHalvesOperation, "splitIntoHalvesOperation");
    theEnv.addFunction("bitwise-extract", "l", 3, 3, "l", extractOperation, "extractOperation");
    theEnv.addFunction("bitwise-modify", "l", 3, 3, "l", modifyOperation, "modifyOperation");
    theEnv.addFunction("bitwise-rotate-left", "l", 2, 2, "l", rotateLeftOperation, "rotateLeftOperation");
    theEnv.addFunction("bitwise-rotate-right", "l", 2, 2, "l", rotateRightOperation, "rotateRightOperation");
    theEnv.addFunction("count-leading-zeros", "l", 1, 1, "l", countLeadingZeros, "countLeadingZeros");
    theEnv.addFunction("count-leading-ones", "l", 1, 1, "l", countLeadingOnes, "countLeadingOnes");
    theEnv.addFunction("count-trailing-zeros", "l", 1, 1, "l", countTrailingZeros, "countTrailingZeros");
    theEnv.addFunction("count-trailing-ones", "l", 1, 1, "l", countTrailingOnes, "countTrailingOnes");
    theEnv.addFunction("population-count", "l", 1, 1, "l", populationCount, "populationCount");
    theEnv.addFunction("platform-endianness", "sy", 0, 0, "sy", getPlatformEndianness, "getPlatformEndianness");

}
namespace {
    std::optional<std::tuple<int64_t, int64_t>>
    extractTwoArguments(Environment& theEnv, Electron::UDFContext* context) noexcept {
        UDFValue arg0, arg1;
        if (!theEnv.firstArgument(context, ArgumentBits::Integer, &arg0)) {
            return std::nullopt;
        } else if (!theEnv.nextArgument(context, ArgumentBits::Integer, &arg1)) {
            return std::nullopt;
        }
        return std::make_optional(std::make_tuple(arg0.integerValue->contents, arg1.integerValue->contents));
    }
    std::optional<int64_t>
    extractOneArgument(Environment& theEnv, Electron::UDFContext* context) noexcept {
        UDFValue arg0;
        if (!theEnv.firstArgument(context, ArgumentBits::Integer, &arg0)) {
            return std::nullopt;
        }
        return std::make_optional(arg0.integerValue->contents);
    }
}
void
leftShift(UDF_ARGS__) noexcept
{
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [value, len] = *result;
        // follow the design of what shlo does on the i960
        if (len < 0) {
            out->lexemeValue = theEnv.falseSymbol();
        } else if (len < 64) {
            out->integerValue = theEnv.createInteger(value << len);
        } else {
            out->integerValue = theEnv.createInteger(0);
        }
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}
void
rightShift(UDF_ARGS__) noexcept
{
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [value, len] = *result;
        // follow the design of what shlo does on the i960
        if (len < 0) {
            out->lexemeValue = theEnv.falseSymbol();
        } else if (len < 64) {
            out->integerValue = theEnv.createInteger(value >> len);
        } else {
            out->integerValue = theEnv.createInteger(0);
        }
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}
void
notOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractOneArgument(theEnv, context); result) {
        out->integerValue = theEnv.createInteger(~*result);
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}
void
andOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [arg0, arg1] = *result;
        // follow the design of what shlo does on the i960
        out->integerValue = theEnv.createInteger(arg0 & arg1);
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
orOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [arg0, arg1] = *result;
        // follow the design of what shlo does on the i960
        out->integerValue = theEnv.createInteger(arg0 | arg1);
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
xorOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [arg0, arg1] = *result;
        // follow the design of what shlo does on the i960
        out->integerValue = theEnv.createInteger(arg0 ^ arg1);
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
setbitOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [src, bitpos] = *result;
        // follow the design of what shlo does on the i960
        out->integerValue = theEnv.createInteger(src | (1ll << (bitpos & 0b111111)));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
clrbitOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [src, bitpos] = *result;
        // follow the design of what shlo does on the i960
        out->integerValue = theEnv.createInteger(src & ~(1ll << (bitpos & 0b111111)));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
notbitOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [src, bitpos] = *result;
        // follow the design of what shlo does on the i960
        out->integerValue = theEnv.createInteger(src ^ (1ll << (bitpos & 0b111111)));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}
void
chkbitOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [src, bitpos] = *result;
        // follow the design of what shlo does on the i960
        out->lexemeValue = theEnv.createBool((src & (1ll << (bitpos & 0b111111))) != 0);
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
alterbitOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0, arg1, arg2;
    if (!theEnv.firstArgument(context, ArgumentBits::Integer, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    } 
    if (!theEnv.nextArgument(context, ArgumentBits::Integer, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Boolean, &arg2)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    auto value = arg0.integerValue->contents;
    auto bitpos = arg1.integerValue->contents;
    // copies the source value to destination with one bit altered.
    if (!theEnv.extractValue<bool>(&arg2)) {
        out->integerValue = theEnv.createInteger(value & ~(1ll << (bitpos & 0b111'111)));
    } else {
        out->integerValue = theEnv.createInteger(value | (1ll << (bitpos & 0b111'111)));
    }
}

void
andnotOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [arg0, arg1] = *result;
        out->integerValue = theEnv.createInteger(arg0 & ~arg1);
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
notandOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [arg0, arg1] = *result;
        out->integerValue = theEnv.createInteger(~arg0 & arg1);
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
ornotOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [arg0, arg1] = *result;
        out->integerValue = theEnv.createInteger(arg0 | ~arg1);
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
notorOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [arg0, arg1] = *result;
        out->integerValue = theEnv.createInteger(~arg0 | arg1);
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
nandOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [arg0, arg1] = *result;
        // follow the design of what shlo does on the i960
        out->integerValue = theEnv.createInteger(~(arg0 & arg1));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
norOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [arg0, arg1] = *result;
        // follow the design of what shlo does on the i960
        out->integerValue = theEnv.createInteger(~(arg0 | arg1));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
xnorOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [arg0, arg1] = *result;
        // follow the design of what shlo does on the i960
        out->integerValue = theEnv.createInteger(~(arg0 ^ arg1));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}
void
splitIntoHalvesOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg;
    if (!theEnv.firstArgument(context, ArgumentBits::Integer, &arg)) {
        out->lexemeValue = theEnv.falseSymbol();
    } else {
        MultifieldBuilder mfb(theEnv);
        auto value = arg.integerValue->contents;
        uint32_t lo = static_cast<uint32_t>(value),
                 hi = static_cast<uint32_t>(value >> 32);
        mfb.append(static_cast<int64_t>(lo));
        mfb.append(static_cast<int64_t>(hi));
        out->multifieldValue = mfb.create();
    }
}

void
extractOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0, arg1, arg2;
    if (!theEnv.firstArgument(context, ArgumentBits::Integer, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    } 
    if (!theEnv.nextArgument(context, ArgumentBits::Integer, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Integer, &arg2)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    auto src = static_cast<uint64_t>(arg0.integerValue->contents);
    auto pos = static_cast<uint64_t>(arg1.integerValue->contents) & 0b111'111;
    auto length = static_cast<uint64_t>(arg2.integerValue->contents);
    if (length > 63) {
        length = 63;
    }
    auto result = (src >> pos) & ((static_cast<uint64_t>(1) << length) - 1);
    out->integerValue = theEnv.createInteger(result);
}

void
modifyOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0, arg1, arg2;
    if (!theEnv.firstArgument(context, ArgumentBits::Integer, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    } 
    if (!theEnv.nextArgument(context, ArgumentBits::Integer, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Integer, &arg2)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    auto src = static_cast<uint64_t>(arg0.integerValue->contents);
    auto src2 = static_cast<uint64_t>(arg1.integerValue->contents);
    auto mask = static_cast<uint64_t>(arg2.integerValue->contents);
    auto result = (src & mask) | (src2 & ~mask);
    out->integerValue = theEnv.createInteger(result);
}
void
rotateRightOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [arg0, arg1] = *result;
        auto src = static_cast<uint64_t>(arg0);
        out->integerValue = theEnv.createInteger(std::rotr(src, arg1));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
rotateLeftOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractTwoArguments(theEnv, context); result) {
        auto [arg0, arg1] = *result;
        auto src = static_cast<uint64_t>(arg0);
        out->integerValue = theEnv.createInteger(std::rotl(src, arg1));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
countLeadingZeros(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractOneArgument(theEnv, context); result) {
        out->integerValue = theEnv.createInteger(std::countl_zero(static_cast<uint64_t>(*result)));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
countLeadingOnes(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractOneArgument(theEnv, context); result) {
        out->integerValue = theEnv.createInteger(std::countl_one(static_cast<uint64_t>(*result)));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
countTrailingZeros(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractOneArgument(theEnv, context); result) {
        out->integerValue = theEnv.createInteger(std::countr_zero(static_cast<uint64_t>(*result)));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
countTrailingOnes(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractOneArgument(theEnv, context); result) {
        out->integerValue = theEnv.createInteger(std::countr_one(static_cast<uint64_t>(*result)));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
getPlatformEndianness(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if constexpr (std::endian::native == std::endian::big) {
        out->lexemeValue = theEnv.createSymbol("big");
    } else if constexpr (std::endian::native == std::endian::little) {
        out->lexemeValue = theEnv.createSymbol("little");
    } else {
        out->lexemeValue = theEnv.createSymbol("mixed");
    }
}
void
populationCount(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    if (auto result = extractOneArgument(theEnv, context); result) {
        out->integerValue = theEnv.createInteger(std::popcount(static_cast<uint64_t>(*result)));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}


} // end namespace Electron
