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

void
InitializeBinaryExtensions(RawEnvironment* env)
{
    auto& theEnv = Environment::fromRaw(env);
    theEnv.addFunction("left-shift", "lb", 2, 2, "l", leftShift, "leftShift");
    theEnv.addFunction("right-shift", "lb", 2, 2, "l", rightShift, "rightShift");
    theEnv.addFunction("bitwise-and", "l", 2, 2, "l", andOperation, "andOperation");
    theEnv.addFunction("bitwise-or", "l", 2, 2, "l", orOperation, "orOperation");
    theEnv.addFunction("bitwise-xor", "l", 2, 2, "l", xorOperation, "xorOperation");
    theEnv.addFunction("bitwise-not", "l", 1, 1, "l", notOperation, "notOperation");
    theEnv.addFunction("setbit", "l", 2, 2, "l", setbitOperation, "setbitOperation");
    theEnv.addFunction("clrbit", "l", 2, 2, "l", clrbitOperation, "clrbitOperation");
    theEnv.addFunction("notbit", "l", 2, 2, "l", notbitOperation, "notbitOperation");
    theEnv.addFunction("chkbit", "b", 2, 2, "l", chkbitOperation, "chkbitOperation");
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
    UDFValue arg;
    if (!theEnv.firstArgument(context, ArgumentBits::Integer, &arg)) {
        out->lexemeValue = theEnv.falseSymbol();
    } else {
        out->integerValue = theEnv.createInteger(~arg.integerValue->contents);
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

} // end namespace Electron
