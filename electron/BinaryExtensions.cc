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
static void leftShift(UDF_ARGS__);
static void rightShift(UDF_ARGS__);

void
InitializeBinaryExtensions(RawEnvironment* env)
{
    auto& theEnv = Environment::fromRaw(env);
    theEnv.addFunction("left-shift", "lb", 2, 2, "l", leftShift, "leftShift");
    theEnv.addFunction("right-shift", "lb", 2, 2, "l", rightShift, "rightShift");
#if 0
    theEnv.addFunction("has-prefix", "b", 2, 2, "sy;sy;sy", hasPrefix, "hasPrefix");
    theEnv.addFunction("has-suffix", "b", 2, 2, "sy;sy;sy", hasSuffix, "hasSuffix");
    theEnv.addFunction("split-once", "mb", 2, 2, "sy;sy;sy", splitOnce, "splitOnce");
    theEnv.addFunction("replace-all", "sy", 3, 3, "sy;sy;sy;sy", replaceAll, "replaceAll");
    theEnv.addFunction("replace-regex-all", "sy", 3, 3, "sy;sy;sy;sy", replaceRegexAll, "replaceRegexAll");
#endif
}
namespace {
    std::optional<std::tuple<int64_t, int64_t>>
    extractTwoArguments(Environment& theEnv, Electron::UDFContext* context) {
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
leftShift(UDF_ARGS__) 
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
rightShift(UDF_ARGS__) 
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
#if 0
void
splitOnce(UDF_ARGS__)
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    UDFValue arg1;
    if (!theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }

    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    std::string str(arg0.lexemeValue->contents);
    std::string split(arg1.lexemeValue->contents);
    MultifieldBuilder mfb(theEnv);
    if (auto result = str.find(split); result == std::string::npos) {
        mfb.append(theEnv.createString(str));
    } else {
        mfb.append(str.substr(0, result), TreatLexemeAsString {});
        mfb.append(str.substr(result + split.length()), TreatLexemeAsString{});
    }
    out->multifieldValue = mfb.create();
}

void
hasPrefix(UDF_ARGS__)
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    UDFValue arg1;
    if (!theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    std::string str(arg0.lexemeValue->contents);
    std::string prefix(arg1.lexemeValue->contents);
    out->lexemeValue = theEnv.createBool(Neutron::hasPrefix(str, prefix));
}

void
hasSuffix(UDF_ARGS__)
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    UDFValue arg1;
    if (!theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    std::string str(arg0.lexemeValue->contents);
    std::string prefix(arg1.lexemeValue->contents);
    out->lexemeValue = theEnv.createBool(Neutron::hasSuffix(str, prefix));
}

void
replaceAll(UDF_ARGS__)
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    UDFValue arg1;
    UDFValue arg2;
    if (!theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg2)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    std::string str(arg0.lexemeValue->contents);
    std::string find(arg1.lexemeValue->contents);
    std::string replacement(arg2.lexemeValue->contents);
    auto result = Neutron::replace_all_copy(str, find, replacement);
    out->lexemeValue = theEnv.createString(result);
}

void
replaceRegexAll(UDF_ARGS__)
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    UDFValue arg1;
    UDFValue arg2;
    if (!theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg2)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    std::string str(arg0.lexemeValue->contents);
    std::string find(arg1.lexemeValue->contents);
    std::string replacement(arg2.lexemeValue->contents);
    auto result = Neutron::replace_all_regex_copy(str, Neutron::regex(find), replacement);
    out->lexemeValue = theEnv.createString(result);
}
#endif

} // end namespace Electron
