/**
 * @file
 * Implementation of the extensions to electron which provide extra binary analysis extensions 
 * @copyright
 * Copyright (c) 2012-2025 Joshua Scoggins
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
#include "electron/StringExtensions.h"
#include "electron/MultifieldBuilder.h"

#include <climits>

extern "C" {
#include "clips/clips.h"
}


namespace Electron
{
    // (binary-explode ?number ?width) => multifield (lsb -> msb)
static void binaryExplodeNumber(UDF_ARGS__);

void
InitializeBinaryExtensions(RawEnvironment* env) 
{
    auto& theEnv = Environment::fromRaw(env);
    theEnv.addFunction("binary-explode", "mb", 2, 2, "l;l;l", binaryExplodeNumber, "binaryExplodeNumber");
}

// backend operation for exploding a given number into a given width
// (1, 2, 3, 4, 5, 6, 7, 8...)
void
binaryExplodeNumber(UDF_ARGS__) 
{
    using NumberBackingStore = decltype(Electron::Integer::contents);
    static constexpr auto MAXIMUM_BIT_COUNT = CHAR_BIT * sizeof(NumberBackingStore);
    auto& theEnv = Environment::fromRaw(env);
    UDFValue number;
    UDFValue size;
    if (!theEnv.firstArgument(context, ArgumentBits::Integer, &number)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Integer, &size)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    auto theNumber = number.integerValue->contents;
    auto theSize = size.integerValue->contents;
    if (theNumber <= 0) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    MultifieldBuilder mfb(theEnv);
    out->multifieldValue = mfb.create();
#if 0
    for (int i = 0; i < 
#endif

}
#if 0
void
InitializeStringExtensions(RawEnvironment* env)
{
    auto& theEnv = Environment::fromRaw(env);
    theEnv.addFunction("has-prefix", "b", 2, 2, "sy;sy;sy", hasPrefix, "hasPrefix");
    theEnv.addFunction("has-suffix", "b", 2, 2, "sy;sy;sy", hasSuffix, "hasSuffix");
    theEnv.addFunction("split-once", "mb", 2, 2, "sy;sy;sy", splitOnce, "splitOnce");
    theEnv.addFunction("replace-all", "sy", 3, 3, "sy;sy;sy;sy", replaceAll, "replaceAll");
    theEnv.addFunction("replace-regex-all", "sy", 3, 3, "sy;sy;sy;sy", replaceRegexAll, "replaceRegexAll");
}
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
#endif


} // end namespace Electron
