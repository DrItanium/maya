/**
 * @file
 * Implementation of the extensions to electron which provide extended numeric operations
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

#include <numeric>

extern "C" {
#include "clips/clips.h"
}


namespace Electron
{
namespace {
static void midpointOperation(UDF_ARGS__) noexcept;
}

void
InitializeNumericExtensions(RawEnvironment* env)
{
    auto& theEnv = Environment::fromRaw(env);
    theEnv.addFunction("midpoint", "ld", 2, 2, "ld", midpointOperation, "midpointOperation");
}
namespace 
{
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
void
midpointOperation(UDF_ARGS__) noexcept
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
} // end namespace 
} // end namespace Electron
