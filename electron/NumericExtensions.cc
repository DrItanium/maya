/**
 * @file
 * Implementation of the extensions to electron which provide extended numeric operations
 * @copyright
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
#include <boost/math/constants/constants.hpp>
#include <boost/math/special_functions/prime.hpp>

extern "C" {
#include "clips/clips.h"
}


namespace Electron
{
using UnderlyingFloat = decltype(CLIPSFloat::contents);
static void midpointOperation(UDF_ARGS__) noexcept;
static void lcmOperation(UDF_ARGS__) noexcept;
static void gcdOperation(UDF_ARGS__) noexcept;
template<UnderlyingFloat f>
static void genericFPConstantOperation(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    out->floatValue = theEnv.createFloat(f);
}

static void nthPrimeNumber(UDF_ARGS__) noexcept;

void
InitializeNumericExtensions(RawEnvironment* env)
{
    auto& theEnv = Environment::fromRaw(env);
    theEnv.addFunction("midpoint", "ld", 2, 2, "ld", midpointOperation, "midpointOperation");
    theEnv.addFunction("lcm", "l", 2, 2, "l", lcmOperation, "lcmOperation");
    theEnv.addFunction("gcd", "l", 2, 2, "l", gcdOperation, "gcdOperation");

#define DefConstantFull(name, fn) \
    theEnv.addFunction(name, "d", 0, 0, "sy", \
            genericFPConstantOperation<boost::math::constants:: fn <UnderlyingFloat>()>, \
            "genericFPConstantOperation<boost::math::constants::" #fn "<UnderlyingFloat>()>") 
#define DefConstant(fn) DefConstantFull( #fn , fn )
    DefConstant(tau);
    DefConstantFull("log-pi", log_pi);
    DefConstant(plastic);
    DefConstant(gauss);
    DefConstant(dottie);
#undef DefConstant
#undef DefConstantFull
    theEnv.addFunction("nth-prime", "ld", 1, 1, "l", 
            nthPrimeNumber,
            "nthPrimeNumber");

}
void
midpointOperation(UDF_ARGS__) noexcept
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0, arg1;
    if (!theEnv.firstArgument(context, ArgumentBits::Number, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Number, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (isFloat(arg0) || isFloat(arg1)) {
        auto a = CVCoerceToFloat(&arg0);
        auto b = CVCoerceToFloat(&arg1);
        out->floatValue = theEnv.createFloat(std::midpoint(a, b));
    } else {
        auto a = CVCoerceToInteger(&arg0);
        auto b = CVCoerceToInteger(&arg1);
        out->integerValue = theEnv.createInteger(std::midpoint(a, b));
    }
    // if both are integers then we stay as integers, if either is a float then we convert both to float
}

void
gcdOperation(UDF_ARGS__) noexcept
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0, arg1;
    if (!theEnv.firstArgument(context, ArgumentBits::Integer, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Integer, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    out->integerValue = theEnv.createInteger(
            std::gcd(arg0.integerValue->contents,
                     arg1.integerValue->contents));
}
void
lcmOperation(UDF_ARGS__) noexcept
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0, arg1;
    if (!theEnv.firstArgument(context, ArgumentBits::Integer, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Integer, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    out->integerValue = theEnv.createInteger(
            std::lcm(arg0.integerValue->contents,
                     arg1.integerValue->contents));
}

void
nthPrimeNumber(UDF_ARGS__) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    if (!theEnv.firstArgument(context, ArgumentBits::Integer, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (auto value = arg0.integerValue->contents; value > 0 && value <= boost::math::max_prime) {
        out->integerValue = theEnv.createInteger(boost::math::prime(value));
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}
} // end namespace Electron
