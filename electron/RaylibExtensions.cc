/**
 * @file
 * Raylib extension features
 * @copyright
 * Copyright (c) 2013-2025 Joshua Scoggins
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
#include "electron/RaylibExtensions.h"

extern "C" {
#include "clips/clips.h"
}
// GetFileName in Raylib conflicts with GetFileName inside of clips!
#include <raylib.h>
namespace Electron
{

void raylibInitializeWindow(UDF_ARGS__);
void raylibCloseWindow(UDF_ARGS__);

void
InitializeRaylibExtensions(RawEnvironment* env)
{
    auto& theEnv = Environment::fromRaw(env);
    theEnv.addFunction("init-window", "b", 3, 3, "syl;l;l;sy", raylibInitializeWindow, "raylibInitializeWindow");
    theEnv.addFunction("close-window", "b", 0, 0, "*", raylibCloseWindow, "raylibCloseWindow");
}

void
raylibInitializeWindow(UDF_ARGS__) {
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    UDFValue arg1;
    UDFValue arg2;
    if (!theEnv.firstArgument(context, ArgumentBits::Integer, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Integer, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg2)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    InitWindow(arg0.integerValue->contents, arg1.integerValue->contents, arg2.lexemeValue->contents);
    out->lexemeValue = theEnv.trueSymbol();
}

void
raylibCloseWindow(UDF_ARGS__) {
    auto& theEnv = Environment::fromRaw(env);
    CloseWindow();
    out->lexemeValue = theEnv.trueSymbol();
}

} // end namespace Electron
