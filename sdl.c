// maya
// Copyright (c) 2012-2016, Joshua Scoggins
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#include "clips.h"
#include "mayasetup.h"
#include "sdl.h"
#if SDL_EXTENSIONS
#include <SDL2/SDL_cpuinfo.h>
#endif

#if !SDL_EXTENSIONS
void InstallSDLExtensions(void* theEnv) { }
#else
#define defun_header(title) \
	static void title(UDFContext*, CLIPSValue*)
defun_header(SystemRamCount);
defun_header(HasAVX);
defun_header(HasSSE42);
#undef defun_header
void InstallSDLExtensions(void* environment) {
	EnvAddUDF(environment, "get-system-ram-count", "i", SystemRamCount, "SystemRamCount", 0, 0, NULL, NULL);
	EnvAddUDF(environment, "system-has-avx", "b", HasAVX, "HasAVX", 0, 0, NULL, NULL);
	EnvAddUDF(environment, "system-has-sse42", "b", HasSSE42, "HasSSE42", 0, 0, NULL, NULL);
}
void SystemRamCount(UDFContext* context, CLIPSValue* ret) {
	mCVSetInteger(ret, SDL_GetSystemRAM());
}
#define boolFunction(title, function) \
	void title (UDFContext* context, CLIPSValue* ret) { \
		mCVSetBoolean(ret, (function)()); \
	}
boolFunction(HasAVX, SDL_HasAVX)
boolFunction(HasSSE42, SDL_HasSSE42)
boolFunction(HasSSE41, SDL_HasSSE41)
#undef boolFunction
#endif
