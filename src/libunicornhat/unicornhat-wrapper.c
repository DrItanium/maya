#define _GNU_SOURCE
/*
maya
Copyright (c) 2012-2015, Joshua Scoggins 
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "clips.h"
#include "libmaya.h"
#include "ws2812-RPi.h"

#if UNICORNHAT
static void ShowPixels(void* theEnv);
static void ClearUnicornhat(void* theEnv);
static double GetBrightness(void* theEnv);
static int SetBrightness(void* theEnv);
static int SetPixelColor(void* theEnv);
static void GetPixelColor(void* theEnv, DATA_OBJECT_PTR ret);
static int NumberOfPixels(void* theEnv);

void UnicornhatInterfaceDefinitions(void* theEnv) {
	init(64);
	initHardware();
	setBrightness(0.2);
	EnvDefineFunction2(theEnv, "unicornhat:number-of-pixels", 'i', PTIEF NumberOfPixels, "NumberOfPixels", "00a");
	EnvDefineFunction2(theEnv, "unicornhat:show", 'v', PTIEF ShowPixels, "ShowPixels", "00a");
	EnvDefineFunction2(theEnv, "unicornhat:clear", 'v', PTIEF ClearUnicornhat, "ClearUnicornhat", "00a");
	EnvDefineFunction2(theEnv, "unicornhat:get-brightness", 'd', PTIEF GetBrightness, "GetBrightness", "00a");
	EnvDefineFunction2(theEnv, "unicornhat:set-brightness", 'b', PTIEF SetBrightness, "SetBrightness", "11dd");
	EnvDefineFunction2(theEnv, "unicornhat:set-pixel-color", 'b', PTIEF SetPixelColor, "SetPixelColor", "44iiiii");
	EnvDefineFunction2(theEnv, "unicornhat:get-pixel-color", 'u', PTIEF GetPixelColor, "GetPixelColor", "11ii");
}
int NumberOfPixels(void* theEnv) {
	return numPixels();
}
void ShowPixels(void* theEnv) {
	show();
}
void ClearUnicornhat(void* theEnv) {
	clear();
}

double GetBrightness(void* theEnv) {
	return getBrightness();
}

int SetBrightness(void* theEnv) {
	double value;
	value = EnvRtnDouble(theEnv, 1);
	if (value < 0.0) {
		EnvPrintRouter(theEnv,WERROR,"Brightness can't be less than zero!\n");
		return FALSE;
	} else if (value > 1.0) {
		EnvPrintRouter(theEnv,WERROR,"Brightness can't be greater than one!\n");
		return FALSE;
	} else {
		return setBrightness(value);
	}
}

int SetPixelColor(void* theEnv) {
	long long pixel, red, green, blue;
	unsigned char r, g, b;
	pixel = EnvRtnLong(theEnv, 1);
	if (pixel < 0 || pixel >= numPixels()) {
		EnvPrintRouter(theEnv,WERROR,"Pixel index is not in range!\n");
		return FALSE;
	}
	red = EnvRtnLong(theEnv, 2);
	if (red < 0 || red > 255) {
		EnvPrintRouter(theEnv, WERROR, "Expected a red intensity value between zero and 255");
		return FALSE;
	}
	green = EnvRtnLong(theEnv, 3);
	if (green < 0 || green > 255) {
		EnvPrintRouter(theEnv, WERROR, "Expected a green intensity value between zero and 255");
		return FALSE;
	}
	blue = EnvRtnLong(theEnv, 4);
	if (blue < 0 || blue > 255) {
		EnvPrintRouter(theEnv, WERROR, "Expected a blue intensity value between zero and 255");
		return FALSE;
	}
	
	r = (unsigned char)red;
	g = (unsigned char)green;
	b = (unsigned char)blue;
	return setPixelColor(pixel, r, g, b);
}

void GetPixelColor(void* theEnv, DATA_OBJECT_PTR ret) {
	Color_t color;
	void* multifield;
	long long index;
	index = EnvRtnLong(theEnv, 1);
	if (index < 0 || index >= numPixels()) {
		EnvPrintRouter(theEnv,WERROR,"Pixel index is not in range!\n");
		SetpType(ret, SYMBOL);
		SetpValue(ret, EnvFalseSymbol(theEnv));
		return;
	}
	color = getPixelColor(index);
	multifield = EnvCreateMultifield(theEnv, 3);

	SetMFType(multifield,1,INTEGER);
	SetMFValue(multifield, 1, EnvAddLong(theEnv, color.r));
	SetMFType(multifield,2,INTEGER);
	SetMFValue(multifield, 2, EnvAddLong(theEnv, color.g));
	SetMFType(multifield,3,INTEGER);
	SetMFValue(multifield, 3, EnvAddLong(theEnv, color.b));
	SetpType(ret, MULTIFIELD);
	SetpValue(ret, multifield);
	SetpDOBegin(ret, 1);
	SetpDOEnd(ret, 3);
	return;
}




#endif /* UNICORNHAT */

