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
#include <stdbool.h>
#if UNICORNHAT
#include "ws2811.h"
#include "board_info.h"

#define TARGET_FREQ    WS2811_TARGET_FREQ
#define GPIO_PIN       18
#define DMA            5

#define WIDTH          8
#define HEIGHT         8
#define LED_COUNT      (WIDTH * HEIGHT)
ws2811_t ledstring =
{
    .freq = TARGET_FREQ,
    .dmanum = DMA,
    .channel =
    {
        [0] =
        {
            .gpionum    = GPIO_PIN,
            .count      = LED_COUNT,
            .invert     = 0,
            .brightness = 55,
        }
    }
};

void setBrightness(int b)
{
    ledstring.channel[0].brightness = b;
    return;
}

void setPixelColorRGB(int pixel, int r, int g, int b)
{
    ledstring.channel[0].leds[pixel] = (r << 16) | (g << 8) | b;
    return;
}

void clearLEDBuffer(void){
    int i;
    for(i=0; i<LED_COUNT;i++){
        setPixelColorRGB(i,0,0,0);
    }
}

/*
  Remap an x/y coordinate to a pixel index
*/
int getPixelPosition(int x, int y){

    int map[8][8] = {
        {7 ,6 ,5 ,4 ,3 ,2 ,1 ,0 },
        {8 ,9 ,10,11,12,13,14,15},
        {23,22,21,20,19,18,17,16},
        {24,25,26,27,28,29,30,31},
        {39,38,37,36,35,34,33,32},
        {40,41,42,43,44,45,46,47},
        {55,54,53,52,51,50,49,48},
        {56,57,58,59,60,61,62,63}
    };

    return map[x][y];
}

void show(){
    ws2811_render(&ledstring);
}
static void ShowPixels(void* theEnv);
static int GetBrightness(void* theEnv);
static int SetBrightness(void* theEnv);
static int SetPixelColor(void* theEnv);
static void GetPixelColor(void* theEnv, DATA_OBJECT_PTR ret);
static int NumberOfPixels(void* theEnv);
static void ShutdownUnicornhat(void* theEnv);
static int InitializeUnicornhat(void* theEnv);
static void Wait(void* theEnv);

void UnicornhatInterfaceDefinitions(void* theEnv) {
	if (!InitializeUnicornhat(theEnv))  {
		EnvPrintRouter(theEnv, WERROR, "Couldn't initialize the unicornhat, disabling functionality!");
		EnvPrintRouter(theEnv, WERROR, "\n");
	} else {
		EnvDefineFunction2(theEnv, "unicornhat:number-of-pixels", 'i', PTIEF NumberOfPixels, "NumberOfPixels", "00a");
		EnvDefineFunction2(theEnv, "unicornhat:show", 'v', PTIEF ShowPixels, "ShowPixels", "00a");
		EnvDefineFunction2(theEnv, "unicornhat:get-brightness", 'i', PTIEF GetBrightness, "GetBrightness", "00a");
		EnvDefineFunction2(theEnv, "unicornhat:set-brightness", 'b', PTIEF SetBrightness, "SetBrightness", "11ii");
		EnvDefineFunction2(theEnv, "unicornhat:set-pixel-color", 'b', PTIEF SetPixelColor, "SetPixelColor", "44iiiii");
		EnvDefineFunction2(theEnv, "unicornhat:get-pixel-color", 'u', PTIEF GetPixelColor, "GetPixelColor", "11ii");
		EnvDefineFunction2(theEnv, "unicornhat:shutdown", 'v', PTIEF ShutdownUnicornhat, "ShutdownUnicornhat", "00a");
		EnvDefineFunction2(theEnv, "wait", 'v', PTIEF Wait, "Wait", "11i");
	}
}
void Wait(void* theEnv) {
	long long duration;
	duration = EnvRtnLong(theEnv, 1);
	if (duration >= 0) {
		usleep(duration);
	}
}
int InitializeUnicornhat(void* theEnv) {
    if (board_info_init() < 0)
    {
        return false;
    }
    if(ws2811_init(&ledstring))
    {
        return false;
    }

    clearLEDBuffer();
	return true;
}
void ShutdownUnicornhat(void* theEnv) {
    int i;
    for (i = 0; i < 64; i++){
        setPixelColorRGB(i,0,0,0);
    }
    ws2811_render(&ledstring);
    ws2811_fini(&ledstring);
}
int NumberOfPixels(void* theEnv) {
	return LED_COUNT;
}
void ShowPixels(void* theEnv) {
	show();
}

int GetBrightness(void* theEnv) {
	return ledstring.channel[0].brightness;
}

int SetBrightness(void* theEnv) {
	long long value;
	value = EnvRtnLong(theEnv, 1);
	if (value < 0) {
		EnvPrintRouter(theEnv,WERROR,"Brightness can't be less than zero!\n");
		return FALSE;
	} else if (value > 255) {
		EnvPrintRouter(theEnv,WERROR,"Brightness can't be greater than 255!\n");
		return FALSE;
	} else {
		setBrightness((unsigned char)value);
		return TRUE;
	}
}

int SetPixelColor(void* theEnv) {
	long long pixel, red, green, blue;
    int result;
	unsigned char r, g, b;
   // char* container;
	pixel = EnvRtnLong(theEnv, 1);
	if (pixel < 0 || pixel >= LED_COUNT) {
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
    setPixelColorRGB(pixel, r, g, b);
	return TRUE;
}

void GetPixelColor(void* theEnv, DATA_OBJECT_PTR ret) {
	ws2811_led_t color;
	void* multifield;
	long long index;
	index = EnvRtnLong(theEnv, 1);
	if (index < 0 || index >= LED_COUNT) {
		EnvPrintRouter(theEnv,WERROR,"Pixel index is not in range!\n");
		SetpType(ret, SYMBOL);
		SetpValue(ret, EnvFalseSymbol(theEnv));
		return;
	}
	color = ledstring.channel[0].leds[index];
	multifield = EnvCreateMultifield(theEnv, 3);

    //ledstring.channel[0].leds[pixel] = (r << 16) | (g << 8) | b;
	SetMFType(multifield,1,INTEGER);
	SetMFValue(multifield, 1, EnvAddLong(theEnv, (color & 0x00FF0000) >> 16));
	SetMFType(multifield,2,INTEGER);
	SetMFValue(multifield, 2, EnvAddLong(theEnv, (color & 0x0000FF00) >> 8));
	SetMFType(multifield,3,INTEGER);
	SetMFValue(multifield, 3, EnvAddLong(theEnv, (color & 0x000000FF)));
	SetpType(ret, MULTIFIELD);
	SetpValue(ret, multifield);
	SetpDOBegin(ret, 1);
	SetpDOEnd(ret, 3);
	return;
}




#endif /* UNICORNHAT */

