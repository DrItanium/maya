
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

#if SPAWN_OTHER_ENVIRONMENTS
static int clipsEnvironmentExternalAddressID;
static void PrintClipsEnvironmentAddress(void* env, char* logicalName, void* theValue);
static intBool DeallocateClipsEnvironment(void* env, void* theValue);
static void NewClipsEnvironment(void* env, DATA_OBJECT* retVal);
void EnvironmentSpawningFunctions(void* theEnv) {
	struct externalAddressType clipsEnvironment = {
		"clips-environment",
		PrintClipsEnvironmentAddress,
		PrintClipsEnvironmentAddress,
		DeallocateClipsEnvironment,
		NewClipsEnvironment,
		NULL,
	};
	clipsEnvironmentExternalAddressID = InstallExternalAddressType(theEnv, &clipsEnvironment);
}

void PrintClipsEnvironmentAddress(void* theEnv, char* logicalName, void* theValue) {
   char buffer[20];
   void* ptr;

   EnvPrintRouter(theEnv, logicalName, "<Pointer-Menu-");
   ptr = ValueToExternalAddress(theValue);
   if(ptr) {
      gensprintf(buffer, "%p", ptr);
   } else {
      gensprintf(buffer, "%p", theValue);
   }
   EnvPrintRouter(theEnv, logicalName, buffer);
   EnvPrintRouter(theEnv, logicalName, ">");
}

intBool DeallocateClipsEnvironment(void* theEnv, void* theValue) {
	if (theValue != NULL) {
		DestroyEnvironment(theValue);
	}
	return TRUE;
}

void NewClipsEnvironment(void* theEnv, DATA_OBJECT* retVal) {
	SetpType(retVal, EXTERNAL_ADDRESS);
	SetpValue(retVal, EnvAddExternalAddress(theEnv, CreateEnvironment(), clipsEnvironmentExternalAddressID));
}


#endif /* SPAWN_OTHER_ENVIRONMENTS */
