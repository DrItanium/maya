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

void MayaDefinitions(void* theEnv) {
	/* Insert declarations here */
#if FILE_SYSTEM_ROOTING
	DefineFSOverrideFunctions(theEnv);
#endif
#if ARCHITECTURE_IDENTIFICATION
   ArchitectureDetectionFunctionDefinitions(theEnv);
#endif /* ARCHITECTURE_IDENTIFICATION */

#if BINARY_LOGICAL_OPERATIONS
   BinaryOperationsFunctionDefinitions(theEnv);
#endif /* BINARY_LOGICAL_OPERATIONS */
#if UNICORNHAT
   UnicornhatInterfaceDefinitions(theEnv);
#endif /* UNICORNHAT */
}

void MayaOptions(void* theEnv) {
#if MAYA_EXTENSIONS
EnvPrintRouter(theEnv,WDISPLAY,"Architecture identification is ");
#if ARCHITECTURE_IDENTIFICATION 
  EnvPrintRouter(theEnv,WDISPLAY,"ON\n");
#else
  EnvPrintRouter(theEnv,WDISPLAY,"OFF\n");
#endif
EnvPrintRouter(theEnv,WDISPLAY, "Binary logical operators are ");
#if BINARY_LOGICAL_OPERATIONS 
  EnvPrintRouter(theEnv,WDISPLAY,"ON\n");
#else
  EnvPrintRouter(theEnv,WDISPLAY,"OFF\n");
#endif
EnvPrintRouter(theEnv,WDISPLAY, "File system rooting is ");
#if FILE_SYSTEM_ROOTING 
  EnvPrintRouter(theEnv,WDISPLAY,"ON\n");
#else
  EnvPrintRouter(theEnv,WDISPLAY,"OFF\n");
#endif
EnvPrintRouter(theEnv,WDISPLAY, "Unicornhat interface functions are ");
#if UNICORNHAT
  EnvPrintRouter(theEnv,WDISPLAY,"ON\n");
#else
  EnvPrintRouter(theEnv,WDISPLAY,"OFF\n");
#endif
	
#endif /* MAYA_EXTENSIONS */
}
