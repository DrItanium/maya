
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
static void PrintClipsEnvironmentAddress(void* env, const char* logicalName, void* theValue);
static intBool DeallocateClipsEnvironment(void* env, void* theValue);
static void NewClipsEnvironment(void* env, DATA_OBJECT* retVal);
static intBool CallClipsEnvironment(void* env, DATA_OBJECT* target, DATA_OBJECT* rv);
void EnvironmentSpawningFunctions(void* theEnv) {
	struct externalAddressType clipsEnvironment = {
		"clips-environment",
		PrintClipsEnvironmentAddress,
		PrintClipsEnvironmentAddress,
		DeallocateClipsEnvironment,
		NewClipsEnvironment,
		CallClipsEnvironment,
	};
	clipsEnvironmentExternalAddressID = InstallExternalAddressType(theEnv, &clipsEnvironment);
}

void PrintClipsEnvironmentAddress(void* theEnv, const char* logicalName, void* theValue) {
	char buffer[20];
	void* ptr;

	EnvPrintRouter(theEnv, logicalName, "<Pointer-Clips-Environment-");
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

intBool CallClipsEnvironment(void* theEnv, DATA_OBJECT* target, DATA_OBJECT* rv) {
	int numberOfArguments;
	DATA_OBJECT theValue, arg0;
	const char* methodName;
	int numRulesToFire;
	const char* path;
	void* otherEnv;
	if ((numberOfArguments = EnvArgCountCheck(theEnv,"call (with type clips-environment)",AT_LEAST,2)) == -1) 
	{ return FALSE; }

	/*================================================*/
	/* The clips-environment action must be a symbol. */
	/*================================================*/

	if (EnvArgTypeCheck(theEnv,"call (with type clips-environment)",2,SYMBOL,&theValue) == FALSE) 
	{ return FALSE; }
	otherEnv = DOPToExternalAddress(target);
	methodName = DOToString(theValue);
	if (strcmp(methodName, "run") == 0) {
		if (numberOfArguments > 3) {
			EnvPrintRouter(theEnv, WERROR, "ERROR: function run allows zero or one arguments, too many arguments provided!\n");
			return FALSE;
		} else if (numberOfArguments == 3) {
			if (EnvArgTypeCheck(theEnv, "call run (with type clips-environment)", 3, INTEGER, &arg0) == FALSE) {
				return FALSE;
			}
			numRulesToFire = DOToInteger(arg0);
		} else {
			numRulesToFire = -1;
		}
		EnvRun(otherEnv, numRulesToFire);
		return TRUE;
	} else if (strcmp(methodName, "batch*") == 0) {
		if (numberOfArguments != 3) {
			EnvPrintRouter(theEnv, WERROR, "ERROR: function batch* requires a path to load!\n");
			return FALSE;
		} else {
			if (EnvArgTypeCheck(theEnv, "call batch* (with type clips-environment)", 3, SYMBOL_OR_STRING, &arg0) == FALSE) {
				return FALSE;
			}
			path = DOToString(arg0);
			return EnvBatchStar(otherEnv, path);
		}
	} else if (strcmp(methodName, "build") == 0) {
		if (numberOfArguments != 3) {
			EnvPrintRouter(theEnv, WERROR, "ERROR: function build requires a string to build from!\n");
			return FALSE;
		} else {
			if (EnvArgTypeCheck(theEnv, "call build (with type clips-environment)", 3, SYMBOL_OR_STRING, &arg0) == FALSE) {
				return FALSE;
			}
			path = DOToString(arg0);
			return EnvBuild(otherEnv, path);
		}
	} else if (strcmp(methodName, "eval") == 0) {
		if (numberOfArguments != 3) {
			EnvPrintRouter(theEnv, WERROR, "ERROR: function eval requires a string to eval from!\n");
			return FALSE;
		} else {
			if (EnvArgTypeCheck(theEnv, "call eval (with type clips-environment)", 3, SYMBOL_OR_STRING, &arg0) == FALSE) {
				return FALSE;
			}
			path = DOToString(arg0);
			return EnvEval(otherEnv, path, rv);
		}
	} else {
		EnvPrintRouter(theEnv, WERROR, "Unknown function ");
		EnvPrintRouter(theEnv, WERROR, methodName);
		EnvPrintRouter(theEnv, WERROR, " was requested to be invoked!\n");
		return FALSE;
	}
}


#endif /* SPAWN_OTHER_ENVIRONMENTS */
