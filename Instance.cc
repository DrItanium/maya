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

#include "Instance.h"
#include "Environment.h"
namespace maya { 

Instance::Instance(Environment* env, void* instancePtr) : _env(env), _instancePtr(instancePtr) { }

Instance::~Instance() { }

bool
Instance::setSlot(const char* slotName, CLIPSValue* value) {
	/// @todo check and see if the set was successful
	return ::EnvDirectPutSlot(_env->getRawEnvironment(), _instancePtr, slotName, value);
}

bool
Instance::setSlot(const std::string& slotName, CLIPSValue* value) {
	return ::EnvDirectPutSlot(_env->getRawEnvironment(), _instancePtr, slotName.c_str(), value);
}

void
Instance::getSlot(const char* slotName, CLIPSValue* value) {
	::EnvDirectGetSlot(_env->getRawEnvironment(), _instancePtr, slotName, value);
}

void
Instance::getSlot(const std::string& slotName, CLIPSValue* value) {
	::EnvDirectGetSlot(_env->getRawEnvironment(), _instancePtr, slotName.c_str(), value);
}

bool
Instance::unmake() {
	bool result = ::EnvUnmakeInstance(_env->getRawEnvironment(), _instancePtr);
	if (result) {
		_instancePtr = 0;
	}
	return result;
	
}
template<typename T>
void
Instance::getSlot(const char* slotName, T&& value) {
	CLIPSValue ret;
	getSlot(slotName, &ret);
	_env->decode(&ret, std::forward<T>(value));
}

template<typename T>
void
Instance::getSlot(const std::string& slotName, T&& value) {
	CLIPSValue ret;
	getSlot(slotName, &ret);
	_env->decode(&ret, std::forward<T>(value));
}

template<typename T>
bool
Instance::setSlot(const std::string& slotName, T&& value) {
	CLIPSValue input;
	_env->encode(&input, std::forward<T>(value));
	return setSlot(slotName, &input);
}
template<typename T>
bool
Instance::setSlot(const char* slotName, T&& value) {
	CLIPSValue input;
	_env->encode(&input, std::forward<T>(value));
	return setSlot(slotName, &input);
}

}
