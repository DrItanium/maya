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

#include "Environment.h"
extern "C" {
	#include "clips.h"
}

#include <utility>

namespace maya 
{
	Environment::Environment() : destroy(true) 
	{
		_env = ::CreateEnvironment();
		/// @todo perform error checking
	}

	Environment::Environment(void* env) : _env(env), destroy(false) { }

	Environment::~Environment() 
	{
		if (destroy) {
			::DestroyEnvironment(_env);
			/// @todo error checking
			_env = 0;
		}
	}

	void* 
	Environment::addSymbol(const char* str)
	{
		return ::EnvAddSymbol(_env, str);
	}

	void*
	Environment::addSymbol(const std::string& str)
	{
		return ::EnvAddSymbol(_env, str.c_str());
	}

	void*
	Environment::addSymbol(bool value) 
	{
		if (value) {
			return ::EnvTrueSymbol(_env);
		} else {
			return ::EnvFalseSymbol(_env);
		}
	}


	template<typename T>
	void*
	Environment::addSymbol(T&& value) {
		return convertToSymbol(this, std::forward<T>(value));
	}
	
}
