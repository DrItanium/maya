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
#ifndef MAYA_CPPLIB_INSTANCE_H__
#define MAYA_CPPLIB_INSTANCE_H__
#include <string>
extern "C" {
	#include "clips.h"
}
namespace maya {
	class Environment;
	class Instance {
		public:
			Instance(Environment* env, void* instancePtr);
			virtual ~Instance();
			bool setSlot(const char* slotName, DATA_OBJECT* value);
			bool setSlot(const std::string& slotName, DATA_OBJECT* value);
			void getSlot(const char* slotName, DATA_OBJECT* ret);
			void getSlot(const std::string& slotName, DATA_OBJECT* ret);
			bool unmake();
			template<typename T>
			bool setSlot(const char* slotName, T&& value);
			template<typename T>
			bool setSlot(const std::string& slotName, T&& value);
			template<typename T>
			void getSlot(const char* slotName, T&& value);
			template<typename T>
			void getSlot(const std::string& slotName, T&& value);
		private:
			Environment* _env;
			void* _instancePtr;
	};
}
#endif // MAYA_CPPLIB_INSTANCE_H__
