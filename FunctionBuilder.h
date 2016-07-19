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
#ifndef MAYA_CPPLIB_FUNCTION_BUILDER_H__
#define MAYA_CPPLIB_FUNCTION_BUILDER_H__
#include <string>
#include <list>
#include <functional>
#include "types.h"
extern "C" {
	#include "clips.h"
}
namespace maya {
	class Environment;
	class FunctionBuilder{
		public:
			FunctionBuilder(Environment* env);
			virtual ~FunctionBuilder();
			void setFunctionReference(const std::string& func);
			void installArgument(uint16 type, void* value);
			void addArgument(CLIPSString chars);
			void addArgument(const std::string& str);
			void addArgument(CLIPSInteger value);
			void addArgument(bool value);
			void addArgument(CLIPSFloat value);
			void addArgument(std::function<void(FunctionBuilder*)> fn);
			void invoke(CLIPSValuePtr ret);
			template<typename T>
			void addArgument(const std::list<T>& list) {
				for (auto const& element : list) {
					addArgument(element);
				}
			}
			template<typename T>
			void addArgument(const std::initializer_list<T>& list) {
				for (auto const &element : list) {
					addArgument(element);
				}
			}

			template<typename T, typename ... Args>
			void addArgument(T a, Args ... b) {
				addArgument(a);
				addArgument(b...);
			}

			


		private:
			Environment* _env;
			FUNCTION_REFERENCE _ref;
			EXPRESSION* curr = nullptr;
			bool functionReferenceSet = false;
	};
}
#endif // MAYA_CPPLIB_FUNCTION_BUILDER_H__
