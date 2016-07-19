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
#ifndef MAYA_CPPLIB_ENVIRONMENT_H__
#define MAYA_CPPLIB_ENVIRONMENT_H__
#include "types.h"
#include <functional>
#include <string>
extern "C" {
	#include "clips.h"
}
namespace maya {
class Instance;
class FunctionBuilder;
class Environment {

	public:
		/**
		 * Build a new environment
		 */
		Environment();
		/**
		 * Wrap an already existing raw environment pointer, this constructor
		 * will not destroy the pointer when finished
		 * @param env the raw environment pointer to wrap
		 */
		Environment(void* env);
		/**
		 * Destroy the given environment if so directed to do so
		 */
		virtual ~Environment();
		/// Returns the raw clips pointer
		void* getRawEnvironment() { return _env; }
		/**
		 * Add the given const char * to the symbol table and return a handle
		 * @param the symbol/string to add to the symbol table
		 * @returns the handle to the entry in the symbol table
		 */
		void* addSymbol(const char* str);
		/**
		 * Add the given string to the symbol table and return a handle
		 * @param the symbol/string to add to the symbol table
		 * @returns the handle to the entry in the symbol table
		 */
		void* addSymbol(const std::string& str);
		/**
		 * Convert the given bool value to it's corresponding symbol
		 * representation
		 * @param value the bool value to convert
		 */
		void* addSymbol(bool value);
		/**
		 * Generic version of the add symbol method
		 * @param value the value to convert to a symbol
		 */
		template<typename T>
		void* addSymbol(T&& value);

		void* addNumber(int8 number);
		void* addNumber(uint8 number);
		void* addNumber(int16 number);
		void* addNumber(uint16 number);
		void* addNumber(int32 number);
		void* addNumber(uint32 number);
		void* addNumber(int64 number);
		void* addNumber(uint64 number);
		void* addNumber(float number);
		void* addNumber(double number);
		void* addNumber(long double number);
		template<typename T>
		void* addNumber(T&& value);

		bool watch(const char* target);
		bool watch(const std::string& target);
		bool unwatch(const char* target);
		bool unwatch(const std::string& target);


		int64 run(int64 count = -1L);
		void reset();
		void clear();
		int loadFile(const char* path);
		int loadFile(const std::string& path);
		bool batchFile(const char* path);
		bool batchFile(const std::string& path);

		void applyToFunction(std::function<void(void*)> fn);
		void* assertFact(const char* str);
		void* assertFact(const std::string& str);
		bool eval(const char* str, CLIPSValue* dobj);
		bool eval(const std::string& str, CLIPSValue* dobj);
		void halt();
		bool build(const char* str);
		bool build(const std::string& str);

		/**
		 * Use the given CLIPSValue to populate the second argument
		 * @param dobj the data object containing the data to extract
		 */
		template<typename T>
		void decode(CLIPSValue* dobj, T&& value);
		template<typename T>
		void encode(CLIPSValue* dobj, T&& value);

		void encodeSymbol(CLIPSValue* dobj, const std::string& str);
		void encodeSymbol(CLIPSValue* dobj, CLIPSString str);
		void decodeSymbol(CLIPSValue* dobj, std::string& str);

	private:
		void* _env;
		bool destroy;
};




void decodeData(CLIPSValue* dobj, CLIPSInteger& value);
void encodeData(CLIPSValue* dobj, CLIPSInteger& value);
void decodeData(CLIPSValue* dobj, CLIPSFloat& value);
void encodeData(CLIPSValue* dobj, CLIPSFloat& value);
void encodeData(CLIPSValue* dobj, CLIPSString value);
void decodeData(CLIPSValue* dobj, std::string& ret);
void encodeData(CLIPSValue* dobj, const std::string& val);
#define encodeDecodePair(type) \
	void decodeData(CLIPSValue* dobj, type & value); \
	void encodeData(CLIPSValue* dobj, type & value)
encodeDecodePair(bool);
encodeDecodePair(float);
#undef encodeDecodePair

}

#endif // end MAYA_CPPLIB_ENVIRONMENT_H__

