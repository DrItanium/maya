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
#include <string>
namespace maya 
{

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
		 * Add the given std::string to the symbol table and return a handle
		 */
		void* addSymbol(const std::string& value);
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
	private:
		void* _env;
		bool destroy;
};

}

#endif // end MAYA_CPPLIB_ENVIRONMENT_H__

