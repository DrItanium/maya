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
#include <list>
extern "C" {
	#include "clips.h"
}
namespace maya {

	using UserDefinedFunction = void (*)(UDFContext*, CLIPSValue*);
	enum class ReturnType : int {
		ExternalAddress = 'a',
		Boolean = 'b',
		Character = 'c',
		DoubleFloat = 'd',
		SingleFloat = 'f',
		Int64 = 'g',
		Int32 = 'i',
		SymbolStringOrInstanceName = 'j',
		SymbolOrString = 'k',
		LongInteger = 'l',
		Multifield = 'm',
		Number = 'n',
		InstanceName = 'o',
		String = 's',
		Any = 'u',
		Void = 'v',
		Symbol = 'w',
		InstanceAddress = 'x',
		DefTemplate = 'y',
		ReturnTypeBitsDefined = 'z',
	};

	enum class CheckModifiers : int {
		Exactly = EXACTLY,
		AtLeast = AT_LEAST,
		NoMoreThan = NO_MORE_THAN,
		Range = RANGE,
	};

	enum class BasicTypes : int {
		Float = FLOAT,
		Integer = INTEGER,
		Symbol = SYMBOL,
		String = STRING,
		Multifield = MULTIFIELD,
		ExternalAddress = EXTERNAL_ADDRESS,
		FactAddress = FACT_ADDRESS,
		InstanceAddress = INSTANCE_ADDRESS,
		InstanceName = INSTANCE_NAME,
		Void = RVOID,

		IntegerOrFloat = INTEGER_OR_FLOAT,
		SymbolOrString = SYMBOL_OR_STRING,
		InstanceOrInstanceName = INSTANCE_OR_INSTANCE_NAME,
	};

	enum class BitTypes : unsigned int {
		Float = FLOAT_TYPE,
		Integer = INTEGER_TYPE,
		Symbol = SYMBOL_TYPE,
		String = STRING_TYPE,
		Multifield = MULTIFIELD_TYPE,
		ExternalAddress = EXTERNAL_ADDRESS_TYPE,
		FactAddress = FACT_ADDRESS_TYPE,
		InstanceAddress = INSTANCE_ADDRESS_TYPE,
		InstanceName = INSTANCE_NAME_TYPE, 
		Void = VOID_TYPE,
		Boolean = BOOLEAN_TYPE,
		Number = NUMBER_TYPES,
		Lexeme = LEXEME_TYPES,
		Address = ADDRESS_TYPES,
		Instance = INSTANCE_TYPES,
		Singlefield = SINGLEFIELD_TYPES,
		Any = ANY_TYPE,
	};
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
	class Instance;
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
			void* addSymbol(T&& sym) {
				return convertToSymbol(this, std::forward<T>(sym));
			}

			void* addNumber(CLIPSInteger number);
			void* addNumber(CLIPSFloat number);
			template<typename T>
			void* addNumber(T&& number) {
				return ::EnvAddLong(_env, number);
			}

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
			void decode(CLIPSValue* dobj, T&& value) {
				decodeData(dobj, std::forward<T>(value));
			}

			template<typename T>
			void encode(CLIPSValue* dobj, T&& value) {
				encodeData(dobj, std::forward<T>(value));
			}

			void encodeSymbol(CLIPSValue* dobj, const std::string& str);
			void encodeSymbol(CLIPSValue* dobj, CLIPSString str);
			void decodeSymbol(CLIPSValue* dobj, std::string& str);

			Instance makeInstance(CLIPSString str);
			Instance makeInstance(const std::string& setup);

			void installExpression(EXPRESSION* expr);
			void deinstallExpression(EXPRESSION* expr);
			void evaluateExpression(EXPRESSION* expr, DATA_OBJECT* ret);
			void reclaimExpressionList(EXPRESSION* expr);

			EXPRESSION* generateConstantExpression(uint16 type, void* value);

			bool generateFunctionExpression(const std::string& name, FUNCTION_REFERENCE* ref);

			void call(const std::string& function);
			void call(const std::string& function, CLIPSValuePtr ref);

			template<typename T>
			void call(const std::string& function, CLIPSValuePtr ret, T arg0) {
				FunctionBuilder fb(this);
				fb.setFunctionReference(function);
				fb.addArgument(arg0);
				fb.invoke(ret);
			}

			template<typename T, typename K>
			void call(const std::string& function, CLIPSValuePtr ret, T arg0, K arg1) {
				FunctionBuilder fb(this);
				fb.setFunctionReference(function);
				fb.addArgument(arg0);
				fb.addArgument(arg1);
				fb.invoke(ret);
			}

			template<typename ... Args>
			void call(const std::string& function, CLIPSValuePtr ret, Args ... args) {
				FunctionBuilder fb(this);
				fb.setFunctionReference(function);
				fb.addArgument(args...);
				fb.invoke(ret);
			}

			template<typename R, typename ... Args>
			void call(const std::string& function, R& ret, Args ... args) {
				CLIPSValue r;
				call(function, &r, args...);
				decode(&r,  ret);
			}

			void encode(CLIPSValuePtr dobj, std::function<void(Environment*, CLIPSValuePtr)> fn);
			void decode(CLIPSValuePtr dobj, std::function<void(Environment*, CLIPSValuePtr)> fn);

			void addUserDefinedFunction(const std::string& name, const std::string& returnType, UserDefinedFunction func, const std::string& nativeFunctionName, int minArgs, int maxArgs, const std::string& restrictions, void* context = nullptr);
			void addUserDefinedFunction(const std::string& name, ReturnType returnType, UserDefinedFunction func, const std::string& nativeFunctionName, int minArgs, int maxArgs, const std::string& restrictions, void* context = nullptr);
			void addUserDefinedBooleanFunction(const std::string& name, UserDefinedFunction func, const std::string& nativeFunctionName, int minArgs, int maxArgs, const std::string& restrictions, void* context = nullptr);

		private:
			void* _env;
			bool destroy;
	};

	class Instance {
		public:
			Instance(Environment* env, void* instancePtr);
			virtual ~Instance();
			bool setSlot(const char* slotName, CLIPSValue* value);
			bool setSlot(const std::string& slotName, CLIPSValue* value);
			void getSlot(const char* slotName, CLIPSValue* ret);
			void getSlot(const std::string& slotName, CLIPSValue* ret);
			bool unmake();
			template<typename T>
			void getSlot(const char* slotName, T&& value) {
				CLIPSValue ret;
				getSlot(slotName, &ret);
				_env->decode(&ret, std::forward<T>(value));
			}

			template<typename T>
			void getSlot(const std::string& slotName, T&& value) {
				CLIPSValue ret;
				getSlot(slotName, &ret);
				_env->decode(&ret, std::forward<T>(value));
			}

			template<typename T>
			bool setSlot(const std::string& slotName, T&& value) {
				CLIPSValue input;
				_env->encode(&input, std::forward<T>(value));
				return setSlot(slotName, &input);
			}
			template<typename T>
			bool setSlot(const char* slotName, T&& value) {
				CLIPSValue input;
				_env->encode(&input, std::forward<T>(value));
				return setSlot(slotName, &input);
			}
		private:
			Environment* _env;
			void* _instancePtr;
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

