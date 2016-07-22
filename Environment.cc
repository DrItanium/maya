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

#include <functional>
#include <utility>

namespace maya {
	Environment::Environment() : destroy(true) {
		_env = ::CreateEnvironment();
		/// @todo perform error checking
	}

	Environment::Environment(void* env) : _env(env), destroy(false) { }

	Environment::~Environment() {
		if (destroy) {
			::DestroyEnvironment(_env);
			/// @todo error checking
			_env = 0;
		}
	}

	void* 
	Environment::addSymbol(const char* str) {
		return ::EnvAddSymbol(_env, str);
	}

	void*
	Environment::addSymbol(const std::string& str) {
		return ::EnvAddSymbol(_env, str.c_str());
	}

	void*
	Environment::addSymbol(bool value) {
		if (value) {
			return ::EnvTrueSymbol(_env);
		} else {
			return ::EnvFalseSymbol(_env);
		}
	}

	void*
	Environment::addNumber(int8 number) {
		return ::EnvAddLong(_env, number);
	}

	void*
	Environment::addNumber(uint8 number) {
		return ::EnvAddLong(_env, number);
	}

	void*
	Environment::addNumber(int16 number) {
		return ::EnvAddLong(_env, number);
	}

	void*
	Environment::addNumber(uint16 number) {
		return ::EnvAddLong(_env, number);
	}

	void*
	Environment::addNumber(int32 number) {
		return ::EnvAddLong(_env, number);
	}

	void*
	Environment::addNumber(uint32 number) {
		return ::EnvAddLong(_env, number);
	}

	void*
	Environment::addNumber(int64 number) {
		return ::EnvAddLong(_env, number);
	}

	void*
	Environment::addNumber(uint64 number) {
		return ::EnvAddLong(_env, int64(number));
	}

	void*
	Environment::addNumber(float number) {
		return ::EnvAddDouble(_env, number);
	}

	void*
	Environment::addNumber(double number) {
		return ::EnvAddDouble(_env, number);
	}

	void*
	Environment::addNumber(long double number) {
		return ::EnvAddDouble(_env, double(number));
	}

	bool
	Environment::watch(const char* target) {
		return ::EnvWatch(_env, target);
	}

	bool
	Environment::watch(const std::string& target) {
		return ::EnvWatch(_env, target.c_str());
	}

	int64
	Environment::run(int64 numRules) {
		return ::EnvRun(_env, numRules);
	}

	void
	Environment::reset() {
		::EnvReset(_env);
	}

	void
	Environment::clear() {
		::EnvClear(_env);
	}

	int
	Environment::loadFile(const char* path) {
		return ::EnvLoad(_env, path);
	}

	int 
	Environment::loadFile(const std::string& path) {
		return ::EnvLoad(_env, path.c_str());
	}

	bool
	Environment::batchFile(const char* path) {
		return ::EnvBatchStar(_env, path);
	}

	bool
	Environment::batchFile(const std::string& path) {
		return ::EnvBatchStar(_env, path.c_str());
	}

	void
	Environment::applyToFunction(std::function<void(void*)> fn) {
		fn(_env);
	}

	bool
	Environment::unwatch(const char* target) {
		return ::EnvUnwatch(_env, target);
	}

	bool
	Environment::unwatch(const std::string& target) {
		return ::EnvUnwatch(_env, target.c_str());
	}

	void*
	Environment::assertFact(const char* str) {
		return ::EnvAssertString(_env, str);
	}

	void*
	Environment::assertFact(const std::string& str) {
		return ::EnvAssertString(_env, str.c_str());
	}

	bool
	Environment::eval(const char* str, CLIPSValue* obj) {
		return ::EnvEval(_env, str, obj);
	}

	bool
	Environment::eval(const std::string& str, CLIPSValue* obj) {
		return ::EnvEval(_env, str.c_str(), obj);
	}

	void
	Environment::halt() {
		::EnvHalt(_env);
	}

	bool
	Environment::build(const char* str) {
		return ::EnvBuild(_env, str);
	}

	bool
	Environment::build(const std::string& str) {
		return ::EnvBuild(_env, str.c_str());
	}



	void decodeData( CLIPSValue* dobj, CLIPSInteger& value) { 
		value = mCVToInteger(dobj); 
	}

	void encodeData( CLIPSValue* dobj, CLIPSInteger& value) { 
		mCVSetInteger(dobj, value); 
	}

	void decodeData( CLIPSValue* dobj, CLIPSFloat& value) {
		value = mCVToFloat(dobj);
	}

	void
	encodeData( CLIPSValue* dobj, CLIPSFloat& value) { 
		mCVSetFloat(dobj, value); 
	}

	void
	decodeData( CLIPSValue* dobj, bool& value) {
		value = mCVIsTrueSymbol(dobj);
	}

	void
	encodeData( CLIPSValue* dobj, bool& value) {
		mCVSetBoolean(dobj, value);
	}

	void
	decodeData( CLIPSValue* dobj, std::string & str) {
		str = mCVToString(dobj);
	}

	void
	encodeData( CLIPSValue* dobj, const std::string& val) {
		mCVSetString(dobj, val.c_str());
	}

	void 
	Environment::encodeSymbol(CLIPSValue* dobj, const std::string& str) {
		mCVSetSymbol(dobj, str.c_str());
	}

	void 
	Environment::encodeSymbol(CLIPSValue* dobj, CLIPSString str) {
		mCVSetSymbol(dobj, str);
	}

	void 
	Environment::decodeSymbol(CLIPSValue* dobj, std::string& str) {
		str = mCVToString(dobj);
	}

	void
	decodeData( CLIPSValue* dobj, float& value) {
		value = mCVToFloat(dobj);
	}

	void
	encodeData( CLIPSValue* dobj, float& value) {
		mCVSetFloat(dobj, value);
	}

	void
	encodeData( CLIPSValue* dobj, CLIPSString value) {
		mCVSetString(dobj, value);
	}

	Instance
	Environment::makeInstance(CLIPSString str) {
		return Instance(this, ::EnvMakeInstance(_env, str));
	}

	Instance
	Environment::makeInstance(const std::string& setup) {
		return Instance(this, ::EnvMakeInstance(_env, setup.c_str()));
	}

	void
	Environment::installExpression(EXPRESSION* expr) {
		::ExpressionInstall(_env, expr);
	}

	void
	Environment::deinstallExpression(EXPRESSION* expr) {
		::ExpressionDeinstall(_env, expr);
	}

	void
	Environment::evaluateExpression(EXPRESSION* expr, CLIPSValuePtr ret) {
		::EvaluateExpression(_env, expr, ret);
	}

	void
	Environment::reclaimExpressionList(EXPRESSION* expr) {
		::ReturnExpression(_env, expr);
	}

	EXPRESSION*
	Environment::generateConstantExpression(uint16 type, void* value) {
		return ::GenConstant(_env, type, value);
	}

	bool
	Environment::generateFunctionExpression(const std::string& name, FUNCTION_REFERENCE* ref) {
		return ::GetFunctionReference(_env, name.c_str(), ref);
	}

	void*
	Environment::addNumber(CLIPSInteger number) {
		return ::EnvAddLong(_env, number);
	}

	void 
	Environment::call(const std::string& function) {
		CLIPSValue dontCare;
		call(function, &dontCare);
	}

	void 
	Environment::call(const std::string& function, CLIPSValuePtr ref) {
		FunctionBuilder fb(this);
		fb.setFunctionReference(function);
		fb.invoke(ref);
	}

	// end Environment
	// begin FunctionBuilder
	FunctionBuilder::FunctionBuilder(Environment* env) : _env(env), curr(nullptr), functionReferenceSet(false) { }

	FunctionBuilder::~FunctionBuilder() { 
		_env->deinstallExpression(&_ref);
		_env->reclaimExpressionList(_ref.argList);
		_ref.argList = nullptr;
		curr = nullptr;
		_env = nullptr;
	}

	void 
		FunctionBuilder::setFunctionReference(const std::string& func) {
			_env->generateFunctionExpression(func, &_ref);
			functionReferenceSet = true;
		}

	void
	FunctionBuilder::installArgument(uint16 type, void* value) {
		if (!functionReferenceSet) {
			throw std::runtime_error("Attempted to build an argument list before setting the function!");
		}
		auto tmp = _env->generateConstantExpression(type, value);
		_env->installExpression(tmp);
		if (_ref.argList == nullptr) {
			_ref.argList = tmp;
			curr = tmp;
		} else {
			curr->nextArg = tmp;
			curr = tmp;
		}
	}

	void
	FunctionBuilder::addArgument(CLIPSString chars) {
		installArgument(STRING, _env->addSymbol(chars));
	}

	void
	FunctionBuilder::addArgument(const std::string& str) {
		installArgument(STRING, _env->addSymbol(str));
	}

	void
	FunctionBuilder::addArgument(CLIPSInteger value) {
		installArgument(INTEGER, _env->addNumber(value));
	}

	void
	FunctionBuilder::addArgument(bool value) {
		installArgument(SYMBOL, value ? (void*)::EnvTrueSymbol(_env->getRawEnvironment()) : (void*)::EnvFalseSymbol(_env->getRawEnvironment()));
	}

	void
	FunctionBuilder::addArgument(CLIPSFloat value) {
		installArgument(FLOAT, _env->addNumber(value));
	}

	void
	FunctionBuilder::addArgument(std::function<void(FunctionBuilder*)> fn) {
		fn(this);
	}

	void
	FunctionBuilder::invoke(CLIPSValuePtr ret) {
		_env->evaluateExpression(&_ref, ret);
	}

	// end FunctionBuilder
	// begin Instance
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
	// end Instance

}
