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

#include "FunctionBuilder.h"
#include "Environment.h"
namespace maya { 

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


}
