// maya
// Copyright (c) 2012-2018, Joshua Scoggins
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
extern "C" {
#include "clips.h"
}
#include "mayasetup.h"
#include "functional.h"
#include <functional>
#include <string>

#if FUNCTIONAL_EXTENSIONS
void MapFunction(Environment* env, clips::udf::Context* context, clips::udf::Value* ret);
void FilterFunction(Environment* env, clips::udf::Context* context, clips::udf::Value* ret);
void ExistsFunction(Environment* env, clips::udf::Context* context, clips::udf::Value* ret);
void NotExistsFunction(Environment* env, clips::udf::Context* context, clips::udf::Value* ret);
void FunctionError(Environment*, int, FunctionCallBuilderError, const std::string&) noexcept;
#endif



extern "C" void InstallFunctionalExtensions(Environment* theEnv) {
#if FUNCTIONAL_EXTENSIONS
    clips::udf::add(theEnv, "map$", "m", 1, UNBOUNDED, "*;y;*", MapFunction, "MapFunction");
    clips::udf::add(theEnv, "filter$", "m", 1, UNBOUNDED, "*;y;*", FilterFunction, "FilterFunction");
	clips::udf::add(theEnv, "exists$", "b", 1, UNBOUNDED, "*;y;*", ExistsFunction, "ExistsFunction");
	clips::udf::add(theEnv, "not-exists$", "b", 1, UNBOUNDED, "*;y;*", NotExistsFunction, "NotExistsFunction");
#endif
}

#if FUNCTIONAL_EXTENSIONS
void
FunctionError(Environment* theEnv, int code, FunctionCallBuilderError err, const std::string& func) noexcept {
	PrintErrorID(theEnv, "FUNCTIONAL", code, false);
	switch(err) {
		case FunctionCallBuilderError::FCBE_PROCESSING_ERROR:
			WriteString(theEnv, STDERR, "Error during evaluation of arguments!\n");
			break;
		case FunctionCallBuilderError::FCBE_ARGUMENT_TYPE_ERROR:
			WriteString(theEnv, STDERR, "Argument type check failed!\n");
			break;
		case FunctionCallBuilderError::FCBE_ARGUMENT_COUNT_ERROR:
			WriteString(theEnv, STDERR, "Argument count check failed!\n");
			break;
		case FunctionCallBuilderError::FCBE_FUNCTION_NOT_FOUND_ERROR:
			WriteString(theEnv, STDERR, "Function '");
			WriteString(theEnv, STDERR, func.c_str());
			WriteString(theEnv, STDERR, "' does not exist!\n");
			break;
		case FunctionCallBuilderError::FCBE_INVALID_FUNCTION_ERROR:
			WriteString(theEnv, STDERR, "Function '");
			WriteString(theEnv, STDERR, func.c_str());
			WriteString(theEnv, STDERR, "' has a custom parser and cannot be used with map$!\n");
			break;
		case FunctionCallBuilderError::FCBE_NULL_POINTER_ERROR:
			WriteString(theEnv, STDERR, "Provided function name is null!\n");
			break;
		case FunctionCallBuilderError::FCBE_NO_ERROR:
			WriteString(theEnv, STDERR, "NO_ERROR SHOULD NEVER EVER BE FIRED!!!\n");
			break;
		default:
			WriteString(theEnv, STDERR, "Unknown function builder error occurred!\n");
			break;
	}

}
void
MapFunction(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value func, curr;
	if (!UDFFirstArgument(context, LEXEME_BITS, &func)) {
        clips::udf::setFalse(env, ret);
		return;
	} else {
		maya::MultifieldBuilder mb(env);
		while (UDFHasNextArgument(context)) {
			if (! UDFNextArgument(context,ANY_TYPE_BITS,&curr)) {
                clips::udf::setFalse(env, ret);
				return;
			} else {
				clips::InternalValue tmp;
				maya::FunctionCallBuilder fcb(env, 0);
				fcb.append(&curr);
				auto result = fcb.call(clips::udf::getLexeme(func), &tmp);
				if (result != FunctionCallBuilderError::FCBE_NO_ERROR) {
					FunctionError(env, 1, result, clips::udf::getLexeme(func));
					break;
				}
				mb.append(&tmp);
			}
		}
		ret->multifieldValue = mb.create();
	}
}

void
FilterFunction(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value func, curr;
	if (!UDFFirstArgument(context, LEXEME_BITS, &func)) {
        clips::udf::setFalse(env, ret);
		return;
	} else {
		maya::MultifieldBuilder mb(env);
		while (UDFHasNextArgument(context)) {
			if (! UDFNextArgument(context,ANY_TYPE_BITS,&curr)) {
                clips::udf::setFalse(env, ret);
				return;
			} else {
				clips::InternalValue tmp;
				maya::FunctionCallBuilder fcb(env, 0);
				fcb.append(&curr);
				auto result = fcb.call(clips::udf::getLexeme(func), &tmp);
				if (result != FunctionCallBuilderError::FCBE_NO_ERROR) {
					FunctionError(env, 1, result, clips::udf::getLexeme(func));
					break;
				}
                if (clips::getBoolean(env, tmp)) {
					mb.append(&curr);
				}
			}
		}
		ret->multifieldValue = mb.create();
	}
}

void
ExistsFunction(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value func, curr;
	if (!UDFFirstArgument(context, LEXEME_BITS, &func)) {
        clips::udf::setFalse(env, ret);
		return;
	} else {
        clips::udf::setFalse(env, ret);
		while (UDFHasNextArgument(context)) {
			if (! UDFNextArgument(context,ANY_TYPE_BITS,&curr)) {
                clips::udf::setFalse(env, ret);
				return;
			} else {
				clips::InternalValue tmp;
				maya::FunctionCallBuilder fcb(env, 0);
				fcb.append(&curr);
				auto result = fcb.call(clips::udf::getLexeme(func), &tmp);
				if (result != FunctionCallBuilderError::FCBE_NO_ERROR) {
					FunctionError(env, 1, result, clips::udf::getLexeme(func));
					break;
				}
                if (clips::getBoolean(env, tmp)) {
                    clips::udf::setTrue(env, ret);
					return;
				}
			}
		}
	}
}

void
NotExistsFunction(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value func, curr;
	if (!UDFFirstArgument(context, LEXEME_BITS, &func)) {
        clips::udf::setFalse(env, ret);
		return;
	} else {
        clips::udf::setTrue(env, ret);
		while (UDFHasNextArgument(context)) {
			if (! UDFNextArgument(context,ANY_TYPE_BITS,&curr)) {
                clips::udf::setFalse(env, ret);
				return;
			} else {
				clips::InternalValue tmp;
				maya::FunctionCallBuilder fcb(env, 0);
				fcb.append(&curr);
				auto result = fcb.call(clips::udf::getLexeme(func), &tmp);
				if (result != FunctionCallBuilderError::FCBE_NO_ERROR) {
					FunctionError(env, 1, result, clips::udf::getLexeme(func));
					break;
				}
                if (clips::getBoolean(env, tmp)) {
                    clips::udf::setFalse(env, ret);
					return;
				}
			}
		}
	}
}

namespace maya {
	FunctionCallBuilder::FunctionCallBuilder(Environment* theEnv, size_t size) : _builder(CreateFunctionCallBuilder(theEnv, size)) { }
	FunctionCallBuilder::~FunctionCallBuilder() {
		FCBDispose(_builder);
		_builder = nullptr;
	}
	FunctionCallBuilder::ErrorKind FunctionCallBuilder::call(const std::string& functionName, clips::InternalValue* ret) noexcept {
		return FCBCall(_builder, functionName.c_str(), ret);
	}
	void FunctionCallBuilder::append(clips::udf::Value* value) noexcept { FCBAppendUDFValue(_builder, value); }
	void FunctionCallBuilder::append(clips::InternalValue* value) noexcept { FCBAppend(_builder, value); }
	void FunctionCallBuilder::append(clips::Integer* value) noexcept { FCBAppendCLIPSInteger(_builder, value); }
	void FunctionCallBuilder::append(int64_t value) noexcept { FCBAppendInteger(_builder, value); }
	void FunctionCallBuilder::append(clips::Float* value) noexcept { FCBAppendCLIPSFloat(_builder, value); }
	void FunctionCallBuilder::append(double value) noexcept { FCBAppendFloat(_builder, value); }
	void FunctionCallBuilder::append(clips::Lexeme* value) noexcept { FCBAppendCLIPSLexeme(_builder, value); }
	void FunctionCallBuilder::append(clips::ExternalAddress* value) noexcept { FCBAppendCLIPSExternalAddress(_builder, value); }
	void FunctionCallBuilder::append(Fact* value) noexcept { FCBAppendFact(_builder, value); }
	void FunctionCallBuilder::append(Instance* value) noexcept { FCBAppendInstance(_builder, value); }
	void FunctionCallBuilder::append(Multifield* value) noexcept { FCBAppendMultifield(_builder, value); }
	void FunctionCallBuilder::appendSymbol(const std::string& sym) noexcept { FCBAppendSymbol(_builder, sym.c_str()); }
	void FunctionCallBuilder::appendString(const std::string& sym) noexcept { FCBAppendString(_builder, sym.c_str()); }
	void FunctionCallBuilder::appendInstanceName(const std::string& sym) noexcept { FCBAppendInstanceName(_builder, sym.c_str()); }
	MultifieldBuilder::MultifieldBuilder(Environment* theEnv, size_t size) : _builder(CreateMultifieldBuilder(theEnv, size)) { }
	MultifieldBuilder::~MultifieldBuilder() { MBDispose(_builder); }
	void MultifieldBuilder::append(clips::udf::Value* value) noexcept { MBAppendUDFValue(_builder, value); }
	void MultifieldBuilder::append(clips::InternalValue* value) noexcept { MBAppend(_builder, value); }
	void MultifieldBuilder::append(clips::Integer* value) noexcept { MBAppendCLIPSInteger(_builder, value); }
	void MultifieldBuilder::append(clips::Float* value) noexcept { MBAppendCLIPSFloat(_builder, value); }
	void MultifieldBuilder::append(clips::Lexeme* value) noexcept { MBAppendCLIPSLexeme(_builder, value); }
	void MultifieldBuilder::append(clips::ExternalAddress* value) noexcept { MBAppendCLIPSExternalAddress(_builder, value); }
	void MultifieldBuilder::append(Fact* value) noexcept { MBAppendFact(_builder, value); }
	void MultifieldBuilder::append(Instance* value) noexcept { MBAppendInstance(_builder, value); }
	void MultifieldBuilder::append(Multifield* value) noexcept { MBAppendMultifield(_builder, value); }
	void MultifieldBuilder::append(int64_t value) noexcept { MBAppendInteger(_builder, value); }
	void MultifieldBuilder::append(double value) noexcept { MBAppendFloat(_builder, value); }
	void MultifieldBuilder::appendSymbol(const std::string& value) noexcept { MBAppendSymbol(_builder, value.c_str()); }
	void MultifieldBuilder::appendString(const std::string& value) noexcept { MBAppendString(_builder, value.c_str()); }
	void MultifieldBuilder::appendInstanceName(const std::string& value) noexcept { MBAppendInstanceName(_builder, value.c_str()); }
} // end namespace maya
#endif

