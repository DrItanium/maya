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
#include "boost.h"
#include "ClipsPlusPlus.h"
#include <string>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/math/common_factor.hpp>
#include <boost/filesystem.hpp>
#include <boost/system/error_code.hpp>
#include <boost/algorithm/clamp.hpp>


#if BOOST_EXTENSIONS
void HasPrefix(Environment*, clips::udf::Context*, clips::udf::Value*);
void HasSuffix(Environment*, clips::udf::Context*, clips::udf::Value*);
void TrimString(Environment*, clips::udf::Context*, clips::udf::Value*);
void TrimStringFront(Environment*, clips::udf::Context*, clips::udf::Value*);
void TrimStringBack(Environment*, clips::udf::Context*, clips::udf::Value*);
void NewUUID(Environment*, clips::udf::Context*, clips::udf::Value*);
void gcdFunction(Environment*, clips::udf::Context*, clips::udf::Value*);
void lcmFunction(Environment*, clips::udf::Context*, clips::udf::Value*);
void FileExists(Environment*, clips::udf::Context*, clips::udf::Value*);
void IsDirectory(Environment*, clips::udf::Context*, clips::udf::Value*);
void IsRegularFile(Environment*, clips::udf::Context*, clips::udf::Value*);
void ClampValue(Environment*, clips::udf::Context*, clips::udf::Value*);
#endif
namespace clips {
    namespace udf {
        AddError add(Environment* env, const std::string& name, const std::string& retType, unsigned short minArgs, unsigned short maxArgs, const std::string& parameterRestrictions, Function* body, const std::string& actualFunctionName, void* context) noexcept {
            return ::AddUDF(env, name.c_str(), retType.c_str(), minArgs, maxArgs, parameterRestrictions.c_str(), body, actualFunctionName.c_str(), context);
        }
        AddError addNoArguments(Environment* env, const std::string& name, const std::string& retType, Function* body, const std::string& actualFunctionName, void* context) noexcept {
            return ::AddUDF(env, name.c_str(), retType.c_str(), 0, 0, "", body, actualFunctionName.c_str(), context);
        }
    } // end namespace udf
} // end namespace clips
extern "C" void InstallBoostExtensions(Environment* theEnv) {
#if BOOST_EXTENSIONS
    clips::udf::add(theEnv, "has-prefix", "b", 2, 2, "sy;sy;sy", HasPrefix, "HasPrefix");
    clips::udf::add(theEnv, "has-suffix", "b", 2, 2, "sy;sy;sy", HasSuffix, "HasSuffix");
	clips::udf::add(theEnv, "string-trim", "y", 1, 1, "s", TrimString, "TrimString");
	clips::udf::add(theEnv, "string-trim-front", "y", 1, 1, "s", TrimStringFront, "TrimStringFront");
	clips::udf::add(theEnv, "string-trim-back", "y",  1, 1, "s", TrimStringBack, "TrimStringBack");
    clips::udf::addNoArguments(theEnv, "new-uuid", "s", NewUUID, "NewUUID");
	clips::udf::add(theEnv, "gcd", "l",  2, 2, "l;l;l", gcdFunction, "gcdFunction");
	clips::udf::add(theEnv, "lcm", "l",  2, 2, "l;l;l", lcmFunction, "lcmFunction");
	clips::udf::add(theEnv, "path-exists",   "b", 1, 1, "sy", FileExists, "FileExists");
	clips::udf::add(theEnv, "directoryp",    "b", 1, 1, "sy", IsDirectory, "IsDirectory");
	clips::udf::add(theEnv, "regular-filep", "b", 1, 1, "sy", IsRegularFile, "IsRegularFile");
	clips::udf::add(theEnv, "clamp", "l",  3, 3, "l;l;l;l", ClampValue, "ClampValue");
#endif
}


#if BOOST_EXTENSIONS
void ClampValue(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value v, lo, hi;
	if (!UDFFirstArgument(context, INTEGER_BIT,  &v)) {
        clips::udf::setFalse(env, ret);
	} else if (!UDFNextArgument(context, INTEGER_BIT, &lo)) {
        clips::udf::setFalse(env, ret);
	} else if (!UDFNextArgument(context, INTEGER_BIT, &hi)) {
        clips::udf::setFalse(env, ret);
	} else {
		ret->integerValue = CreateInteger(env, boost::algorithm::clamp(CVCoerceToInteger(&v), CVCoerceToInteger(&lo), CVCoerceToInteger(&hi)));
	}
}

void FileExists(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value path;
	if (!UDFFirstArgument(context, LEXEME_BITS, &path)) {
        clips::udf::setFalse(env, ret);
	} else {
		std::string p(path.lexemeValue->contents);
        clips::udf::setBoolean(env, ret, boost::filesystem::exists(p));
	}
}

void IsDirectory(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value path;
	if (!UDFFirstArgument(context, LEXEME_BITS, &path)) {
        clips::udf::setFalse(env, ret);
	} else {
		std::string p(path.lexemeValue->contents);
        clips::udf::setBoolean(env, ret, boost::filesystem::is_directory(p));
	}
}

void IsRegularFile(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value path;
	if (!UDFFirstArgument(context, LEXEME_BITS, &path)) {
        clips::udf::setFalse(env, ret);
	} else {
		std::string p(path.lexemeValue->contents);
        clips::udf::setBoolean(env, ret, boost::filesystem::is_regular_file(p));
	}
}

void gcdFunction(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value first, second;
	if (!UDFFirstArgument(context, INTEGER_BIT, &first)) {
        clips::udf::setFalse(env, ret);
	} else if (!UDFNextArgument(context, INTEGER_BIT, &second)) {
        clips::udf::setFalse(env, ret);
	} else {
		ret->integerValue = CreateInteger(env, boost::math::gcd(CVCoerceToInteger(&first), CVCoerceToInteger(&second)));
	}
}
void lcmFunction(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value first, second;
	if (!UDFFirstArgument(context, INTEGER_BIT, &first)) {
        clips::udf::setFalse(env, ret);
	} else if (!UDFNextArgument(context, INTEGER_BIT, &second)) {
        clips::udf::setFalse(env, ret);
	} else {
		ret->integerValue = CreateInteger(env, boost::math::lcm(CVCoerceToInteger(&first), CVCoerceToInteger(&second)));
	}
}
void NewUUID(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	boost::uuids::random_generator rgen;
	boost::uuids::uuid theUUID(rgen());
	const std::string tmp = boost::lexical_cast<std::string>(theUUID);
	ret->value = CreateSymbol(env, tmp.c_str());
}
void HasPrefix(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value data, prefix;
	if (!UDFFirstArgument(context, LEXEME_BITS, &data)) {
        clips::udf::setFalse(env, ret);
		return;
	} else if (!UDFNextArgument(context, LEXEME_BITS, &prefix)) {
        clips::udf::setFalse(env, ret);
		return;
	}
	std::string dataStr(data.lexemeValue->contents);
	std::string prefixStr(prefix.lexemeValue->contents);
    clips::udf::setBoolean(env, ret, boost::starts_with(dataStr, prefixStr));
}

void HasSuffix(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value data, suffix;
	if (!UDFFirstArgument(context, LEXEME_BITS, &data)) {
        clips::udf::setFalse(env, ret);
		return;
	} else if (!UDFNextArgument(context, LEXEME_BITS, &suffix)) {
        clips::udf::setFalse(env, ret);
		return;
	}
	std::string dataStr(data.lexemeValue->contents);
	std::string suffixStr(suffix.lexemeValue->contents);
    clips::udf::setBoolean(env, ret, boost::ends_with(dataStr, suffixStr));
}
void TrimString(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value str;
	if (!UDFFirstArgument(context, STRING_BIT, &str)) {
        clips::udf::setFalse(env, ret);
	} else {
		std::string tmp(str.lexemeValue->contents);
		boost::algorithm::trim(tmp);
		ret->value = CreateString(env, tmp.c_str());
	}
}
void TrimStringFront(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value str;
	if (!UDFFirstArgument(context, STRING_BIT, &str)) {
        clips::udf::setFalse(env, ret);
	} else {
		std::string tmp(str.lexemeValue->contents);
		boost::algorithm::trim_left(tmp);
		ret->value = CreateString(env, tmp.c_str());
	}
}
void TrimStringBack(Environment* env, clips::udf::Context* context, clips::udf::Value* ret) {
	clips::udf::Value str;
	if (!UDFFirstArgument(context, STRING_BIT, &str)) {
        clips::udf::setFalse(env, ret);
	} else {
		std::string tmp(str.lexemeValue->contents);
		boost::algorithm::trim_right(tmp);
		ret->value = CreateString(env, tmp.c_str());
	}
}
#endif



