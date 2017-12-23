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
extern "C" {
#include "clips.h"
}
#include "mayasetup.h"
#include "boost.h"
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
#include <boost/algorithm/minmax.hpp>


#if BOOST_EXTENSIONS
void HasPrefix(Environment*, UDFContext*, UDFValue*);
void HasSuffix(Environment*, UDFContext*, UDFValue*);
void TrimString(Environment*, UDFContext*, UDFValue*);
void TrimStringFront(Environment*, UDFContext*, UDFValue*);
void TrimStringBack(Environment*, UDFContext*, UDFValue*);
void NewUUID(Environment*, UDFContext*, UDFValue*);
void gcdFunction(Environment*, UDFContext*, UDFValue*);
void lcmFunction(Environment*, UDFContext*, UDFValue*);
void FileExists(Environment*, UDFContext*, UDFValue*);
void IsDirectory(Environment*, UDFContext*, UDFValue*);
void IsRegularFile(Environment*, UDFContext*, UDFValue*);
void ClampValue(Environment*, UDFContext*, UDFValue*);
//void MinMaxFunction(Environment*, UDFContext*, UDFValue*);
#endif

extern "C" void InstallBoostExtensions(Environment* theEnv) {
#if BOOST_EXTENSIONS
	AddUDF(theEnv, "has-prefix", "b", 2, 2, "sy;sy;sy", HasPrefix, "HasPrefix",  NULL);
	AddUDF(theEnv, "has-suffix", "b", 2, 2, "sy;sy;sy", HasSuffix, "HasSuffix",  NULL);
	AddUDF(theEnv, "string-trim", "y", 1, 1, "s", TrimString, "TrimString", NULL);
	AddUDF(theEnv, "string-trim-front", "y", 1, 1, "s", TrimStringFront, "TrimStringFront", NULL);
	AddUDF(theEnv, "string-trim-back", "y",  1, 1, "s", TrimStringBack, "TrimStringBack", NULL);
	AddUDF(theEnv, "new-uuid", "s", 0, 0, "", NewUUID, "NewUUID", NULL);
	AddUDF(theEnv, "gcd", "l",  2, 2, "l;l;l", gcdFunction, "gcdFunction", NULL);
	AddUDF(theEnv, "lcm", "l",  2, 2, "l;l;l", lcmFunction, "lcmFunction", NULL);
	AddUDF(theEnv, "path-exists",   "b", 1, 1, "sy", FileExists, "FileExists", NULL);
	AddUDF(theEnv, "directoryp",    "b", 1, 1, "sy", IsDirectory, "IsDirectory", NULL);
	AddUDF(theEnv, "regular-filep", "b", 1, 1, "sy", IsRegularFile, "IsRegularFile", NULL);
	AddUDF(theEnv, "clamp", "l",  3, 3, "l;l;l;l", ClampValue, "ClampValue", NULL);
//	AddUDF(theEnv, "min-max", "m", 2, 2, "ld;ld;ld", MinMaxFunction, "MinMaxFunction", NULL);
#endif
}


#if BOOST_EXTENSIONS
//void MinMaxFunction(Environment* env, UDFContext* context, UDFValue* ret) {
//	UDFValue a, b;
//	if (!UDFFirstArgument(context, NUMBER_BITS, &a)) {
//		ret->lexemeValue = FalseSymbol(env);
//	} else if (!UDFNextArgument(context, NUMBER_BITS, &b)) {
//		ret->lexemeValue = FalseSymbol(env);
//	} else {
//		Environment* environment = UDFContextEnvironment(context);
//		ret->type = MULTIFIELD;
//		ret->begin = 0;
//		ret->end = 1;
//		ret->value = EnvCreateMultifield(environment, 2L);
//		if (CVIsType(&a, INTEGER_TYPE) && CVIsType(&b, INTEGER_TYPE)) {
//			auto result = boost::minmax(CVToInteger(&a), CVToInteger(&b));
//			SetMFType(ret->value, 1, INTEGER);
//			SetMFValue(ret->value, 1, EnvAddLong(environment, result.get<0>()));
//			SetMFType(ret->value, 2, INTEGER);
//			SetMFValue(ret->value, 2, EnvAddLong(environment, result.get<1>()));
//		} else {
//			// one of them is FLOAT_TYPE
//			auto result = boost::minmax(CVToFloat(&a), CVToFloat(&b));
//			SetMFType(ret->value, 1, FLOAT);
//			SetMFValue(ret->value, 1, EnvAddDouble(environment, result.get<0>()));
//			SetMFType(ret->value, 2, FLOAT);
//			SetMFValue(ret->value, 2, EnvAddDouble(environment, result.get<1>()));
//		}
//	}
//}
void ClampValue(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue v, lo, hi;
	if (!UDFFirstArgument(context, INTEGER_TYPE,  &v)) {
		ret->lexemeValue = FalseSymbol(env);
	} else if (!UDFNextArgument(context, INTEGER_TYPE, &lo)) {
		ret->lexemeValue = FalseSymbol(env);
	} else if (!UDFNextArgument(context, INTEGER_TYPE, &hi)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		ret->integerValue = CreateInteger(env, boost::algorithm::clamp(CVCoerceToInteger(&v), CVCoerceToInteger(&lo), CVCoerceToInteger(&hi)));
	}
}

void FileExists(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue path;
	if (!UDFFirstArgument(context, LEXEME_BITS, &path)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		ret->lexemeValue = boost::filesystem::exists(path.lexemeValue->contents) ? TrueSymbol(env) : FalseSymbol(env);
	}
}

void IsDirectory(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue path;
	if (!UDFFirstArgument(context, LEXEME_BITS, &path)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		ret->lexemeValue = boost::filesystem::is_directory(path.lexemeValue->contents) ? TrueSymbol(env) : FalseSymbol(env);
	}
}

void IsRegularFile(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue path;
	if (!UDFFirstArgument(context, LEXEME_BITS, &path)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		ret->lexemeValue = boost::filesystem::is_regular_file(path.lexemeValue->contents) ? TrueSymbol(env) : FalseSymbol(env);
	}
}

void gcdFunction(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue first, second;
	if (!UDFFirstArgument(context, INTEGER_TYPE, &first)) {
		ret->lexemeValue = FalseSymbol(env);
	} else if (!UDFNextArgument(context, INTEGER_TYPE, &second)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		ret->integerValue = CreateInteger(env, boost::math::gcd(CVCoerceToInteger(&first), CVCoerceToInteger(&second)));
	}
}
void lcmFunction(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue first, second;
	if (!UDFFirstArgument(context, INTEGER_TYPE, &first)) {
		ret->lexemeValue = FalseSymbol(env);
	} else if (!UDFNextArgument(context, INTEGER_TYPE, &second)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		ret->integerValue = CreateInteger(env, boost::math::lcm(CVCoerceToInteger(&first), CVCoerceToInteger(&second)));
	}
}
void NewUUID(Environment* env, UDFContext* context, UDFValue* ret) {
	boost::uuids::random_generator rgen;
	boost::uuids::uuid theUUID(rgen());
	const std::string tmp = boost::lexical_cast<std::string>(theUUID);
	ret->value = CreateSymbol(env, tmp.c_str());
}
void HasPrefix(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue data, prefix;
	if (!UDFFirstArgument(context, LEXEME_BITS, &data)) {
		ret->lexemeValue = FalseSymbol(env);
		return;
	} else if (!UDFNextArgument(context, LEXEME_BITS, &prefix)) {
		ret->lexemeValue = FalseSymbol(env);
		return;
	}
	std::string dataStr(data.lexemeValue->contents);
	std::string prefixStr(prefix.lexemeValue->contents);
	ret->lexemeValue = boost::starts_with(dataStr, prefixStr) ? TrueSymbol(env) : FalseSymbol(env);
}

void HasSuffix(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue data, suffix;
	if (!UDFFirstArgument(context, LEXEME_BITS, &data)) {
		ret->lexemeValue = FalseSymbol(env);
		return;
	} else if (!UDFNextArgument(context, LEXEME_BITS, &suffix)) {
		ret->lexemeValue = FalseSymbol(env);
		return;
	}
	std::string dataStr(data.lexemeValue->contents);
	std::string suffixStr(suffix.lexemeValue->contents);
	ret->lexemeValue = boost::ends_with(dataStr, suffixStr) ? TrueSymbol(env) : FalseSymbol(env);
}
void TrimString(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue str;
	if (!UDFFirstArgument(context, STRING_BIT, &str)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		std::string tmp(str.lexemeValue->contents);
		boost::algorithm::trim(tmp);
		ret->value = CreateString(env, tmp.c_str());
	}
}
void TrimStringFront(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue str;
	if (!UDFFirstArgument(context, STRING_BIT, &str)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		std::string tmp(str.lexemeValue->contents);
		boost::algorithm::trim_left(tmp);
		ret->value = CreateString(env, tmp.c_str());
	}
}
void TrimStringBack(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue str;
	if (!UDFFirstArgument(context, STRING_BIT, &str)) {
		ret->lexemeValue = FalseSymbol(env);
	} else {
		std::string tmp(str.lexemeValue->contents);
		boost::algorithm::trim_right(tmp);
		ret->value = CreateString(env, tmp.c_str());
	}
}
#endif



