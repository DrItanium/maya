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
#ifndef CLIPS_PLUS_PLUS_H__
#define CLIPS_PLUS_PLUS_H__

extern "C" {
    #include "clips.h"
}

namespace clips {
    using Void = ::CLIPSVoid;
    using Lexeme = ::CLIPSLexeme;
    using Float = ::CLIPSFloat;
    using Integer = ::CLIPSInteger;
    using BitMap = ::CLIPSBitMap;
    using ExternalAddress = ::CLIPSExternalAddress;
    using InternalValue = ::CLIPSValue;
    namespace udf {
        using Value = ::UDFValue;
        using Context = ::UDFContext;
    } // end namespace udf
    namespace entity {
        using Record = ::EntityRecord;
        using PrintFunction = ::EntityPrintFunction;
        using EvaluationFunction = ::EntityEvaluationFunction;
        using BusyCountFunction = ::EntityBusyCountFunction;
    } // end namespace entity

} // end namespace clips


#endif // end CLIPS_PLUS_PLUS_H__
