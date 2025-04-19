/**
 * @file
 * i960 Instruction Encoding
 * @copyright
 * maya-app
 * Copyright (c) 2012-2025, Joshua Scoggins
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

#ifndef MAYA_I960_H__
#define MAYA_I960_H__
#include <cstdint>
namespace i960 {
using LongOrdinal = uint64_t;
using LongInteger = int64_t;
using Ordinal = uint32_t;
using Integer = int32_t;
using ShortOrdinal = uint16_t;
using ShortInteger = int16_t;
using ByteOrdinal = uint8_t;
using ByteInteger = int8_t;
struct [[gnu::packed]] REGFormatInstruction {
    Ordinal src1 : 5;
    Ordinal s1 : 1;
    Ordinal s2 : 1;
    Ordinal opcodeLo : 4;
    Ordinal m1 : 1;
    Ordinal m2 : 1;
    Ordinal m3 : 1;
    Ordinal src2 : 5;
    Ordinal srcDst : 5;
    Ordinal opcode : 8;
};
static_assert(sizeof(REGFormatInstruction) == sizeof(Ordinal));

struct [[gnu::packed]] COBRFormatInstruction {
    Ordinal s2 : 1;
    Ordinal t : 1;
    Ordinal displacement : 11;
    Ordinal m1 : 1;
    Ordinal src2 : 5;
    Ordinal src1 : 5;
    Ordinal opcode : 8;
};
static_assert(sizeof(COBRFormatInstruction) == sizeof(Ordinal));

} // end namespace i960

#endif // end !defined(MAYA_I960_H__)
