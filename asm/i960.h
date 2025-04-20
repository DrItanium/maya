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
#include <variant>
#include <tuple>
namespace i960 {
using LongOrdinal = uint64_t;
using LongInteger = int64_t;
using Ordinal = uint32_t;
using Integer = int32_t;
using ShortOrdinal = uint16_t;
using ShortInteger = int16_t;
using ByteOrdinal = uint8_t;
using ByteInteger = int8_t;
using Address = Ordinal;
using DoubleInstruction = std::tuple<Ordinal, Ordinal>;
using EncodedInstruction = std::variant<Ordinal, DoubleInstruction>;
using InstructionPlacement = std::tuple<Address, EncodedInstruction>;
template<typename T>
struct TreatAs {
    using Type = T;
};

constexpr InstructionPlacement place(Address baseAddress, EncodedInstruction inst) noexcept {
    return std::make_tuple(baseAddress, inst);
}
constexpr EncodedInstruction makeInstruction(Ordinal lo) noexcept {
    return lo;
}
constexpr EncodedInstruction makeInstruction(Ordinal lo, Ordinal hi) noexcept {
    return std::make_tuple(lo, hi);
}
constexpr InstructionPlacement place(Address baseAddress, Ordinal lo) noexcept {
    return place(baseAddress, makeInstruction(lo));
}
constexpr InstructionPlacement place(Address baseAddress, Ordinal lo, Ordinal hi) noexcept {
    return place(baseAddress, makeInstruction(lo, hi));
}
enum class CTRLOpcodes : Ordinal {
    b = 0x8,
    call,
    ret,
    bal,
    bno = 0x10,
    bg,
    be,
    bge,
    bl,
    bne,
    ble,
    bo,
    faultno,
    faultg,
    faulte,
    faultge,
    faultl,
    faultne,
    faultle,
    faulto,
};
constexpr Ordinal encode(CTRLOpcodes opcode) noexcept {
    return (static_cast<Ordinal>(opcode) << 24) & 0xFF00'0000;
}
constexpr Ordinal encodeCTRL(CTRLOpcodes opcode, Integer displacement) noexcept {
    return encode(opcode) | (displacement & 0x00'FFFFFC);
}
static_assert(encodeCTRL(CTRLOpcodes::b, -1) == 0x08'FFFFFC, "Bad encoding work!");

constexpr Ordinal branch(Integer displacement) noexcept { return encodeCTRL(CTRLOpcodes::b, displacement); }
constexpr Ordinal branchAndLink(Integer displacement) noexcept { return encodeCTRL(CTRLOpcodes::bal, displacement); }
constexpr Ordinal call(Integer displacement) noexcept { return encodeCTRL(CTRLOpcodes::call, displacement); }
constexpr Ordinal ret() noexcept { return encodeCTRL(CTRLOpcodes::ret, 0); }

#define X(title, c) constexpr Ordinal title (Integer displacement) { return encodeCTRL(CTRLOpcodes:: c , displacement ); }
X(branchIfUnordered, bno);
X(branchIfGreaterThan, bg);
X(branchIfEqual, be);
X(branchIfGreaterThanOrEqual, bge);
X(branchIfLessThan, bl);
X(branchIfNotEqual, bne);
X(branchIfLessThanOrEqual, ble);
X(branchIfOrdered, bo);
#undef X

} // end namespace i960

#endif // end !defined(MAYA_I960_H__)
