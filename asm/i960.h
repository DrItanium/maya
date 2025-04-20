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

constexpr auto branch(Integer displacement) noexcept { return encodeCTRL(CTRLOpcodes::b, displacement); }
constexpr auto b(Integer displacement) noexcept { return branch(displacement); }
constexpr auto branchAndLink(Integer displacement) noexcept { return encodeCTRL(CTRLOpcodes::bal, displacement); }
constexpr auto bal(Integer displacement) noexcept { return branchAndLink(displacement); }
constexpr auto call(Integer displacement) noexcept { return encodeCTRL(CTRLOpcodes::call, displacement); }
constexpr auto callSubroutine(Integer displacement) noexcept { return call(displacement); }
constexpr auto ret() noexcept { return encodeCTRL(CTRLOpcodes::ret, 0); }

#define X(title, c) \
    constexpr auto branchIf ## title (Integer displacement) noexcept { return encodeCTRL(CTRLOpcodes:: b ## c , displacement ); } \
    constexpr auto b ## c (Integer displacement) noexcept { return branchIf ## title (displacement); } 
X(Unordered, no);
X(GreaterThan, g);
X(Equal, e);
X(GreaterThanOrEqual, ge);
X(LessThan, l);
X(NotEqual, ne);
X(LessThanOrEqual, le);
X(Ordered, o);
#undef X
#define X(title, c) \
    constexpr auto faultIf ## title () noexcept { return encodeCTRL(CTRLOpcodes:: fault ## c , 0 ); } \
    constexpr auto fault ## c () noexcept { return faultIf ## title ( ) ; } 
                
X(Unordered, no);
X(GreaterThan, g);
X(Equal, e);
X(GreaterThanOrEqual, ge);
X(LessThan, l);
X(NotEqual, ne);
X(LessThanOrEqual, le);
X(Ordered, o);
#undef X

enum class Register : Ordinal {
    pfp,
    sp,
    rip,
    r3,
    r4,
    r5,
    r6,
    r7,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,
    g0,
    g1,
    g2,
    g3,
    g4,
    g5,
    g6,
    g7,
    g8,
    g9,
    g10,
    g11,
    g12,
    g13,
    g14,
    fp,
    ignore = pfp,
};

constexpr Ordinal encodeForSrcDest(Register value) noexcept {
    //0b00000000'11111000'00000000'00000000;
    return (static_cast<Ordinal>(value) & 0b11111) << 19;
}
static_assert(encodeForSrcDest(Register::pfp) == 0x00'00'00'00);
static_assert(encodeForSrcDest(Register::sp) == 0x00'08'00'00);
static_assert(encodeForSrcDest(Register::rip) == 0x00'10'00'00);
static_assert(encodeForSrcDest(Register::r3) == 0x00'18'00'00);
static_assert(encodeForSrcDest(Register::fp) == 0x00'F8'00'00);
// aliases
constexpr auto encodeForCOBRSrc1(Register value) noexcept { return encodeForSrcDest(value); }

constexpr Ordinal encodeForSrc2(Register value) noexcept {
    //0b00000000'00000'11111'000000'00000000;
    //0b00000000'00000111'11000000'00000000;
    return (static_cast<Ordinal>(value) & 0b11111) << 14;
}
static_assert(encodeForSrc2(Register::fp) == 0x00'07'C0'00);

constexpr auto encodeForAbase(Register value) noexcept { return encodeForSrc2(value); }
constexpr Ordinal BitM3Set_REG = 0b00000000'00000'00000'1'0'0'0000'00'00000;
constexpr Ordinal BitM2Set_REG = 0b00000000'00000'00000'0'1'0'0000'00'00000;
constexpr Ordinal BitM1Set_REG = 0b00000000'00000'00000'0'0'1'0000'00'00000;
constexpr Ordinal DisplacementMask_COBR = 0b00000000'00000'00000'0'1111111111100;
constexpr Ordinal Enable_MEMB = BitM2Set_REG;
constexpr Ordinal BitM1Set_COBR = BitM3Set_REG;
constexpr Ordinal ModeSet_MEMA = BitM3Set_REG;
enum class COBROpcodes : Ordinal {
    testno = 0x20,
    testg,
    teste,
    testge,
    testl,
    testne,
    testle,
    testo,
    bbc = 0x30,
    cmpobg,
    cmpobe,
    cmpobge,
    cmpobl,
    cmpobne,
    cmpoble,
    bbs,
    cmpibno,
    cmpibg,
    cmpibe,
    cmpibge,
    cmpibl,
    cmpibne,
    cmpible,
    cmpibo,
};
constexpr Ordinal encode(COBROpcodes opcode) noexcept {
    return (static_cast<Ordinal>(opcode) << 24) & 0xFF00'0000;
}
constexpr Ordinal encodeCOBR(COBROpcodes opcode, Register src1, Register src2, Integer displacement) noexcept {
    return encode(opcode) | encodeForCOBRSrc1(src1) | encodeForSrc2(src2) | (displacement & DisplacementMask_COBR);
}
constexpr Ordinal encodeCOBR(COBROpcodes opcode, ByteOrdinal src1, Register src2, Integer displacement) noexcept {
    // just toggle M1 on in this case also chop down to between 0 and 31
    return encodeCOBR(opcode, static_cast<Register>(src1 & 0b11111), src2, displacement) | BitM1Set_COBR;
}
constexpr auto branchIfBitClear(Register bitpos, Register src, Integer targ) noexcept { return encodeCOBR(COBROpcodes::bbc, bitpos, src, targ); }
constexpr auto branchIfBitClear(ByteOrdinal bitpos, Register src, Integer targ) noexcept { return encodeCOBR(COBROpcodes::bbc, bitpos, src, targ); }
constexpr auto bbc(Register bitpos, Register src, Integer targ) noexcept { return branchIfBitClear(bitpos, src, targ); }
constexpr auto bbc(ByteOrdinal bitpos, Register src, Integer targ) noexcept { return branchIfBitClear(bitpos, src, targ); }
constexpr auto branchIfBitSet(Register bitpos, Register src, Integer targ) noexcept { return encodeCOBR(COBROpcodes::bbs, bitpos, src, targ); }
constexpr auto branchIfBitSet(ByteOrdinal bitpos, Register src, Integer targ) noexcept { return encodeCOBR(COBROpcodes::bbs, bitpos, src, targ); }
constexpr auto bbs(Register bitpos, Register src, Integer targ) noexcept { return branchIfBitSet(bitpos, src, targ); }
constexpr auto bbs(ByteOrdinal bitpos, Register src, Integer targ) noexcept { return branchIfBitSet(bitpos, src, targ); }
constexpr auto branchIfMSBSet(Register src, Integer targ) noexcept { return branchIfBitSet(31, src, targ); }
constexpr auto bbsMSB(Register src, Integer targ) noexcept { return branchIfMSBSet(src, targ); }
constexpr auto branchIfMSBClear(Register src, Integer targ) noexcept { return branchIfBitClear(31, src, targ); }
constexpr auto bbcMSB(Register src, Integer targ) noexcept { return branchIfMSBClear(src, targ); }
static_assert(branchIfBitClear(0, Register::ignore, 0) == (encode(COBROpcodes::bbc) | BitM1Set_COBR));
#define X(title, k) constexpr auto testIf ## title ( Register dest) noexcept { return encodeCOBR(COBROpcodes :: test ## k , dest, Register::ignore, 0); }
X(Unordered, no);
X(GreaterThan, g);
X(Equal, e);
X(GreaterThanOrEqual, ge);
X(LessThan, l);
X(NotEqual, ne);
X(LessThanOrEqual, le);
X(Ordered, o);
#undef X

#define X(title, k) \
    constexpr auto compareAndBranchOrdinalIf ## title ( Register src1, Register src2, Integer displacement) noexcept { return encodeCOBR(COBROpcodes :: cmpob ## k , src1, src2, displacement); } \
    constexpr auto compareAndBranchOrdinalIf ## title ( ByteOrdinal src1, Register src2, Integer displacement) noexcept { return encodeCOBR(COBROpcodes :: cmpob ## k , src1, src2, displacement); } \
    constexpr auto cmpob ## k ( Register src1, Register src2, Integer displacement) noexcept { return encodeCOBR(COBROpcodes :: cmpob ## k , src1, src2, displacement); } \
    constexpr auto cmpob ## k ( ByteOrdinal src1, Register src2, Integer displacement) noexcept { return encodeCOBR(COBROpcodes :: cmpob ## k , src1, src2, displacement); } \
    constexpr auto cmpob ## k ## z ( Register src2 , Integer displacement) noexcept { return cmpob ## k ( 0 , src2 , displacement ); } \
    constexpr auto compareAndBranchOrdinalIf ## title ## Zero ( Register src2, Integer displacement) noexcept { return cmpob ## k ## z ( src2, displacement ); }


//X(Unordered, no);
X(GreaterThan, g);
X(Equal, e);
X(GreaterThanOrEqual, ge);
X(LessThan, l);
X(NotEqual, ne);
X(LessThanOrEqual, le);
//X(Ordered, o);
#undef X
#define X(title, k) \
    constexpr auto cmpib ## k ( Register src1, Register src2, Integer displacement) noexcept { return encodeCOBR(COBROpcodes :: cmpib ## k , src1, src2, displacement); } \
    constexpr auto cmpib ## k ( ByteOrdinal src1, Register src2, Integer displacement) noexcept { return encodeCOBR(COBROpcodes :: cmpib ## k , src1, src2, displacement); } \
    constexpr auto compareAndBranchIntegerIf ## title ( Register src1, Register src2, Integer displacement) noexcept { return cmpib ## k ( src1, src2, displacement ); } \
    constexpr auto compareAndBranchIntegerIf ## title ( ByteOrdinal src1, Register src2, Integer displacement) noexcept { return cmpib ## k ( src1, src2, displacement); } \
    constexpr auto cmpib ## k ## z (Register src2, Integer displacement) noexcept { return cmpib ## k ( 0, src2, displacement ); } \
    constexpr auto compareAndBranchIntegerIf ## title ## Zero (Register src2, Integer displacement) noexcept { return cmpib ## k ## z ( src2, displacement ); }
    
X(Unordered, no);
X(GreaterThan, g);
X(Equal, e);
X(GreaterThanOrEqual, ge);
X(LessThan, l);
X(NotEqual, ne);
X(LessThanOrEqual, le);
X(Ordered, o);
#undef X

} // end namespace i960

#endif // end !defined(MAYA_I960_H__)
