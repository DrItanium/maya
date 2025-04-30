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
#include "features/overloaded.h"
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
constexpr bool valid(CTRLOpcodes opcode) noexcept {
    switch(opcode) {
        case CTRLOpcodes::b:
        case CTRLOpcodes::call:
        case CTRLOpcodes::ret:
        case CTRLOpcodes::bal:
        case CTRLOpcodes::bno:
        case CTRLOpcodes::bg:
        case CTRLOpcodes::be:
        case CTRLOpcodes::bge:
        case CTRLOpcodes::bl:
        case CTRLOpcodes::bne:
        case CTRLOpcodes::ble:
        case CTRLOpcodes::bo:
        case CTRLOpcodes::faultno:
        case CTRLOpcodes::faultg:
        case CTRLOpcodes::faulte:
        case CTRLOpcodes::faultge:
        case CTRLOpcodes::faultl:
        case CTRLOpcodes::faultne:
        case CTRLOpcodes::faultle:
        case CTRLOpcodes::faulto:
            return true;
        default:
            return false;
    }
}
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
constexpr bool valid(Register reg) noexcept {
    switch (reg) {
        case Register::pfp:
        case Register::sp:
        case Register::rip:
        case Register::r3:
        case Register::r4:
        case Register::r5:
        case Register::r6:
        case Register::r7:
        case Register::r8:
        case Register::r9:
        case Register::r10:
        case Register::r11:
        case Register::r12:
        case Register::r13:
        case Register::r14:
        case Register::r15:
        case Register::g0:
        case Register::g1:
        case Register::g2:
        case Register::g3:
        case Register::g4:
        case Register::g5:
        case Register::g6:
        case Register::g7:
        case Register::g8:
        case Register::g9:
        case Register::g10:
        case Register::g11:
        case Register::g12:
        case Register::g13:
        case Register::g14:
        case Register::fp:
            return true;
        default:
            return false;
    }
}
constexpr bool longRegisterAligned(Register reg) noexcept {
    return valid(reg) && ((static_cast<uint8_t>(reg) & 0b1) == 0);
}
constexpr bool quadRegisterAligned(Register reg) noexcept {
    return valid(reg) && ((static_cast<uint8_t>(reg) & 0b11) == 0);
}
constexpr bool tripleRegisterAligned(Register reg) noexcept {
    return quadRegisterAligned(reg);
}

constexpr Ordinal encodeForSrcDest(Register value) noexcept {
    //0b00000000'11111000'00000000'00000000;
    return (static_cast<Ordinal>(value) & 0b11111) << 19;
}
template<typename T>
constexpr auto encodeForSrcDest(T value) noexcept {
    return encodeForSrcDest(static_cast<Register>(static_cast<ByteOrdinal>(value) & 0b11111));
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
template<typename T>
constexpr auto encodeForSrc2(T value) noexcept {
    return encodeForSrc2(static_cast<Register>(static_cast<ByteOrdinal>(value) & 0b11111));
}
template<typename T>
constexpr Ordinal encodeForSrc1(T value) noexcept {
    return static_cast<Ordinal>(value) & 0b11111;
}
static_assert(encodeForSrc2(Register::fp) == 0x00'07'C0'00);

constexpr auto encodeForAbase(Register value) noexcept { return encodeForSrc2(value); }
constexpr auto encodeForIndex(Register value) noexcept { return encodeForSrc1(value); }
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

enum class REGOpcodes : Ordinal {
    notbit = 0x580,
    op_and,
    andnot,
    setbit,
    notand,
    op_xor = 0x586,
    op_or,
    nor,
    xnor,
    op_not,
    ornot,
    clrbit,
    notor,
    nand,
    alterbit,
    addo,
    addi,
    subo,
    subi,
    shro = 0x598,
    shrdi = 0x59A,
    shri,
    shlo,
    rotate,
    shli,
    cmpo = 0x5a0,
    cmpi,
    concmpo = 0x5A2,
    concmpi = 0x5A3,
    cmpinco,
    cmpinci,
    cmpdeco,
    cmpdeci,
    scanbyte = 0x5AC,
    chkbit = 0x5AE,
    addc = 0x5B0,
    subc = 0x5B2,
    mov = 0x5CC,
    movl = 0x5DC,
    movt = 0x5EC,
    movq = 0x5FC,
    synmov = 0x600,
    synmovl,
    synmovq,
    cmpstr,
    movqstr,
    movstr,
    atmod = 0x610,
    atadd = 0x612,
    inspacc,
    ldphy,
    synld,
    fill = 0x617,
    spanbit  = 0x640,
    scanbit,
    daddc,
    dsubc,
    dmovt,
    modac,
    condrec,
    modify = 0x650,
    extract,
    modtc = 0x654,
    modpc,
    receive,
    calls = 0x660,
    send = 0x662,
    sendserv,
    resumprcs,
    scedprcs,
    saveprcs,
    condwait = 0x668,
    wait,
    signal,
    mark = 0x66B,
    fmark = 0x66C,
    flushreg = 0x66D,
    syncf = 0x66F,
    emul = 0x670,
    ediv = 0x671,
    ldtime = 0x673,
    cvtir = 0x674,
    cvtilr = 0x675,
    scalerl = 0x676,
    scaler = 0x677,
    atanr = 0x680,
    logepr = 0x681,
    logr = 0x682,
    remr = 0x683,
    cmpor = 0x684,
    cmpr = 0x685,
    sqrtr = 0x688,
    expr = 0x689,
    logbnr = 0x68A,
    roundr = 0x68B,
    sinr = 0x68C,
    cosr = 0x68D,
    tanr = 0x68E,
    classr = 0x68F,
    atanrl = 0x690,
    logeprl = 0x691,
    logrl = 0x692,
    remrl = 0x693,
    cmporl = 0x694,
    cmprl = 0x695,
    sqrtrl = 0x698,
    exprl = 0x699,
    logbnrl = 0x69A,
    roundrl = 0x69B,
    sinrl = 0x69C,
    cosrl = 0x69D,
    tanrl = 0x69E,
    classrl = 0x69F,
    cvtri = 0x6C0,
    cvtril = 0x6C1,
    cvtzri = 0x6C2,
    cvtzril = 0x6C3,
    movr = 0x6C9,
    movrl = 0x6D9,
    cpysre = 0x6E2,
    cpyrsre = 0x6E3,
    movre = 0x6E1,
    mulo = 0x701,
    remo = 0x708,
    divo = 0x70B,
    muli = 0x741,
    remi  = 0x748,
    modi = 0x749,
    divi = 0x74B,
    divr = 0x78B,
    mulr = 0x78C,
    subr = 0x78D,
    addr = 0x78F,
    divrl = 0x79B,
    mulrl = 0x79C,
    subrl = 0x79D,
    addrl = 0x79F,
};
    [[nodiscard]] constexpr bool isFPInstruction(REGOpcodes opcode) noexcept {
        switch (opcode) {
            case REGOpcodes::cvtri:
            case REGOpcodes::cvtril:
            case REGOpcodes::cvtzri:
            case REGOpcodes::cvtzril:
            case REGOpcodes::movr:
            case REGOpcodes::movrl:
            case REGOpcodes::cpysre:
            case REGOpcodes::cpyrsre:
            case REGOpcodes::atanr:
            case REGOpcodes::logepr:
            case REGOpcodes::logr:
            case REGOpcodes::remr:
            case REGOpcodes::cmpor:
            case REGOpcodes::cmpr:
            case REGOpcodes::sqrtr:
            case REGOpcodes::expr:
            case REGOpcodes::logbnr:
            case REGOpcodes::roundr:
            case REGOpcodes::sinr:
            case REGOpcodes::cosr:
            case REGOpcodes::tanr:
            case REGOpcodes::classr:
            case REGOpcodes::atanrl:
            case REGOpcodes::logeprl:
            case REGOpcodes::logrl:
            case REGOpcodes::remrl:
            case REGOpcodes::cmporl:
            case REGOpcodes::cmprl:
            case REGOpcodes::sqrtrl:
            case REGOpcodes::exprl:
            case REGOpcodes::logbnrl:
            case REGOpcodes::roundrl:
            case REGOpcodes::sinrl:
            case REGOpcodes::cosrl:
            case REGOpcodes::tanrl:
            case REGOpcodes::classrl:
            case REGOpcodes::cvtir:
            case REGOpcodes::cvtilr:
            case REGOpcodes::scaler:
            case REGOpcodes::scalerl:
            case REGOpcodes::movre:
            case REGOpcodes::divr:
            case REGOpcodes::mulr:
            case REGOpcodes::subr:
            case REGOpcodes::addr:
            case REGOpcodes::divrl:
            case REGOpcodes::mulrl:
            case REGOpcodes::subrl:
            case REGOpcodes::addrl:
                return true;
            default:
                return false;
        }
    }
/**
 * @brief Special form for fp based instructions which are shoved into the * literal field.
 */
enum class FPReg : ByteOrdinal {
     fp0 = 0,
     fp1,
     fp2,
     fp3,
     PositiveZeroPointZero = 0b10000,
     PositiveOnePointZero = 0b10110,
};
[[nodiscard]] constexpr bool valid(FPReg reg) noexcept {
    switch (reg) {
        case FPReg::fp0:
        case FPReg::fp1:
        case FPReg::fp2:
        case FPReg::fp3:
        case FPReg::PositiveZeroPointZero:
        case FPReg::PositiveOnePointZero:
            return true;
        default:
            return false;
    }
}


constexpr Ordinal encode(REGOpcodes opcode) noexcept {
    Ordinal conv = static_cast<Ordinal>(opcode) & 0xFFF;
    Ordinal lo4 = conv & 0xF;
    Ordinal hi8 = (conv >> 4) & 0xFF;
    Ordinal hi8Pos = hi8 << 24;
    Ordinal lo4Pos = lo4 << 7;
    return hi8Pos | lo4Pos;
}
using REGFormatOperand = std::variant<Register, FPReg, ByteOrdinal>;

constexpr Ordinal encodeSrcDest(REGFormatOperand operand) noexcept {
    return std::visit(Neutron::overloaded {
        [](Register reg) { return encodeForSrcDest(reg); },
        [](ByteOrdinal byte) { return encodeForSrcDest(byte) | BitM3Set_REG; },
        [](FPReg fp) { return encodeForSrcDest(fp) | BitM3Set_REG; },
        [](auto) { return 0xFFFF'FFFF; }
    }, operand);
}
constexpr Ordinal encodeSrc2(REGFormatOperand operand) noexcept {
    return std::visit(Neutron::overloaded {
        [](Register reg) { return encodeForSrc2(reg); },
        [](ByteOrdinal byte) { return encodeForSrc2(byte) | BitM2Set_REG; },
        [](FPReg fp) { return encodeForSrc2(fp) | BitM2Set_REG; },
        [](auto) { return 0xFFFF'FFFF; }
    }, operand);
}
constexpr Ordinal encodeSrc1(REGFormatOperand operand) noexcept {
    return std::visit(Neutron::overloaded {
        [](Register reg) { return encodeForSrc1(reg); },
        [](ByteOrdinal byte) { return encodeForSrc1(byte) | BitM1Set_REG; },
        [](FPReg fp) { return encodeForSrc1(fp) | BitM1Set_REG; },
        [](auto) { return 0xFFFF'FFFF; }
    }, operand);
}
// unlike the other formats, reg format instructions have many control bits so
// we want to actually make the design as simple as possible.
constexpr Ordinal encodeREG(REGOpcodes opcode, REGFormatOperand src1, REGFormatOperand src2, REGFormatOperand srcDest) noexcept {
    return encode(opcode) | encodeSrcDest(srcDest) | encodeSrc1(src1) | encodeSrc2(src2);
}
#define X(title, opcode) \
    constexpr Ordinal title ( Register src1, Register src2, Register srcDest) noexcept { return encodeREG(opcode , src1, src2, srcDest); } \
    constexpr Ordinal title ( ByteOrdinal src1, Register src2, Register srcDest) noexcept { return encodeREG(opcode , src1, src2, srcDest); } \
    constexpr Ordinal title ( Register src1, ByteOrdinal src2, Register srcDest) noexcept { return encodeREG(opcode , src1, src2, srcDest); } \
    constexpr Ordinal title ( ByteOrdinal src1, ByteOrdinal src2, Register srcDest) noexcept { return encodeREG(opcode , src1, src2, srcDest); }
    X(op_and, REGOpcodes::op_and);
    X(andnot, REGOpcodes::andnot);
    X(op_xor, REGOpcodes::op_xor);
    X(op_or, REGOpcodes::op_or);
    X(nor, REGOpcodes::nor);
    X(xnor, REGOpcodes::xnor);
    X(ornot, REGOpcodes::ornot);
    X(notor, REGOpcodes::notor);
    X(nand, REGOpcodes::nand);
    X(addo, REGOpcodes::addo);
    X(addi, REGOpcodes::addi);
    X(subo, REGOpcodes::subo);
    X(subi, REGOpcodes::subi);
    X(shro, REGOpcodes::shro);
    X(shrdi, REGOpcodes::shrdi);
    X(shri, REGOpcodes::shri);
    X(shlo, REGOpcodes::shlo);
    X(rotate, REGOpcodes::rotate);
    X(shli, REGOpcodes::shli);
    X(cmpinco, REGOpcodes::cmpinco);
    X(cmpinci, REGOpcodes::cmpinci);
    X(cmpdeco, REGOpcodes::cmpdeco);
    X(cmpdeci, REGOpcodes::cmpdeci);
    X(addc, REGOpcodes::addc);
    X(subc, REGOpcodes::subc);
    X(mulo, REGOpcodes::mulo);
    X(muli, REGOpcodes::muli);
    X(divo, REGOpcodes::divo);
    X(divi, REGOpcodes::divi);


#undef X
#define X(title, opcode) \
    constexpr Ordinal title ( Register bitpos, Register src, Register dest) noexcept { return encodeREG(opcode , bitpos, src, dest); } \
    constexpr Ordinal title ( ByteOrdinal bitpos, Register src, Register dest) noexcept { return encodeREG(opcode , bitpos, src, dest); } \
    constexpr Ordinal title ( Register bitpos, ByteOrdinal src, Register dest) noexcept { return encodeREG(opcode , bitpos, src, dest); } \
    constexpr Ordinal title ( ByteOrdinal bitpos, ByteOrdinal src, Register dest) noexcept { return encodeREG(opcode , bitpos, src, dest); }
    X(notbit, REGOpcodes::notbit);
    X(setbit, REGOpcodes::setbit);
    X(clrbit, REGOpcodes::clrbit);
    X(alterbit, REGOpcodes::alterbit);
#undef X
constexpr Ordinal chkbit(Register bitpos, Register src) noexcept { return encodeREG(REGOpcodes::chkbit, bitpos, src, Register::ignore); }
constexpr Ordinal chkbit(ByteOrdinal bitpos, Register src) noexcept { return encodeREG(REGOpcodes::chkbit , bitpos, src, Register::ignore); }
constexpr Ordinal chkbit(Register bitpos, ByteOrdinal src) noexcept { return encodeREG(REGOpcodes::chkbit , bitpos, src, Register::ignore); }
constexpr Ordinal chkbit(ByteOrdinal bitpos, ByteOrdinal src) noexcept { return encodeREG(REGOpcodes::chkbit , bitpos, src, Register::ignore); }
constexpr Ordinal extract ( Register bitpos, Register len, Register srcDest) noexcept { return encodeREG(REGOpcodes::extract , bitpos, len, srcDest); }
constexpr Ordinal extract ( ByteOrdinal bitpos, Register len, Register srcDest) noexcept { return encodeREG(REGOpcodes::extract , bitpos, len, srcDest); }
constexpr Ordinal extract ( Register bitpos, ByteOrdinal len, Register srcDest) noexcept { return encodeREG(REGOpcodes::extract , bitpos, len, srcDest); }
constexpr Ordinal extract ( ByteOrdinal bitpos, ByteOrdinal len, Register srcDest) noexcept { return encodeREG(REGOpcodes::extract , bitpos, len, srcDest); }
#define X(title, opcode) \
    constexpr Ordinal title (Register src, Register srcDest) noexcept { return encodeREG(opcode, src, Register::ignore, srcDest); } \
    constexpr Ordinal title (ByteOrdinal src, Register srcDest) noexcept { return encodeREG(opcode, src, Register::ignore, srcDest); }
    X(op_not, REGOpcodes::op_not);
    X(mov, REGOpcodes::mov);
    X(movl, REGOpcodes::movl);
    X(movt, REGOpcodes::movt);
    X(movq, REGOpcodes::movq);
#undef X

#define X(title, opcode) \
    constexpr Ordinal title ( Register src1, Register src2) noexcept { return encodeREG(opcode , src1, src2, Register::ignore); } \
    constexpr Ordinal title ( ByteOrdinal src1, Register src2) noexcept { return encodeREG(opcode , src1, src2, Register::ignore); } \
    constexpr Ordinal title ( Register src1, ByteOrdinal src2) noexcept { return encodeREG(opcode , src1, src2, Register::ignore); } \
    constexpr Ordinal title ( ByteOrdinal src1, ByteOrdinal src2) noexcept { return encodeREG(opcode , src1, src2, Register::ignore); }
    X(cmpo, REGOpcodes::cmpo);
    X(cmpi, REGOpcodes::cmpi);
    X(concmpo, REGOpcodes::concmpo);
    X(concmpi, REGOpcodes::concmpi);
    X(scanbyte, REGOpcodes::scanbyte);
#undef X
    constexpr auto synmov(Register dest, Register src) noexcept { return encodeREG(REGOpcodes::synmov, dest, src, Register::ignore); }
    constexpr auto synmovl(Register dest, Register src) noexcept { return encodeREG(REGOpcodes::synmovl, dest, src, Register::ignore); }
    constexpr auto synmovq(Register dest, Register src) noexcept { return encodeREG(REGOpcodes::synmovq, dest, src, Register::ignore); }
    constexpr auto synld(Register src, Register dest) noexcept { return encodeREG(REGOpcodes::synld, src, Register::ignore, dest); }
enum class MEMOpcodes : Ordinal {
    ldob = 0x80,
    stob = 0x82,
    bx = 0x84,
    balx = 0x85,
    callx = 0x86,
    ldos = 0x88,
    stos = 0x8A,
    lda = 0x8C,
    ld = 0x90,
    st = 0x92,
    ldl = 0x98,
    stl = 0x9A,
    ldt = 0xA0,
    stt = 0xA2,
    ldq = 0xB0,
    stq = 0xB2,
    ldib = 0xC0,
    stib = 0xC2,
    ldis = 0xC8,
    stis = 0xCA,
};
constexpr bool valid(MEMOpcodes opcodes) noexcept {
    switch (opcodes) {
        case MEMOpcodes::ldob:
        case MEMOpcodes::stob:
        case MEMOpcodes::bx:
        case MEMOpcodes::balx:
        case MEMOpcodes::callx:
        case MEMOpcodes::ldos:
        case MEMOpcodes::stos:
        case MEMOpcodes::lda:
        case MEMOpcodes::ld:
        case MEMOpcodes::st:
        case MEMOpcodes::ldl:
        case MEMOpcodes::stl:
        case MEMOpcodes::ldt:
        case MEMOpcodes::stt:
        case MEMOpcodes::ldq:
        case MEMOpcodes::stq:
        case MEMOpcodes::ldib:
        case MEMOpcodes::stib:
        case MEMOpcodes::ldis:
        case MEMOpcodes::stis:
            return true;
        default:
            return false;
    }
}
enum class MEMBMode : ByteOrdinal {
    abase = 0b0100,
    ipPlusDisplacement = 0b0101,
    abasePlusIndexTimesScale = 0b0111,
    displacement = 0b1100,
    abasePlusDisplacement = 0b1101,
    indexTimesScalePlusDisplacement = 0b1110,
    abasePlusIndexTimesScalePlusDisplacement = 0b1111,
};
constexpr bool valid(MEMBMode mode) noexcept {
    switch (mode) {
        case MEMBMode::abase:
        case MEMBMode::ipPlusDisplacement:
        case MEMBMode::abasePlusIndexTimesScale:
        case MEMBMode::displacement:
        case MEMBMode::abasePlusDisplacement:
        case MEMBMode::indexTimesScalePlusDisplacement:
        case MEMBMode::abasePlusIndexTimesScalePlusDisplacement:
            return true;
        default:
            return false;
    }
}
constexpr bool isDoubleWideInstruction(MEMBMode mode) noexcept {
    switch (mode) {
        case MEMBMode::ipPlusDisplacement:
        case MEMBMode::displacement:
        case MEMBMode::abasePlusDisplacement:
        case MEMBMode::indexTimesScalePlusDisplacement:
        case MEMBMode::abasePlusIndexTimesScalePlusDisplacement:
            return true;
        default:
            return false;
    }
}
constexpr Ordinal encode(MEMOpcodes opcode) noexcept {
    return (static_cast<Ordinal>(opcode) << 24) & 0xFF00'0000;
}
constexpr Ordinal encode(MEMBMode mode) noexcept {
    return (static_cast<Ordinal>(mode) & 0b1111) << 10;
}
constexpr EncodedInstruction encodeMEMA(MEMOpcodes opcode, Register srcDest, Register abase, bool mode, Ordinal offset) noexcept {
    return encode(opcode) | encodeForSrcDest(srcDest) | encodeForAbase(abase) | (mode ? ModeSet_MEMA : 0) | (offset & 0x00000'FFF);
}

constexpr EncodedInstruction encodeMEMB(MEMOpcodes opcode, Register srcDest, Register abase, MEMBMode mode, ByteOrdinal scale = 0, Register index = Register::ignore, Integer displacement = 0) noexcept {
    Ordinal lower = encode(opcode) | encodeForSrcDest(srcDest) | encodeForAbase(abase) | encode(mode) | (static_cast<Ordinal>(scale & 0b111) << 7) | encodeForIndex(index);
    if (isDoubleWideInstruction(mode)) {
        return DoubleInstruction{ lower, displacement };
    } else {
        return lower;
    }
}
struct TreatAsIPRelative { };
constexpr auto encodeAbaseMEM(MEMOpcodes opcode, Register srcDest, Register abase) noexcept {
    return encodeMEMB(opcode, srcDest, abase, MEMBMode::abase, 0, Register::ignore, 0);
}
constexpr auto encodeOffset(MEMOpcodes opcode, Register srcDest, Ordinal offset) noexcept {
    return encodeMEMA(opcode, srcDest, Register::ignore, false, offset);
}
constexpr auto encodeAbasePlusOffset(MEMOpcodes opcode, Register srcDest, Register abase, Ordinal offset) noexcept {
    if (offset == 0) {
        // no need to waste two instructions
        return encodeAbaseMEM(opcode, srcDest, abase);
    } else {
        return encodeMEMA(opcode, srcDest, abase, true, offset);
    }
}
constexpr auto encodeAbasePlusDisplacementMEM(MEMOpcodes opcode, Register srcDest, Register abase, Integer displacement) noexcept {
    if (displacement == 0) {
        // no need to waste two instructions
        return encodeAbaseMEM(opcode, srcDest, abase);
    } else if (displacement < 4096 && displacement > 0) {
        // use MEMA instead of MEMB to save space
        return encodeAbasePlusOffset(opcode, srcDest, abase, displacement);
    } else {
        return encodeMEMB(opcode, srcDest, abase, MEMBMode::abasePlusDisplacement, 0, Register::ignore, displacement);
    }
}
constexpr auto encodeIPRelativeMEM(MEMOpcodes opcode, Register srcDest, Integer displacement) noexcept {
    return encodeMEMB(opcode, srcDest, Register::ignore, MEMBMode::ipPlusDisplacement, 0, Register::ignore,
                      displacement);
}
constexpr auto encodeAbasePlusIndexTime2ToTheScaleMEM(MEMOpcodes opcode, Register srcDest, Register abase, ByteOrdinal scale, Register index) noexcept {
    /// @todo sanity check scale
    return encodeMEMB(opcode, srcDest, abase, MEMBMode::abasePlusIndexTimesScale, scale, index, 0);
}
constexpr auto encodeIndexTimesScalePlusDisplacementMEM(MEMOpcodes opcode, Register srcDest, ByteOrdinal scale, Register index, Integer displacement) noexcept {
    return encodeMEMB(opcode, srcDest, Register::ignore, MEMBMode::indexTimesScalePlusDisplacement, scale, index, displacement);
}
constexpr auto encodeAbasePlusIndexTimesScalePlusDisplacementMEM(MEMOpcodes opcode, Register srcDest, Register abase, ByteOrdinal scale, Register index, Integer displacement) noexcept {
    if (displacement == 0) {
        return encodeAbasePlusIndexTime2ToTheScaleMEM(opcode, srcDest, abase, scale, index);
    } else {
        return encodeMEMB(opcode, srcDest, abase, MEMBMode::abasePlusIndexTimesScalePlusDisplacement, scale, index,
                          displacement);
    }
}
constexpr auto encodeDisplacementMEM(MEMOpcodes opcode, Register srcDest, Integer displacement) noexcept {
    if (displacement >= 0 && displacement < 4096) {
        // use MEMA instead of MEMB to save space
        return encodeOffset(opcode, srcDest, displacement);
    } else {
        return encodeMEMB(opcode, srcDest, Register::ignore, MEMBMode::displacement, 0, Register::ignore, displacement);
    }
}
#define X(title, opcode) \
    constexpr auto title ( Register srcDest, Register abase) noexcept { return encodeAbaseMEM(opcode, srcDest, abase); } \
    constexpr auto title ( Register srcDest, Register abase, Integer displacement) noexcept { return encodeAbasePlusDisplacementMEM(opcode, srcDest, abase, displacement); } \
    constexpr auto title ( Register srcDest, Integer displacement) noexcept { return encodeDisplacementMEM(opcode, srcDest, displacement); } \
    constexpr auto title ( Register srcDest, Integer displacement, TreatAsIPRelative) noexcept { return encodeIPRelativeMEM(opcode, srcDest, displacement); } \
    constexpr auto title ( Register srcDest, Register index, ByteOrdinal scale, Integer displacement) noexcept { return encodeIndexTimesScalePlusDisplacementMEM(opcode, srcDest, scale, index, displacement); } \
    constexpr auto title ( Register srcDest, Register abase, Register index, ByteOrdinal scale) noexcept { return encodeAbasePlusIndexTime2ToTheScaleMEM(opcode, srcDest, abase, scale, index); } \
    constexpr auto title ( Register srcDest, Register abase, Register index, ByteOrdinal scale, Integer displacement) noexcept { return encodeAbasePlusIndexTimesScalePlusDisplacementMEM(opcode , srcDest , abase , scale , index , displacement ); }
X(ldob, MEMOpcodes::ldob);
X(stob, MEMOpcodes::stob);
X(balx, MEMOpcodes::balx);
X(ldos, MEMOpcodes::ldos);
X(stos, MEMOpcodes::stos);
X(lda, MEMOpcodes::lda);
X(ld, MEMOpcodes::ld);
X(st, MEMOpcodes::st);
X(ldl, MEMOpcodes::ldl);
X(stl, MEMOpcodes::stl);
X(ldt, MEMOpcodes::ldt);
X(stt, MEMOpcodes::stt);
X(ldq, MEMOpcodes::ldq);
X(stq, MEMOpcodes::stq);
X(ldib, MEMOpcodes::ldib);
X(stib, MEMOpcodes::stib);
X(ldis, MEMOpcodes::ldis);
X(stis, MEMOpcodes::stis);
#undef X

constexpr auto bx(Register abase) noexcept { return encodeAbaseMEM(MEMOpcodes::bx, Register::ignore, abase); }
constexpr auto bx(Register abase, Integer displacement) noexcept { return encodeAbasePlusDisplacementMEM(MEMOpcodes::bx, Register::ignore, abase, displacement); }
constexpr auto bx(Integer displacement) noexcept { return encodeDisplacementMEM(MEMOpcodes::bx, Register::ignore, displacement); }
constexpr auto bx(Integer displacement, TreatAsIPRelative) noexcept { return encodeIPRelativeMEM(MEMOpcodes::bx, Register::ignore, displacement); }
constexpr auto bx(Register abase, Register index, ByteOrdinal scale) noexcept { return encodeAbasePlusIndexTime2ToTheScaleMEM(MEMOpcodes::bx, Register::ignore, abase, scale, index); }
constexpr auto bx(Register index, ByteOrdinal scale, Integer displacement) noexcept { return encodeIndexTimesScalePlusDisplacementMEM(MEMOpcodes::bx, Register::ignore, scale, index, displacement); }
constexpr auto bx(Register abase, Register index, ByteOrdinal scale, Integer displacement) noexcept { return encodeAbasePlusIndexTimesScalePlusDisplacementMEM(MEMOpcodes::bx, Register::ignore, abase, scale, index, displacement); }
constexpr auto callx(Register abase) noexcept { return encodeAbaseMEM(MEMOpcodes::callx, Register::ignore, abase); }
constexpr auto callx(Register abase, Integer displacement) noexcept { return encodeAbasePlusDisplacementMEM(MEMOpcodes::callx, Register::ignore, abase, displacement); }
constexpr auto callx(Integer displacement) noexcept { return encodeDisplacementMEM(MEMOpcodes::callx, Register::ignore, displacement); }
constexpr auto callx(Integer displacement, TreatAsIPRelative) noexcept { return encodeIPRelativeMEM(MEMOpcodes::callx, Register::ignore, displacement); }
constexpr auto callx(Register abase, Register index, ByteOrdinal scale) noexcept { return encodeAbasePlusIndexTime2ToTheScaleMEM(MEMOpcodes::callx, Register::ignore, abase, scale, index); }
constexpr auto callx(Register index, ByteOrdinal scale, Integer displacement) noexcept { return encodeIndexTimesScalePlusDisplacementMEM(MEMOpcodes::callx, Register::ignore, scale, index, displacement); }
constexpr auto callx(Register abase, Register index, ByteOrdinal scale, Integer displacement) noexcept { return encodeAbasePlusIndexTimesScalePlusDisplacementMEM(MEMOpcodes::callx, Register::ignore, abase, scale, index, displacement); }

} // end namespace i960

#endif // end !defined(MAYA_I960_H__)
