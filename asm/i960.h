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
using EncodedInstruction = std::tuple<Address, std::variant<Ordinal, LongOrdinal>>;
enum class InstructionClass : ByteOrdinal {
    Unknown,
    REG,
    COBR,
    CTRL,
    MEM,
};
enum class InstructionOpcode : ShortOrdinal {
    Unknown = 0x00,
    // CTRL
    b = 0x08,
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
    // COBR
    testno,
    testg,
    teste,
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
    // MEM
    ldob = 0x80,
    stob = 0x82,
    bx = 0x84,
    balx,
    callx,
    ldos = 0x88,
    stos = 0x8a,
    lda = 0x8c,
    ld = 0x90,
    st = 0x92,
    ldl = 0x98,
    sdl = 0x9a,
    ldt = 0xa0,
    stt = 0xa2,
    ldq = 0xb0,
    stq = 0xb2,
    ldib = 0xc0,
    stib = 0xc2,
    ldis = 0xc8,
    stis = 0xca,
    // REG Instruction
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
    cmpob,
    cmpib,
    cmpos,
    cmpis,
    shro,
    shrdi = 0x59a,
    shri,
    shlo,
    rotate,
    shli,
    cmpo = 0x5a0,
    cmpi,
    concmpo,
    concmpi,
    cmpinco,
    cmpinci,
    cmpdeco,
    cmpdeci,
    scanbyte = 0x5ac,
    chkbit = 0x5ae,
    addc = 0x5b0,
    subc = 0x5b2,
    mov = 0x5cc,
    eshro = 0x5d8,
    movl = 0x5dc,
    movt = 0x5ec,
    movq = 0x5fc,
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
    spanbit = 0x640,
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
    schedprcs,
    saveprcs,
    condwait = 0x668,
    wait,
    signal,
    mark,
    fmark,
    flushreg,
    syncf = 0x66f,
    emul,
    ediv,
    ldtime = 0x673,
    cvtir, 
    cvtilr,
    scalerl,
    scaler,
    atanr = 0x680,
    logepr,
    logr,
    remr,
    cmpor,
    cmpr,
    sqrtr = 0x688,
    expr,
    logbnr,
    roundr,
    sinr,
    cosr,
    tanr,
    classr,
    atanrl,
    logeprl,
    logrl,
    remrl,
    cmporl,
    cmprl,
    sqrtrl = 0x698,
    exprl,
    logbnrl,
    roundrl,
    sinrl,
    cosrl,
    tanrl,
    classrl,
    cvtri,
    cvtril,
    cvtzri,
    cvtzril,
    movr = 0x6c9,
    movrl = 0x6d9,
    cpysre = 0x6e2,
    cpyrsre,
    movre = 0x6e9,
    mulo = 0x701,
    remo = 0x708,
    divo = 0x70b,
    muli = 0x741,
    remi = 0x748,
    modi,
    divi = 0x74b,
    divr = 0x78b,
    mulr,
    subr,
    addr = 0x78f,
    divrl = 0x79b,
    mulrl,
    subrl,
    addrl = 0x79f,
};
[[nodiscard]] constexpr auto determineInstructionClass(ByteOrdinal opcode) noexcept {
    switch (opcode) {
        case 0x08 ... 0x1f:
            return InstructionClass::CTRL;
        case 0x20 ... 0x3f:
            return InstructionClass::COBR;
        case 0x58 ... 0x7f:
            return InstructionClass::REG;
        case 0x80 ... 0xff:
            return InstructionClass::MEM;
        default:
            return InstructionClass::Unknown;
    }
}
[[nodiscard]] constexpr auto determineInstructionClass(InstructionOpcode opcode) noexcept {
    switch (static_cast<ShortOrdinal>(opcode)) {
        case 0x08 ... 0x1f:
            return InstructionClass::CTRL;
        case 0x20 ... 0x3f:
            return InstructionClass::COBR;
        case 0x580 ... 0x7ff:
            return InstructionClass::REG;
        case 0x80 ... 0xff:
            return InstructionClass::MEM;
        default:
            return InstructionClass::Unknown;
    }
}
union [[gnu::packed]] Instruction {
    LongOrdinal full;
    Ordinal halves[sizeof(LongOrdinal)/sizeof(Ordinal)];
    struct {
        Ordinal : 24;
        Ordinal opcode : 8;
    } generic;
    static_assert(sizeof(generic) == sizeof(Ordinal));
    struct {
        Ordinal src1 : 5;
        Ordinal : 2;
        Ordinal opcodeLo : 4;
        Ordinal m1 : 1;
        Ordinal m2 : 1;
        Ordinal m3 : 1;
        Ordinal src2 : 5;
        Ordinal srcDest : 5;
        Ordinal opcode : 8;
    } reg;
    static_assert(sizeof(reg) == sizeof(Ordinal));
    struct {
        Ordinal s2 : 1;
        Ordinal t : 1;
        Ordinal displacement : 11;
        Ordinal m1 : 1;
        Ordinal src2 : 5;
        Ordinal src1 : 5;
        Ordinal opcode : 8;
    } cobr;
    static_assert(sizeof(cobr) == sizeof(Ordinal));
    struct {
        Ordinal : 2;
        Ordinal displacement : 22;
        Ordinal opcode : 8;
    } ctrl;
    static_assert(sizeof(ctrl) == sizeof(Ordinal));
    struct {
        Ordinal : 12;
        Ordinal mode : 1;
        Ordinal : 19;
    } memoryGeneric;
    static_assert(sizeof(memoryGeneric) == sizeof(Ordinal));
    struct {
        Ordinal offset : 12;
        Ordinal mode : 2; 
        Ordinal abase : 5;
        Ordinal srcDest : 5;
        Ordinal opcode : 8;
    } mema;
    static_assert(sizeof(mema) == sizeof(Ordinal));
    struct {
        Ordinal index : 5;
        Ordinal : 2;
        Ordinal scale : 3;
        Ordinal mode : 4; 
        Ordinal abase : 5;
        Ordinal srcDest : 5;
        Ordinal opcode : 8;
        Integer optionalDisplacement;
    } memb;
    static_assert(sizeof(memb) == sizeof(LongOrdinal));
    [[nodiscard]] constexpr auto getInstructionClass() const noexcept {
        return determineInstructionClass(generic.opcode);
    }
    [[nodiscard]] constexpr InstructionOpcode getOpcode() const noexcept {
        ShortOrdinal so = generic.opcode;
        if (getInstructionClass() == InstructionClass::REG) {
            so <<= 4;
            so |= reg.opcodeLo;
        } 
        return static_cast<InstructionOpcode>(so);
    }
};

} // end namespace i960

#endif // end !defined(MAYA_I960_H__)
