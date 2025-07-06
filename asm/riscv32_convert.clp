; 
; @file
; i960 Instruction Encoding
; @copyright
; maya-app
; Copyright (c) 2012-2025, Joshua Scoggins
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
; 
; Create i960 assembly instructions from an rv32 base operation set

(defmodule i960
           (export ?ALL))

(defmodule riscv32
           (import i960 
                   ?ALL)
           (export ?ALL))

(defmodule MAIN
           (import i960
                   ?ALL)
           (import riscv32
                   ?ALL))

; start with the core pseudo-instruction
(defgeneric i960::ldconst)

(defmethod i960::ldconst 
  ((?value LEXEME)
   (?destination SYMBOL))
  (format nil
          "ldconst %s, %s"
          ?value
          ?destination))

(defmethod i960::ldconst
  ((?value NUMBER)
   (?destination SYMBOL))
  (ldconst (str-cat ?value)
           ?destination))

(defmethod i960::one-arg-instruction
  ((?opcode SYMBOL)
   (?arg0 LEXEME
          NUMBER)
   (?comment LEXEME))
  (format nil
          "%s %s # %s"
          ?opcode
          (str-cat ?arg0)
          ?comment))
(defmethod i960::two-arg-instruction
  ((?opcode SYMBOL)
   (?arg0 LEXEME
          NUMBER)
   (?arg1 LEXEME
          NUMBER)
   (?comment STRING))
  (format nil
          "%s %s, %s # %s"
          ?opcode
          (str-cat ?arg0)
          (str-cat ?arg1)
          ?comment))

(defmethod i960::three-arg-instruction
  ((?opcode SYMBOL)
   (?arg0 LEXEME
          NUMBER)
   (?arg1 LEXEME
          NUMBER)
   (?arg2 LEXEME
          NUMBER)
   (?comment STRING))
  (format nil
          "%s %s, %s, %s # %s"
          ?opcode
          (str-cat ?arg0)
          (str-cat ?arg1)
          (str-cat ?arg2)
          ?comment))

(defgeneric i960::register-convert:rv32->i960
            "convert the given riscv32 register to an i960 register")
; the riscv32 environment being emulated has the following march designation
; RV32EIMACBU_Zfinx
; I am using picorv32 as a good reference for the march designation
; There will be modifications but the E designator is very important as there is no way I can map all 32 registers safely
; If I decide to add support for floating point then Zfinx is important as the i960 only has four extra fp registers and is meant to use normal registers like Zfinix
; The atomic options are also somewhat questionable but the i960 has them
; The compressed ops are there just in case but really don't add anything special (they may get removed)
; The bit manipulation extensions are important
; The multiply ops are definitely going in
; The U instruction is something I saw on the picorv32 project but I may remove
; I am not against adding custom rv32 instructions (like you see with the picorv32)
; Here is the register layout:
; x0/zero -> g11 if a source register is needed
;            0 if a source immediate is needed/allowed
;            r3 if zero is a destination
;            we have to do this multiple register allocation so that things like loading to the zero register can take proper place
;            It is also important to note that g11 being zero is meant to be more accurate to how rv32 works
; x1/ra -> g14 (i960 makes this the default link register, lets keep that)
; x2/sp -> sp/r1 (keeping it to the hardware stack pointer means I can take advantage of i960 faults, interrupts and service calls safely)
;          One thing to keep in mind is that the stack grows upwards towards higher address on the i960 so some modifications will be needed to offsets
; x3/gp -> g12
; x4/tp -> g13
; x5/t0 -> g6
; x6/t1 -> g7
; x7/t2 -> g8
; x8/s0/fp -> g9 (rv32's fp is not the same as i960's fp so we need to act accordingly)
; x9/s1    -> g10 
; x10/a0 -> g0
; x11/a1 -> g1
; x12/a2 -> g2
; x13/a3 -> g3
; x14/a4 -> g4
; x15/a5 -> g5
; These registers are for internal i960 tracking and will not be directly used by the translator
; pfp/r0 -> previous frame pointer (i960 only) 
; rip/r2 -> return instruction pointer (i960 only) 
; fp/g15 -> frame pointer (i960 only)
; The rest of the free i960 registers are allocated as such:
; r4 -> at ; assembler temporary used for emulation of rv32 instructions through i960 instructions
; r5 -> 0x0000'0FFF ; Holds the mask for the lower 12-bits of a 32-bit value (useful for %lo emulation)
; r6 -> 0xFFFF'F000 ; Holds the mask for the upper 20-bits of a 32-bit value (useful for %hi emulation)
; r7-r15 -> free for other use (probably more registers
(defglobal i960
           ?*zero-source-register* = g11
           ?*zero-dest-register* = r3
           ?*ra-register* = g14
           ?*gp-register* = g12
           ?*tp-register* = g13
           ?*t0-register* = r4 
           ?*t1-register* = r5
           ?*t2-register* = r6
           ?*s0-register* = g6
           ?*s1-register* = g7
           ?*a0-register* = g0
           ?*a1-register* = g1
           ?*a2-register* = g2
           ?*a3-register* = g3
           ?*a4-register* = g4
           ?*a5-register* = g5
           ; these are for internal purposes only but can be referenced in hand written assembly
           ?*t3-register* = r7 ; aka assembler temporary
           ?*t4-register* = r8 ; 
           ?*t5-register* = r9 ;
           ?*t6-register* = r10 ;
           ; s3 and s2 are not mapped at all!
           ?*s4-register* = r15
           ?*s5-register* = r14
           ?*s6-register* = r13
           ?*s7-register* = r12 
           ?*s8-register* = r11 
           ?*s9-register* = g8 ; aka free for context transfer
           ?*s10-register* = g9 ; aka hi-mask
           ?*s11-register* = g10 ; aka lo-mask
           ?*at-register* = ?*t3-register*
           )

(defmethod i960::register-convert:rv32->i960
  ((?register SYMBOL)
   (?kind SYMBOL))
  (switch ?register
          (case zero then 
            (switch ?kind
                    (case source-immediate then 0)
                    (case source-register then ?*zero-source-register*)
                    (case destination then ?*zero-dest-register*)
                    (default ?*zero-source-register*)))
          (case ra then ?*ra-register*)
          ; sp is known as sp as well on the i960
          (case gp then ?*gp-register*)
          (case tp then ?*tp-register*)
          (case t0 then ?*t0-register*)
          (case t1 then ?*t1-register*)
          (case t2 then ?*t2-register*)
          (case s0 then ?*s0-register*)
          (case fp then ?*s0-register*) ; this is the rv32 frame pointer
          (case s1 then ?*s1-register*)
          (case a0 then ?*a0-register*)
          (case a1 then ?*a1-register*)
          (case a2 then ?*a2-register*)
          (case a3 then ?*a3-register*)
          (case a4 then ?*a4-register*)
          (case a5 then ?*a5-register*)
          (case pc then ip) ; special case
          ; internal uses
          (case t3 then ?*t3-register*)
          (case t4 then ?*t4-register*)
          (case t5 then ?*t5-register*)
          (case t6 then ?*t6-register*)
          (case s4 then ?*s4-register*)
          (case s5 then ?*s5-register*)
          (case s6 then ?*s6-register*)
          (case s7 then ?*s7-register*)
          (case s8 then ?*s8-register*)
          (case s9 then ?*s9-register*)
          (case s10 then ?*s10-register*)
          (case s11 then ?*s11-register*)
          (case at then ?*at-register*)
          (default ?register)))


(defmethod i960::register-convert:rv32->i960
  ((?register SYMBOL))
  (register-convert:rv32->i960 ?register
                               gpr))


(defmethod riscv32::add
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?rs2 SYMBOL))
  (three-arg-instruction addo
                         (register-convert:rv32->i960 ?rs2)
                         (register-convert:rv32->i960 ?rs1)
                         (register-convert:rv32->i960 ?rd)
                         ""))

(defmethod riscv32::sub
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?rs2 SYMBOL))
  (three-arg-instruction subo
                         (register-convert:rv32->i960 ?rs2)
                         (register-convert:rv32->i960 ?rs1)
                         (register-convert:rv32->i960 ?rd)
                         ""))
(defmethod riscv32::xor
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?rs2 SYMBOL))
  (three-arg-instruction xor
                         (register-convert:rv32->i960 ?rs2)
                         (register-convert:rv32->i960 ?rs1)
                         (register-convert:rv32->i960 ?rd)
                         ""))


(defmethod riscv32::op-and
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?rs2 SYMBOL))
  (three-arg-instruction and
                         (register-convert:rv32->i960 ?rs2)
                         (register-convert:rv32->i960 ?rs1)
                         (register-convert:rv32->i960 ?rd)
                         ""))
(defmethod riscv32::op-andi
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?imm SYMBOL
         NUMBER))
  (create$ (ldconst ?imm 
                    (register-convert:rv32->i960 at))
           (op-and ?rd ?rs1 at)))
(defmethod riscv32::op-or
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?rs2 SYMBOL))
  (three-arg-instruction or
                         (register-convert:rv32->i960 ?rs2)
                         (register-convert:rv32->i960 ?rs1)
                         (register-convert:rv32->i960 ?rd)
                         ""))

(defmethod riscv32::op-ori
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?imm SYMBOL
         NUMBER))
  (create$ (ldconst ?imm 
                    (register-convert:rv32->i960 at))
           (op-or ?rd 
                  at ; make sure that it gets stashed in src2 for a bypass optimization
                  ?rs1)))

(defmethod riscv32::op-xori
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?imm SYMBOL
         NUMBER))
  (create$ (ldconst ?imm 
                    (register-convert:rv32->i960 at))
           (xor ?rd 
                at  ; make sure that it gets stashed in src2 for a bypass optimization
                ?rs1)))

(defmethod riscv32::sll
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?rs2 SYMBOL))
  (three-arg-instruction shlo
                         (register-convert:rv32->i960 ?rs2)
                         (register-convert:rv32->i960 ?rs1)
                         (register-convert:rv32->i960 ?rd)))

(defmethod riscv32::slli
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?rs2 INTEGER))
  (three-arg-instruction shlo
                         ?rs2
                         (register-convert:rv32->i960 ?rs1)
                         (register-convert:rv32->i960 ?rd)))

(defmethod riscv32::sra
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?rs2 SYMBOL))
  (three-arg-instruction shri
                         (register-convert:rv32->i960 ?rs2)
                         (register-convert:rv32->i960 ?rs1)
                         (register-convert:rv32->i960 ?rd)))
(defmethod riscv32::srai
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?shamt INTEGER))
  (three-arg-instruction shri
                         ?shamt
                         (register-convert:rv32->i960 ?rs1)
                         (register-convert:rv32->i960 ?rd)))

(defmethod riscv32::srl
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?rs2 SYMBOL))
  (three-arg-instruction shro
                         (register-convert:rv32->i960 ?rs2)
                         (register-convert:rv32->i960 ?rs1)
                         (register-convert:rv32->i960 ?rd)))
(defmethod riscv32::srli
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?shamt INTEGER))
  (three-arg-instruction shro
                         ?shamt
                         (register-convert:rv32->i960 ?rs1)
                         (register-convert:rv32->i960 ?rd)))

(defmethod riscv32::lui
  ((?rd SYMBOL)
   (?imm SYMBOL
         NUMBER))
  ; we want to emulate this as best as possible so the psuedo code is:
  ; and ?imm, 0xFFFF'F000, ?rd
  ; The implementation is wasteful since we will probably be adding the lower part back in 
  (create$ (ldconst ?imm
                    (register-convert:rv32->i960 at))
           (op-and ?rd
                   at
                   hi-mask)))
; @todo implement auipc and addi
(defmethod riscv32::slt
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?rs2 SYMBOL))
  (create$ (two-arg-instruction cmpi 
                                (register-convert:rv32->i960 ?rs1)
                                (register-convert:rv32->i960 ?rs2))
           (one-arg-instruction testl
                                (register-convert:rv32->i960 ?rd))))

(defmethod riscv32::slti
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?imm SYMBOL
         INTEGER))
  (create$ (ldconst ?imm
                    (register-convert:rv32->i960 at))
           (slt ?rd 
                ?rs1
                at)))

(defmethod riscv32::sltu
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?rs2 SYMBOL))
  (create$ (two-arg-instruction cmpo
                                (register-convert:rv32->i960 ?rs1)
                                (register-convert:rv32->i960 ?rs2))
           (one-arg-instruction testl
                                (register-convert:rv32->i960 ?rd))))

(defmethod riscv32::sltiu
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?imm SYMBOL
         INTEGER))
  (create$ (ldconst ?imm
                    (register-convert:rv32->i960 at))
           (sltu ?rd 
                 ?rs1
                 at)))

(defmethod riscv32::j
  ((?offset SYMBOL
            INTEGER))
  (one-arg-instruction b 
                       ?offset))

(defmethod riscv32::jr
  ((?rs1 SYMBOL))
  (one-arg-instruction bx
                       (format nil
                               "(%s)"
                               ?rs1)))

; @todo figure out jal since it is ?offset(ip) and thus we need to figure out the best way to call it!
(defmethod riscv32::ret () (jr ra))
; does call have other forms?
(defmethod riscv32::op-call 
  ((?func SYMBOL))
  (two-arg-instruction balx
                       ?func
                       (register-convert:rv32->i960 ra)))
          
(defmethod riscv32::jalr
  ((?rd SYMBOL)
   (?rs1 SYMBOL)
   (?offset LEXEME
            INTEGER))
  (two-arg-instruction balx
                       (format nil
                               "%s(%s)"
                               (if (and (integerp ?offset)
                                        (= ?offset 0)) then
                                 ""
                                 else
                                 (str-cat ?offset))
                               (register-convert:rv32->i960 ?rs1))
                       (register-convert:rv32->i960 ?rd)))
(defmethod riscv32::jalr
  ((?rs SYMBOL))
  (jalr ra
        ?rs
        0))
                       
(defmethod riscv32::la
  ((?rd SYMBOL)
   (?sym SYMBOL))
  (ldconst ?sym
           (register-convert:rv32->i960 ?rd)))
(defmethod riscv32::li
  ((?rd SYMBOL)
   (?imm INTEGER))
  (ldconst ?imm
           (register-convert:rv32->i960 ?rd)))

(defmethod riscv32::mv
  ((?rd SYMBOL)
   (?rs SYMBOL))
  (two-arg-instruction mov
                       (register-convert:rv32->i960 ?rs)
                       (register-convert:rv32->i960 ?rd)))

(defmethod riscv32::op-not
  ((?rd SYMBOL)
   (?rs SYMBOL))
  (two-arg-instruction not
                       (register-convert:rv32->i960 ?rs)
                       (register-convert:rv32->i960 ?rd)))

