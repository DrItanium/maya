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
; Create i960 assembly instructions

(defmodule i960
           (export ?ALL))

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

; now with the underlying core instruction kinds
(defgeneric i960::reg-format-instruction)
(defgeneric i960::cobr-format-instruction)
(defgeneric i960::ctrl-format-instruction)
(defgeneric i960::mem-format-instruction)

(defmethod i960::reg-format-instruction
  ((?opcode SYMBOL)
   (?src1 SYMBOL
          INTEGER
          (<= 0 ?current-argument 31))
   (?src2 SYMBOL
          INTEGER
          (<= 0 ?current-argument 31))
   (?srcDest SYMBOL
             INTEGER
             (<= 0 ?current-argument 31)))
  (format nil
          "%s %s, %s, %s"
          ?opcode
          (sym-cat ?src1)
          (sym-cat ?src2)
          (sym-cat ?srcDest)))
; For CTRL and COBR, the lowest two bits of the displacement are going to be clear in the encoding
(defmethod i960::ctrl-format-instruction
  ((?opcode SYMBOL)
   (?displacement LEXEME
                  INTEGER))
  (format nil
          "%s %s"
          ?opcode
          (str-cat ?displacement)))

(defmethod i960::cobr-format-instruction
  ((?opcode SYMBOL)
   (?src1 SYMBOL
          INTEGER
          (<= 0 ?current-argument 31))
   (?src2 SYMBOL)
   (?displacement LEXEME
                  INTEGER))
  (format nil
          "%s %s, %s, %s"
          (sym-cat ?src1)
          ?src2
          (str-cat ?displacement)))

            
