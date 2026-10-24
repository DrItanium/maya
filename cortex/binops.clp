;maya
;Copyright (c) 2012-2026, Joshua Scoggins
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are met:
;    * Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;    * Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
; 
; code module to demonstrate different features of the new binary operations
(defmodule binary-operations
           (export deffunction 
                   ?ALL)
           (export defgeneric 
                   ?ALL)
           ; hide all defglobals for now
           (export defglobal
                   ?NONE))
(defglobal binary-operations
           ?*integer-bitwidth* = (* (bits-per-byte)
                                    (bytes-per-integer))
           ?*float-bitwidth* = (* (bits-per-byte)
                                  (bytes-per-float))
           ?*pointer-bitwidth* = (* (bits-per-byte)
                                    (bytes-per-pointer))
           ?*highest-valid-bit-index* = (- ?*integer-bitwidth* 
                                           1))
(deffunction binary-operations::bits-per-integer
             "Return the number of bits that make up a clips integer"
             ()
             ?*integer-bitwidth*)

(deffunction binary-operations::bits-per-float
             "Return the number of bits that make up a clips float"
             ()
             ?*float-bitwidth*)

(deffunction binary-operations::bits-per-pointer
             "Return the number of bits that a pointer is composed of on this target platform"
             ()
             ?*pointer-bitwidth*)
(deffunction binary-operations::valid-bit-index
             (?index)
             (<= 0
                 ?index
                 ?*highest-valid-bit-index*))
(deffunction binary-operations::valid-bit-width
             (?index)
             (<= 1
                 ?index
                 ?*integer-bitwidth*))
(defgeneric binary-operations::make-bitwidth-mask
            "Constructs a bitmask given a width")
(defmethod binary-operations::make-bitwidth-mask
  ((?width INTEGER
           (valid-bit-width ?current-argument)))
  (- (bitwise-left-shift 1 
                         ?width) 1))
(defmethod binary-operations::make-bitwidth-mask
  ((?width INTEGER))
  FALSE)

(defgeneric binary-operations::extract-bit
            "extract a single bit out of an integer and convert to a boolean")

(defmethod binary-operations::extract-bit
  ((?value INTEGER)
   (?index INTEGER
           (valid-bit-index ?current-argument)))
  (bitwise-chkbit ?value
                  ?index))

(defmethod binary-operations::extract-bit
  ((?value INTEGER)
   (?index INTEGER))
  FALSE)
