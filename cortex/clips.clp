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

(include cortex/common.clp)
(include cortex/tokenizer.clp)

(defmodule finish-clips-parsing
           "Last stage in the clips processing setup which marks the translation-unit as finished"
           (import tokenizer
                   defclass translation-unit))

(defrule finish-clips-parsing::finish-up
         ?obj <- (object (is-a translation-unit)
                         (status TOKENIZED))
         =>
         (modify-instance ?obj
                          (status PARSED)))
(defmodule clips
           "top-level clips module that is used for parsing functionality"
           (import common
                   ?ALL)
           (import tokenizer
                   ?ALL)
           (import constraint-identification
                   ?ALL)
           (import wildcard-identification
                   ?ALL)
           (import variable-identification
                   ?ALL)
           (export ?ALL))


(defrule clips::do-clips-parsing
         (object (is-a translation-unit)
                 (status UNPROCESSED)
                 (valid TRUE))
         =>
         ; this design should allow one to load files after the fact
         ; linkage would take place after parsing is done!
         (printout stderr
                   "Found an unprocessed translation-unit!" crlf)
         (focus tokenizer
                raise-constants
                constraint-identification
                wildcard-identification
                variable-identification
                finish-clips-parsing))

(defrule clips::leave-clips
         (declare (salience -10000))
         =>
         (printout stderr
                   "leaving clips module" crlf))

(defrule clips::enter-clips
         (declare (salience 10000))
         =>
         (printout stderr
                   "entering clips module" crlf))

