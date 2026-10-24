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
(defmodule clips-common
           (import common
                   ?ALL)
           (import tokenizer
                   ?ALL)
           (export ?ALL))
(defclass clips-common::construct
  (is-a has-parent))
(defclass clips-common::titled-construct
  (is-a construct
        has-title
        has-comment))

(defmodule identify-global-constructs
           (import clips-common
                   ?ALL)
           (export ?ALL))
(defclass identify-global-constructs::defglobal-construct
  (is-a construct)
  (multislot target-module
        (type SYMBOL)
        (storage local)
        (visibility public)
        (cardinality 0 1)
        (default ?NONE))
  (multislot assignments
             (storage local)
             (visibility public)
             (default ?NONE)))

(defclass identify-global-constructs::global-assignment
  (is-a has-parent)
  (slot target-variable
        (type INSTANCE)
        (allowed-classes variable)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot expression
        (type INSTANCE)
        (storage local)
        (visibility public)
        (default ?NONE)))

;(defrule identify-global-constructs::find-legal-defglobal-construct



(defmodule finish-clips-parsing
           "Last stage in the clips processing setup which marks the translation-unit as parsed"
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

(defglobal clips
           ?*module-passes* = (create$ tokenizer
                                       raise-constants
                                       constraint-identification
                                       wildcard-identification
                                       variable-identification
                                       ; parsing phases
                                       identify-global-constructs
                                       ; must be last
                                       finish-clips-parsing))

                                       
(defrule clips::do-clips-parsing
         (object (is-a translation-unit)
                 (status UNPROCESSED)
                 (valid TRUE))
         =>
         ; this design should allow one to load files after the fact
         ; linkage would take place after parsing is done!
         (printout stderr
                   "Found an unprocessed translation-unit!" crlf)
         (focus (expand$ ?*module-passes*)))


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

