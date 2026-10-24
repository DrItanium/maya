;maya
;Copyright (c) 2012-2025, Joshua Scoggins
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
(defmodule common
           (export ?ALL))

(defclass common::has-parent
  (is-a USER)
  (slot parent
        (type SYMBOL 
              INSTANCE)
        (allowed-symbols FALSE)
        (storage local)
        (visibility public))
  (message-handler has-parent primary))

(defmessage-handler common::has-parent has-parent primary
                    (?parent)
                    (or (eq ?self:parent
                            ?parent)
                        (send ?self:parent
                              has-parent
                              ?parent)))
(defmessage-handler PRIMITIVE has-parent primary
                    (?parent)
                    FALSE)


(defclass common::container
  (is-a has-parent)
  (multislot contents
             (storage local)
             (visibility public)))

(defclass common::has-description
  (is-a USER)
  (slot description
        (type STRING)
        (storage local)
        (visibility public)))

(defclass common::has-title
  (is-a USER)
  (slot title
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE)))


(deffunction common::contains$
             (?looking-for $?in)
             (not (neq ?looking-for
                       (expand$ ?in))))

(defclass common::has-kind
  (is-a USER)
  (slot kind
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass common::has-single-value
  (is-a USER)
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass common::has-comment
  (is-a USER)
  (slot comment
        (type STRING)
        (storage local)
        (visibility public)
        (default-dynamic "")))

; --- technically these are redundant but act as a good sanity check
(defgeneric common::token-constantp
            "Can the given type be considered a token constant?")
(defmethod common::token-constantp
  ((?type SYMBOL))
  (switch ?type
          (case SYMBOL then TRUE)
          (case STRING then TRUE)
          (case INTEGER then TRUE)
          (case FLOAT then TRUE)
          (default FALSE)))
