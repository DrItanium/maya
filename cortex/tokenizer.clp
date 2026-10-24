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

(defmodule tokenizer
           (import common
                   ?ALL)
           (export deftemplate
                   tracker)
           (export defclass
                   ?ALL))

(defclass tokenizer::expression
  (is-a container))
(defclass tokenizer::translation-unit
  (is-a expression)
  (slot parent
        (source composite)
        (access initialize-only)
        (default FALSE))
  (slot path
        (type LEXEME)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default ?NONE))
  (slot permissions
        (type LEXEME)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default-dynamic "r"))
  (slot logical-name
        (type SYMBOL)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default-dynamic (gensym*)))
  (slot valid
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public)
        (access initialize-only))
  (slot is-open
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public)
        (default FALSE))
  (message-handler init after))
(defmessage-handler tokenizer::translation-unit init after
                    ()
                    (bind ?self:valid
                          (bind ?self:is-open
                                (open ?self:path
                                      ?self:logical-name
                                      ?self:permissions))))




(defclass tokenizer::token
  (is-a has-parent
        has-kind)
  (multislot value
             (storage local)
             (visibility public)
             (default ?NONE)))

(deftemplate tokenizer::tracker
             (slot logical-name
                   (type SYMBOL)
                   (default ?NONE))
             (slot status
                   (type SYMBOL)
                   (allowed-symbols PROCESSING
                                    FINISHED
                                    ERROR)
                   (default PROCESSING))
             (slot translation-unit
                   (type INSTANCE)
                   (default ?NONE))
             (slot current-expression
                   (type INSTANCE)
                   (default ?NONE))
             (multislot current-token
                        (default ?NONE)))
(defrule tokenizer::create-tracker
         "Each translation-unit needs a corresponding tracker to work with"
         (object (is-a translation-unit)
                 (name ?tu)
                 (valid TRUE)
                 (logical-name ?lu))
         (not (tracker (translation-unit ?tu)))
         =>
         (assert (tracker (translation-unit ?tu)
                          (logical-name ?lu)
                          (current-expression ?tu)
                          (current-token (next-token ?lu)))))

(defrule tokenizer::create-token
         ?f <- (tracker (current-expression ?expr)
                        (logical-name ?ln)
                        (status PROCESSING)
                        (current-token ?kind 
                                       $?value))
         ?obj <- (object (is-a expression)
                         (name ?expr)
                         (contents $?contents))
         =>
         (modify-instance ?obj
                          (contents $?contents
                                    (make-instance of token
                                                   (parent ?expr)
                                                   (kind ?kind)
                                                   (value $?value))))
         (modify ?f
                 (current-token (next-token ?ln))))


(defrule tokenizer::enter-expression
         (declare (salience 1))
         ?f <- (tracker (current-expression ?expr)
                        (logical-name ?ln)
                        (status PROCESSING)
                        (current-token LEFT_PARENTHESIS $?))
         ?obj <- (object (is-a expression)
                         (name ?expr)
                         (contents $?contents))
         =>
         (modify-instance ?obj
                          (contents $?contents
                                    (bind ?new-expr
                                          (make-instance of expression
                                                         (parent ?expr)
                                                         (contents)))))
         (modify ?f
                 (current-expression ?new-expr)
                 (current-token (next-token ?ln))))

(defrule tokenizer::leave-expression
         (declare (salience 1))
         ?f <- (tracker (current-expression ?expr)
                        (translation-unit ~?expr)
                        (logical-name ?ln)
                        (status PROCESSING)
                        (current-token RIGHT_PARENTHESIS 
                                       $?))
         (object (is-a expression)
                 (name ?expr)
                 (parent ?p))
         =>
         (modify ?f
                 (current-expression ?p)
                 (current-token (next-token ?ln))))

(defrule tokenizer::leave-expression-and-fail
         (declare (salience 1))
         ?f <- (tracker (current-expression ?expr)
                        (translation-unit ?expr)
                        (logical-name ?ln)
                        (status PROCESSING)
                        (current-token RIGHT_PARENTHESIS 
                                       $?))
         =>
         (printout stdout
                   "Found an extra right paren on " ?ln ! crlf)
         (modify ?f
                 (status ERROR)
                 (current-token (next-token ?ln))))

(defrule tokenizer::finish-tokenization-success
         (declare (salience 1))
         ?f <- (tracker (current-expression ?expr)
                        (translation-unit ?expr)
                        (status PROCESSING)
                        (logical-name ?ln)
                        (current-token STOP $?))
         =>
         (modify ?f
                 (status FINISHED)))

(defrule tokenizer::finish-tokenization-fail
         (declare (salience 1))
         ?f <- (tracker (current-expression ?expr)
                        (translation-unit ~?expr)
                        (status PROCESSING)
                        (logical-name ?ln)
                        (current-token STOP 
                                       $?))
         =>
         (printout stdout
                   "End of the file before closing everything up on " ?ln ! crlf)
         (modify ?f
                 (status ERROR)))
(defmodule raise-constants
           "Custom pass that hoists constants like lexeme's and numbers out of tokens into direct CLIPS representations"
           (import tokenizer
                   ?ALL))

(defrule raise-constants::hoist-constant
         ?obj <- (object (is-a token)
                         (parent ?parent)
                         (kind INTEGER|FLOAT|SYMBOL|STRING)
                         (value ?value)
                         (name ?tok))
         ?expr <- (object (is-a expression)
                          (name ?parent)
                          (contents $?a ?tok $?b))
         =>
         (unmake-instance ?obj)
         (modify-instance ?expr
                          (contents ?a ?value ?b)))

(defmodule tokens->wildcards
           "custom pass which converts a wildcard into a new wildcard type"
           (import tokenizer 
                   ?ALL)
           (export ?ALL))
(defclass tokens->wildcards::wildcard
    (is-a has-parent)
    (slot is-multifield
          (type SYMBOL)
          (allowed-symbols FALSE
                           TRUE
                           UNDEFINED)
          (storage local)
          (visibility public)
          (default ?NONE))
    (slot value
          (type SYMBOL)
          (storage local)
          (visibility public)
          (default ?NONE)))

(defrule tokens->wildcards::convert
         ?t <- (object (is-a token)
                       (kind ?kind&SF_WILDCARD|MF_WILDCARD)
                       (value ?value)
                       (name ?tok)
                       (parent ?p))
         =>
         (unmake-instance ?t)
         (make-instance ?tok of wildcard
                        (parent ?p)
                        (value ?value)
                        (is-multifield (switch ?kind
                                               (case SF_WILDCARD then FALSE)
                                               (case MF_WILDCARD then TRUE)
                                               (default UNDEFINED)))))
                        



