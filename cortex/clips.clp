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
; ----
(defmodule clips-common
           (import common
                   ?ALL)
           (import tokenizer
                   ?ALL)
           (import variable-identification
                   ?ALL)
           (import wildcard-identification
                   ?ALL)
           (import constraint-identification
                   ?ALL)
           (export ?ALL))
(defclass clips-common::construct
  (is-a has-parent))
(defclass clips-common::titled-construct
  (is-a construct
        has-title
        has-comment))

(defclass clips-common::slot-attribute
  (is-a has-parent
        has-kind)
  (multislot values
             (storage local)
             (visibility public)
             (default ?NONE)))

(defclass clips-common::slot
  (is-a has-parent
        has-title)
  (slot kind
        (storage shared)
        (allowed-symbols UNDEFINED
                         single
                         multi)
        (access read-only)
        (default UNDEFINED))
  (multislot attributes
             (storage local)
             (visibility public)
             (default ?NONE)))

(defclass clips-common::single-slot
  (is-a slot)
  (slot kind
        (source composite)
        (default single)))

(defclass clips-common::multi-slot
  (is-a slot)
  (slot kind
        (source composite)
        (default multi)))
; ----
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

(defrule identify-global-constructs::new-defglobal-with-specified-module
         (declare (salience 1))
         (object (is-a translation-unit)
                 (status TOKENIZED)
                 (name ?tu))
         ?ex <- (object (is-a expression)
                        (name ?expr)
                        (parent ?tu)
                        (contents defglobal 
                                  ?module&:(symbolp ?module)
                                  $?rest))
         =>
         (unmake-instance ?ex)
         (make-instance ?expr of defglobal-construct
                        (parent ?tu)
                        (target-module ?module)
                        (assignments $?rest)))

(defrule identify-global-constructs::new-defglobal-without-specified-module
         (object (is-a translation-unit)
                 (status TOKENIZED)
                 (name ?tu))
         ?ex <- (object (is-a expression)
                        (name ?expr)
                        (parent ?tu)
                        (contents defglobal 
                                  $?rest))
         =>
         (unmake-instance ?ex)
         (make-instance ?expr of defglobal-construct
                        (parent ?tu)
                        (target-module) ; no module specified
                        (assignments $?rest)))

(defrule identify-global-constructs::new-assignment-found
         ?obj <- (object (is-a defglobal-construct)
                         (name ?dfg)
                         (assignments $?before
                                      ?name =(sym-cat "=")  ?expression
                                      $?after))
         ?vo <- (object (is-a variable)
                        (name ?name)
                        (is-global TRUE)
                        (is-multifield FALSE)
                        (value ?var))
         ?eo <- (object (is-a expression)
                        (name ?expression))
         =>
         (bind ?assignment
               (make-instance of global-assignment
                              (parent ?dfg)
                              (target-variable ?name)
                              (expression ?expression)))
         (modify-instance ?vo
                          (parent ?assignment))
         (modify-instance ?eo
                          (parent ?assignment))
         (modify-instance ?obj
                          (assignments $?before
                                       ?assignment
                                       $?after)))
; TODO: add checks for bad global assignments

; ----
(defmodule identify-generic-constructs
           (import clips-common 
                   ?ALL)
           (export ?ALL))
(defclass identify-generic-constructs::defgeneric-construct
  (is-a titled-construct))

(defrule identify-generic-constructs::create-defgeneric-full
         (object (is-a translation-unit)
                 (status TOKENIZED)
                 (name ?tu))
         ?ex <- (object (is-a expression)
                        (name ?expr)
                        (parent ?tu)
                        (contents defgeneric 
                                  ?name
                                  ?comment))
         =>
         (unmake-instance ?ex)
         (make-instance ?expr of defgeneric-construct
                        (parent ?tu)
                        (title ?name)
                        (comment ?comment)))
(defrule identify-generic-constructs::create-defgeneric-no-comment
         (object (is-a translation-unit)
                 (status TOKENIZED)
                 (name ?tu))
         ?ex <- (object (is-a expression)
                        (name ?expr)
                        (parent ?tu)
                        (contents defgeneric 
                                  ?name))
         =>
         (unmake-instance ?ex)
         (make-instance ?expr of defgeneric-construct
                        (parent ?tu)
                        (title ?name)))
; ----
(defmodule identify-deffacts-constructs
           (import clips-common 
                   ?ALL)
           (export ?ALL))

(defclass identify-deffacts-constructs::deffacts-construct
  (is-a titled-construct)
  (multislot patterns
             (storage local)
             (visibility public)
             (default ?NONE)))

(defrule identify-deffacts-constructs::create-deffacts-full
         (declare (salience 1))
         (object (is-a translation-unit)
                 (status TOKENIZED)
                 (name ?tu))
         ?ex <- (object (is-a expression)
                        (name ?expr)
                        (parent ?tu)
                        (contents deffacts
                                  ?title 
                                  ?comment&:(stringp ?comment)
                                  $?rest))
         =>
         (unmake-instance ?ex)
         (make-instance ?expr of deffacts-construct
                        (parent ?tu)
                        (title ?title)
                        (comment ?comment)
                        (patterns $?rest)))

(defrule identify-deffacts-constructs::create-deffacts-no-comment
         (object (is-a translation-unit)
                 (status TOKENIZED)
                 (name ?tu))
         ?ex <- (object (is-a expression)
                        (name ?expr)
                        (parent ?tu)
                        (contents deffacts
                                  ?title 
                                  $?rest))
         =>
         (unmake-instance ?ex)
         (make-instance ?expr of deffacts-construct
                        (parent ?tu)
                        (title ?title)
                        (patterns $?rest)))
; ----
(defmodule identify-deftemplate-constructs
           (import clips-common 
                   ?ALL)
           (export ?ALL))

(defclass identify-deftemplate-constructs::deftemplate-construct
  (is-a titled-construct)
  (multislot definitions
             (storage local)
             (visibility public)
             (default ?NONE)))



(defrule identify-deftemplate-constructs::create-deftemplate-full
         (declare (salience 1))
         (object (is-a translation-unit)
                 (status TOKENIZED)
                 (name ?tu))
         ?ex <- (object (is-a expression)
                        (name ?expr)
                        (parent ?tu)
                        (contents deftemplate
                                  ?title 
                                  ?comment&:(stringp ?comment)
                                  $?rest))
         =>
         (unmake-instance ?ex)
         (make-instance ?expr of deftemplate-construct
                        (parent ?tu)
                        (title ?title)
                        (comment ?comment)
                        (definitions $?rest)))

(defrule identify-deftemplate-constructs::create-deftemplate-no-comment
         (object (is-a translation-unit)
                 (status TOKENIZED)
                 (name ?tu))
         ?ex <- (object (is-a expression)
                        (name ?expr)
                        (parent ?tu)
                        (contents deftemplate
                                  ?title 
                                  $?rest))
         =>
         (unmake-instance ?ex)
         (make-instance ?expr of deftemplate-construct
                        (parent ?tu)
                        (title ?title)
                        (definitions $?rest)))
(defrule identify-deftemplate-constructs::create-single-slot
         (object (is-a deftemplate-construct)
                 (name ?p)
                 (definitions $? ?a $?))
         ?expr <- (object (is-a expression)
                          (name ?a)
                          (parent ?p)
                          (contents slot 
                                    ?name
                                    $?attributes))
         =>
         (unmake-instance ?expr)
         (make-instance ?a of single-slot
                        (parent ?p)
                        (title ?name)
                        (attributes ?attributes)))


; TODO: add sanity checking
; ----
(defmodule identify-deffunction-constructs
           (import clips-common 
                   ?ALL)
           (export ?ALL))

(defclass identify-deffunction-constructs::deffunction-construct
  (is-a titled-construct)
  (slot arguments
        (type INSTANCE)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot actions
             (storage local)
             (visibility public)
             (default ?NONE)))

(defrule identify-deffunction-constructs::create-deffunction-full
         (declare (salience 1))
         (object (is-a translation-unit)
                 (status TOKENIZED)
                 (name ?tu))
         ?ex <- (object (is-a expression)
                        (name ?expr)
                        (parent ?tu)
                        (contents deffunction
                                  ?title 
                                  ?comment&:(stringp ?comment)
                                  ?args
                                  $?rest))
         (object (is-a expression)
                 (name ?args))
         =>
         (unmake-instance ?ex)
         (make-instance ?expr of deffunction-construct
                        (parent ?tu)
                        (title ?title)
                        (comment ?comment)
                        (arguments ?args)
                        (actions ?rest)))

(defrule identify-deffunction-constructs::create-deffunction-no-comment
         (object (is-a translation-unit)
                 (status TOKENIZED)
                 (name ?tu))
         ?ex <- (object (is-a expression)
                        (name ?expr)
                        (parent ?tu)
                        (contents deffunction
                                  ?title 
                                  ?args
                                  $?rest))
         (object (is-a expression)
                 (name ?args))
         =>
         (unmake-instance ?ex)
         (make-instance ?expr of deffunction-construct
                        (parent ?tu)
                        (title ?title)
                        (arguments ?args)
                        (actions ?rest)))
; TODO: add sanity checking
; ----


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
           (import clips-common
                   ?ALL)
           (import identify-global-constructs
                   ?ALL)
           (import identify-generic-constructs
                   ?ALL)
           (import identify-deffacts-constructs
                   ?ALL)
           (import identify-deftemplate-constructs
                   ?ALL)
           (import identify-deffunction-constructs
                   ?ALL)
           (export ?ALL))

(defglobal clips
           ?*module-passes* = (create$ tokenizer
                                       raise-constants
                                       constraint-identification
                                       wildcard-identification
                                       variable-identification
                                       ; parsing phases
                                       ; construct declaration
                                       identify-global-constructs
                                       identify-generic-constructs
                                       identify-deftemplate-constructs
                                       identify-deffunction-constructs
                                       ; reset targets
                                       identify-deffacts-constructs
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

