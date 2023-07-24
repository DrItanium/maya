;maya
;Copyright (c) 2012-2023, Joshua Scoggins
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

(defclass has-parent
  (is-a USER)
  (slot parent
        (type SYMBOL 
              INSTANCE)
        (allowed-symbols FALSE)
        (storage local)
        (visibility public)))
(defclass has-title
  (is-a USER)
  (slot title
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass has-description
  (is-a USER)
  (slot description
        (type STRING)
        (storage local)
        (visibility public)))
(defclass container
  (is-a has-parent)
  (multislot contents
             (storage local)
             (visibility public)))
(defclass atomic-value
  (is-a has-parent)
  (slot kind
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE))
  )

(defclass file-container
  (is-a container)
  (slot file-name
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE)))


(deffunction display-tokenize
             (?file ?out)
             (bind ?depth
                   0)
             (while (neq (nth$ 1 
                               (bind ?result 
                                     (next-token ?file)))
                         STOP) do
                    (bind ?first
                          (nth$ 1 
                                ?result))
                    (if (eq ?first 
                            RIGHT_PARENTHESIS) then
                      (bind ?depth 
                            (- ?depth 1)))
                    (loop-for-count (?i 1 ?depth) do
                                    (printout ?out tab))
                    (if (eq ?first 
                            LEFT_PARENTHESIS) then
                      (bind ?depth 
                            (+ ?depth 1)))
                    (printout ?out
                              ?result crlf)
                    )
             )
(deffunction inner-container
             "keep collecting until we hit a right paren and return this object"
             (?parent ?id)
             (bind ?top 
                   (make-instance of container
                                  (parent ?parent)))
             (bind ?children
                   (create$))
             (while (neq (nth$ 1 
                               (bind ?result
                                     (next-token ?id))) STOP) do
                    (bind ?kind
                          (nth$ 1 ?result))
                    (bind ?value
                          (nth$ 2 ?result))
                    (switch ?kind
                            (case LEFT_PARENTHESIS then
                              (bind ?children
                                    ?children
                                    (inner-container (instance-name ?top)
                                                     ?id)))
                            (case RIGHT_PARENTHESIS then
                              (modify-instance ?top
                                               (contents ?children))
                              (return (instance-name ?top)))
                            (default (bind ?children
                                           ?children
                                           (make-instance of atomic-value
                                                          (parent (instance-name ?top))
                                                          (kind ?kind)
                                                          (value ?value)))))
                    )
             (printout stderr 
                       "Hit end of file before finishing expression!" crlf)
             ; if we get here then an error has occurred and we need to display it as such
             FALSE)


(deffunction contain-file
             (?file-name)
             (if (not (open ?file-name 
                            (bind ?file-id 
                                  (gensym*))
                            "r")) then
               FALSE
               else
               (bind ?top
                     (make-instance of file-container
                                    (parent FALSE)
                                    (file-name ?file-name)))
               (bind ?children
                     (create$))
               (while (neq (nth$ 1 
                                 (bind ?result
                                       (next-token ?file-id)))
                           STOP) do
                      (bind ?kind
                            (nth$ 1 ?result))
                      (bind ?value
                            (nth$ 2 ?result))
                      (switch ?kind
                              (case LEFT_PARENTHESIS then
                                (bind ?out
                                      (inner-container (instance-name ?top)
                                                       ?file-id))
                                (if ?out then
                                  (bind ?children
                                        ?children
                                        ?out)
                                  else
                                  (printout stderr 
                                            "Terminating early!" crlf)
                                  (close ?file-id)
                                  (return FALSE)))
                              (case RIGHT_PARENTHESIS then
                                (printout stderr
                                          "Random right paren found!" crlf)
                                (close ?file-id)
                                (return FALSE))
                              (default (bind ?children
                                      ?children
                                      (make-instance of atomic-value
                                                     (parent (instance-name ?top))
                                                     (kind ?kind)
                                                     (value ?value))))))
               (close ?file-id)
               (modify-instance ?top
                                (contents ?children))
               (return ?top))
             )
(defclass parser
  (is-a USER)
  (slot top-element
        (type INSTANCE)
        (storage local)
        (visibility public))
  (slot current-element
        (type INSTANCE)
        (storage local)
        (visibility public))
  (slot path
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot id
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic (gensym*)))
  (slot valid
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public))
  (slot parsed 
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public))
  (slot parsing 
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public))
  (multislot current-token
             (storage local)
             (visibility public))
  (message-handler init after))
(defmessage-handler parser init after
                    ()
                    (bind ?self:valid
                          (open ?self:path
                                ?self:id
                                "r"))
                    (bind ?self:parsing
                          ?self:valid)
                    (bind ?self:top-element
                          (make-instance of file-container
                                         (parent FALSE)
                                         (file-name ?self:path)))
                    (bind ?self:current-element
                          ?self:top-element))

(deffacts parsing-stages
          (stage (current generate-files)
                 (rest process-file
                       sanity-check
                       hoisting
                       identify-structures)))
(defrule generate-file-container
         (stage (current generate-files))
         ?f <- (parse file ?path)
         =>
         (retract ?f)
         (make-instance of parser
                        (path ?path)))

(defrule read-token
         (stage (current process-file))
         ?f <- (object (is-a parser)
                       (current-token)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id))
         =>
         (modify-instance ?f 
                          (current-token (next-token ?id))))

(defrule stop-parsing-file
         "If we see a stop token then just terminate immediately!"
         (stage (current process-file))
         ?f <- (object (is-a parser)
                       (current-token STOP ?)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id))
         =>
         (close ?id)
         (modify-instance ?f 
                          (current-token)
                          (parsing FALSE)
                          (parsed TRUE)))

(defrule make-new-target-container
         (stage (current process-file))
         ?f <- (object (is-a parser)
                       (current-token LEFT_PARENTHESIS ?)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id)
                       (current-element ?target))
         ?k <- (object (is-a container)
                       (name ?target)
                       (contents $?prior))
         =>
         (bind ?ncurr
               (make-instance of container
                              (parent ?target)))
         (modify-instance ?f 
                          (current-token)
                          (current-element ?ncurr))
         (modify-instance ?k
                          (contents ?prior
                                    ?ncurr)))
(defrule leave-current-element:valid
         (stage (current process-file))
         ?f <- (object (is-a parser)
                       (current-token RIGHT_PARENTHESIS ?)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id)
                       (current-element ?target))
         (object (is-a container)
                 (name ?target)
                 (parent ?parent&~FALSE))
         =>
         (modify-instance ?f
                          (current-token)
                          (current-element ?parent)))

(defrule leave-current-element:invalid-no-parent
         (stage (current process-file))
         ?f <- (object (is-a parser)
                       (current-token RIGHT_PARENTHESIS $?)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id)
                       (path ?path)
                       (current-element ?target)
                       (name ?name))
         (object (is-a container)
                 (name ?target)
                 (parent FALSE))
         =>
         (printout werror
                   "ERROR: mismatched parens, found a right paren without a matching left paren" crlf
                   "Target file: " ?path crlf
                   "Target parser: " ?name crlf)
         (halt))
(defrule make-atomic-value 
         (stage (current process-file))
         ?f <- (object (is-a parser)
                       (current-token ?kind&~LEFT_PARENTHESIS&~RIGHT_PARENTHESIS&~STOP ?value)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id)
                       (current-element ?target))
         ?k <- (object (is-a container)
                       (name ?target)
                       (contents $?prior))
         =>
         (modify-instance ?f 
                          (current-token))
         (modify-instance ?k
                          (contents ?prior
                                    (make-instance of atomic-value
                                                   (parent ?target)
                                                   (kind ?kind)
                                                   (value ?value)))))

(defrule found-lparen-mismatch-at-end
         (stage (current sanity-check))
         ?f <- (object (is-a parser)
                       (parsing FALSE)
                       (valid TRUE)
                       (parsed TRUE)
                       (path ?path)
                       (top-element ?top)
                       (current-element ?v&~?top)
                       (name ?name))
         =>
         (printout werror
                   "ERROR: mismatched parens, found a left paren without a matching right paren after finishing parsing" crlf
                   "Target File: " ?path crlf
                   "Target parser: " ?name crlf
                   "Mismatched container: " ?v crlf)
         (halt))

(defrule raise-symbols-out-of-atoms
         (stage (current hoisting))
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (contents $?a ?sym-ref $?b))
         ?k <- (object (is-a atomic-value)
                       (name ?sym-ref)
                       (kind SYMBOL)
                       (value ?sym))
         =>
         (unmake-instance ?k)
         (modify-instance ?f 
                          (contents ?a ?sym ?b)))
