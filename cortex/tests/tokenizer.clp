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
; define a simple MAIN module that contains all of the components necessary for testing that the tokenizer plus passes are working right
(defmodule MAIN
           (import tokenizer
                   ?ALL)
           (import constraint-identification
                   ?ALL)
           (import wildcard-identification
                   ?ALL)
           (import variable-identification
                   ?ALL)
           (export ?ALL))


(definstances MAIN::standard-parse-setup
              (of translation-unit
                  (path cortex/tokenizer.clp))
              (of translation-unit
                  (path cortex/common.clp))
              (of translation-unit
                  (path /this/is/not/a/real/path)))

(defrule MAIN::run-tokenization
         (object (is-a translation-unit)
                 (status UNPROCESSED))
         =>
         ; there are a series of actions we want to perform when we find an unprocessed translation
         ; unit
         (focus tokenizer ; first construct tokens
                raise-constants ; then raise constants up out of tokens
                variable-identification ; then identify variables
                wildcard-identification ; then identify wildcards
                constraint-identification ; then identify constraints
         ))

(defrule MAIN::status-message-generation
         "Print out status messages while processing translation units"
         (declare (salience 1))
         (object (is-a translation-unit)
                 (valid TRUE)
                 (status ?k)
                 (path ?path))
         =>
         (switch ?k
                 (case UNPROCESSED then 
                   (printout stderr
                             "Processing " ?path crlf))
                 (case ERROR then 
                   (printout stderr 
                             "Failed to process " ?path crlf))
                 (case PARSED then
                   (printout stderr
                             "Parsed " ?path crlf))
                 (case FINISHED then
                   (printout stderr
                             "Successfully finished processing " ?path crlf))
                 (default
                   (printout stderr
                             "Something went funky while processing " ?path crlf))))

(defrule MAIN::invalid-translation-unit-message
         (object (is-a translation-unit)
                 (valid FALSE)
                 (path ?path))
         =>
         (printout stderr
                   "Invalid translation unit " ?path crlf))



