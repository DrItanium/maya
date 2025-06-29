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
; Load the contents of a given file line by line and display that
(deffunction MAIN::begin
             "This function must be here, it is called by maya-app first"
             ()
             )
(deftemplate MAIN::stage
             (slot current
                   (type SYMBOL) 
                   (default ?NONE))
             (multislot rest
                        (type SYMBOL)))

(defclass MAIN::has-parent
  (is-a USER)
  (slot parent
        (type INSTANCE
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass MAIN::source-line
  (is-a has-parent)
  (slot line-number
        (type INTEGER)
        (range 1 ?VARIABLE)
        (default ?NONE))
  (multislot generations
             (type LEXEME)
             (default ?NONE))
  (slot decomposed
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (multislot decomposition))


(defclass MAIN::source-file
  (is-a USER)
  (slot path
        (type LEXEME)
        (default ?NONE))
  (slot line-number-count
        (type INTEGER)
        (range 0 ?VARIABLE)
        (access initialize-only)
        (default ?NONE))
  (multislot lines))

(deftemplate MAIN::source-file-walker
            (slot id
                  (type SYMBOL)
                  (default (gensym*)))
            (slot path
                  (type LEXEME)
                  (default ?NONE))
            (slot line-number-count
                  (type INTEGER)
                  (range 0 ?VARIABLE)
                  (default 0))
            (slot file-handle
                  (type SYMBOL)
                  (default ?NONE))
            (slot current-item
                  (default ?NONE))
            (slot finished
                  (type SYMBOL)
                  (allowed-symbols FALSE
                                   TRUE)
                  (default FALSE))
            (multislot lines
                       (type INSTANCE)))

(defrule MAIN::next-stage
         (declare (salience -10000))
         ?f <- (stage (rest ?next $?rest))
         =>
         (modify ?f 
                 (current ?next)
                 (rest ?rest)))

(defrule MAIN::done-parsing-file
         (stage (current walk-file))
         ?f <- (source-file-walker (finished FALSE)
                                   (current-item FALSE|EOF)
                                   (line-number-count ?lnc)
                                   (path ?path)
                                   (id ?id)
                                   (file-handle ?fh)
                                   (lines $?lines))
         =>
         (retract ?f)
         (close ?fh)
         (make-instance ?id of source-file
                        (path ?path)
                        (line-number-count ?lnc)
                        (lines ?lines)))
(defrule MAIN::try-open-file
         (stage (current walk-file))
         ?f <- (lineize ?path)
         =>
         (retract ?f)
         (if (open ?path
                   (bind ?fh
                         (gensym*))
                   "r") then
           (assert (source-file-walker (path ?path)
                                       (file-handle ?fh)
                                       (current-item (readline ?fh))))))
(defrule MAIN::create-source-line
         (stage (current walk-file))
         ?f <- (source-file-walker (finished FALSE)
                                   (current-item ?line&~EOF&~FALSE)
                                   (line-number-count ?lnc)
                                   (id ?id)
                                   (file-handle ?fh)
                                   (lines $?lines))
         =>
         (bind ?nlnc (+ ?lnc 1))
         (modify ?f
                 (current-item (readline ?fh))
                 (line-number-count ?nlnc)
                 (lines $?lines
                        (make-instance of source-line
                                       (parent (symbol-to-instance-name ?id))
                                       (line-number ?nlnc)
                                       (generations ?line)))))
(defrule MAIN::isolate-commas-fact
         (declare (salience 10000))
         (stage (current isolate-commas))
         (object (is-a source-line)
                 (name ?id))
         =>
         (assert (isolate commas on ?id)))
(defrule MAIN::isolate-commas
         (stage (current isolate-commas))
         ?f <- (isolate commas on ?id)
         ?obj <- (object (is-a source-line)
                         (name ?id)
                         (generations $?a 
                                      ?last))
         =>
         (retract ?f)
         (modify-instance ?obj
                          (generations $?a
                                       ?last
                                       (str-replace ?last "," " , "))))



(defrule MAIN::decompose-source-line
         (stage (current tokenization))
         ?obj <- (object (is-a source-line)
                         (decomposed FALSE)
                         (generations $? ?line))
         =>
         (modify-instance ?obj
                          (decomposed TRUE)
                          (decomposition (explode$ ?line))))

(defrule MAIN::print-source-line
         (stage (current display-result))
         (object (is-a source-line)
                 (decomposed TRUE)
                 (parent ?p)
                 (line-number ?ln)
                 (generations ?str $?)
                 (decomposition $?result))
         (object (is-a source-file)
                 (name ?p)
                 (path ?path))

         =>
         (printout stdout
                   "[" ?path ":" ?ln "]: \"" ?str "\" -> ... -> " ?result crlf))
                
; to run this program, just execute maya-app from this directory


(deffacts MAIN::file-to-read
          ;(lineize test.s)
          (lineize flops.s)
          (stage (current walk-file)
                 (rest isolate-commas
                       tokenization
                       display-result)))
