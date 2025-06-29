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

(deffacts MAIN::file-to-read
          (lineize test.s))
(defclass MAIN::source-line
  (is-a USER)
  (slot parent
        (type SYMBOL
              INSTANCE)
        (allowed-symbols FALSE)
        (default ?NONE))
  (slot line-number
        (type INTEGER)
        (range 1 ?VARIABLE)
        (default ?NONE))
  (slot raw
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


(defrule MAIN::create-source-file-walker
         ?f <- (lineize ?path)
         =>
         (retract ?f)
         (if (open ?path 
                   (bind ?file-handle 
                         (gensym*)) 
                   "r") then
           (bind ?source-file-name
                 (symbol-to-instance-name (gensym*)))
           (bind ?line-number
                 0)
           (bind ?lines
                 (create$))
           (while (neq (bind ?current-line
                                  (readline ?file-handle))
                            FALSE
                            EOF) do
                  (bind ?line-number
                        (+ ?line-number
                           1))
                  (bind ?lines
                        ?lines
                        (make-instance of source-line
                                       (parent ?source-file-name)
                                       (line-number ?line-number)
                                       (raw ?current-line)))
                  (bind ?line-number
                        (+ ?line-number
                           1))
                  )
           (make-instance ?source-file-name of source-file
                          (path ?path)
                          (line-number-count ?line-number)
                          (lines ?lines))
           (close ?file-handle)
           )
         )

(defrule MAIN::decompose-source-line
         ?obj <- (object (is-a source-line)
                         (decomposed FALSE)
                         (raw ?line))
         =>
         (bind ?new-line
               (str-replace ?line "," " , "))
         (modify-instance ?obj
                          (decomposed TRUE)
                          (decomposition (explode$ ?new-line))))

(defrule MAIN::print-source-line
         (object (is-a source-line)
                 (decomposed TRUE)
                 (parent ?p)
                 (line-number ?ln)
                 (raw ?str)
                 (decomposition $?result))
         (object (is-a source-file)
                 (name ?p)
                 (path ?path))

         =>
         (printout stdout
                   "[" ?path ":" ?ln "]: \"" ?str "\" -> " ?result crlf))
                
; to run this program, just execute maya-app from this directory


