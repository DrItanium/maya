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
(defglobal MAIN
           ?*text-transform-passes* = (create$ isolate-commas))
(deftemplate MAIN::text-transformation-pass-tracker
             (slot target
                   (type INSTANCE)
                   (default ?NONE))
             (multislot passes
                        (type LEXEME)
                        (default ?NONE)))
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
(defrule MAIN::declare-text-transformation-passes
         (declare (salience 10000))
         (stage (current do-text-transformations))
         (object (is-a source-line)
                 (name ?id))
         =>
         (assert (text-transformation-pass-tracker (target ?id)
                                                   (passes ?*text-transform-passes*))))
         
(defrule MAIN::isolate-commas
         (stage (current do-text-transformations))
         ?f <- (text-transformation-pass-tracker (target ?id)
                                                 (passes $?a
                                                         isolate-commas
                                                         $?b))
         ?obj <- (object (is-a source-line)
                         (name ?id)
                         (generations $?a 
                                      ?last))
         =>
         (modify ?f
                 (passes ?a ?b))
         (if (str-index "," 
                        ?last) then
           (modify-instance ?obj
                            (generations $?a
                                         ?last
                                         (str-replace ?last 
                                                      "," 
                                                      " , "))))
         )
(defrule MAIN::remove-text-transformation-request-contents
         (declare (salience -9999))
         ?f <- (text-transformation-pass-tracker (passes))
         =>
         (retract ?f))
(defrule MAIN::decompose-source-line
         (stage (current tokenization))
         ?obj <- (object (is-a source-line)
                         (decomposed FALSE)
                         (generations $? ?line))
         =>
         (modify-instance ?obj
                          (decomposed TRUE)
                          (decomposition (explode$ ?line))))

(deftemplate MAIN::source-line-fact
             (slot target
                   (type INSTANCE)
                   (default ?NONE))
             (slot class
                   (type SYMBOL)
                   (default ?NONE))
             (multislot item
                   (type LEXEME)
                   (default ?NONE)))
(defrule MAIN::identify-label-declaration
         (stage (current label-identification))
         (object (is-a source-line)
                 (name ?name)
                 (decomposed TRUE)
                 (decomposition ?title&:(and (symbolp ?title)
                                             (has-suffix ?title 
                                                         ":"))))
         =>
         (assert (source-line-fact (target ?name)
                                   (class label-declaration)
                                   (item (string-to-field (sub-string 1 
                                                                      (- (str-length ?title)
                                                                         1)
                                                                      ?title))))))

(defrule MAIN::find-label-usage
         (stage (current label-identification))
         (source-line-fact (class label-declaration)
                           (target ?other)
                           (item ?label))
         (object (is-a source-line)
                 (decomposition $? ?label $?)
                 (name ?user))
         =>
         (assert (source-line-fact (target ?user)
                                   (class uses-label)
                                   (item ?other))))
(defrule MAIN::identify-global-label-declaration
         (stage (current label-identification))
         (object (is-a source-line)
                 (decomposition .globl ?name)
                 (name ?decl))
         =>
         (assert (source-line-fact (target ?decl)
                                   (class global-label-decl)
                                   (item ?name))))
(defrule MAIN::use-global-label-for-external-declaration
         "When we see a global declaration without a label declaration then we need to make sure it is known to be a label"
         (declare (salience -1)) ; make sure that we've processed everything else
         (stage (current label-identification))
         (source-line-fact (class global-label-decl)
                           (target ?ref)
                           (item ?name))
         (not (source-line-fact (class label-declaration)
                                (item ?name)))
         =>
         (assert (source-line-fact (target ?ref)
                                   (class label-declaration)
                                   (item ?name))))
; --------------
(defglobal MAIN
           ?*instructions* = (create$ add addi
                                      neg
                                      sub
                                      mul
                                      mulh
                                      mulhu
                                      mulhsu
                                      div
                                      rem
                                      and andi
                                      not
                                      or ori
                                      xor xori
                                      sll slli
                                      srl srli
                                      sra srai
                                      li
                                      lui
                                      auipc
                                      lw
                                      lh
                                      lhu
                                      lb
                                      lbu
                                      la
                                      sw
                                      sh
                                      sb
                                      j
                                      jal
                                      jalr
                                      call
                                      ret
                                      beq
                                      beqz
                                      bne
                                      bnez
                                      blt
                                      bltu
                                      bltz
                                      bgt
                                      bgtu
                                      bgtz
                                      ble
                                      bleu
                                      blez
                                      bge
                                      bgeu
                                      bgez
                                      slt slti
                                      sltu sltiu
                                      seqz
                                      snez
                                      sltz
                                      sgtz
                                      ebreak
                                      ecall
                                      fence fence.i
                                      mv
                                      nop
                                      csrrc csrrci
                                      csrrs csrrsi
                                      csrrw csrrwi
                                      )


; --------------
(defrule MAIN::print-source-line
         (stage (current display-result))
         (object (is-a source-file)
                 (path ?path)
                 (lines $?lines))
         =>
         (progn$ (?line ?lines)
                 (printout stdout 
                           "[" ?path ":" 
                           (send ?line
                                 get-line-number)
                           "]: ")
                 (progn$ (?str (send ?line
                                     get-generations))
                         (printout stdout
                                   "\"" ?str "\" -> "))
                 (printout stdout
                           (send ?line
                                 get-decomposition)
                           crlf)))

; to run this program, just execute maya-app from this directory


(deffacts MAIN::file-to-read
          ;(lineize test.s)
          (lineize flops.s)
          (stage (current walk-file)
                 (rest do-text-transformations
                       tokenization
                       label-identification
                       display-result)))
