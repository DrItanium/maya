(clear) ; Test for illegal export/import constructs
(defmodule A (export ?ALL))
(defmodule B (export deffacts ?ALL))
(defmodule B (export defrule ?ALL))
(defmodule B (export defmethod ?ALL))
(defmodule B (export defmessage-handler ?ALL))
(defmodule B (export definstances ?ALL))
(defmodule B (import A deffacts ?ALL))
(defmodule B (import A defrule ?ALL))
(defmodule B (import A defmethod ?ALL))
(defmodule B (import A defmessage-handler ?ALL))
(defmodule B (import A definstances ?ALL))
(list-defmodules)
(clear) ; Test for importing undefined constructs
(defmodule A (export ?ALL))
(defmodule B (import A deftemplate foo))
(defmodule B (import A defglobal foo))
(defmodule B (import A defclass FOO))
(defmodule B (import A defgeneric foo))
(clear) ; Test for name conflicts
(defmodule A (export ?ALL))
(defrule A::foo (a) =>)
(deffacts A::foo)
(deftemplate A::foo)
(defglobal A ?*foo* = 3)
(defgeneric A::foo)
(defmethod A::foo ())
(defclass A::FOO (is-a USER))
(defmessage-handler A::FOO bar ())
(definstances A::foo)
(defmodule MAIN (import A ?ALL) (export ?ALL))
(defmodule B (import MAIN ?ALL))
(defrule B::foo (b) =>)
(deffacts B::foo)
(deftemplate B::foo)
(defglobal B ?*foo* = 3)
(defgeneric B::foo)
(defmethod B::foo ())
(defclass B::FOO (is-a USER))
(defmessage-handler B::FOO bar ())
(definstances B::foo)
(defmodule C (export ?ALL))
(defmodule D (import A ?ALL) (import C ?ALL))
(defrule C::foo =>)
(deffacts C::foo)
(deftemplate C::foo)
(defglobal C ?*foo* = 3)
(defgeneric C::foo)
(defmethod C::foo ())
(defclass C::FOO (is-a USER))
(defmessage-handler C::FOO bar ())
(definstances C::foo)
(defmodule E (import A ?ALL) (import C ?ALL))
(list-defrules *)
(list-deffacts *)
(list-deftemplates *)
(list-defglobals *)
(list-defgenerics *)
(list-defmethods A::foo)
(list-defmethods B::foo)
(list-defmethods C::foo)
(list-defmethods D::foo)
(list-defclasses *)
(list-defmessage-handlers A::FOO)
(list-defmessage-handlers B::FOO)
(list-defmessage-handlers C::FOO)
(list-defmessage-handlers D::FOO)
(list-definstances *)
(clear) ; Test for scope
(setgen 1)
(defmodule A (export ?ALL))
(deftemplate A::foo (slot x))
(defglobal A ?*foo* = 3)
(defgeneric A::foo)
(defmethod A::foo () x)
(defclass A::FOO (is-a USER) (role concrete) (slot x))
(defmodule B (import A ?ALL))
(deffacts B::foo (foo (x 3)))
(defrule B::bar (foo (x 3)) => (printout t ?*foo* " " (foo)))
(make-instance of FOO)
(instances)
(clear) ; Test fact scoping
(defmodule A)
(assert (a))
(assert (b))
(defmodule B)
(assert (b))
(assert (c))
(assert (c))
(facts A)
(facts B)
(facts MAIN)
(facts *)
(clear) ; Test redefinition of MAIN module
(defmodule MAIN (export ?ALL))
(defmodule MAIN (export deftemplate ?ALL))
(clear)
(defmodule MAIN (export ?ALL))
(clear) ; Test ?NONE keyword
(defmodule MAIN (export ?NONE ?ALL))
(defmodule MAIN (export deftemplate ?NONE ?ALL))
(clear)
(deftemplate MAIN::foo (slot x))
(defmodule MAIN (export ?NONE))
(defmodule FOO (import MAIN ?NONE))
(clear)
(deftemplate MAIN::foo (slot x))
(defmodule MAIN (export ?NONE))
(defmodule FOO (import MAIN deftemplate ?NONE))
(clear)
(deftemplate MAIN::foo (slot x))
(defmodule MAIN (export deftemplate ?NONE))
(defmodule FOO (import MAIN ?NONE))
(clear)
(deftemplate MAIN::foo (slot x))
(defmodule MAIN (export deftemplate ?NONE))
(defmodule FOO (import MAIN deftemplate ?NONE))
(clear)
(deftemplate MAIN::foo (slot x))
(defmodule MAIN (export deftemplate foo))
(defmodule FOO (import MAIN ?NONE))
(assert (foo (x 3)))
(clear)
(deftemplate MAIN::foo (slot x))
(defmodule MAIN (export deftemplate foo))
(defmodule FOO (import MAIN deftemplate ?NONE))
(assert (foo (x 3)))
(clear) ; Miscellaneous (previously a bug)
(defmodule FOO (export deftemplate woz pif))
(deftemplate FOO::bar)
(deftemplate FOO::woz)
(defmodule YAK1 (import FOO deftemplate bar))
(defmodule YAK2 (import FOO deftemplate woz))
(defmodule YAK3 (import FOO deftemplate pif))
(defmodule FIB)
(defmodule BIK (import FIB ?ALL))
(defmodule FIB1 (export defglobal ?ALL))
(defmodule BIK1 (import FIB1 deftemplate ?ALL))
(clear) ; Implied deftemplates causing conflict
(defmodule MAIN (export ?ALL))
(defmodule BAR (import MAIN ?ALL))
(deftemplate BAR::b (slot x))
(assert (b (x 3)))
(set-current-module MAIN)
(assert (b 3))
(defrule foo => (assert (b)))
(defrule foo (b) =>)
(list-deftemplates *)
(clear)
(defmodule MAIN (export ?ALL))
(deftemplate a (slot x))
(defmodule BAR (import MAIN ?ALL) (export ?ALL))
(deftemplate b (slot x))
(defmodule YAK (import MAIN ?ALL) (import BAR ?ALL))
(deftemplate c (slot x))
(assert (c (x 3)))
(set-current-module BAR)
(assert (b (x 4)))
(set-current-module MAIN)
(assert (a (x 5)))
(defrule YAK::c (a (x ?)) (b (x ?)) (c (x ?)) =>)
(defrule BAR::b (a (x ?)) (b (x ?)) =>)
(defrule MAIN::a (a (x ?)) =>)
(agenda *)
(clear)
