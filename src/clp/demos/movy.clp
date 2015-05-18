(defglobal MAIN
           ?*dead-last* = -10000
           ?*frames* = 512 
           ?*wait-duration* = (create$ (/ 1 30) sec)
           ?*stages* = (create$ update
                                refresh))
(definstances paddles
              (of paddle
                  (color 128 0 128)
                  (column 0)
                  (min 1)
                  (max 6)
                  (pixels 5 3 1))
              (of paddle
                  (color 128 0 128)
                  (column 7)
                  (min 1)
                  (max 6)
                  (pixels 6 4 2))
              (of paddle
                  (orientation horizontal)
                  (color 128 0 128)
                  (column 7)
                  (pixels 7 5 3 1))
              (of paddle
                  (min 1)
                  (max 6)
                  (orientation horizontal)
                  (color 0 0 128)
                  (column 6))
              (of paddle
                  (min 1)
                  (max 6)
                  (orientation horizontal)
                  (color 0 128 0)
                  (column 5))
              (of paddle
                  (min 1)
                  (max 6)
                  (orientation horizontal)
                  (color 128 0 0)
                  (column 4))
              (of paddle
                  (min 1)
                  (max 6)
                  (orientation horizontal)
                  (color 128 128 0)
                  (column 3))
              (of paddle
                  (min 1)
                  (max 6)
                  (orientation horizontal)
                  (color 0 128 128)
                  (column 2))
              (of paddle
                  (orientation horizontal)
                  (min 1)
                  (max 6)
                  (color 61 43 31)
                  (column 1))
              (of paddle
                  (orientation horizontal)
                  (color 128 0 128)
                  (pixels 6 4 2 0)
                  (column 0)))
(deffacts stages
          (count ?*frames*)
          (stages initialize
                  ?*stages*))

(defrule initialize-direction
         (stages initialize $?)
         ?f <- (object (is-a paddle)
                       (name ?name))
         =>
         (send ?f update)
         (assert (direction ?name down)))
(defrule next-stage
         (declare (salience ?*dead-last*))
         ?f <- (stages ? $?rest)
         =>
         (retract ?f)
         (assert (stages $?rest)))

(defrule restart
         (declare (salience ?*dead-last*))
         ?f <- (stages)
         ?f2 <- (count ?count&:(> ?count 0))
         =>
         (retract ?f ?f2)
         (assert (count (- ?count 1))
                 (stages ?*stages*)))


(defrule finish
         (declare (salience ?*dead-last*))
         ?f <- (stages)
         ?f2 <- (count 0)
         =>
         (retract ?f ?f2))

(defrule move-direction-up-ok
         (stages update $?)
         (direction ?obj
                    up)
         (test (not (send ?obj hit-top)))
         =>
         (send ?obj move-up)
         (send ?obj update))

(defrule move-direction-up-nope
         (stages update $?)
         ?f <- (direction ?obj
                          up)
         (test (send ?obj hit-top))
         =>
         (retract ?f)
         (assert (update-direction ?obj down)))

(defrule move-direction-down-ok
         (stages update $?)
         (direction ?obj
                    down)
         (test (not (send ?obj hit-bottom)))
         =>
         (send ?obj move-down)
         (send ?obj update))

(defrule move-direction-down-nope
         (stages update $?)
         ?f <- (direction ?obj
                          down)
         (test (send ?obj hit-bottom))
         =>
         (retract ?f)
         (assert (update-direction ?obj up)))


(defrule update-directions
         (stages refresh $?)
         ?f <- (update-direction ?name ?dir)
         =>
         (retract ?f)
         (assert (direction ?name ?dir)))



(defrule draw
         (stages refresh $?)
         =>
         (unicornhat:show)
         (wait (expand$ ?*wait-duration*)))
