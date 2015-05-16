(defgeneric rand-intensity)
(defgeneric rand-pixel)
(defgeneric unicornhat:clear)
(defgeneric rand-screen
            "Set each pixel on the unicornhat to a random color")
(defgeneric rand-screen-splash
            "show different random screens")
 
(defglobal MAIN
           ?*default-intensity-cap* = 128
           ?*micro-to-milliseconds* = 1000
           ?*micro-to-seconds* = (* ?*micro-to-milliseconds* 1000)
           ?*micro-to-minutes* = (* ?*micro-to-seconds* 60)
           ?*micro-to-hours* = (* ?*micro-to-minutes* 60)
           ?*led-off* = (create$ 0 0 0)
           ?*default-wait-amount* = 10
           ?*basic-wait-quantum* = usec
           )
(defmethod wait
  ((?amount INTEGER)
   (?scale SYMBOL 
           (not (neq (lowcase ?scale)
                     millisecond
                     milliseconds
                     milli-seconds
                     milli-second
                     msec
                     ms))))
  (wait (* ?amount 
           ?*micro-to-milliseconds*)))

(defmethod wait
  ((?amount FLOAT))
  (wait (integer ?amount)))
(defmethod wait
  ((?amount NUMBER)
   (?scale SYMBOL 
           (not (neq (lowcase ?scale)
                     second
                     seconds
                     sec
                     s))))
  (wait (* ?amount 
           ?*micro-to-seconds*)))

(defmethod wait
  ((?amount NUMBER)
   (?scale SYMBOL
           (not (neq (lowcase ?scale)
                     usec
                     us
                     microseconds
                     micro-seconds
                     micro-second
                     micro-seconds))))
  (wait ?amount))

(defmethod wait
  ((?amount NUMBER)
   (?scale SYMBOL 
           (not (neq (lowcase ?scale)
                     min
                     mins
                     m
                     minutes
                     minute))))
  (wait (* ?amount
           ?*micro-to-minutes*)))
(defmethod wait
  ((?amount NUMBER)
   (?scale SYMBOL
           (not (neq (lowcase ?scale)
                     h
                     hr
                     hour
                     hours))))
  (wait (* ?amount
           ?*micro-to-hours*)))
(defmethod unicornhat:get-pixel-color
  ((?x INTEGER)
   (?y INTEGER))
  (unicornhat:get-pixel-color (unicornhat:get-pixel-position ?x ?y)))
(defmethod unicornhat:set-pixel-color
  ((?x INTEGER)
   (?y INTEGER)
   (?rgb MULTIFIELD 
         INTEGER))
  (unicornhat:set-pixel-color (unicornhat:get-pixel-position ?x ?y)
                              (expand$ ?rgb)))
(defmethod unicornhat:set-pixel-color
  ((?index-sym SYMBOL
               (eq (lowcase ?index-sym)
                   index:))
   (?index INTEGER)
   (?rgb MULTIFIELD
         INTEGER))
  (unicornhat:set-pixel-color ?index
                              (expand$ ?rgb)))
(defmethod unicornhat:set-pixel-color
 ((?x INTEGER)
  (?y INTEGER)
  (?r INTEGER)
  (?g INTEGER)
  (?b INTEGER))
 (unicornhat:set-pixel-color ?x ?y 
                             (create$ ?r ?g ?b)))
(defmethod rand-intensity
  ((?cap INTEGER))
  (mod (random) ?cap))
(defmethod rand-intensity
  ()
  (rand-intensity ?*default-intensity-cap*))

(defmethod random-pixel-color
  ((?index-sym SYMBOL
               (eq (lowcase ?index-sym)
                   index:))
   (?index INTEGER)
   (?cap INTEGER))
  (unicornhat:set-pixel-color ?index
                              (rand-intensity ?cap)
                              (rand-intensity ?cap)
                              (rand-intensity ?cap)))
(defmethod random-pixel-color
  ((?index-sym SYMBOL
               (eq ?index-sym index:))
   (?index INTEGER))
  (random-pixel-color index: ?index
                      ?*default-intensity-cap*))

(defmethod random-pixel-color
  ((?x INTEGER)
   (?y INTEGER))
  (random-pixel-color ?x 
                      ?y 
                      ?*default-intensity-cap*))
(defmethod random-pixel-color
  ((?x INTEGER)
   (?y INTEGER)
   (?cap INTEGER))
  (unicornhat:set-pixel-color (unicornhat:get-pixel-position ?x ?y)
                              (rand-intensity ?cap)
                              (rand-intensity ?cap)
                              (rand-intensity ?cap)))
(defmethod unicornhat:clear
  ((?show SYMBOL (not (neq ?show FALSE
                           TRUE))))
  (loop-for-count (?a 0 (- (unicornhat:number-of-pixels) 1)) do
                  (unicornhat:set-pixel-color index: ?a
                                              ?*led-off*))
  (if ?show then
    (unicornhat:show))
  TRUE)
(defmethod unicornhat:clear
  ()
  (unicornhat:clear TRUE))
(defmethod rand-screen
  ((?cap INTEGER))
  (loop-for-count (?x 0 7) do
                  (loop-for-count (?y 0 7) do
                                  (random-pixel-color ?x ?y ?cap))))
(defmethod rand-screen
  ()
  (rand-screen ?*default-intensity-cap*))
(defmethod rand-screen-splash
  ((?iterations INTEGER)
   (?cap INTEGER)
   (?delay-sym SYMBOL
               (eq (lowcase ?delay-sym)
                   delay:))
   (?delay NUMBER)
   (?delay-scale SYMBOL))
  (loop-for-count (?i 1 ?iterations) do
                  (rand-screen ?cap)
                  (unicornhat:show)
                  (wait ?delay
                        ?delay-scale)))
(defmethod rand-screen-splash
  ((?iterations INTEGER)
   (?cap INTEGER)
   (?delay-sym SYMBOL
               (eq (lowcase ?delay-sym)
                   delay:))
   (?delay NUMBER))
  (rand-screen-splash ?iterations
                      ?cap
                      ?delay-sym
                      ?delay
                      ?*basic-wait-quantum*))
(defmethod rand-screen-splash
  ((?iterations INTEGER)
   (?delay-sym SYMBOL
               (eq (lowcase ?delay-sym)
                   delay:))
   (?delay NUMBER)
   (?delay-scale SYMBOL))
  (rand-screen-splash ?iterations
                      ?*default-intensity-cap*
                      ?delay-sym
                      ?delay
                      ?delay-scale))
(defmethod rand-screen-splash
  ((?iterations INTEGER)
   (?delay-sym SYMBOL
               (eq (lowcase ?delay-sym)
                   delay:))
   (?delay NUMBER))
  (rand-screen-splash ?iterations
                      ?*default-intensity-cap*
                      ?delay-sym
                      ?delay
                      ?*basic-wait-quantum*))
(defmethod rand-screen-splash
  ((?iterations INTEGER)
   (?cap INTEGER))
  (rand-screen-splash ?iterations
                      ?cap
                      delay: ?*default-wait-amount* ?*basic-wait-quantum*))
(defmethod rand-screen-splash
  ((?iterations INTEGER))
  (rand-screen-splash ?iterations
                      ?*default-intensity-cap*))

