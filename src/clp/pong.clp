; This is meant to be a simulation of pong.
(defglobal MAIN
           ?*enabled-pixel* = (create$ 96 96 96))
(defgeneric move-paddle
            "Move either the left or right side paddle up and down")
(defgeneric enabled-pixels
            "returns the y coordinates of enabled pixels on a given column")
(defgeneric scale-intensity)
(defmethod scale-intensity
  ((?intensity INTEGER)
   (?brightness INTEGER))
  (right-shift (* ?intensity
                  (+ ?brightness 1))
               8))
(defmethod scale-intensity
  ((?intensity INTEGER))
  (scale-intensity ?intensity
                   (unicornhat:get-brightness)))
(defmethod scale-intensity
  ((?intensities MULTIFIELD
                 INTEGER))
  (bind ?output (create$))
  (progn$ (?intensity ?intensities)
          (bind ?output (create$ ?output (scale-intensity ?intensity))))
  (return ?output))
(defmethod scale-intensity
  (($?intensities INTEGER))
  (scale-intensity ?intensities))
(defmethod enabled-pixels
  ((?x INTEGER))
  (bind ?enabled (create$))
  (bind ?brightness (+ (unicornhat:get-brightness) 1))
  (loop-for-count (?y 0 7) do
                  (bind ?pixel (unicornhat:get-pixel-color ?x
                                                           ?y))

                  (if (not ?pixel) then 
                    (return FALSE)
                    else
                    (bind ?pixel (scale-intensity ?pixel)))
                  ; add the intensities together, the pixel is off if we get zero back
                  (if (> (+ (expand$ ?pixel)) 0) then
                    (bind ?enabled (create$ ?enabled
                                            ?y))))
  (return ?enabled))

(defclass paddle
  (is-a USER)
  (slot min 
        (type INTEGER)
        (default-dynamic 0))
  (slot max
        (type INTEGER)
        (default-dynamic 7))
  (multislot color
             (type INTEGER)
             (cardinality 3 3)
             (default-dynamic ?*enabled-pixel*))
  (slot column
        (type INTEGER)
        (default ?NONE))
  (multislot pixels
             (type INTEGER)
             (default-dynamic 3 4))
  (slot orientation
        (type SYMBOL)
        (allowed-symbols vertical
                         horizontal))
  (message-handler hit-top primary)
  (message-handler hit-bottom primary)
  (message-handler move-up primary)
  (message-handler move-down primary)
  (message-handler random-move primary)
  (message-handler update primary))
(defmessage-handler paddle random-move primary
                    ()
                    (send ?self (if (= (mod (abs (random)) 2) 1) then 
                                  (if (send ?self hit-top) then move-down else move-up)
                                  else
                                  (if (send ?self hit-bottom) then move-up else move-down))))
(defmessage-handler paddle hit-top primary
                    () 
                    (switch (length$ ?self:pixels)
                            (case 0 then FALSE)
                            (case 1 then (= (nth$ 1 ?self:pixels) 
                                            ?self:min))
                            (default (= (min (expand$ ?self:pixels)) 
                                        ?self:min))))
(defmessage-handler paddle hit-bottom primary
                    ()
                    (switch (length$ ?self:pixels)
                            (case 0 then FALSE)
                            (case 1 then (= (nth$ 1 ?self:pixels) 
                                            ?self:max))
                            (default (= (max (expand$ ?self:pixels)) 
                                        ?self:max))))
(defmessage-handler paddle move-up primary
                    ()
                    (bind ?curr (create$))
                    (if (send ?self hit-top) then
                      (return FALSE)
                      else
                      (progn$ (?pixel ?self:pixels) 
                              (bind ?curr (create$ ?curr (- ?pixel 1))))
                      (bind ?self:pixels ?curr)
                      (return TRUE)))

(defmessage-handler paddle move-down primary
                    ()
                    (bind ?curr (create$))
                    (if (send ?self hit-bottom) then
                      (return FALSE)
                      else
                      (progn$ (?pixel ?self:pixels)
                              (bind ?curr (create$ ?curr (+ ?pixel 1))))
                      (bind ?self:pixels ?curr)
                      (return TRUE)))

(defmessage-handler paddle update primary
                    ()
                    ; just update the entire column but don't show the result
                    (loop-for-count (?y ?self:min ?self:max) do
                                    (unicornhat:set-pixel-color (if (eq ?self:orientation vertical) then ?self:column else ?y)
                                                                (if (eq ?self:orientation vertical) then ?y else ?self:column)
                                                                (if (member$ ?y ?self:pixels) then
                                                                  ?self:color
                                                                  else
                                                                  ?*led-off*)))
                    (return TRUE))
