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
  (slot column
        (type INTEGER)
        (default ?NONE))
  (multislot pixels
             (type INTEGER)
             (default-dynamic 3 4))
  (message-handler move-up primary)
  (message-handler move-down primary)
  (message-handler update primary))

(defmessage-handler paddle move-up primary
                    ()
                    (bind ?curr (create$))
                    (if (= (min (expand$ ?self:pixels)) 0) then
                      (return FALSE)
                      else
                      (progn$ (?pixel ?self:pixels) 
                              (bind ?curr (create$ ?curr (- ?pixel 1))))
                      (bind ?self:pixels ?curr)
                      (return TRUE)))

(defmessage-handler paddle move-down primary
                    ()
                    (bind ?curr (create$))
                    (if (= (max (expand$ ?self:pixels)) 7) then
                      (return FALSE)
                      else
                      (progn$ (?pixel ?self:pixels)
                              (bind ?curr (create$ ?curr (+ ?pixel 1))))
                      (bind ?self:pixels ?curr)
                      (return TRUE)))

(defmessage-handler paddle update primary
                    ()
                    ; just update the entire column but don't show the result
                    (loop-for-count (?y 0 7) do
                                    (unicornhat:set-pixel-color ?self:column
                                                                ?y
                                                                (if (member$ ?y ?self:pixels) then
                                                                  ?*enabled-pixel*
                                                                  else
                                                                  ?*led-off*)))
                    (return TRUE))

(definstances paddles
              (left of paddle (column 7))
              (right of paddle (column 0)))
