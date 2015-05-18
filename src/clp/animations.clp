(defclass pixel
  (is-a USER)
  (slot red
        (type INTEGER)
        (range 0 ?VARIABLE)
        (default-dynamic 0))
  (slot green
        (type INTEGER)
        (range 0 ?VARIABLE)
        (default-dynamic 0))
  (slot blue
        (type INTEGER)
        (range 0 ?VARIABLE)
        (default-dynamic 0))
  (message-handler apply-to-cell primary)
  (message-handler create$ primary))

(defmessage-handler pixel create$ primary
                    ()
                    (create$ ?self:red
                             ?self:green
                             ?self:blue))
(defmessage-handler pixel apply-to-cell primary
                    (?x ?y)
                    (unicornhat:set-pixel-color ?x
                                                ?y
                                                ?self:red
                                                ?self:green
                                                ?self:blue))

(definstances colors
              (off of pixel)
              (white of pixel
                     (red 255)
                     (green 255)
                     (blue 255))
              (blue of pixel
                    (blue 255))
              (green of pixel 
                     (green 255))
              (red of pixel
                   (red 255))
              (yellow of pixel
                      (green 255)
                      (blue 255))
              (purple of pixel
                      (red 255)
                      (blue 255))
              (princeton-orange of pixel
                                (red 245)
                                (green 128)
                                (blue 37))
              )

