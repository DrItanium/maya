(reset)
(unicornhat:set-brightness 64)
(unicornhat:clear)
(deffunction decr
			 (?value)
			 (max (- ?value 1) 0))
(deffunction decr-pixel 
			 (?x ?y)
			 (bind ?c (unicornhat:get-pixel-color ?x ?y))
			 (unicornhat:set-pixel-color ?x ?y
										 (decr (nth$ 1 ?c))
										 (decr (nth$ 2 ?c))
										 (decr (nth$ 3 ?c))))

(deffunction action 
			 ()
			 (loop-for-count (?x 0 7) do
							 (loop-for-count (?y 0 7) do
											 (decr-pixel ?x ?y))))

(deffunction go
			 (?iterations)
			 (rand-screen-splash 1 255)
			 (loop-for-count (?i 0 ?iterations) do
							 (action)
							 (unicornhat:show)
							 (wait (/ 1 60) sec)))

(go 255)

