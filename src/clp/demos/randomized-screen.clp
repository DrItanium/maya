(reset)
(unicornhat:set-brightness 25)
(unicornhat:clear)
(loop-for-count (?x 0 7) do
				(loop-for-count (?y 0 7) do
								(send (switch (mod (random) 8)
											  (case 0 then [off])
											  (case 1 then [white])
											  (case 2 then [red])
											  (case 3 then [green])
											  (case 4 then [blue])
											  (case 5 then [yellow])
											  (case 6 then [purple])
											  (case 7 then [princeton-orange])
											  (default [off]))
									  apply-to-cell ?x ?y)
								(unicornhat:show)
								(wait 0.5 sec)))

