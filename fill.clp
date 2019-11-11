(deftemplate floortile
	0 100 variant
	(
		(sparse (0 0) (20 0) (40 1) (60 1) (80 0.5) (100 0))
		 (rocky (0 0) (80 1) (100 0))
		 (water (0 0) (20 1) (40 0))
		 (wall (95 0) (100 1))
	)
)



(defrule defuzzify1
 (declare (salience -1))
 ?f <- (floortile ?)
=>
 (bind ?t (moment-defuzzify ?f))
 (assert (crispfloortile ?t))
(plot-fuzzy-value t * nil nil 
	(create-fuzzy-value floortile sparse)
	(create-fuzzy-value floortile rocky))
 )
 
 (deffacts init-facts
	(floortile rocky)
)