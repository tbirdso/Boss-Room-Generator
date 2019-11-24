(deftemplate floortype
	0 100 variant
	(
		 (water (PI 10 25))
		(sparse (PI 30 50))
		 (rocky (PI 25 75))
		 (wall (S 90 95))
	)
)

(deftemplate floortile
	(slot ftype (type FUZZY-VALUE floortype))
	(slot x (default 0))
	(slot z (default 0))
)

(defrule plot-fuzzy
	(not (floortile))
=>
	(plot-fuzzy-value t "srw|" nil nil 
		(create-fuzzy-value floortype sparse)
		(create-fuzzy-value floortype rocky)
		(create-fuzzy-value floortype water)
		(create-fuzzy-value floortype wall))
)

(defrule plot-fuzzies
(floortile (ftype ?a) (x 1) (z 1))
(floortile (ftype ?b) (x 2) (z 1))
(floortile (ftype ?c) (x 3) (z 1))
(floortile (ftype ?d) (x 4) (z 1))
=>
	(plot-fuzzy-value t "abcd" nil nil ?a ?b ?c ?d)
)

(defrule plot-unions
(declare (salience -1))
(floortile (ftype ?a) (x 2) (z 1))
=>
	(plot-fuzzy-value t "x" nil nil (fuzzy-union ?a (create-fuzzy-value floortype rocky)))
)

(defrule defuzzify1
 ?f <- (floortile (ftype ?ft) (x ?x) (z ?z))
=>
 (bind ?t (maximum-defuzzify ?ft))
 (assert (crispfloortile ?t ?x ?z))
 (printout t "Made crisp " (get-fuzzy-slot ?f ftype) crlf)
 )

(defrule try-make-rocky
	(floortile (ftype extremely rocky) (x 1) (z 1))
	?t <- (floortile (ftype ?f&rocky) (x 2) (z 1))
=>
	(printout t "ftype is " ?f crlf)
	(bind ?new-f (fuzzy-union (create-fuzzy-value floortype rocky) ?f))
	(modify ?t (ftype (create-fuzzy-value floortype somewhat sparse)))
	(plot-fuzzy-value t "12x" nil nil ?f ?new-f (get-fuzzy-slot ?t ftype))
)
	
(deffacts init-facts
	(floortile (ftype extremely rocky) (x 1) (z 1)) CF 0.9
	(floortile (ftype somewhat sparse) (x 2) (z 1))
	(floortile (ftype sparse) (x 3) (z 1))
	(floortile (ftype sparse) (x 4) (z 1))
)