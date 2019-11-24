(deftemplate floortype
	0 100 variant
	(
		(sparse (0 1) (100 0))
		(rocky (0 0) (100 1))
		(empty (0 1) (0 1))
	)
)

(deftemplate floortile
	(slot x (default 0))
	(slot ftype (type FUZZY-VALUE floortype))
	(slot visited-rocky (default FALSE))
	(slot visited-sparse (default FALSE))
)

(deftemplate keytile
	(slot x (default 0))
	(slot ftype (type FUZZY-VALUE floortype))
)

(deftemplate crispfloortile
	(slot fval (default 50))
	(slot variant (default nil))
	(slot x (default 0))
)

(deftemplate params
	(slot x-min (default 0))
	(slot x-max (default 5))
)


(defrule make-init-tile
	(declare (salience 3))
	(params (x-min ?xmin) (x-max ?xmax))
	(not (floortile (x ?xmin)))
=>
	(assert (floortile (x ?xmin) (ftype empty)))
)

(defrule make-adj-tile
	(declare (salience 3))
	(params (x-max ?xmax))
	(or
		(floortile (x ?x))
		(keytile (x ?x))
	)
	(not (floortile (x =(+ ?x 1))))
	(not (keytile (x =(+ ?x 1))))
	(test (< ?x ?xmax))
=>
	(assert (floortile (x (+ ?x 1)) (ftype empty)))
)


(defrule x-rocky-update-1
	(params (x-max ?xmax))
	(keytile (ftype extremely rocky) (x ?x))
	?t <- (floortile (x =(+ ?x 1)) (ftype ?f) (visited-rocky FALSE))
=>
	(modify ?t (ftype (fuzzy-intersection ?f (create-fuzzy-value floortype rocky))) (visited-rocky TRUE))
)

(defrule x-sparse-update
	(keytile (ftype extremely sparse) (x ?x))
	?ft <- (floortile (x =(- ?x 1)) (ftype ?f) (visited-sparse FALSE))
=>
	(modify ?ft (ftype (fuzzy-intersection ?f (create-fuzzy-value floortype sparse))) (visited-sparse TRUE))
	(plot-fuzzy-value t "on" nil nil ?f (fuzzy-intersection ?f (create-fuzzy-value floortype sparse)))
)


(defrule defuzzify-1
	(declare (salience -1))
	(floortile (x ?x) (ftype ?f))
=>
	(assert (crispfloortile (x ?x) (fval (moment-defuzzify ?f))))
)

(defrule label-rocky
	(declare (salience -1))
	?c <- (crispfloortile (fval ?val) (variant nil))
	(test (> ?val 50))
=>
	(modify ?c (variant rocky))
)

(defrule label-sparse
	(declare (salience -1))
	?c <- (crispfloortile (fval ?val) (variant nil))
	(test (<= ?val 50))
=>
	(modify ?c (variant sparse))
)



(deffacts init-facts
	(params (x-min 0) (x-max 5))
	(keytile (ftype extremely rocky) (x 1)) CF 0.9
	(keytile (ftype extremely sparse) (x 4)) CF 0.8
)