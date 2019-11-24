(deftemplate floortype
	0 10 variant
	(
		(sparse (3 0) (4 1) (6 0.4) (7 0.5) (8 1))
		(rocky (Z 1 7))
	)
)

(deftemplate keytile
	(slot x (default 0))
	(slot ftype (type FUZZY-VALUE floortype))
)

(deftemplate floortile
	(slot variant)
	(slot x (default 0))
	(slot ftype (type FUZZY-VALUE floortype))
	(slot visited)
)

(deftemplate floorvariant
	(slot id)
	(slot ftype (type FUZZY-VALUE floortype))
)

(deftemplate crispfloortile
	(slot variant (default sparse))
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
	(bind ?f (create-fuzzy-value floortype
				(?xmin 0) (?xmin 1) (?xmin 0)))
	(assert (floortile (x ?xmin) (ftype ?f)))
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
	(bind ?x1 (+ ?x 1))
	(bind ?f (create-fuzzy-value floortype
					(?x1 0) (?x1 1) (?x1 0)))
	(assert (floortile (x ?x1) (ftype ?f)))
)

(defrule rocky-tile
	(or
		(floortile (x ?x) (ftype rocky))
		(keytile (x ?x) (ftype extremely rocky))
	)
	(not (floortile (x ?x) (ftype extremely sparse)))
=>
	(assert (crispfloortile (variant rocky) (x ?x)))
)

(defrule sparse-tile
	(or
		(floortile (x ?x) (ftype somewhat sparse))
		(keytile (x ?x) (ftype extremely sparse))
	)
	(not (floortile (x ?x) (ftype extremely rocky)))
=>
	(assert (crispfloortile (variant sparse) (x ?x)))
)

(defrule discard-rocky
	?s <- (crispfloortile (variant sparse) (x ?x))
	?r <- (crispfloortile (variant rocky) (x ?x))
	(test (<= (get-cf ?r) (get-cf ?s)))
=>
	(retract ?r)
)

(defrule discard-sparse
	?s <- (crispfloortile (variant sparse) (x ?x))
	?r <- (crispfloortile (variant rocky) (x ?x))
	(test (> (get-cf ?r) (get-cf ?s)))
=>
	(retract ?s)
)

(defrule plot-variants
=>
	(plot-fuzzy-value t "rs" nil nil 
		(create-fuzzy-value floortype rocky) 
		(create-fuzzy-value floortype sparse))
)

(defrule make-template
=>
	(deftemplate floortype0
		0 10 variant (
			(sparse (PI 3 4))
			(rocky (PI 1 2))
	))
)


(deffacts init-facts
	(params (x-min 0) (x-max 10))
	(keytile (ftype extremely rocky) (x 1)) CF 0.9
	(keytile (ftype extremely sparse) (x 4)) CF 0.8
	(keytile (ftype extremely sparse) (x 8)) CF 1.0
)