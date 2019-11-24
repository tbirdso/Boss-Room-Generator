(deftemplate floortype
	0 100 variant
	(
		(sparse (0 1) (100 0))
		(rocky (0 0) (100 1))
	)
)

(deftemplate floortile
	(slot ftype (type FUZZY-VALUE floortype))
	(slot x (default 0))
)

(deftemplate crispfloortile
	(slot variant (default sparse))
	(slot x (default 0))
)

(deftemplate params
	(slot x-min (default 0))
	(slot x-max (default 5))
)


(deftemplate floorset
	0 10 x-domain
)



(defrule adj-1
	(declare (salience 0))
	(params (x-min ?xmin) (x-max ?xmax))
	(floortile (ftype extremely rocky) (x ?x))
	(and
		(test (> ?x ?xmin))
		(not (floortile (x =(- ?x 1))))
	)
=>
	(assert (floortile (ftype rocky) (x (- ?x 1))))
)

(defrule adj-2
	(declare (salience 0))
	(params (x-min ?xmin) (x-max ?xmax))
	(floortile (ftype extremely rocky) (x ?x))
	(and
		(test (< ?x ?xmax))
		(not (floortile (x =(+ ?x 1))))
	)
=>
	(assert (floortile (ftype rocky) (x (+ ?x 1))))
)

(defrule adj-3
	(declare (salience 0))
	(not (floortile (x 3)))
	?x2 <-	(floortile (ftype extremely rocky) (x 2))
	?x4 <-	(floortile (ftype extremely sparse) (x 4))
=>
	(bind ?t (fuzzy-intersection (get-fuzzy-slot ?x2 ftype) (get-fuzzy-slot ?x4 ftype)))
	(assert (floortile (ftype ?t) (x 3)))
)


(defrule defuzzify1
 (declare (salience -1))
 ?f <- (floortile (ftype extremely rocky) (x ?x))
 (not (crispfloortile (x ?x)))
=>
 (assert (crispfloortile (variant rocky) (x ?x)))
 (printout t "Made crisp rocky" crlf)
 )


(defrule plot-fuzzies
	(declare (salience -1))
	(floortile (ftype ?x1) (x 1))
	(floortile (ftype ?x2) (x 2))
	(floortile (ftype ?x3) (x 3))
(floortile (ftype ?x4) (x 4))
=>
	(plot-fuzzy-value t "1234" nil nil ?x1 ?x2 ?x3 ?x4)
)

(defrule plot-fuzzy
	(salience 1)
	(not (floortile))
=>
	(plot-fuzzy-value t "srw|" nil nil 
		(create-fuzzy-value floortype sparse)
		(create-fuzzy-value floortype rocky)
	)
)


(deffacts init-facts
	(params (x-min 0) (x-max 5))
	(floortile (ftype extremely rocky) (x 1)) CF 0.9
	(floortile (ftype sparse) (x 4)) CF 0.8
)