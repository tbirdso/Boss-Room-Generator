(deftemplate floortype
	-5 25 variant
	(
		(empty (0 0) (20 0))
	)
)

(deftemplate floortile
	(slot variant)
	(slot x (default 0))
	(slot z (default 0))
	(slot x-visited (default FALSE))
	(slot z-visited (default FALSE))
)

(deftemplate keytile
	(slot x (default 0))
	(slot z (default 0))
	(slot variant)
	(slot x-visited (default FALSE))
	(slot z-visited (default FALSE))
)

(deftemplate floorvariant
	(slot id)
	(slot ftype (type FUZZY-VALUE floortype))
)

(deftemplate params
	(slot x-min (default 0))
	(slot x-max (default 5))
	(slot z-min (default 0))
	(slot z-max (default 5))
)

(deftemplate scanner
	(slot row (default 0))
	(slot col (default 0))
)




(defrule remove-dup-keys
	(declare (salience 60))
	?k1 <- (keytile (x ?x) (z ?z))
	?k2 <- (keytile (x ?x) (z ?z))
	(test (neq (fact-index ?k1) (fact-index ?k2)))
=>
	(printout t "Retracting duplicate keytile at " ?x " " ?z crlf)
	(retract ?k2)
)




(defrule reset-fns
	(declare (salience 41))
	(scanner)
	?fv-r <- (floorvariant (id rocky))
	?fv-s <- (floorvariant (id sparse))
	(not (fns-set-for-row TRUE))
=>
	(retract ?fv-r)
	(retract ?fv-s)
	(assert (floorvariant (id rocky) (ftype (0 0))))
	(assert (floorvariant (id sparse) (ftype (0 0))))
	(assert (fns-set-for-row TRUE))
)

(defrule init-fns
	(declare (salience 40))
=>
	(assert (floorvariant (id rocky) (ftype (0 0))))
	(assert (floorvariant (id sparse) (ftype (0 0))))
	(assert (scanner (row 0) (col 0)))
)


(defrule inform-rocky-x
	(declare (salience 40))
	(params (z-max ?zmax))
	(scanner (row ?z&~?zmax))
	?ft <- (keytile (variant rocky) (x ?x) (z ?z) (x-visited FALSE))
	?v <- (floorvariant (id rocky) (ftype ?f))
=>
	(bind ?cf (get-cf ?ft))
	(bind ?base (* ?cf 5))
	(bind ?t (fuzzy-union ?f (create-fuzzy-value floortype (PI ?base ?x))))
	(modify ?v (ftype ?t))

	(disable-rule-cf-calculation)
	(retract ?ft)
	(assert (keytile (variant rocky) (x ?x) (z ?z) (x-visited TRUE)) CF ?cf)
	(enable-rule-cf-calculation)
)

(defrule inform-sparse-x
	(declare (salience 40))
	(params (z-max ?zmax))
	(scanner (row ?z&~?zmax))
	?ft <- (keytile (variant sparse) (x ?x) (z ?z) (x-visited FALSE))
	?v <- (floorvariant (id sparse) (ftype ?f))
=>
	(bind ?cf (get-cf ?ft))
	(bind ?base (* ?cf 5))
	(bind ?t (fuzzy-union ?f (create-fuzzy-value floortype (PI ?base ?x))))
	(modify ?v (ftype ?t))

	(disable-rule-cf-calculation)
	(retract ?ft)
	(assert (keytile (variant sparse) (x ?x) (z ?z) (x-visited TRUE)) CF ?cf)
	(enable-rule-cf-calculation)
)

(defrule inform-rocky-z
	(declare (salience 30))
	(params (x-max ?xmax) (z-max ?zmax))
	(scanner (col ?x&~?xmax) (row ?zmax))
	?ft <- (keytile (variant rocky) (x ?x) (z ?z) (x-visited TRUE) (z-visited FALSE))
	?v <- (floorvariant (id rocky) (ftype ?f))
=>
	(bind ?cf (get-cf ?ft))
	(bind ?base (* ?cf 5))
	(bind ?t (fuzzy-union ?f (create-fuzzy-value floortype (PI ?base ?z))))
	(modify ?v (ftype ?t))
	
	(disable-rule-cf-calculation)
	(retract ?ft)
	(assert (keytile (variant rocky) (x ?x) (z ?z) (z-visited TRUE)) CF ?cf)
	(enable-rule-cf-calculation)
)

(defrule inform-sparse-z
	(declare (salience 30))
	(params (x-max ?xmax) (z-max ?zmax))
	(scanner (col ?x&~?xmax) (row ?zmax))
	?ft <- (keytile (variant sparse) (x ?x) (z ?z) (x-visited TRUE) (z-visited FALSE))
	?v <- (floorvariant (id sparse) (ftype ?f))
=>
	(bind ?cf (get-cf ?ft))
	(bind ?base (* ?cf 5))
	(bind ?t (fuzzy-union ?f (create-fuzzy-value floortype (PI ?base ?z))))
	(modify ?v (ftype ?t))
	
	(disable-rule-cf-calculation)
	(retract ?ft)
	(assert (keytile (variant sparse) (x ?x) (z ?z) (x-visited TRUE) (z-visited TRUE)) CF ?cf)
	(enable-rule-cf-calculation)
)




(defrule plot-variants
	(declare (salience 21))
	?r <- (floorvariant (id rocky))
	?s <- (floorvariant (id sparse))
=>
	(plot-fuzzy-value t "rs" nil nil (get-fuzzy-slot ?r ftype) (get-fuzzy-slot ?s ftype))
)





(defrule make-init-tile
	(declare (salience 20))
	(params (x-min ?xmin) (z-min ?zmin))
	(not (floortile (x ?xmin) (z ?zmin)))
=>
	(assert (floortile (x ?xmin) (z ?zmin)))
)

(defrule make-adj-tile-x
	(declare (salience 20))
	(params (x-max ?xmax))
	(or
		(floortile (x ?x) (z ?z))
		(keytile (x ?x) (z ?z))
	)
	(not (floortile (x =(+ ?x 1)) (z ?z)))
	(not (keytile (x =(+ ?x 1)) (z ?z)))
	(test (< ?x ?xmax))
=>
	(bind ?x1 (+ ?x 1))
	(assert (floortile (x ?x1) (z ?z)))
)

(defrule make-adj-tile-z
	(declare (salience 20))
	(params (z-max ?zmax))
	(or
		(floortile (x ?x) (z ?z))
		(keytile (x ?x) (z ?z))
	)
	(not (floortile (x ?x) (z =(+ ?z 1))))
	(not (keytile (x ?x) (z =(+ ?z 1))))
	(test (< ?z ?zmax))
=>
	(bind ?z1 (+ ?z 1))
	(assert (floortile (x ?x) (z ?z1)))
)







(defrule update-rocky-x
	(declare (salience 10))
	(scanner (row ?z))
	(floorvariant (id rocky) (ftype ?frocky))
	(floorvariant (id sparse) (ftype ?fsparse))
	?tile <- (floortile (x ?x) (z ?z) (x-visited FALSE) (z-visited FALSE))
	(test (> (get-fs-value ?frocky ?x) (get-fs-value ?fsparse ?x)))
=>
	(bind ?fs (get-fs-value ?frocky ?x))
	(printout t "Make rocky " ?x ": Rocky " (get-fs-value ?frocky ?x) " and sparse " (get-fs-value ?fsparse ?x) crlf)

	(disable-rule-cf-calculation)
	(retract ?tile)
	(assert (floortile (variant rocky) (x ?x) (z ?z) (x-visited TRUE) (z-visited FALSE)) CF ?fs)
	(enable-rule-cf-calculation)
)

(defrule update-sparse-x
	(declare (salience 10))
	(scanner (row ?z))
	(floorvariant (id rocky) (ftype ?frocky))
	(floorvariant (id sparse) (ftype ?fsparse))
	(params (x-max ?xmax))
	?tile <- (floortile (x ?x) (z ?z) (x-visited FALSE))
	(test (<= (get-fs-value ?frocky ?x) (get-fs-value ?fsparse ?x)))
=>
	(bind ?fs (get-fs-value ?fsparse ?x))
	(printout t "Make sparse " ?x ": Rocky " (get-fs-value ?frocky ?x) " and sparse " (get-fs-value ?fsparse ?x) crlf)

	(disable-rule-cf-calculation)
	(retract ?tile)
	(assert (floortile (variant sparse) (x ?x) (z ?z) (x-visited TRUE) (z-visited FALSE)) CF ?fs)
	(enable-rule-cf-calculation)
)

(defrule update-rocky-z
	(declare (salience 0))
	(scanner (col ?x))
	(floorvariant (id rocky) (ftype ?frocky))
	(floorvariant (id sparse) (ftype ?fsparse))
	?tile <- (floortile (x ?x) (z ?z) (x-visited TRUE) (z-visited FALSE))
	(test (> (get-fs-value ?frocky ?z) (get-fs-value ?fsparse ?z)))
	(test (> (get-fs-value ?frocky ?z) (get-cf ?tile)))
=>
	(bind ?fs (get-fs-value ?frocky ?z))
	(printout t "Make rocky " ?z ": Rocky " (get-fs-value ?frocky ?z) " and sparse " (max (get-fs-value ?fsparse ?z) (get-cf ?tile)) crlf)

	(disable-rule-cf-calculation)
	(retract ?tile)
	(assert (floortile (variant rocky) (x ?x) (z ?z) (x-visited TRUE) (z-visited TRUE)) CF ?fs)
	(enable-rule-cf-calculation)
)

(defrule update-sparse-z
	(declare (salience 10))
	(scanner (col ?x))
	(floorvariant (id rocky) (ftype ?frocky))
	(floorvariant (id sparse) (ftype ?fsparse))
	?tile <- (floortile (x ?x) (z ?z) (x-visited TRUE) (z-visited FALSE))
	(test (< (get-fs-value ?frocky ?x) (get-fs-value ?fsparse ?z)))
	(test (< (get-cf ?tile) (get-fs-value ?fsparse ?z)))
=>
	(bind ?fs (get-fs-value ?fsparse ?z))
	(printout t "Make sparse " ?z ": Rocky " (max (get-fs-value ?frocky ?x) (get-cf ?tile)) " and sparse " (get-fs-value ?fsparse ?x) crlf)

	(disable-rule-cf-calculation)
	(retract ?tile)
	(assert (floortile (variant sparse) (x ?x) (z ?z) (x-visited TRUE) (z-visited TRUE)) CF ?fs)
	(enable-rule-cf-calculation)
)








(defrule move-scanner-row
	(declare (salience -10))
	(params (z-max ?zmax))
	?s <- (scanner (row ?z&~?zmax))
	?r <- (fns-set-for-row TRUE)
=>
	(modify ?s (row (+ ?z 1)))
	(retract ?r)
)

(defrule move-scanner-col
	(declare (salience -10))
	(params (x-max ?xmax) (z-max ?zmax))
	?s <- (scanner (col ?x&~?xmax) (row ?zmax))
	?r <- (fns-set-for-row TRUE)
=>
	(modify ?s (col (+ ?x 1)))
	(retract ?r)
)




(deffacts init-facts
	(params (x-min 0) (x-max 5) (z-min 0) (z-max 3))
	(keytile (variant rocky) (x 1) (z 0)) CF 0.21
	(keytile (variant sparse) (x 4) (z 0)) CF 0.8
	(keytile (variant rocky) (x 2) (z 1)) CF 1.0
)