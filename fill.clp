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

(deftemplate floorcolor
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



(defrule make-init-tile
	(declare (salience 40))
	(params (x-min ?xmin) (z-min ?zmin))
	(not (floortile (x ?xmin) (z ?zmin)))
	(not (keytile (x ?xmin) (z ?zmin)))
=>
	(assert (floortile (x ?xmin) (z ?zmin)))
)

(defrule make-adj-tile-x "Fill grid on x-axis"
	(declare (salience 40))
	(params (x-max ?xmax))
	(or
		(floortile (x ?x) (z ?z))
		(keytile (x ?x) (z ?z))
	)
	(not (floortile (x =(+ ?x 1)) (z ?z)))
	(not (keytile (x =(+ ?x 1)) (z ?z)))
	(test (< (+ ?x 1) ?xmax))
=>
	(bind ?x1 (+ ?x 1))
	(assert (floortile (x ?x1) (z ?z)))
)

(defrule make-adj-tile-z "Fill grid on z-axis"
	(declare (salience 40))
	(params (z-max ?zmax))
	(or
		(floortile (x ?x) (z ?z))
		(keytile (x ?x) (z ?z))
	)
	(not (floortile (x ?x) (z =(+ ?z 1))))
	(not (keytile (x ?x) (z =(+ ?z 1))))
	(test (< (+ ?z 1) ?zmax))
=>
	(bind ?z1 (+ ?z 1))
	(assert (floortile (x ?x) (z ?z1)))
)




(defrule reset-fns	"Clear variant profiles when scanner moves"
	(declare (salience 32))
	(params (x-max ?xmax))
	(scanner (col ?x) (row ?z))
	(test (neq ?x ?xmax))

	?fv-r <- (floorcolor (id blue))
	?fv-s <- (floorcolor (id red))
	(not (fns-set-for-row TRUE))
=>
	(retract ?fv-r)
	(retract ?fv-s)
	(assert (floorcolor (id blue) (ftype (0 0))))
	(assert (floorcolor (id red) (ftype (0 0))))
	(assert (fns-set-for-row TRUE))
)

(defrule init-fns	"Initialize variant profiles"
	(declare (salience 31))
=>
	(assert (floorcolor (id blue) (ftype (0 0))))
	(assert (floorcolor (id red) (ftype (0 0))))
	(assert (scanner (row 0) (col 0)))
)


(defrule inform-blue-x	"Develop blue variant profile"
	(declare (salience 31))
	(params (z-max ?zmax))
	(scanner (row ?z&~?zmax))
	?ft <- (keytile (variant blue) (x ?x) (z ?z) (x-visited FALSE))
	?v <- (floorcolor (id blue) (ftype ?f))
=>
	(bind ?cf (get-cf ?ft))
	(bind ?base (* ?cf 5))
	(bind ?t (fuzzy-union ?f (create-fuzzy-value floortype (PI ?base ?x))))
	(modify ?v (ftype ?t))

	(disable-rule-cf-calculation)
	(retract ?ft)
	(assert (keytile (variant blue) (x ?x) (z ?z) (x-visited TRUE)) CF ?cf)
	(enable-rule-cf-calculation)
)

(defrule inform-red-x	"Develop red variant profile"
	(declare (salience 31))
	(params (z-max ?zmax))
	(scanner (row ?z&~?zmax))
	?ft <- (keytile (variant red) (x ?x) (z ?z) (x-visited FALSE))
	?v <- (floorcolor (id red) (ftype ?f))
=>
	(bind ?cf (get-cf ?ft))
	(bind ?base (* ?cf 5))
	(bind ?t (fuzzy-union ?f (create-fuzzy-value floortype (PI ?base ?x))))
	(modify ?v (ftype ?t))

	(disable-rule-cf-calculation)
	(retract ?ft)
	(assert (keytile (variant red) (x ?x) (z ?z) (x-visited TRUE)) CF ?cf)
	(enable-rule-cf-calculation)
)

(defrule inform-blue-z		"Develop blue variant profile"
	(declare (salience 30))
	(params (x-max ?xmax) (z-max ?zmax))
	(scanner (col ?x&~?xmax) (row ?zmax))
	?ft <- (keytile (variant blue) (x ?x) (z ?z) (x-visited TRUE) (z-visited FALSE))
	?v <- (floorcolor (id blue) (ftype ?f))
=>
	(bind ?cf (get-cf ?ft))
	(bind ?base (* ?cf 5))
	(bind ?t (fuzzy-union ?f (create-fuzzy-value floortype (PI ?base ?z))))
	(modify ?v (ftype ?t))
	
	(disable-rule-cf-calculation)
	(retract ?ft)
	(assert (keytile (variant blue) (x ?x) (z ?z) (z-visited TRUE)) CF ?cf)
	(enable-rule-cf-calculation)
)

(defrule inform-red-z	"Develop red variant profile"
	(declare (salience 30))
	(params (x-max ?xmax) (z-max ?zmax))
	(scanner (col ?x&~?xmax) (row ?zmax))
	?ft <- (keytile (variant red) (x ?x) (z ?z) (x-visited TRUE) (z-visited FALSE))
	?v <- (floorcolor (id red) (ftype ?f))
=>
	(bind ?cf (get-cf ?ft))
	(bind ?base (* ?cf 5))
	(bind ?t (fuzzy-union ?f (create-fuzzy-value floortype (PI ?base ?z))))
	(modify ?v (ftype ?t))
	
	(disable-rule-cf-calculation)
	(retract ?ft)
	(assert (keytile (variant red) (x ?x) (z ?z) (x-visited TRUE) (z-visited TRUE)) CF ?cf)
	(enable-rule-cf-calculation)
)




(defrule plot-variants
	(declare (salience 21))
	?r <- (floorcolor (id blue))
	?s <- (floorcolor (id red))
=>
	(plot-fuzzy-value t "rs" nil nil (get-fuzzy-slot ?r ftype) (get-fuzzy-slot ?s ftype))
)






(defrule update-x	"Predict variant for row scan"
	(declare (salience 10))
	(params (z-max ?zmax))
	(scanner (row ?z&~?zmax))
	(floorcolor (id blue) (ftype ?fblue))
	(floorcolor (id red) (ftype ?fred))
	?tile <- (floortile (variant ?v) (x ?x) (z ?z) (x-visited FALSE))
	(or
		(test (<= (get-cf ?tile) (get-fs-value ?fred ?x)))
		(test (<= (get-cf ?tile) (get-fs-value ?fblue ?x)))
		(test (eq ?v nil))
	)
=>

	(if (<= (get-fs-value ?fblue ?x) (get-fs-value ?fred ?x))
		then
		(bind ?new-v red)
		(bind ?fs (get-fs-value ?fred ?x))
		(bind ?cf (* ?fs (- 1 (get-fs-value ?fblue ?x))))
		else
		(bind ?new-v blue)
		(bind ?fs (get-fs-value ?fblue ?x))
		(bind ?cf (* ?fs (- 1 (get-fs-value ?fred ?x))))
	)

	(printout t "Make " ?new-v " " ?x ": blue " (get-fs-value ?fblue ?x) " and red " (get-fs-value ?fred ?x) crlf)

	(disable-rule-cf-calculation)
	(retract ?tile)
	(assert (floortile (variant ?new-v) (x ?x) (z ?z) (x-visited TRUE) (z-visited FALSE)) CF ?cf)
	(enable-rule-cf-calculation)
)

(defrule update-up-left-on-row-scan	"Predict diagonal variant for row scan"
	(declare (salience 10))
	(scanner (row ?z))
	(params (x-min ?xmin) (z-min ?zmin))
	?tile <- (keytile (x ?x) (z ?z))

	(test (> ?x ?xmin))
	(test (> ?z ?zmin))
	
	?target <- (floortile (variant ?v) (x ?x0) (z ?z0) (x-visited ?xvisited))
	(test (eq ?x0 (- ?x 1)))
	(test (eq ?z0 (- ?z 1)))
	
	(floorcolor (id blue) (ftype ?fblue))
	(floorcolor (id red) (ftype ?fred))

	(or
		(test (eq ?v nil))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fred ?x0))))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fblue ?x0))))
	)
=>
	(bind ?scalar 0.25)
	(if (<= (get-fs-value ?fblue ?x0) (get-fs-value ?fred ?x0))
		then
		(bind ?new-v red)
		(bind ?fs (get-fs-value ?fred ?x0))
		else
		(bind ?new-v blue)
		(bind ?fs (get-fs-value ?fblue ?x0))	
	)
	(bind ?cf (* ?fs ?scalar))

	(disable-rule-cf-calculation)
	(retract ?target)
	(assert (floortile (variant ?new-v) (x ?x0) (z ?z0) (x-visited ?xvisited) (z-visited FALSE)) CF ?cf)
	(enable-rule-cf-calculation)
)

(defrule update-up-right-on-row-scan	"Predict diagonal variant for row scan"
	(declare (salience 10))
	(scanner (row ?z))
	(params (x-max ?xmax) (z-min ?zmin))
	?tile <- (keytile (x ?x) (z ?z))

	(test (< ?x (- ?xmax 1)))
	(test (> ?z ?zmin))
	
	?target <- (floortile (variant ?v) (x ?x0) (z ?z0) (x-visited ?xvisited))
	(test (eq ?x0 (+ ?x 1)))
	(test (eq ?z0 (- ?z 1)))
	
	(floorcolor (id blue) (ftype ?fblue))
	(floorcolor (id red) (ftype ?fred))

	(or
		(test (eq ?v nil))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fred ?x0))))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fblue ?x0))))
	)
=>
	(bind ?scalar 0.25)
	(if (<= (get-fs-value ?fblue ?x0) (get-fs-value ?fred ?x0))
		then
		(bind ?new-v red)
		(bind ?fs (get-fs-value ?fred ?x0))
		else
		(bind ?new-v blue)
		(bind ?fs (get-fs-value ?fblue ?x0))	
	)
	(bind ?cf (* ?fs ?scalar))


	(disable-rule-cf-calculation)
	(retract ?target)
	(assert (floortile (variant ?new-v) (x ?x0) (z ?z0) (x-visited ?xvisited) (z-visited FALSE)) CF ?cf)
	(enable-rule-cf-calculation)
)

(defrule update-down-right-on-row-scan	"Predict diagonal variant for row scan"
	(declare (salience 10))
	(scanner (row ?z))
	(params (x-max ?xmax) (z-max ?zmax))
	?tile <- (keytile (x ?x) (z ?z))

	(test (< ?x (- ?xmax 1)))
	(test (< ?z (- ?zmax 1)))
	
	?target <- (floortile (variant ?v) (x ?x0) (z ?z0) (x-visited ?xvisited))
	(test (eq ?x0 (+ ?x 1)))
	(test (eq ?z0 (+ ?z 1)))
	
	(floorcolor (id blue) (ftype ?fblue))
	(floorcolor (id red) (ftype ?fred))

	(or
		(test (eq ?v nil))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fred ?x0))))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fblue ?x0))))
	)
=>
	(bind ?scalar 0.25)
	(if (<= (get-fs-value ?fblue ?x0) (get-fs-value ?fred ?x0))
		then
		(bind ?new-v red)
		(bind ?fs (get-fs-value ?fred ?x0))
		else
		(bind ?new-v blue)
		(bind ?fs (get-fs-value ?fblue ?x0))	
	)
	(bind ?cf (* ?fs ?scalar))

	(disable-rule-cf-calculation)
	(retract ?target)
	(assert (floortile (variant ?new-v) (x ?x0) (z ?z0) (x-visited ?xvisited) (z-visited FALSE)) CF ?cf)
	(enable-rule-cf-calculation)
)

(defrule update-down-left-on-row-scan	"Predict diagonal variant for row scan"
	(declare (salience 10))
	(scanner (row ?z))
	(params (x-min ?xmin) (z-max ?zmax))
	?tile <- (keytile (x ?x) (z ?z))

	(test (> ?x ?xmin))
	(test (< ?z (- ?zmax 1)))
	
	?target <- (floortile (variant ?v) (x ?x0) (z ?z0) (x-visited ?xvisited))
	(test (eq ?x0 (- ?x 1)))
	(test (eq ?z0 (+ ?z 1)))
	
	(floorcolor (id blue) (ftype ?fblue))
	(floorcolor (id red) (ftype ?fred))

	(or
		(test (eq ?v nil))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fred ?x0))))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fblue ?x0))))
	)
=>
	(bind ?scalar 0.25)
	(if (<= (get-fs-value ?fblue ?x0) (get-fs-value ?fred ?x0))
		then
		(bind ?new-v red)
		(bind ?fs (get-fs-value ?fred ?x0))
		else
		(bind ?new-v blue)
		(bind ?fs (get-fs-value ?fblue ?x0))	
	)
	(bind ?cf (* ?fs ?scalar))

	(disable-rule-cf-calculation)
	(retract ?target)
	(assert (floortile (variant ?new-v) (x ?x0) (z ?z0) (x-visited ?xvisited) (z-visited FALSE)) CF ?cf)
	(enable-rule-cf-calculation)
)

(defrule update-z	"Predict variant for column scan"
	(declare (salience 0))
	(params (z-max ?zmax))
	(scanner (col ?x) (row ?zmax))
	(floorcolor (id blue) (ftype ?fblue))
	(floorcolor (id red) (ftype ?fred))
	?tile <- (floortile (x ?x) (z ?z) (x-visited ?xvisited) (z-visited FALSE))
	(or
		(test (< (get-cf ?tile) (get-fs-value ?fred ?z)))
		(test (< (get-cf ?tile) (get-fs-value ?fblue ?z)))
	)
=>
	(if (<= (get-fs-value ?fblue ?z) (get-fs-value ?fred ?z))
		then
		(bind ?new-v red)
		(bind ?fs (get-fs-value ?fred ?z))
		(bind ?cf (* ?fs (- 1 (max (get-fs-value ?fblue ?z) (get-cf ?tile)))))
		else
		(bind ?new-v blue)
		(bind ?fs (get-fs-value ?fblue ?z))
		(bind ?cf (* ?fs (- 1 (max (get-fs-value ?fred ?z) (get-cf ?tile)))))
	)


	(printout t "Make " ?new-v ": " ?z ": blue " (max (get-fs-value ?fblue ?z) (get-cf ?tile)) " and red " (get-fs-value ?fred ?z) crlf)

	(disable-rule-cf-calculation)
	(retract ?tile)
	(assert (floortile (variant ?new-v) (x ?x) (z ?z) (x-visited ?xvisited) (z-visited TRUE)) CF ?cf)
	(enable-rule-cf-calculation)
)








(defrule move-scanner-row	"Iterate over rows of the grid (fires m times)"
	(declare (salience -10))
	(params (z-max ?zmax))
	?s <- (scanner (row ?z&~?zmax))
	?r <- (fns-set-for-row TRUE)
=>
	(modify ?s (row (+ ?z 1)))
	(retract ?r)
)

(defrule move-scanner-col	"Iterate over columns of the grid (fires n times)"
	(declare (salience -10))
	(params (x-max ?xmax) (z-max ?zmax))
	?s <- (scanner (col ?x&~?xmax) (row ?zmax))
	?r <- (fns-set-for-row TRUE)
=>
	(modify ?s (col (+ ?x 1)))
	(retract ?r)
)