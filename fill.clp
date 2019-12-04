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

(deftemplate file 
	(slot fname)
	(slot fptr)
	(slot mode)
)




(defrule init-f-reader
	(declare (salience 90))
	?output-file <- (file (fname ?src) (mode r) (fptr nil))
=>
	(open ?src readFile "r")
	(modify ?output-file (fptr readFile))
)

(defrule read-next
	(declare (salience 89))
	(file (fptr ?reader&~nil) (mode r))
	(or (keytile) (not (keytile)))
	(params)
	(not (read-EOF))
=>
	(bind ?x (read ?reader))
	(if (neq ?x EOF)
	then
		(bind ?z (read ?reader))
		(bind ?v (read ?reader))
		(bind ?cf (read ?reader))

		(disable-rule-cf-calculation)
		(assert (keytile (x ?x) (z ?z) (variant ?v)) CF ?cf)
		(enable-rule-cf-calculation)

	else
		(assert (read-EOF))
	)
)

(defrule read-first
	(declare (salience 89))
	(file (fptr ?reader&~nil) (mode r))
	(not (params))
=>
	(bind ?x-min (read ?reader))
	(bind ?x-max (read ?reader))
	(bind ?z-min (read ?reader))
	(bind ?z-max (read ?reader))
	(assert (params (x-min ?x-min) (x-max ?x-max) (z-min ?z-min) (z-max ?z-max)))
)

(defrule close-f-reader
	(declare (salience 88))
	?f <- (file (fptr ?reader&~nil) (mode r))
=>
	(close ?reader)
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

(defrule remove-overflow-keys "Remove key tiles that overflow the map bounds"
	(declare (salience 60))
	(params (x-min ?xmin) (x-max ?xmax) (z-min ?zmin) (z-max ?zmax))
	?k1 <- (keytile (x ?x) (z ?z))
	(or
		(test (< ?x ?xmin))
		(test (>= ?x ?xmax))
		(test (< ?z ?zmin))
		(test (>= ?z ?zmax))
	)
=>
	(printout t "Retracting overflow keytile at " ?x " " ?z crlf)
	(retract ?k1)
)


(defrule make-init-tile "Populate tile at the origin of the grid"
	(declare (salience 40))
	(params (x-min ?xmin) (z-min ?zmin))
	(not (floortile (x ?xmin) (z ?zmin)))
	(not (keytile (x ?xmin) (z ?zmin)))
=>
	(assert (floortile (x ?xmin) (z ?zmin)))
)

(defrule make-adj-tile-x "Populate grid on x-axis"
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

(defrule make-adj-tile-z "Populate grid on z-axis"
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
	?fv-y <- (floorcolor (id yellow))
	?fv-s <- (floorcolor (id red))
	(not (fns-set-for-row TRUE))
=>
	(retract ?fv-r)
	(retract ?fv-y)
	(retract ?fv-s)
	(assert (floorcolor (id blue) (ftype (0 0))))
	(assert (floorcolor (id yellow) (ftype (0 0))))
	(assert (floorcolor (id red) (ftype (0 0))))
	(assert (fns-set-for-row TRUE))
)

(defrule init-fns	"Initialize variant profiles"
	(declare (salience 31))
=>
	(assert (floorcolor (id blue) (ftype (0 0))))
	(assert (floorcolor (id yellow) (ftype (0 0))))
	(assert (floorcolor (id red) (ftype (0 0))))
	(assert (scanner (row 0) (col 0)))
)


(defrule inform-variant-row	"Develop color variant profile"
	(declare (salience 31))
	(params (z-max ?zmax))
	(scanner (row ?z&~?zmax))
	?ft <- (keytile (variant ?color) (x ?x) (z ?z) (x-visited FALSE))
	?v <- (floorcolor (id ?color) (ftype ?f))
=>
	(bind ?scalar 4)
	(bind ?cf (get-cf ?ft))
	(bind ?base (* ?cf ?scalar))
	(bind ?t (fuzzy-union ?f (create-fuzzy-value floortype (PI ?base ?x))))
	(modify ?v (ftype ?t))

	(disable-rule-cf-calculation)
	(retract ?ft)
	(assert (keytile (variant ?color) (x ?x) (z ?z) (x-visited TRUE)) CF ?cf)
	(enable-rule-cf-calculation)
)


(defrule inform-variant-col		"Develop color variant profile"
	(declare (salience 30))
	(params (x-max ?xmax) (z-max ?zmax))
	(scanner (col ?x&~?xmax) (row ?zmax))
	?ft <- (keytile (variant ?color) (x ?x) (z ?z) (x-visited TRUE) (z-visited FALSE))
	?v <- (floorcolor (id ?color) (ftype ?f))
=>
	(bind ?scalar 4)
	(bind ?cf (get-cf ?ft))
	(bind ?base (* ?cf ?scalar))
	(bind ?t (fuzzy-union ?f (create-fuzzy-value floortype (PI ?base ?z))))
	(modify ?v (ftype ?t))
	
	(disable-rule-cf-calculation)
	(retract ?ft)
	(assert (keytile (variant ?color) (x ?x) (z ?z) (z-visited TRUE)) CF ?cf)
	(enable-rule-cf-calculation)
)





(defrule plot-variants "Plot membership functions along the scanned row/column"
	(declare (salience 21))
	(floorcolor (id red) (ftype ?r))
	(floorcolor (id yellow) (ftype ?y))
	(floorcolor (id blue) (ftype ?b))
=>
	(plot-fuzzy-value t "ryb" nil nil ?r ?y ?b)
)






(defrule update-x	"Predict variant for row scan"
	(declare (salience 10))
	(params (z-max ?zmax))
	(scanner (row ?z&~?zmax))
	(floorcolor (id blue) (ftype ?fblue))
	(floorcolor (id yellow) (ftype ?fyellow))
	(floorcolor (id red) (ftype ?fred))
	?tile <- (floortile (variant ?v) (x ?x) (z ?z) (x-visited FALSE))


	(or
		(test (<= (get-cf ?tile) (get-fs-value ?fred ?x)))
		(test (<= (get-cf ?tile) (get-fs-value ?fblue ?x)))
		(test (<= (get-cf ?tile) (get-fs-value ?fyellow ?x)))
		(test (eq ?v nil))
	)
=>
	(bind ?m-val (max (get-fs-value ?fred ?x) (get-fs-value ?fyellow ?x) (get-fs-value ?fblue ?x)))

	(if (eq (get-fs-value ?fred ?x) ?m-val)
		then
		(bind ?new-v red)
		(bind ?fs (get-fs-value ?fred ?x))
		(bind ?cf (* ?fs (- 1 (max (get-fs-value ?fblue ?x) (get-fs-value ?fyellow ?x)))))
		else
		(if (eq (get-fs-value ?fyellow ?x) ?m-val)
			then
			(bind ?new-v yellow)
			(bind ?fs (get-fs-value ?fyellow ?x))
			(bind ?cf (* ?fs (- 1 (max (get-fs-value ?fred ?x) (get-fs-value ?fblue ?x)))))
			else
			(bind ?new-v blue)
			(bind ?fs (get-fs-value ?fblue ?x))
			(bind ?cf (* ?fs (- 1 (max (get-fs-value ?fred ?x) (get-fs-value ?fyellow ?x)))))
		)
	)

	(printout t "Make " ?new-v " " ?x ": blue " (get-fs-value ?fblue ?x) " and yellow " (get-fs-value ?fyellow ?x) " and red " (get-fs-value ?fred ?x) crlf)

	(disable-rule-cf-calculation)
	(retract ?tile)
	(assert (floortile (variant ?new-v) (x ?x) (z ?z) (x-visited TRUE) (z-visited FALSE)) CF ?cf)
	(enable-rule-cf-calculation)
)

(defrule update-left-updown-on-row-scan	"Predict diagonal variant for row scan"
	(declare (salience 10))
	(scanner (row ?z))
	?tile <- (keytile (x ?x) (z ?z))
	
	?target <- (floortile (variant ?v) (x ?x0) (z ?z0) (x-visited ?xvisited))
	(test (eq ?x0 (- ?x 1)))
	(or
		(test (eq ?z0 (- ?z 1)))
		(test (eq ?z0 (+ ?z 1)))
	)
	
	(floorcolor (id blue) (ftype ?fblue))
	(floorcolor (id yellow) (ftype ?fyellow))
	(floorcolor (id red) (ftype ?fred))

	(or
		(test (eq ?v nil))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fred ?x0))))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fyellow ?x0))))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fblue ?x0))))
	)
=>
	(bind ?scalar 0.25)
	(bind ?m-val (max (get-fs-value ?fred ?x0) (get-fs-value ?fyellow ?x0) (get-fs-value ?fblue ?x0)))

	(if (eq ?m-val (get-fs-value ?fred ?x0))
		then
		(bind ?new-v red)
		(bind ?fs (get-fs-value ?fred ?x0))
		else
		(if (eq ?m-val (get-fs-value ?fyellow ?x0))
			then
			(bind ?new-v yellow)
			(bind ?fs (get-fs-value ?fyellow ?x0))	
			else
			(bind ?new-v blue)
			(bind ?fs (get-fs-value ?fblue ?x0))	
		)
	)
	(bind ?cf (* ?fs ?scalar))

	(disable-rule-cf-calculation)
	(retract ?target)
	(assert (floortile (variant ?new-v) (x ?x0) (z ?z0) (x-visited ?xvisited) (z-visited FALSE)) CF ?cf)
	(enable-rule-cf-calculation)
)



(defrule update-right-updown-on-row-scan	"Predict diagonal variant for row scan"
	(declare (salience 10))
	(scanner (row ?z))
	?tile <- (keytile (x ?x) (z ?z))
	
	?target <- (floortile (variant ?v) (x ?x0) (z ?z0) (x-visited ?xvisited))
	(test (eq ?x0 (+ ?x 1)))
	(or
		(test (eq ?z0 (- ?z 1)))
		(test (eq ?z0 (+ ?z 1)))
	)
	
	(floorcolor (id blue) (ftype ?fblue))
	(floorcolor (id yellow) (ftype ?fyellow))
	(floorcolor (id red) (ftype ?fred))
	
	(or
		(test (eq ?v nil))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fred ?x0))))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fyellow ?x0))))
		(test (< (get-cf ?target) (* 0.25 (get-fs-value ?fblue ?x0))))
	)
=>
	(bind ?scalar 0.25)
	(bind ?m-val (max (get-fs-value ?fred ?x0) (get-fs-value ?fyellow ?x0) (get-fs-value ?fblue ?x0)))

	(if (eq ?m-val (get-fs-value ?fred ?x0))
		then
		(bind ?new-v red)
		(bind ?fs (get-fs-value ?fred ?x0))
		else
		(if (eq ?m-val (get-fs-value ?fyellow ?x0))
			then
			(bind ?new-v yellow)
			(bind ?fs (get-fs-value ?fyellow ?x0))	
			else
			(bind ?new-v blue)
			(bind ?fs (get-fs-value ?fblue ?x0))	
		)
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
	(floorcolor (id yellow) (ftype ?fyellow))
	(floorcolor (id red) (ftype ?fred))
	?tile <- (floortile (x ?x) (z ?z) (x-visited ?xvisited) (z-visited FALSE))
	(or
		(test (< (get-cf ?tile) (get-fs-value ?fred ?z)))
		(test (< (get-cf ?tile) (get-fs-value ?fyellow ?z)))
		(test (< (get-cf ?tile) (get-fs-value ?fblue ?z)))
	)
=>
	(bind ?m-val (max (get-fs-value ?fred ?z) (get-fs-value ?fyellow ?z) (get-fs-value ?fblue ?z)))

	(if (eq (get-fs-value ?fred ?z) ?m-val)
		then
		(bind ?new-v red)
		(bind ?fs (get-fs-value ?fred ?z))
		(bind ?cf (* ?fs (- 1 (max (get-fs-value ?fblue ?z) (get-fs-value ?fyellow ?z)))))
		else
		(if (eq (get-fs-value ?fyellow ?z) ?m-val)
			then
			(bind ?new-v yellow)
			(bind ?fs (get-fs-value ?fyellow ?z))
			(bind ?cf (* ?fs (- 1 (max (get-fs-value ?fred ?z) (get-fs-value ?fblue ?z)))))
			else
			(bind ?new-v blue)
			(bind ?fs (get-fs-value ?fblue ?z))
			(bind ?cf (* ?fs (- 1 (max (get-fs-value ?fred ?z) (get-fs-value ?fyellow ?z)))))
		)
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


(defrule init-f-writer
(declare (salience -90))
?output-file <- (file (fname ?dst) (mode w) (fptr nil))
=>
(open ?dst writeFile "w")
(modify ?output-file (fptr writeFile))
)

(defrule write-floortile
(declare (salience -90))
(file (fptr ?writer&~nil) (mode w))
(or
	?t <- (floortile (x ?x) (z ?z) (variant ?color))
	?t <- (keytile (x ?x) (z ?z) (variant ?color))
)
=>
(printout t "Writing tile " ?x " " ?z crlf)
(printout ?writer ?x "," ?z "," ?color crlf)
)

(defrule close-f-writer
(declare (salience -91))
?f <- (file (fptr ?writer&~nil) (mode w))
=>
(close ?writer)
)