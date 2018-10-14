;;==========================================================================
;;=========== informacion generale =======================
;;==========================================================================
(defglobal ?*nod-gen* = 0)

(deffacts datos
    	(mapa L 5 W 8)
        (alma 7 1)
        (obst 1 3)
        (obst 4 1)
        (obst 4 4)
        (obst 4 5)
        (obst 4 3)
        (obst 5 3)
        (obst 8 3)
)


;;============================================================================
;;===== reglas =======
;;============================================================================

;; (estado R ?x ?y Cx $?cx Cy $?cy A V )

(defrule arriba
    ?f <- (estado R ?x ?y $?r)
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (- ?y 1)) $?r)))
    (not (exists (alma ?x =(- ?y 1))))
    (not (exists (obst ?x =(- ?y 1))))
    (test (> ?y 1))
    =>
    (assert (estado R ?x (- ?y 1) $?r))
)
(defrule arriba-con-caja
    ?f <-(estado R ?x ?y $?r C ?x =(- ?y 1) V $?t )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (- ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (- ?y 2)) $?t)))

    (not (exists (alma ?x =(- ?y 2))))
    (not (exists (obst ?x =(- ?y 2))))
    (test (> ?y 2))
    =>
    (assert (estado R ?x (- ?y 1) $?r C ?x (- ?y 2) V $?t))
)
(defrule arriba-caja-en-almacen
    ?f <-(estado R ?x ?y $?r C ?x =(- ?y 1) V $?t )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (- ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (- ?y 2)) $?t)))

    (exists (alma ?x =(- ?y 2)))
    (not (exists (obst ?x =(- ?y 2))))
    (test (> ?y 2))
    =>
    (assert (estado R ?x (- ?y 1) $?r C ?x (- ?y 2) E $?t))
)

(defrule abajo
    ?f <-(estado R ?x ?y $?r)
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (+ ?y 1)) $?r)))
    (not (exists (alma ?x =(+ ?y 1) )))
    (not (exists (obst ?x =(+ ?y 1) )))
    (test (< ?y ?l))
    =>
    (assert (estado R ?x (+ ?y 1) $?r))
)
(defrule abajo-con-caja
    ?f <-(estado R ?x ?y $?r C ?x =(+ ?y 1) V $?t )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (+ ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (+ ?y 2)) $?t)))

    (not (exists (alma ?x =(+ ?y 2))))
    (not (exists (obst ?x =(+ ?y 2))))
    (test (< ?y (- ?l 1)))
    =>
    (assert (estado R ?x (+ ?y 1) $?r C ?x (+ ?y 2) V $?t))
)
(defrule abajo-caja-en-almacen
    ?f <-(estado R ?x ?y $?r C ?x =(+ ?y 1) V $?t )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (+ ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (+ ?y 2)) $?t)))

    (exists (alma ?x =(+ ?y 2)))
    (not (exists (obst ?x =(+ ?y 2))))
    (test (< ?y (- ?l 1)))
    =>
    (assert (estado R ?x (+ ?y 1) $?r C ?x (+ ?y 2) E $?t))
)

(defrule derecha
    ?f <- (estado R ?x ?y $?r)
    (mapa L ?l W ?w)
    (test (not (member$ (create$ (+ ?x 1) ?y) $?r)))
    (not (exists (alma =(+ ?x 1) ?y )))
    (not (exists (obst =(+ ?x 1) ?y )))
    (test (< ?x ?w))
    =>
    (assert (estado  R (+ ?x 1) ?y $?r))
)
(defrule derecha-con-caja
    ?f <-(estado R ?x ?y $?r C =(+ ?x 1) ?y V $?t )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ (+ ?x 2) ?y) $?r)))
    (test (not (member$ (create$ (+ ?x 2) ?y) $?t)))

    (not (exists (alma =(+ ?x 2) ?y )))
    (not (exists (obst =(+ ?x 2) ?y)))
    (test (< ?x (- ?w 1)))
    =>
    (assert (estado R (+ ?x 1) ?y  $?r C (+ ?x 2) ?y V $?t))
)
(defrule abajo-caja-en-almacen
    ?f <-(estado R ?x ?y $?r C =(+ ?x 1) ?y V $?t )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (+ ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (+ ?y 2)) $?t)))

    (exists (alma =(+ ?x 2) ?y))
    (not (exists (obst ?x =(+ ?y 2))))
    (test (< ?x (- ?w 1)))
    =>
    (assert (estado R (+ ?x 1) ?y $?r C (+ ?x 2) ?y E $?t))
)

(defrule izquierda
    ?f <- (estado R ?x ?y $?r)
    (mapa L ?l W ?w)
    (test (not (member$ (create$ (- ?x 1) ?y) $?r)))
    (not (exists (alma =(- ?x 1) ?y )))
    (not (exists (obst =(- ?x 1) ?y )))
    (test (> ?x 1))
    =>
    (assert (estado  R (- ?x 1) ?y $?r))
)
(defrule izquierda-con-caja
    ?f <-(estado R ?x ?y $?r C =(- ?x 1) ?y V $?t )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ (- ?x 2) ?y) $?r)))
    (test (not (member$ (create$ (- ?x 2) ?y) $?t)))

    (not (exists (alma =(- ?x 2) ?y)))
    (not (exists (obst =(- ?x 2) ?y)))
    (test (> ?x 2))
    =>
    (assert (estado R (- ?x 1) ?y $?r C (- ?x 2) ?y V $?t))
)
(defrule abajo-caja-en-almacen
    ?f <-(estado R ?x ?y $?r C =(- ?x 1) ?y V $?t )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (+ ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (+ ?y 2)) $?t)))

    (exists (alma =(- ?x 2) ?y))
    (not (exists (obst =(- ?x 2) ?y)))
    (test (> ?x 2))
    =>
    (assert (estado R (- ?x 1) ?y $?r C (- ?x 2) ?y E $?t))
)


;;=============================================================================
;;======= inicio =======================================
;;=============================================================================

(defrule objetivo
    (declare (salience 100))
    ?f <- (estado $?r E $?t)
    (test (not (member$ V $?r )))
    (test (not (member$ V $?t )))
    =>
    (printout t "FINISH" crlf)
    (halt)
)
(deffunction inicio()
    (reset)

    (printout t "sokoban game")
    (printout t "Profundidad Maxima:= " )
	(bind ?prof (read))

    (assert (estado R 1 4 C 3 4 V))
    (assert (profundidad-maxima ?prof))
)
