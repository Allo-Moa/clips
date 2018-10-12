;;==========================================================================
;;=========== informacion generale =======================
;;==========================================================================
(defglobal ?*nod-gen* = 0)

(deffacts datos
    	(mapa V 1 7 L 5 W 8)
        (obst 1 3)
        (obst 1 4)
        (obst 3 4)
        (obst 4 4)
        (obst 4 5)
        (obst 4 3)
        (obst 5 3)
        (obst 3 8)
)


;;============================================================================
;;===== reglas =======
;;============================================================================

;; (estado R ?x ?y Cx $?cx Cy $?cy A V )

(defrule arriba
    ?f <- (estado R ?x ?y $?r)
    (mapa V ?x2 ?y2 L ?l W ?w)
    (test (not (member$ (create$ ?x (- ?y 1)) $?r)))
    (not (exists (obst ?x =(- ?y 1))))
    (test (> ?y 1))
    =>
    (assert (estado R ?x (- ?y 1) $?r))
)
(defrule arriba-con-caja
    ?f <-(estado R ?x ?y $?r C ?x =(- ?y 1) $?t )
    (mapa V ?x2 ?y2 L ?l W ?w)
    (not (exists (obst ?x =(- ?y 1))))
    (test (> ?y 1))
    =>
    (assert (estado R ?x (- ?y 1) $?r C ?x =(- ?y 1) $?t))
)

(defrule abajo
    ?f <-(estado R ?x ?y $?r)
    (mapa V ?x2 ?y2 L ?l W ?w)
    (not (exists (obst ?x =(+ ?y 1))))
    (test (< ?y ?l))
    =>
    (assert (estado R ?x (+ ?y 1) $?r))
)

(defrule derecha
    ?f <- (estado R ?x ?y $?r)
    (mapa V ?x2 ?y2 L ?l W ?w)
    (not (exists (obst =(+ ?x 1) ?y )))
    (test (< ?x ?w))
    =>
    (assert (estado  R (+ ?x 1) ?y $?r))
)

(defrule izquierda
    ?f <- (estado R ?x ?y $?r)
    (mapa V ?x2 ?y2 L ?l W ?w)
    (not (exists (obst =(- ?x 1) ?y )))
    (test (> ?x 1))
    =>
    (assert (estado  R (- ?x 1) ?y $?r))
)

;;=============================================================================
;;======= inicio =======================================
;;=============================================================================


(deffunction inicio()
    (reset)

    (printout t "sokoban game")
    (printout t "Profundidad Maxima:= " )
	(bind ?prof (read))

    (assert (estado R 1 4 C 2 2 C 3 4 A V))
    (assert (profundidad-maxima ?prof))
)
