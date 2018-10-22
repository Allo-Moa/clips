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
    ?f <- (estado R ?x ?y $?r movimiento ?mov nivel ?nivel)
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (- ?y 1)) $?r)))
    (not (exists (alma ?x =(- ?y 1))))
    (not (exists (obst ?x =(- ?y 1))))

    (test (neq ?mov abajo))
    (test (> ?y 1))
    =>
    (assert (estado R ?x (- ?y 1) $?r movimiento arriba nivel (+ ?nivel 1)) )
    (bind ?*nod-gen* (+ ?*nod-gen* 1)))
)
(defrule arriba-con-caja
    ?f <-(estado R ?x ?y $?r C ?x =(- ?y 1) V $?t movimiento ? nivel ?nivel )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (- ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (- ?y 2)) $?t)))

    (not (exists (alma ?x =(- ?y 2))))
    (not (exists (obst ?x =(- ?y 2))))
    (test (> ?y 2))
    =>
    (assert (estado R ?x (- ?y 1) $?r C ?x (- ?y 2) V $?t movimiento nulo nivel (+ ?nivel 1) ))
    (bind ?*nod-gen* (+ ?*nod-gen* 1)))
)
(defrule arriba-caja-en-almacen
    ?f <-(estado R ?x ?y $?r C ?x =(- ?y 1) V $?t movimiento ? nivel ?nivel )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (- ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (- ?y 2)) $?t)))

    (exists (alma ?x =(- ?y 2)))
    (not (exists (obst ?x =(- ?y 2))))
    (test (> ?y 2))
    =>
    (assert (estado R ?x (- ?y 1) $?r C ?x (- ?y 2) E $?t movimiento nulo nivel (+ ?nivel 1) ))
    (bind ?*nod-gen* (+ ?*nod-gen* 1)))
)

(defrule abajo
    ?f <-(estado R ?x ?y $?r movimiento ?mov nivel ?nivel)
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (+ ?y 1)) $?r)))
    (not (exists (alma ?x =(+ ?y 1) )))
    (not (exists (obst ?x =(+ ?y 1) )))

    (test (neq ?mov arriba))
    (test (< ?y ?l))
    =>
    (assert (estado R ?x (+ ?y 1) $?r movimiento abajo nivel (+ ?nivel 1) ))
    (bind ?*nod-gen* (+ ?*nod-gen* 1)))
)
(defrule abajo-con-caja
    ?f <-(estado R ?x ?y $?r C ?x =(+ ?y 1) V $?t movimiento ? nivel ?nivel )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (+ ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (+ ?y 2)) $?t)))

    (not (exists (alma ?x =(+ ?y 2))))
    (not (exists (obst ?x =(+ ?y 2))))
    (test (< ?y (- ?l 1)))
    =>
    (assert (estado R ?x (+ ?y 1) $?r C ?x (+ ?y 2) V $?t movimiento nulo nivel (+ ?nivel 1) ))
    (bind ?*nod-gen* (+ ?*nod-gen* 1)))
)
(defrule abajo-caja-en-almacen
    ?f <-(estado R ?x ?y $?r C ?x =(+ ?y 1) V $?t movimiento ? nivel ?nivel )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (+ ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (+ ?y 2)) $?t)))

    (exists (alma ?x =(+ ?y 2)))
    (not (exists (obst ?x =(+ ?y 2))))
    (test (< ?y (- ?l 1)))
    =>
    (assert (estado R ?x (+ ?y 1) $?r C ?x (+ ?y 2) E $?t movimiento nulo nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1)))
)

(defrule derecha
    ?f <- (estado R ?x ?y $?r movimiento ?mov nivel ?nivel)
    (mapa L ?l W ?w)
    (test (not (member$ (create$ (+ ?x 1) ?y) $?r)))
    (not (exists (alma =(+ ?x 1) ?y )))
    (not (exists (obst =(+ ?x 1) ?y )))

    (test (neq ?mov izquierda))
    (test (< ?x ?w))
    =>
    (assert (estado  R (+ ?x 1) ?y $?r movimiento derecha nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1)))
)
(defrule derecha-con-caja
    ?f <-(estado R ?x ?y $?r C =(+ ?x 1) ?y V $?t movimiento ? nivel ?nivel )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ (+ ?x 2) ?y) $?r)))
    (test (not (member$ (create$ (+ ?x 2) ?y) $?t)))

    (not (exists (alma =(+ ?x 2) ?y )))
    (not (exists (obst =(+ ?x 2) ?y)))
    (test (< ?x (- ?w 1)))
    =>
    (assert (estado R (+ ?x 1) ?y  $?r C (+ ?x 2) ?y V $?t movimiento nulo nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1)))
)
(defrule abajo-caja-en-almacen
    ?f <-(estado R ?x ?y $?r C =(+ ?x 1) ?y V $?t movimiento ? nivel ?nivel )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (+ ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (+ ?y 2)) $?t)))

    (exists (alma =(+ ?x 2) ?y))
    (not (exists (obst ?x =(+ ?y 2))))
    (test (< ?x (- ?w 1)))
    =>
    (assert (estado R (+ ?x 1) ?y $?r C (+ ?x 2) ?y E $?t movimiento nulo nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1)))
)

(defrule izquierda
    ?f <- (estado R ?x ?y $?r movimiento ?mov nivel ?nivel)
    (mapa L ?l W ?w)
    (test (not (member$ (create$ (- ?x 1) ?y) $?r)))
    (not (exists (alma =(- ?x 1) ?y )))
    (not (exists (obst =(- ?x 1) ?y )))

    (test (neq ?mov derecha))
    (test (> ?x 1))
    =>
    (assert (estado  R (- ?x 1) ?y $?r  movimiento izquierda nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1)))
)
(defrule izquierda-con-caja
    ?f <-(estado R ?x ?y $?r C =(- ?x 1) ?y V $?t movimiento ? nivel ?nivel)
    (mapa L ?l W ?w)
    (test (not (member$ (create$ (- ?x 2) ?y) $?r)))
    (test (not (member$ (create$ (- ?x 2) ?y) $?t)))

    (not (exists (alma =(- ?x 2) ?y)))
    (not (exists (obst =(- ?x 2) ?y)))
    (test (> ?x 2))
    =>
    (assert (estado R (- ?x 1) ?y $?r C (- ?x 2) ?y V $?t movimiento nulo nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1)))
)
(defrule abajo-caja-en-almacen
    ?f <-(estado R ?x ?y $?r C =(- ?x 1) ?y V $?t movimiento ? nivel ?nivel )
    (mapa L ?l W ?w)
    (test (not (member$ (create$ ?x (+ ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (+ ?y 2)) $?t)))

    (exists (alma =(- ?x 2) ?y))
    (not (exists (obst =(- ?x 2) ?y)))
    (test (> ?x 2))
    =>
    (assert (estado R (- ?x 1) ?y $?r C (- ?x 2) ?y E $?t movimiento nulo nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1)))
)


;;=============================================================================
;;======= inicio =======================================
;;=============================================================================

(defrule objetivo
    (declare (salience 100))
    ?f <- (estado $?r E $?t nivel ?n)
    (test (not (member$ V $?r )))
    (test (not (member$ V $?t )))
    =>
    (printout t "FINISH: Problem solved" crlf)
    (printout t "Depth level : " ?n crlf)
    (printout t "Number of Nodes expended " ?*nod-gen* crlf)
    (halt)
)
(deffunction inicio()
    (reset)

    (printout t "sokoban game" crlf)
    (printout t "Profundidad Maxima:= " )
	(bind ?prof (read))

    (assert (estado R 1 4 C 3 4 V movimiento nulo nivel 0))
    (assert (profundidad-maxima ?prof))
)
