;;==========================================================================
;;=========== informacion generale =======================
;;==========================================================================
(defglobal ?*nod-gen* = 0)

(deffacts datos
    	(mapa L 5 W 8)
        (alma 7 1)
        (alma 5 4)
        (alma 5 5)
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
    ?f <- (estado R ?x ?y $?r nivel ?nivel)
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ ?x (- ?y 1)) $?r)))
    (not (exists (estado R ?x =(- ?y 1) $?r nivel ?)))
    (not (exists (alma ?x =(- ?y 1))))
    (not (exists (obst ?x =(- ?y 1))))
    (test (> ?y 1))
    (test (< ?nivel ?prof))
    =>
    (assert (estado R ?x (- ?y 1) $?r nivel (+ ?nivel 1)) )
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule arriba-con-caja
    ?f <-(estado R ?x ?y $?r C ?x =(- ?y 1) V $?t nivel ?nivel )
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ ?x (- ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (- ?y 2)) $?t)))
    (not (exists (alma ?x =(- ?y 2))))
    (not (exists (obst ?x =(- ?y 2))))
    (test (> ?y 2))
    (test (< ?nivel ?prof))
    =>
    (assert (estado R ?x (- ?y 1) $?r C ?x (- ?y 2) V $?t nivel (+ ?nivel 1) ))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule arriba-caja-en-almacen
    ?f <-(estado R ?x ?y $?r C ?x =(- ?y 1) V $?t nivel ?nivel )
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ ?x (- ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (- ?y 2)) $?t)))
    (exists (alma ?x =(- ?y 2)))
    (not (exists (obst ?x =(- ?y 2))))
    (test (> ?y 2))
    (test (< ?nivel ?prof))
    =>
    (assert (estado R ?x (- ?y 1) $?r C ?x (- ?y 2) E $?t nivel (+ ?nivel 1) ))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)

(defrule abajo
    ?f <-(estado R ?x ?y $?r nivel ?nivel)
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ ?x (+ ?y 1)) $?r)))
    (not (exists (estado R ?x =(+ ?y 1) $?r nivel ?)))
    (not (exists (alma ?x =(+ ?y 1) )))
    (not (exists (obst ?x =(+ ?y 1) )))
    (test (< ?y ?l))
    (test (< ?nivel ?prof))
    =>
    (assert (estado R ?x (+ ?y 1) $?r nivel (+ ?nivel 1) ))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule abajo-con-caja
    ?f <-(estado R ?x ?y $?r C ?x =(+ ?y 1) V $?t nivel ?nivel )
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ ?x (+ ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (+ ?y 2)) $?t)))
    (not (exists (alma ?x =(+ ?y 2))))
    (not (exists (obst ?x =(+ ?y 2))))
    (test (< ?y (- ?l 1)))
    (test (< ?nivel ?prof))
    =>
    (assert (estado R ?x (+ ?y 1) $?r C ?x (+ ?y 2) V $?t nivel (+ ?nivel 1) ))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule abajo-caja-en-almacen
    ?f <-(estado R ?x ?y $?r C ?x =(+ ?y 1) V $?t nivel ?nivel )
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ ?x (+ ?y 2)) $?r)))
    (test (not (member$ (create$ ?x (+ ?y 2)) $?t)))
    (exists (alma ?x =(+ ?y 2)))
    (not (exists (obst ?x =(+ ?y 2))))
    (test (< ?y (- ?l 1)))
    (test (< ?nivel ?prof))
    =>
    (assert (estado R ?x (+ ?y 1) $?r C ?x (+ ?y 2) E $?t nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)

(defrule derecha
    ?f <- (estado R ?x ?y $?r nivel ?nivel)
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ (+ ?x 1) ?y) $?r)))
    (not (exists (estado R =(+ ?x 1) ?y $?r nivel ?)))
    (not (exists (alma =(+ ?x 1) ?y )))
    (not (exists (obst =(+ ?x 1) ?y )))
    (test (< ?x ?w))
    (test (< ?nivel ?prof))
    =>
    (assert (estado  R (+ ?x 1) ?y $?r nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule derecha-con-caja
    ?f <-(estado R ?x ?y $?r C =(+ ?x 1) ?y V $?t nivel ?nivel )
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ (+ ?x 2) ?y) $?r)))
    (test (not (member$ (create$ (+ ?x 2) ?y) $?t)))
    (not (exists (alma =(+ ?x 2) ?y )))
    (not (exists (obst =(+ ?x 2) ?y)))
    (test (< ?x (- ?w 1)))
    (test (< ?nivel ?prof))
    =>
    (assert (estado R (+ ?x 1) ?y  $?r C (+ ?x 2) ?y V $?t nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule derecha-caja-en-almacen
    ?f <-(estado R ?x ?y $?r C =(+ ?x 1) ?y V $?t nivel ?nivel )
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ (+ ?x 2) ?y) $?r)))
    (test (not (member$ (create$ (+ ?x 2) ?y) $?t)))
    (exists (alma =(+ ?x 2) ?y))
    (not (exists (obst =(+ ?x 2) ?y)))
    (test (< ?x (- ?w 1)))
    (test (< ?nivel ?prof))
    =>
    (assert (estado R (+ ?x 1) ?y $?r C (+ ?x 2) ?y E $?t nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)

(defrule izquierda
    ?f <- (estado R ?x ?y $?r nivel ?nivel)
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ (- ?x 1) ?y) $?r)))
    (not (exists (estado R =(- ?x 1) ?y $?r nivel ?)))
    (not (exists (alma =(- ?x 1) ?y )))
    (not (exists (obst =(- ?x 1) ?y )))
    (test (> ?x 1))
    (test (< ?nivel ?prof))
    =>
    (assert (estado  R (- ?x 1) ?y $?r  nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule izquierda-con-caja
    ?f <-(estado R ?x ?y $?r C =(- ?x 1) ?y V $?t nivel ?nivel)
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ (- ?x 2) ?y) $?r)))
    (test (not (member$ (create$ (- ?x 2) ?y) $?t)))
    (not (exists (alma =(- ?x 2) ?y)))
    (not (exists (obst =(- ?x 2) ?y)))
    (test (> ?x 2))
    (test (< ?nivel ?prof))
    =>
    (assert (estado R (- ?x 1) ?y $?r C (- ?x 2) ?y V $?t nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule izquierda-caja-en-almacen
    ?f <-(estado R ?x ?y $?r C =(- ?x 1) ?y V $?t nivel ?nivel )
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ (- ?x 2) ?y) $?r)))
    (test (not (member$ (create$ (- ?x 2) ?y) $?t)))
    (exists (alma =(- ?x 2) ?y))
    (not (exists (obst =(- ?x 2) ?y)))
    (test (> ?x 2))
    (test (< ?nivel ?prof))
    =>
    (assert (estado R (- ?x 1) ?y $?r C (- ?x 2) ?y E $?t nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
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
(defrule no_solucion
    (declare (salience -99))
    (estado $? )
    =>
    (printout t "SOLUCION NO ENCONTRADA" crlf)
    (printout t "NUMERO DE NODOS EXPANDIDOS O REGLAS DISPARADAS " ?*nod-gen* crlf)
    (halt)
)
(deffunction inicio()
    (reset)

    (printout t "sokoban game" crlf)
    (printout t "Profundidad Maxima:= " )
	(bind ?prof (read))
    (printout t "Tipo de Busqueda " crlf "    1.- Anchura" crlf "    2.- Profundidad" crlf )
    (bind ?a (read))
    (if (= ?a 1)
           then    (set-strategy breadth)
           else   (set-strategy depth))

    (assert (estado R 1 4 C 3 4 V C 2 2 V C 6 2 V nivel 0))
    (assert (profundidad-maxima ?prof))
)
