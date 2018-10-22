;;==========================================================================
;;=========== informacion generale =======================
;;==========================================================================
(defglobal ?*nod-gen* = 0)

(deffacts datos
    	(mapa L 3 W 4)
        (alma 1 1)

        (obst 1 3)
        (obst 2 2)
        (obst 3 2)

)


;;============================================================================
;;===== reglas =======
;;============================================================================


(defrule arriba
    ?f <- (estado R ?x ?y $?r nivel ?nivel)
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ ?x (abs (mod (- ?y 1) ?l))) $?r)))
    (not (exists (estado R ?x =(abs (mod (- ?y 1) ?l)) $?r nivel ?)))
    (not (exists (alma ?x =(abs (mod (- ?y 1) ?l)))))
    (not (exists (obst ?x =(abs (mod (- ?y 1) ?l)))))
    (test (< ?nivel ?prof))
    =>
    (assert (estado R ?x (abs (mod (- ?y 1) ?l)) $?r nivel (+ ?nivel 1)) )
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule arriba-con-caja
    (mapa L ?l W ?w)
    ?f <-(estado R ?x ?y $?r C ?x =(abs (mod (- ?y 1) ?l)) V $?t nivel ?nivel )

    (profundidad-maxima ?prof)
    (test (not (member$ (create$ ?x (abs (mod (- ?y 2) ?l))) $?r)))
    (test (not (member$ (create$ ?x (abs (mod (- ?y 2) ?l))) $?t)))
    (not (exists (alma ?x =(abs (mod (- ?y 2) ?l)))))
    (not (exists (obst ?x =(abs (mod (- ?y 2) ?l)))))

    (test (< ?nivel ?prof))
    =>
    (assert (estado R ?x (abs (mod (- ?y 1) ?l)) $?r C ?x (abs (mod (- ?y 2) ?l)) V $?t nivel (+ ?nivel 1) ))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule arriba-caja-en-almacen
    (mapa L ?l W ?w)
    ?f <-(estado R ?x ?y $?r C ?x =(abs (mod (- ?y 1) ?l)) V $?t nivel ?nivel )
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ ?x (abs (mod (- ?y 2) ?l))) $?r)))
    (test (not (member$ (create$ ?x (abs (mod (- ?y 2) ?l))) $?t)))
    (exists (alma ?x =(abs (mod (- ?y 2) ?l))))
    (not (exists (obst ?x =(abs (mod (- ?y 2) ?l)))))

    (test (< ?nivel ?prof))
    =>
    (assert (estado R ?x (abs (mod (- ?y 1) ?l))  $?r C ?x (mod (- ?y 2) ?l) E $?t nivel (+ ?nivel 1) ))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)

(defrule abajo
    ?f <-(estado R ?x ?y $?r nivel ?nivel)
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ ?x (abs (mod (+ ?y 1) ?l))) $?r)))
    (not (exists (estado R ?x =(abs (mod (+ ?y 1) ?l)) $?r nivel ?)))
    (not (exists (alma ?x =(abs (mod (+ ?y 1) ?l)) )))
    (not (exists (obst ?x =(abs (mod (+ ?y 1) ?l)) )))

    (test (< ?nivel ?prof))
    =>
    (assert (estado R ?x (mod (abs (mod (+ ?y 1) ?l)) ?l) $?r nivel (+ ?nivel 1) ))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule abajo-con-caja
    (mapa L ?l W ?w)
    ?f <-(estado R ?x ?y $?r C ?x =(abs (mod (+ ?y 1) ?l)) V $?t nivel ?nivel )
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ ?x (abs (mod (+ ?y 2) ?l))) $?r)))
    (test (not (member$ (create$ ?x (abs (mod (+ ?y 2) ?l))) $?t)))
    (not (exists (alma ?x =(abs (mod (+ ?y 2) ?l)))))
    (not (exists (obst ?x =(abs (mod (+ ?y 2) ?l)))))

    (test (< ?nivel ?prof))
    =>
    (assert (estado R ?x (mod (abs (mod (+ ?y 1) ?l)) ?l) $?r C ?x (abs (mod (+ ?y 2) ?l)) V $?t nivel (+ ?nivel 1) ))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule abajo-caja-en-almacen
    (mapa L ?l W ?w)
    ?f <-(estado R ?x ?y $?r C ?x =(abs (mod (+ ?y 1) ?l)) V $?t nivel ?nivel )
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ ?x (abs (mod (+ ?y 2) ?l))) $?r)))
    (test (not (member$ (create$ ?x (abs (mod (+ ?y 2) ?l))) $?t)))
    (exists (alma ?x =(abs (mod (+ ?y 2) ?l))))
    (not (exists (obst ?x =(abs (mod (+ ?y 2) ?l)))))

    (test (< ?nivel ?prof))
    =>
    (assert (estado R ?x (mod (abs (mod (+ ?y 1) ?l)) ?l) $?r C ?x (abs (mod (+ ?y 2) ?l)) E $?t nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)

(defrule derecha
    ?f <- (estado R ?x ?y $?r nivel ?nivel)
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ (abs (mod (+ ?x 1) ?w)) ?y) $?r)))
    (not (exists (estado R =(abs (mod (+ ?x 1) ?w)) ?y $?r nivel ?)))
    (not (exists (alma =(abs (mod (+ ?x 1) ?w)) ?y )))
    (not (exists (obst =(abs (mod (+ ?x 1) ?w)) ?y )))

    (test (< ?nivel ?prof))
    =>
    (assert (estado  R (abs (mod (+ ?x 1) ?l)) ?y $?r nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule derecha-con-caja
    (mapa L ?l W ?w)
    ?f <-(estado R ?x ?y $?r C =(abs (mod (+ ?x 1) ?l)) ?y V $?t nivel ?nivel )
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ (abs (mod (+ ?x 2) ?w)) ?y) $?r)))
    (test (not (member$ (create$ (abs (mod (+ ?x 2) ?w)) ?y) $?t)))
    (not (exists (alma =(abs (mod (+ ?x 2) ?w)) ?y )))
    (not (exists (obst =(abs (mod (+ ?x 2) ?w)) ?y)))

    (test (< ?nivel ?prof))
    =>
    (assert (estado R (abs (mod (+ ?x 1) ?l)) ?y  $?r C (abs (mod (+ ?x 2) ?w)) ?y V $?t nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule derecha-caja-en-almacen
    (mapa L ?l W ?w)
    ?f <-(estado R ?x ?y $?r C =(abs (mod (+ ?x 1) ?l)) ?y V $?t nivel ?nivel )

    (profundidad-maxima ?prof)
    (test (not (member$ (create$ (abs (mod (+ ?x 2) ?w)) ?y) $?r)))
    (test (not (member$ (create$ (abs (mod (+ ?x 2) ?w)) ?y) $?t)))
    (exists (alma =(abs (mod (+ ?x 2) ?w)) ?y))
    (not (exists (obst =(abs (mod (+ ?x 2) ?w)) ?y)))

    (test (< ?nivel ?prof))
    =>
    (assert (estado R (abs (mod (+ ?x 1) ?l)) ?y $?r C (abs (mod (+ ?x 2) ?w)) ?y E $?t nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)

(defrule izquierda
    ?f <- (estado R ?x ?y $?r nivel ?nivel)
    (mapa L ?l W ?w)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ (abs (mod (- ?x 1) ?w)) ?y) $?r)))
    (not (exists (estado R =(abs (mod (- ?x 1) ?w)) ?y $?r nivel ?)))
    (not (exists (alma =(abs (mod (- ?x 1) ?w)) ?y )))
    (not (exists (obst =(abs (mod (- ?x 1) ?w)) ?y )))

    (test (< ?nivel ?prof))
    =>
    (assert (estado  R (abs (mod (- ?x 1) ?w)) ?y $?r  nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule izquierda-con-caja
    (mapa L ?l W ?w)
    ?f <-(estado R ?x ?y $?r C =(abs (mod (- ?x 1) ?w)) ?y V $?t nivel ?nivel)
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ (abs (mod (- ?x 2) ?w)) ?y) $?r)))
    (test (not (member$ (create$ (abs (mod (- ?x 2) ?w)) ?y) $?t)))
    (not (exists (alma =(abs (mod (- ?x 2) ?w)) ?y)))
    (not (exists (obst =(abs (mod (- ?x 2) ?w)) ?y)))

    (test (< ?nivel ?prof))
    =>
    (assert (estado R (abs (mod (- ?x 1) ?w)) ?y $?r C (abs (mod (- ?x 2) ?w)) ?y V $?t nivel (+ ?nivel 1)))
    (bind ?*nod-gen* (+ ?*nod-gen* 1))
)
(defrule izquierda-caja-en-almacen
    (mapa L ?l W ?w)
    ?f <-(estado R ?x ?y $?r C =(abs (mod (- ?x 1) ?w)) ?y V $?t nivel ?nivel )
    (profundidad-maxima ?prof)
    (test (not (member$ (create$ (abs (mod (- ?x 2) ?w)) ?y) $?r)))
    (test (not (member$ (create$ (abs (mod (- ?x 2) ?w)) ?y) $?t)))
    (exists (alma =(abs (mod (- ?x 2) ?w)) ?y))
    (not (exists (obst =(abs (mod (- ?x 2) ?w)) ?y)))

    (test (< ?nivel ?prof))
    =>
    (assert (estado R (abs (mod (- ?x 1) ?w)) ?y $?r C (abs (mod (- ?x 2) ?w)) ?y E $?t nivel (+ ?nivel 1)))
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

    (assert (estado R 1 3 C 4 2 V  nivel 0))
    (assert (profundidad-maxima ?prof))
)
