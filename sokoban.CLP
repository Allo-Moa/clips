;;sokoban

(deffacts datos
    (mapa V 1 2 L 3 W 3)
)
(defrule arriba
    ?f <-(estado R ?x ?y C ?u ?v A V)
    (mapa V ?x2 ?y2 L ?l W $?w)
    (test (< ?y ?l))
    =>
    (assert (estado R ?x (+ ?y 1) C ?u ?v A V))
)
(defrule izquierda
    ?f <- (estado R ?x ?y C ?u ?v A V)
    (mapa V ?x2 ?y2 L ?l W ?w)
    (test (> ?x 1))
    =>
    (assert (estado R ?x (+ ?x)))
)

(deffunction inicio()
    (reset)

    (printout t "sokoban game")
    ;;(printout t "Profundidad Maxima:= " )
	;;(bind ?prof (read))

    (assert (estado R 3 3 C 2 2 A V))
    ;;(assert (profundidad-maxima ?prof))
)
