(deffacts data
    (puzzle W E B B W ))

(defrule final
?f1 <- (puzzle $W W E B $B)
=>
(halt))

(defrule empty-1-left
?f1 <- (puzzle $?x ?y E $?z)
=>
(assert (puzzle $?x E ?y $?z)))

(defrule empty-1-right
?f1 <- (puzzle $?x E ?y $?z)
=>
(assert (puzzle $?x ?y E $?z)))

(defrule empty-2-left
?f1 <- (puzzle $?x ?y1 ?y2 E $?z)
=>
(assert (puzzle $?x E ?y2 ?y1 $?z)))

(defrule empty-2-right
?f1 <- (puzzle $?x E ?y1 ?y2 $?z)
=>
(assert (puzzle $?x ?y2 ?y1 E $?z)))
