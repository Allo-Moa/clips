(deffacts data

    (lista a b b a a c)
    (lista a b b a b a b c)
    (lista a b b a a a c)

)

(defrule myrule
    (lista ?x $?y a ?x ?z)
    =>
    
    )
