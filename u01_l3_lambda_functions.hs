q32 x = (\x -> x*2) x*2

overwrite x = let x = 2
              in
                let x = 3
                in 
                    let x = 4
                    in
                        x


-- la funzione esterna dà 3 come argomento della funzione interna
-- la funzione interna è la funzione identità con argomento 4
-- l'argomento della funzione esterna non viene quindi considerato
lambdaoverwrite x = (\x -> (\x -> x) 4) 3


-- lexical scope examples
x = 2
add1 y = y + x
add2 y = (\x -> y + x) 3
add3 y = (\y -> (\x -> y + x) 1 ) y


counter x = (\y -> (\z -> z) y + 1) x + 1
