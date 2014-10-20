--module Probabilities (combinations, permutations, pdfbinom, pdfhyper, pdfpoission, cdfbinom, cdfhyper, cdfpoisson) where

-- DISCRETE PROBABILITY DISTRIBUTIONS

import Debug
import List
import Maybe

combinations : number -> number -> number
combinations n k = if | k > n -> 0
                      | otherwise -> (factorial n) / ((factorial k) * (factorial (n - k)))

permutations : number -> number -> number
permutations n k = if | k > n -> 0
                      | otherwise -> (factorial n) / factorial (n - k)

{- Probability Density Function for the Binomial distribution -}
pdfbinom : number -> number -> number -> number
pdfbinom n prob x =  
       let nCx = combinations n x  
       in
          nCx * ((prob^x) * (1 - prob)^(n-x)) 

{- Probability Density Function for the Hypergeometric distribution -}
pdfhyper : number -> number -> number -> number -> number
pdfhyper pN pS n x = 
       let scx = combinations pS x  
           nscnx = combinations (pN-pS) (n-x) 
           ncn = combinations pN n 
       in
          (scx * nscnx) / ncn 

{- Probability Density Function for the Poission distribution -}
pdfpoisson mu x = ((mu^x)*(e^(-mu))) / factorial x


{- Cumulative Distribution Function for the Binomial distribution -}
cdfbinom : number -> number -> [number]
cdfbinom n prob = tail <| scanl (+) 0 <| map (pdfbinom n prob) [0..n]


{- Cumulative Distribution Function for the Hypergeometric distribution -}
cdfhyper : number -> number -> number -> [number]
cdfhyper pN pS n = tail <| scanl (+) 0 <| map (pdfhyper pN pS n) [0..n]


{- Cumulative Distribution Function for the Poisson distribution -}
cdfpoisson : number -> number -> [number]
cdfpoisson mu x = tail <| scanl (+) 0 <| map (pdfpoisson mu) [0..x]


{- Utility functions -}

factorial : number -> number
factorial n = if n < 1 then 1 else n * factorial (n-1)

-- Round n to m number of decimals
dec : number -> number -> number
dec m n = (toFloat << round <| n * 10^m) / (10^m)



--main = asText <| combinations 3 2
--main = asText <| permutations 3 2
--main = asText <| map (dec 2 << pdfbinom 3 0.3) [0..3] 
main = asText <| map (dec 2) <| cdfbinom 3 0.3
--main = asText <| map (dec 2 << pdfhyper 20 6 3) [0..3] 
--main = asText <| map (dec 3 << pdfhyper 20 6 3) [0..3] 
--main = asText <| map (dec 3) (cdfhyper 20 6 3) 
--main = asText <| map ((dec 3) << (pdfpoisson 5)) [0..10]
--main = asText <| map (dec 3) (cdfpoisson 5 10)
