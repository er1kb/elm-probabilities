module Probabilities (combinations, permutations, pdfbinom, pdfhyper, pdfpoisson, cdfbinom, cdfhyper, cdfpoisson) where

{-| Functions for calculating values of common discrete probability distributions. 

# Combinations and permutations
@docs combinations, permutations

# Probability density functions
@docs pdfbinom, pdfhyper, pdfpoisson

# Cumulative density functions
@docs cdfbinom, cdfhyper, cdfpoisson

-}

import Debug
import List

{-| The number of ways to combine k out of n items. Order is irrelevant, so each unique set of items appears only once. Assuming two items a and b, (a,b) and (b,a) are one and the same combination. 

   combinations 3 2 == 3
   combinations 4 2 == 6
   map (combinations 4) [1..4] == [4,6,4,1]
-}
combinations : number -> number -> number
combinations n k = if | k > n -> 0
                      | otherwise -> (factorial n) / ((factorial k) * (factorial (n - k)))

{-| The number of possible permutations when picking k out of n items. Each unique group of items can be ordered in different ways, so choosing items a and b will yield two different permutations: (a,b) and (b,a). 

    permutations 3 2 == 6
    permutations 4 2 == 12
    map (permutations 4) [1..4] == [4,12,24,24]
-}
permutations : number -> number -> number
permutations n k = if | k > n -> 0
                      | otherwise -> (factorial n) / factorial (n - k)

{-| Binomial (Bernoulli) probability distribution function, for mutually independent events. 

    pdfbinom 4 0.5 2 == 0.375
    map (pdfbinom 4 0.5) [0..4] == [0.0625, 0.25, 0.375, 0.25, 0.0625]
-}
pdfbinom : number -> number -> number -> number
pdfbinom n prob x =  
       let nCx = combinations n x  
       in
           if | x > n -> 0
              | otherwise -> nCx * ((prob^x) * (1 - prob)^(n-x)) 

{-| Cumulative binomial probability for mutually independent events. Returns a list with the running total of probabilities from 0 to n. 

    cdfbinom 4 0.5 == [0.0625, 0.3125, 0.6875, 0.9375, 1]
-}
cdfbinom : number -> number -> [number]
cdfbinom n prob = tail <| scanl (+) 0 <| map (pdfbinom n prob) [0..n]


{-| Hypergeometric probability distribution function, for mutually dependent events. 

    pdfhyper 50 25 4 2 == 0.391
    map (pdfhyper 50 25 4) [0..4] == [0.055, 0.25, 0.391, 0.25, 0.055]
-}
pdfhyper : number -> number -> number -> number -> number
pdfhyper pN pS n x = 
       let scx = combinations pS x  
           nscnx = combinations (pN-pS) (n-x) 
           ncn = combinations pN n 
       in
          (scx * nscnx) / ncn 

{-| Cumulative hypergeometric probability for mutually dependent events. Returns a list with the running total of probabilities from 0 to n.

    cdfhyper 50 25 4 == [0.055, 0.305, 0.695, 0.945, 1] 
-}
cdfhyper : number -> number -> number -> [number]
cdfhyper pN pS n = tail <| scanl (+) 0 <| map (pdfhyper pN pS n) [0..n]


{-| Poission probability distribution function, for anticipating unlikely events. Given a known average of mu, what is the likelihood of x? For example, if a certain workplace has 2 hard drive failures a week on average, compute the probability of getting 3 failures in a given week. 

    pdfpoisson 2 3 == 0.18
    map (pdfpoisson 2) [0..5] == [0.135, 0.271, 0.271, 0.18, 0.09, 0.036]
-}
pdfpoisson mu x = ((mu^x)*(e^(-mu))) / factorial x

{-| Cumulative poisson probability for unlikely events. Returns a list with the running total of probabilities from 0 to x. The list does not sum to 1 as the number of events is not limited by x. For example, in the above example it is possible to have 6 hard drive failures or more in one week, although probabilities become very small the further you get from the expected value. 

    cdfpoisson 2 5 == [0.135, 0.406, 0.677, 0.857, 0.947, 0.983] 
-}
cdfpoisson : number -> number -> [number]
cdfpoisson mu x = tail <| scanl (+) 0 <| map (pdfpoisson mu) [0..x]


{- Utility functions -}
{-| Factorial for a number n is the product of [1..n]. It can be used to calculate combinations and permutations. It is implemented backwards and recursively, starting with n * (n-1), then (n * (n-1)) * (n-2), and so on. -}
factorial : number -> number
factorial n = if n < 1 then 1 else n * factorial (n-1)

{-| Rounds a number n to m number of decimals -}
dec : number -> number -> number
dec m n = (toFloat << round <| n * 10^m) / (10^m)


{- HARDCORE TESTING ROUTINES BELOW :-) -}

--main = asText <| combinations 10 5
--main = asText <| map (permutations 4) [1..4]
--main = asText <| permutations 4 2
--main = asText <| map (pdfbinom 4 0.5) [0..4]
--main = asText <| (pdfbinom 4 0.5) 2
--main = asText <| map (dec 9 << pdfbinom 4 0.5) [0..5]
--main = asText <| map (dec 2 << pdfbinom 3 0.3) [0..3] 
--main = asText <| cdfbinom 4 0.5
--main = asText <| map (dec 2 << pdfhyper 20 6 3) [0..3] 
--main = asText <| map (dec 3 << pdfhyper 50 25 4) [0..4] 
--main = asText <| map (dec 3 << pdfhyper 20 6 3) [0..3] 
--main = asText <| map (dec 3) (cdfhyper 50 25 4) 
--main = asText <| map ((dec 3) << (pdfpoisson 5)) [0..10]
--main = asText <| map (dec 3) (cdfpoisson 2 5)
--main = asText <| map (dec 3 << pdfpoisson 2) [0..5]
