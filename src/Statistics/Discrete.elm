module Statistics.Discrete (combinations, permutations, binom, hyper, poisson, pdfbinom, pdfhyper, pdfpoisson, cdfbinom, cdfhyper, cdfpoisson, pdftable, cdftable, factorial, fib) where
--
{-| Functions for calculating values of common discrete probability distributions. 

# Combinations and permutations
@docs combinations, permutations

# Probability distributions
@docs binom, hyper, poisson

# Probability density functions
@docs pdfbinom, pdfhyper, pdfpoisson

# Cumulative density functions
@docs cdfbinom, cdfhyper, cdfpoisson

# Helpers
@docs pdftable, cdftable, factorial, fib

-}

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

type alias Binom = { name:String, discrete:Bool, mu:Float, sigma:Float, pdf:(Float -> Float), cdf:(Float -> Float) }

{-| Constructs a new binomial distribution with n tries and prob probability of "success" on each try. -}
binom : number -> number -> Binom
binom n prob = { name="binomial",  
                 discrete=True,
                 mu=n * prob, 
                 sigma=sqrt <| (n * prob) * (1 - prob),
                 pdf=pdfbinom n prob,
                 cdf=cdfbinom n prob }



type alias Hyper = { name:String, discrete:Bool, mu:Float, sigma:Float, pdf:(Float -> Float), cdf:(Float -> Float) }

{-| Constructs a new hypergeometric distribution with n tries and prob probability of "success" on each try. -}
hyper : number -> number -> number -> Hyper
hyper pN pS n = { name="hypergeometric",  
                  discrete=True,
                  mu=n * (pS / pN), 
                  sigma=sqrt <| (n * (pS/pN)) * (1 - (pS/pN)) * ((pN-n)/(pN-1)),
                  pdf=pdfhyper pN pS n,
                  cdf=cdfhyper pN pS n }


type alias Poisson = { name:String, discrete:Bool, mu:Float, sigma:Float, pdf:(Float -> Float), cdf:(Float -> Float) }

{-| Constructs a new Poisson distribution with mean mu. -}
poisson : number -> Poisson
poisson mu = { name="poisson",  
               discrete=True,
               mu=mu, 
               sigma=sqrt mu,
               pdf=pdfpoisson mu,
               cdf=cdfpoisson mu  
             }


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

{-| Cumulative binomial probability for mutually independent events. Returns the total probability of 0 to x. 

    cdfbinom 4 0.5 2 == 0.6875
-}
cdfbinom : number -> number -> number -> number
cdfbinom n prob x = List.sum <| List.map (pdfbinom n prob) [0..x]


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

    cdfhyper 50 25 4 2 == 0.695
-}
cdfhyper : Float -> Float -> Float -> Float -> Float
cdfhyper pN pS n x = List.sum <| List.map (pdfhyper pN pS n) [0..x]


{-| Poisson probability distribution function, for anticipating unlikely events. Given a known average of mu, what is the likelihood of x? For example, if a certain workplace has 2 hard drive failures a week on average, compute the probability of getting 3 failures in a given week. 

    pdfpoisson 2 3 == 0.18
    map (pdfpoisson 2) [0..5] == [0.135, 0.271, 0.271, 0.18, 0.09, 0.036]
-}
pdfpoisson mu x = ((mu^x)*(e^(-mu))) / factorial x

{-| Cumulative poisson probability for unlikely events. Returns a list with the running total of probabilities from 0 to x. The list does not sum to 1 as the number of events is not limited by x. For example, in the above example it is possible to have 6 hard drive failures or more in one week, although probabilities become very small the further you get from the expected value. 

    cdfpoisson 2 2 == 0.677 
-}
cdfpoisson : Float -> Float -> Float
cdfpoisson mu x = List.sum <| List.map (pdfpoisson mu) [0..x]


{- Utility functions -}


{-| List of probabilities for 0 .. x -}
pdftable dist x = List.map (dist.pdf) [0..x]

{-| List of cumulative probabilities for 0 .. x. -}
cdftable dist x = List.tail <| List.scanl (+) 0 <| List.map (dist.cdf) [0..x]


{-| Factorial for a number n is the product of [1..n]. It can be used to calculate combinations and permutations. It is implemented backwards and recursively, starting with n * (n-1), then (n * (n-1)) * (n-2), and so on. -}
factorial : number -> number
factorial n = if n < 1 then 1 else n * factorial (n-1)


{-| Calculates the Fibonacci sequence. Capped at 19 to avoid unintentional lock-ups and because it probably doesn't make sense to visualize more than that. -}
fib : Int -> Float
fib n = if | n > 19 -> (-1)
           | n == 0 -> 0
           | n == 1 -> 1
           | otherwise -> fib (n-1) + fib (n-2)


