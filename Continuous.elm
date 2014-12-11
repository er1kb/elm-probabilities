module Continuous (uniform, normal, standardnormal, exponential, pdfuniform, pdfnormal, pdfstandardnormal, pdfexponential, cdfuniform, cdfnormal, cdfstandardnormal, cdfexponential, area, bins, interpolate, integrate, slope, tangent, normalize, dec) where

{-| Functions for calculating values of common continuous probability distributions. 

# Distribution constructors
@docs uniform, normal, standardnormal, exponential

# Probability density functions
@docs pdfuniform, pdfnormal, pdfstandardnormal, pdfexponential

# Cumulative density functions 
@docs cdfuniform, cdfnormal, cdfstandardnormal, cdfexponential

-}

import Debug
import List


type alias Uniform = { name:String, discrete:Bool, mu:Float, sigma:Float, f:(Float -> Float), interval:(Float,Float) }

{-| Constructs a new uniform distribution with the given interval. -}
uniform : (number,number) -> Uniform
uniform (from,to) = { name="uniform",  
                      discrete=False,
                      mu=(to-from)/2, 
                      sigma=sqrt ((to - from)^2 / 12),
                      f=pdfuniform (from,to),
                      interval=(from,to) }


type alias Normal = { name:String, discrete:Bool, mu:Float, sigma:Float, f:(Float -> Float), p:((Float,Float) -> Float) }

{-| Constructs a new normal distribution with the given mean (mu) and standard deviation (sigma). -}
normal : number -> number -> Normal
normal mu sigma = { name="normal",  
                    discrete=False,
                    mu=mu, 
                    sigma=sigma,
                    f=pdfnormal mu sigma,
                    p=cdfnormal mu sigma 100
                   }


type alias Standardnormal = { name:String, discrete:Bool, mu:Float, sigma:Float, f:(Float -> Float), p:((Float,Float) -> Float) }

{-| Constructs a new standard normal distribution with mean (mu) 0 and standard deviation (sigma) 1. -}
standardnormal : Standardnormal
standardnormal = { name="standard normal",  
                   discrete=False,
                   mu=0, 
                   sigma=1,
                   f=pdfstandardnormal,
                   p=cdfstandardnormal 100
                   }


type alias Exponential = { name:String, discrete:Bool, lambda:Float, f:(Float -> Float), p:((Float,Float) -> Float) }

{-| Constructs a new exponential distribution with the lambda parameter. -}
exponential : number -> Exponential
exponential lambda = { name="exponential",  
                       discrete=False,
                       lambda=lambda,
                       f=pdfexponential lambda,
                       p=cdfexponential lambda 100
                       }


{-| Probability density function for a normal distribution with mean mu and standard deviation sigma. This function computes the height of the curve at a given x. -}
pdfuniform : (number,number) -> number -> number
pdfuniform (from,to) x = 
   let 
      a = from
      b = to
      domain = b - a
   in
      if x >= 0 && a <= x && x <= b then (1 / domain) else 0



{-| Probability density function for a normal distribution with mean mu and standard deviation sigma. This function computes the height of the curve at a given x. -}
pdfnormal : number -> number -> number -> number
pdfnormal mu sigma x = 
      let top = 1 / (sigma * sqrt (2 * pi))
          exponent = (-0.5) * ((x - mu) / sigma)^2
      in
          top * e^exponent


{-| Probability density function for a standardized normal distribution, with mean 0 and standard deviation 1. -}
pdfstandardnormal : number -> number
pdfstandardnormal x = pdfnormal 0 1 x



{-| Probability density function for an exponential distribution. -}
pdfexponential : number -> number -> number
pdfexponential lambda x = 
   let 
       pdf = lambda * e^(-lambda * x)
   in
      if x >= 0 then pdf else 0


{-| Cumulative density function for a normal distribution. Returns an approximate one-sided integral of the interval [from, to] using the trapezium rule. More steps/breaks will yield a higher precision. 

    cdfnormal 0 1 10 (-1,1) == 0.681
    map (dec 3 << (\x -> cdfnormal 0 1 x (-1,1))) [2,4,6,10,100] == [0.641,0.673,0.678,0.681,0.683]
    map (dec 3 << (\x -> cdfnormal 0 1 100 (-x,x))) [1,2,3] == [0.683,0.954,0.997]
-}

{-| Cumulative density function for a uniform distribution with mean mu and standard deviation sigma. This function computes the height of the curve at a given x. -}
cdfuniform : number
cdfuniform = 1
--TODO


cdfnormal : number -> number -> number -> (number,number) -> number
cdfnormal mu sigma nsteps (from,to) = 
   integrate (from,to) nsteps (pdfnormal mu sigma)


{-| Cumulative density function for a standardized normal distribution.  

   cdfstandardnormal 10 (-1,1) == 0.683   -- ≈68 % within ±1 standard deviation around the mean
-}
cdfstandardnormal :  number -> (number,number) -> number
cdfstandardnormal nsteps (from,to) = integrate (from,to) nsteps (pdfnormal 0 1)


cdfexponential : number -> number -> (number,number) -> number
cdfexponential lambda nsteps (from,to) = 
   let from2 = if from >= 0 then from else 0
   in integrate (from2,to) nsteps (pdfexponential lambda)



{-| Approximating an integral with a trapezium/trapezoid shape. -}
area : number -> (number,number) -> number
area dx (y1,y2) = dx * (y1 + y2) / 2

{-| Splitting a list of x values into adjacent bins, as in a histogram. A list [a,b,c,d] becomes [(a,b),(b,c),(c,d)] -}
bins : List number -> List (number,number)
bins ys = List.map2 (,) (List.take ((List.length ys)-1) ys) (List.tail ys)

{-| Interpolate an interval for integration and plotting. -}
interpolate (from,to) nsteps f = 
   let
       dxrange = (to - from) -- length of the interval
       dx = dxrange / (nsteps) -- size of chunks (trapezia) to calculate individually
       interpolator = List.map (\x -> (x / nsteps)) [0..nsteps]
   in
       (dx, List.map (\x -> from + x * dxrange) interpolator)

{-| Integrating a function f over an interval in a given number of steps. -}
integrate : (number,number) -> number -> (number -> number) -> number
integrate (from,to) nsteps f =  
   let  
       --dxrange = (to - from) -- length of the interval
       --dx = dxrange / (nsteps) -- size of chunks (trapezia) to calculate individually
       --interpolator = map (\x -> (x / nsteps)) [0..nsteps]
       --steps = map (\x -> from + x * dxrange) interpolator
       (dx, steps) = interpolate (from,to) nsteps f
       ys = List.map f steps
       trapezia = bins ys
   in
       List.sum <| List.map (area dx) trapezia


{-| Finding the approximate derivative of a function at a given point. -}
slope : number -> number -> (number -> number) -> number
slope dx x f =
   let
       offset = dx / 2
       x1 = x - offset
       x2 = x + offset 
       y1 = f x1
       y2 = f x2
   in
       (y2 - y1) / dx

{-| Finding the tangent (derivative) of a function at a given point.  
Returns a record containing slope and intercept. -}
--tangent : ...
tangent dx x f =  
   let
       m = dec 3 <| slope dx x f
       b = (f x) - (m * x)
   in
       { slope = m, intercept = b, x = x, dx = dx }


{- Utility functions -}
{-| Factorial for a number n is the product of [1..n]. It can be used to calculate combinations and permutations. It is implemented backwards and recursively, starting with n * (n-1), then (n * (n-1)) * (n-2), and so on. -}
factorial : number -> number
factorial n = if n < 1 then 1 else n * factorial (n-1)

{-| Rounds a number n to m number of decimals -}
dec : number -> number -> number
dec m n = (toFloat << round <| n * 10^m) / (10^m)

{-| Normalize values to the range of 0,1  -}
normalize : (number,number) -> number -> number
normalize (xmin,xmax) x = (x - xmin) / (xmax - xmin)




{- HARDCORE TESTING ROUTINES BELOW :-) -}

--main = asText <| pdfnormal 0 1 0
--main = asText <| zip [1,2,3] <| map (dec 3 << cdfnormal 0 1 100) [(-1,1),(-2,2),(-3,3)]
--main = asText <| map (dec 3 << (\x -> cdfnormal 0 1 100 (-x,x))) [1,2,3]
--main = asText <| map (dec 3 << (\x -> cdfnormal 0 1 x (-1,1))) [2,4,6,10,100]
--main = asText <| chunks [1,2,3] 
--main = asText <| map (\(a,b) -> (dec 3 a, dec 3 b)) <| integrate (-1,1) 10 (pdfnormal 0 1)
--main = asText <| integrate (-1,1) 10 (pdfnormal 0 1)
--main = asText <| cdfnormal 0 1 10 (-1,1)
--main = asText <| zip [1,2,3] <| map (dec 3 << cdfnormal 0 1 100) [(-1,1),(-2,2),(-3,3)]
--main = asText <| cdfexponential 0.5 10 (0,2) 
--main = asText <| cdfstandardnormal 100 (-1,1)
--main = asText <| cdfexponential (1/20) 100 (0,5)
--main = asText <| 1 - cdfexponential (1/20) 100 (0,40)
--main = asText <| integrate (25,30) 100 <| pdfuniform (0,30)
--main = asText <| normalize (0,8) 4
--main = asText <| cdfuniform
--main = asText <| dec 3 <| integrate (0,1) 100 (\x -> e^x)
--main = asText <| dec 3 <| integrate (1,pi) 100 (\x -> 1/x)
--main = asText <| slope 0.01 2 (\x -> 1/x)
--main = asText <| tangent 0.01 2 (\x -> 1/x)
--meh = normal 100 10
--main = asText <| (dec 3 << meh.p) (90,110)
--main = asText <| meh.f 80
--meh = standardnormal
--main = asText <| (dec 3 << meh.p) (-1,1)
--meh = exponential 0.5 
--main = asText <| (dec 3 << meh.p) (0,3)

--integrating a volume of revolution around the x axis -->
--main = asText <| dec 2 <| integrate (1,3) 100 (\x -> pi * (1 / sqrt x)^2)

--main = asText <| dec 2 <| integrate (-pi, 0) 100 sin
