module Statistics.Continuous (uniform, normal, standardnormal, exponential, pdfuniform, pdfnormal, pdfstandardnormal, pdfexponential, cdfuniform, cdfnormal, cdfstandardnormal, cdfexponential, area, bins, interpolate, integrate, slope, tangent, normalize, dec, zscore, mean, variance, stddev, correlate, linreg) where

{-| Functions for calculating values of common continuous probability distributions. 

# Probability distribution constructors
@docs uniform, normal, standardnormal, exponential

# Probability density functions
@docs pdfuniform, pdfnormal, pdfstandardnormal, pdfexponential

# Cumulative density functions 
@docs cdfuniform, cdfnormal, cdfstandardnormal, cdfexponential

# Statistical computation
@docs mean, variance, stddev, linreg

# Helpers
@docs area, interpolate, integrate, slope, tangent, normalize, dec, zscore 

-}

import List


type alias Uniform = { name:String, discrete:Bool, mu:Float, sigma:Float, pdf:(Float -> Float), cdf:(Float), interval:(Float,Float) }

{-| Constructs a new uniform distribution with the given interval. -}
uniform : (number,number) -> Uniform
uniform (from,to) = { name="uniform",  
                      discrete=False,
                      mu=(to-from)/2, 
                      sigma=sqrt ((to - from)^2 / 12),
                      pdf=pdfuniform (from,to),
                      cdf=cdfuniform,
                      interval=(from,to) }


type alias Normal = { name:String, discrete:Bool, mu:Float, sigma:Float, pdf:(Float -> Float), cdf:((Float,Float) -> Float) }

{-| Constructs a new normal distribution with the given mean (mu) and standard deviation (sigma). -}
normal : number -> number -> Normal
normal mu sigma = { name="normal",  
                    discrete=False,
                    mu=mu, 
                    sigma=sigma,
                    pdf=pdfnormal mu sigma,
                    cdf=cdfnormal mu sigma 100
                   }


type alias Standardnormal = { name:String, discrete:Bool, mu:Float, sigma:Float, pdf:(Float -> Float), cdf:((Float,Float) -> Float) }

{-| Constructs a new standard normal distribution with mean (mu) 0 and standard deviation (sigma) 1. -}
standardnormal : Standardnormal
standardnormal = { name="standard normal",  
                   discrete=False,
                   mu=0, 
                   sigma=1,
                   pdf=pdfstandardnormal,
                   cdf=cdfstandardnormal 100
                   }


type alias Exponential = { name:String, discrete:Bool, lambda:Float, pdf:(Float -> Float), cdf:((Float,Float) -> Float) }

{-| Constructs a new exponential distribution with the lambda parameter. -}
exponential : number -> Exponential
exponential lambda = { name="exponential",  
                       discrete=False,
                       lambda=lambda,
                       pdf=pdfexponential lambda,
                       cdf=cdfexponential lambda 100
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
cdfuniform = 1.0
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

{-| Interpolating an interval for integration and plotting. -}
interpolate (from,to) nsteps = 
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
       (dx, steps) = interpolate (from,to) nsteps
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
tangent : Float -> Float -> (Float -> Float) -> { dx:Float, intercept:Float, slope:Float, x:Float }
tangent dx x f =  
   let
       m = dec 3 <| slope dx x f
       b = (f x) - (m * x)
   in
       { slope = m, intercept = b, x = x, dx = dx }


{- Utility functions -}

{-| Rounds a number n to m number of decimals -}
dec : number -> number -> number
dec m n = (toFloat << round <| n * 10^m) / (10^m)

{-| Normalize values to the range of 0,1  -}
normalize : (number,number) -> number -> number
normalize (xmin,xmax) x = if xmin - xmax == 0 then 0.5 else (x - xmin) / (xmax - xmin)

{-| Calculate z score -}
zscore : number -> number -> number -> number
zscore mu sigma x = (x - mu) / sigma



{-| Calculate the mean of a list -}
mean : List Float -> Float
mean xs = (List.sum xs) / (toFloat <| List.length xs)

{-| Calculate the variance of a list -}
variance : List Float -> Float
variance xs =  
   let
       n = List.length xs
       meanx = mean xs
       sqDev = List.sum <| List.map (\x -> (x - meanx)^2) xs
   in
       sqDev / (toFloat n - 1)

{-| Calculate the standard deviation of a list -}
stddev : List Float -> Float
stddev xs = sqrt <| variance xs 

correlate : List (Float,Float) -> Float
correlate pairs = 
   let
       n = List.length pairs
       xs = List.map fst pairs
       ys = List.map snd pairs
       meanx = mean xs
       meany = mean ys
       sdx = stddev xs
       sdy = stddev ys
       xyDeviations = List.sum <| List.map (\(x,y) -> (x - meanx) * (y - meany)) pairs
       r = xyDeviations / ((toFloat n - 1) * sdx * sdy)
   in
       dec 4 r


type alias LinearRegression = {
   a : Float,
   b : Float,
   f : (Float -> Float),
   residuals : List Float
}

--linreg : List (Float,Float) -> (Float -> Float)
linreg : List (Float,Float) -> LinearRegression
linreg pairs =  
   let
       n = List.length pairs
       xs = List.map fst pairs
       ys = List.map snd pairs
       meanx = mean xs
       meany = mean ys
       sdx = stddev xs
       sdy = stddev ys
       --xyDeviations = List.sum <| List.map (\(x,y) -> (x - meanx) * (y - meany)) pairs
       --r = xyDeviations / ((toFloat n - 1) * sdx * sdy)
       r = correlate pairs
       b = r * (sdy / sdx)
       a = meany - (b * meanx) 
       f = (\x -> a + b * x)
       residuals = List.map (\(x,y) -> dec 2 <| y - (f x)) pairs
   in
       { a = dec 4 a, b = dec 4 b, f = f, residuals = residuals }

-- Slope comes out wrong! 
