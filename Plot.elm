-- function graphs for plotting distributions
-- a work in progress

import Discrete as D
import Continuous as C
import Graphics.Collage
import Window
import Mouse

plottype d = case d.discrete of
   True -> "plot discrete"
   False -> "plot continuous"


--data Discrete d = Binom d | Hyper d | Poisson d
--data Continuous d = Uniform d | Normal d | Standardnormal d | Exponential d
--data Distribution d = Discrete d | Continuous d

{-| Calculating trapezium points for plotting them as polygons. -}
trapezoid m dx f (x1,x2) =  map (\(x,y) -> (m*x, m*y)) [(x1,0), (x2,0), (x2,f x2), (x1,f x1)]

plotbins multiplier (from,to) nsteps f = 
   let
      (dx,steps) = C.interpolate (from,to) nsteps f
      xs = C.bins steps
      points = map (trapezoid multiplier dx f) xs 
   in
      group <| map (\x -> outlined (solid blue) <| polygon x) points

plotcurve m (from,to) nsteps f = 
   let
      (dx,steps) = C.interpolate (from,to) nsteps f
      ys = map (\(x,y) -> (m*x,m*y)) <| zip steps (map f steps)
   in
      --ys
      traced (solid red) <| path ys


main = lift3 (plotc (800,800) (\x -> x^2) (0,4)) (constant (0,2)) Mouse.x Window.dimensions 

plotc (iw,ih) f (fmin,fmax) (from,to) nbins (ow,oh) =  
   let
      xmargin = (toFloat iw)/20
      ymargin = (toFloat ih)/20
      xoffset = (toFloat iw)/2 - xmargin
      yoffset = (toFloat ih)/2 - ymargin
      xscale = (toFloat iw) - xoffset
      yscale = fmax / ((toFloat ih) - yoffset)
      interval = (to - from)
      --multiplier = (toFloat iw) / (interval * 1.2)
      nsteps = 300
      multiplier = 200 
      integral = C.integrate (from,to) (toFloat nbins) f
   in 
      collage iw ih  
             [move (-xoffset, -yoffset) <| plotcurve multiplier (from,to) nsteps f, 
              move (-xoffset, -yoffset) <| plotbins multiplier (from,to) (toFloat nbins) f,
              toForm <| leftAligned <| toText <| "number of bins: " ++ (show nbins),
              move (0,-30) <| toForm <| leftAligned <| toText <| "Approximate &#x222b;"  
                     ++ " from " ++ (show <| C.dec 3 from)  
                     ++ " to " ++ (show <| C.dec 3 to)  
                     ++ " = " ++ (show <| integral)
                     ++ " = " ++ (show <| C.dec 3 <| integral)]










--main = lift3 (plotc zdist.f) (constant (0,6)) Mouse.x Window.dimensions 

{- 
TODO: 
make xscale and yscale functions
implement grid + scales
-}

--main = lift (plotc (300,64) (0,8*pi) (tan << cos << sin)) Window.dimensions 
--zdist = C.normal 3 1
--main = lift (plotc (300,40) (0,6) zdist.f) Window.dimensions 

--main = lift3 (plotc (tan << cos << sin)) (constant (0,4*pi)) Mouse.x Window.dimensions 
