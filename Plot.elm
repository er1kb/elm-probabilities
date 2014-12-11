-- function graphs for plotting distributions
-- a work in progress

import Discrete as D
import Continuous as C
import Graphics.Collage as GC
import Signal
import Color
import Text
import Window
import Mouse
import List

plottype d = case d.discrete of
   True -> "plot discrete"
   False -> "plot continuous"


--data Discrete d = Binom d | Hyper d | Poisson d
--data Continuous d = Uniform d | Normal d | Standardnormal d | Exponential d
--data Distribution d = Discrete d | Continuous d

{-| Calculating trapezium points for plotting them as polygons. -}
trapezoid m dx f (x1,x2) =  List.map (\(x,y) -> (m*x, m*y)) [(x1,0), (x2,0), (x2,f x2), (x1,f x1)]

plotbins multiplier (from,to) nsteps f = 
   let
      (dx,steps) = C.interpolate (from,to) nsteps f
      xs = C.bins steps
      points = List.map (trapezoid multiplier dx f) xs 
   in
      GC.group <| List.map (\x -> GC.outlined (GC.solid Color.blue) <| GC.polygon x) points

plotcurve m (from,to) nsteps f = 
   let
      (dx,steps) = C.interpolate (from,to) nsteps f
      ys = List.map (\(x,y) -> (m*x,m*y)) <| List.map2 (,) steps (List.map f steps)
   in
      --ys
      GC.traced (GC.solid Color.red) <| GC.path ys


main = Signal.map3 (plotc (800,800) (\x -> x^2) (0,4)) (Signal.constant (0,2)) Mouse.x Window.dimensions 

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
      GC.collage iw ih  
             [GC.move (-xoffset, -yoffset) <| plotcurve multiplier (from,to) nsteps f, 
              GC.move (-xoffset, -yoffset) <| plotbins multiplier (from,to) (toFloat nbins) f]
              --GC.toForm <| Text.leftAligned <| toText <| "number of bins: " ++ (show nbins),
              --GC.move (0,-30) <| GC.toForm <| Text.leftAligned <| toText <| "Approximate &#x222b;"  
              --       ++ " from " ++ (show <| C.dec 3 from)  
              --       ++ " to " ++ (show <| C.dec 3 to)  
              --       ++ " = " ++ (show <| integral)
              --       ++ " = " ++ (show <| C.dec 3 <| integral)]










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
