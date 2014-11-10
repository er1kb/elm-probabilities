-- function graphs for plotting distributions
-- a work in progress

import Discrete as D
import Continuous as C
import Graphics.Collage
import Window

plottype d = case d.discrete of
   True -> "plot discrete"
   False -> "plot continuous"


--data Discrete d = Binom d | Hyper d | Poisson d
--data Continuous d = Uniform d | Normal d | Standardnormal d | Exponential d
--data Distribution d = Discrete d | Continuous d

{-| Calculating trapezium points for plotting them as polygons. -}
trapezium m dx f (x1,x2) =  map (\(x,y) -> (m*x, m*y)) [(x1,0), (x2,0), (x2,f x2), (x1,f x1)]

plotbins multiplier (from,to) nsteps f = 
   let
      (dx,steps) = C.interpolate (from,to) nsteps f
      xs = C.bins steps
      points = map (trapezium multiplier dx f) xs 
   in
      group <| map (\x -> outlined (solid blue) <| polygon x) points

plotcurve m (from,to) nsteps f = 
   let
      (dx,steps) = C.interpolate (from,to) nsteps f
      ys = map (\(x,y) -> (m*x,m*y)) <| zip steps (map f steps)
   in
      --ys
      traced (solid red) <| path ys


main = lift (plotc (300,64) (0,8*pi) (tan << cos << sin)) Window.dimensions 
--zdist = C.normal 3 1
--main = lift (plotc (300,40) (0,6) zdist.f) Window.dimensions 

{- 
TODO: 
make xscale and yscale functions
implement grid + scales
-}

plotc (nsteps,nbins) (from,to) f (w,h) =  
   let
      xmargin = (toFloat w)/20
      ymargin = (toFloat h)/20
      xoffset = (toFloat w)/2 - xmargin
      yoffset = (toFloat h)/2 - ymargin
      interval = (to - from)
      multiplier = (toFloat w) / (interval * 1.2)
      --multiplier = 100 
   in 
      collage w h  
             [move (-xoffset, 0) <| plotcurve multiplier (from,to) nsteps f, 
              move (-xoffset, 0) <| plotbins multiplier (from,to) nbins f]
