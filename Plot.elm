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


--geom_rug direction multiplier scaleVar positions = 
--   let
--       rugs = List.map (\x -> GC.move (if direction == "x" then (x*multiplier,0) else (0,x*multiplier)) <| GC.filled Color.black <| GC.rect 2.0 2.0) positions
--   in
--       GC.group rugs



{-| Calculating trapezium points for plotting them as polygons. -}
trapezoid (xm,ym) (xscale, yscale) dx f (x1,x2) =  
   List.map (\(x,y) -> (xm * (xscale x), ym * (yscale y))) 
            [(x1,1), (x2,1), (x2,f x2), (x1,f x1)]

plotbins (xm,ym) (dx, steps) f (xscale, yscale) = 
   let
      --(dx,steps) = C.interpolate (from,to) nsteps f
      xs = C.bins steps
      points = List.map (trapezoid (xm,ym) (xscale,yscale) dx f) xs 
   in
      GC.group <| List.map (\x -> GC.outlined (GC.solid Color.blue) <| GC.polygon x) points

plotcurve (xm,ym) (from,to) nsteps f (xscale, yscale) = 
   let
      (dx,steps) = C.interpolate (from,to) nsteps f
      ys = List.map (\(x,y) -> (xm * (xscale x),ym * (yscale y))) <| List.map2 (,) steps (List.map f steps)
   in
      --ys
      GC.traced (GC.solid Color.red) <| GC.path ys


main = Signal.map3 (plotc (1400,400) (\x -> 2 + (cos x)) (-2*pi,2*pi)) (Signal.constant (-2*pi,2*pi)) Mouse.x Window.dimensions 
--main = Signal.map Text.asText Window.dimensions

plotc (plotWidth,plotHeight) f (xmin,xmax) (from,to) nbins (windowWidth,windowHeight) =  
   let
      xmargin = (toFloat plotWidth)/10
      ymargin = (toFloat plotHeight)/10
      xoffset = (toFloat plotWidth)/2 - xmargin
      yoffset = (toFloat plotHeight)/2 - ymargin
      xscale = C.normalize (xmin,xmax)
      interval = (to - from)
      interp = List.tail <| List.map (C.normalize (1,100)) [1..100] 
      xs = List.map (\x -> from + interval * x) interp
      ys = List.map f xs
      yscale = C.normalize (List.minimum ys, List.maximum ys)
      nsteps = 300
      ymultiplier = (toFloat plotHeight) - (2 * ymargin)
      xmultiplier = (toFloat plotWidth) - (2 * xmargin)
      (dx,steps) = C.interpolate (from,to) (toFloat nbins) f
      integral = C.integrate (from,to) (toFloat nbins) f
      plotFrame = GC.traced (GC.solid Color.black)  
         <| GC.path [(0,0),(0,ymultiplier), (xmultiplier,ymultiplier),(xmultiplier,0),(0,0)]
      zeroLine = GC.traced (GC.solid Color.black)  
      <| GC.path [(0,ymultiplier * 0.5), (xmultiplier, ymultiplier * 0.5)] 
      --yAxis = GC.traced (GC.solid Color.black) <| GC.path [(0,0),(0,ymultiplier)]
      --xAxis = GC.traced (GC.solid Color.black) <| GC.path [(0,0),(xmultiplier,0)]
   in 
      GC.collage plotWidth plotHeight  
             [(GC.filled Color.grey (GC.ngon 8 ((toFloat plotHeight)/2))),
                GC.move (-xoffset, -yoffset) <| plotcurve (xmultiplier,ymultiplier) (from,to) nsteps f (xscale,yscale), 
              GC.move (-xoffset, -yoffset) <| plotbins (xmultiplier, ymultiplier) (dx,steps) f (xscale,yscale),
              --GC.move (-xoffset, -yoffset) <| yAxis,
              --GC.move (-xoffset, -yoffset) <| xAxis,
              GC.move (-xoffset, -yoffset) <| zeroLine,
              GC.move (-xoffset, -yoffset) <| plotFrame,
              GC.toForm <| Text.leftAligned <| Text.fromString <| "number of bins: " ++ (toString nbins),
              GC.move (0,-30) <| GC.toForm <| Text.leftAligned <| Text.fromString <| "Approximate &#x222b;"  
                     ++ " from " ++ (toString <| C.dec 3 from)  
                     ++ " to " ++ (toString <| C.dec 3 to)  
                     ++ " = " ++ (toString <| integral)
                     ++ " = " ++ (toString <| C.dec 3 <| integral)]


              --GC.move (-xoffset, -yoffset - 10.0) <| geom_rug "x" xmultiplier xscale steps,
              --GC.move (-xoffset - 10, -yoffset) <| geom_rug "y" xmultiplier xscale steps]

{- 
TODO: 
make everything scale properly
implement grid + scales
-}

