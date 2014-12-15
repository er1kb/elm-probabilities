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

type alias PlotConfig = {
         f: (Float -> Float),
         xs: List Float,
         ys: List Float, 
         xDomain: (Float,Float), 
         yDomain: (Float,Float),
         xExtent: Float,
         steps:Int,
         dx:Float,
         interpolator:List Float,
         xScale:(Float -> Float),
         yScale:(Float -> Float)
      }

plotConfig : (Float -> Float) -> (Float, Float) -> Int -> PlotConfig
plotConfig f (from,to) steps = 
   let
       xDomain = (to, from)
       yDomain = (-1,1)
       xExtent = (to - from)
       interpolator = List.tail <| List.map (C.normalize (1,toFloat steps)) [1..toFloat steps] 
       xs = List.map (\x -> from + xExtent * x) interpolator
       ys = List.map f xs
       dx = xExtent / (toFloat steps)
       xScale = C.normalize xDomain
       yScale = C.normalize (List.minimum ys, List.maximum ys)
   in
                        {  
                           f = f, 
                           xs = xs,
                           ys = ys,
                           xDomain = xDomain, 
                           yDomain = yDomain,
                           xExtent = xExtent,
                           steps = steps,
                           dx = dx,
                           interpolator = interpolator,
                           xScale = xScale,
                           yScale = yScale
                        }


pc = plotConfig (\x -> 0 + sin x) (-2*pi,2*pi) 100
--pc = Signal.map (plotConfig sin (-2*pi,2*pi)) Mouse.x 

{-| Calculating trapezium points for plotting them as polygons. -}
trapezoid (xm,ym) (xscale, yscale) dx f (x1,x2) =  
   List.map (\(x,y) -> (xm * (xscale x), ym * (yscale y))) 
            [(x1,0), (x2,0), (x2,f x2), (x1,f x1)]

geom_trapezoid cfg (xm,ym) (dx, steps) = 
   let
      --(dx,steps) = C.interpolate (from,to) nsteps f
      xscale = cfg.xScale
      yscale = cfg.yScale
      xs = C.bins steps
      points = List.map (trapezoid (xm,ym) (xscale,yscale) dx cfg.f) xs 
   in
      GC.group <| List.map (\x -> GC.outlined (GC.solid Color.blue) <| GC.polygon x) points

geom_curve cfg (xm,ym) nsteps = 
   let
      xscale = cfg.xScale
      yscale = cfg.yScale
      (dx,steps) = C.interpolate cfg.xDomain (toFloat nsteps) cfg.f
      ys = List.map (\(x,y) -> (xm * (xscale x),ym * (yscale y))) <| List.map2 (,) steps (List.map cfg.f steps)
   in
      GC.traced (GC.solid Color.red) <| GC.path ys

--main = Text.asText pc

main = Signal.map3 (plotc (1400,400) (\x -> 0 + sin x) (-2*pi,2*pi) pc) (Signal.constant (-2*pi,2*pi)) Mouse.x Window.dimensions 
--main = Signal.map Text.asText Window.dimensions

plotc (plotWidth,plotHeight) f (xmin,xmax) cfg (from,to) nbins (windowWidth,windowHeight) =  
   let
      xmargin = (toFloat plotWidth) * 0.1
      ymargin = (toFloat plotHeight) * 0.1
      xoffset = (toFloat plotWidth)/2 - xmargin
      yoffset = (toFloat plotHeight)/2 - ymargin
      ymultiplier = (toFloat plotHeight) - (2 * ymargin)
      xmultiplier = (toFloat plotWidth) - (2 * xmargin)
      baseline = List.map (\x -> 0) [1..List.length cfg.xs]
      (dx,steps) = C.interpolate cfg.xDomain (toFloat nbins) cfg.f
      integral = C.integrate cfg.xDomain (toFloat nbins) cfg.f
      plotFrame = GC.traced (GC.solid Color.black)  
         <| GC.path [(0,0),(0,ymultiplier), (xmultiplier,ymultiplier),(xmultiplier,0),(0,0)]
      zeroLine = GC.traced (GC.solid Color.black)  
      <| GC.path [(0,ymultiplier * 0.5), (xmultiplier, ymultiplier * 0.5)] 
      --yAxis = GC.traced (GC.solid Color.black) <| GC.path [(0,0),(0,ymultiplier)]
      --xAxis = GC.traced (GC.solid Color.black) <| GC.path [(0,0),(xmultiplier,0)]
   in 
      GC.collage plotWidth plotHeight  
             [
                (GC.filled Color.grey (GC.ngon 8 ((toFloat plotHeight)/2))),
                --GC.move (-xoffset, -yoffset) <| geom_curve cfg (xmultiplier,ymultiplier) cfg.steps, 
              GC.move (-xoffset, -yoffset) <| geom_trapezoid cfg (xmultiplier, ymultiplier) (dx,steps),
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


      --xscale = C.normalize (xmin,xmax)
      --nsteps = toFloat cfg.steps
      --interval = (to - from)
      --interp = List.tail <| List.map (C.normalize (1,100)) [1..100] 
      --xs = List.map (\x -> from + interval * x) interp
      --ys = List.map f xs
      --yscale = C.normalize (List.minimum ys, List.maximum ys)



{- 
TODO: 
make everything scale properly
implement grid + scales
-}

