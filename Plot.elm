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
       xDomain = (from, to)
       yDomain = (-1,1)
       xExtent = (to - from)
       interpolator = List.tail <| List.map (C.normalize (1,toFloat steps)) [1..toFloat steps] 
       xs = List.map (\x -> from + xExtent * x) interpolator
       ys = List.map f xs
       dx = xExtent / (toFloat steps)
       xScale = C.normalize xDomain
       baselinePos = if (List.minimum ys < 0) then List.minimum ys else 0 
       baselineNeg = if (List.maximum ys > 0) then List.maximum ys else 0 
       yScale = C.normalize (baselinePos, baselineNeg)
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


pc = plotConfig (\x -> logBase e x) (1,2*pi) 100
--pc = plotConfig (\x -> e^x) (-2*pi,2*pi) 100
--pc = plotConfig (\x -> C.pdfstandardnormal x) (-2*pi,2*pi) 100
--pc = Signal.map (plotConfig sin (-2*pi,2*pi)) Mouse.x 

point (xm,ym) (xscale, yscale) f x y =  
      (xm * (xscale x), ym * (yscale <| f x)) 

geom_point cfg (xm,ym) (dx, steps) = 
   let
      xscale = cfg.xScale
      yscale = cfg.yScale
      --ys = List.map cfg.f steps -- SLOOOW
      points = List.map2 (point (xm,ym) (xscale,yscale) cfg.f) steps cfg.ys 
   in
      GC.group <| List.map (\(x,y) -> GC.move (x,y) <| GC.filled Color.purple <| GC.circle 2) points

bar (xm,ym) (xscale, yscale) f (x1',x2) =  
   let
       x1 = if (xscale x1' < 0) then x2 else x1'
       x3 = (x1+x2)/2
       midpoint = if (xscale x1' < 0) then 0 else f x3
   in
      List.map (\(x,y) -> (xm * (xscale x), ym * (yscale y))) 
               [(x1,0), (x2,0), (x2,midpoint), (x1,midpoint)]

geom_bar cfg (xm,ym) (dx', steps) = 
   let
      xscale = cfg.xScale
      yscale = cfg.yScale
      dx = dx' / 2
      xs = C.bins <| List.map (\x -> x - dx) steps
      points = List.map (bar (xm,ym) (xscale,yscale) cfg.f) xs 
   in
      GC.group <| List.map (\x -> GC.outlined (GC.solid Color.blue) <| GC.polygon x) points


{-| Calculating trapezium points for plotting them as polygons. -}
trapezoid (xm,ym) (xscale, yscale) f (x1,x2) =  
   List.map (\(x,y) -> (xm * (xscale x), ym * (yscale y))) 
            [(x1,0), (x2,0), (x2,f x2), (x1,f x1)]

geom_trapezoid cfg (xm,ym) (dx, steps) = 
   let
      xscale = cfg.xScale
      yscale = cfg.yScale
      xs = C.bins steps
      points = List.map (trapezoid (xm,ym) (xscale,yscale) cfg.f) xs 
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

geom_hline cfg (xm, ym) y =
      GC.traced (GC.solid Color.black)  
      <| GC.path [(0,ym * (cfg.yScale y)), (xm, ym * (cfg.yScale y))] 

geom_vline cfg (xm, ym) x =
      GC.traced (GC.solid Color.black)  
      <| GC.path [(xm * (cfg.xScale x),0), (xm * (cfg.xScale x), ym)] 


geom_abline cfg (xm,ym) x = 
   let
       tangent = C.tangent cfg.dx x cfg.f
       m = tangent.slope
       b = tangent.intercept
       fun = (\x -> m * x + b)
       x1 = x - 200
       y1 = fun x1
       x2 = x + 200
       y2 = fun x2
       ys = [(200 * x, cfg.yScale y1),(250*x, cfg.yScale y2)]
   in
       GC.move (0,200) <| GC.traced (GC.solid Color.red) <| GC.path ys
       --GC.move (xm * cfg.xScale x,ym * cfg.yScale (cfg.f x)) <| GC.filled Color.red <| GC.circle 2
       

--meh (mx,my) (ww,wh) = Text.asText <| (C.normalize (0,toFloat ww) (toFloat mx))
--main = Signal.map2 meh Mouse.position Window.dimensions

main = Signal.map3 (plotc (1400,400) (\x -> -3 + sin x) (-2*pi,2*pi) pc) (Signal.constant (-2*pi,2*pi)) Mouse.x Window.dimensions 
--main = Signal.map Text.asText Window.dimensions

plotc (plotWidth,plotHeight) f (xmin,xmax) cfg (from,to) nbins' (windowWidth,windowHeight) =  
   let
      maxbins = 400
      windowScale = C.normalize (0,toFloat windowWidth)
      nbins = round <| (toFloat nbins') / 4
      xpos = (fst cfg.xDomain) + cfg.xExtent * windowScale (toFloat nbins')
      xmargin = (toFloat plotWidth) * 0.1
      ymargin = (toFloat plotHeight) * 0.1
      xoffset = (toFloat plotWidth)/2 - xmargin
      yoffset = (toFloat plotHeight)/2 - ymargin
      innerWidth = (toFloat plotWidth) - xmargin
      innerHeight = (toFloat plotHeight) - ymargin
      ymultiplier = (toFloat plotHeight) - (2 * ymargin)
      xmultiplier = (toFloat plotWidth) - (2 * xmargin)
      zero = cfg.yScale 0
      baseline = List.map (\x -> 0) [1..List.length cfg.xs]
      (dx,steps) = C.interpolate cfg.xDomain (toFloat nbins) cfg.f
      integral = C.integrate cfg.xDomain (toFloat nbins) cfg.f
      plotFrame = GC.traced (GC.solid Color.black)  
         <| GC.path [(0,0),(0,ymultiplier), (xmultiplier,ymultiplier),(xmultiplier,0),(0,0)]
      zeroX = geom_hline cfg (xmultiplier,ymultiplier) 0
      zeroY = geom_vline cfg (xmultiplier,ymultiplier) 0
      xMarker = geom_vline cfg (xmultiplier,ymultiplier) xpos
      tangent = geom_abline cfg (xmultiplier,ymultiplier) xpos
      yAxis = GC.traced (GC.solid Color.black) <| GC.path [(0,0),(0,ymultiplier)]
      --xAxis = GC.traced (GC.solid Color.black) <| GC.path [(0,0),(xmultiplier,0)]
   in 
      GC.collage plotWidth plotHeight  
             [
                GC.filled Color.grey  
                  <| GC.rect innerWidth innerHeight,
                GC.filled Color.lightGrey  
                  <| GC.rect xmultiplier ymultiplier,
                --(GC.filled Color.grey (GC.ngon 8 ((toFloat plotHeight)/2))),
                GC.move (-xoffset, -yoffset) <| geom_curve cfg (xmultiplier,ymultiplier) cfg.steps, 
              --GC.move (-xoffset, -yoffset) <| geom_trapezoid cfg (xmultiplier, ymultiplier) (dx,steps),
              GC.move (-xoffset, -yoffset) <| geom_bar cfg (xmultiplier, ymultiplier) (dx,steps),
              GC.move (-xoffset, -yoffset) <| geom_point cfg (xmultiplier, ymultiplier) (dx,steps),
              GC.move (-xoffset - (xmargin / 4), -yoffset) <| yAxis,
              GC.move (-xoffset, -yoffset) <| zeroX,
              GC.move (-xoffset, -yoffset) <| zeroY,
              GC.move (-xoffset, -yoffset) <| xMarker,
              GC.move (-xoffset, -yoffset) <| tangent,
              --GC.move (-xoffset, -yoffset) <| xAxis,
              GC.move (-xoffset, -yoffset) <| plotFrame,
              GC.move (-innerWidth/10,innerHeight/4) <| GC.toForm <| Text.rightAligned <| Text.fromString  
              <| "number of bins: " ++ (toString nbins) 
              ++ "\nApproximate &#x222b;"  
                     ++ " from " ++ (toString <| C.dec 3 from)  
                     ++ " to " ++ (toString <| C.dec 3 to)  
                     ++ "\n = " ++ (toString <| integral)
                     ++ " = " ++ (toString <| C.dec 3 <| integral)]


              --GC.move (-xoffset, -yoffset - 10.0) <| geom_rug "x" xmultiplier xscale steps,
              --GC.move (-xoffset - 10, -yoffset) <| geom_rug "y" xmultiplier xscale steps]


{- 
TODO: 
implement grid + scales
implement theme object to hold color, linestyle, etc.
-}

