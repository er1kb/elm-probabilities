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
         yScale:(Float -> Float),
         yExtent:Float 
      }

plotConfig : (Float -> Float) -> (Float, Float) -> Int -> PlotConfig
plotConfig f (from,to) steps = 
   let
       xDomain = (from, to)
       --yDomain = (-1,1)
       xExtent = (to - from)
       interpolator = List.tail <| List.map (C.normalize (1,toFloat steps)) [1..toFloat steps] 
       xs = List.map (\x -> from + xExtent * x) interpolator
       ys = List.map f xs
       dx = xExtent / (toFloat steps)
       xScale = C.normalize xDomain
       baselinePos = if (List.minimum ys < 0) then List.minimum ys else -0.5 
       baselineNeg = if (List.maximum ys > 0) then List.maximum ys else 0.5 
       yScale = C.normalize (baselinePos, baselineNeg)
       ymin = List.minimum ys
       ymax = List.maximum ys
       yDomain = (ymin, ymax)
       yExtent = ymax - ymin
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
                           yScale = yScale,
                           yExtent = yExtent
                        }


--pc = plotConfig (\x -> logBase e x) (1,2*pi) 100
--pc = plotConfig (\x -> 6 * sin x) (-2*pi,2*pi) 100
pc = plotConfig (\x -> 0.5 * (cos x)) (-2*pi,2*pi) 100
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
      aes = { colour = Color.blue, 
              linestyle = GC.solid
            }
      xscale = cfg.xScale
      yscale = cfg.yScale
      dx = dx' / 2
      xs = C.bins <| List.map (\x -> x - dx) steps
      points = List.map (bar (xm,ym) (xscale,yscale) cfg.f) xs 
   in
      GC.group <| List.map (\x -> GC.outlined (aes.linestyle aes.colour) <| GC.polygon x) points


{-| Calculating trapezium points for plotting them as polygons. -}
trapezoid (xm,ym) (xscale, yscale) f (x1,x2) =  
   List.map (\(x,y) -> (xm * (xscale x), ym * (yscale y))) 
            [(x1,0), (x2,0), (x2,f x2), (x1,f x1)]

geom_trapezoid cfg (xm,ym) (dx, steps) = 
   let
      aes = { colour = Color.blue, 
              linestyle = GC.solid
            }
      xscale = cfg.xScale
      yscale = cfg.yScale
      xs = C.bins steps
      points = List.map (trapezoid (xm,ym) (xscale,yscale) cfg.f) xs 
   in
      GC.group <| List.map (\x -> GC.outlined (aes.linestyle aes.colour) <| GC.polygon x) points

geom_curve cfg (xm,ym) nsteps = 
   let
      aes = { colour = Color.darkGrey, 
               linestyle = GC.solid
            }
      xscale = cfg.xScale
      yscale = cfg.yScale
      (dx,steps) = C.interpolate cfg.xDomain (toFloat nsteps) cfg.f
      ys = List.map (\(x,y) -> (xm * (xscale x),ym * (yscale y))) <| List.map2 (,) steps (List.map cfg.f steps)
   in
      GC.traced (aes.linestyle aes.colour) <| GC.path ys

geom_hline cfg (xm, ym) y =
   let
       aes = { colour = Color.black, 
               linestyle = GC.solid
            }
   in
      GC.traced (aes.linestyle aes.colour)  
      <| GC.path [(0,ym * (cfg.yScale y)), (xm, ym * (cfg.yScale y))] 

geom_vline cfg (xm, ym) x =
   let
       aes = { colour = Color.black, 
               linestyle = GC.solid
            }
   in
      GC.traced (aes.linestyle aes.colour)  
      <| GC.path [(xm * (cfg.xScale x),0), (xm * (cfg.xScale x), ym)] 


geom_abline cfg (xm,ym) x' = 
   let
       aes = { annotationPosition = (80,50), 
               colour = Color.red, 
               dx = 0.033
            }
       xmin = fst cfg.xDomain
       ymin = fst cfg.yDomain
       ymax = snd cfg.yDomain
       --dx = cfg.dx / 4
       dx = aes.dx
       x = xmin + (cfg.xExtent * x')
       tangent = C.tangent dx x cfg.f
       m = tangent.slope
       b = tangent.intercept
       fun = (\x -> m * x + b)
       y = (fun x)
       x1 = xmin + (cfg.xExtent * (x' - dx))
       y1 = fun x1
       x2 = xmin + (cfg.xExtent * (x' + dx))
       y2 = fun x2
       ys = [(xm * (cfg.xScale x1), ym * (cfg.yScale y1)),
             (xm * (cfg.xScale x2), ym * (cfg.yScale y2))]
       annotation = GC.move aes.annotationPosition <| GC.toForm <| Text.leftAligned <| Text.fromString  
            <| "slope: " ++ (toString m) ++ "x\nintercept: " ++ (toString <| C.dec 3 b)
   in
        --GC.move (80,300) <| GC.toForm <| Text.rightAligned <| Text.fromString <| toString <| List.map (\(a,b) -> (C.dec 2 a, C.dec 2 b)) ys
        --GC.move (80,300) <| GC.toForm <| Text.rightAligned <| Text.fromString <| toString <| tangent
        --GC.move (80,300) <| GC.toForm <| Text.rightAligned <| Text.fromString <| toString <| y
        --GC.move (80,280) <| GC.toForm <| Text.rightAligned <| Text.fromString  <| (toString <| (C.dec 2 x1,C.dec 2 x,C.dec 2 x2, C.dec 2 (x2 - x1))) ++ "\n" ++ (toString <| (C.dec 2 y1,C.dec 2y,C.dec 2 y2, C.dec 2 (y2 - y1))) ++ "\n" ++ (toString (C.dec 2 ymin, C.dec 2 ymax))
       GC.group [GC.traced (GC.solid Color.red) <| GC.path ys,  
       GC.move (xm * cfg.xScale x,ym * cfg.yScale (cfg.f x))  
       <| GC.filled aes.colour <| GC.circle 2,  
       annotation]
       
axisY cfg (xm,ym) xmargin = 
   let
       xmin = fst cfg.xDomain
       ymin = fst cfg.yDomain
       ymax = snd cfg.yDomain
       pos = (xm * (cfg.xScale xmin) - (xmargin / 4),0)
       tickPositions = [ymin, 0, ymax]
       tickLabels = GC.group <| List.map (\y -> GC.move (-xmargin / 8,ym * cfg.yScale y)  
         <| GC.toForm <| Text.rightAligned <| Text.fromString <|  
         toString <| C.dec 2 y) tickPositions
       ticks = GC.group <| List.map (\y -> GC.move (-xmargin * 0.02,ym * cfg.yScale y)  
         <| GC.traced (GC.solid Color.black) <| GC.path [(0,0),(12,0)]) tickPositions
       yBar = GC.traced (GC.solid Color.black) <| GC.path [(10,0),(10,ym)]
   in
      GC.move pos <| GC.group  
      [tickLabels, ticks, yBar]

--meh (mx,my) (ww,wh) = Text.asText <| (C.normalize (0,toFloat ww) (toFloat mx))
--main = Signal.map2 meh Mouse.position Window.dimensions

main = Signal.map3 (plotc (1400,400) (\x -> -3 + sin x) (-2*pi,2*pi) pc) (Signal.constant (-2*pi,2*pi)) Mouse.x Window.dimensions 
--main = Signal.map Text.asText Window.dimensions

plotc (plotWidth,plotHeight) f (xmin,xmax) cfg (from,to) mouseX (windowWidth,windowHeight) =  
   let
      maxbins = 400
      windowScale = C.normalize (0,toFloat windowWidth)
      nbins = round <| (toFloat mouseX) / 4
      --xpos = (fst cfg.xDomain) + cfg.xExtent * windowScale (toFloat mouseX)
      wpos = windowScale (toFloat mouseX)
      xpos = (fst cfg.xDomain) + cfg.xExtent * wpos
      ypos = cfg.f <| xpos
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
      yMarker = geom_hline cfg (xmultiplier,ymultiplier) ypos
      tangent = geom_abline cfg (xmultiplier,ymultiplier) wpos
      --yAxis = GC.traced (GC.solid Color.black) <| GC.path [(0,0),(0,ymultiplier)]
      yAxis = axisY cfg (xmultiplier,ymultiplier) xmargin
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
              --GC.move (-xoffset, -yoffset) <| geom_point cfg (xmultiplier, ymultiplier) (dx,steps),
              GC.move (-xoffset, -yoffset) <| yAxis,
              GC.move (-xoffset, -yoffset) <| zeroX,
              GC.move (-xoffset, -yoffset) <| zeroY,
              GC.move (-xoffset, -yoffset) <| xMarker,
              GC.move (-xoffset, -yoffset) <| yMarker,
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
geom_bar: make half bars at each end
implement variable placement of annotation 
figure out what equals 200 and why translating the tangent by this amount is just right
-}

