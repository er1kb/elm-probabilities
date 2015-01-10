-- function graphs for plotting distributions
-- a work in progress

import Discrete as D
import Continuous as C
import Graphics.Collage as GC
import Graphics.Element (Element, empty, image, flow, down, right, layers)
import Signal
import Color (..)
import Text
import Window
import Mouse
import List

--plottype d = case d.discrete of
--   True -> "plot discrete"
--   False -> "plot continuous"


--data Discrete d = Binom d | Hyper d | Poisson d
--data Continuous d = Uniform d | Normal d | Standardnormal d | Exponential d
--data Distribution d = Discrete d | Continuous d


type alias Distribution = {
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
         yExtent:Float, 
         plotLimits:(Float,Float)
      }

distribution : (Float -> Float) -> (Float, Float) -> Int -> Distribution
distribution f (from,to) steps = 
   let
       xDomain = (from, to)
       --yDomain = (-1,1)
       xExtent = (to - from)
       --interpolator = List.tail <| List.map (C.normalize (1,toFloat steps)) [1..toFloat steps] 
       interpolator = List.map (C.normalize (0,toFloat steps)) [0..toFloat steps] 
       xs = List.map (\x -> from + xExtent * x) interpolator
       ys = List.map f xs
       dx = xExtent / (toFloat steps)
       xScale = C.normalize xDomain
       ymin = List.minimum ys
       ymax = List.maximum ys
       --yminlog = logBase e (abs ymin)
       --ymaxlog = logBase e (abs ymax)
       baselinePos = if (ymin < 0) then List.minimum ys else -(ymax / 10)
       baselineNeg = if (ymax > 0) then List.maximum ys else abs (ymin / 10) 
       plotLimits = (baselinePos, baselineNeg)
       yScale = C.normalize plotLimits
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
                           yExtent = yExtent,
                           plotLimits = plotLimits
                        }

const = 4
nsteps = 400
--pc = distribution (\x -> logBase e x) (1,2*pi) 100
--pc = distribution (\x -> -6 + 3 * sin x) (-1*pi,2*pi) 100
pc = distribution (\x -> 1 * (cos (const*x))) (0,2*pi) nsteps
pc2 = distribution (\x -> -1 * (sin (const*x))) (0,2*pi) nsteps
pc3 = distribution (\x -> 1 * (sin (const*x))) (0,2*pi) nsteps
pc4 = distribution (\x -> -1 * (cos (const*x))) (0,2*pi) nsteps
--pc5 = distribution (\x -> 3 * (cos (2*x))) (0,2*pi) 100

--pc5 = distribution (\x -> 3 * (cos (2.01*x))) (-12*pi,12*pi) 100
--pc5 = distribution (\x -> 3 * (cos (2*x))) (0,2*pi) 100
--pc5 = distribution (\x -> 3 * (cos (2*x))) (-pi,pi) 100
--pc5 = distribution (\x -> 3 * (sin (2*x))) (-pi,pi) 100

--pc = distribution (\x -> 0.1 * x^2) (-3*pi,2*pi) 100
--pc = distribution (\x -> e^x) (-2*pi,2*pi) 100
--pc = distribution (\x -> C.pdfstandardnormal x) (-4,4) 100
--pc = Signal.map (distribution sin (-2*pi,2*pi)) Mouse.x 


geom_point aes defaults d dims = 
   let
      --(dx,steps) = C.interpolate d.xDomain (toFloat dims.nbins)
      xm = dims.xm
      ym = dims.ym
      colour = lookup .colour aes defaults
      pointsize = lookup .pointsize aes defaults
      visibility = lookup .visibility aes defaults
      x = lookup .x aes defaults
      y = d.f x
      pos = point (xm,ym) (d.xScale, d.yScale) d.f x y
   in
      GC.move pos <| GC.alpha visibility <| GC.filled colour <| GC.circle pointsize



point (xm,ym) (xscale, yscale) f x y =  
      (xm * (xscale x), ym * (yscale <| f x)) 

geom_points aes defaults d dims = 
   let
      (dx,steps) = C.interpolate d.xDomain (toFloat dims.nbins)
      xm = dims.xm
      ym = dims.ym
      colour = lookup .colour aes defaults
      pointsize = lookup .pointsize aes defaults
      visibility = lookup .visibility aes defaults
      xscale = d.xScale
      yscale = d.yScale
      --ys = List.map d.f steps -- SLOOOW, need to limit the number of points
      points = List.map2 (point (xm,ym) (xscale,yscale) d.f) steps d.ys 
   in
      GC.alpha visibility <| GC.group <| List.map (\(x,y) -> GC.move (x,y) <| GC.filled colour <| GC.circle pointsize) points

bar (xm,ym) (xscale, yscale) limits f (x1',x2') =  
   let
       lower = fst limits
       upper = snd limits
       x1 = if (x1' < lower) then lower else x1'
       x2 = if (x2' > upper) then upper else x2'
       --x1 = if (xscale x1' < 0) then x2 else x1'
       --x2 = if (xscale x2' > 1) then x1 else x2'
       x3 = (x1'+x2')/2
       --x3 = (x1+x2)/2
       --midpoint = if (xscale x1' < 0) then 0 else f x3
       midpoint = f x3
   in
      --List.map (\(x,y) -> (xm * (xscale x), ym * (yscale y))) 
      --         [(x1,0), (x2,0), (x2,midpoint), (x1,midpoint)]
      List.map (\(x,y) -> (xm * (xscale x), ym * (yscale y))) 
               [(x1,0), (x2,0), (x2,midpoint), (x1,midpoint)]

geom_bar aes defaults d dims = 
   let
      --(dx',steps) = C.interpolate d.xDomain (toFloat dims.nbins)
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
      limits = lookup .limits aes defaults
      (dx',steps) = C.interpolate limits (toFloat dims.nbins)
      xscale = d.xScale
      yscale = d.yScale
      dx = dx' / 2
      --xs = C.bins <| List.map (\x -> x - dx) steps
      lowerbar = [((fst limits) - dx')]
      upperbar = [((snd limits) + dx')]
      --xs = C.bins <| List.map (\x -> x - dx) (lowerbar ++ steps ++ upperbar)
      xs = C.bins <| List.map (\x -> x - dx) (steps ++ upperbar)
      --points = List.map (bar (xm,ym) (xscale,yscale) d.f) xs 
      points = List.map (bar (xm,ym) (xscale,yscale) limits d.f) xs 
   in
      GC.alpha visibility <| GC.group <| List.map (\x -> GC.outlined (linetype colour) <| GC.polygon x) points


{-| Calculating trapezium points for plotting them as polygons. -}
trapezoid (xm,ym) (xscale, yscale) f (x1,x2) =  
   List.map (\(x,y) -> (xm * (xscale x), ym * (yscale y))) 
            [(x1,0), (x2,0), (x2,f x2), (x1,f x1)]

geom_trapezoid aes defaults d dims = 
   let
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
      limits = lookup .limits aes defaults
      (dx,steps) = C.interpolate limits (toFloat dims.nbins)
      --(dx,steps) = C.interpolate d.xDomain (toFloat dims.nbins)
      xm = dims.xm
      ym = dims.ym
      xscale = d.xScale
      yscale = d.yScale
      xs = C.bins steps
      points = List.map (trapezoid (xm,ym) (xscale,yscale) d.f) xs 
   in
      GC.alpha visibility <| GC.group <| List.map (\x -> GC.outlined (linetype colour) <| GC.polygon x) points


geom_integral aes defaults d dims = 
   let
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
      limits = lookup .limits aes defaults
      (dx,steps) = C.interpolate limits (toFloat d.steps)
      x = lookup .x aes defaults
      --(dx,steps) = C.interpolate (snd limits,x) (toFloat d.steps)
      xm = dims.xm
      ym = dims.ym
      --xs = C.bins steps
      xs = List.map (\x -> (x,0)) steps
      rev_steps = List.reverse steps
      ys = List.map d.f rev_steps
      --area = xs
      points = xs ++ (List.map2 (,) rev_steps ys)
      area = List.map (\(x,y) -> (xm * (d.xScale x), ym * (d.yScale y))) points 
      --area = List.map (\(x,y) -> (dims.xm * (d.xScale x), dims.ym * (d.yScale y))) <| List.map2 (,) steps (List.map d.f steps)
   in
      GC.alpha visibility <| GC.filled colour <| GC.polygon area

geom_curve aes defaults d dims = 
   let
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
      linethickness = lookup .linethickness aes defaults -- not actually used yet
      xscale = d.xScale
      yscale = d.yScale
      (dx,steps) = C.interpolate d.xDomain (toFloat d.steps)
      ys = List.map (\(x,y) -> (dims.xm * (xscale x), dims.ym * (yscale y))) <| List.map2 (,) steps (List.map d.f steps)
   in
      GC.alpha visibility <| GC.traced (linetype colour) <| GC.path ys

geom_hline aes defaults d dims =
   let
      --y = dims.ypos
      y = lookup .y aes defaults
      --y = snd dims.pos
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
   in
      GC.alpha visibility <| GC.traced (linetype colour)  
      <| GC.path [(0,ym * (d.yScale y)), (xm, ym * (d.yScale y))] 

geom_vline aes defaults d dims =
   let
      --x = dims.xpos
      x = lookup .x aes defaults
      --x = fst dims.pos
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
   in
      GC.alpha visibility <| GC.traced (linetype colour)  
      <| GC.path [(xm * (d.xScale x),0), (xm * (d.xScale x), ym)] 

geom_circle aes defaults d dims =
   let
      xm = dims.xm
      ym = dims.ym
      xmin = fst d.xDomain
      ymin = fst d.yDomain
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      --radius = lookup .radius aes defaults
      radius = d.yExtent / 2
      visibility = lookup .visibility aes defaults
      --xpos = fst dims.pos
      --radius = ym * 0.2
      --radius = xm * xpos * 0.2
      --roundthing = GC.circle (xm * (d.xScale (xmin + radius)))
      roundthing = GC.circle (ym * (d.yScale (ymin + radius)))
      --origin = (xm * d.xScale 0, ym * d.yScale 0)
      xmid = (d.xExtent / 2) + xmin
      ymid = (d.yExtent / 2) + ymin
      origin = (xm * d.xScale xmid, ym * d.yScale ymid)
   in
      GC.move origin <| GC.alpha visibility <| GC.outlined (linetype colour) <| roundthing


geom_angle aes defaults d dims =
   let
       xm = dims.xm
       ym = dims.ym
       xmin = fst d.xDomain
       ymin = fst d.yDomain
       linetype = lookup .linetype aes defaults
       colour = lookup .colour aes defaults
       pointsize = lookup .pointsize aes defaults
       --radius = lookup .radius aes defaults
       radius = d.yExtent / 2
       x = lookup .x aes defaults
       annotate = lookup .annotate aes defaults
       translate = lookup .translate aes defaults
       --coordinates = fromPolar (d.xScale (xmin + radius), x) 
       coordinates = fromPolar (d.yScale (ymin + radius), x) 
       --pos = (C.dec 3 (fst coordinates), C.dec 3 (snd coordinates))
       --pos = (xm * (fst coordinates), xm * (snd coordinates))
       pos = (ym * (fst coordinates), ym * (snd coordinates))
       --origin = (xm * d.xScale 0, ym * d.yScale 0)
       xmid = (d.xExtent / 2) + xmin
       ymid = (d.yExtent / 2) + ymin
       origin = (xm * d.xScale xmid, ym * d.yScale ymid)
       annotationPosition = (xm * d.xScale (fst translate),  
                             ym * d.yScale (snd translate))
       point = GC.move pos  
               <| GC.move origin  
               <| GC.filled colour <| GC.circle pointsize
       label = GC.move annotationPosition <| GC.toForm <| Text.rightAligned  
               <| Text.fromString <| "&theta; &asymp; " ++ (toString <| C.dec 2 x)  
               ++ "\nr = " ++ (toString <| C.dec 2 (d.f x))
   in
      GC.group [point, label]

geom_curve_polar aes defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       xmin = fst d.xDomain
       ymin = fst d.yDomain
       ymax = snd d.yDomain
       linetype = lookup .linetype aes defaults
       colour = lookup .colour aes defaults

       --origin = (xm * d.xScale 0, ym * d.yScale 0)
       xmid = (d.xExtent / 2) + xmin
       ymid = (d.yExtent / 2) + ymin
       origin = (xm * d.xScale xmid, ym * d.yScale ymid)
       visibility = lookup .visibility aes defaults
       radius = d.yExtent / 2
       rscale = d.yScale (ymin + radius)
       points = List.map2 (,) d.ys d.xs
       ys = List.map (fromPolar << (\(r,t) -> (2 * rscale * ym * (d.yScale (r - ymax)), t))) points
   in
      GC.move origin <| GC.alpha visibility <| GC.traced (linetype colour) <| GC.path ys


geom_integral_polar aes defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      xmin = fst d.xDomain
      ymin = fst d.yDomain
      ymax = snd d.yDomain
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
      limits = lookup .limits aes defaults

      x = lookup .x aes defaults
      x' = if x < (fst limits) then (fst limits) else x
      x'' = if x' > (snd limits) then (snd limits) else x'
      (dx,steps) = C.interpolate (fst limits, x'') (toFloat d.steps)

      radius = d.yExtent
      rscale = d.yScale (ymin + radius)
      --origin = (xm * d.xScale 0, ym * d.yScale 0)
      xmid = (d.xExtent / 2) + xmin
      ymid = (d.yExtent / 2) + ymin
      origin = (xm * d.xScale xmid, ym * d.yScale ymid)
      ys' = List.map d.f steps
      points = [(2 * rscale * ym * (d.yScale (ymin)), xmin)]  
               ++ (List.map2 (,) ys' steps) 

      area = List.map (fromPolar << (\(r,t) ->  
         (rscale * ym * (d.yScale (r - ymax)), t))) points
   in
      GC.move origin <| GC.alpha visibility <| GC.filled colour <| GC.polygon area



geom_position_polar aes defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       xmin = fst d.xDomain
       ymin = fst d.yDomain
       ymax = snd d.yDomain
       (xpos,ypos) = dims.pos 
       colour = lookup .colour aes defaults
       pointsize = lookup .pointsize aes defaults
       --radius = lookup .radius aes defaults
       radius = d.yExtent / 2
       --rscale = d.xScale (xmin + radius)
       rscale = d.yScale (ymin + radius)
       --origin = (xm * d.xScale 0, ym * d.yScale 0)
       xmid = (d.xExtent / 2) + xmin
       ymid = (d.yExtent / 2) + ymin
       origin = (xm * d.xScale xmid, ym * d.yScale ymid)
       pos = fromPolar (2 * rscale * ym * (d.yScale (ypos - ymax)), xpos)

       point = GC.move pos  
               <| GC.move origin  
               <| GC.filled colour <| GC.circle pointsize
   in
      point


geom_trace_polar aes defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       xmin = fst d.xDomain
       ymin = fst d.yDomain
       ymax = snd d.yDomain
       x = lookup .x aes defaults
       linetype = lookup .linetype aes defaults
       colour = lookup .colour aes defaults
       visibility = lookup .visibility aes defaults
       --origin = (xm * d.xScale 0, ym * d.yScale 0)
       xmid = (d.xExtent / 2) + xmin
       ymid = (d.yExtent / 2) + ymin
       origin = (xm * d.xScale xmid, ym * d.yScale ymid)
       (dx,steps) = C.interpolate (fst d.xDomain, x) (toFloat d.steps)
       ys = List.map d.f steps
       radius = d.yExtent / 2
       rscale = d.yScale (ymin + radius)
       points' = List.map2 (,) ys steps 
       points = List.map (fromPolar << (\(r,t) -> (2 * rscale * ym * (d.yScale (r - ymax)), t))) points'
   in
      GC.move origin <| GC.alpha visibility <| GC.traced (linetype colour) <| GC.path points

geom_trace aes defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
      linethickness = lookup .linethickness aes defaults -- not actually used yet
      x = lookup .x aes defaults
      (dx,steps) = C.interpolate (fst d.xDomain,x) (toFloat d.steps)
      ys = List.map (\(x,y) -> (xm * (d.xScale x), ym * (d.yScale y)))  
                     <| List.map2 (,) steps (List.map d.f steps)
   in
      GC.alpha visibility <| GC.traced (linetype colour) <| GC.path ys



geom_tangent aes defaults d dims = 
   let
       --x' = .wposx dims
       xm = dims.xm
       ym = dims.ym
       linetype = lookup .linetype aes defaults
       linethickness = lookup .linethickness aes defaults
       colour = lookup .colour aes defaults
       visibility = lookup .visibility aes defaults
       pointsize = lookup .pointsize aes defaults
       translate = lookup .translate aes defaults
       annotate = lookup .annotate aes defaults
       decimals = lookup .decimals aes defaults
       (w',h') = lookup .dims aes defaults
       (w,h) = (toFloat w', toFloat h')
       annotationPosition = (xm * d.xScale (fst translate), ym * d.yScale (snd translate))
       --dx = 0.3 * (1 - (snd dims.wpos)) * d.xExtent 
       --dx = 0.3 * (1 - (snd dims.wpos)) * d.xExtent 
       delta = lookup .delta aes defaults
       --dx = delta * (1 - (snd dims.wpos)) * d.xExtent
       --dx = delta * (1 - (snd dims.wpos)) -- control dx with mouse y
       dx = delta
       xmin = fst d.xDomain
       ymin = fst d.yDomain
       ymax = snd d.yDomain
       --x = dims.xpos
       x = fst dims.pos
       tangent = C.tangent dx x d.f
       --tangent = C.tangent h x d.f
       m = tangent.slope
       b = tangent.intercept
       fun = (\x -> m * x + b)
       --y = (fun x)
       --x1 = xmin + (d.xExtent * (x' - dx))
       --x1 = xmin + (d.xExtent * x') - dx
       x' = xm * d.xScale x
       y' = xm * d.xScale (d.f x) 
       x1 = x - (dx / 2)
       x1' = xm * d.xScale x1
       y1 = fun x1
       y1' = ym * d.yScale y1
       --x2 = xmin + (d.xExtent * (x' + dx))
       --x2 = xmin + (d.xExtent * x') + dx
       x2 = x + (dx / 2)
       x2' = xm * d.xScale x2
       y2 = fun x2
       y2' = ym * d.yScale y2
       --ys = [(xm * d.xScale x1, ym * d.yScale y1),
       --      (xm * d.xScale x2, ym * d.yScale y2)]
       ys = [(x1', y1'),
             (x2', y2')]
       --xpos = (xm * d.xScale x,ym * d.yScale (d.f x))
       xpos = (x',ym * d.yScale (d.f x))
       --x1pos = (xm * d.xScale x1,ym * d.yScale (d.f x1))
       x1pos = (x1',ym * d.yScale (d.f x1))
       --x2pos = (xm * d.xScale x2,ym * d.yScale (d.f x2))
       x2pos = (x2',ym * d.yScale (d.f x2))
       vline1 = GC.alpha visibility <| GC.traced (GC.dotted colour) <| GC.path [List.head ys, x1pos]
       vline2 = GC.alpha visibility <| GC.traced (GC.dotted colour) <| GC.path [List.head <| List.tail ys, x2pos]
       xdot = GC.move xpos <| GC.filled colour <| GC.circle pointsize 
       x1dot = GC.move x1pos <| GC.filled colour <| GC.circle pointsize 
       x2dot = GC.move x2pos <| GC.filled colour <| GC.circle pointsize 
       --annotation = GC.move annotationPosition <| GC.toForm <| Text.leftAligned <| Text.fromString  
            --<| "slope: " ++ (toString m) ++ "x\nintercept: " ++ (toString <| C.dec 3 b)
       symbol = GC.filled colour <| GC.rect w h
       label = GC.move (w * 1.6,0) <| GC.toForm <| Text.leftAligned <| Text.fromString  
                  <| "&Delta;x = " ++ (toString <| C.dec decimals dx) 
       annotation = if annotate then  
                       GC.move annotationPosition <| GC.group [symbol,label] 
                    else GC.toForm empty
   in
        --GC.move (80,300) <| GC.toForm <| Text.rightAligned <| Text.fromString <| toString <| List.map (\(a,b) -> (C.dec 2 a, C.dec 2 b)) ys
        --GC.move (80,300) <| GC.toForm <| Text.rightAligned <| Text.fromString <| toString <| tangent
        --GC.move (80,300) <| GC.toForm <| Text.rightAligned <| Text.fromString <| toString <| y
        --GC.move (80,280) <| GC.toForm <| Text.rightAligned <| Text.fromString  <| (toString <| (C.dec 2 x1,C.dec 2 x,C.dec 2 x2, C.dec 2 (x2 - x1))) ++ "\n" ++ (toString <| (C.dec 2 y1,C.dec 2y,C.dec 2 y2, C.dec 2 (y2 - y1))) ++ "\n" ++ (toString (C.dec 2 ymin, C.dec 2 ymax))
       GC.alpha visibility <| GC.group [GC.traced (GC.solid colour) <| GC.path ys,  
       x1dot,  
       vline1,
       xdot,  
       x2dot,  
       vline2,
       annotation
       ]
       
-- customization object for axes?
--axisY d (xm,ym) xmargin = 
yAxis aes defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       xmargin = fst dims.margins
       xmin = fst d.xDomain
       --ymin = fst d.yDomain
       ymin = fst d.plotLimits
       --ymax = snd d.yDomain
       ymax = snd d.plotLimits
       pos = (xm * (d.xScale xmin) - (xmargin / 4),0)
       tickPositions = [ymin, 0, ymax]
       tickLabels = GC.group <| List.map (\y -> GC.move (-xmargin / 8,ym * d.yScale y)  
         <| GC.toForm <| Text.rightAligned <| Text.fromString <|  
         toString <| C.dec 2 y) tickPositions
       ticks = GC.group <| List.map (\y -> GC.move (-xmargin * 0.02,ym * d.yScale y)  
         <| GC.traced (GC.solid black) <| GC.path [(0,0),(12,0)]) tickPositions
       yBar = GC.traced (GC.solid black) <| GC.path [(10,0),(10,ym)]
       yLabel = GC.move (-xmargin * 0.35, ym * 0.5) <|  
         GC.toForm <| Text.rightAligned <| Text.fromString "Y" 
   in
      GC.move pos <| GC.group  
      [tickLabels, ticks, yBar, yLabel]

-- customization object for axes?
--axisX d (xm,ym) ymargin = 
xAxis aes defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       ymargin = fst dims.margins
       --ymin = fst d.yDomain
       ymin = fst d.plotLimits
       xmin = fst d.xDomain
       xmax = snd d.xDomain
       pos = (0,ym * (d.yScale ymin) - (ymargin / 12))
       tickPositions = [xmin, 0, xmax]
       tickLabels = GC.group <| List.map (\x -> GC.move (xm * d.xScale x,-ymargin * 0.3)  
         <| GC.toForm <| Text.centered <| Text.fromString <|  
         toString <| C.dec 2 x) tickPositions
       ticks = GC.group <| List.map (\x -> GC.move (xm * d.xScale x, -ymargin * 0.2)  
         <| GC.traced (GC.solid black) <| GC.path [(0,0),(0,12)]) tickPositions
       xBar = GC.traced (GC.solid black) <| GC.path [(0,-11),(xm,-11)]
       xLabel = GC.move (xm * 0.5,-ymargin * 0.55) <|  
         GC.toForm <| Text.rightAligned <| Text.fromString "X" 
   in
      GC.move pos <| GC.group  
      [tickLabels, ticks, xBar, xLabel]

--
title aes defaults d dims = 
   let
       txt = lookup .txt aes defaults
       ymargin = fst dims.margins
       xm = dims.xm
       ym = dims.ym
       --ymax = snd d.yDomain
       ymax = snd d.plotLimits
       midx = ((fst d.xDomain) + (snd d.xDomain)) / 2
       pos = (xm * (d.xScale midx),ym * (d.yScale ymax) + (ymargin / 4))
       --pos = (0,0)
       ttl = GC.toForm <| Text.centered <| Text.height 22 <| Text.fromString <| txt
   in
      GC.move pos <| ttl


legend aes defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       (w',h') = lookup .dims aes defaults
       (w,h) = (toFloat w', toFloat h')

       txt = lookup .txt aes defaults
       --txt = lookup .txt aes defaults
       colour = lookup .colour aes defaults
       translate = lookup .translate aes defaults
       annotationPosition = (xm * d.xScale (fst translate),  
                             ym * d.yScale (snd translate))
       symbol = GC.filled colour <| GC.rect w h
       label = GC.move (w * 1.5,0) <| GC.toForm <| Text.leftAligned <| Text.fromString txt
   in
       GC.move annotationPosition <| GC.group [symbol, label]


geom_doge aes defaults d dims = 
   let
      wh = lookup .dims aes defaults
      dogeurl = "https://31.media.tumblr.com/b23ef59e7838d323c281de41a31d672a/tumblr_inline_n0rlcbgFBe1s0subn.gif"
      dogeimg = image (fst wh) (snd wh) dogeurl
      xm = dims.xm
      ym = dims.ym
      (xpos,ypos) = dims.pos
   in
      GC.move (xm * d.xScale xpos, ym * d.yScale ypos) <| GC.toForm dogeimg

{-- CUSTOM AESTHETICS --}
customAes = { aes | colour <- Just blue,
                     linetype <- Just GC.solid, 
                     visibility <- Just 0.2,
                     radius <- Just 1,
                     translate <- Just (-1.57, 0.5),
                     --limits <- Just (pi/2,(3*pi)/2),
                     pointsize <- Just 8
                  } 
pointAes = { aes | pointsize <- Just 6, 
                     colour <- Just red,  
                     linethickness <- Just 5,
                     translate <- Just (-5.3,-0.14)
                  }
curveAes = { aes | colour <- Just darkGrey  
                  }
annotationAes = { aes | translate <- Just (-pi,0.1),
                        decimals <- Just 5,  
                        txt <- Just "y = 0.2cos(x) within -2pi &le; x &le; 2pi", 
                        limits <- Just (-pi/2,pi/2)
                     }

geoms = [layer_background { aes | colour <- Just white }, 
         xAxis aes,
         yAxis aes,
         --geom_curve curveAes, 
         geom_trace curveAes, 
         --geom_curve_polar { aes | colour <- Just red }, 
         geom_trace_polar { aes | colour <- Just red }, 
         --geom_points pointAes,
         geom_point pointAes,
         geom_hline { aes | x <- Just 0, y <- Just 0 },  
         geom_vline { aes | x <- Just 0, y <- Just 0 },
         --geom_tangent pointAes,
         --geom_tangent { pointAes | delta <- Just (1.5 * pi), colour <- Just black, translate <- Just (-5,0.16) },
         --geom_tangent { pointAes | delta <- Just pi, colour <- Just darkBlue, translate <- Just (-5,0.12) },
         --geom_tangent { pointAes | delta <- Just (0.5 * pi), colour <- Just purple, translate <- Just (-5,0.08) },
         ----geom_tangent { pointAes | delta <- Just (0.2 * pi), colour <- Just red },
         --geom_tangent { pointAes | delta <- Just 0.25, colour <- Just darkOrange, translate <- Just (-5,0.04) },
         --legend { aes | translate <- Just (3,0.1), colour <- Just black, txt <- Just "&Delta;x = " },
         --title annotationAes
         --geom_hline { aes | x <- Just 1, y <- Just -1 },  
         --geom_hline { aes | x <- Just 1, y <- Just 1 },  
         geom_vline { aes | x <- Just (-pi/2), y <- Just 1 },
         geom_vline { aes | x <- Just (pi/2), y <- Just 1 },
         --geom_bar customAes,
         --geom_trapezoid customAes,
         --geom_integral customAes,
         geom_integral_polar customAes,
         geom_circle customAes,
         geom_angle customAes,
         geom_position_polar customAes
         --geom_doge { aes | dims <- Just (50,50) }
         --annotate_integral annotationAes 
         ]

type alias Input = Signal { x : Int,
                            y : Int, 
                            width : Int, 
                            height : Int
                           }
input : Input
input = Signal.map2 (\(mx,my) (w,h) -> { x = mx, y = my, width = w, height = h}) Mouse.position Window.dimensions

--main : Signal Element
--main = Signal.map2 (plotc (1400,800) pc geoms) Mouse.position Window.dimensions 

--main : Signal Element
--main = Signal.map plots input
--plots input = 
--   plotc (1400,600) pc5  
--      (geoms ++ [title { aes | txt <- Just "y = 0.2cos(x) within -2&pi; &le; x &le; 2&pi;" }]) input

main : Signal Element
main = Signal.map plots input
plots input =  
      flow right <| List.map (flow down) [ 
      [plotc (700,400) pc  
         (geoms ++ [title { aes | txt <- Just "y = cos(x) within -2&pi; &le; x &le; 2&pi;" }])  
         input,
      plotc (700,400) pc2  
         (geoms ++ [title { aes | txt <- Just "y' = -sin(x) within -2&pi; &le; x &le; 2&pi;" }])  
         input],
      [plotc (700,400) pc3  
         (geoms ++ [title { aes | txt <- Just "y = sin(x) within -2&pi; &le; x &le; 2&pi;" }])  
         input,
      plotc (700,400) pc4  
         (geoms ++ [title { aes | txt <- Just "y' = -cos(x) within -2&pi; &le; x &le; 2&pi;" }])  
         input]
      ]

--plotc : (Int,Int) -> PlotConfig -> (Int,Int) -> (Int,Int) -> Element
--plotc (plotWidth,plotHeight) d geoms (mouseX,mouseY) (windowWidth,windowHeight) =  
plotc (plotWidth,plotHeight) d geoms input =  
   let

      -- Pass two "aesthetics" to each geom, where a custom one can override defaults
      defaults = { aesDefault | delta <- Just d.dx,  
                                x <- Just xpos,  
                                y <- Just ypos, 
                                theta <- Just (pi / 4),
                                --input <- Just input,
                                limits <- Just d.xDomain
                             }
      
      windowScaleX = C.normalize (0,toFloat input.width)    --- 
      windowScaleY = C.normalize (0,toFloat input.height)   ---
      nbins = round <| (toFloat input.x) / 4   --- 
      wpos = (windowScaleX (toFloat input.x), windowScaleY (toFloat input.y))   --- 
      xpos = (fst d.xDomain) + d.xExtent * (fst wpos)
      ypos = d.f xpos
      --radius = sqrt (xpos^2 + ypos^2)
      xmargin = (toFloat plotWidth) * 0.1
      ymargin = (toFloat plotHeight) * 0.2
      xoffset = (toFloat plotWidth)/2 - xmargin
      yoffset = (toFloat plotHeight)/2 - ymargin
      innerWidth = (toFloat plotWidth) - xmargin
      innerHeight = (toFloat plotHeight) - ymargin
      ymultiplier = (toFloat plotHeight) - (2 * ymargin)
      xmultiplier = (toFloat plotWidth) - (2 * xmargin)
      --baseline = List.map (\x -> 0) [1..List.length d.xs]

      dims = {
         windowScaleX = windowScaleX,
         windowScaleY = windowScaleY,
         nbins = nbins,
         wpos = wpos,
         --xpos = xpos,
         --ypos = ypos,
         pos = (xpos,ypos),
         xmargin = xmargin,
         ymargin = ymargin,
         margins = (xmargin,ymargin),
         --xoffset = xoffset,
         --yoffset = yoffset,
         innerWidth = innerWidth,
         innerHeight = innerHeight,
         ym = ymultiplier,
         xm = xmultiplier
      }

      --zeroX = geom_hline customAes defaults d dims 0
      --zeroY = geom_vline customAes defaults d dims 0
      offsets = (-xoffset, -yoffset)
   in 
      GC.collage plotWidth plotHeight  
            [GC.move offsets <| GC.group <| List.map (\g -> g defaults d dims) geoms]
             --[  
             -- --GC.move offsets <| xMarker,
             -- --GC.move offsets <| yMarker,
             -- GC.move offsets <| annotate_integral annotationAes defaults d dims,
             -- ]


layer_background aes defaults d dims = 
   let
       colour = lookup .colour aes defaults
       xm = dims.xm
       ym = dims.ym
       colour2 = lightGrey
   in
      GC.group  
      [ 
         --GC.move (dims.xoffset, dims.yoffset) <| GC.group  
         GC.move (xm * 0.5, ym * 0.5) <| GC.group  
         <| [GC.filled colour <| GC.rect dims.innerWidth dims.innerHeight,
            GC.filled colour2 <| GC.rect xm ym],  

         GC.traced (GC.solid black) <|  
            GC.path [(0,0),(0,ym), (xm,ym),(xm,0),(0,0)] 
      ]

annotate_integral aes defaults d dims = 
   let
       decimals = lookup .decimals aes defaults
       translate = lookup .translate aes defaults
       limits = lookup .limits aes defaults
       xmin = fst limits
       xmax = snd limits
       integral = C.integrate limits (toFloat dims.nbins) d.f
       --xmin = fst d.xDomain
       --xmax = snd d.xDomain
       --integral = C.integrate d.xDomain (toFloat dims.nbins) d.f
   in
       GC.move (dims.xm * (d.xScale (fst translate)),  
                dims.ym * (d.yScale (snd translate)))
              <| GC.toForm <| Text.rightAligned <| Text.fromString  
              <| "number of bins: " ++ (toString dims.nbins) 
              ++ "\n&#x222b;"  
                     ++ " from " ++ (toString <| C.dec decimals xmin)  
                     ++ " to " ++ (toString <| C.dec decimals xmax)  
                     ++ " &#8776; " ++ (toString <| C.dec decimals <| integral)


{-- 
TODO: 
geom_wheel, with rotation? 
input time?
implement grid...  
linestyles and textstyles?! 
improve axes 
--}

       --mouseX = .x <| lookup .input aes defaults


{- aesthetics -}

lookup key child parent = let
                            childValue = key child
                            parentValue = key parent
                          in
                            case (childValue,parentValue) of
                                (Just c,_) -> c
                                (Nothing, Just p) -> p


            
type alias AES = { visibility:Maybe Float, 
                   linetype:Maybe (Color -> GC.LineStyle),
                   linethickness:Maybe Float,
                   pointsize:Maybe Float,
                   colour:Maybe Color,  
                   translate:Maybe (Float,Float),
                   decimals: Maybe Float,
                   dims: Maybe (Int,Int),
                   txt: Maybe String,
                   delta: Maybe Float,
                   limits: Maybe (Float,Float),
                   annotate: Maybe Bool,
                   x: Maybe Float,
                   y: Maybe Float,
                   radius: Maybe Float,
                   theta: Maybe Float,
                   input: Maybe { height : Int, width : Int, x : Int, y : Int }
                }

aes : AES
aes = { visibility = Nothing, 
        linetype = Nothing,  
        linethickness = Nothing,  
        pointsize = Nothing,  
        colour = Nothing,
        translate = Nothing,
        decimals = Nothing,
        dims = Nothing,
        txt = Nothing,
        delta = Nothing,
        limits = Nothing,
        annotate = Nothing,
        x = Nothing, 
        y = Nothing,
        radius = Nothing,
        theta = Nothing,
        input = Nothing
     }

aesDefault : AES
aesDefault = { aes | visibility <- Just 1, 
                     linetype <- Just GC.solid,  
                     linethickness <- Just 1,  
                     pointsize <- Just 2,  
                     colour <- Just black,
                     translate <- Just (0,0),
                     decimals <- Just 2,
                     dims <- Just (30,3),
                     txt <- Just "Plot title",
                     delta <- Nothing,
                     limits <- Nothing,
                     annotate <- Just True, 
                     x <- Nothing,
                     y <- Nothing,
                     radius <- Just 1,
                     theta <- Nothing,
                     input <- Nothing
               }
