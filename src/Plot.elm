-- function graphs for plotting distributions
-- a work in progress

module Plot (Distribution, distribution, discrete, geom_step, geom_point, geom_points, geom_bar, geom_trapezoid, geom_area, geom_curve, geom_hlinerange, geom_vlinerange, geom_hline, geom_vline, geom_hline_polar, geom_vline_polar, geom_circle, geom_angle, geom_curve_polar, geom_area_polar, geom_position_polar, geom_trace_polar, geom_trace, geom_abline, geom_tangent, yAxis, xAxis, title, legend, geom_doge, geom_mario, plot, background, annotate_integral, Aes, aes, aesDefault, point, bar, trapezoid, geom_none, lookup, Geom) where


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
--import List (map, map2, head, tail, reverse, minimum, maximum, length)


type alias Geom = GC.Form

geom_none : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_none aes' defaults d dims = 
   let
      x = lookup .x aes' defaults
   in
      GC.toForm <| Text.plainText <| toString x


--type alias Input = Signal { x : Int,
--                            y : Int, 
--                            width : Int, 
--                            height : Int
--                           }
--input : Input
----input m w = { x = fst m, y = snd m, width = fst w, height = snd w}
--input = Signal.map2 (\(mx,my) (w,h) -> { x = mx, y = my, width = w, height = h}) Mouse.position Window.dimensions

type alias Distribution = {
         discrete: Bool,
         f: (Float -> Float),
         xs: List Float,
         ys: List Float, 
         polar:List (Float,Float),
         xDomain: (Float,Float), 
         yDomain: (Float,Float),
         xExtent: Float,
         rmax:Float,
         steps:Int,
         dx:Float,
         --interpolator:List Float,
         xScale:(Float -> Float),
         yScale:(Float -> Float),
         yExtent:Float, 
         xLimits:(Float,Float),
         yLimits:(Float,Float),
         xyRatio:Float,
         pOffset:Float,
         toX:(Float -> Float),
         toY:(Float -> Float)
      }

distribution : (Float -> Float) -> (Float, Float) -> (Float, Float) -> Int -> Distribution
distribution f (xmin,xmax) (ymin,ymax) steps = 
   let
       xDomain = (xmin, xmax)
       --yDomain = (-1,1)
       xExtent = (xmax - xmin)
       --interpolator = List.tail <| List.map (C.normalize (1,toFloat steps)) [1..toFloat steps] 
       interpolator = List.map (C.normalize (0,toFloat steps)) [0..toFloat steps] 
       xs = List.map (\x -> xmin + xExtent * x) interpolator
       ys = List.map f xs
       dx = xExtent / (toFloat steps)
       xScale = C.normalize xDomain
       --ymin = List.minimum ys
       --ymax = List.maximum ys
       rmax = if (abs ymax) > (abs ymin) then (abs ymax) else (abs ymin)
       yLimits = (-rmax, rmax)
       xLimits = yLimits
       --yLimits = (ymin, ymax)
       yScale = C.normalize yLimits
       --xScale = C.normalize xLimits
       yDomain = (ymin, ymax)
       yExtent = ymax - ymin
       --xyRatio = rmax / (snd xDomain)
       xyRatio = rmax / (xExtent / 2)
       sign = (xmin / xmin) * (xmax / xmax)
       --skew = (abs xmax) - (abs from)
       pOffset = (xmax + xmin) / 2
       --polar = List.map fromPolar <| List.map2 (,) ys xs
       polar = mkPolar xs ys
       toX = (\x -> (fst xDomain) + xExtent * (xScale x))
       toY = (\y -> (fst yDomain) + yExtent * (yScale y))
   in
                        {  
                           discrete = False,
                           f = f, 
                           xs = xs,
                           ys = ys,
                           polar = polar,
                           xDomain = xDomain, 
                           yDomain = yDomain,
                           xExtent = xExtent,
                           rmax = rmax,
                           steps = steps,
                           dx = dx,
                           --interpolator = interpolator,
                           xScale = xScale,
                           yScale = yScale,
                           yExtent = yExtent,
                           xLimits = xLimits,
                           yLimits = yLimits,
                           xyRatio = xyRatio,
                           pOffset = pOffset,
                           toX = toX,
                           toY = toY
                        }


discrete : (Float -> Float) -> (Float, Float) -> (Float, Float) -> Distribution
discrete f (xmin',xmax') (ymin,ymax) = 
   let
       xmin = xmin' - 0.5
       xmax = xmax' + 0.5
       xDomain = (xmin, xmax)
       xExtent = (xmax - xmin)
       xs = [xmin' .. xmax']
       ys = List.map f xs
       steps = List.length xs
       dx = 1
       xScale = C.normalize xDomain
       --ymin = List.minimum ys
       --ymax = List.maximum ys
       rmax = if (abs ymax) > (abs ymin) then (abs ymax) else (abs ymin)
       yLimits = (-rmax, rmax)
       xLimits = yLimits
       yScale = C.normalize yLimits
       yDomain = (ymin, ymax)
       yExtent = ymax - ymin
       xyRatio = rmax / (snd xDomain)
       skew = (xmax - xmin)
       pOffset = skew / 2
       --polar = List.map fromPolar <| List.map2 (,) ys xs
       polar = mkPolar xs ys
       toX = (\x -> (fst xDomain) + xExtent * (xScale x))
       toY = (\y -> (fst yDomain) + yExtent * (yScale y))
   in
                        {  
                           discrete = True,
                           f = f, 
                           xs = xs,
                           ys = ys,
                           polar = polar,
                           xDomain = xDomain, 
                           yDomain = yDomain,
                           xExtent = xExtent,
                           rmax = rmax,
                           steps = steps,
                           dx = dx,
                           --interpolator = interpolator,
                           xScale = xScale,
                           yScale = yScale,
                           yExtent = yExtent,
                           xLimits = xLimits,
                           yLimits = yLimits,
                           xyRatio = xyRatio,
                           pOffset = pOffset,
                           toX = toX,
                           toY = toY
                        }

geom_step : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_step aes' defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      colour = lookup .colour aes' defaults
      pointsize = lookup .pointsize aes' defaults
      visibility = lookup .visibility aes' defaults
      linetype = lookup .linetype aes' defaults
      x = lookup .x aes' defaults
      dynamic = lookup .dynamic aes' defaults
      nsteps = if dynamic then (toFloat dims.nbins) else (toFloat d.steps) 
      (dx,steps') = if d.discrete then (1,d.xs) else (C.interpolate d.xDomain nsteps)
      lastx = List.head <| List.reverse steps'
      points = List.concatMap (\x -> [(x, d.f x),(if (x < lastx) then x+dx else x, d.f x)]) steps'

      ys = List.map (\(x,y) -> (xm * (d.xScale x), ym * (d.yScale y))) points 
                                 -- <| List.map2 (,) steps' (List.map d.f steps)
   in
      GC.alpha visibility <| GC.traced (linetype colour) <| GC.path ys

mkPolar xs ys = List.map fromPolar <| List.map2 (,) ys xs
revolve points x = List.map (\(px,py) -> (((0.5 * py * cos x) - (0.5 * px * sin x)) + x, ((py * sin x) + (px * cos x)))) points





geom_point : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_point aes' defaults d dims = 
   let
      --(dx,steps) = C.interpolate d.xDomain (toFloat dims.nbins)
      xm = dims.xm
      ym = dims.ym
      colour = lookup .colour aes' defaults
      pointsize = lookup .pointsize aes' defaults
      visibility = lookup .visibility aes' defaults
      --colour = lookup .colour aes aesDefault
      --pointsize = lookup .pointsize aes aesDefault
      --visibility = lookup .visibility aes aesDefault
      x = lookup .x aes' defaults
      --x = 1.57
      y = d.f x
      pos = point (xm,ym) (d.xScale, d.yScale) d.f x y
   in
      GC.move pos <| GC.alpha visibility <| GC.filled colour <| GC.circle pointsize



point (xm,ym) (xscale, yscale) f x y =  
      (xm * (xscale x), ym * (yscale <| f x)) 

geom_points : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_points aes' defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      colour = lookup .colour aes' defaults
      pointsize = lookup .pointsize aes' defaults
      visibility = lookup .visibility aes' defaults
      dynamic = lookup .dynamic aes' defaults
      nsteps = if dynamic then (toFloat dims.nbins) else (toFloat d.steps) 
      --(dx,steps) = C.interpolate d.xDomain nsteps
      (dx,steps) = if d.discrete then (1,d.xs) else (C.interpolate d.xDomain nsteps)
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


-- keep one separate "bar chart" for discrete distributions?!
geom_bar : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_bar aes' defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      limits' = lookup .limits aes' defaults
      limits = if d.discrete then d.xDomain else limits'
      dynamic = lookup .dynamic aes' defaults
      nsteps = if dynamic then (toFloat dims.nbins) else (toFloat d.steps) 
      (dx',steps) = if d.discrete then (1,d.xs) else (C.interpolate d.xDomain nsteps)
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



-- integral approximation by bars/rectangles
--geom_bar : Aes -> Aes -> Distribution -> Dimensions -> Geom
--geom_bar aes' defaults d dims = 
--   let
--      --(dx',steps) = C.interpolate d.xDomain (toFloat dims.nbins)
--      xm = dims.xm
--      ym = dims.ym
--      linetype = lookup .linetype aes' defaults
--      colour = lookup .colour aes' defaults
--      visibility = lookup .visibility aes' defaults
--      limits = lookup .limits aes' defaults
--      (dx',steps) = C.interpolate limits (toFloat dims.nbins)
--      xscale = d.xScale
--      yscale = d.yScale
--      dx = dx' / 2
--      --xs = C.bins <| List.map (\x -> x - dx) steps
--      lowerbar = [((fst limits) - dx')]
--      upperbar = [((snd limits) + dx')]
--      --xs = C.bins <| List.map (\x -> x - dx) (lowerbar ++ steps ++ upperbar)
--      xs = C.bins <| List.map (\x -> x - dx) (steps ++ upperbar)
--      --points = List.map (bar (xm,ym) (xscale,yscale) d.f) xs 
--      points = List.map (bar (xm,ym) (xscale,yscale) limits d.f) xs 
--   in
--      GC.alpha visibility <| GC.group <| List.map (\x -> GC.outlined (linetype colour) <| GC.polygon x) points


{-| Calculating trapezium points for plotting them as polygons. -}
trapezoid (xm,ym) (xscale, yscale) f baseline (x1,x2) =  
   List.map (\(x,y) -> (xm * (xscale x), ym * (yscale y))) 
            [(x1,baseline x1), (x2,baseline x2), (x2,f x2), (x1,f x1)]
            --[(x1,0), (x2,0), (x2,f x2), (x1,f x1)]

geom_trapezoid : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_trapezoid aes' defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      baseline = lookup .fun aes' defaults
      limits = lookup .limits aes' defaults
      (dx,steps) = C.interpolate limits (toFloat dims.nbins)
      --(dx,steps) = C.interpolate d.xDomain (toFloat dims.nbins)
      --xscale = d.xScale
      --yscale = d.yScale
      xs = C.bins steps
      points = List.map (trapezoid (xm,ym) (d.xScale,d.yScale) d.f baseline) xs 
   in
      GC.alpha visibility <| GC.group <| List.map (\x -> GC.outlined (linetype colour) <| GC.polygon x) points


geom_area : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_area aes' defaults d dims = 
   let
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      baseline = lookup .fun aes' defaults
      dynamic = lookup .dynamic aes' defaults
      negate = lookup .negate aes' defaults

      limits = lookup .limits aes' defaults
      x = lookup .x aes' defaults
      limits' = if negate then (x, snd limits) else (fst limits, x)

      (dx,steps) = C.interpolate (if dynamic then limits' else limits) (toFloat d.steps)
      --(dx,steps) = C.interpolate (snd limits,x) (toFloat d.steps)
      xm = dims.xm
      ym = dims.ym
      --xs = C.bins steps
      xs = List.map (\x -> (x,baseline x)) steps
      rev_steps = List.reverse steps
      f = if d.discrete then (d.f << roundf) else d.f
      ys = List.map f rev_steps
      --area = xs
      points = xs ++ (List.map2 (,) rev_steps ys)
      area = List.map (\(x,y) -> (xm * (d.xScale x), ym * (d.yScale y))) points 
      --area = List.map (\(x,y) -> (dims.xm * (d.xScale x), dims.ym * (d.yScale y))) <| List.map2 (,) steps (List.map d.f steps)
   in
      GC.alpha visibility <| GC.filled colour <| GC.polygon area

geom_curve : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_curve aes' defaults d dims = 
   let
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      linethickness = lookup .linethickness aes' defaults -- not actually used yet
      xscale = d.xScale
      yscale = d.yScale
      --(dx,steps) = C.interpolate d.xDomain (toFloat d.steps)
      (dx,steps) = if d.discrete then (1,d.xs) else (C.interpolate d.xDomain (toFloat d.steps))
      ys = List.map (\(x,y) -> (dims.xm * (xscale x), dims.ym * (yscale y))) <| List.map2 (,) steps (List.map d.f steps)
   in
      GC.alpha visibility <| GC.traced (linetype colour) <| GC.path ys

geom_vlinerange : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_vlinerange aes' defaults d dims =
   GC.toForm empty


geom_hlinerange : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_hlinerange aes' defaults d dims =
   let
      y = lookup .y aes' defaults
      xm = dims.xm
      ym = dims.ym
      (xmin,xmax) = d.xDomain
      limits = lookup .limits aes' defaults
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      annotate = lookup .annotate aes' defaults
      label = lookup .label aes' defaults
      precision = lookup .precision aes' defaults
      translate = lookup .translate aes' defaults
      txt' = GC.move (xm * d.xScale ((snd limits) + (fst translate)), ym * d.yScale (y + snd translate)) <| GC.toForm <| Text.plainText label
      -- HOW TO ANNOTATE?
      txt = if annotate then txt' else (GC.toForm empty)
      line = GC.alpha visibility <| GC.traced (linetype colour)  
      <| GC.path [(xm * d.xScale (fst limits), ym * (d.yScale y)), (xm * d.xScale (snd limits), ym * (d.yScale y))] 
   in
      GC.group [txt,line]

geom_hline : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_hline aes' defaults d dims =
   let
      --y = dims.ypos
      y = lookup .y aes' defaults
      --y = snd dims.pos
      xm = dims.xm
      ym = dims.ym
      (xmin,xmax) = d.xDomain
      --limits = lookup .limits aes' defaults
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      annotate = lookup .annotate aes' defaults
      label = lookup .label aes' defaults
      precision = lookup .precision aes' defaults
      translate = lookup .translate aes' defaults
      txt' = GC.move (xm * d.xScale (xmax + fst translate), ym * d.yScale (y + snd translate)) <| GC.toForm <| Text.plainText <| label ++ (toString (C.dec precision <| d.toY y)) 
      txt = if annotate then txt' else (GC.toForm empty)
      line = GC.alpha visibility <| GC.traced (linetype colour)  
      <| GC.path [(0,ym * (d.yScale y)), (xm, ym * (d.yScale y))] 
   in
      GC.group [txt,line]



geom_vline : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_vline aes' defaults d dims =
   let
      --x = dims.xpos
      x = lookup .x aes' defaults
      --x = fst dims.pos
      (ymin,ymax) = d.yDomain
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      annotate = lookup .annotate aes' defaults
      label = lookup .label aes' defaults
      precision = lookup .precision aes' defaults
      translate = lookup .translate aes' defaults
      txt' = GC.move (xm * d.xScale (x + fst translate), ym * d.yScale (ymax + snd translate)) <| GC.toForm <| Text.plainText <| label ++ (toString (C.dec precision <| d.toX x)) 
      txt = if annotate then txt' else (GC.toForm empty)
      line = GC.alpha visibility <| GC.traced (linetype colour)  
      <| GC.path [(xm * (d.xScale x),0), (xm * (d.xScale x), ym)] 
   in
      GC.group [txt,line]


geom_hline_polar : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_hline_polar aes' defaults d dims =
   let
      (xmin,xmax) = d.xDomain
      (ymin,ymax) = d.yDomain
      y = lookup .y aes' defaults
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      fit = lookup .fit aes' defaults
      xyRatio = if fit then d.xyRatio else 1
      --offset = if rotate then 0 else d.pOffset
      points = List.map fromPolar  
         <| List.map (\x -> (y,x)) d.xs
      hline = GC.path  
         <| List.map (\(x,y) -> (xm * d.xScale (d.pOffset + (x / xyRatio)), ym * d.yScale y)) 
         points
         -- <| List.map fromPolar [(xmin, y), (xmax, y)]  
   in
      GC.alpha visibility <| GC.traced (linetype colour) <| hline 
      -- <| GC.path [(0,ym * (d.yScale y)), (xm, ym * (d.yScale y))] 

geom_vline_polar : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_vline_polar aes' defaults d dims =
   let
      (xmin,xmax) = d.xDomain
      (ymin,ymax) = d.yDomain
      x = lookup .x aes' defaults
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      fit = lookup .fit aes' defaults
      xyRatio = if fit then d.xyRatio else 1
      --offset = if rotate then 0 else d.pOffset
      points = List.map fromPolar  
         <| List.map (\y -> (y,x)) d.ys
      hline = GC.path  
         <| List.map (\(x,y) -> (xm * d.xScale (d.pOffset + (x / xyRatio)), ym * d.yScale y)) 
         points
   in
      GC.alpha visibility <| GC.traced (linetype colour) <| hline 
      --GC.alpha visibility <| GC.traced (linetype colour)  
      -- <| GC.path [(xm * (d.xScale x),0), (xm * (d.xScale x), ym)] 

geom_trace_polar : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_trace_polar aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       x = lookup .x aes' defaults
       --y = lookup .y aes' defaults
       linetype = lookup .linetype aes' defaults
       colour = lookup .colour aes' defaults
       visibility = lookup .visibility aes' defaults
       rotate = lookup .rotate aes' defaults
       fit' = lookup .fit aes' defaults
       fit = if rotate then False else fit'
       xyRatio = if fit then d.xyRatio else 1
       offset = if rotate then 0 else d.pOffset
       --origin = (0,0)
       (dx,steps) = C.interpolate (fst d.xDomain, x) (toFloat d.steps)
       --ys = List.map d.f steps
       --ys = List.map fromPolar <| List.map2 (,) (List.map d.f steps) steps
       ys = mkPolar steps (List.map d.f steps)

       points' = if rotate then revolve ys x else ys
       points = List.map (\(x,y) -> (xm * d.xScale (offset + (x / xyRatio)), ym * d.yScale y)) points'
       --points = List.map (\(x,y) -> (xm * d.xScale (x / xyRatio), ym * d.yScale y)) <| List.map fromPolar <| List.map2 (,) ys steps 
   in
      --GC.move origin <|  
      GC.alpha visibility <| GC.traced (linetype colour) <| GC.path points

geom_circle : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_circle aes' defaults d dims =
   let
      xm = dims.xm
      ym = dims.ym
      x = lookup .x aes' defaults
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      --fit = lookup .fit aes' defaults
      rotate = lookup .rotate aes' defaults
      fit' = lookup .fit aes' defaults
      fit = if rotate then False else fit'
      xyRatio = if fit then d.xyRatio else 1
      offset = if rotate then 0 else d.pOffset
      ys = List.map (fromPolar << (\x -> (d.rmax,x))) d.xs
      points' = if rotate then revolve ys x else ys

      points = List.map (\(x,y) -> (xm * d.xScale (offset + (x / xyRatio)), ym * d.yScale y)) points'

      thing = GC.path points
      origin = (0,0)
   in
      GC.move origin <| GC.alpha visibility <| GC.outlined (linetype colour) <| thing



geom_angle : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_angle aes' defaults d dims =
   let
       xm = dims.xm
       ym = dims.ym
       linetype = lookup .linetype aes' defaults
       colour = lookup .colour aes' defaults
       pointsize = lookup .pointsize aes' defaults
       rotate = lookup .rotate aes' defaults
       fit' = lookup .fit aes' defaults
       fit = if rotate then False else fit'
       xyRatio = if fit then d.xyRatio else 1
       offset = if rotate then 0 else d.pOffset
       radius = d.yExtent / 2
       x = lookup .x aes' defaults
       annotate = lookup .annotate aes' defaults
       translate = lookup .translate aes' defaults
       --(x',y') = fromPolar (d.rmax, x) 
       point' = [fromPolar (d.rmax, x)] 
       point = if rotate then revolve point' x else point'
       (x',y') = List.head point

       pos = (xm * d.xScale (offset + (x' / xyRatio)), ym * d.yScale y')
       --origin = (0,0)
       annotationPosition = (xm * d.xScale (d.pOffset + fst translate),  
                             ym * d.yScale (snd translate))
       dot = GC.move pos  
               --<| GC.move origin  
               <| GC.filled colour <| GC.circle pointsize
       label' = GC.move annotationPosition <| GC.toForm <| Text.rightAligned <| Text.color colour 
               <| Text.fromString <| "&theta; &asymp; " ++ (toString <| C.dec 2 x)  
               ++ "\nr = " ++ (toString <| C.dec 2 (d.f x))
       label = if annotate then label' else (GC.toForm empty)

   in
      GC.group [dot, label]

geom_curve_polar : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_curve_polar aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       x = lookup .x aes' defaults
       linetype = lookup .linetype aes' defaults
       colour = lookup .colour aes' defaults
       visibility = lookup .visibility aes' defaults
       rotate = lookup .rotate aes' defaults
       fit' = lookup .fit aes' defaults
       fit = if rotate then False else fit'
       xyRatio = if fit then d.xyRatio else 1
       offset = if rotate then 0 else d.pOffset
       origin = lookup .translate aes' defaults
       points' = if rotate then revolve d.polar x else d.polar
       points = List.map (\(x,y) -> (xm * d.xScale (offset + (x / xyRatio)), ym * d.yScale y)) points'
   in
      GC.move origin <| GC.alpha visibility <| GC.traced (linetype colour) <| GC.path points


geom_area_polar : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_area_polar aes' defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      limits = lookup .limits aes' defaults
      rotate = lookup .rotate aes' defaults
      fit' = lookup .fit aes' defaults
      fit = if rotate then False else fit'
      xyRatio = if fit then d.xyRatio else 1
      offset = if rotate then 0 else d.pOffset
      baseline = lookup .fun aes' defaults

      dynamic = lookup .dynamic aes' defaults
      negate = lookup .negate aes' defaults

      x = lookup .x aes' defaults
      x' = if x < (fst limits) then (fst limits) else x
      x'' = if x' > (snd limits) then (snd limits) else x'

      limits' = if negate then (x'', snd limits) else (fst limits, x'')

      (dx,steps) = C.interpolate (if dynamic then limits' else limits) (toFloat d.steps)

      origin = (0,0)
      ys' = List.map d.f steps

      f_points = [origin] ++ (List.map2 (,) ys' steps) 

      baseline_ys = List.map baseline steps 
      baseline_points = List.reverse <| [origin] ++ (List.map2 (,) baseline_ys steps) 

      points' = List.map fromPolar (f_points ++ baseline_points)
      points = if rotate then revolve points' x else points'

      area = List.map (\(x,y) -> (xm * d.xScale (offset + (x / xyRatio)), ym * d.yScale y)) points 
   in
      GC.move origin <| GC.alpha visibility <| GC.filled colour <| GC.polygon area



geom_position_polar : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_position_polar aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       --(xpos,ypos) = dims.pos 
       x' = lookup .x aes' defaults
       y' = lookup .y aes' defaults
       colour = lookup .colour aes' defaults
       visibility = lookup .visibility aes' defaults
       pointsize = lookup .pointsize aes' defaults
       fit = lookup .fit aes' defaults
       xyRatio = if fit then d.xyRatio else 1
       --offset = if rotate then 0 else d.pOffset
       origin = (0,0)
       pos = (\(x,y) -> (xm * d.xScale (d.pOffset + (x / xyRatio)), ym * d.yScale y)) <| fromPolar (y',x')

       point = GC.move pos  
               <| GC.move origin <| GC.alpha visibility 
               <| GC.filled colour <| GC.circle pointsize
   in
      point



geom_trace : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_trace aes' defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      linethickness = lookup .linethickness aes' defaults -- not actually used yet
      x = lookup .x aes' defaults
      (dx,steps) = C.interpolate (fst d.xDomain,x) (toFloat d.steps)
      ys = List.map (\(x,y) -> (xm * (d.xScale x), ym * (d.yScale y)))  
                     <| List.map2 (,) steps (List.map d.f steps)
   in
      GC.alpha visibility <| GC.traced (linetype colour) <| GC.path ys




geom_abline : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_abline aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       linetype = lookup .linetype aes' defaults
       linethickness = lookup .linethickness aes' defaults
       colour = lookup .colour aes' defaults
       visibility = lookup .visibility aes' defaults
       limits = lookup .limits aes' defaults
       dynamic = lookup .dynamic aes' defaults
       negate = lookup .negate aes' defaults
       x = lookup .x aes' defaults
       fun = lookup .fun aes' defaults

       x' = if x < (fst limits) then (fst limits) else x
       x'' = if x' > (snd limits) then (snd limits) else x'

       limits' = if negate then (x'', snd limits) else (fst limits, x'')
       (x1,x2) = if dynamic then limits' else limits
       points = [(xm * d.xScale x1, ym * d.yScale (fun x1)), 
                 (xm * d.xScale x2, ym * d.yScale (fun x2))] 
   in
       GC.alpha visibility <| GC.traced (linetype colour) <| GC.path <| points

geom_tangent : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_tangent aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       linetype = lookup .linetype aes' defaults
       linethickness = lookup .linethickness aes' defaults
       colour = lookup .colour aes' defaults
       visibility = lookup .visibility aes' defaults
       pointsize = lookup .pointsize aes' defaults
       translate = lookup .translate aes' defaults
       annotate = lookup .annotate aes' defaults
       precision = lookup .precision aes' defaults
       (w',h') = lookup .dims aes' defaults
       (w,h) = (toFloat w', toFloat h')
       delta = lookup .delta aes' defaults
       x = lookup .x aes' defaults

       tangent = C.tangent delta x d.f
       fun = (\x -> tangent.slope * x + tangent.intercept) 
       annotationPosition = (xm * d.xScale (fst translate),  
                             ym * d.yScale (snd translate))

       x1 = x - (delta / 2)
       x1' = xm * d.xScale x1
       y1 = fun x1
       y1' = ym * d.yScale y1
       x2 = x + (delta / 2)
       x2' = xm * d.xScale x2
       y2 = fun x2
       y2' = ym * d.yScale y2
       ys = [(x1', y1'),
             (x2', y2')]
       xpos = (xm * d.xScale x,ym * d.yScale (d.f x))
       x1pos = (x1',ym * d.yScale (d.f x1))
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
                  <| "&Delta;x = " ++ (toString <| C.dec precision delta) 
       annotation = if annotate then  
                       GC.move annotationPosition <| GC.group [symbol,label] 
                    else GC.toForm empty
   in
       GC.alpha visibility <| GC.group [GC.traced (GC.solid colour) <| GC.path ys,  
       x1dot,  
       vline1,
       xdot,  
       x2dot,  
       vline2,
       annotation
       ]
       

--makeTicks (from,to) =
--   let
--       below0 = [ceiling from..0]
--       above0 = [0..floor to]
--       positions = [ceiling from .. floor to]
--   in
--      []


yAxis : Aes -> Aes -> Distribution -> Dimensions -> Geom
yAxis aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       --axis = lookup .axis aes' defaults
       --axislabel = (snd << .labels) axis
       label = lookup .label aes' defaults
       tickspacing = lookup .tickspacing aes' defaults
       xmargin = fst dims.margins
       xmin = fst d.xDomain
       --ymin = fst d.yDomain
       (ymin,ymax) = d.yLimits
       --ymax = snd d.yDomain
       --ymax = snd d.yLimits
       pos = (xm * (d.xScale xmin) - (xmargin / 4),0)
       tickPositions = List.filter (\n -> (round n) % tickspacing == 0) 
          <| List.map toFloat [ceiling ymin .. floor ymax]
       tickLabels = GC.group <| List.map (\y -> GC.move (-xmargin / 8,ym * d.yScale y)  
         <| GC.toForm <| Text.rightAligned <| Text.fromString <| 
         toString <| C.dec 2 y) tickPositions
       ticks = GC.group <| List.map (\y -> GC.move (-xmargin * 0.02,ym * d.yScale y)  
         <| GC.traced (GC.solid black) <| GC.path [(0,0),(12,0)]) tickPositions
       yBar = GC.traced (GC.solid black) <| GC.path [(10,0),(10,ym)]
       yLabel = GC.move (-xmargin * 0.35, ym * 0.5) <|  
         GC.toForm <| Text.leftAligned <| Text.fromString label
   in
      GC.move pos <| GC.group  
      [tickLabels, ticks, yBar, yLabel]

xAxis : Aes -> Aes -> Distribution -> Dimensions -> Geom
xAxis aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       --axis = lookup .axis aes' defaults
       --axislabel = (fst << .labels) axis
       label = lookup .label aes' defaults
       tickspacing = lookup .tickspacing aes' defaults
       ymargin = snd dims.margins
       ymin = fst d.yLimits
       (xmin,xmax) = d.xDomain
       pos = (0,ym * (d.yScale ymin) - (ymargin / 12))
       --tickPositions = [xmin, 0, xmax]
       tickPositions = List.filter (\n -> (round n) % tickspacing == 0) 
          <| List.map toFloat [ceiling xmin .. floor xmax]
       tickLabels = GC.group <| List.map (\x -> GC.move (xm * d.xScale x,-ymargin * 0.3)  
         <| GC.toForm <| Text.centered <| Text.fromString <|  
         toString <| C.dec 2 x) tickPositions
       ticks = GC.group <| List.map (\x -> GC.move (xm * d.xScale x, -ymargin * 0.2)  
         <| GC.traced (GC.solid black) <| GC.path [(0,0),(0,12)]) tickPositions
       xBar = GC.traced (GC.solid black) <| GC.path [(0,-11),(xm,-11)]
       xLabel = GC.move (xm * 0.5,-ymargin * 0.55) <|  
         GC.toForm <| Text.centered <| Text.fromString label
   in
      GC.move pos <| GC.group  
      [tickLabels, ticks, xBar, xLabel]

--
title : Aes -> Aes -> Distribution -> Dimensions -> Geom
title aes' defaults d dims = 
   let
       label = lookup .label aes' defaults
       ymargin = fst dims.margins
       xm = dims.xm
       ym = dims.ym
       --ymax = snd d.yDomain
       ymax = snd d.yLimits
       midx = ((fst d.xDomain) + (snd d.xDomain)) / 2
       pos = (xm * (d.xScale midx),ym * (d.yScale ymax) + (ymargin / 4))
       --pos = (0,0)
       ttl = GC.toForm <| Text.centered <| Text.height 22 <| Text.fromString <| label
   in
      GC.move pos <| ttl


legend : Aes -> Aes -> Distribution -> Dimensions -> Geom
legend aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       (w',h') = lookup .dims aes' defaults
       (w,h) = (toFloat w', toFloat h')

       label' = lookup .label aes' defaults
       --label = lookup .label aes' defaults
       colour = lookup .colour aes' defaults
       translate = lookup .translate aes' defaults
       annotationPosition = (xm * d.xScale (fst translate),  
                             ym * d.yScale (snd translate))
       symbol = GC.move (w * -2,0) <| GC.filled colour <| GC.rect w h
       label = GC.toForm <| Text.leftAligned <| Text.fromString label'
   in
       GC.move annotationPosition <| GC.group [symbol, label]


geom_doge : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_doge aes' defaults d dims = 
   let
      wh = lookup .dims aes' defaults
      dogeurl = "https://31.media.tumblr.com/b23ef59e7838d323c281de41a31d672a/tumblr_inline_n0rlcbgFBe1s0subn.gif"
      dogeimg = image (fst wh) (snd wh) dogeurl
      xm = dims.xm
      ym = dims.ym
      x = lookup .x aes' defaults
      y = lookup .y aes' defaults
   in
      GC.move (xm * d.xScale x, ym * d.yScale y) <| GC.toForm dogeimg

geom_mario : Aes -> Aes -> Distribution -> Dimensions -> Geom
geom_mario aes' defaults d dims = 
   let
      wh = lookup .dims aes' defaults
      mariourl = "http://elm-lang.org/imgs/mario/walk/right.gif"
      marioimg = image (fst wh) (snd wh) mariourl
      xm = dims.xm
      ym = dims.ym
      --(xpos,ypos) = dims.pos
      x = lookup .x aes' defaults
      y = lookup .y aes' defaults
      yoffset = 0.4 * toFloat (snd wh)
   in
      GC.move (xm * d.xScale x, yoffset + ym * d.yScale y) <| GC.toForm marioimg



plot (plotWidth,plotHeight) d geoms m w =  
   let
      input = { x = (fst m), y = (snd m), width = (fst w), height = (snd w) }

       --Pass two "aesthetics" to each geom, where a custom one can override defaults
      defaults = { aesDefault | delta <- Just d.dx,  
                                limits <- Just d.xDomain,
                                x <- Just xpos,  
                                y <- Just ypos, 
                                theta <- Just (pi / 4)
                                --input <- Just input,
                             }
      windowScaleX = C.normalize (0,toFloat input.width)    --- 
      windowScaleY = C.normalize (0,toFloat input.height)   ---
      nbins = round <| (toFloat input.x) / 4   --- 
      wpos = (windowScaleX (toFloat input.x), windowScaleY (toFloat input.y))   --- 
      xpos = (fst d.xDomain) + d.xExtent * (fst wpos)
      ypos = if d.discrete then (d.f << (toFloat << round)) xpos else d.f xpos
      xmargin = (toFloat plotWidth) * 0.1
      ymargin = (toFloat plotHeight) * 0.2
      xoffset = (toFloat plotWidth)/2 - xmargin
      yoffset = (toFloat plotHeight)/2 - ymargin
      innerWidth = (toFloat plotWidth) - xmargin
      innerHeight = (toFloat plotHeight) - ymargin
      ymultiplier = (toFloat plotHeight) - (2 * ymargin)
      xmultiplier = (toFloat plotWidth) - (2 * xmargin)
      --toX = (\x -> (fst d.xDomain) + d.xExtent * (d.xScale x))
      --toY = (\y -> (fst d.yDomain) + d.yExtent * (d.yScale y))

      dims : Dimensions
      dims = {
         windowScaleX = windowScaleX,
         windowScaleY = windowScaleY,
         nbins = nbins,
         wpos = wpos,
         --pos = (xpos,ypos),
         xmargin = xmargin,
         ymargin = ymargin,
         margins = (xmargin,ymargin),
         innerWidth = innerWidth,
         innerHeight = innerHeight,
         ym = ymultiplier,
         xm = xmultiplier
         --toX = toX,
         --toY = toY
      }

      --zeroX = geom_hline customAes defaults d dims 0
      --zeroY = geom_vline customAes defaults d dims 0
      offsets = (-xoffset, -yoffset)
   in 
      GC.collage plotWidth plotHeight  
            [GC.move offsets <| GC.group <| List.map (\g -> g defaults d dims) geoms]
            --[GC.move offsets <| GC.group <| List.map (\(g,a) -> g a defaults d dims) geoms]
             -- [  
             -- --GC.move offsets <| xMarker,
             -- --GC.move offsets <| yMarker,
             -- GC.move offsets <| annotate_integral annotationAes defaults d dims,
             -- ]


background : Aes -> Aes -> Distribution -> Dimensions -> Geom
background aes' defaults d dims = 
   let
       colour = lookup .colour aes' defaults
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

annotate_integral : Aes -> Aes -> Distribution -> Dimensions -> Geom
annotate_integral aes' defaults d dims = 
   let
       precision = lookup .precision aes' defaults
       translate = lookup .translate aes' defaults
       limits = lookup .limits aes' defaults
       --baseline = lookup .fun aes' defaults -- !!
       xmin = fst limits
       xmax = snd limits
       --integral = C.integrate limits (toFloat dims.nbins) d.f
       integral = C.integrate limits (toFloat d.steps) d.f
       --xmin = fst d.xDomain
       --xmax = snd d.xDomain
       --integral = C.integrate d.xDomain (toFloat dims.nbins) d.f
   in
       GC.move (dims.xm * fst translate,  
                dims.ym * snd translate)
       --GC.move (dims.xm * (d.xScale (fst translate)),  
       --         dims.ym * (d.yScale (snd translate)))
              <| GC.toForm <| Text.rightAligned <| Text.fromString  
              -- <| "number of bins: " ++ (toString dims.nbins) 
              <| "\n&#x222b;"  
                     ++ " from " ++ (toString <| C.dec precision xmin)  
                     ++ " to " ++ (toString <| C.dec precision xmax)  
                     ++ " &#8776; " ++ (toString <| C.dec precision <| integral)


{-- 
TODO: 
rename xscale/yscale --> fromX/fromY
move geoms to separate file
Set y plotting limits manually, for layering purposes.
Colouring positive and negative integrals independently?
Input time?
Implement grid...  
Linestyles and textstyles?! 
Improve axes: number of ticks etc. 
Annotation for vline and hline
geom_abline_polar?
finish geom_vlinerange
Polar plotting gets messed up when the origin is far from 0... 
--}



{- aesthetics -}

lookup key child parent = let
                            childValue = key child
                            parentValue = key parent
                          in
                            case (childValue,parentValue) of
                                (Just c,_) -> c
                                (Nothing, Just p) -> p

type alias Dimensions = {
         windowScaleX : (Float -> Float),
         windowScaleY : (Float -> Float),
         nbins : Int,
         wpos : (Float,Float),
         --pos : (Float,Float),
         xmargin : Float,
         ymargin : Float,
         margins : (Float,Float),
         innerWidth : Float,
         innerHeight : Float,
         ym : Float,
         xm : Float
         --toX : (Float -> Float),
         --toY : (Float -> Float)
}
            
type alias Aes = { visibility:Maybe Float, 
                   linetype:Maybe (Color -> GC.LineStyle),
                   linethickness:Maybe Float,
                   pointsize:Maybe Float,
                   colour:Maybe Color,  
                   translate:Maybe (Float,Float),
                   precision: Maybe Float,
                   dims: Maybe (Int,Int),
                   label: Maybe String,
                   delta: Maybe Float,
                   limits: Maybe (Float,Float),
                   annotate: Maybe Bool,
                   x: Maybe Float,
                   y: Maybe Float,
                   radius: Maybe Float,
                   theta: Maybe Float,
                   fun: Maybe (Float -> Float),
                   dynamic: Maybe Bool,
                   negate: Maybe Bool,
                   --axis: Maybe { labels : (String,String) },
                   --axis: Maybe Axis,
                   fit: Maybe Bool,
                   rotate: Maybe Bool,
                   tickspacing: Maybe Int
                   --input: Maybe { height : Int, width : Int, x : Int, y : Int }
                }

aes : Aes
aes = { visibility = Nothing, 
        linetype = Nothing,  
        linethickness = Nothing,  
        pointsize = Nothing,  
        colour = Nothing,
        translate = Nothing,
        precision = Nothing,
        dims = Nothing,
        label = Nothing,
        delta = Nothing,
        limits = Nothing,
        annotate = Nothing,
        x = Nothing, 
        y = Nothing,
        radius = Nothing,
        theta = Nothing,
        fun = Nothing,
        dynamic = Nothing,
        negate = Nothing,
        --axis = Nothing,
        fit = Nothing,
        rotate = Nothing,
        tickspacing = Nothing
        --input = Nothing
     }


aesDefault : Aes
aesDefault = { visibility = Just 1, 
               linetype = Just GC.solid,  
               linethickness = Just 1,  
               pointsize = Just 4.0,  
               colour = Just black,
               translate = Just (0,0),
               precision = Just 2,
               dims = Just (30,3),
               label = Just "...",
               delta = Just 0.1,
               limits = Just (-1,1),
               annotate = Just True, 
               x = Just 0,
               y = Just 0,
               radius = Just 1,
               theta = Just 0,
               fun = Just (\x -> 0),
               dynamic = Just True,
               negate = Just False,
               --axis = Just { labels = ("X", "Y"), nticks = 5 },
               --axis = Just (axis ("X","Y") 5),
               fit = Just True,
               rotate = Just False,
               --input = Nothing
               tickspacing = Just 5 
               }

roundf = toFloat << round

--type alias Axis = {
--   labels : (String,String),
--   nticks : Int 
--}

--axis : (String,String) -> Int -> Axis
--axis labels nticks = { labels = labels, nticks = nticks }
