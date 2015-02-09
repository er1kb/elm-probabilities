module Plot (Graph, continuous, discrete, fromPairs, geom_vlineranges, geom_step, geom_text, geom_point, geom_points, geom_bar, geom_trapezoid, geom_area, geom_curve, geom_hlinerange, geom_vlinerange, geom_hline, geom_vline, geom_hline_polar, geom_vline_polar, geom_circle, geom_angle, geom_curve_polar, geom_abline_polar, geom_area_polar, geom_position_polar, geom_trace_polar, geom_trace, geom_abline, derivative, yAxis, xAxis, title, legend, geom_image, plot, background, annotate_integral, Aes, aes, aesDefault, point, bar, trapezoid, lookup, Dimensions, Geom) where


import Statistics.Discrete as D
import Statistics.Continuous as C
import Graphics.Collage (toForm, alpha, traced, path, move, filled, circle, group, outlined, polygon, dotted, rect, solid, rotate, LineStyle, Form, collage)
import Graphics.Element (Element, empty, image, flow, down, right, layers)
import Color (Color, lightGrey, black)
import Text (plainText, fromString, color, height, leftAligned, centered, rightAligned)
import List (map, map2, concatMap, filter, head, tail, reverse, minimum, maximum, length)

{-| Functions for calculating values of common continuous probability distributions. 

# Plot a graph
@docs plot, Graph, continuous, discrete, fromPairs

# Aesthetics
@docs aes, aesDefault

# Standard charting elements
@docs background, title, xAxis, yAxis

# Geoms
Visual mappings.
@docs Geom

# Layers
More problem specific features that may include different kinds of visual mappings.
@docs derivative

# Annotations
@docs legend, annotate_integral

-}

--point, bar, trapezoid,  
--lookup, Dimensions, Geom



{-| Geoms are visual representations of data. 

The Geom type is a synonym for the Form type in Graphics.Collage. All geom constructors have the same type signature, e.g. 
geom_point : Aes -> Aes -> Graph -> Dimensions -> Geom

All of these arguments except the first are invoked through the main plotting function, meaning you do not interface with a geom directly this way. Instead, the syntax for calling one is:
geom_point aes
...where aes is a record constructor for an aesthetic object (Aes type). Aesthetics handle all customization of the visual mapping, with respect to position, colour, labelling and so on. See separate documentation. 

For now these geoms are available, but the list is subject to change. Some of them have counterparts for polar plotting. Depending on which features that turn out to be practically useful, geoms may be merged, split, improved, added or removed. There is no point in writing help sections for each geom individually but I will get around to summarizing which geom uses which aesthetic attributes. 

Cartesian - (x,y) coordinates
geom_abline 
geom_area 
geom_bar 
geom_circle 
geom_curve 
geom_hline 
geom_hlinerange 
geom_image   
geom_point 
geom_points 
geom_step 
geom_text 
geom_trace
geom_trapezoid 
geom_vline 
geom_vlinerange 
geom_vlineranges 

Polar - (r,theta) coordinates
geom_angle 
geom_area_polar 
geom_curve_polar 
geom_hline_polar 
geom_position_polar 
geom_trace_polar 
geom_vline_polar 

-}
type alias Geom = Form

--geom_none : Aes -> Aes -> Graph -> Dimensions -> Geom
--geom_none aes' defaults d dims = 
--   let
--      x = lookup .x aes' defaults
--   in
--      toForm <| plainText <| toString x

{-| The identity function. Input and output are identical. -}
id : a -> a
id = (\x -> x)

roundf = toFloat << round

mkPolar xs ys = map fromPolar <| map2 (,) ys xs

spin points x = map (\(px,py) -> (((0.5 * py * cos x) - (0.5 * px * sin x)) + x, ((py * sin x) + (px * cos x)))) points



{-| A graph is the abstract representation of a function applied over a certain interval of x values. It contains the x and y limits of the function as well as functions for normalizing input. The graph is one of a couple of things you feed into the plotting function.  

type alias Graph = {
         discrete: Bool,            -- discrete or continuous distribution?
         f: (Float -> Float),       -- the mathematical function
         xs: List Float,            -- x values
         ys: List Float,            -- y values 
         polar: List (Float,Float), -- (x,y) values for polar plotting
         pairs: List (Float,Float), -- (x,y) pairs (data points supplied by the used)
         xDomain: (Float,Float),    -- minimum and maximum on the x axis 
         yDomain: (Float,Float),    -- minimum and maximum on the y axis 
         xExtent: Float,            -- size of the domain (x)
         rmax:Float,                -- maximum radius (either positive or negative y)
         steps:Int,                 -- number of steps to evaluate the function
         dx:Float,                  -- distance in x between evaluation points
         xScale:(Float -> Float),   -- function for normalizing x values
         yScale:(Float -> Float),   -- function for normalizing y values
         yExtent:Float,             -- size of the range/co-domain (y) 
         xLimits:(Float,Float),     -- manual x limits for plot
         yLimits:(Float,Float),     -- manual y limits for plot
         xyRatio:Float,             -- hack for mixing polar and cartesian coordinates
         pOffset:Float,             -- for polar plots that are not symmetric at x=0
         toX:(Float -> Float),      -- xScale reversed: normalized values into x
         toY:(Float -> Float)       -- yScale reversed: normalized values into y
      }


-}
type alias Graph = {
         discrete: Bool,
         f: (Float -> Float),
         xs: List Float,
         ys: List Float, 
         polar:List (Float,Float),
         pairs: List (Float,Float),
         xDomain: (Float,Float), 
         yDomain: (Float,Float),
         xExtent: Float,
         rmax:Float,
         steps:Int,
         dx:Float,
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


{-| Constructs a continuous distribution. Suitable for trigonometric functions, continuous probability functions (e.g. normal, exponential), logaritms and exponentials, linear functions, and so on. You need to tell it which function to evaluate (f), the x and y limits, and the number of steps to evaluate the function.  

Let's dwell on the last argument (steps). Even though the function (f) is continuous, the representations and computations of it must be discrete. If you want to plot a sin function for 0 ≤ x ≤ 2pi, then 100 steps will draw a smooth curve. For plotting over a larger domain and especially for squiggly lines (such as y = sin16x) you should at least double that number, but also consider the performance penalty of adding computations. This should to be tweaked by the user as it's also affected by the plot size. 

-}
continuous : (Float -> Float) -> (Float, Float) -> (Float, Float) -> Int -> Graph
continuous f (xmin,xmax) (ymin,ymax) steps = 
   let
       xDomain = (xmin, xmax)
       xExtent = (xmax - xmin)
       interpolator = map (C.normalize (0,toFloat steps)) [0..toFloat steps] 
       xs = map (\x -> xmin + xExtent * x) interpolator
       ys = map f xs
       dx = xExtent / (toFloat steps)
       xScale = C.normalize xDomain
       --ymin = minimum ys
       --ymax = maximum ys
       rmax = if (abs ymax) > (abs ymin) then (abs ymax) else (abs ymin)
       --yLimits = (-rmax, rmax)
       --yLimits = (ymin, ymax)
       yLimits = (minimum ys, maximum ys)
       --xLimits = yLimits
       xLimits = xDomain
       --yScale = C.normalize yLimits
       yDomain = (ymin, ymax)
       yScale = C.normalize yDomain
       yExtent = ymax - ymin
       xyRatio = rmax / (xExtent / 2)
       sign = (xmin / xmin) * (xmax / xmax)
       pOffset = (xmax + xmin) / 2
       --polar = map fromPolar <| map2 (,) ys xs
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
                           pairs = [],
                           xDomain = xDomain, 
                           yDomain = yDomain,
                           xExtent = xExtent,
                           rmax = rmax,
                           steps = steps,
                           dx = dx,
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


{-| Constructs a discrete graph. Use this for discrete probability distributions such as the binomial, Poisson and hypergeometric distributions. There are other discrete distributions such as the Fibonacci series, which may or may not lend themselves to plotting. 

-}
discrete : (Float -> Float) -> (Float, Float) -> (Float, Float) -> Graph
discrete f (xmin',xmax') (ymin,ymax) = 
   let
       xmin = xmin' - 0.5
       xmax = xmax' + 0.5
       xDomain = (xmin, xmax)
       xExtent = (xmax - xmin)
       xs = [xmin' .. xmax']
       ys = map f xs
       steps = length xs
       dx = 1
       xScale = C.normalize xDomain
       --ymin = minimum ys
       --ymax = maximum ys
       rmax = if (abs ymax) > (abs ymin) then (abs ymax) else (abs ymin)
       --yLimits = (-rmax, rmax)
       --yLimits = (ymin, ymax)
       yLimits = (minimum ys, maximum ys)
       --xLimits = yLimits
       xLimits = (xmin', xmax')
       --yScale = C.normalize yLimits
       yDomain = (ymin, ymax)
       yScale = C.normalize yDomain
       yExtent = ymax - ymin
       xyRatio = rmax / (snd xDomain)
       skew = (xmax - xmin)
       pOffset = skew / 2
       --polar = map fromPolar <| map2 (,) ys xs
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
                           pairs = [],
                           xDomain = xDomain, 
                           yDomain = yDomain,
                           xExtent = xExtent,
                           rmax = rmax,
                           steps = steps,
                           dx = dx,
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

{-| For when you have (x,y) data points. Suitable for doing linear regression. 

fromPairs : List (Float, Float) -> (Float, Float) -> (Float, Float) -> Graph
fromPairs pairs (xmin,xmax) (ymin,ymax)

In case you're unfamiliar with Elm lists, pairs can be constructed by:
List.map2 (,) xs ys
...where xs and ys are lists containing the x and y variables respectively.

-}
fromPairs : List (Float, Float) -> (Float, Float) -> (Float, Float) -> Graph
fromPairs pairs (xmin,xmax) (ymin,ymax) = 
   let
       xDomain = (xmin, xmax)
       xExtent = (xmax - xmin)
       steps = length pairs
       xs = map fst pairs
       ys = map snd pairs
       dx = 1
       xScale = C.normalize xDomain
       --ymin = minimum ys
       --ymax = maximum ys
       rmax = if (abs ymax) > (abs ymin) then (abs ymax) else (abs ymin)
       --yLimits = (-rmax, rmax)
       --yLimits = (ymin, ymax)
       yLimits = (minimum ys, maximum ys)
       --xLimits = yLimits
       xLimits = xDomain
       --yScale = C.normalize yLimits
       yDomain = (ymin, ymax)
       yScale = C.normalize yDomain
       yExtent = ymax - ymin
       xyRatio = rmax / (xExtent / 2)
       sign = (xmin / xmin) * (xmax / xmax)
       pOffset = (xmax + xmin) / 2
       --polar = map fromPolar <| map2 (,) ys xs
       polar = mkPolar xs ys
       toX = (\x -> (fst xDomain) + xExtent * (xScale x))
       toY = (\y -> (fst yDomain) + yExtent * (yScale y))
   in
                        {  
                           discrete = False,
                           f = id, 
                           xs = xs,
                           ys = ys,
                           polar = polar,
                           pairs = pairs,
                           xDomain = xDomain, 
                           yDomain = yDomain,
                           xExtent = xExtent,
                           rmax = rmax,
                           steps = steps,
                           dx = dx,
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


geom_vlineranges : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_vlineranges aes' defaults d dims =
   let
      --x = lookup .x aes' defaults
      xm = dims.xm
      ym = dims.ym
      (ymin,ymax) = d.yDomain
      fun = lookup .fun aes' defaults
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      --annotate = lookup .annotate aes' defaults
      --label = lookup .label aes' defaults
      --precision = lookup .precision aes' defaults
      --translate = lookup .translate aes' defaults
      --txt' = move (xm * d.xScale (x + (fst translate)), ym * d.yScale ((snd limits) + snd translate)) <| toForm <| plainText label
      --txt = if annotate then txt' else (toForm empty)
      lineranges = map (\(x,y) -> alpha visibility <| traced (linetype colour)  
      <| path [(xm * d.xScale x, ym * d.yScale y), (xm * d.xScale x, ym * d.yScale (fun x))]) d.pairs
   in
      group lineranges



geom_step : Aes -> Aes -> Graph -> Dimensions -> Geom
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
      nsteps' = lookup .nsteps aes' defaults
      nsteps = if dynamic then (toFloat nsteps') else (toFloat d.steps) 
      --nsteps = if dynamic then (toFloat dims.nbins) else (toFloat d.steps) 
      (dx,steps') = if d.discrete then (1,d.xs) else (C.interpolate d.xDomain nsteps)
      lastx = head <| reverse steps'
      points = concatMap (\x -> [(x, d.f x),(if (x < lastx) then x+dx else x, d.f x)]) steps'

      ys = map (\(x,y) -> (xm * (d.xScale x), ym * (d.yScale y))) points 
                                 -- <| map2 (,) steps' (map d.f steps)
   in
      alpha visibility <| traced (linetype colour) <| path ys


geom_text : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_text aes' defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      colour = lookup .colour aes' defaults
      label = lookup .label aes' defaults
      translate = lookup .translate aes' defaults
      precision = lookup .precision aes' defaults
      pointsize = lookup .pointsize aes' defaults
      visibility = lookup .visibility aes' defaults
      x = lookup .x aes' defaults
      y = lookup .y aes' defaults
      pos = (xm * d.xScale x, ym * d.yScale y)
      annotationPosition = (xm * (d.xScale x) + (fst translate),  
                            ym * (d.yScale y) + (snd translate))
      txt = label ++ (toString <| C.dec precision x) ++ ", " ++ (toString <| C.dec precision y)
      label' = move annotationPosition <| toForm <| rightAligned <| color colour 
              <| fromString <| txt
   in
      label'




geom_point : Aes -> Aes -> Graph -> Dimensions -> Geom
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
      y = lookup .y aes' defaults
      --y = d.f x
      --pos = point (xm,ym) (d.xScale, d.yScale) d.f x
      pos = (xm * d.xScale x, ym * d.yScale y)
   in
      move pos <| alpha visibility <| filled colour <| circle pointsize



point (xm,ym) (xscale, yscale) f x =  
      (xm * (xscale x), ym * (yscale <| f x)) 

geom_points : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_points aes' defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      colour = lookup .colour aes' defaults
      pointsize = lookup .pointsize aes' defaults
      visibility = lookup .visibility aes' defaults
      dynamic = lookup .dynamic aes' defaults
      nsteps' = lookup .nsteps aes' defaults
      nsteps = if dynamic then (toFloat nsteps') else (toFloat d.steps) 
      --nsteps = if dynamic then (toFloat dims.nbins) else (toFloat d.steps) 
      (dx,steps) = if d.discrete then (1,d.xs) else (C.interpolate d.xDomain nsteps)
      points' = map (point (xm,ym) (d.xScale,d.yScale) d.f) steps
      points = if (length d.pairs > 0)
                  then map (\(x,y) -> (xm * d.xScale x, ym * d.yScale y)) d.pairs  
                  else points'
   in
      alpha visibility <| group <| map (\(x,y) -> move (x,y) <| filled colour <| circle pointsize) points

bar (xm,ym) (xscale, yscale) limits f (x1',x2') =  
   let
       lower = fst limits
       upper = snd limits
       x1 = if (x1' < lower) then lower else x1'
       x2 = if (x2' > upper) then upper else x2'
       x3 = (x1'+x2')/2
       midpoint = f x3
   in
      map (\(x,y) -> (xm * (xscale x), ym * (yscale y))) 
               [(x1,0), (x2,0), (x2,midpoint), (x1,midpoint)]


-- keep one separate "bar chart" for discrete distributions?!
geom_bar : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_bar aes' defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      limits' = lookup .limits aes' defaults
      --limits = if d.discrete then d.xDomain else limits'
      limits = if d.discrete then d.xLimits else limits'
      dynamic = lookup .dynamic aes' defaults
      nsteps' = lookup .nsteps aes' defaults
      nsteps = if dynamic then (toFloat nsteps') else (toFloat d.steps) 
      --nsteps = if dynamic then (toFloat dims.nbins) else (toFloat d.steps) 
      (dx',steps) = if d.discrete then (1,d.xs) else (C.interpolate d.xDomain nsteps)
      dx = dx' / 2
      --xs = C.bins <| map (\x -> x - dx) steps
      lowerbar = [((fst limits) - dx')]
      upperbar = [((snd limits) + dx')]
      --xs = C.bins <| map (\x -> x - dx) (lowerbar ++ steps ++ upperbar)
      xs = C.bins <| map (\x -> x - dx) (steps ++ upperbar)
      points = map (bar (xm,ym) (d.xScale,d.yScale) limits d.f) xs 
   in
      alpha visibility <| group <| map (\x -> outlined (linetype colour) <| polygon x) points



-- integral approximation by bars/rectangles
--geom_bar : Aes -> Aes -> Graph -> Dimensions -> Geom
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
--      --xs = C.bins <| map (\x -> x - dx) steps
--      lowerbar = [((fst limits) - dx')]
--      upperbar = [((snd limits) + dx')]
--      --xs = C.bins <| map (\x -> x - dx) (lowerbar ++ steps ++ upperbar)
--      xs = C.bins <| map (\x -> x - dx) (steps ++ upperbar)
--      --points = map (bar (xm,ym) (xscale,yscale) d.f) xs 
--      points = map (bar (xm,ym) (xscale,yscale) limits d.f) xs 
--   in
--      alpha visibility <| group <| map (\x -> outlined (linetype colour) <| polygon x) points


{-| Calculating trapezium points for plotting them as polygons. -}
trapezoid (xm,ym) (xscale, yscale) f baseline (x1,x2) =  
   map (\(x,y) -> (xm * (xscale x), ym * (yscale y))) 
            [(x1,baseline x1), (x2,baseline x2), (x2,f x2), (x1,f x1)]

geom_trapezoid : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_trapezoid aes' defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      baseline = lookup .fun aes' defaults
      limits = lookup .limits aes' defaults
      nsteps = lookup .nsteps aes' defaults
      (dx,steps) = C.interpolate limits (toFloat nsteps)
      --(dx,steps) = C.interpolate limits (toFloat dims.nbins)
      xs = C.bins steps
      points = map (trapezoid (xm,ym) (d.xScale,d.yScale) d.f baseline) xs 
   in
      alpha visibility <| group <| map (\x -> outlined (linetype colour) <| polygon x) points


geom_area : Aes -> Aes -> Graph -> Dimensions -> Geom
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
      xs = map (\x -> (x,baseline x)) steps
      rev_steps = reverse steps
      f = if d.discrete then (d.f << roundf) else d.f
      ys = map f rev_steps
      --area = xs
      points = xs ++ (map2 (,) rev_steps ys)
      area = map (\(x,y) -> (xm * (d.xScale x), ym * (d.yScale y))) points 
      --area = map (\(x,y) -> (dims.xm * (d.xScale x), dims.ym * (d.yScale y))) <| map2 (,) steps (map d.f steps)
   in
      alpha visibility <| filled colour <| polygon area

geom_curve : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_curve aes' defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      linethickness = lookup .linethickness aes' defaults -- not actually used yet
      --(dx,steps) = C.interpolate d.xDomain (toFloat d.steps)
      (dx,steps) = if d.discrete then (1,d.xs) else (C.interpolate d.xDomain (toFloat d.steps))
      ys = map (\(x,y) -> (xm * (d.xScale x), ym * (d.yScale y))) <| map2 (,) steps (map d.f steps)
   in
      alpha visibility <| traced (linetype colour) <| path ys



geom_vlinerange : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_vlinerange aes' defaults d dims =
   let
      x = lookup .x aes' defaults
      xm = dims.xm
      ym = dims.ym
      (ymin,ymax) = d.yDomain
      limits = lookup .limits aes' defaults
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      annotate = lookup .annotate aes' defaults
      label = lookup .label aes' defaults
      precision = lookup .precision aes' defaults
      translate = lookup .translate aes' defaults
      txt' = move (xm * d.xScale (x + (fst translate)), ym * d.yScale ((snd limits) + snd translate)) <| toForm <| plainText label
      txt = if annotate then txt' else (toForm empty)
      line = alpha visibility <| traced (linetype colour)  
      <| path [(xm * d.xScale x, ym * d.yScale (fst limits)), (xm * d.xScale x, ym * d.yScale (snd limits))] 
   in
      group [txt,line]


geom_hlinerange : Aes -> Aes -> Graph -> Dimensions -> Geom
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
      txt' = move (xm * d.xScale ((snd limits) + (fst translate)), ym * d.yScale (y + snd translate)) <| toForm <| plainText label
      txt = if annotate then txt' else (toForm empty)
      line = alpha visibility <| traced (linetype colour)  
      <| path [(xm * d.xScale (fst limits), ym * (d.yScale y)), (xm * d.xScale (snd limits), ym * (d.yScale y))] 
   in
      group [txt,line]

geom_hline : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_hline aes' defaults d dims =
   let
      y = lookup .y aes' defaults
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
      --txt' = move (xm * d.xScale (xmax + fst translate), ym * d.yScale (y + snd translate)) <| toForm <| plainText <| label ++ (toString (C.dec precision <| d.toY y)) 
      txt' = move (xm * d.xScale (xmax + fst translate), ym * d.yScale (y + snd translate)) <| toForm <| rightAligned <| color colour <| fromString <| label ++ (toString (C.dec precision <| d.toY y)) 
      txt = if annotate then txt' else (toForm empty)
      line = alpha visibility <| traced (linetype colour)  
      <| path [(0,ym * (d.yScale y)), (xm, ym * (d.yScale y))] 
   in
      group [txt,line]

       --label' = move annotationPosition <| toForm <| rightAligned <| color colour 
       --        <| fromString <| "&theta; &asymp; " ++ (toString <| C.dec 2 x)  
       --        ++ "\nr = " ++ (toString <| C.dec 2 (d.f x))


geom_vline : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_vline aes' defaults d dims =
   let
      x = lookup .x aes' defaults
      (ymin,ymax) = d.yDomain
      xm = dims.xm
      ym = dims.ym
      --limits = lookup .limits aes' defaults
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      annotate = lookup .annotate aes' defaults
      label = lookup .label aes' defaults
      precision = lookup .precision aes' defaults
      translate = lookup .translate aes' defaults
      --txt' = move (xm * d.xScale (x + fst translate), ym * d.yScale (ymax + snd translate)) <| toForm <| plainText <| label ++ (toString (C.dec precision <| d.toX x)) 
      txt' = move (xm * d.xScale (x + fst translate), ym * d.yScale (ymax + snd translate)) <| toForm <| rightAligned <| color colour <| fromString <| label ++ (toString (C.dec precision <| d.toX x)) 
      txt = if annotate then txt' else (toForm empty)
      line = alpha visibility <| traced (linetype colour)  
      <| path [(xm * (d.xScale x),0), (xm * (d.xScale x), ym)] 
   in
      group [txt,line]


geom_hline_polar : Aes -> Aes -> Graph -> Dimensions -> Geom
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
      --offset = if revolve then 0 else d.pOffset
      points = map fromPolar  
         <| map (\x -> (y,x)) d.xs
      hline = path  
         <| map (\(x,y) -> (xm * d.xScale (d.pOffset + (x / xyRatio)), ym * d.yScale y)) 
         points
   in
      alpha visibility <| traced (linetype colour) <| hline 

geom_vline_polar : Aes -> Aes -> Graph -> Dimensions -> Geom
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
      --offset = if revolve then 0 else d.pOffset
      points = map fromPolar  
         <| map (\y -> (y,x)) d.ys
      hline = path  
         <| map (\(x,y) -> (xm * d.xScale (d.pOffset + (x / xyRatio)), ym * d.yScale y)) 
         points
   in
      alpha visibility <| traced (linetype colour) <| hline 


geom_trace_polar : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_trace_polar aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       x = lookup .x aes' defaults
       --y = lookup .y aes' defaults
       linetype = lookup .linetype aes' defaults
       colour = lookup .colour aes' defaults
       visibility = lookup .visibility aes' defaults
       revolve = lookup .revolve aes' defaults
       fit' = lookup .fit aes' defaults
       fit = if revolve then False else fit'
       xyRatio = if fit then d.xyRatio else 1
       offset = if revolve then 0 else d.pOffset
       --origin = (0,0)
       (dx,steps) = C.interpolate (fst d.xDomain, x) (toFloat d.steps)
       --ys = map d.f steps
       --ys = map fromPolar <| map2 (,) (map d.f steps) steps
       ys = mkPolar steps (map d.f steps)

       points' = if revolve then spin ys x else ys
       points = map (\(x,y) -> (xm * d.xScale (offset + (x / xyRatio)), ym * d.yScale y)) points'
       --points = map (\(x,y) -> (xm * d.xScale (x / xyRatio), ym * d.yScale y)) <| map fromPolar <| map2 (,) ys steps 
   in
      --move origin <|  
      alpha visibility <| traced (linetype colour) <| path points

geom_circle : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_circle aes' defaults d dims =
   let
      xm = dims.xm
      ym = dims.ym
      x = lookup .x aes' defaults
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      --fit = lookup .fit aes' defaults
      revolve = lookup .revolve aes' defaults
      fit' = lookup .fit aes' defaults
      fit = if revolve then False else fit'
      xyRatio = if fit then d.xyRatio else 1
      offset = if revolve then 0 else d.pOffset
      ys = map (fromPolar << (\x -> (d.rmax,x))) d.xs
      points' = if revolve then spin ys x else ys

      points = map (\(x,y) -> (xm * d.xScale (offset + (x / xyRatio)), ym * d.yScale y)) points'

      thing = path points
      origin = (0,0)
   in
      move origin <| alpha visibility <| outlined (linetype colour) <| thing



geom_angle : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_angle aes' defaults d dims =
   let
       xm = dims.xm
       ym = dims.ym
       linetype = lookup .linetype aes' defaults
       colour = lookup .colour aes' defaults
       pointsize = lookup .pointsize aes' defaults
       revolve = lookup .revolve aes' defaults
       fit' = lookup .fit aes' defaults
       fit = if revolve then False else fit'
       xyRatio = if fit then d.xyRatio else 1
       offset = if revolve then 0 else d.pOffset
       radius = d.yExtent / 2
       x = lookup .x aes' defaults
       annotate = lookup .annotate aes' defaults
       translate = lookup .translate aes' defaults
       point' = [fromPolar (d.rmax, x)] 
       point = if revolve then spin point' x else point'
       (x',y') = head point

       pos = (xm * d.xScale (offset + (x' / xyRatio)), ym * d.yScale y')
       annotationPosition = (xm * d.xScale (d.pOffset + fst translate),  
                             ym * d.yScale (snd translate))
       dot = move pos  
               <| filled colour <| circle pointsize
       label' = move annotationPosition <| toForm <| rightAligned <| color colour 
               <| fromString <| "&theta; &asymp; " ++ (toString <| C.dec 2 x)  
               ++ "\nr = " ++ (toString <| C.dec 2 (d.f x))
       label = if annotate then label' else (toForm empty)
   in
      group [dot, label]

geom_curve_polar : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_curve_polar aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       x = lookup .x aes' defaults
       linetype = lookup .linetype aes' defaults
       colour = lookup .colour aes' defaults
       visibility = lookup .visibility aes' defaults
       revolve = lookup .revolve aes' defaults
       fit' = lookup .fit aes' defaults
       fit = if revolve then False else fit'
       xyRatio = if fit then d.xyRatio else 1
       offset = if revolve then 0 else d.pOffset
       origin = lookup .translate aes' defaults
       points' = if revolve then spin d.polar x else d.polar
       points = map (\(x,y) -> (xm * d.xScale (offset + (x / xyRatio)), ym * d.yScale y)) points'
   in
      move origin <| alpha visibility <| traced (linetype colour) <| path points


geom_abline_polar : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_abline_polar aes' defaults d dims = 
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

       revolve = lookup .revolve aes' defaults
       fit' = lookup .fit aes' defaults
       fit = if revolve then False else fit'
       xyRatio = if fit then d.xyRatio else 1
       offset = if revolve then 0 else d.pOffset

       x' = if x < (fst limits) then (fst limits) else x
       x'' = if x' > (snd limits) then (snd limits) else x'


       limits' = if negate then (x'', snd limits) else (fst limits, x'')
       (x1,x2) = if dynamic then limits' else limits

       (dx,steps) = C.interpolate (x1, x2) (toFloat d.steps)
       ys = mkPolar steps (map fun steps)
       points' = if revolve then spin ys x else ys
       points = map (\(x,y) -> (xm * d.xScale (offset + (x / xyRatio)), ym * d.yScale y)) points'
       --points = [(xm * d.xScale x1, ym * d.yScale (fun x1)), 
       --          (xm * d.xScale x2, ym * d.yScale (fun x2))] 
   in
       alpha visibility <| traced (linetype colour) <| path <| points


geom_area_polar : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_area_polar aes' defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      limits = lookup .limits aes' defaults
      revolve = lookup .revolve aes' defaults
      fit' = lookup .fit aes' defaults
      fit = if revolve then False else fit'
      xyRatio = if fit then d.xyRatio else 1
      offset = if revolve then 0 else d.pOffset
      baseline = lookup .fun aes' defaults

      dynamic = lookup .dynamic aes' defaults
      negate = lookup .negate aes' defaults

      x = lookup .x aes' defaults
      x' = if x < (fst limits) then (fst limits) else x
      x'' = if x' > (snd limits) then (snd limits) else x'

      limits' = if negate then (x'', snd limits) else (fst limits, x'')

      (dx,steps) = C.interpolate (if dynamic then limits' else limits) (toFloat d.steps)

      origin = (0,0)
      ys' = map d.f steps

      f_points = [origin] ++ (map2 (,) ys' steps) 

      baseline_ys = map baseline steps 
      baseline_points = reverse <| [origin] ++ (map2 (,) baseline_ys steps) 

      points' = map fromPolar (f_points ++ baseline_points)
      points = if revolve then spin points' x else points'

      area = map (\(x,y) -> (xm * d.xScale (offset + (x / xyRatio)), ym * d.yScale y)) points 
   in
      move origin <| alpha visibility <| filled colour <| polygon area



geom_position_polar : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_position_polar aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       x' = lookup .x aes' defaults
       y' = lookup .y aes' defaults
       colour = lookup .colour aes' defaults
       visibility = lookup .visibility aes' defaults
       pointsize = lookup .pointsize aes' defaults
       fit = lookup .fit aes' defaults
       xyRatio = if fit then d.xyRatio else 1
       --offset = if revolve then 0 else d.pOffset
       origin = (0,0)
       pos = (\(x,y) -> (xm * d.xScale (d.pOffset + (x / xyRatio)), ym * d.yScale y)) <| fromPolar (y',x')

       point = move pos  
               <| move origin <| alpha visibility 
               <| filled colour <| circle pointsize
   in
      point



geom_trace : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_trace aes' defaults d dims = 
   let
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes' defaults
      colour = lookup .colour aes' defaults
      visibility = lookup .visibility aes' defaults
      dynamic = lookup .dynamic aes' defaults
      negate = lookup .negate aes' defaults
      x = lookup .x aes' defaults
      limits = lookup .limits aes' defaults
      limits' = if negate then (x, snd limits) else (fst limits, x)
      --(dx,steps) = C.interpolate (fst d.xDomain,x) (toFloat d.steps)
      (dx,steps) = C.interpolate (if dynamic then limits' else limits) (toFloat d.steps)
      ys = map (\(x,y) -> (xm * (d.xScale x), ym * (d.yScale y)))  
                     <| map2 (,) steps (map d.f steps)
   in
      alpha visibility <| traced (linetype colour) <| path ys



geom_abline : Aes -> Aes -> Graph -> Dimensions -> Geom
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
       alpha visibility <| traced (linetype colour) <| path <| points


derivative : Aes -> Aes -> Graph -> Dimensions -> Geom
derivative aes' defaults d dims = 
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


       vline1 = alpha visibility <| traced (dotted colour) <| path [head ys, x1pos]
       vline2 = alpha visibility <| traced (dotted colour) <| path [head <| tail ys, x2pos]
       xdot = move xpos <| filled colour <| circle pointsize 
       x1dot = move x1pos <| filled colour <| circle pointsize 
       x2dot = move x2pos <| filled colour <| circle pointsize 
       --annotation = move annotationPosition <| toForm <| leftAligned <| fromString  
            --<| "slope: " ++ (toString m) ++ "x\nintercept: " ++ (toString <| C.dec 3 b)
       symbol = filled colour <| rect w h
       label = move (w * 1.6,0) <| toForm <| leftAligned <| fromString  
                  <| "&Delta;x = " ++ (toString <| C.dec precision delta) 
       annotation = if annotate then  
                       move annotationPosition <| group [symbol,label] 
                    else toForm empty
   in
       alpha visibility <| group [traced (solid colour) <| path ys,  
       x1dot,  
       vline1,
       xdot,  
       x2dot,  
       vline2,
       annotation
       ]
       
mkTicks (a,b) tickspacing' = 
  let
    r = b - a
    m = head <| reverse <| filter (\m -> 10^m <= r) [-10..10] -- -1
    ten = if m <= 0 then 10^(m - 1) else 1 -- 0.1
    tickspacing = ceiling (tickspacing' / ten)
    ticks = map (\n -> ten * (toFloat n)) <|
            filter (\n -> if n /= 0 then n % tickspacing == 0 else True) 
            [ceiling (a / ten) .. floor (b / ten)]
  in
    ticks


yAxis : Aes -> Aes -> Graph -> Dimensions -> Geom
yAxis aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       label = lookup .label aes' defaults
       --fun = lookup .fun aes' defaults
       labelfun = lookup .labelfun aes' defaults
       precision = lookup .precision aes' defaults
       colour = lookup .colour aes' defaults
       revolve = lookup .revolve aes' defaults
       angle = if revolve then pi/2 else 0
       (xoffset,yoffset) = lookup .translate aes' defaults
       tickspacing = lookup .tickspacing aes' defaults
       xmargin = fst dims.margins
       xmin = fst d.xDomain
       --(ymin,ymax) = d.yLimits
       (ymin,ymax) = d.yDomain
       pos = (xm * (d.xScale xmin) - (xmargin / 4),0)
       tickPositions = mkTicks (ymin,ymax) tickspacing
       --tickPositions = filter (\n -> (round n) % tickspacing == 0) 
       --   <| map toFloat [ceiling ymin .. floor ymax]
       tickLabels = group <| map (\y -> move (xoffset + (-xmargin / 5), yoffset + ym * d.yScale y)  
         <| toForm <| rightAligned <| fromString <| 
         labelfun (toString <| C.dec precision y)) tickPositions
       ticks = group <| map (\y -> move (0,ym * d.yScale y)  
         <| traced (solid colour) <| path [(0,0),(10,0)]) tickPositions
       yBar = traced (solid colour) <| path [(10,0),(10,ym)]
       yLabel = rotate angle <|
          move (xoffset + (-xmargin / 2), yoffset + (ym * 0.5)) <|  
         toForm <| leftAligned <| fromString label
   in
      move pos <| group  
      [tickLabels, ticks, yBar, yLabel]

xAxis : Aes -> Aes -> Graph -> Dimensions -> Geom
xAxis aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       label = lookup .label aes' defaults
       --fun = lookup .fun aes' defaults
       --labelfun = lookup .labelfun aes' defaults
       precision = lookup .precision aes' defaults
       colour = lookup .colour aes' defaults
       (xoffset,yoffset) = lookup .translate aes' defaults
       tickspacing = lookup .tickspacing aes' defaults
       ymargin = snd dims.margins
       --ymin = fst d.yLimits
       ymin = fst d.yDomain
       xscale = if d.discrete then C.normalize (fst d.xDomain + 0.5, snd d.xDomain - 0.5) else d.xScale
       (xmin,xmax) = d.xDomain
       pos = (0,ym * (d.yScale ymin) - (ymargin / 8))
       --tickPositions = [xmin, 0, xmax]
       tickPositions = mkTicks (xmin,xmax) tickspacing
       --tickPositions = filter (\n -> (round n) % tickspacing == 0) 
       --   <| map toFloat [ceiling xmin .. floor xmax]
       tickLabels = group <| map (\x -> move (xoffset + (xm * xscale x),yoffset + (-ymargin / 3))  
         <| toForm <| centered <| fromString <|  
         toString <| C.dec precision x) tickPositions
       ticks = group <| map (\x -> move (xm * xscale x, 0)  
         <| traced (solid colour) <| path [(0,0),(0,-10)]) tickPositions
       xBar = traced (solid colour) <| path [(0,0),(xm,0)]
       xLabel = move (xoffset + (xm * 0.5),yoffset + (-ymargin * 0.6)) <|  
         toForm <| centered <| fromString label
   in
      move pos <|  
      group  
      [tickLabels, ticks, xBar, xLabel]

title : Aes -> Aes -> Graph -> Dimensions -> Geom
title aes' defaults d dims = 
   let
       label = lookup .label aes' defaults
       ymargin = fst dims.margins
       xm = dims.xm
       ym = dims.ym
       --ymax = snd d.yLimits
       ymax = snd d.yDomain
       midx = ((fst d.xDomain) + (snd d.xDomain)) / 2
       pos = (xm * (d.xScale midx),ym * (d.yScale ymax) + (ymargin / 4))
       ttl = toForm <| centered <| height 22 <| fromString <| label
   in
      move pos <| ttl


legend : Aes -> Aes -> Graph -> Dimensions -> Geom
legend aes' defaults d dims = 
   let
       xm = dims.xm
       ym = dims.ym
       (w',h') = lookup .dims aes' defaults
       (w,h) = (toFloat w', toFloat h')

       label' = lookup .label aes' defaults
       colour = lookup .colour aes' defaults
       translate = lookup .translate aes' defaults
       annotationPosition = (xm * d.xScale (fst translate),
                             ym * d.yScale (snd translate))
       symbol = move (w * -2,0) <| filled colour <| rect w h
       label = toForm <| leftAligned <| fromString label'
   in
       move annotationPosition <| group [symbol, label]


geom_image : Aes -> Aes -> Graph -> Dimensions -> Geom
geom_image aes' defaults d dims = 
   let
      (w,h) = lookup .dims aes' defaults
      url = lookup .label aes' defaults
      img = image w h url
      xm = dims.xm
      ym = dims.ym
      x = lookup .x aes' defaults
      y = lookup .y aes' defaults
      theta = lookup .theta aes' defaults
      (xoffset,yoffset) = lookup .translate aes' defaults
   in
      rotate theta <| move (xm * d.xScale x, yoffset + ym * d.yScale y) <| toForm img



{-| ... -}
plot (plotWidth,plotHeight) d geoms m w =  
   let
      input = { x = (fst m), y = (snd m), width = (fst w), height = (snd w) }

       --Pass two "aesthetics" to each geom, where a custom one can override defaults
      defaults = { aesDefault | delta <- Just d.dx,  
                                limits <- Just d.xDomain,
                                x <- Just xpos,  
                                y <- Just ypos, 
                                --theta <- Just (pi / 4),
                                nsteps <- Just nsteps
                             }
      windowScaleX = C.normalize (0,toFloat input.width)    --- 
      windowScaleY = C.normalize (0,toFloat input.height)   ---
      nsteps = round <| (toFloat input.x) / 4   --- 
      wpos = (windowScaleX (toFloat input.x), windowScaleY (toFloat input.y))   --- 
      xpos = (fst d.xDomain) + d.xExtent * (fst wpos)
      ypos = if d.discrete then (d.f << (toFloat << round)) xpos else d.f xpos
      xmargin = (toFloat plotWidth) * 0.1
      ymargin = (toFloat plotHeight) * 0.2
      xoffset = (toFloat plotWidth)/2 - xmargin
      yoffset = (toFloat plotHeight)/2 - ymargin
      ymultiplier = (toFloat plotHeight) - (2 * ymargin)
      xmultiplier = (toFloat plotWidth) - (2 * xmargin)

      dims : Dimensions
      dims = {
         margins = (xmargin,ymargin),
         plotWidth = (toFloat plotWidth),
         plotHeight = (toFloat plotHeight),
         ym = ymultiplier,
         xm = xmultiplier
      }

      offsets = (-xoffset, -yoffset)
   in 
      collage plotWidth plotHeight  
            [move offsets <| group <| map (\g -> g defaults d dims) geoms]






background : Aes -> Aes -> Graph -> Dimensions -> Geom
background aes' defaults d dims = 
   let
       colour = lookup .colour aes' defaults
       visibility = lookup .visibility aes' defaults
       xm = dims.xm
       ym = dims.ym
       colour2 = lightGrey
   in
      group  
      [ 
         move (xm * 0.5, ym * 0.5) <| group  
         <| [alpha visibility <| filled colour <| rect dims.plotWidth dims.plotHeight,
            filled colour2 <| rect xm ym],  

         traced (solid black) <|  
            path [(0,0),(0,ym), (xm,ym),(xm,0),(0,0)] 
      ]

annotate_integral : Aes -> Aes -> Graph -> Dimensions -> Geom
annotate_integral aes' defaults d dims = 
   let
       precision = lookup .precision aes' defaults
       translate = lookup .translate aes' defaults
       limits = lookup .limits aes' defaults
       --baseline = lookup .fun aes' defaults -- !!
       (xmin,xmax) = limits
       integral = C.integrate limits (toFloat d.steps) d.f
   in
       move (dims.xm * fst translate,
                dims.ym * snd translate) -- plot scale
       --move (dims.xm * (d.xScale (fst translate)),  -- graph scale
       --         dims.ym * (d.yScale (snd translate)))
              <| toForm <| rightAligned <| fromString  
              -- <| "number of bins: " ++ (toString dims.nbins) 
              <| "\n&#x222b;"  
                     ++ " from " ++ (toString <| C.dec precision xmin)  
                     ++ " to " ++ (toString <| C.dec precision xmax)  
                     ++ " &#8776; " ++ (toString <| C.dec precision <| integral)


{-- 
TODO: 
Negate geom_trace_polar?
Add documentation to Plot module
Consistent naming of single and multiple geoms, eg point/points
Rename xscale/yscale --> fromX/fromY
(xm * d.xScale x) etc is a recurring pattern. Factor this out completely into a function?
Colouring positive and negative integrals independently?
Background grid?
Decrease right margin or make better use of the empty space?
Linestyles and textstyles?! 
Factor out x and y labels from axes, to be able to move/rotate them independently?
"Translate" given in the scale of x,y or percent of plot height,width or both?
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
         margins : (Float,Float),
         plotWidth : Float,
         plotHeight : Float,
         ym : Float,
         xm : Float
}
            

{-| ... -}
type alias Aes = { visibility:Maybe Float, 
                   linetype:Maybe (Color -> LineStyle),
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
                   labelfun: Maybe (String -> String),
                   dynamic: Maybe Bool,
                   negate: Maybe Bool,
                   fit: Maybe Bool,
                   revolve: Maybe Bool,
                   tickspacing: Maybe Float, 
                   nsteps: Maybe Int
                }

{-| ... -}
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
        labelfun = Nothing,
        dynamic = Nothing,
        negate = Nothing,
        fit = Nothing,
        revolve = Nothing,
        tickspacing = Nothing,
        nsteps = Nothing
     }


{-| ... -}
aesDefault : Aes
aesDefault = { visibility = Just 1, 
               linetype = Just solid,  
               linethickness = Just 1,  
               pointsize = Just 4.0,  
               colour = Just black,
               translate = Just (0,0),
               precision = Just 2,
               dims = Just (30,3),
               label = Just "",
               delta = Just 0.1,
               limits = Just (-1,1),
               annotate = Just True, 
               x = Just 0,
               y = Just 0,
               radius = Just 1,
               theta = Just 0,
               fun = Just (\x -> 0),
               labelfun = Just (\x -> x),
               dynamic = Just True,
               negate = Just False,
               fit = Just True,
               revolve = Just False,
               tickspacing = Just 1,
               nsteps = Just 10
               }

