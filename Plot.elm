-- function graphs for plotting distributions
-- a work in progress

import Discrete as D
import Continuous as C
import Graphics.Collage as GC
import Graphics.Element (Element)
import Signal
import Color (..)
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
--pc = plotConfig (\x -> 3 + 6 * sin x) (-2*pi,2*pi) 100
pc = plotConfig (\x -> 0.2 * (cos x)) (-2*pi,2*pi) 100
--pc = plotConfig (\x -> e^x) (-2*pi,2*pi) 100
--pc = plotConfig (\x -> C.pdfstandardnormal x) (-4,4) 100
--pc = Signal.map (plotConfig sin (-2*pi,2*pi)) Mouse.x 

point (xm,ym) (xscale, yscale) f x y =  
      (xm * (xscale x), ym * (yscale <| f x)) 

geom_point aes defaults cfg dims (dx, steps) = 
   let
      xm = dims.xm
      ym = dims.ym
      colour = lookup .colour aes defaults
      pointsize = lookup .pointsize aes defaults
      visibility = lookup .visibility aes defaults
      xscale = cfg.xScale
      yscale = cfg.yScale
      --ys = List.map cfg.f steps -- SLOOOW, need to limit the number of points
      points = List.map2 (point (xm,ym) (xscale,yscale) cfg.f) steps cfg.ys 
   in
      GC.alpha visibility <| GC.group <| List.map (\(x,y) -> GC.move (x,y) <| GC.filled colour <| GC.circle pointsize) points

bar (xm,ym) (xscale, yscale) f (x1',x2) =  
   let
       x1 = if (xscale x1' < 0) then x2 else x1'
       x3 = (x1+x2)/2
       midpoint = if (xscale x1' < 0) then 0 else f x3
   in
      List.map (\(x,y) -> (xm * (xscale x), ym * (yscale y))) 
               [(x1,0), (x2,0), (x2,midpoint), (x1,midpoint)]

geom_bar aes defaults cfg dims (dx', steps) = 
   let
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
      xscale = cfg.xScale
      yscale = cfg.yScale
      dx = dx' / 2
      xs = C.bins <| List.map (\x -> x - dx) steps
      points = List.map (bar (xm,ym) (xscale,yscale) cfg.f) xs 
   in
      GC.alpha visibility <| GC.group <| List.map (\x -> GC.outlined (linetype colour) <| GC.polygon x) points


{-| Calculating trapezium points for plotting them as polygons. -}
trapezoid (xm,ym) (xscale, yscale) f (x1,x2) =  
   List.map (\(x,y) -> (xm * (xscale x), ym * (yscale y))) 
            [(x1,0), (x2,0), (x2,f x2), (x1,f x1)]

geom_trapezoid aes defaults cfg dims (dx, steps) = 
   let
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
      xm = dims.xm
      ym = dims.ym
      xscale = cfg.xScale
      yscale = cfg.yScale
      xs = C.bins steps
      points = List.map (trapezoid (xm,ym) (xscale,yscale) cfg.f) xs 
   in
      GC.alpha visibility <| GC.group <| List.map (\x -> GC.outlined (linetype colour) <| GC.polygon x) points

geom_curve aes defaults cfg dims = 
   let
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
      linethickness = lookup .linethickness aes defaults -- not actually used yet
      xscale = cfg.xScale
      yscale = cfg.yScale
      (dx,steps) = C.interpolate cfg.xDomain (toFloat cfg.steps) cfg.f
      ys = List.map (\(x,y) -> (dims.xm * (xscale x), dims.ym * (yscale y))) <| List.map2 (,) steps (List.map cfg.f steps)
   in
      GC.alpha visibility <| GC.traced (linetype colour) <| GC.path ys

geom_hline aes defaults cfg dims y =
   let
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
   in
      GC.alpha visibility <| GC.traced (linetype colour)  
      <| GC.path [(0,ym * (cfg.yScale y)), (xm, ym * (cfg.yScale y))] 

geom_vline aes defaults cfg dims x =
   let
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
   in
      GC.alpha visibility <| GC.traced (linetype colour)  
      <| GC.path [(xm * (cfg.xScale x),0), (xm * (cfg.xScale x), ym)] 


geom_tangent aes defaults cfg dims x' = 
   let
       --aes = { annotationPosition = (80,50), 
       --        colour = red, 
       --        dx = 0.033
       --     }
       xm = dims.xm
       ym = dims.ym
       linetype = lookup .linetype aes defaults
       colour = lookup .colour aes defaults
       visibility = lookup .visibility aes defaults
       annotationPosition = (80,50)
       dx = 0.033
       xmin = fst cfg.xDomain
       ymin = fst cfg.yDomain
       ymax = snd cfg.yDomain
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
       annotation = GC.move annotationPosition <| GC.toForm <| Text.leftAligned <| Text.fromString  
            <| "slope: " ++ (toString m) ++ "x\nintercept: " ++ (toString <| C.dec 3 b)
   in
        --GC.move (80,300) <| GC.toForm <| Text.rightAligned <| Text.fromString <| toString <| List.map (\(a,b) -> (C.dec 2 a, C.dec 2 b)) ys
        --GC.move (80,300) <| GC.toForm <| Text.rightAligned <| Text.fromString <| toString <| tangent
        --GC.move (80,300) <| GC.toForm <| Text.rightAligned <| Text.fromString <| toString <| y
        --GC.move (80,280) <| GC.toForm <| Text.rightAligned <| Text.fromString  <| (toString <| (C.dec 2 x1,C.dec 2 x,C.dec 2 x2, C.dec 2 (x2 - x1))) ++ "\n" ++ (toString <| (C.dec 2 y1,C.dec 2y,C.dec 2 y2, C.dec 2 (y2 - y1))) ++ "\n" ++ (toString (C.dec 2 ymin, C.dec 2 ymax))
       GC.alpha visibility <| GC.group [GC.traced (GC.solid red) <| GC.path ys,  
       GC.move (xm * cfg.xScale x,ym * cfg.yScale (cfg.f x))  
       <| GC.filled colour <| GC.circle 2,  
       annotation]
       
-- customization object for axes?
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
         <| GC.traced (GC.solid black) <| GC.path [(0,0),(12,0)]) tickPositions
       yBar = GC.traced (GC.solid black) <| GC.path [(10,0),(10,ym)]
   in
      GC.move pos <| GC.group  
      [tickLabels, ticks, yBar]

-- customization object for axes?
axisX cfg (xm,ym) ymargin = 
   let
       ymin = fst cfg.yDomain
       xmin = fst cfg.xDomain
       xmax = snd cfg.xDomain
       pos = (0,ym * (cfg.yScale ymin) - (ymargin / 12))
       tickPositions = [xmin, 0, xmax]
       tickLabels = GC.group <| List.map (\x -> GC.move (xm * cfg.xScale x,-ymargin * 0.3)  
         <| GC.toForm <| Text.centered <| Text.fromString <|  
         toString <| C.dec 2 x) tickPositions
       ticks = GC.group <| List.map (\x -> GC.move (xm * cfg.xScale x, -ymargin * 0.2)  
         <| GC.traced (GC.solid black) <| GC.path [(0,0),(0,12)]) tickPositions
       xBar = GC.traced (GC.solid black) <| GC.path [(0,-10),(xm,-10)] -- replace 10!!
   in
      GC.move pos <| GC.group  
      [tickLabels, ticks, xBar]

--meh (mx,my) (ww,wh) = Text.asText <| (C.normalize (0,toFloat ww) (toFloat mx))
--main = Signal.map2 meh Mouse.position Window.dimensions

main : Signal Element
main = Signal.map2 (plotc (1400,400) pc) Mouse.x Window.dimensions 
--main = Signal.map Text.asText Window.dimensions

plotc : (Int,Int) -> PlotConfig -> Int -> (Int,Int) -> Element
plotc (plotWidth,plotHeight) cfg mouseX (windowWidth,windowHeight) =  
   let
      -- make plot dimensions responsive --> 
      --plotWidth = round ((toFloat windowWidth) * 0.8)
      --plotHeight = round ((toFloat windowHeight) * 0.8)

      -- Pass two "aesthetics" to each geom, where a custom one can override defaults
      defaults = aesDefault
      customAes = { aes | colour <- Just blue,
                          linetype <- Just GC.solid, 
                          visibility <- Just 1
                       } 
      pointAes = { aes | pointsize <- Just 4, 
                         colour <- Just orange  
                       }
      curveAes = { aes | colour <- Just darkGrey  
                       }
      
      windowScale = C.normalize (0,toFloat windowWidth)
      nbins = round <| (toFloat mouseX) / 4
      --xpos = (fst cfg.xDomain) + cfg.xExtent * windowScale (toFloat mouseX)
      wpos = windowScale (toFloat mouseX)
      xpos = (fst cfg.xDomain) + cfg.xExtent * wpos
      ypos = cfg.f <| xpos
      xmargin = (toFloat plotWidth) * 0.1
      ymargin = (toFloat plotHeight) * 0.2
      xoffset = (toFloat plotWidth)/2 - xmargin
      yoffset = (toFloat plotHeight)/2 - ymargin
      innerWidth = (toFloat plotWidth) - xmargin
      innerHeight = (toFloat plotHeight) - ymargin
      ymultiplier = (toFloat plotHeight) - (2 * ymargin)
      xmultiplier = (toFloat plotWidth) - (2 * xmargin)
      zero = cfg.yScale 0

      dims = {
         windowScale = windowScale,
         nbins = nbins,
         wpos = wpos,
         xpos = xpos,
         ypos = ypos,
         xmargin = xmargin,
         ymargin = ymargin,
         xoffset = xoffset,
         yoffset = yoffset,
         innerWidth = innerWidth,
         innerHeight = innerHeight,
         ym = ymultiplier,
         xm = xmultiplier,
         zero = zero
      }

      baseline = List.map (\x -> 0) [1..List.length cfg.xs]
      (dx,steps) = C.interpolate cfg.xDomain (toFloat nbins) cfg.f
      integral = C.integrate cfg.xDomain (toFloat nbins) cfg.f
      plotFrame = GC.traced (GC.solid black)  
         <| GC.path [(0,0),(0,ymultiplier), (xmultiplier,ymultiplier),(xmultiplier,0),(0,0)]
      zeroX = geom_hline customAes defaults cfg dims 0
      zeroY = geom_vline customAes defaults cfg dims 0
      xMarker = geom_vline customAes defaults cfg dims xpos
      yMarker = geom_hline customAes defaults cfg dims ypos
      tangent = geom_tangent customAes defaults cfg dims wpos
      --yAxis = GC.traced (GC.solid black) <| GC.path [(0,0),(0,ymultiplier)]
      yAxis = axisY cfg (xmultiplier,ymultiplier) xmargin
      xAxis = axisX cfg (xmultiplier,ymultiplier) ymargin
      --xAxis = GC.traced (GC.solid black) <| GC.path [(0,0),(xmultiplier,0)]
      offsets = (-xoffset, -yoffset)
   in 
      GC.collage plotWidth plotHeight  
             [  
                GC.filled grey  <| GC.rect innerWidth innerHeight,
                GC.filled lightGrey <| GC.rect xmultiplier ymultiplier,
                --(GC.filled grey (GC.ngon 8 ((toFloat plotHeight)/2))),
                GC.move offsets <| geom_curve curveAes defaults cfg dims, 
              GC.move offsets <| geom_trapezoid customAes defaults cfg dims (dx,steps),
              --GC.move offsets <| geom_bar customAes defaults cfg dims (dx,steps),
              GC.move offsets <| geom_point pointAes defaults cfg dims (dx,steps),
              GC.move offsets <| yAxis,
              GC.move offsets <| xAxis,
              GC.move offsets <| zeroX,
              GC.move offsets <| zeroY,
              GC.move offsets <| xMarker,
              GC.move offsets <| yMarker,
              GC.move offsets <| tangent,
              --GC.move (-xoffset, -yoffset) <| xAxis,
              GC.move offsets <| plotFrame,
              GC.move (-innerWidth/10,innerHeight/4) <| GC.toForm <| Text.rightAligned <| Text.fromString  
              <| "number of bins: " ++ (toString nbins) 
              ++ "\nApproximate &#x222b;"  
                     ++ " from " ++ (toString <| C.dec 3 (fst cfg.xDomain))  
                     ++ " to " ++ (toString <| C.dec 3 (snd cfg.xDomain))  
                     --++ "\n = " ++ (toString <| integral)
                     ++ " = " ++ (toString <| C.dec 3 <| integral)]


              --GC.move (-xoffset, -yoffset - 10.0) <| geom_rug "x" xmultiplier xscale steps,
              --GC.move (-xoffset - 10, -yoffset) <| geom_rug "y" xmultiplier xscale steps]


{- 
TODO: 
implement grid + axes
implement theme/aes object to hold color, linestyle, etc.
factor out annotation and simplify positioning
geom_bar: make half bars at each end? The "number of bins" annotation does not match. 
should be able to integrate smaller portions of the x domain (as in hypothesis testing etc.)
should be able to specify the range of tangent lines, so as to show that smaller h lead to better approximations of the derivative
annotation position should be given in the domains of x,y
-}


lookup f child parent = let
                            childValue = f child
                            parentValue = f parent
                        in
                            case (childValue,parentValue) of
                                (Just n,_) -> n
                                (Nothing, Just n2) -> n2


type alias AESdefault = { visibility:Maybe Float, 
                          linetype:Maybe (Color -> GC.LineStyle),
                          linethickness:Maybe Float,
                          pointsize:Maybe Float, 
                          colour:Maybe Color
                       }

aesDefault : AESdefault
aesDefault = { visibility = Just 1, 
               linetype = Just GC.solid,  
               linethickness = Just 1,  
               pointsize = Just 2,  
               colour = Just black
            }

type alias AES = { visibility:Maybe Float, 
                   linetype:Maybe (Color -> GC.LineStyle),
                   linethickness:Maybe Float,
                   pointsize:Maybe Float,
                   colour:Maybe Color  
                }

aes : AES
aes = { visibility = Nothing, 
        linetype = Nothing,  
        linethickness = Nothing,  
        pointsize = Nothing,  
        colour = Nothing
     }


--aesChild = { aes | linetype <- Just "dotted",
--                   pointsize <- Just 4 }
--
--aesParent = aesDefault
