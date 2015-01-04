-- function graphs for plotting distributions
-- a work in progress

import Discrete as D
import Continuous as C
import Graphics.Collage as GC
import Graphics.Element (Element, image)
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
         yExtent:Float, 
         plotLimits:(Float,Float)
      }

plotConfig : (Float -> Float) -> (Float, Float) -> Int -> PlotConfig
plotConfig f (from,to) steps = 
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


--pc = plotConfig (\x -> logBase e x) (1,2*pi) 100
--pc = plotConfig (\x -> -6 + 3 * sin x) (-1*pi,2*pi) 100
pc = plotConfig (\x -> 0.2 * (cos x)) (-2*pi,2*pi) 100
--pc = plotConfig (\x -> 0.1 * x^2) (-3*pi,2*pi) 100
--pc = plotConfig (\x -> e^x) (-2*pi,2*pi) 100
--pc = plotConfig (\x -> C.pdfstandardnormal x) (-4,4) 100
--pc = Signal.map (plotConfig sin (-2*pi,2*pi)) Mouse.x 

point (xm,ym) (xscale, yscale) f x y =  
      (xm * (xscale x), ym * (yscale <| f x)) 

geom_point aes defaults cfg dims = 
   let
      (dx,steps) = C.interpolate cfg.xDomain (toFloat dims.nbins)
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

geom_bar aes defaults cfg dims = 
   let
      (dx',steps) = C.interpolate cfg.xDomain (toFloat dims.nbins)
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

geom_trapezoid aes defaults cfg dims = 
   let
      (dx,steps) = C.interpolate cfg.xDomain (toFloat dims.nbins)
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
      (dx,steps) = C.interpolate cfg.xDomain (toFloat cfg.steps)
      ys = List.map (\(x,y) -> (dims.xm * (xscale x), dims.ym * (yscale y))) <| List.map2 (,) steps (List.map cfg.f steps)
   in
      GC.alpha visibility <| GC.traced (linetype colour) <| GC.path ys

geom_hline aes defaults cfg dims =
   let
      y = dims.ypos
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
   in
      GC.alpha visibility <| GC.traced (linetype colour)  
      <| GC.path [(0,ym * (cfg.yScale y)), (xm, ym * (cfg.yScale y))] 

geom_vline aes defaults cfg dims =
   let
      x = dims.xpos
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
   in
      GC.alpha visibility <| GC.traced (linetype colour)  
      <| GC.path [(xm * (cfg.xScale x),0), (xm * (cfg.xScale x), ym)] 

geom_circle aes defaults cfg dims =
   let
      xm = dims.xm
      ym = dims.ym
      linetype = lookup .linetype aes defaults
      colour = lookup .colour aes defaults
      visibility = lookup .visibility aes defaults
      radius = ym * 0.2
      roundthing = GC.circle radius
      pos = (xm * cfg.xScale 0, ym * cfg.yScale 0)
   in
      GC.move pos <| GC.alpha visibility <| GC.outlined (linetype colour) <| roundthing






geom_tangent aes defaults cfg dims = 
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
       annotationPosition = (xm * cfg.xScale (fst translate), ym * cfg.yScale (snd translate))
       --dx = 0.033
       --dx = 0.8 
       dx = 0.3 * (1 - dims.wposy) * cfg.xExtent 
       --h = 0.0001 -- smaller dx for calculating a slightly more precise derivative
       xmin = fst cfg.xDomain
       ymin = fst cfg.yDomain
       ymax = snd cfg.yDomain
       x = dims.xpos
       tangent = C.tangent (dx * 2) x cfg.f
       --tangent = C.tangent h x cfg.f
       m = tangent.slope
       b = tangent.intercept
       fun = (\x -> m * x + b)
       y = (fun x)
       --x1 = xmin + (cfg.xExtent * (x' - dx))
       --x1 = xmin + (cfg.xExtent * x') - dx
       x1 = x - dx
       y1 = fun x1
       --x2 = xmin + (cfg.xExtent * (x' + dx))
       --x2 = xmin + (cfg.xExtent * x') + dx
       x2 = x + dx
       y2 = fun x2
       ys = [(xm * cfg.xScale x1, ym * cfg.yScale y1),
             (xm * cfg.xScale x2, ym * cfg.yScale y2)]
       xpos = (xm * cfg.xScale x,ym * cfg.yScale (cfg.f x))
       x1pos = (xm * cfg.xScale x1,ym * cfg.yScale (cfg.f x1))
       x2pos = (xm * cfg.xScale x2,ym * cfg.yScale (cfg.f x2))
       vline1 = GC.alpha visibility <| GC.traced (GC.dotted red) <| GC.path [List.head ys, x1pos]
       vline2 = GC.alpha visibility <| GC.traced (GC.dotted red) <| GC.path [List.head <| List.tail ys, x2pos]
       xdot = GC.move xpos <| GC.filled colour <| GC.circle pointsize 
       x1dot = GC.move x1pos <| GC.filled colour <| GC.circle pointsize 
       x2dot = GC.move x2pos <| GC.filled colour <| GC.circle pointsize 
       annotation = GC.move annotationPosition <| GC.toForm <| Text.leftAligned <| Text.fromString  
            <| "slope: " ++ (toString m) ++ "x\nintercept: " ++ (toString <| C.dec 3 b)
   in
        --GC.move (80,300) <| GC.toForm <| Text.rightAligned <| Text.fromString <| toString <| List.map (\(a,b) -> (C.dec 2 a, C.dec 2 b)) ys
        --GC.move (80,300) <| GC.toForm <| Text.rightAligned <| Text.fromString <| toString <| tangent
        --GC.move (80,300) <| GC.toForm <| Text.rightAligned <| Text.fromString <| toString <| y
        --GC.move (80,280) <| GC.toForm <| Text.rightAligned <| Text.fromString  <| (toString <| (C.dec 2 x1,C.dec 2 x,C.dec 2 x2, C.dec 2 (x2 - x1))) ++ "\n" ++ (toString <| (C.dec 2 y1,C.dec 2y,C.dec 2 y2, C.dec 2 (y2 - y1))) ++ "\n" ++ (toString (C.dec 2 ymin, C.dec 2 ymax))
       GC.alpha visibility <| GC.group [GC.traced (GC.solid red) <| GC.path ys,  
       x1dot,  
       vline1,
       xdot,  
       x2dot,  
       vline2,
       annotation]
       
-- customization object for axes?
--axisY cfg (xm,ym) xmargin = 
yAxis aes defaults cfg dims = 
   let
       xm = dims.xm
       ym = dims.ym
       xmargin = dims.xmargin
       xmin = fst cfg.xDomain
       --ymin = fst cfg.yDomain
       ymin = fst cfg.plotLimits
       --ymax = snd cfg.yDomain
       ymax = snd cfg.plotLimits
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
--axisX cfg (xm,ym) ymargin = 
xAxis aes defaults cfg dims = 
   let
       xm = dims.xm
       ym = dims.ym
       ymargin = dims.ymargin
       --ymin = fst cfg.yDomain
       ymin = fst cfg.plotLimits
       xmin = fst cfg.xDomain
       xmax = snd cfg.xDomain
       pos = (0,ym * (cfg.yScale ymin) - (ymargin / 12))
       tickPositions = [xmin, 0, xmax]
       tickLabels = GC.group <| List.map (\x -> GC.move (xm * cfg.xScale x,-ymargin * 0.3)  
         <| GC.toForm <| Text.centered <| Text.fromString <|  
         toString <| C.dec 2 x) tickPositions
       ticks = GC.group <| List.map (\x -> GC.move (xm * cfg.xScale x, -ymargin * 0.2)  
         <| GC.traced (GC.solid black) <| GC.path [(0,0),(0,12)]) tickPositions
       xBar = GC.traced (GC.solid black) <| GC.path [(0,-11),(xm,-11)] -- replace 10!!
   in
      GC.move pos <| GC.group  
      [tickLabels, ticks, xBar]

--
title aes defaults cfg dims = 
   let
       txt = lookup .txt aes defaults
       xm = dims.xm
       ym = dims.ym
       --ymax = snd cfg.yDomain
       ymax = snd cfg.plotLimits
       midx = ((fst cfg.xDomain) + (snd cfg.xDomain)) / 2
       pos = (xm * (cfg.xScale midx),ym * (cfg.yScale ymax) + (dims.ymargin / 4))
       --pos = (0,0)
       ttl = GC.toForm <| Text.centered <| Text.fromString <| txt
   in
      GC.move pos <| ttl


geom_doge aes defaults cfg dims = 
   let
      wh = lookup .dims aes defaults
      dogeurl = "https://31.media.tumblr.com/b23ef59e7838d323c281de41a31d672a/tumblr_inline_n0rlcbgFBe1s0subn.gif"
      dogeimg = image (fst wh) (snd wh) dogeurl
      xm = dims.xm
      ym = dims.ym
   in
      GC.move (xm * cfg.xScale dims.xpos, ym * cfg.yScale dims.ypos) <| GC.toForm dogeimg

{-- CUSTOM AESTHETICS --}
customAes = { aes | colour <- Just blue,
                     linetype <- Just GC.solid, 
                     visibility <- Just 1
                  } 
pointAes = { aes | pointsize <- Just 4, 
                     colour <- Just red,  
                     linethickness <- Just 5,
                     translate <- Just (-5.3,-0.14)
                  }
curveAes = { aes | colour <- Just darkGrey  
                  }
annotationAes = { aes | translate <- Just (-pi,0.1),
                        decimals <- Just 5,  
                        txt <- Just "y = 0.2cos(x) within -2pi &le; x &le; 2pi" }

geoms = [layer_background { aes | colour <- Just grey }, 
         xAxis aes,
         yAxis aes,
         geom_curve curveAes, 
         --geom_point pointAes,
         --geom_bar customAes,
         --geom_trapezoid customAes,
         --geom_circle customAes,
         --geom_hline aes, geom_vline aes,
         geom_tangent pointAes,
         title annotationAes,
         geom_doge aes
         --annotate_integral annotationAes 
         ]

--meh (mx,my) (ww,wh) = Text.asText <| (C.normalize (0,toFloat ww) (toFloat mx))
--main = Signal.map2 meh Mouse.position Window.dimensions

main : Signal Element
main = Signal.map2 (plotc (1400,600) pc geoms) Mouse.position Window.dimensions 
--main = Signal.map Text.asText Window.dimensions


--plotc : (Int,Int) -> PlotConfig -> (Int,Int) -> (Int,Int) -> Element
plotc (plotWidth,plotHeight) cfg geoms (mouseX,mouseY) (windowWidth,windowHeight) =  
   let
      -- make plot dimensions responsive --> 
      --plotWidth = round ((toFloat windowWidth) * 0.8)
      --plotHeight = round ((toFloat windowHeight) * 0.8)

      -- Pass two "aesthetics" to each geom, where a custom one can override defaults
      defaults = aesDefault
      
      windowScaleX = C.normalize (0,toFloat windowWidth)
      windowScaleY = C.normalize (0,toFloat windowHeight)
      nbins = round <| (toFloat mouseX) / 4
      --xpos = (fst cfg.xDomain) + cfg.xExtent * windowScale (toFloat mouseX)
      wposx = windowScaleX (toFloat mouseX)
      wposy = windowScaleY (toFloat mouseY)
      xpos = (fst cfg.xDomain) + cfg.xExtent * wposx
      ypos = cfg.f xpos
      xmargin = (toFloat plotWidth) * 0.1
      ymargin = (toFloat plotHeight) * 0.2
      xoffset = (toFloat plotWidth)/2 - xmargin
      yoffset = (toFloat plotHeight)/2 - ymargin
      innerWidth = (toFloat plotWidth) - xmargin
      innerHeight = (toFloat plotHeight) - ymargin
      ymultiplier = (toFloat plotHeight) - (2 * ymargin)
      xmultiplier = (toFloat plotWidth) - (2 * xmargin)
      zero = cfg.yScale 0
      --baseline = List.map (\x -> 0) [1..List.length cfg.xs]

      dims = {
         windowScaleX = windowScaleX,
         windowScaleY = windowScaleY,
         nbins = nbins,
         wposx = wposx,
         wposy = wposy,
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

      --zeroX = geom_hline customAes defaults cfg dims 0
      --zeroY = geom_vline customAes defaults cfg dims 0
      --tangent = geom_tangent pointAes defaults cfg dims wposx
      offsets = (-xoffset, -yoffset)
   in 
      GC.collage plotWidth plotHeight  
            [GC.move offsets <| GC.group <| List.map (\g -> g defaults cfg dims) geoms]
             --[  
             -- --GC.move offsets <| xMarker,
             -- --GC.move offsets <| yMarker,
             -- GC.move offsets <| annotate_integral annotationAes defaults cfg dims,
             -- ]


layer_background aes defaults cfg dims = 
   let
       colour = lookup .colour aes defaults
       xm = dims.xm
       ym = dims.ym
   in
      GC.group  
      [ 
         GC.move (dims.xoffset, dims.yoffset) <| GC.group  
         <| [GC.filled colour <| GC.rect dims.innerWidth dims.innerHeight,
            GC.filled lightGrey <| GC.rect xm ym],  

         GC.traced (GC.solid black) <|  
            GC.path [(0,0),(0,ym), (xm,ym),(xm,0),(0,0)] 
      ]

annotate_integral aes defaults cfg dims = 
   let
       integral = C.integrate cfg.xDomain (toFloat dims.nbins) cfg.f
       decimals = lookup .decimals aes defaults
       translate = lookup .translate aes defaults
       xmin = fst cfg.xDomain
       xmax = snd cfg.xDomain
   in
       GC.move (dims.xm * (cfg.xScale (fst translate)), dims.ym * (cfg.yScale (snd translate)))
              <| GC.toForm <| Text.rightAligned <| Text.fromString  
              <| "number of bins: " ++ (toString dims.nbins) 
              ++ "\n&#x222b;"  
                     ++ " from " ++ (toString <| C.dec decimals xmin)  
                     ++ " to " ++ (toString <| C.dec decimals xmax)  
                     --++ "\n = " ++ (toString <| dims.integral)
                     ++ " &#8776; " ++ (toString <| C.dec decimals <| integral)


{- 
TODO: 
make parent and child "configs" with maybe types, perhaps rename them into something more appropriate
implement grid... 
factor out annotation and simplify positioning
geom_bar: make half bars at each end? The "number of bins" annotation does not match. 
should be able to integrate smaller portions of the x domain (as in hypothesis testing etc.)
should be able to specify the range of tangent lines, so as to show that smaller h lead to better approximations of the derivative
separate derivative and integral calculations from their visual representations?
visualize the precision/convergence of integration at different number of partial integrals
-}



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
                   txt: Maybe String
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
        txt = Nothing
     }

aesDefault : AES
aesDefault = { aes | visibility <- Just 1, 
                     linetype <- Just GC.solid,  
                     linethickness <- Just 1,  
                     pointsize <- Just 2,  
                     colour <- Just black,
                     translate <- Just (0,0),
                     decimals <- Just 2,
                     dims <- Just (50,50),
                     txt <- Just "Plot title"
               }

