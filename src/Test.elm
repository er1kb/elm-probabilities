import Plot (..)
import Color (..)
import Graphics.Collage as GC
import Graphics.Element (Element, empty, image, flow, down, right, outward, layers)
import Signal
import Color (..)
import Text
import Window
import Mouse
import List
import Html
import Continuous as C
import Discrete as D


const = 4
nsteps = 400
--pc = distribution (\x -> logBase e x) (1,2*pi) 100
--pc = distribution (\x -> -6 + 3 * sin x) (-1*pi,2*pi) 100
--pc = distribution (\x -> 1 * (cos (const*x))) (0,2*pi) nsteps
--pc2 = distribution (\x -> -1 * (sin (const*x))) (0,2*pi) nsteps
--pc3 = distribution (\x -> 1 * (sin (const*x))) (0,2*pi) nsteps
--pc4 = distribution (\x -> -1 * (cos (const*x))) (0,2*pi) nsteps
--pc5 = distribution (\x -> 3 * (cos (2*x))) (0,2*pi) 100

--pc5 = distribution (\x -> 3 * (cos (2.01*x))) (-12*pi,12*pi) 100
--pc5 = distribution (\x -> 3 * (cos (2*x))) (0,2*pi) 100
--pc5 = distribution (\x -> 3 * (cos (2*x))) (-pi,pi) 100
--pc5 = distribution (\x -> 3 * (sin (2*x))) (-pi,pi) 100

--pc = distribution (\x -> 0.1 * x^2) (-3*pi,2*pi) 100
--pc = distribution (\x -> e^x) (-2*pi,2*pi) 100
--pc = distribution (\x -> C.pdfstandardnormal x) (-4,4) 100
--pc = Signal.map (distribution sin (-2*pi,2*pi)) Mouse.x 




--type alias Input = Signal { x : Int,
--                            y : Int, 
--                            width : Int, 
--                            height : Int
--                           }
--input : Input
--input = Signal.map2 (\(mx,my) (w,h) -> { x = mx, y = my, width = w, height = h}) Mouse.position Window.dimensions

--bl = (\x -> 2 + 4 * sin (1*x))
bl = (\x -> 0)

customAes = { aes | radius <- Just 2, pointsize <- Just 9, colour <- Just darkBlue, visibility <- Just 0.6, annotate <- Just True }
geoms1 = [ 
         --background { aes | colour <- Just lightGrey }, 
         --geom_area { aes | fun <- Just (\x -> 0.5), colour <- Just grey, dynamic <- Just True, negate <- Just False },
         --geom_area { aes | colour <- Just darkGreen, visibility <- Just 0.4, dynamic <- Just True, negate <- Just True },
         --geom_trapezoid { aes | fun <- Just bl },
         --geom_bar { aes | fun <- Just (\x -> sin (4*x)), colour <- Just darkBlue },
         --geom_step customAes,
         geom_curve customAes,
         geom_point customAes,
         --geom_trace_polar customAes,
         geom_trace_polar { aes | colour <- Just purple, dynamic <- Just True, rotate <- Just True, negate <- Just True },
         --geom_area_polar { aes | colour <- Just lightBlue, dynamic <- Just True, rotate <- Just True, negate <- Just True, limits <- Just (-pi/2,pi/2) },
         --geom_curve_polar { aes | colour <- Just purple, dynamic <- Just True, rotate <- Just True },
         --geom_circle { aes | colour <- Just purple, dynamic <- Just True, rotate <- Just True },
         geom_angle { aes | colour <- Just red, dynamic <- Just True, rotate <- Just True },
         geom_trace { aes | colour <- Just orange },
         --geom_hline customAes,
         geom_vline { aes | label <- Just "x = ", translate <- Just (0.5,0) },
         geom_abline { customAes | limits <- Just (-pi/2,pi/2), dynamic <- Just False, fun <- Just (\x -> cos (4*x)), linetype <- Just GC.dotted },
         --geom_tangent { aes | colour <- Just red, delta <- Just 1.51, translate <- Just (-2.6,0.5), precision <- Just 1 },
         --geom_hline { aes | y <- Just 0.5 },
         xAxis { aes | label <- Just "X" },  
         yAxis { aes | label <- Just "Y" },
         title { aes | label <- Just "y = cos4x" }
         ]
--d1 = (distribution (\x -> 4 * (cos (0.5*x))) (0,2*pi) 200)
--d1 = (distribution (\x -> 2 + 4 * cos x) (-2*pi,2*pi) 200)
d1 = (distribution (\x -> cos (4*x)) (0,2*pi) 200)
--d1 = (distribution (\x -> sin (4*x)) (-pi,pi) 200)
--d1 = (distribution (\x -> C.pdfstandardnormal x) (-pi,pi) 200)
--d1 = (distribution (C.pdfnormal 190 7) (170,210) 200)

--d2 = (distribution (\x -> 2 + 4 * (cos (0.5*x))) (0,2*pi) 200)
--d2 = (distribution (\x -> 2 + 4 * cos x) (-2*pi,2*pi) 200)
d2 = (distribution (\x -> cos (4*x)) (0,2*pi) 200)
--d2 = (distribution (\x -> (sin (4*x))) (-pi,pi) 200)
--d2 = (distribution (\x -> C.pdfstandardnormal x) (-pi,pi) 200)
--d2 = (distribution (C.pdfnormal 190 7) (189,191) 200)

--d3 = discrete (\n -> C.fib (round n)) (0,8)
--d3 = discrete (\n -> toFloat <| (round n)+3) (0,8)
--d3 = discrete ((D.pdfpoisson 3) << (toFloat << round)) (0,10)
--d3 = discrete (\n -> List.sum <| List.map (D.pdfbinom 10 0.5) [1..(toFloat << round) n]) (0,15)
--d3 = discrete ((D.pdfbinom 10 0.5) << (toFloat << round)) (0,10)
--d3 = discrete (D.pdfbinom 60 0.25) (0,50)
--d3 = discrete (\n -> (1 + (1/n))^n) (1,50)




render m w =  
   let
      mouseY = toFloat <| snd m
      ypos = C.normalize (toFloat <| snd w,0) mouseY
      n' = (toFloat << floor) (60 * ypos)
      n = (if n' < 3 then 3 else n')
      --bin = D.pdfbinom n 0.25
      bin = D.binom n 0.25
      d3 = discrete ((\n -> n * 100) << (bin.pdf)) (0,60)
      extrageoms = [ 
         geom_vline { aes | x <- Just bin.mu, label <- Just "µ=", translate <- Just (4,0) },  
         geom_vline { aes | x <- Just n, label <- Just "n=", translate <- Just (0,-8) },  
         geom_hlinerange { aes | y <- Just (-4), limits <- Just (bin.mu - bin.sigma, bin.mu + bin.sigma), label <- Just ("&sigma;=" ++ (toString <| C.dec 2 bin.sigma)), translate <- Just (4,0) },  
         geom_hlinerange { aes | y <- Just (-6), limits <- Just (bin.mu - 2*bin.sigma, bin.mu + 2*bin.sigma), label <- Just "", translate <- Just (4,0) },  
         geom_hlinerange { aes | y <- Just (-8), limits <- Just (bin.mu - 3*bin.sigma, bin.mu + 3*bin.sigma), label <- Just "", translate <- Just (4,0) },  
      geom_area { aes | limits <- Just (bin.mu - bin.sigma, bin.mu + bin.sigma), colour <- Just blue, visibility <- Just 0.5, dynamic <- Just False }]
   in

      flow down [
      flow right 
      <| [ 
      --layers [ 
         plot (800,400) d1 geoms1 m w,
      --plot (800,600) d2 geoms2 m w ], layers [ 
         --plot (600,600) (distribution (\x -> cos (4*x)) (-pi,pi) 200) geoms3 m w 
         plot (400,400) d2 geoms3 m w 
   --, plot (600,600) { d2 | yScale <- C.normalize (-1,1) } geoms4 m w ]  
         ], flow right <|  
         [Html.toElement 400 200 <| Html.text ("This is text... x position : " ++ (toString (fst m))),
         plot (600,300) d3 (geoms4 ++ extrageoms) m w] ]

main = Signal.map2 render Mouse.position Window.dimensions


--geoms2 = [
--         geom_area { aes | fun <- Just (\x -> 0.2), colour <- Just darkRed, visibility <- Just 0.2 },
--         geom_point customAes,
--         geom_trace { aes | colour <- Just orange }
--         ]
geoms3 = [ 
         --background { aes | colour <- Just lightGrey }, 
         --geom_area_polar { aes | fun <- Just (\x -> 0.5), colour <- Just darkGreen, visibility <- Just 0.4, negate <- Just False },
         --geom_area_polar { aes | colour <- Just darkGreen, visibility <- Just 0.4, negate <- Just False, fun <- Just bl },
         --geom_trace_polar { aes | colour <- Just red },
         geom_trace_polar { aes | colour <- Just purple, dynamic <- Just True, negate <- Just True },
         --geom_curve_polar { aes | colour <- Just red, fit <- Just True },
         geom_position_polar customAes,
         geom_angle { aes | colour <- Just red, pointsize <- Just 6, translate <- Just (-0.5*pi,0.5) },
         geom_circle customAes,  
         --geom_hline_polar customAes,
         geom_vline_polar customAes,
         yAxis customAes,
         xAxis customAes,
         geom_hline { aes | y <- Just 0 },
         --geom_abline_polar { aes | colour <- Just orange, fun <- Just (\x -> 0.5*x), fit <- Just True },
         --geom_hline { aes | y <- Just 3.52 },
         --geom_hline { aes | y <- Just -3.52 },
         --geom_vline { aes | x <- Just pi },
         geom_vline { aes | x <- Just 0, annotate <- Just False }
         --geom_vline { aes | x <- Just (2*pi), annotate <- Just False }
         ]

--geoms4 = [
--         geom_area_polar { aes | fun <- Just (\x -> 0.2), colour <- Just darkRed, visibility <- Just 0.2 },
--         geom_position_polar { aes | colour <- Just orange },
--         geom_angle { aes | colour <- Just red, pointsize <- Just 6 },
--         geom_circle { aes | radius <- Just 0.4 }
--         ]

geoms4 = [  
          geom_mario { aes | dims <- Just (50,50) },
          --geom_hline aes, geom_vline aes,
          --geom_hline { customAes | y <- Just 1 },
          --geom_hline { aes | y <- Just e },
          --geom_hline { customAes | y <- Just 0.1 },
          geom_vline { customAes | x <- Just 15, annotate <- Just False },
          --geom_vline { customAes | x <- Just 0 },
          geom_points { aes | dynamic <- Just False, pointsize <- Just 3, colour <- Just darkRed },
          xAxis aes,  
          yAxis { aes | label <- Just "%" },
          geom_bar { aes | colour <- Just lightBlue, dynamic <- Just False },
          --geom_step { aes | colour <- Just red },
          --geom_curve { aes | colour <- Just green },
          --geom_tangent { aes | annotate <- Just False },
          title { aes | label <- Just "Bin(π=0.25,n=n) where 3 ≤ n ≤ 60" }]
