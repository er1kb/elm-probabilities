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




--type alias Input = Signal { x : Int,
--                            y : Int, 
--                            width : Int, 
--                            height : Int
--                           }
--input : Input
--input = Signal.map2 (\(mx,my) (w,h) -> { x = mx, y = my, width = w, height = h}) Mouse.position Window.dimensions

--bl = (\x -> 2 + 4 * sin (1*x))
bl = (\x -> 0)

customAes = { aes | radius <- Just 2, txt <- Just "grrRRr!", pointsize <- Just 9, colour <- Just darkBlue, visibility <- Just 0.6 }
geoms1 = [ 
         --background { aes | colour <- Just lightGrey }, 
         --geom_integral { aes | baseline <- Just (\x -> 0.5), colour <- Just grey, dynamic <- Just True, negate <- Just False },
         --geom_integral { aes | colour <- Just darkGreen, visibility <- Just 0.4, dynamic <- Just True, negate <- Just False },
         --geom_trapezoid { aes | baseline <- Just bl },
         geom_bar { aes | baseline <- Just (\x -> sin (4*x)), colour <- Just darkBlue },
         --geom_step customAes,
         --geom_curve aes,
         --geom_point customAes,
         --geom_trace_polar customAes,
         geom_trace { aes | colour <- Just orange },
         geom_hline customAes,
         geom_vline customAes,
         --geom_tangent { aes | colour <- Just red, delta <- Just 1.5, translate <- Just (-2.6,0.5) },
         --geom_hline { aes | y <- Just 0.5 },
         xAxis { aes | axis <- Just { labels = ("X", "_") } },  
         yAxis { aes | axis <- Just { labels = ("_", "Y") }},
         title { aes | txt <- Just "y = 2 + 4cosx" }
         ]
--d1 = (distribution (\x -> 4 * (cos (0.5*x))) (0,2*pi) 200)
d1 = (distribution (\x -> 2 + 4 * cos x) (-2*pi,2*pi) 200)
--d1 = (distribution (\x -> cos (4*x)) (-pi,pi) 200)
--d1 = (distribution (\x -> sin (4*x)) (-pi,pi) 200)
--d1 = (distribution (\x -> C.pdfstandardnormal x) (-pi,pi) 200)
--d1 = (distribution (C.pdfnormal 190 7) (170,210) 200)
--d2 = (distribution (\x -> 2 + 4 * (cos (0.5*x))) (0,2*pi) 200)
d2 = (distribution (\x -> 2 + 4 * cos x) (-2*pi,2*pi) 200)
--d2 = (distribution (\x -> cos (4*x)) (-pi,pi) 200)
--d2 = (distribution (\x -> (sin (4*x))) (-pi,pi) 200)
--d2 = (distribution (\x -> C.pdfstandardnormal x) (-pi,pi) 200)
--d2 = (distribution (C.pdfnormal 190 7) (170,210) 200)

--d3 = discrete (\n -> C.fib (round n)) (0,8)
--d3 = discrete (\n -> toFloat <| (round n)+3) (0,8)
--d3 = discrete (D.pdfpoisson 1) (0,8)
d3 = discrete (\n -> List.sum <| List.map (D.pdfbinom 10 0.5) [1..n]) (0,10)

--fibs = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181]


render m w =  
   flow down [
   flow right 
   <| [ 
   --layers [ 
      plot (800,600) d1 geoms1 m w,
    --plot (800,600) d2 geoms2 m w ], layers [ 
      --plot (600,600) (distribution (\x -> cos (4*x)) (-pi,pi) 200) geoms3 m w 
      plot (500,600) d2 geoms3 m w 
--, plot (600,600) { d2 | yScale <- C.normalize (-1,1) } geoms4 m w ]  
      ], flow right <|  
      [Html.toElement 400 200 <| Html.text ("This is text... x position : " ++ (toString (fst m))),
      plot (400,400) d3 geoms4 m w] ]

main = Signal.map2 render Mouse.position Window.dimensions


--geoms2 = [
--         geom_integral { aes | baseline <- Just (\x -> 0.2), colour <- Just darkRed, visibility <- Just 0.2 },
--         geom_point customAes,
--         geom_trace { aes | colour <- Just orange }
--         ]
geoms3 = [ 
         --background { aes | colour <- Just lightGrey }, 
         --geom_integral_polar { aes | baseline <- Just (\x -> 0.5), colour <- Just darkGreen, visibility <- Just 0.4, negate <- Just False },
         geom_integral_polar { aes | colour <- Just darkGreen, visibility <- Just 0.4, negate <- Just False, baseline <- Just bl },
         geom_trace_polar { aes | colour <- Just red },
         --geom_curve_polar { aes | colour <- Just red },
         geom_position_polar customAes,
         geom_angle { aes | colour <- Just red, pointsize <- Just 6 },
         geom_circle customAes,  
         --geom_hline_polar customAes,
         geom_vline_polar customAes,
         yAxis customAes,
         xAxis customAes,
         geom_hline { aes | y <- Just 0 },
         --geom_hline { aes | y <- Just 3.52 },
         --geom_hline { aes | y <- Just -3.52 },
         --geom_vline { aes | x <- Just pi },
         geom_vline { aes | x <- Just 0 }
         ]

--geoms4 = [
--         geom_integral_polar { aes | baseline <- Just (\x -> 0.2), colour <- Just darkRed, visibility <- Just 0.2 },
--         geom_position_polar { aes | colour <- Just orange },
--         geom_angle { aes | colour <- Just red, pointsize <- Just 6 },
--         geom_circle { aes | radius <- Just 0.4 }
--         ]

geoms4 = [  
          geom_hline aes, geom_vline aes,
          --geom_hline { customAes | y <- Just 0 },
          --geom_vline { customAes | x <- Just 0 },
          geom_points { aes | dynamic <- Just False },
          xAxis aes, yAxis aes,
          geom_bar { aes | colour <- Just lightBlue, dynamic <- Just False },
          geom_step { aes | colour <- Just red },
          title { aes | txt <- Just "0 ≤ Bin(π=0.5,n=10) ≤ 10" }]
