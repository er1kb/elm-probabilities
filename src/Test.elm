import Plot (..)
import Color (..)
import Graphics.Collage as GC
import Graphics.Element (Element, empty, image, flow, down, right, outward, layers, spacer)
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
--bl = (\x -> 0)

geoms1 = [
         geom_curve customAes,
         geom_point customAes,
         geom_trace_polar { aes | colour <- Just purple, dynamic <- Just True, rotate <- Just True, negate <- Just True },

         geom_angle { aes | colour <- Just red, dynamic <- Just True, rotate <- Just True },
         geom_trace { aes | colour <- Just orange },
         geom_vline { customAes | label <- Just "x = ", translate <- Just (0.5,0) },
         geom_hline { aes | annotate <- Just False, colour <- Just darkGrey, y <- Just 0 },
         geom_hline { aes | annotate <- Just False, colour <- Just darkGrey, y <- Just 1 },
         geom_hline { aes | annotate <- Just False, colour <- Just darkGrey, y <- Just -1 },
         xAxis { aes | label <- Just "X" },  
         yAxis { aes | label <- Just "Y" }
         ]

customAes = { aes | radius <- Just 2, pointsize <- Just 9, colour <- Just darkBlue, visibility <- Just 0.6, annotate <- Just True }
--geoms1 = [ 
--         --background { aes | colour <- Just lightGrey }, 
--         --geom_area { aes | fun <- Just (\x -> 0.5), colour <- Just grey, dynamic <- Just True, negate <- Just False },
--         --geom_area { aes | colour <- Just darkGreen, visibility <- Just 0.4, dynamic <- Just True, negate <- Just True },
--         --geom_trapezoid { aes | fun <- Just bl },
--         --geom_bar { aes | fun <- Just (\x -> sin (4*x)), colour <- Just darkBlue },
--         --geom_step customAes,
--         geom_curve customAes,
--         geom_point customAes,
--         --geom_trace_polar customAes,
--         geom_trace_polar { aes | colour <- Just purple, dynamic <- Just True, rotate <- Just True, negate <- Just True },
--         --geom_area_polar { aes | colour <- Just lightBlue, dynamic <- Just True, rotate <- Just True, negate <- Just True, limits <- Just (-pi/2,pi/2) },
--         --geom_curve_polar { aes | colour <- Just purple, dynamic <- Just True, rotate <- Just True },
--         --geom_circle { aes | colour <- Just purple, dynamic <- Just True, rotate <- Just True },
--         geom_angle { aes | colour <- Just red, dynamic <- Just True, rotate <- Just True },
--         geom_trace { aes | colour <- Just orange },
--         --geom_hline customAes,
--         geom_vline { customAes | label <- Just "x = ", translate <- Just (0.5,0) },
--         geom_abline { customAes | limits <- Just (pi/2,6*pi/4), dynamic <- Just False, fun <- Just (\x -> cos (4*x)), linetype <- Just GC.dotted },
--         --geom_tangent { aes | colour <- Just red, delta <- Just 1.51, translate <- Just (-2.6,0.5), precision <- Just 1 },
--         --geom_hline { aes | y <- Just 0.5 },
--         xAxis { aes | label <- Just "X" },  
--         yAxis { aes | label <- Just "Y" },
--         title { aes | label <- Just "y = cos4x" }
--         ]
--d1 = (distribution (\x -> 4 * (cos (0.5*x))) (0,2*pi) 200)
--d1 = (distribution (\x -> 2 + 4 * cos x) (-2*pi,2*pi) 200)
--d1 = (distribution (\x -> cos (4*x)) (0,2*pi) 200)
--d1 = (distribution (\x -> sin (4*x)) (-pi,pi) 200)
--d1 = (distribution (\x -> C.pdfstandardnormal x) (-pi,pi) 200)
--d1 = (distribution (C.pdfnormal 190 7) (170,210) 200)

--d2 = (distribution (\x -> 2 + 4 * (cos (0.5*x))) (0,2*pi) 200)
--d2 = (distribution (\x -> 2 + 4 * cos x) (-2*pi,2*pi) 200)
--d2 = (distribution (\x -> cos (4*x)) (0,2*pi) 200)
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
      mouseX = toFloat <| fst m
      ypos = C.normalize (0.9 * (toFloat << snd) w,0.2 * (toFloat << snd) w) mouseY
      --ypos = C.normalize (toFloat <| snd w,0) mouseY
      xpos = C.normalize (toFloat <| fst w,0) mouseX
      --n' = (toFloat << floor) (4 * ypos)
      n' = (toFloat << floor) (400 * (C.dec 2 ypos))
      --n = (if n' < 0 then 0 else n')
      n = if | n' < 0 -> 0
             | n' > 400 -> 400 
             | otherwise -> n'
      const = n * 0.01
      --bin = D.pdfbinom n 0.25
      --bin = D.binom n 0.25
      --d3 = discrete ((\n -> n * 100) << (bin.pdf)) (0,60)
      d1 = (distribution (\x -> cos (const*x)) (0,2*pi) (-1,1) 200)
      d2 = (distribution (\x -> sin (const*x)) (0,2*pi) (-1,1) 200)
      plotTitle1 = [ title { aes | label <- Just ("y = cos" ++ toString const ++ "x") }]
      plotTitle2 = [ title { aes | label <- Just ("y = sin" ++ toString const ++ "x") }]
      --extrageoms = [ 
      --   geom_vline { aes | x <- Just bin.mu, label <- Just "µ=", translate <- Just (4,0) },  
      --   geom_vline { aes | x <- Just n, label <- Just "n=", translate <- Just (0,-8) },  
      --   geom_hlinerange { aes | y <- Just (-4), limits <- Just (bin.mu - bin.sigma, bin.mu + bin.sigma), label <- Just ("&sigma;=" ++ (toString <| C.dec 2 bin.sigma)), translate <- Just (4,0) },  
      --   geom_hlinerange { aes | y <- Just (-6), limits <- Just (bin.mu - 2*bin.sigma, bin.mu + 2*bin.sigma), label <- Just "", translate <- Just (4,0) },  
      --   geom_hlinerange { aes | y <- Just (-8), limits <- Just (bin.mu - 3*bin.sigma, bin.mu + 3*bin.sigma), label <- Just "", translate <- Just (4,0) },  
      --geom_area { aes | limits <- Just (bin.mu - bin.sigma, bin.mu + bin.sigma), colour <- Just blue, visibility <- Just 0.5, dynamic <- Just False }, title { aes | label <- Just ("Bin(π=0.25,n=" ++ toString n ++ ")") } ]
   in

      flow down 
      [  
        flow right <|  
         [plot (1000,300) d1 (geoms1 ++ plotTitle1) m w,
             --spacer 450 300,
              plot (300,300) d1 geoms3 m w 
              ],
        flow right <|  
         [plot (1000,300) d2 (geoms1 ++ plotTitle2) m w,
             --spacer 450 300,
              plot (300,300) d2 geoms3 m w 
              ]

          ]
         

main = Signal.map2 render Mouse.position Window.dimensions


--geoms2 = [
--         geom_area { aes | fun <- Just (\x -> 0.2), colour <- Just darkRed, visibility <- Just 0.2 },
--         geom_point customAes,
--         geom_trace { aes | colour <- Just orange }
--         ]
geoms3 = [ 
         geom_trace_polar { aes | colour <- Just purple, dynamic <- Just True, negate <- Just True },
         geom_position_polar customAes,
         geom_angle { aes | colour <- Just red, pointsize <- Just 6, translate <- Just (-0.5*pi,0.5) },
         geom_circle customAes,  
         --geom_hline_polar customAes,
         geom_vline_polar customAes,
         --yAxis customAes,
         --xAxis customAes,
         geom_hline { aes | y <- Just 0, annotate <- Just False, colour <- Just darkGrey },
         geom_vline { aes | x <- Just pi, annotate <- Just False, colour <- Just darkGrey }
         ]

--geoms4 = [
--         geom_area_polar { aes | fun <- Just (\x -> 0.2), colour <- Just darkRed, visibility <- Just 0.2 },
--         geom_position_polar { aes | colour <- Just orange },
--         geom_angle { aes | colour <- Just red, pointsize <- Just 6 },
--         geom_circle { aes | radius <- Just 0.4 }
--         ]

--geoms4 = [  
--          geom_mario { aes | dims <- Just (50,50) },
--          --geom_hline aes, geom_vline aes,
--          --geom_hline { customAes | y <- Just 1 },
--          --geom_hline { aes | y <- Just e },
--          --geom_hline { customAes | y <- Just 0.1 },
--          geom_vline { customAes | x <- Just 15, annotate <- Just False },
--          --geom_vline { customAes | x <- Just 0 },
--          geom_points { aes | dynamic <- Just False, pointsize <- Just 3, colour <- Just darkRed },
--          xAxis aes,  
--          yAxis { aes | label <- Just "%" },
--          geom_bar { aes | colour <- Just lightBlue, dynamic <- Just False } ]
