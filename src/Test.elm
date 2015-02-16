import Plot (..)
import Color (..)
import Graphics.Collage as GC
import Graphics.Element (Element, empty, image, flow, down, right, outward, layers, spacer)
import Signal
import Color (..)
--import Text
import Window
import Mouse
import List
import Html
import Statistics.Continuous as C
import Statistics.Discrete as D
import Statistics.Data.Mtcars (..)
import Statistics.Data.Iris (..)





geoms1 = [
         geom_curve customAes,
         --geom_step { customAes | nsteps <- Just 20 },
         --geom_bar { customAes | nsteps <- Just 50 },
         annotate_integral { customAes | translate <- Just (0.1,0.7) },
         geom_point customAes,
         geom_point { aes | y <- Just 1, colour <- Just red, pointsize <- Just 6 },
         geom_trace_polar { aes | colour <- Just purple, dynamic <- Just True, revolve <- Just True, negate <- Just False },

         --geom_text { customAes | translate <- Just (0,20), precision <- Just 1 },
         geom_trace { aes | colour <- Just orange },
         geom_vline { customAes | label <- Just "x = ", translate <- Just (0.5,0) },
         geom_hline { aes | annotate <- Just False, colour <- Just darkGrey, y <- Just 0 },
         geom_hline { aes | annotate <- Just False, colour <- Just darkGrey, y <- Just 1 },
         geom_hline { aes | annotate <- Just False, colour <- Just darkGrey, y <- Just -1 },
         xAxis { aes | label <- Just "X", tickspacing <- Just 2 },  
         yAxis { aes | label <- Just "Y", tickspacing <- Just 0.5 }
         --geom_image { aes | label <- Just "http://img.informer.com/icons/png/48/3136/3136259.png", dims <- Just (50,50), theta <- Just pi }
         ]

customAes = { aes | radius <- Just 2, pointsize <- Just 9, colour <- Just darkBlue, visibility <- Just 0.6, annotate <- Just True }



render m w =  
   let
      margins = (70,70)
      mouseY = toFloat <| snd m
      mouseX = toFloat <| fst m
      ypos = C.normalize (0.9 * (toFloat << snd) w,0.2 * (toFloat << snd) w) mouseY
      xpos = C.normalize (0, toFloat <| snd w) mouseX
      n' = (toFloat << floor) (400 * (C.dec 2 ypos))
      n = if | n' < 0 -> 0
             | n' > 400 -> 400 
             | otherwise -> n'
      const = C.dec 2 <| n * 0.01
      d1 = (continuous (\x -> cos (const*x)) (0,2*pi) (-1,1) 200)
      d2 = (continuous (\x -> sin (const*x)) (0,2*pi) (-1,1) 200)
      plotTitle1 = [ title { aes | label <- Just ("y = cos" ++ toString const ++ "x") }]
      plotTitle2 = [ title { aes | label <- Just ("y = sin" ++ toString const ++ "x") }]
      --polar_abline = [ geom_abline_polar { aes | fun <- Just (\x -> 0.1 * x), dynamic <- Just True, negate <- Just True, colour <- Just blue } ]
      --d3 = (continuous (\x -> x/(8*pi)) (0,8*pi) (-1,1) 200)
   in

      flow down 
      [  
        flow right <|  
         [plot (1000,300) margins d1 (geoms1 ++ plotTitle1) m w,
              plot (300,300) margins d1 geoms2 m w 
              ],
        flow right <|  
         [plot (1000,300) margins d2 (geoms1 ++ plotTitle2) m w,
              plot (300,300) margins d2 geoms2 m w 
              ]
--
--        flow outward <|  
--         [ 
--              plot (1000,300) margins1 d3 geoms3 m w
--
--              ]

          ]
         

main = Signal.map2 render Mouse.position Window.dimensions


geoms2 = [ 
         geom_trace_polar { aes | colour <- Just purple, dynamic <- Just True, negate <- Just True },
         geom_position_polar customAes,
         geom_angle { aes | colour <- Just red, pointsize <- Just 6, translate <- Just (-0.5*pi,0.5) },
         geom_circle customAes,  
         geom_vline_polar customAes,
         geom_hline { aes | y <- Just 0, annotate <- Just False, colour <- Just darkGrey },
         geom_vline { aes | x <- Just pi, annotate <- Just False, colour <- Just darkGrey }
         ]

geoms3 = [ 
   geom_curve_polar { aes | revolve <- Just False, fit <- Just True },
   --geom_circle { aes | revolve <- Just False, fit <- Just True },
   xAxis { aes | tickspacing <- Just 5 }

   ]
