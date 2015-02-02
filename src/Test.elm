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


const = 4
nsteps = 400


geoms1 = [
         geom_curve customAes,
         geom_point customAes,
         geom_trace_polar { aes | colour <- Just purple, dynamic <- Just True, revolve <- Just True, negate <- Just True },

         geom_angle { aes | colour <- Just red, dynamic <- Just True, revolve <- Just True },
         geom_trace { aes | colour <- Just orange },
         geom_vline { customAes | label <- Just "x = ", translate <- Just (0.5,0) },
         geom_hline { aes | annotate <- Just False, colour <- Just darkGrey, y <- Just 0 },
         geom_hline { aes | annotate <- Just False, colour <- Just darkGrey, y <- Just 1 },
         geom_hline { aes | annotate <- Just False, colour <- Just darkGrey, y <- Just -1 },
         xAxis { aes | label <- Just "X" },  
         yAxis { aes | label <- Just "Y" }
         ]

customAes = { aes | radius <- Just 2, pointsize <- Just 9, colour <- Just darkBlue, visibility <- Just 0.6, annotate <- Just True }



render m w =  
   let
      mouseY = toFloat <| snd m
      mouseX = toFloat <| fst m
      ypos = C.normalize (0.9 * (toFloat << snd) w,0.2 * (toFloat << snd) w) mouseY
      xpos = C.normalize (toFloat <| fst w,0) mouseX
      n' = (toFloat << floor) (400 * (C.dec 2 ypos))
      n = if | n' < 0 -> 0
             | n' > 400 -> 400 
             | otherwise -> n'
      const = C.dec 2 <| n * 0.01
      d1 = (continuous (\x -> cos (const*x)) (0,2*pi) (-1,1) 200)
      d2 = (continuous (\x -> sin (const*x)) (0,2*pi) (-1,1) 200)
      --tpls = [(1,-2.1),(1.1,-1.5),(pi,0),(1.5*pi,1.75)]
      d3 = fromPairs iris (0,7) (0,3)
      regr = C.linreg iris
      regressionLine = [ geom_abline { aes | fun <- Just regr.f, dynamic <- Just False }]
      plotTitle1 = [ title { aes | label <- Just ("y = cos" ++ toString const ++ "x") }]
      plotTitle2 = [ title { aes | label <- Just ("y = sin" ++ toString const ++ "x") }]
   in

      flow down 
      [  
        flow right <|  
         [plot (1000,300) d1 (geoms1 ++ plotTitle1) m w,
              plot (300,300) d1 geoms3 m w 
              ],
        flow right <|  
         [plot (1000,300) d2 (geoms1 ++ plotTitle2) m w,
              plot (300,300) d2 geoms3 m w 
              ],
        flow right <|  
         [plot (1000,200) d3 (geoms4 ++ regressionLine) m w,
               Html.toElement 300 200 <| Html.text <| "regression model: " ++ toString regr
              
              ]

          ]
         

main = Signal.map2 render Mouse.position Window.dimensions


geoms3 = [ 
         geom_trace_polar { aes | colour <- Just purple, dynamic <- Just True, negate <- Just True },
         geom_position_polar customAes,
         geom_angle { aes | colour <- Just red, pointsize <- Just 6, translate <- Just (-0.5*pi,0.5) },
         geom_circle customAes,  
         geom_vline_polar customAes,
         geom_hline { aes | y <- Just 0, annotate <- Just False, colour <- Just darkGrey },
         geom_vline { aes | x <- Just pi, annotate <- Just False, colour <- Just darkGrey }
         ]

geoms4 = [ 
      geom_points { aes | dynamic <- Just False}, 
      geom_hline { aes | y <- Just 0, annotate <- Just False, colour <- Just darkGrey },
      geom_hline { aes | y <- Just (-1.26), annotate <- Just False, colour <- Just darkGrey },
      geom_vline { aes | x <- Just pi, annotate <- Just False, colour <- Just darkGrey },
      title { aes | label <- Just "iris$Petal.Width ~ iris$Petal.Length" },
      xAxis aes, yAxis aes ]


iris = [(1.4,0.2), (1.4,0.2), (1.3,0.2), (1.5,0.2), (1.4,0.2), (1.7,0.4), (1.4,0.3), (1.5,0.2), (1.4,0.2), (1.5,0.1), (1.5,0.2), (1.6,0.2), (1.4,0.1), (1.1,0.1), (1.2,0.2), (1.5,0.4), (1.3,0.4), (1.4,0.3), (1.7,0.3), (1.5,0.3), (1.7,0.2), (1.5,0.4), (1,0.2), (1.7,0.5), (1.9,0.2), (1.6,0.2), (1.6,0.4), (1.5,0.2), (1.4,0.2), (1.6,0.2), (1.6,0.2), (1.5,0.4), (1.5,0.1), (1.4,0.2), (1.5,0.2), (1.2,0.2), (1.3,0.2), (1.4,0.1), (1.3,0.2), (1.5,0.2), (1.3,0.3), (1.3,0.3), (1.3,0.2), (1.6,0.6), (1.9,0.4), (1.4,0.3), (1.6,0.2), (1.4,0.2), (1.5,0.2), (1.4,0.2), (4.7,1.4), (4.5,1.5), (4.9,1.5), (4,1.3), (4.6,1.5), (4.5,1.3), (4.7,1.6), (3.3,1), (4.6,1.3), (3.9,1.4), (3.5,1), (4.2,1.5), (4,1), (4.7,1.4), (3.6,1.3), (4.4,1.4), (4.5,1.5), (4.1,1), (4.5,1.5), (3.9,1.1), (4.8,1.8), (4,1.3), (4.9,1.5), (4.7,1.2), (4.3,1.3), (4.4,1.4), (4.8,1.4), (5,1.7), (4.5,1.5), (3.5,1), (3.8,1.1), (3.7,1), (3.9,1.2), (5.1,1.6), (4.5,1.5), (4.5,1.6), (4.7,1.5), (4.4,1.3), (4.1,1.3), (4,1.3), (4.4,1.2), (4.6,1.4), (4,1.2), (3.3,1), (4.2,1.3), (4.2,1.2), (4.2,1.3), (4.3,1.3), (3,1.1), (4.1,1.3), (6,2.5), (5.1,1.9), (5.9,2.1), (5.6,1.8), (5.8,2.2), (6.6,2.1), (4.5,1.7), (6.3,1.8), (5.8,1.8), (6.1,2.5), (5.1,2), (5.3,1.9), (5.5,2.1), (5,2), (5.1,2.4), (5.3,2.3), (5.5,1.8), (6.7,2.2), (6.9,2.3), (5,1.5), (5.7,2.3), (4.9,2), (6.7,2), (4.9,1.8), (5.7,2.1), (6,1.8), (4.8,1.8), (4.9,1.8), (5.6,2.1), (5.8,1.6), (6.1,1.9), (6.4,2), (5.6,2.2), (5.1,1.5), (5.6,1.4), (6.1,2.3), (5.6,2.4), (5.5,1.8), (4.8,1.8), (5.4,2.1), (5.6,2.4), (5.1,2.3), (5.1,1.9), (5.9,2.3), (5.7,2.5), (5.2,2.3), (5,1.9), (5.2,2), (5.4,2.3), (5.1,1.8)]
