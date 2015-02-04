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
      --vis = if ypos < 0 then 0 else ypos
      n' = (toFloat << floor) (400 * (C.dec 2 ypos))
      n = if | n' < 0 -> 0
             | n' > 400 -> 400 
             | otherwise -> n'
      const = C.dec 2 <| n * 0.01
      d1 = (continuous (\x -> cos (const*x)) (0,2*pi) (-1,1) 200)
      d2 = (continuous (\x -> sin (const*x)) (0,2*pi) (-1,1) 200)
      --tpls = [(1,-2.1),(1.1,-1.5),(pi,0),(1.5*pi,1.75)]
      cars = List.map2 (,) hp mpg
      --iris = List.map2 (,) petalLength petalWidth
      --cor = C.correlate iris
      --d3 = fromPairs iris (0,7) (-2,5)
      d3 = fromPairs cars (0,340) (10,35)
      --regr = C.linreg iris
      regr = C.linreg cars
      regressionLine = [ geom_abline { aes | fun <- Just regr.f, dynamic <- Just False, limits <- Just (0,300) }]
      regressionPoints = [  
      residuals { aes | fun <- Just regr.f, colour <- Just green, linetype <- Just GC.dotted }, 
         geom_points { aes | dynamic <- Just False, colour <- Just blue, pointsize <- Just 4, visibility <- Just 1 }
      ]
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
         [plot (1000,200) d3 (geoms4 ++ regressionPoints ++ regressionLine) m w,
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
      geom_hline { aes | y <- Just 0, annotate <- Just False, colour <- Just darkGrey },
      geom_hline { aes | y <- Just (-1.26), annotate <- Just False, colour <- Just darkGrey },
      --geom_vline { aes | x <- Just pi, annotate <- Just False, colour <- Just darkGrey },
      title { aes | label <- Just "Linear regression: mpg ~ hp" },
      xAxis { aes | tickspacing <- Just 50, translate <- Just (0,-5) },  
      yAxis aes ]

