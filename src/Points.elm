import Plot (..)
import Color (..)
import Graphics.Collage as GC
import Graphics.Element (Element, empty, image, flow, down, right, outward, layers, spacer)
import Signal
import Color (..)
--import Text
import Window
import Mouse
import List (..)
import String (dropLeft)
import Html
import Statistics.Continuous as C
import Statistics.Discrete as D
--import Statistics.Data.Mtcars (..)
--import Statistics.Data.Iris (..)


clicks = Signal.foldp (\pos l -> pos :: l) [(0,0), (600,0)]  
      (Signal.sampleOn Mouse.clicks Mouse.position)

render (w,h) (mx,my) points' =  
   let
       width = toFloat w
       height = toFloat h
       mouseX = toFloat mx
       mouseY = toFloat my
       xScale = C.normalize (0,width)
       yScale = C.normalize (height,0)
       xmax = 10
       xlim = (0,xmax)
       ymax = 10
       ylim = (0,ymax)
       xpos = xmax * xScale mouseX
       ypos = ymax * yScale mouseY
       points = map (\(x,y) -> (xmax * xScale (toFloat x), ymax * yScale (toFloat y)))  
         (if length points' > 2  
            then (take ((length points') - 2) points')  
            else points') 
       xs = map fst points
       ys = map snd points
       meanX = C.mean xs
       meanY = C.mean ys
       g = fromPairs points xlim ylim
       lm = C.linreg points
       equation = "ŷ = " ++ (toString <| C.dec 2 lm.a) ++  
       if lm.b > 0
          then " + " ++ (toString <| C.dec 2 lm.b) 
          else " - " ++ (dropLeft 1 <| toString <| C.dec 2 lm.b) 
       ++ "x"
       titleText = if length points < 3 then "ŷ = mx + b" else equation
       geoms = [ 
          background { aes | colour <- Just white },
          geom_text { aes | x <- Just xpos, y <- Just ypos, precision <- Just 1 },
          --geom_point { aes | x <- Just xpos, y <- Just ypos, colour <- Just grey },
          geom_abline { aes | dynamic <- Just False, fun <- Just lm.f, colour <- Just orange },
          geom_vlineranges { aes | fun <- Just lm.f, linetype <- Just GC.dotted, colour <- Just red },
          title { aes | label <- Just titleText },
          geom_vline { aes | x <- Just meanX, linetype <- Just GC.dotted, colour <- Just blue, label <- Just "Mean x = ", translate <- Just (1.2,-0.3) },
          geom_hline { aes | y <- Just meanY, linetype <- Just GC.dotted, colour <- Just blue, label <- Just "Mean y = ", translate <- Just (0,-0.4) },
          xAxis axisAes,
          yAxis axisAes,

          geom_points { aes | colour <- Just darkBlue, visibility <- Just 0.4, pointsize <- Just 8 } 
          ]
   in

        flow right <|  
         [plot (600,600) g geoms (mx,my) (w,h),
               Html.toElement 300 300 <| Html.text <| "signal: "  
               ++ toString points
                -- ++ toString (xScale mouseX,yScale mouseY)
         ]
      
         

main = Signal.map3 render Window.dimensions Mouse.position clicks


axisAes = { aes | tickspacing <- Just 2 }
