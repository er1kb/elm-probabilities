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
import Html.Attributes (align, style)
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
       xScale = (clamp 0 1) << C.normalize (0,width)
       yScale = (clamp 0 1) << C.normalize (height,0)
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
       freqTable l from to = map length <| map (\n -> filter (\n' -> (round n') == n) l) [from..to]
       ftx = freqTable xs 0 10
       fty = freqTable ys 0 10
       ft = discrete (\n -> (toFloat <| length <| filter (\(x,y) -> n == ((toFloat << round) x)) points) / (toFloat (sum ftx))) xlim (0,1)
       sdx = C.stddev xs
       nd = continuous (C.pdfnormal meanX sdx) xlim (0,1) 100
       g = fromPairs points xlim ylim
       lm = C.linreg points
       equation = "ŷ = " ++ (toString <| C.dec 2 lm.a) ++  
       if lm.b > 0
          then " + " ++ (toString <| C.dec 2 lm.b) 
          else " - " ++ (dropLeft 1 <| toString <| C.dec 2 lm.b) 
       ++ "x"
       titleText = if length points < 3 then "ŷ = mx + b" else equation
       lmGeoms = [ 
          background { aes | colour <- Just grey },
          geom_text { aes | x <- Just xpos, y <- Just ypos, precision <- Just 1 },
          --geom_point { aes | x <- Just xpos, y <- Just ypos, colour <- Just grey },
          geom_abline { aes | dynamic <- Just False, fun <- Just lm.f, colour <- Just orange },
          geom_vlineranges { aes | fun <- Just lm.f, linetype <- Just GC.dotted, colour <- Just lightRed },
          geom_vlinerange { aes | limits <- Just (lm.f xpos, ypos), linetype <- Just GC.dashed, colour <- Just black },
          title { aes | label <- Just titleText },
          geom_vline { aes | x <- Just meanX, linetype <- Just GC.dotted, colour <- Just blue, label <- Just "x&#772; = ", translate <- Just (1.2,-0.3) },
          geom_hline { aes | y <- Just meanY, linetype <- Just GC.dotted, colour <- Just blue, label <- Just "y&#772; = ", translate <- Just (0,-0.4) },
          xAxis axisAes,
          yAxis axisAes,

          geom_points { aes | colour <- Just darkBlue, visibility <- Just 0.4, pointsize <- Just 8 } 
          ]
       ftGeoms = [  
         geom_bar aes,
         xAxis aes,
         yAxis { aes | tickspacing <- Just 0.2 },
          geom_vline { aes | x <- Just meanX, linetype <- Just GC.dotted, colour <- Just blue, label <- Just "x&#772; = ", translate <- Just (1.2,-0.3) },
         geom_point { aes | colour <- Just darkGrey, x <- Just xpos, y <- Just (ypos / ymax) }
       ]
       ndGeoms = [ 
          geom_curve { aes | colour <- Just blue }
          ]
       tableAttr = [style [("border-collapse","collapse"),  
                           ("border-width", "0px"),  
                           ("border-style","solid"),
                           ("width","280px")] ]
       tdAttr = [align "center",  
                 style [("border-bottom","1pt solid black"),
                        ("width","70px")] ] 
   in
      flow down [
        flow right <|  
         [  plot (600,300) g lmGeoms (mx,my) (w,h),
               --Html.toElement 300 300 <| Html.text <| "signal: "  
               --++ toString points
            Html.toElement 300 300 <| 
               Html.table tableAttr (
                  Html.caption [] [ 
                     Html.text "List of added data points"
                     ] :: 
                  [Html.thead [] [ 
                     Html.th [] [Html.text "X"],
                     Html.th [] [Html.text "Y"],
                     Html.th [] [Html.text "Predicted"],
                     Html.th [] [Html.text "Residual"]
                     ]]  
                     ++
                  map (\(x,y) -> Html.tr [] [
                           Html.td tdAttr [Html.text (toString <| C.dec 2 x)],
                           Html.td tdAttr [Html.text (toString <| C.dec 2 y)],
                           Html.td tdAttr [Html.text (toString <| C.dec 2 (lm.f x))],  
                           Html.td tdAttr [Html.text (toString <| C.dec 2 (y - (lm.f x)))]  
                           ])  
                  (take 10 points)
                  ++ (if length points <= 10 then [] else 
                  [Html.tr [] <|
                     [Html.td tdAttr [Html.text "..."],
                      Html.td tdAttr [Html.text "..."],
                      Html.td tdAttr [Html.text "..."],
                      Html.td tdAttr [Html.text "..."]
                     ]
                  ])
                  ++
                  [Html.text ("stddev = " ++ (toString sdx))]
               )
                -- ++ toString (xScale mouseX,yScale mouseY)
         ],
         flow outward [
            plot (600,300) ft ftGeoms (mx,my) (w,h),
            plot (600,300) nd ndGeoms (mx,my) (w,h)
            ]
      ]
      
         

main = Signal.map3 render Window.dimensions Mouse.position clicks


axisAes = { aes | tickspacing <- Just 2 }
