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
import Statistics.Data.Mtcars (..)
import Statistics.Data.Iris (..)


clicks = Signal.foldp (\pos l -> pos :: l) [(0,0)]  
      (Signal.sampleOn Mouse.clicks Mouse.position)

render (w,h) (mx,my) points' =  
   let
       lmmargins = (70,70)
       ftxmargins = (70,70)
       ftymargins = (70,70)
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
         (if length points' > 1  
            then (take ((length points') - 1) points')  
            else points') 
       xs = map fst points
       ys = map snd points
       meanX = C.mean xs
       meanY = C.mean ys
       freqTable l from to = map length <| map (\n -> filter (\n' -> (round n') == n) l) [from..to]
       ftableX = freqTable xs 0 10
       ftableY = freqTable ys 0 10
       ftx = discrete (\n -> (toFloat <| length <| filter (\(x,y) -> n == ((toFloat << round) x)) points) / (toFloat (sum ftableX))) xlim (0,1)
       fty = discrete (\n -> (toFloat <| length <| filter (\(x,y) -> n == ((toFloat << round) y)) points) / (toFloat (sum ftableY))) ylim (0,1)
       sdx = C.stddev xs
       sdy = C.stddev ys
       ndxdist = C.normal meanX sdx
       ndydist = C.normal meanY sdy
       --nd = continuous (C.pdfnormal meanX sdx) xlim (0,1) 100
       ndx = continuous ndxdist.pdf xlim (0,1) 100
       ndy = continuous ndydist.pdf ylim (0,1) 100
       g = fromPairs points xlim ylim
       lm = C.linreg points
       equation = "ŷ = " ++ (toString <| C.dec 2 lm.a) ++  
       if lm.b > 0
          then " + " ++ (toString <| C.dec 2 lm.b) 
          else " - " ++ (dropLeft 1 <| toString <| C.dec 2 lm.b) 
       ++ "x"
       titleText = if length points < 3 then "ŷ = mx + b" else equation
       r = C.correlate points
       lmGeoms = [ 
          background { aes | colour <- Just grey },
          geom_text { aes | x <- Just xpos, y <- Just ypos, precision <- Just 1 },
          --geom_point { aes | x <- Just xpos, y <- Just ypos, colour <- Just grey },
          geom_abline { aes | dynamic <- Just False, fun <- Just lm.f, colour <- Just orange },
          geom_vlineranges { aes | fun <- Just lm.f, linetype <- Just GC.dotted, colour <- Just lightRed },
          geom_vlinerange { aes | limits <- Just (lm.f xpos, ypos), linetype <- Just GC.dashed, colour <- Just black },
          title { aes | label <- Just titleText },
          geom_vline { aes | x <- Just meanX, linetype <- Just GC.dashed, colour <- Just blue, annotate <- Just False },
          geom_hline { aes | y <- Just meanY, linetype <- Just GC.dashed, colour <- Just blue, annotate <- Just False },
          xAxis { axisAes | label <- Just "X" },
          yAxis { axisAes | label <- Just "Y" },

          geom_points { aes | colour <- Just darkBlue, visibility <- Just 0.4, pointsize <- Just 8 },
          annotate_text { aes | translate <- Just (10.7,lm.f 10.7), label <- Just ("r=" ++ (toString r)) }
          ]
       ftxGeoms = [  
         geom_bar aes,
         --xAxis aes,
         --yAxis { aes | tickspacing <- Just 0.2 },
         geom_vline { aes | x <- Just meanX, linetype <- Just GC.dashed, colour <- Just blue, label <- Just "x&#772; = ", translate <- Just (1.2,-0.1) },
         geom_hlinerange { aes | y <- Just 0.7, linetype <- Just GC.dashed, colour <- Just blue, label <- Just ("&sigma; = " ++ toString (C.dec 2 ndxdist.sigma)), translate <- Just (-1,0), limits <- Just (ndxdist.mu - ndxdist.sigma, ndxdist.mu + ndxdist.sigma) }]

       ftyGeoms = [  
         background { aes | colour <- Just grey },
         geom_bar aes,
         --xAxis { aes | tickspacing <- Just 5 },
         --yAxis { aes | tickspacing <- Just 0.5 },
         geom_vline { aes | x <- Just meanY, linetype <- Just GC.dashed, colour <- Just blue, label <- Just "y&#772; = ", translate <- Just (1.2,-0.1) },
         geom_hlinerange { aes | y <- Just 0.7, linetype <- Just GC.dashed, colour <- Just blue, label <- Just ("&sigma; = " ++ toString (C.dec 2 ndydist.sigma)), translate <- Just (-1,0), limits <- Just (ndydist.mu - ndydist.sigma, ndydist.mu + ndydist.sigma) }]
       ndyGeoms = [ 
         geom_vlinerange { aes | x <- Just ypos, limits <- Just (0,0.5), annotate <- Just False },
         geom_point { aes | colour <- Just darkGrey, x <- Just ypos, y <- Just 0.5 },
         geom_curve { aes | colour <- Just blue }
         ]
       ndxGeoms = [ 
         background { aes | colour <- Just grey },
         --geom_area { aes | limits <- Just (ndxdist.mu - ndxdist.sigma, ndxdist.mu + ndxdist.sigma), dynamic <- Just False, colour <- Just darkBlue, visibility <- Just 0.4 }, 
         geom_vlinerange { aes | limits <- Just (0,0.5), annotate <- Just False },
         geom_point { aes | colour <- Just darkGrey, x <- Just xpos, y <- Just 0.5 },
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
         [   
         --flow outward [
            GC.collage 200 300  
            [ GC.rotate (pi/2) <| GC.toForm <| plot (300,200) ftymargins fty ftyGeoms (mx,my) (w,h),
            GC.rotate (pi/2) <| GC.toForm <| plot (300,200) ftymargins ndy ndyGeoms (mx,my) (w,h)
            ],
            plot (600,300) lmmargins g lmGeoms (mx,my) (w,h),
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
                  --++
                  --[Html.text ("stddev = " ++ (toString sdx))]
               )
                -- ++ toString (xScale mouseX,yScale mouseY)
         ],
         flow right <| [
            spacer 200 300,
         flow outward [
            plot (600,200) ftxmargins ndx ndxGeoms (mx,my) (w,h),
            plot (600,200) ftxmargins ftx ftxGeoms (mx,my) (w,h)
            ]
      ]
   ]
      
         

main = Signal.map3 render Window.dimensions Mouse.position clicks


axisAes = { aes | tickspacing <- Just 2, revolve <- Just True }
