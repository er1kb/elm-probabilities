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
import Html.Attributes as HA
import Continuous as C
import Discrete as D

render m w =  
   let
      mouseY = toFloat <| snd m
      ypos = C.normalize (toFloat <| snd w,0) mouseY
      n' = (toFloat << floor) (60 * ypos)
      n = (if n' < 4 then 4 else n')
      p = 0.25
      bin = D.binom n p
      bindist = discrete ((\n -> n * 100) << (bin.pdf)) (0,60)
      bindist2 = discrete bin.pdf (0,60)
      bingeoms = mkGeoms n bin
      bintitle = [title { aes | label <- Just ("Bin(π=0.25,n=" ++ toString n ++ ")") }] 
      mu = C.dec 2 bin.mu
      s = C.dec 2 bin.sigma
      po = D.poisson bin.mu
      podist = discrete ((\n -> n * 100) << (po.pdf)) (0,60)
      pogeoms = mkGeoms n po
      potitle = [title { aes | label <- Just ("Po(µ=" ++ toString mu ++ ")") } ]
      nd = C.normal bin.mu bin.sigma
      nddist = distribution ((\n -> n * 100) << (nd.pdf)) (0,60) 200
      ndgeoms = mkGeoms n nd 
      ndtitle = [title { aes | label <- Just ("N(µ=" ++ toString mu  
                                           ++ ",&sigma;=" ++ toString s ++ ")") } ]
      ndintegral = [annotate_integral { aes | translate <- Just (0.8,0.6), limits <- Just (mu-s,mu+s) }] 
   in

      flow down [ 
         Html.toElement 1000 50 <| Html.h1 [HA.align "center"] [Html.text "Binomialfördelningen och dess vänner"] 
                ,
         flow right <|  
            [Html.toElement 500 300 <| Html.text bintext,
            plot (600,300) bindist (geomsD ++ bingeoms ++ bintitle) m w], 

         flow right <| 
            [Html.toElement 500 300 <| Html.text potext,
            plot (600,300) podist (geomsD ++ pogeoms ++ potitle) m w], 

         flow right <| 
            [Html.toElement 500 300 <| Html.text ndtext,
               flow outward <| 
               [plot (600,300) nddist (geomsC ++ ndgeoms ++ ndtitle) m w,
               plot (600,300) nddist ndintegral m w]
            ]
         ]

main = Signal.map2 render Mouse.position Window.dimensions

geomsD = [  
          geom_mario { aes | dims <- Just (50,50) },
          geom_vline { aes | x <- Just 15, annotate <- Just False },
          geom_points { aes | dynamic <- Just False, pointsize <- Just 3, colour <- Just darkRed },
          xAxis { aes | label <- Just "X" },  
          yAxis { aes | label <- Just "%" },
          geom_bar { aes | colour <- Just lightBlue, dynamic <- Just False }]

geomsC = [  
          geom_mario { aes | dims <- Just (50,50) },
          geom_vline { aes | x <- Just 15, annotate <- Just False },
          geom_vline { aes | x <- Just 20, annotate <- Just False, colour <- Just red, linetype <- Just GC.dotted },
          --geom_points { aes | dynamic <- Just False, pointsize <- Just 3, colour <- Just darkRed },
          geom_curve { aes | dynamic <- Just False, colour <- Just grey },
          xAxis { aes | label <- Just "X" },  
          yAxis { aes | label <- Just "%" }
          --geom_bar { aes | colour <- Just lightBlue, dynamic <- Just False }  
          ]

mkGeoms n pdist = [ 
   geom_vline { aes | x <- Just pdist.mu, label <- Just "µ=", translate <- Just (4,0) },  
   geom_vline { aes | x <- Just n, label <- Just "n=", translate <- Just (0,-8) },  
   geom_hlinerange { aes | y <- Just (-4), limits <- Just (pdist.mu - pdist.sigma, pdist.mu + pdist.sigma), label <- Just ("&sigma;=" ++ (toString <| C.dec 2 pdist.sigma)), translate <- Just (4,0) },  
   geom_hlinerange { aes | y <- Just (-6), limits <- Just (pdist.mu - 2*pdist.sigma, pdist.mu + 2*pdist.sigma), label <- Just "", translate <- Just (4,0) },  
   geom_hlinerange { aes | y <- Just (-8), limits <- Just (pdist.mu - 3*pdist.sigma, pdist.mu + 3*pdist.sigma), label <- Just "", translate <- Just (4,0) },  
   geom_area { aes | limits <- Just (pdist.mu - pdist.sigma, pdist.mu + pdist.sigma), colour <- Just blue, visibility <- Just 0.5, dynamic <- Just False } ] 

bintext = "Binomialfördelningen beskriver sannolikheten för en händelse med två ömsesidigt uteslutande utfall och ett känt antal försök. Det gäller till exempel olika typer av spel, där utfallen är vinst och förlust. Varje försök är oberoende: utfallet av ett tärningskast beror inte på det föregående kastet. För att konstruera denna fördelning behövs två komponenter: sannolikheten för att händelsen inträffar (π) och antal försök (n). I verkligheten sker en slumpmässig variation men i det långa loppet kommer antalet positiva utfall att närma sig väntevärdet (µ), som enkelt beräknas genom n * π. Binomialfördelningen är en diskret fördelning, eftersom x-värdena (0..n) utgörs av heltal. Allt eftersom antalet försök växer blir binomialfördelningen mer omständlig att beräkna, och det kan därför vara en god idé att \"approximera\" den med en liknande fördelning."

potext = "Poissonfördelningen är en annan diskret sannolikhetsfördelning som kan användas för att efterlikna binomialfördelningen. Eftersom poissonfördelningen används för mer ovanliga/osannolika händelser så passar den bra för stora n och små π, dvs ett stort antal försök och liten sannolikhet för positivt utfall. En tumregel är n ≥ 10 och π ≤ 0,1. Med andra ord bör den inte användas som här då π=0.25. Poängen med diagrammet är snarare att visa fördelningarna och hur de liknar varandra. Poissonfördelningen har en något plattare form och har därmed en större standardavvikelse, dvs att det totala utfallet förväntas variera inom ett större intervall. Standardavvikelsen är för övrigt roten ur väntevärdet. "

ndtext = "Normalfördelningen är en välkänd kontinuerlig fördelning som kan användas istället för binomialfördelningen om en av flera möjliga tumregler uppfylls, till exempel nπ≥5 och n(1-π)≥5. Här betyder det att n≥20. Fördelningen N(µ,σ) konstrueras med väntevärdet och standardavvikelsen från den ursprungliga binomialfördelningen. Om det vi försöker mäta liknar en normalfördelning så kan vi räkna med att ungefär två tredjedelar (68,3%) av utfallen sker inom ±1 standardavvikelse omkring väntevärdet. "