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
import Statistics.Continuous as C
import Statistics.Discrete as D
--import Statistics.Data.Mtcars
--import Statistics.Data.Iris


render m w =  
   let
      margins = (70,70)
      mouseY = toFloat <| snd m
      --ypos = C.normalize ((toFloat << snd) w,0) mouseY
      ypos = C.normalize (0.95 * (toFloat << snd) w,0.1 * (toFloat << snd) w) mouseY
      n' = (toFloat << floor) (60 * ypos)
      --n = (if n' < 4 then 4 else n')
      n = if | n' < 4 -> 4
             | n' > 60 -> 60
             | otherwise -> n'
      --n = clamp 4 60 n'
      p = 0.25
      --p = C.dec 3 (1/6)
      --ylim = (-9,50) 
      ylim = (-0.09,0.5) 
      bin = D.binom n p
      --bindist = discrete ((\n -> n * 100) << (bin.pdf)) (0,60) ylim
      bindist = discrete bin.pdf (0,60) ylim
      bindist2 = discrete bin.pdf (0,60) ylim
      bingeoms = mkGeoms n bin True
      bintitle = [title { aes | label <- Just ("Bin(π=" ++ toString p ++ ",n=" ++ toString n ++ ")") }] 
      mu = C.dec 2 bin.mu
      s = C.dec 2 bin.sigma
      po = D.poisson bin.mu
      --podist = discrete ((\n -> n * 100) << (po.pdf)) (0,60) ylim
      podist = discrete po.pdf (0,60) ylim
      pogeoms = mkGeoms n po False
      potitle = [title { aes | label <- Just ("Po(µ=" ++ toString mu ++ ")") } ]
      nd = C.normal bin.mu bin.sigma
      --nddist = continuous ((\n -> n * 100) << (nd.pdf)) (0,60) ylim 200
      nddist = continuous nd.pdf (0,60) ylim 200
      ndgeoms = mkGeoms n nd (n >= 20)
      ndtitle = [title { aes | label <- Just ("N(µ=" ++ toString mu  
                                           ++ ",&sigma;=" ++ toString s ++ ")") } ]
      ndintegral = [annotate_integral { aes | translate <- Just (0.8,0.5), limits <- Just (mu-s,mu+s) }] 
   in 

      flow down [ 
         Html.toElement 1000 50 <| Html.h1 [HA.align "center"] [Html.text "Approximating a binomial distribution"],

         flow right <|  
            [Html.toElement 600 250 <| Html.text bintext,
            plot (600,250) margins bindist (geomsD ++ bingeoms ++ bintitle) m w], 

         flow right <| 
            [Html.toElement 600 250 <| Html.text ndtext,
               flow outward <| 
               [plot (600,250) margins nddist (geomsC ++ ndgeoms ++ ndtitle) m w,
               plot (600,250) margins nddist ndintegral m w] ],

         flow right <| 
            [Html.toElement 600 250 <| Html.text potext,
            plot (600,250) margins podist (geomsD ++ pogeoms ++ potitle) m w
            ]
         ]



main = Signal.map2 render Mouse.position Window.dimensions

geomsD = [  
          background { aes | colour <- Just lightBlue, visibility <- Just 0.4 },
          geom_image { aes | dims <- Just (50,50), label <- Just "http://elm-lang.org/imgs/mario/walk/right.gif", translate <- Just (0,20) },
          xAxis { aes | label <- Just "Antal försök", tickspacing <- Just 10 },  
          yAxis { aes | label <- Just "Sannolikhet", tickspacing <- Just 0.1, revolve <- Just True },
          geom_bar { aes | colour <- Just lightBlue, dynamic <- Just False } 
          ]

geomsC = [  
          background { aes | colour <- Just lightBlue, visibility <- Just 0.4 },
          geom_image { aes | dims <- Just (50,50), label <- Just "http://elm-lang.org/imgs/mario/walk/right.gif", translate <- Just (0,20) },
          --geom_vline { aes | x <- Just 15, annotate <- Just False },
          geom_vline { aes | x <- Just 20, annotate <- Just False, colour <- Just darkGreen, linetype <- Just GC.dotted },
          geom_curve { aes | dynamic <- Just False, colour <- Just grey },
          xAxis { aes | label <- Just "Antal försök", tickspacing <- Just 10 },  
          yAxis { aes | label <- Just "Sannolikhet", tickspacing <- Just 0.1, revolve <- Just True }
          --tangent { aes | translate <- Just (57,5) }
          ]

mkGeoms n pdist use = [ 
   geom_vline { aes | x <- Just pdist.mu, label <- Just "µ=", translate <- Just (4,0.05) },  
   geom_vline { aes | x <- Just n, label <- Just "n=", translate <- Just (0,-0.05), colour <- Just darkGrey },  
   geom_hlinerange { aes | y <- Just (-0.03), limits <- Just (pdist.mu - pdist.sigma, pdist.mu + pdist.sigma), label <- Just ("&sigma;=" ++ (toString <| C.dec 2 pdist.sigma)), translate <- Just (6,0) },  
   geom_hlinerange { aes | y <- Just (-0.05), limits <- Just (pdist.mu - 2*pdist.sigma, pdist.mu + 2*pdist.sigma), label <- Just "", translate <- Just (4,0) },  
   geom_hlinerange { aes | y <- Just (-0.07), limits <- Just (pdist.mu - 3*pdist.sigma, pdist.mu + 3*pdist.sigma), label <- Just "", translate <- Just (4,0) },  
   geom_area { aes | limits <- Just (pdist.mu - pdist.sigma, pdist.mu + pdist.sigma), colour <- Just (if use then darkGreen else darkBlue), visibility <- Just 0.5, dynamic <- Just False, nsteps <- Just 20 }
   ] 


bintext = "Binomialfördelningen beskriver sannolikheter för två ömsesidigt uteslutande utfall och ett känt antal försök. Det gäller till exempel olika typer av spel, där utfallen är vinst och förlust. Varje försök är oberoende: utfallet av ett tärningskast beror inte på det föregående kastet. För att konstruera denna fördelning behövs två komponenter: sannolikheten för att händelsen inträffar (π) och antal försök (n). I verkligheten sker en slumpmässig variation men i det långa loppet kommer antalet positiva utfall att närma sig väntevärdet (µ), som enkelt beräknas genom n * π. Binomialfördelningen är en diskret fördelning, eftersom x-värdena (0..n) utgörs av heltal. Allt eftersom antalet försök växer blir binomialfördelningen mer omständlig att beräkna, och det kan därför vara en god idé att \"approximera\" den med en liknande fördelning. Exemplet till höger illustrerar fördelningen för mellan 4 och 60 försök, då sannolikheten för positivt utfall är en fjärdedel."

potext = "Poissonfördelningen är en annan diskret sannolikhetsfördelning som kan användas för att efterlikna binomialfördelningen. Eftersom poissonfördelningen används för mer ovanliga/osannolika händelser så passar den bra för stora n och små π, dvs ett stort antal försök och liten sannolikhet för positivt utfall. En tumregel är n ≥ 10 och π ≤ 0,1. Med andra ord är den olämplig att använda här då π=0.25. Poängen med diagrammet är snarare att visa fördelningarna och hur de liknar varandra. Poissonfördelningen har en något plattare form och en större standardavvikelse, dvs att det totala utfallet förväntas variera inom ett större intervall. Standardavvikelsen för en poissonfördelning är för övrigt roten ur väntevärdet. "

ndtext = "Normalfördelningen är en välkänd kontinuerlig fördelning som kan användas istället för binomialfördelningen om en av flera möjliga tumregler uppfylls, till exempel nπ≥5 och n(1-π)≥5. Här betyder det att n≥5/π = n≥20. Med andra ord kan vi byta ut binomialfördelningen mot en motsvarande normalfördelning då antalet försök är minst 20. Fördelningen N(µ,σ) konstrueras med väntevärdet och standardavvikelsen från den ursprungliga binomialfördelningen. Om det vi försöker mäta liknar en normalfördelning så kan vi räkna med att ungefär två tredjedelar (0.68) av utfallen sker inom ±1 standardavvikelse omkring väntevärdet."
