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

id = (\n -> n)

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
          xAxis { aes | label <- Just "Number of trials", tickspacing <- Just 10 },  
          yAxis { aes | label <- Just "Probability", tickspacing <- Just 0.1, revolve <- Just True },
          geom_bar { aes | colour <- Just lightBlue, dynamic <- Just False } 
          ]

geomsC = [  
          background { aes | colour <- Just lightBlue, visibility <- Just 0.4 },
          geom_image { aes | dims <- Just (50,50), label <- Just "http://elm-lang.org/imgs/mario/walk/right.gif", translate <- Just (0,20) },
          --geom_vline { aes | x <- Just 15, annotate <- Just False },
          geom_vline { aes | x <- Just 20, annotate <- Just False, colour <- Just darkGreen, linetype <- Just GC.dotted },
          geom_curve { aes | dynamic <- Just False, colour <- Just grey },
          xAxis { aes | label <- Just "Number of trials", tickspacing <- Just 10 },  
          yAxis { aes | label <- Just "Probability", tickspacing <- Just 0.1, revolve <- Just True }
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


bintext = "Binomial distributions can be used to model probabilities where you have two mutually excluding events and a known number of trials. For example, a game might result in either win or loss for a given number of rounds. Also, each trial is independent from the others: the outcome of rolling a die does not depend on the previous roll. To construct this probability distribution, two components are needed: the probability of an event (π) and the number of trials (n). In practice results will vary, but in the long run the total outcome will approach the expected value (µ), which is n * π. The binomial distribution is discrete, since the x values (0..n) are integers. As more trials are performed, calculations become increasingly demanding. For this reason, you sometimes \"approximate\" it by using a similar distribution. The example on the right illustrates a binomial distribution of between 4 and 60 trials, when the probability of success is 1/4 (=0.25)." 


ndtext = "The normal distribution is a well-known continuous distribution which can be used to approximate binomial probabilities if one out of several criteria are met, for example nπ≥5 och n(1-π)≥5. In this case it means n≥5/π = n≥20. In other words we can swap a binomial distribution for the corresponding normal one when the number of trials is at least 20. The normal distribution, denoted N(µ,σ), is constructed with the expected value (µ) and standard deviation (σ) from the original binomial one. If what we are trying to measure resembles a normal distribution we can expect approximately two thirds (0.68) of all outcomes to be within ±1 standard deviation about the mean. "

potext = "The Poisson distribution is another discrete probability distribution, which sometimes can act as a substitute for the binomial one. Since the Poisson distribution is used for rare or unlikely events, it's good for large n and small π, i.e. it works well for a large number of trials and a small probability of success. One rule of thumb is n ≥ 10 and π ≤ 0,1. In other words we shouldn't use it when π=0.25. Even though it makes a poor approximation, you can still tell from looking at the charts that the two distributions are similar. The Poisson distribution is slightly flatter with a larger standard deviation, meaning that the outcome is expected to vary more. It uses a single parameter(µ) and the standard deviation is assumed to be the square root of this value. " 


