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






customAes = { aes | radius <- Just 2, pointsize <- Just 9, colour <- Just darkBlue, visibility <- Just 0.6, annotate <- Just True }

render m w =  
   let
      cars = List.map2 (,) hp mpg
      d_cars = fromPairs cars (0,340) (0,35)
      lm_cars = C.linreg cars
      geoms_cars = [ 
         geom_hline { aes | y <- Just 0, annotate <- Just False, colour <- Just darkGrey },
         title { aes | label <- Just "Linear regression of the Mtcars dataset: mpg ~ hp" },
         xAxis { aes | tickspacing <- Just 50, translate <- Just (0,-5), label <- Just "Horse Power (hp)" },  
         yAxis { aes | tickspacing <- Just 10, revolve <- Just True, label <- Just "Fuel consumption (mpg)" }, 
      geom_abline { aes | fun <- Just lm_cars.f, dynamic <- Just False, limits <- Just (0,340) },
      geom_vlineranges { aes | fun <- Just lm_cars.f, colour <- Just green, linetype <- Just GC.dotted }, 
      geom_points { aes | dynamic <- Just False, colour <- Just blue, pointsize <- Just 4, visibility <- Just 1 },
      legend { aes | translate <- Just (50,10), label <- Just "residuals", colour <- Just green, linetype <- Just GC.dotted }
      ]

      iris = List.map2 (,) petalLength petalWidth

      d_iris = fromPairs iris (0,7) (-2,5)
      lm_iris = C.linreg iris
      geoms_iris = [ 
         geom_hline { aes | y <- Just 0, annotate <- Just False, colour <- Just darkGrey },
         title { aes | label <- Just "Linear regression of the Iris dataset: petalWidth ~ petalLength" },
         xAxis { aes | tickspacing <- Just 2, translate <- Just (0,-5), label <- Just "Petal Length" },  
         yAxis { aes | tickspacing <- Just 2, revolve <- Just True, label <- Just "Petal Width" }, 
         geom_abline { aes | fun <- Just lm_iris.f, dynamic <- Just False, limits <- Just (0,7) }
      ]

      iris' = List.map2 (\(x,y) s -> (x,y,s)) iris species
      setosa = List.map (\(x,y,s) -> (x,y)) <| List.filter (\(x,y,s) -> s == "setosa") iris'
      versicolor = List.map (\(x,y,s) -> (x,y)) <| List.filter (\(x,y,s) -> s == "versicolor") iris'
      virginica = List.map (\(x,y,s) -> (x,y)) <| List.filter (\(x,y,s) -> s == "virginica") iris'
      d_setosa = fromPairs setosa (0,7) (-2,5)
      d_versicolor = fromPairs versicolor (0,7) (-2,5)
      d_virginica = fromPairs virginica (0,7) (-2,5)
      vis = 0.6 -- visibility/alpha
      ps = 4 -- pointsize 
      geoms_setosa = [ geom_points { aes | dynamic <- Just False, colour <- Just blue, pointsize <- Just ps, visibility <- Just vis }, legend { aes | translate <- Just (1,4), label <- Just "Setosa", colour <- Just blue }, geom_vline { aes | x <- Just 1.462, colour <- Just blue, label <- Just "mean = ", linetype <- Just GC.dotted } 
      --geom_hline { aes | y <- Just 0.246, colour <- Just blue, label <- Just "mean = ", linetype <- Just GC.dotted, annotate <- Just False }  
      ]
      geoms_versicolor = [ geom_points { aes | dynamic <- Just False, colour <- Just orange, pointsize <- Just ps, visibility <- Just vis }, legend { aes | translate <- Just (1,3.5), label <- Just "Versicolor", colour <- Just orange }, geom_vline { aes | x <- Just 4.26, colour <- Just orange, label <- Just "mean = ", linetype <- Just GC.dotted }  
      --geom_hline { aes | y <- Just 1.326, colour <- Just orange, label <- Just "mean = ", linetype <- Just GC.dotted, annotate <- Just False }  
      ]
      geoms_virginica = [ geom_points { aes | dynamic <- Just False, colour <- Just red, pointsize <- Just ps, visibility <- Just vis }, legend { aes | translate <- Just (1,3), label <- Just "Virginica", colour <- Just red }, geom_vline { aes | x <- Just 5.552, colour <- Just red, label <- Just "mean = ", linetype <- Just GC.dotted }  
      --geom_hline { aes | y <- Just 2.026, colour <- Just red, label <- Just "mean = ", linetype <- Just GC.dotted, annotate <- Just False }   
      ]
   in

      flow down 
      [  
        flow right <|  
         [plot (1000,300) d_cars (geoms_cars) m w,
               Html.toElement 300 300 <| Html.text <| "linear model: " ++ toString lm_cars],

        flow right <|  
         [ 
            flow outward [ 
               plot (1000,300) d_iris (geoms_iris) m w,
                  plot (1000,300) d_setosa (geoms_setosa) m w,
                  plot (1000,300) d_versicolor (geoms_versicolor) m w,
                  plot (1000,300) d_virginica (geoms_virginica) m w
            ],
               Html.toElement 300 300 <| Html.text <| "linear model: " ++ toString lm_iris
              ]

          ]
       
         

main = Signal.map2 render Mouse.position Window.dimensions




