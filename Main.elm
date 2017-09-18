module Main exposing (..)

import Collage
import Layout
import Element
import View
import Html
import Map
import Dict


main : Html.Html msg
main =
    let
        layout =
            { orientation = Layout.pointyOrientation
            , size = ( 40.0, 40.0 )
            , origin = ( 0.0, 0.0 )
            }

        seaWithLand =
            Map.update (Map.hexagonOfSea 5) ( 0, 2 ) Map.Earth
                |> Dict.toList
    in
        Collage.collage 800 800 (List.map (View.hexToForm layout) seaWithLand |> List.concat)
            |> Element.toHtml
