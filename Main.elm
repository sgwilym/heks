module Main exposing (..)

import Collage
import Hex exposing (Hex)
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
            , size = ( 30.0, 30.0 )
            , origin = ( 0.0, 0.0 )
            }

        lotsOfHexes =
            List.map
                (\( hash, _ ) ->
                    let
                        ( q, r ) =
                            hash
                    in
                        Hex q r
                )
                (Dict.toList
                    (Map.hexagonOfSea 7)
                )
    in
        Collage.collage 800 800 (List.map (View.hexToForm layout) lotsOfHexes |> List.concat)
            |> Element.toHtml
