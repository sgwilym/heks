module Main exposing (..)

import Collage
import Hex exposing (Hex)
import Layout
import Element
import View
import Html


lotsOfHexes : List Hex
lotsOfHexes =
    [ --centre
      Hex 0 0
    , -- NE
      Hex 0 1
    , -- E
      Hex 1 0
    , -- SE
      Hex 1 -1
    , -- SW
      Hex 0 -1
    , -- W
      Hex -1 0
    , -- NW
      Hex -1 1
    ]


main : Html.Html msg
main =
    let
        layout =
            { orientation = Layout.pointyOrientation
            , size = ( 50.0, 50.0 )
            , origin = ( 0.0, 0.0 )
            }

        hex =
            Hex 0 0
    in
        Collage.collage 600 600 (List.map (View.hexToForm layout) lotsOfHexes |> List.concat)
            |> Element.toHtml
