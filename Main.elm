module Main exposing (..)

import Collage
import Hex exposing (Hex)
import Layout
import Element
import View
import Html


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
        Collage.collage 600 600 [ View.hexToForm layout hex ]
            |> Element.toHtml
