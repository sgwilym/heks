module View exposing (..)

import Collage
import Layout
import Hex exposing (Hex)
import Color
import Text
import Map


hexToForm : Layout.Layout -> ( ( Int, Int ), Map.Terrain ) -> List Collage.Form
hexToForm layout ( ( q, r ), terrain ) =
    let
        hex =
            Hex q r

        hexagonShape =
            Collage.polygon (Layout.polygonCorners layout hex)

        hexagonForm =
            case terrain of
                Map.Earth ->
                    Collage.filled Color.lightBlue hexagonShape

                Map.Sea ->
                    Collage.filled Color.blue hexagonShape

        coordinateText =
            Collage.text
                ((Text.fromString (String.join "  " [ toString hex.q, toString hex.r, toString (Hex.s hex) ]))
                    |> Text.color Color.white
                )
                |> Collage.move (Layout.hexToPoint layout hex)
    in
        [ hexagonForm
        , coordinateText
        ]
