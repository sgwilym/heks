module View exposing (..)

import Collage
import Layout
import Hex exposing (Hex)
import Color
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
                    Collage.filled Color.blue hexagonShape

                Map.Sea ->
                    Collage.filled Color.lightBlue hexagonShape
    in
        [ hexagonForm ]
