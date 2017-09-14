module View exposing (..)

import Collage
import Layout
import Hex exposing (Hex)
import Color
import Text


hexToForm : Layout.Layout -> Hex -> List Collage.Form
hexToForm layout hex =
    [ Collage.polygon (Layout.polygonCorners layout hex)
        |> Collage.outlined (Collage.solid Color.black)
    , Collage.text (Text.fromString (String.join " " [ toString hex.q, toString hex.r ])) |> Collage.move (Layout.hexToPoint layout hex)
    ]
