module View exposing (..)

import Collage
import Layout
import Hex exposing (Hex)
import Color


hexToForm : Layout.Layout -> Hex -> Collage.Form
hexToForm layout hex =
    Collage.polygon (Layout.polygonCorners layout hex)
        |> Collage.outlined (Collage.solid Color.black)
