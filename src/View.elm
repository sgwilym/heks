module View exposing (..)

import Collage
import Text
import Color
import HexGrid exposing (HexGrid)
import Terrain exposing (Terrain)
import CrossGrid
import Cons


gridToHintLabels : HexGrid.Layout -> HexGrid Terrain -> List Collage.Form
gridToHintLabels layout grid =
    let
        ( xRows, zRows ) =
            CrossGrid.rows grid
    in
        List.map (rowToHintLabelForm layout CrossGrid.X) xRows
            ++ List.map (rowToHintLabelForm layout CrossGrid.Z) zRows


rowToHintLabelForm : HexGrid.Layout -> CrossGrid.Axis -> CrossGrid.Row Terrain -> Collage.Form
rowToHintLabelForm layout axis row =
    let
        firstHex =
            let
                sorted =
                    Cons.sortBy
                        (\( ( x, z ), _ ) ->
                            case axis of
                                CrossGrid.X ->
                                    z

                                CrossGrid.Z ->
                                    x
                        )
                        row

                first =
                    case axis of
                        CrossGrid.Z ->
                            Cons.head sorted

                        CrossGrid.X ->
                            Cons.reverse sorted |> Cons.head
            in
                first
                    |> Tuple.first

        hintText =
            CrossGrid.hint row
                (\terrain ->
                    case terrain of
                        Terrain.Earth ->
                            True

                        _ ->
                            False
                )
                |> List.map toString
                |> String.join " "

        displacement =
            case axis of
                CrossGrid.X ->
                    (HexGrid.hexToPixel layout (HexGrid.neighbor 5 firstHex))

                CrossGrid.Z ->
                    (HexGrid.hexToPixel layout (HexGrid.neighbor 3 firstHex))

        rotation =
            case axis of
                CrossGrid.X ->
                    degrees 60

                CrossGrid.Z ->
                    degrees 0

        colour =
            case axis of
                CrossGrid.X ->
                    Color.blue

                CrossGrid.Z ->
                    Color.green

        hintForm =
            Text.fromString hintText |> Text.color colour |> Text.bold |> Collage.text |> Collage.move displacement |> Collage.rotate rotation
    in
        hintForm


hexToForm : HexGrid.Layout -> HexGrid Terrain -> HexGrid.Point -> List Collage.Form
hexToForm layout grid point =
    let
        hexTerrain =
            HexGrid.valueAt point grid

        hexagonShape =
            Collage.polygon (HexGrid.polygonCorners layout point)

        hexagonForm =
            case hexTerrain of
                Just terrain ->
                    case terrain of
                        Terrain.Earth ->
                            Collage.filled Color.lightGreen hexagonShape

                        Terrain.Sea ->
                            Collage.filled Color.blue hexagonShape

                Nothing ->
                    Collage.filled Color.gray hexagonShape

        ( x, z ) =
            point

        overlay =
            if (x % 2 == 0) && (z % 2 == 0) then
                Collage.filled (Color.rgba 255 255 255 0.3) hexagonShape
            else if (x % 2 == 0) && (z % 2 /= 0) then
                Collage.filled (Color.rgba 255 255 255 0.25) hexagonShape
            else if (x % 2 /= 0) && (z % 2 == 0) then
                Collage.filled (Color.rgba 255 255 255 0.05) hexagonShape
            else
                Collage.filled (Color.rgba 255 255 255 0.1) hexagonShape
    in
        [ hexagonForm, overlay ]
