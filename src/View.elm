module View exposing (..)

import Collage
import Text
import Color
import HexGrid exposing (HexGrid)
import Terrain exposing (Terrain)
import Puzzle
import CrossGrid
import Cons
import Dict
import Set exposing (Set)


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
            Text.fromString hintText |> Text.color colour |> Text.bold |> Text.height 20 |> Collage.text |> Collage.move displacement |> Collage.rotate rotation
    in
        hintForm


gridToForms : HexGrid.Layout -> HexGrid Terrain -> List Collage.Form
gridToForms layout (HexGrid.HexGrid a dict) =
    List.map (pointToForm layout Terrain.Sea) (Dict.keys dict) |> List.concat


guessesToForms : HexGrid.Layout -> HexGrid Puzzle.Guess -> List Collage.Form
guessesToForms layout (HexGrid.HexGrid radius dict) =
    Dict.map
        (\point guess ->
            case guess of
                Puzzle.Filled ->
                    filledToForm layout point

                Puzzle.Cross ->
                    crossToForm layout point

                Puzzle.NoGuess ->
                    []
        )
        dict
        |> Dict.values
        |> List.concat


filledToForm : HexGrid.Layout -> HexGrid.Point -> List Collage.Form
filledToForm layout point =
    pointToForm layout Terrain.Earth point


crossToForm : HexGrid.Layout -> HexGrid.Point -> List Collage.Form
crossToForm layout point =
    let
        armForm =
            Collage.rect (layout.screenX * 0.8) (layout.screenX * 0.2)
                |> Collage.filled Color.lightRed

        crossForms =
            [ Collage.rotate (degrees 45) armForm
            , Collage.rotate (degrees -45) armForm
            ]
                |> List.map (Collage.move (HexGrid.hexToPixel layout point))
    in
        crossForms


pointToForm : HexGrid.Layout -> Terrain.Terrain -> HexGrid.Point -> List Collage.Form
pointToForm layout terrain point =
    let
        hexagonShape =
            Collage.polygon (HexGrid.polygonCorners layout point)

        hexagonForm =
            case terrain of
                Terrain.Earth ->
                    Collage.filled Color.lightGreen hexagonShape

                Terrain.Sea ->
                    Collage.filled Color.blue hexagonShape

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
