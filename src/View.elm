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


gridToHintLabels : HexGrid.Layout -> HexGrid Terrain -> HexGrid Puzzle.Guess -> List Collage.Form
gridToHintLabels layout grid guesses =
    let
        ( xRows, zRows ) =
            CrossGrid.rows grid

        ( gxRows, gzRows ) =
            CrossGrid.rows guesses

        txRows =
            List.map2 (,) xRows gxRows

        tzRows =
            List.map2 (,) zRows gzRows
    in
        List.map (rowToHintLabelForm layout CrossGrid.X) txRows
            ++ List.map (rowToHintLabelForm layout CrossGrid.Z) tzRows


rowToHintLabelForm : HexGrid.Layout -> CrossGrid.Axis -> ( CrossGrid.Row Terrain, CrossGrid.Row Puzzle.Guess ) -> Collage.Form
rowToHintLabelForm layout axis ( solutionRow, guessRow ) =
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
                        solutionRow

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
            CrossGrid.hint
                solutionRow
                guessRow
                (\terrain ->
                    case terrain of
                        Terrain.Earth ->
                            True

                        _ ->
                            False
                )
                |> List.map
                    (\( hintNumber, isSatisfied ) ->
                        if isSatisfied then
                            "(" ++ (toString hintNumber) ++ ")"
                        else
                            toString hintNumber
                    )
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

        hintForm =
            Text.fromString hintText |> Text.color Color.black |> Text.bold |> Text.height 20 |> Collage.text |> Collage.move displacement |> Collage.rotate rotation
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
