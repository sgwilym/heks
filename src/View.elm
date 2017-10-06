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
                            Collage.filled Color.lightBlue hexagonShape

                        Terrain.Sea ->
                            Collage.filled Color.blue hexagonShape

                Nothing ->
                    Collage.filled Color.gray hexagonShape

        islandShape =
            Collage.oval 40 30

        islandForm =
            let
                form =
                    case hexTerrain of
                        Just terrain ->
                            case terrain of
                                Terrain.Earth ->
                                    Collage.filled Color.lightGreen islandShape

                                Terrain.Sea ->
                                    Collage.filled Color.blue islandShape

                        Nothing ->
                            Collage.filled Color.gray islandShape
            in
                Collage.move (HexGrid.hexToPixel layout point) form

        neighbours =
            HexGrid.neighbors point

        directionToPolygon direction =
            let
                range =
                    case direction of
                        0 ->
                            List.range 5 6

                        1 ->
                            List.range 4 5

                        2 ->
                            List.range 3 4

                        3 ->
                            List.range 2 3

                        4 ->
                            List.range 1 2

                        _ ->
                            List.range 0 1

                getCorner : Int -> ( Float, Float )
                getCorner corner =
                    let
                        ( x, y ) =
                            HexGrid.hexCornerOffset layout (toFloat corner)

                        ( cX, cY ) =
                            HexGrid.hexToPixel layout point
                    in
                        ( x + cX, y + cY )
            in
                HexGrid.hexToPixel layout point :: (List.map getCorner range)

        neighbourToMaybeForm ( direction, maybeTerrain ) =
            case hexTerrain of
                Just reallyTerrain ->
                    case reallyTerrain of
                        Terrain.Earth ->
                            case maybeTerrain of
                                Just terrain ->
                                    case terrain of
                                        Terrain.Earth ->
                                            Just (Collage.filled Color.lightGreen (Collage.polygon (directionToPolygon direction)))

                                        _ ->
                                            Nothing

                                Nothing ->
                                    Nothing

                        _ ->
                            Nothing

                Nothing ->
                    Nothing

        directionPairs =
            List.indexedMap
                (\index point ->
                    ( index, HexGrid.valueAt point grid )
                )
                neighbours

        neighbourForms =
            List.filterMap
                neighbourToMaybeForm
                directionPairs
    in
        hexagonForm :: (islandForm :: neighbourForms)
