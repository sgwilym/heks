module View exposing (..)

import Collage
import Color
import HexGrid exposing (HexGrid)
import Terrain exposing (Terrain)
import Beast exposing (Beast)


beastToForm : HexGrid.Layout -> Beast -> Collage.Form
beastToForm layout beast =
    Collage.circle 20 |> Collage.filled Color.white |> Collage.move (HexGrid.hexToPixel layout beast.location)


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

                        Terrain.Pasture _ ->
                            Collage.filled Color.lightBlue hexagonShape

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

                                Terrain.Pasture _ ->
                                    Collage.filled Color.green islandShape

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
                                        Terrain.Sea ->
                                            Nothing

                                        _ ->
                                            Just (Collage.filled Color.lightGreen (Collage.polygon (directionToPolygon direction)))

                                Nothing ->
                                    Nothing

                        Terrain.Pasture _ ->
                            case maybeTerrain of
                                Just terrain ->
                                    case terrain of
                                        Terrain.Sea ->
                                            Nothing

                                        _ ->
                                            Just (Collage.filled Color.green (Collage.polygon (directionToPolygon direction)))

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
